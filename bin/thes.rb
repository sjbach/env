#!/usr/local/bin/ruby
#!/usr/bin/ruby
#
# Half-assed Collins Thesaurus scraper.
# (Formerly thesaurus.com)
#
# Setup:
#  sudo apt install libruby ruby-dev rubygems libxml2 libxslt-dev
#  sudo apt install ruby-nokogiri
#

require 'uri'
require 'rubygems'
require 'nokogiri'
require 'open-uri'

$debug = false

if ARGV[0].nil?
  $stderr.puts "Usage: thes <word>"
  exit 0
end

def to_terminal_rows(words)
  rows = []
  rows << words.compact \
               .map { |word| word.strip } \
               .select { |word| !word.empty? } \
               .inject do |x,y|
    if (x + ', ' + y).length < terminal_width()
      x + ', ' + y
    else
      rows << x
      y.strip
    end
  end

  "  " + rows.join("\n  ")
end

def wrap_text(text, indent="  ", width=terminal_width())
  # Regex modified from:
  # http://blog.macromates.com/2006/wrapping-text-with-regular-expressions/
  width -= indent.length
  text.gsub(/(.{1,#{width}})( +|$)\n?|(.{#{width}})/, "#{indent}\\1\\3\n")
end

def terminal_width
  stty_width =
    if $stdin.isatty and $stdout.isatty
      %x{stty size}.split[1].to_i - 2
    else
      78
    end
  stty_width < 0 ? 78 : stty_width
end

def uri_escape(str)
  # Square brackets are not caught as invalid characters...
  URI.escape(str).gsub("[", "%5B") \
                 .gsub("]", "%5D")
end

class AssertionError < StandardError ; end

def assert(condition, message = 'assertion failure')
  raise AssertionError.new(message) unless condition
end

def die(message = 'unexpected circumstance')
  raise AssertionError.new(message)
end

def node_has_class(node, str_or_array)
  classes = node.attributes['class'].to_s.split
  case str_or_array
  when String
    return classes.include?(str_or_array)
  when Array
    return (classes & str_or_array).any?
  else
    die("bad class: #{str_or_array.class}")
  end
end

def remove_worthless_content(doc)
  worthless_content_selectors = [
    '.socialButtons',
    '.iconContainer',
    '.openButton',
    '.miniIconSenseContainer',
    '.copyright',
    '.content-box-videos',
    '.content-box-origin',
    # Cross-references; useful, but not useful here.
    '.xr',
    # Similarly spelled words.
    '.content-box-nearby-words'
  ]
  worthless_content_selectors.each do |selector|
    while true do
      element = doc.at_css(selector)
      if element.nil?
        break
      else
        element.remove()
      end
    end
  end
  return doc
end

def clean(el)
  el.inner_text.strip
end

# Fork + file descriptor magic to wrap the output in `less`.
$rd, $wr = IO.pipe
if fork()
  $wr.close
  $stdin.reopen($rd)
  exec "less"
end
$rd.close

def open_and_sanitize_doc(url)
  begin
    doc = Nokogiri::HTML(open(url))
  rescue OpenURI::HTTPError => error
    if error.io.status.first =~ /^4/
      $stderr.puts "Not found: '#{ARGV[0]}'"
      exit 1
    else
      raise error
    end
  end

  if doc.at_css('h1:contains("Sorry, no results for")')
    if doc.at_css('.suggested_words')
      $wr.puts 'Not found.  Did you mean:'
      $wr.puts to_terminal_rows(doc.css('.suggested_words li a').map { |el| clean(el) })
    else
      $wr.puts "Not found: '#{term}'."
    end
    exit 1
  end

  return remove_worthless_content(doc)
end

def main
  docs = []

  term = uri_escape(ARGV[0])
  docs << open_and_sanitize_doc("https://www.collinsdictionary.com/dictionary/english-thesaurus/#{term}")

  additional_pages = docs[0].css('.pagination a').map { |a| a['href'] }
  additional_pages.each do |url|
    docs << open_and_sanitize_doc(url)
  end

  main_content_found_and_printed = false

  docs.each do |doc|
    doc.css('.homograph-entry').each do |homograph_entry|
      # There may always be a single 'homograph-entry'; uncertain.
      homograph_entry.css('.page').each do |page|
        # There may always be a single 'page' within a doc; uncertain.
        page.css('> .content-box').each do |content_box|
          if node_has_class(content_box, 'entry')
            if !content_box.at_css('.content-box-header')
              if main_content_found_and_printed
                # Sometimes this is blank after the first page/doc.
                next
              end
              die("Unexpectedly empty content-box-header: #{content_box}")
            end
            # (Main entry)
            function = clean(content_box.at_css('.content-box-header'))
            #$wr.puts clean(content_box.at_css('.content-box-header')) + ':'
            content_box.css('.sense').each do |sense|
              sense.css('.synonymBlock').each do |block|
                # Probably only ever a single 'synonymBlock'.
                $wr.puts (
                  clean(block.at_css('.sensehead')) +
                  clean(block.at_css('.firstSyn')) +
                  " [#{function}]")
                $wr.puts wrap_text("<#{clean(block.at_css('.quote'))}>")
              end
              sense.css('.containerBlock > div').each do |block|
                $wr.puts "#{block['data-name']}:"
                block.children.each do |el|
                  $wr.puts wrap_text(clean(el))
                end
              end
              $wr.puts
            end
            # TODO: There are Idiom and Related Words sections here that I'm
            # ignoring.  See e.g. 'morning'.
            main_content_found_and_printed = true
          else
            # (Additional information)
            $wr.puts clean(content_box.at_css('.content-box-header')) + ':'
            content_box.css('.sense').each do |sense|
              $wr.puts sense.at_css('.syns_head').children.map { |el| clean(el) }.join(' | ')
              sense.css('.syns_container > div').each do |div|
                $wr.puts wrap_text(clean(div))
              end
              $wr.puts
            end
          end
        end
      end
    end
  end

end

main()

