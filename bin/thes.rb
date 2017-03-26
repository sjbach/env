#!/usr/bin/ruby
#!/usr/bin/ruby2.1
#
# Thesaurus.com scraper.
#
# Setup:
#  sudo aptitude install libruby ruby-dev rubygems libxml2 libxslt-dev
#  sudo aptitude install ruby-nokogiri
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

# Fork + file descriptor magic to wrap the output in `less`.
$rd, $wr = IO.pipe
if fork()
  $wr.close
  $stdin.reopen($rd)
  exec "less"
end
$rd.close

def clean(el)
  el.inner_text.strip
end

def main
  term = uri_escape(ARGV[0])
  begin
    doc = Nokogiri::HTML(open("http://thesaurus.com/browse/#{term}"))
  rescue OpenURI::HTTPError => error
    if error.io.status.first =~ /^4/
      $stderr.puts "Not found: '#{ARGV[0]}'"
      exit 1
    else
      raise error
    end
  end

  return if doc.at_css('.words-gallery-no-results')
  return if doc.at_css('#words-gallery-no-results')

  doc.css("div.synonyms").each do |div|
    $wr.puts "Entry: #{clean(div.at_css('strong.ttl'))} (#{clean(div.at_css('em.txt'))})"
    $wr.puts 'Synonyms...'
    $wr.puts to_terminal_rows(div.css('.relevancy-list span.text').map { |el| clean(el) })
    if not div.css('.antonyms span.text').empty?
      $wr.puts 'Antonyms...'
      $wr.puts to_terminal_rows(div.css('.antonyms span.text').map { |el| clean(el) })
    end
      $wr.puts
  end

  doc.css("div.syn_of_syns").each do |div|
    $wr.puts "Entry: #{clean(div.at_css('.subtitle a'))} (#{clean(div.at_css('.def'))})"
    $wr.puts 'Synonyms...'
    $wr.puts to_terminal_rows(div.css('li a').map { |el| clean(el) })
    # No Antonyms available in content. :-(
    $wr.puts
  end

  if not doc.css('div#example-sentences p').empty?
    $wr.puts 'Example Sentences:'
    doc.css("div#example-sentences p").each do |p|
      $wr.puts wrap_text(clean(p)).sub(/^  /,"- ")
    end
    $wr.puts
  end

  if not doc.css('div#word-origin p').empty?
    $wr.puts 'Word Origin & History:'
    doc.css("div#word-origin p").each do |p|
      $wr.puts wrap_text(clean(p).gsub(/\s+/, ' ')).sub(/^  /,"- ")
    end
  end

  # Sections that used to be available but are no longer available:
  # - Notes
  # - Related
  # - Concept
  # - Category

end

main()

