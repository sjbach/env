#!/usr/bin/ruby
#
# Merriam-Webster.com scraper
#
# Works okay on the ~1200 words I tried.
#
# Had to be re-written for m-w.com's Aug./Sept. 2010 site relaunch.
#
# Setup:
#  sudo aptitude install libruby ruby-dev rubygems libxml2 libxslt-dev
#  sudo gem install --version 0.6.164 hpricot
#

# TODO:
# - rewrite using nokogiri or something similar
# - do an initial pass of the DOM removing <style> and Facebook/G+ stuff

require 'uri'
require 'rubygems'
require 'hpricot'
require 'open-uri'
require 'net/http'

$debug = false

if ARGV[0].nil?
  $stderr.puts "Usage: m-w <word>"
  exit 0
end

def main
  url = uri_escape("http://www.merriam-webster.com/dictionary/#{ARGV[0]}")
  doc = Hpricot(open(url))

  # Check to see if there are multiple entries
  entries = []

  doc.search("//ol[@class = 'results']") do |ol|
    ol.search("//li/a") do |a|
      entries << a.attributes["href"]
    end
  end

  d "found entries:\n" + entries.join("\n")

  success = true

  if entries.empty?
    success &= parse_outer_entry(doc)
  else
    uri = URI.parse(url)
    (0..(entries.length-1)).each do |i|
      params = { 'start' => 0, 'show' => i.to_s, 'ref' => 'dictionary' }
      d "uri: #{uri}, params: #{params}"
      resp, data = Net::HTTP.post_form(uri, params)
      d "resp: #{resp}"
      if resp.class == Net::HTTPOK
        success &= parse_outer_entry(Hpricot(data))
      else
        puts "form post failed for entry #{i}"
        exit 1
      end
    end
  end

  if not success
    puts "No definition found for '#{ARGV[0]}'"
    exit 1
  end
end

# Debug print
def d(str)
  if $debug
    $stderr.puts str
  end
end

class AssertionError < StandardError ; end

def assert(condition, message = 'assertion failure')
  raise AssertionError.new(message) unless condition
end

def wrap_text(text, indent="  ", width=terminal_width())
  # Regex modified from:
  # http://blog.macromates.com/2006/wrapping-text-with-regular-expressions/
  width -= indent.length
  text.gsub(/(.{1,#{width}})( +|$)\n?|(.{#{width}})/, "#{indent}\\1\\3\n")
end

def terminal_width
  stty_width = %x{stty size}.split[1].to_i - 2
  stty_width < 0 ? 78 : stty_width
end

def uri_escape(str)
  # Square brackets are not caught as invalid characters...
  URI.escape(str).gsub("[", "%5B") \
                 .gsub("]", "%5D")
end

class DictEntry

  attr_accessor :word, :has_image, :available, :function, :usage,
    :pronunciation, :etymology, :first_use, :related, :synonyms_etc,
    :synonyms_discussion, :usage_discussion, :definitions,
    :special_definitions, :examples, :transitive_verb, :variant, :encyclopedia,
    :source

  def initialize
    @word = nil
    @available = true
    @has_image = false
    @function = nil
    @usage = nil
    @pronunciation = nil
    @etymology = nil
    @first_use = nil
    @related = []
    @synonyms_etc = []
    @synonyms_discussion = nil
    @usage_discussion = nil
    @definitions = []
    @special_definitions = []
    @examples = []
    @transitive_verb = :unset
    @variant = nil
    @encyclopedia = nil
    @source = nil
  end

  def pretty_print
    #
    # Print the parsed dictionary entry.
    # Note: pronunciation skipped.
    #

    if not @available
      puts "Entry: #{@word} -- unavailable"
      puts
      return
    end

    puts "Entry: #{@word} #{@has_image ? '  (has image)' : ''}"
    puts "Source: #{@source}" if @source
    puts "Function: #{@function}" if @function
    if not @related.empty?
      @related.each do |r|
        puts wrap_text(r)
      end
    end
    puts("Usage: #{@usage}") if @usage
    if @etymology
      puts "Etymology..."
      puts wrap_text(@etymology, " ")
    end
    if @first_use
      puts "First Use: #{@first_use}"
    end

    puts "Definitions..."
    print_definitions(@definitions)

    if not @special_definitions.empty?
      puts "Special Definitions..."
      print_definitions(@special_definitions)
    end

    if not @examples.empty?
      puts "Examples..."
      @examples.each do |e|
        wrapped = wrap_text("#{e}", "   ")
        puts wrapped.sub(/^  /," -")
      end
    end
    if not @synonyms_etc.empty?
      puts "Similar..."
      @synonyms_etc.each do |a|
        puts " #{a[0]}: "
        puts wrap_text(a[1], "   ")
      end
    end
    if @synonyms_discussion
      puts "Synonyms Discussion..."
      puts wrap_text(@synonyms_discussion, "  ")
    end
    if @usage_discussion
      puts "Usage Discussion..."
      puts wrap_text(@usage_discussion, "  ")
    end
    if @variant
      puts "Variant: #{@variant}"
    end
    if @encyclopedia
      puts "Encyclopedia..."
      puts wrap_text(@encyclopedia, "  ")
    end

    puts
  end
end

def parse_outer_entry(doc)

  doc.search("div.definition") do |div_definition|
    entry = DictEntry.new
    word = (div_definition/"h1").inner_text
    # Hack to remove "About Our Definitions" interfering with the entry
    entry.word = word.sub(/About Our Definitions.*/, '')
    d "found definition for #{entry.word}"

    # STEVE should this be somewhere further down?
    entry.available = (div_definition/'div.teaser').empty?

    if div_definition.search("div#mwEntryData").empty?
      entry.pretty_print
    else
      div_definition.search("div#mwEntryData") do |div_mwEntryData|
        headword_divs = div_mwEntryData.search('div.headword')
        if headword_divs and headword_divs.length > 1
          d "multiple headwords"
          # Hack: break the HTML into sections, each holding all content between
          # div.headwords.
          headword_divs.each do |headword_div|
            els = [headword_div]
            iter = headword_div.next_sibling
            while iter and not iter.classes.include?("headword")
              d "adding #{iter.to_html[0..20]} after #{els.length}"
              els << iter
              iter = iter.next_sibling
            end
            d "creating new doc with #{els.length} elements"
            # Hpricot crashes if I try to create an Element instead of a whole
            # document.
            new_div_definition = Hpricot(
              '<div class="definition">' +
              els.map{ |el| el.to_html }.join +
              '</div>').search('div.definition')
              parse_inner_entry(new_div_definition,
                                Marshal::load(Marshal.dump(entry)))
          end
        else
          parse_inner_entry(div_definition, Marshal::load(Marshal.dump(entry)))
        end
      end
    end
  end
end

def parse_inner_entry(div_definition, entry)

      div_definition.search("div.headword") do |div_headword|
        entry.function = (div_headword/"span.main-fl").inner_text.strip
        unless (div_headword/"span.usg").empty?
          entry.usage = (div_headword/"span.usg").inner_text.strip
        end
        unless (div_headword/"span.pr").empty?
          entry.pronunciation = (div_headword/"span.pr").inner_text.strip
        end
        # STEVE gotta be a better way.
        if div_headword.children.find {|c| c.class == Hpricot::Elem and c.name == "em"}
          entry.source = div_headword.children.find {
            |c| c.class == Hpricot::Elem and c.name == "em"
          }.inner_text.strip
        end
      end

      div_processor = lambda { |div|

        case div.get_attribute('class')
        when /^(sblk$|sense-block)/
          d 'sblk'
          parse_definition(div, entry.transitive_verb, entry.definitions)
        when /example-sentences/
          d 'example-sentences'
          div.search("li") do |li|
            unless li.inner_text == '[+]more[-]hide'
              entry.examples << li.inner_text
            end
          end
        when /etymology/
          d 'etymology'
          assert(entry.etymology.nil?)
          # Sometimes includes a First Use sub div note.
          extra_text = \
            (div/">div.content>div").to_a.inject("") { |s,e|
              s + "\n#{e.inner_text.strip}"
            }
          if not extra_text.empty?
            div.search(">div.content>div") do |div_inner|
              div_inner.swap('')
            end
          end
          entry.etymology = (div/">div.content").inner_text.strip + extra_text
        when /first-use/
          d "first-use"
          assert(entry.first_use.nil?)
          entry.first_use = (div/">div.content").inner_text
        when /synonyms-reference/
          d 'synonyms-reference'
          div.search(">div>div>div>div>div") do |div_related|
            case div_related.get_attribute('class')
            when /(syn|ant|rel|near)-para/
              inner_text = div_related.inner_text.strip
              if inner_text =~ /^(.*): (.*)/
                type = $1
                words = $2
                entry.synonyms_etc << [type, words]
              else
                puts "Could not parse synonyms thing: #{inner_text}"
                exit 1
              end
            when /see-more/
              nil # ignore
            else
              puts "unknown synonmys: #{div_related.get_attribute('class')}"
              exit 1
            end
          end
        when /synonyms-discussion/
          assert(entry.synonyms_discussion.nil?)
          entry.synonyms_discussion = (div/">div.content").inner_text.strip
        when /usage-discussion/
          assert(entry.usage_discussion.nil?)
          entry.usage_discussion = (div/">div.content").inner_text.strip
        when /^us$/
          assert(entry.usage_discussion.nil?)
          entry.usage_discussion = div.inner_text.strip
        when /^r$/  # related?
          d 'r'
          entry.related << div.inner_text
        when /^vt$/
          # transitive verb -- applies to subsequent definitions
          entry.transitive_verb = (div.inner_text !~ /intransitive/)
        when /^art$/
          entry.has_image = true
        when /^variant$/
          assert(entry.variant.nil?)
          entry.variant = (div/">div.content").inner_text.strip
        when /^concise-link$/
          # td class blurb
          encyclopedia = (div/">table>tr>td").inner_text.strip
          # Strip initial "<word> ? " before entry.
          encyclopedia = encyclopedia.sub(/^\s*#{entry.word}\s*\W*\s*/,'')
          # Strip everything after READ ARTICLE.
          entry.encyclopedia = encyclopedia.sub(/\s*READ.*/,'')
        when /^dr$/
          # Special use of word?
          div.search(">div.d>div") do |div_inner|
            parse_definition(div_inner, :unset, entry.special_definitions)
          end
        when /rhyming-dictionary/
        when /britannica-entry/
        when /browse/
        when /learners-link/
        when /wcentral-link/
        when /dictButtons/
        when nil
          d "Skipped: #{div}"
        when /^d$/
          d "Skipping weird nested div.d"
        else
          puts "unknown block: >>#{div.get_attribute('class')}<<"
          puts div
          exit 1
        end
      }

      div_definition.search("div.d") do |div_d|
        div_d.search(">div") do |div1|

          # Open the KonaBody container, if needed.
          if div1.get_attribute('class') == "KonaBody"
            d "Saw KonaBody"
            div1.search(">div") do |div2|
              d "KonaBody iter"
              div_processor.call(div2)
            end
          else
            d "No container"
            div_processor.call(div1)
          end
        end
      end

  # TODO: validate parse

  entry.pretty_print

  return true
end

def parse_definition(div_elem, transitive_verb, array)
  unless (div_elem/"div.snum").empty?
    num = (div_elem/"div.snum").inner_text
  end

  # Sometimes senses include sub-senses with letters as indicators.
  div_elem.search("div.scnt") do |div_scnt|
    if (div_scnt/"em.sn").empty?
      # No lettered sub-senses.
      definition = div_scnt.inner_text.strip.sub(/^: /,'')
      array << [num, nil, definition, transitive_verb]
    else
      div_scnt.search("span.ssens") do |span_ssens|
        if (span_ssens/"em.sn").empty?
          letter = nil
        else
          letter = (span_ssens/"em.sn").inner_text.strip
        end
        definition = span_ssens.inner_text.strip.sub(/^#{letter}[^:]*: /,'')
        array << [num, letter, definition, transitive_verb]
      end
    end
  end
end

def print_definitions(definitions)
  prev_tv = :unset
  prev_num = :unset
  has_numbers = definitions.find { |a| a[0] != nil }
  has_letters = definitions.find { |a| a[1] != nil }
  definitions.each do |a|
    num = if a[0]
            a[0]
          elsif has_numbers
            " "
          else
            nil
          end
    letter = a[1] || " "
    definition = a[2].gsub(/ +/, ' ')
    tv = a[3]
    if tv != prev_tv
      if tv
        puts " /transitive verb/"
      else
        puts " /intransitive verb/"
      end
      prev_tv = tv
    end
    if has_letters
      if prev_num == num
        lead = " #{' ' * (num or '').length} #{letter} : "
      else
        lead = " #{num} #{letter} : "
      end
      prev_num = num
    elsif num
      lead = "  #{num} : "
    else
      lead = "  : "
    end
    wrapped = wrap_text(definition, " " * lead.length)
    # Insert the lead on the first line
    puts wrapped.sub(/^ */,lead)
  end
end

main()

