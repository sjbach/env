#!/usr/bin/ruby1.9.1
#
# Merriam-Webster.com scraper
#
# Works okay on the ~1200 words I tried, as of July, 2013.
#
# Setup:
#  sudo aptitude install libruby ruby-dev rubygems libxml2 libxslt-dev
#  sudo gem install nokogiri
#

# TODO:
# - do an initial pass of the DOM removing <style> and Facebook/G+ stuff

require 'uri'
require 'rubygems'
require 'nokogiri'
require 'open-uri'
require 'net/http'

$debug = false

if ARGV[0].nil?
  $stderr.puts "Usage: m-w <word>"
  exit 0
end

def main
  url = uri_escape("http://www.merriam-webster.com/dictionary/#{ARGV[0]}")
  doc = parse_doc(open(url))

  # Check to see if there are multiple entries
  entries = []

  doc.xpath("//ol[@class = 'results']").each do |ol|
    ol.xpath(".//li/a").each do |a|
      entries << a.attributes["href"]
    end
  end

  d "found entries:\n" + entries.join("\n")

  success = true

  if entries.empty?
    success &= scrape_outer_entry(doc)
  else
    uri = URI.parse(url)
    (0..(entries.length-1)).each do |i|
      params = { 'start' => 0, 'show' => i.to_s, 'ref' => 'dictionary' }
      d "uri: #{uri}, params: #{params}"
      resp = Net::HTTP.post_form(uri, params)
      d "resp: #{resp}"
      if resp.class == Net::HTTPOK
        success &= scrape_outer_entry(parse_doc(resp.body))
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

# Parse input as an HTML document and remove elements from the DOM that are
# known to interfere with parsing or are otherwise unwanted.
def parse_doc(content)
  worthless_content_selectors = [
    'script',
    'style',
    '#homograph-tool-tip',
    '.gplusBtn',
    '.citeBtn',
    '.facebookBtn',
    '#fb-comments',
    '.fb-comments',
    '#facebook',
  ]
  doc = Nokogiri::HTML(content)
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

  doc
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
    :source, :bio_note

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
    @bio_note = nil
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
    if @bio_note
      puts "Biographical Note..."
      puts wrap_text(@bio_note, "  ")
    end

    puts
  end
end

def scrape_outer_entry(doc)
  doc.css("div.definition").each do |div_definition|
    entry = DictEntry.new
    word = div_definition.at_css('h1').inner_text
    # Hack to remove "About Our Definitions" interfering with the entry
    entry.word = word.sub(/About Our Definitions.*/, '')
    d "found definition for #{entry.word}"

    # STEVE should this be somewhere further down?
    entry.available = div_definition.at_css('div.teaser').nil?

    if div_definition.css("div#mwEntryData").empty?
      entry.pretty_print()
    else
      div_definition.css("div#mwEntryData").each do |div_mwEntryData|
        headword_divs = div_mwEntryData.css('div.headword')
        if headword_divs and headword_divs.length > 1
          d "multiple headwords"
          # Hack: break the HTML into sections, each holding all content between
          # div.headwords.
          headword_divs.each do |headword_div|
            els = [headword_div]
            iter = headword_div.next_sibling
            while iter and not (iter.attributes['class'] &&
                                iter.attributes['class'].content.include?('headword'))
              d "adding #{iter.to_html[0..20]} after #{els.length}"
              els << iter
              iter = iter.next_sibling
            end
            d "creating new doc with #{els.length} elements"
            # There might be a better way to do this.
            new_div_definition = parse_doc(
              '<div class="definition">' +
              els.map{ |el| el.to_html }.join +
              '</div>').at_css('div.definition')
              scrape_inner_entry(new_div_definition,
                                 Marshal::load(Marshal.dump(entry)))
          end
        else
          scrape_inner_entry(div_definition,
                             Marshal::load(Marshal.dump(entry)))
        end
      end
    end
  end
end

def scrape_inner_entry(div_definition, entry)
  div_definition.css("div.headword").each do |div_headword|
    entry.function = div_headword.at_css("span.main-fl") && \
      div_headword.at_css("span.main-fl").inner_text.strip
    unless div_headword.at_css("span.usg").nil?
      entry.usage = div_headword.at_css("span.usg").inner_text.strip
    end
    unless div_headword.at_css("span.pr").nil?
      entry.pronunciation = div_headword.at_css("span.pr").inner_text.strip
    end

    # STEVE gotta be a better way.
    if div_headword.children.find {|c| c.class == Nokogiri::XML::Element and c.name == "em"}
      entry.source = div_headword.children.find { |c|
        c.class == Nokogiri::XML::Element and c.name == "em"
      }.inner_text.strip
    end
  end

  div_processor = lambda { |div|

    case div.get_attribute('class')
    when /^(sblk$|sense-block)/
      d 'sblk'
      scrape_definition(div, entry.transitive_verb, entry.definitions)
    when /example-sentences/
      d 'example-sentences'
      div.css("li").each do |li|
        unless li.inner_text.strip == '[+]more[-]hide'
          entry.examples << li.inner_text.strip
        end
      end
    when /etymology/
      d 'etymology'
      assert(entry.etymology.nil?)
      # Sometimes includes a First Use sub div note.
      extra_text = div.css(">div.content>div").to_a.inject("") { |s,el|
          s + "\n#{el.inner_text.strip}"
      }
      if not extra_text.empty?
        div.css(">div.content>div").each do |div_inner|
          div_inner.swap('')
        end
      end
      entry.etymology = \
        div.at_css(">div.content").inner_text.strip + extra_text
    when /first-use/
      d "first-use"
      assert(entry.first_use.nil?)
      entry.first_use = div.at_css(">div.content").inner_text
    when /synonyms-reference/
      d 'synonyms-reference'
      div.css('dl').each do |dl|
        type = dl.at_css('dt').inner_text.strip
        words = dl.at_css('dd').inner_text.strip
        entry.synonyms_etc << [type, words]
      end
    when /synonyms-discussion/
      assert(entry.synonyms_discussion.nil?)
      entry.synonyms_discussion = \
        div.at_css(">div.content").inner_text.strip
    when /usage-discussion/
      assert(entry.usage_discussion.nil?)
      entry.usage_discussion = div.at_css(">div.content").inner_text.strip
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
      entry.variant = div.at_css(">div.content").inner_text.strip
    when /^concise-link$/
      # td class blurb
      encyclopedia = div.at_css(">table>tr>td").inner_text.strip
      # Strip initial "<word> ? " before entry.
      encyclopedia = encyclopedia.sub(/^\s*#{entry.word}\s*\W*\s*/,'')
      # Strip everything after READ ARTICLE.
      entry.encyclopedia = encyclopedia.sub(/\s*READ.*/,'')
    when /^dr$/
      # Special use of word?
      div.css(">div.d>div").each do |div_inner|
        scrape_definition(div_inner, :unset, entry.special_definitions)
      end
    when /^bio-note$/
      entry.bio_note = div.at_css("div.content").inner_text.strip
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

  div_definition.css("div.d").each do |div_d|
    div_d.css(">div").each do |div1|

      # Open the KonaBody container, if needed.
      if div1.get_attribute('class') == "KonaBody"
        d "Saw KonaBody"
        div1.css(">div").each do |div2|
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

  entry.pretty_print()

  return true
end

def scrape_definition(div_elem, transitive_verb, array)
  unless div_elem.at_css("div.snum").nil?
    num = div_elem.at_css("div.snum").inner_text
  end

  # Sometimes senses include sub-senses with letters as indicators.
  div_elem.css("div.scnt").each do |div_scnt|
    if div_scnt.at_css("em.sn").nil?
      # No lettered sub-senses.
      definition = div_scnt.inner_text.strip.sub(/^: /,'')
      array << [num, nil, definition, transitive_verb]
    else
      div_scnt.css("span.ssens").each do |span_ssens|
        if span_ssens.at_css("em.sn").nil?
          letter = nil
        else
          letter = span_ssens.at_css("em.sn").inner_text.strip
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

