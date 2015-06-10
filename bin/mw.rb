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
      if resp.class == Net::HTTPMovedPermanently
        d "moved permanently: #{resp} --> #{resp.header['location']}"
        resp = Net::HTTP.post_form(URI.parse(resp.header['location']), params)
      end
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
    '.ask-the-editors',
    'script',
    'style',
    '#homograph-tool-tip',
    '.gplusBtn',
    '.citeBtn',
    '.def-header',
    '.left_rail',
    '.right_rail',
    '.facebookBtn',
    '#fb-comments',
    '.fb-comments',
    '.facebook-comments',
    '#facebook',
    '.learners-link',
    '.wcentral-link',
    '.popularity-score',
    '#scrabble_speed_bump_container',
    '.spacer_dots',
    '#ROS_textAd',
    'input',
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
    :pronunciation, :etymology, :first_use, :relateds, :synonyms_etc,
    :synonyms_discussion, :usage_discussion, :definitions, :quick_definitions,
    :special_usages, :examples, :transitive_verb, :variant, :encyclopedia,
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
    @relateds = []
    @synonyms_etc = []
    @synonyms_discussion = nil
    @usage_discussion = nil
    @definitions = []
    @special_usages = []
    @quick_definitions = []
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

    @quick_definitions.each do |quick|
      wrapped = wrap_text(quick, "    ")
      # Insert the leading colon on the first line.
      puts wrapped.sub(/^ */, '  : ')
    end

    puts "Source: #{@source}" if @source
    puts "Function: #{@function}" if @function
    if not @relateds.empty?
      @relateds.each do |r|
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

    if not @special_usages.empty?
      puts "Special Usages..."
      print_special_usages(@special_usages)
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
  div_content = doc.css("div.main_content_area")
  if div_content.empty?
    $stderr.puts "No main_content_area found"
    return false
  elsif div_content.length > 1
    $stderr.puts "Warning: multiple main_content_area"
  elsif div_content.at_css('.reference_page_title_secondary')
    text =
      div_content.at_css('.reference_page_title_secondary').inner_text.strip
    return false if text == 'Not Found'
  end
  div_content = div_content.first

  entry = DictEntry.new
  word =
    (div_content.at_css('.headword h1') ?
     div_content.at_css('.headword h1') :
     div_content.at_css('.reference_section_title_secondary h1')).inner_text

  # Hack to remove "About Our Definitions" interfering with the entry.
  # (This is probably no longer necessary, but it doesn't hurt anything.)
  # Note that entry.word may be overridden below.
  entry.word = word.sub(/About Our Definitions.*/, '')
  d "found definition for #{entry.word}"

  # STEVE should this be somewhere further down?
  entry.available = div_content.at_css('div.teaser').nil?

  mw_entry_data = div_content.css("div#mwEntryData")
  if mw_entry_data.empty?
    entry.pretty_print()
  else
    if mw_entry_data.length > 1
      $stderr.puts "Warning: multiple mwEntryData"
    end
    # STEVE remove loop once I decide it's unnecessary
    mw_entry_data.each do |div_mwEntryData|
      headword_divs = div_mwEntryData.css('div.headword')
      if headword_divs and headword_divs.length > 1
        d "multiple headwords"
        # Hack: break the HTML into sections, each holding all content between
        # div.headwords.
        headword_divs.each do |headword_div|
          cloned_entry = Marshal::load(Marshal.dump(entry))
          cloned_entry.word = scrape_syllable_separated_word(headword_div)
          cloned_entry.quick_definitions =
            scrape_quick_definitions(headword_div)
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
          new_div_mwEntryData = parse_doc(
            '<div id="mwEntryData">' +
            els.map{ |el| el.to_html }.join +
            '</div>').at_css('div#mwEntryData')
          scrape_inner_entry(new_div_mwEntryData, cloned_entry)
        end
      else
        cloned_entry = Marshal::load(Marshal.dump(entry))
        cloned_entry.word = scrape_syllable_separated_word(headword_divs[0])
        cloned_entry.quick_definitions =
          scrape_quick_definitions(headword_divs[0])
        scrape_inner_entry(div_mwEntryData, cloned_entry)
      end
    end
  end
  return true
end

def scrape_syllable_separated_word(headword_div)
  d headword_div
  el = (headword_div.at_css('.hw-syllables') ?
        headword_div.at_css('.hw-syllables') :
        (headword_div.at_css('h1') ?
         headword_div.at_css('h1') :
         headword_div.at_css('h2')))
  el.at_css('sup').remove if el.at_css('sup')
  return el.inner_text.strip
end

def scrape_quick_definitions(headword_div)
  headword_div.css('.ld_on_collegiate p').map { |quick_el|
    clean_definition(quick_el).sub(/^: /, '')
  }
end

def scrape_inner_entry(div_mwEntryData, entry)
  div_mwEntryData.css("div.headword").each do |div_headword|
    entry.function = div_headword.at_css("span.main-fl") && \
      div_headword.at_css("span.main-fl").inner_text.strip
    if div_headword.at_css("span.usg")
      entry.usage = div_headword.at_css("span.usg").inner_text.strip
    end
    if div_headword.at_css("span.pr")
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
      scrape_definition(div, entry.transitive_verb, entry)
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
      # Note: may no longer ever exist at this part of the DOM.
      d 'r'
      entry.relateds << div.inner_text
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
    when /^bio-note$/
      entry.bio_note = div.at_css("div.content").inner_text.strip
    when /rhyming-dictionary/
      actual_type = div.at_css('>h2>span').inner_text.strip
      if actual_type !~ /Rhymes with/
        # Some extra content is listed under rhyming-dictionary, but isn't
        # actually about rhyming, e.g. 'Other Business Terms' or 'Other
        # Philosophy Terms'.
        words = div.at_css('div.content').inner_text.strip
        entry.synonyms_etc << [actual_type, words]
      end
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

  div_mwEntryData.css("div.d").each do |div_d|

    # This weird definition HTML appears for foreign terms
    if div_d.css('>strong')
      definition =
        div_d.xpath('strong/following-sibling::text()')
            .inner_text
            .gsub(/[[:space:]]/, ' ')
            .strip
      unless definition.empty?
        entry.definitions << [nil, nil, definition, :unset]
      end
    end

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

def clean_definition(el)
  # m-w.com has started to insert &nbsp; characters into the definitions to
  # force 2+ spaces.  We only want a max of a single space, though.
  el.inner_text.strip.gsub(/[[:space:]]+/, ' ')
end

def scrape_definition(div_elem, transitive_verb, entry)
  if div_elem.at_css("div.snum")
    num = div_elem.at_css("div.snum").inner_text
  end

  # Sometimes senses include sub-senses with letters as indicators.
  div_elem.css("div.scnt").each do |div_scnt|
    next if not div_scnt.ancestors('div.dr').empty?
    next if not div_scnt.ancestors('div.r').empty?

    if div_scnt.at_css("em.sn").nil?
      # No lettered sub-senses.
      definition = clean_definition(div_scnt).sub(/^: /, '')
      entry.definitions << [num, nil, definition, transitive_verb]
    else
      div_scnt.css("span.ssens").each do |span_ssens|
        if span_ssens.at_css("em.sn").nil?
          letter = nil
        else
          letter = span_ssens.at_css("em.sn").inner_text.strip
        end
        definition = clean_definition(span_ssens).sub(
          /^#{letter}[^:]*: +/,
          '')
        entry.definitions << [num, letter, definition, transitive_verb]
      end
    end
  end

  # Word information that isn't specific to this particular definition, but
  # still appears in the schema at this level for whatever reason.  It's likely
  # that these only appear on the last definition.

  # Word variants, e.g. '— pre·scrip·tive·ly   adverb'.
  div_elem.css("div.r").each do |div_r|
    entry.relateds << div_r.inner_text.strip
  end

  # Special usages, e.g. '— in microcosm <newline> <definition>'
  div_elem.css("div.dr").each do |div_dr|
    special_ssens = div_dr.at_css('.ssens')
    definition = clean_definition(special_ssens).sub(/^: /, '')
    special_ssens.remove()
    usage = div_dr.inner_text.strip
    entry.special_usages << [usage, definition]
  end
end

def print_special_usages(usages)
  usages.each do |u|
    usage, definition = *u
    puts " #{usage}"
    wrapped = wrap_text(definition, "     ")
    # Insert the leading colon on the first line.
    puts wrapped.sub(/^ */, '   : ')
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

