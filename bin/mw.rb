#!/usr/local/bin/ruby
##!/usr/bin/ruby
#!/usr/bin/ruby2.1
#
# Merriam-Webster.com scraper
#
# Setup:
#  sudo apt install libruby ruby-dev rubygems libxml2 libxslt-dev
#  sudo apt install ruby-nokogiri
#

require 'uri'
require 'rubygems'
require 'nokogiri'
require 'net/http'

#require 'pp'
$debug = false

if ARGV[0].nil?
  $stderr.puts "Usage: mw <word>"
  exit 0
end

class String
  # Hack: String#strip doesn't remove non-breaking space A0 by default, so
  # pre-convert them to ' '.
  def strip_nbsp
    return self.gsub("\u00A0", " ").strip
  end

  # Include A0.
  def squeeze_whitespace
    return self.gsub(/[\n\u00A0]/, " ").squeeze(" ")
  end
end

def main
  uri =
    URI.parse(
      uri_escape(
        "https://www.merriam-webster.com/dictionary/#{ARGV[0]}"))
  doc = parse_and_sanitize_doc(uri_open_following_redirects(uri))

  # TODO does this happen in practice?  Looks like we get a 404
  if doc.at_css('body.definitions-page').nil?
    puts "No definition found for '#{ARGV[0]}'"
    exit 1
  end

  ever_parsed_full_def = false
  just_parsed_headword = false
  suppress_newline = false
  parsing_kids_definitions = false

  doc.css('div.card-box, .typo7').each do |card_box|
    classes = card_box.attributes['class'].to_s.split

    if classes.include?('typo7')
      # This is a text header separating groups of definitions.

      # If we were previously parsing "Defined for Kids" or "Defined for
      # English Language Learners" definitions, we must be done now.
      parsing_kids_definitions = false

      case card_box.content.downcase
      when /\bkids\b/,
           /english language learners/
        # These are essentially redundant with the other definitions, so we
        # don't print them.
        parsing_kids_definitions = true
      when /medical/
        puts
        puts header_text(' Medical ')
        puts
        suppress_newline = true
      when /\blaw\b/
        puts
        puts header_text(' Law ')
        puts
        suppress_newline = true
      when /phrases/
        puts
        puts header_text(' Phrases ')
        puts
        suppress_newline = true
      when /\bfinancial\b/
        puts
        puts header_text(' Financial ')
        puts
        suppress_newline = true
      else
        puts "Unexpected header: #{card_box.content}"
        exit 1
      end
      next
    end

    if parsing_kids_definitions
      # Suppress the definition parsing below.
      next
    elsif classes.include?('examples-box')
      puts 'Examples...'
      card_box.css('.definition-list li').each do |e|
        wrapped = wrap_text("#{e.content.strip_nbsp}", "   ")
        puts wrapped.sub(/^  /," -")
      end
    elsif classes.include?('fresh-examples-box')
      # See: 'influence'.
      puts 'Recent examples...'
      card_box.css('.definition-list li').each do |e|
        assert(e.at_css('.cite-example'))
        assert(e.at_css('.cite-credit'))
        puts wrap_text(
          (e.at_css('.cite-example').content.strip_nbsp + " " +
           e.at_css('.cite-credit').content.strip_nbsp).gsub(/\s+/, ' '),
          "   ").sub(/^  /," -")
      end

    elsif classes.include?('origin-box')
      puts 'Origin...'
      card_box.css('div.card-primary-content p, ' +
                   # See: 'troglodyte'.
                   'div.card-primary-content .et').each do |p|
        stripped_content = p.content.strip_nbsp
        if !stripped_content.empty?
          puts wrap_text(stripped_content, " ")
        end
      end
    elsif classes.include?('rhymes-with-box')
      # (Elided.)
    elsif classes.include?('first-use-box')
      # TODO put all on one line?
      puts 'First known use...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, " ")
      end
    elsif (classes.include?('related-box') ||
           # See: 'troglodyte'.
           classes.include?('thesaurus-synonyms-box'))
      puts 'Related...'
      parse_and_print_synonym_box(card_box)
    elsif classes.include?('other-x-terms-box')
      puts 'Other field terms...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, " ")
      end
    elsif classes.include?('related-phrases-box')
      puts 'Related phrases (separate definitions)...'
      card_box.css('div.card-primary-content li').each do |li|
        wrapped = wrap_text("#{li.content.strip_nbsp}", "   ")
        puts wrapped.sub(/^  /," -")
      end

    # TODO: check to see if this is still ever emitted, or if it's been
    # supplanted by syns-box.
    elsif classes.include?('synonym-discussion-box')
      puts 'Synonym discussion...'
      card_box.css('div.card-primary-content .definition-block').each do |li|
        puts wrap_text(li.content.strip_nbsp, " ")
      end

    # See e.g. 'shun'.
    elsif classes.include?('syns-box')
      puts 'Synonym discussion...'
      card_box.css('.syn').each do |syn_el|
        syn_el.css('li .t').to_a.map do |li_t|
          # Hack: create text representation of bullet; not robust.
          li_t.add_previous_sibling('[(*) ')
          li_t.add_next_sibling(']')
        end
        puts wrap_text(syn_el.content.squeeze_whitespace.strip_nbsp, " ")
      end

    elsif classes.include?('history-box')
      puts 'History...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, " ")
      end

    elsif (classes.include?('little-gems-box') or
           # See: 'family'.
           classes.include?('sinote-box'))
      puts 'Aside...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, "  ")
      end

    elsif classes.include?('variants-box')
      # TODO: put all on one line?
      puts 'Variants...'
      card_box.css('div.card-primary-content').each do |content|
        puts wrap_text(content.content.strip_nbsp, " ")
      end

    elsif classes.include?('usage-box')
      puts 'Usage...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, "  ")
      end

    elsif classes.include?('headscratcher-box')
      puts 'Headscratcher...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, "  ")
      end

    elsif classes.include?('bio-box')
      puts 'Biographical note...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, "  ")
      end

    elsif classes.include?('art-box')
      puts '[Has illustration]'

    elsif classes.include?('w3-note-box')
      puts '[Has larger entry in unabridged dictionary]'

    # Only has been seen subsequent to Financial Definition section.
    # See e.g. 'warrant'.
    elsif classes.include?('investing-answer')
      parse_and_print_investing_answer(card_box)

    elsif classes.include?('headword-box')
      if (!just_parsed_headword && ever_parsed_full_def &&
          !suppress_newline)
        puts
      end
      assert(!classes.include?('another-def'))
      parse_and_print_headword_box(card_box, !just_parsed_headword)
      ever_parsed_full_def = true
      suppress_newline = false
      just_parsed_headword = true

    elsif classes.include?('another-def')
      if (!just_parsed_headword && ever_parsed_full_def &&
          !suppress_newline)
        puts
      end
      assert(!classes.include?('headword-box'))
      parse_and_print_another_def(card_box, !just_parsed_headword)
      ever_parsed_full_def = true
      suppress_newline = false
      just_parsed_headword = false

    else
      puts "Unexpected box type: #{classes.join(',')}"
      exit 1
    end
  end
end

# Parse input as an HTML document and remove elements from the DOM that are
# known to interfere with parsing or are otherwise unwanted.
def parse_and_sanitize_doc(content)
  worthless_content_selectors = [
    'input',
    'script',
    'style',
    'head > meta',
    'head > link',
    # 'Word of the Day'
    '.right-rail',
    # Quiz links, etc.; click spam.
    '.recirc-bar',
    # Ask the Editors, word games; click spam.
    '.additional-content-area',
    '.menu-mob-cnt',
    # Social media misc.
    '.seen-and-heard-block',
    '.social-btns',
    '.social-sidebar',
    # Popularity.
    '.popularity-block',
    # Pronunciation buttons; can complicate parsing a bit.
    '.play-pron',
    # Harmless, but not useful to this scraper.
    '.global-footer',
    '.shrinkheader-t',
    '.also-found-in-card',
    '.search-cnt',
    '.menu-filler',
    '.filler-cnt',
    '.mob-search-btn',
    '.desk-search-btn',
    '.nav-btn-cnt',
    '.since1828-cnt',
    '.logo-cnt',
    '.link-cta-container',
    '#cite-module',
    # Ad/tracking stuff?
    '.central-abl-box',
    # "'foo' was our Word of the Day [...]. Hear the podcast!".
    # Interferes with card parsing.
    '.podcast-player-block',
    # Definitions of individual words in a multi-word search term.  I don't
    # find these useful.
    '.word-by-word-box',
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

def die(message = 'unexpected circumstance')
  raise AssertionError.new(message)
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

def header_text(text, width=terminal_width())
  return (('-' * (width/ 2 - text.length/2)) +
          text +
          ('-' * (width/ 2 - text.length/2)))
end

def uri_escape(str)
  # Square brackets are not caught as invalid characters...
  URI.escape(str).gsub("[", "%5B") \
                 .gsub("]", "%5D")
end

def uri_open_following_redirects(uri)
  5.times do
    assert(uri.scheme == "https")
    response = Net::HTTP.get_response(uri)
    case response
    when Net::HTTPSuccess
      return response.body
    when Net::HTTPNotFound
      $stderr.puts "404: No definition found."
      exit 1
    when Net::HTTPRedirection
      uri.path = uri_escape(response['location'])
    else
      # Should be doing something more intelligent here.
      die("Unimplemented HTTP response condition: #{response}")
    end
  end
  die("Too many redirects for uri: #{uri}")
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

def parse_and_print_investing_answer(card_box_node)
  is_first_section = true
  card_box_node.css('.section').each do |section_el|
    puts if !is_first_section
    is_first_section = false
    puts " #{section_el.at_css('h2').content.strip_nbsp}"
    puts
    is_first_p = true
    section_el.css('p').each do |p_el|
      puts if !is_first_p
      is_first_p = false
      puts wrap_text(p_el.content.strip_nbsp, "  ")
    end
  end
end


def parse_and_print_synonym_box(card_box_node)
  heading = nil
  text = ''
  card_box_node.at_css('.definition-block').children.each do |node|
    assert(node.element? || node.text?)
    case node.name
    when 'h6'
      unless heading.nil?
        puts wrap_text(text.strip_nbsp, "   ")
      end
      heading = node.content.strip_nbsp
      puts wrap_text("#{heading}:", " ")
      text = ''
    when 'div'
      # Convert "See more at <thesaurus link>".
      # TODO: include m-w.com/thesaurus content?
      assert(node.content.strip_nbsp =~ /^See more at/, 'Unexpected text')
      text += ' |-> (thes)'
    else  # In practice, 'text', 'a', and 'em'.
      text += node.content
    end
  end
  puts wrap_text(text.strip_nbsp, "   ")
end

def parse_and_print_headword_box(card_box_node, print_term = true)
  if print_term
    term = card_box_node.at_css('.entry-hword .hword')
    function = card_box_node.at_css('.entry-attr .fl')
    if term and function
      # Note: might be this doesn't ever occur.
      puts "Full: #{term.content.strip_nbsp} [#{function.content.strip_nbsp}]"
    elsif term
      puts "Full: #{term.content.strip_nbsp}"
    else
      assert(function.nil?, 'Sentence function specified but no term')
    end
  end

  pronunciation = card_box_node.css('.entry-attr .prs .pr').to_a.map { |pr_el|
    pr_el.content.strip_nbsp
  }.join(', ')
  if pronunciation.empty?
    syllables = card_box_node.at_css('.entry-attr .word-syllables')
    if syllables
      puts "Syllables: #{syllables.content.strip_nbsp}"
    end
  else
    puts "Pronunciation: #{pronunciation}"
  end

  # See e.g. 'inflame'.
  if card_box_node.at_css('.entry-attr.vrs')
    variants =
      card_box_node.css('.entry-attr.vrs .vr').to_a.map { |vr_el|
        assert(vr_el.at_css('.va'))
        if vr_el.at_css('.vl')
          "[#{vr_el.at_css('.vl').content.strip_nbsp}] "\
          "#{vr_el.at_css('.va').content.strip_nbsp}"
        else
          "#{vr_el.at_css('.va').content.strip_nbsp}"
        end
      }.join(', ')
    puts "Variants: #{variants}"
  end
end

def parse_and_print_another_def(card_box_node, print_term = true)
  # This card often includes the headword-box stuff for entries after the
  # first.
  parse_and_print_headword_box(card_box_node, print_term)

  # These appear within a vg, but I bet in practice they only appear once.
  inflections =
    card_box_node.css('.vg-ins .in').to_a.map { |in_el|
      assert(in_el.at_css('.if'))
      parsed = "#{in_el.at_css('.if').content.strip_nbsp}"
      if in_el.at_css('.il')
        parsed += " [#{in_el.at_css('.il').content.strip_nbsp}]"
      end
      if in_el.at_css('.prs')
        parsed += " " + in_el.css('.prs .pr .mw').to_a.map { |pr|
          pr.content.strip_nbsp
        }.join(',')
      end
      parsed
    }.join('  ')
  puts "Inflections: #{inflections.strip_nbsp}" if !inflections.empty?

  # Sanity checking.
  assert(card_box_node.css('.entry').length == 1,
         'Expected only a single entry')
  card_box_node.css('.mw_t_bc').each do |mw_t_bc_el|
    # All of these should just be semicolons.
    assert(mw_t_bc_el.content.strip_nbsp == ":")
  end

  card_box_node.css('.entry').each do |entry_el|
    sn_chain = []
    entry_el.css('> .vg, .uros').each do |vg_or_uros_el|

      classes = vg_or_uros_el.attributes['class'].to_s.split
      if classes.include?('vg')
        assert(!classes.include?('uros'), 'vg is also a uros')
        vg_el = vg_or_uros_el
        if vg_el.at_css('.vd')
          # E.g. "transitive verb" -- see second def of "warrant".
          puts " [#{vg_el.at_css('.vd').content.strip_nbsp}]"
        end
        vg_el.css('> .sb').each do |sb_el|
          sb_el.xpath('./*[starts-with(@class, "sb-")]').each do |sb_num_el|
            sb_num_el.css('> .sense').each do |sense_el|
              case sense_el.css('.sn').length
              when 0
                # Uncommon
                sn_chain << Sn.new
              when 1
                # Common
                sn_chain << Sn.parse(sense_el.at_css('.sn'))
              else
                die('Expected at most a single .sn')
              end
              assert(sense_el.css('> .dt').length == 1,
                     'Expected only a single .dt')
              dt = Dt.parse(sense_el.at_css('.dt'))
              #print_dt(dt, sn_chain)
              print_dt(dt, sn_chain.last)
            end
          end
        end
      elsif classes.include?('uros')
        vg_or_uros_el.css('.uro').each do |uro_el|
          assert(uro_el.css('.ure').length == 1, 'Expected a single .ure')
          word = uro_el.at_css('.ure').content.strip_nbsp
          pronunciations =
            uro_el.css('.pr .mw').to_a.map { |pr|
              pr.content.strip_nbsp
            }.join(',')
          function = uro_el.at_css('.fl').content.strip_nbsp
          puts " —#{word} #{pronunciations} [#{function}]"
          # TODO: parse 'utxt' when it appears; see e.g. compulsive, abscess.
        end
      else
        die('Should be unreachable')
      end
    end
  end
end

# Representation for definition enumerations, e.g. "1 a (2)".
# "Sn" refers to the class name used in m-w.com's DOM.
class Sn
  attr_accessor :sense_num, :sub_alpha, :sub_num

  def self.parse(sn_el)
    # Preconditions.
    assert(node_has_class(sn_el, 'sn'), 'element is not class .sn')
    assert(sn_el.css('.num').length <= 1, 'Expected at most one .num')
    assert(sn_el.css('.sub-num').length <= 1, 'Expected at most one .sub-num')

    sn = Sn.new
    initial_member_len = sn.instance_variables.length  # (0)
    sn_el.children.each do |el|
      if el.text?
          assert(sn.sub_alpha.nil?)
          sn.sub_alpha = el.content.strip_nbsp
      else
        el_classes = el.attributes['class'].to_s.split
        if el_classes.include?('num')
          assert(sn.sense_num.nil?)
          sn.sense_num = el.content.strip_nbsp
        elsif el_classes.include?('sub-num')
          assert(sn.sub_num.nil?, "@sub_num already set: #{sn.sub_num}")
          sn.sub_num = el.content.strip_nbsp
        else
          die("unexpected node type in Sn.parse: #{el}")
        end
      end
    end

    # If this is a top-level Sn and sub_num is set, then sub_alpha should also
    # be set.
    assert(sn.sense_num.nil? || sn.sub_num.nil? || sn.sub_alpha,
           'Sense hierarchy assumption violated')
    final_member_len = sn.instance_variables.length  # > 0
    assert(final_member_len > initial_member_len,
           'No Sn members set')
    return sn
  end

  def indent_length
    if @sense_num
      assert(!@sense_num.empty?)
      return 0
    elsif @sub_alpha
      assert(!@sub_alpha.empty?)
      return 2
    elsif @sub_num
      assert(!@sub_num.empty?)
      return 4
    else
      # This is just a placeholder Sn representing the absence of a .sn node.
      return 0
    end
  end

  def to_s
    [@sense_num, @sub_alpha, @sub_num].compact.join(' ')
  end
end

# Representation for an actual definition.  Might be compound, but should be
# just text.
# "Dt" refers to the class name used in m-w.com's DOM.
class Dt
  attr_accessor :defs
  SENTINEL = "::COLON::"

  # Note: modifies DOM.
  def self.parse(dt_el)
    # Preconditions.
    assert(node_has_class(dt_el, 'dt'), 'element is not class .dt')
    assert(dt_el.css('.mw_t_bc').length > 0,
           'Expected at least one .mw_t_bc (colon)')

    dt = Dt.new
    dt_el.css('.mw_t_bc').to_a.map do |mw_t_bc|
      mw_t_bc.content = SENTINEL
    end
    dt_el.css('li .t').to_a.map do |li_t|
      # Hack: create text representation of bullet; not robust.
      li_t.add_previous_sibling('(*) ')
    end

    subs = dt_el.css('.subs')
    if !subs.empty?
      # TODO: implement; see e.g. 'warrant', 'company.
      puts "<Warning: sub definitions excised>"
      subs.remove
    end

    dt.defs = dt_el.content.split(SENTINEL).map { |d|
      d.squeeze_whitespace.strip
    }.select { |d|
      !d.empty?
    }
    assert(dt.defs.length > 0)
    return dt
  end
end

def print_dt(dt, sn)
  # Preconditions.
  assert(!dt.defs.empty?, 'No definitions in dt')

  colon = ' : '
  prefix_str = "#{' ' * sn.indent_length}#{sn.to_s}"
  dt.defs.each_with_index do |d, i|
    wrapped = wrap_text(d, " " * (prefix_str.length + colon.length))
    wrapped[0...(prefix_str.length + colon.length)] =
      if i == 0
        "#{prefix_str}#{colon}"
      else
        "#{' ' * prefix_str.length}#{colon}"
      end
    puts wrapped
  end
end

main()

