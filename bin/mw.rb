#!/usr/bin/ruby
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

  ever_parsed_quick_or_full_def = false
  just_parsed_quick_def = false
  just_parsed_full_def = false
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
    elsif classes.include?('origin-box')
      puts 'Origin...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, " ")
      end
    elsif classes.include?('rhymes-with-box')
      # (Elided.)
    elsif classes.include?('first-use-box')
      # TODO put all on one line?
      puts 'First known use...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, " ")
      end
    elsif classes.include?('related-box')
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

    elsif classes.include?('synonym-discussion-box')
      puts 'Synonym discussion...'
      card_box.css('div.card-primary-content .definition-block').each do |li|
        puts wrap_text(li.content.strip_nbsp, " ")
      end

    elsif classes.include?('history-box')
      puts 'History...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.content.strip_nbsp, " ")
      end

    elsif classes.include?('little-gems-box')
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

    elsif classes.include?('art-box')
      puts '[Has illustration]'

    # Note: m-w.com seems to have stopped emitting this section.  I'm not sure
    # if it'll come back (I found it useful) so leaving this logic in for now.
    elsif classes.include?('quick-def-box')
      if (ever_parsed_quick_or_full_def &&
          (just_parsed_quick_def || just_parsed_full_def) &&
          !suppress_newline)
        puts
      end
      parse_and_print_quick_def_box(card_box)
      just_parsed_quick_def = true
      just_parsed_full_def = false
      ever_parsed_quick_or_full_def = true
      suppress_newline = false

    elsif classes.include?('full-def-box')
      if (!just_parsed_headword && ever_parsed_quick_or_full_def &&
           (just_parsed_full_def || !just_parsed_quick_def) &&
           !suppress_newline)
        puts
      end
      parse_and_print_full_def_box(card_box, !just_parsed_headword)
      just_parsed_quick_def = false
      just_parsed_full_def = true
      ever_parsed_quick_or_full_def = true
      suppress_newline = false
      just_parsed_headword = classes.include?('headword-box')

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
  stty_width = %x{stty size}.split[1].to_i - 2
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

def node_is_nonstandard_intro_colon(node)
  return (node.name == 'strong' and node.content.strip_nbsp == ':')
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


def parse_and_print_quick_def_box(card_box_node)
  term = card_box_node.at_css('h1, h2 i') or card_box_node.at_css('h1, h2')
  function = card_box_node.at_css('.word-attributes .main-attr')
  pronunciation = card_box_node.at_css('.word-attributes .pr')
  syllables = card_box_node.at_css('.word-attributes .word-syllables')

  if term and function
    puts "Quick: #{term.content.strip_nbsp} [#{function.content.strip_nbsp}]"
  elsif term
    puts "Quick: #{term.content.strip_nbsp}"
  else
    assert(function.nil?, 'Sentence function specified but no term')
  end

  puts "Pronunciation: #{pronunciation.content.strip_nbsp}" if pronunciation
  if syllables && !pronunciation
    puts "Syllables: #{syllables.content.strip_nbsp}"
  end

  card_box_node.css('.definition-list .definition-inner-item > span').each \
  do |outer_span|
    prefix = ''
    text = ''
    outer_span.children.each do |node|
      assert(node.element? || node.text?)
      if node_has_class(node, 'intro-colon')
        prefix += "#{node.content.strip_nbsp} "
      else
        text += node.content
      end
    end

    wrapped = wrap_text("#{text.strip_nbsp}", " " + " " * prefix.length)
    wrapped[0..(1 + prefix.length - 1)] = " #{prefix}"
    puts wrapped
  end
end

def parse_and_print_full_def_box(card_box_node, print_term = true)
  term = card_box_node.at_css('h1, h2 i') or card_box_node.at_css('h1, h2')
  # TODO double check this ever occurs
  function = card_box_node.at_css('.word-attributes .main-attr')
  pronunciation = card_box_node.at_css('.word-attributes .pr')
  syllables = card_box_node.at_css('.word-attributes .word-syllables')
  inflections =
    card_box_node.css('.inflections > span').to_a.map { |i|
      i.content.strip_nbsp
    }.join('  ')

  if print_term
    if term and function
      # Note: might be this doesn't ever occur.
      puts "Full: #{term.content.strip_nbsp} [#{function.content.strip_nbsp}]"
    elsif term
      puts "Full: #{term.content.strip_nbsp}"
    elsif print_term
      assert(function.nil?, 'Sentence function specified but no term')
    end
  end

  puts "Pronunciation: #{pronunciation.content.strip_nbsp}" if pronunciation
  if syllables && !pronunciation
    puts "Syllables: #{syllables.content.strip_nbsp}"
  end
  puts "Inflections: #{inflections.strip_nbsp}" if !inflections.empty?

  card_box_node.css('.inner-box-wrapper > ' +
                    '.card-primary-content, .dro, .uro').each do |el|
    if node_has_class(el, 'dro')
      # (Word inflections; not sure what 'dro' is short for.)
      #
      # TODO: use the same parsing as for 'uro', below.
      el.css('.runon-attributes').each do |runon|
        runon.css('em').each do |em|
          # adjective, adverb, noun, etc.
          assert(em.elements.empty?, 'Unexpected HTML in em')
          em.content = "[#{em.content}]"
        end
        puts " —#{runon.content.strip_nbsp}"
      end
    end
    if node_has_class(el, 'uro')
      # (Word inflections; not sure what 'uro' is short for.)
      #
      # These are usually trivial but sometimes have structure.
      # (See e.g. 'compulsive')
      el.css('.runon-attributes').each do |runon|
        ro_struct = {}
        runon.elements.each do |runon_el|
          case runon_el.name
          when 'h2'
            ro_struct['word'] = runon_el.content.strip_nbsp
          when 'em'
            ro_struct['function'] = runon_el.content.strip_nbsp
          when 'span'
            if node_has_class(runon_el, 'pr')
              ro_struct['pronunciation'] = runon_el.content.strip_nbsp
            elsif node_has_class(runon_el, 'in')
              assert(runon_el.elements.length == 2,
                     "Unexpected runon element: #{runon_el}")
              ro_struct['plural'] =
                "[#{runon_el.elements[0].content.strip_nbsp}: "\
                "#{runon_el.elements[1].content.strip_nbsp}]"
            elsif node_has_class(runon_el, 'utxt')
              ro_struct['example'] =
                wrap_text("#{runon_el.content.strip_nbsp}",
                          "    ").sub(/^   /,"  -")
            else
              die("Unexpected runon element: #{runon_el}")
            end
          else
            die("Unexpected runon element: #{runon_el}")
          end
        end
        # (Most of these will usually be nil.)
        puts " —#{ro_struct['word']} [#{ro_struct['function']}] "\
             "#{ro_struct['pronunciation']} #{ro_struct['plural']}"
        puts ro_struct['example'] if ro_struct['example']
      end
    end

    card_primary_contents = el.css('.card-primary-content')
    if node_has_class(el, 'card-primary-content')
      card_primary_contents << el
    end

    card_primary_contents.each do |card_primary_content|
      definition_list_items =
        # Common case.
        card_primary_content.css('.definition-list > li, ' \
                                 '.definition-list > .d > li')
      if (definition_list_items.empty? and
          card_primary_content.at_css('.definition-list') and
          card_primary_content.at_css('.definition-list > span'))
        # Unusual case: a single definition given by nodes within an ol tag,
        # but the nodes aren't li elements.
        # See: 'sic transit gloria mundi'.
        definition_list_items =
          card_primary_content.at_css('.definition-list').children
      end

      definition_list_items.each do |item|  # (Note: may not be li elements.)
        if node_has_class(item, 'vt')
          # adjective, adverb, noun, etc.
          puts " [#{item.content.strip_nbsp}]"
          if item.next_sibling.name == 'span'
            # Degenerate sub-definition.  See: 'inflame'.
            assert(
              node_is_nonstandard_intro_colon(item.next_sibling.elements[0]))
            def_item = DefItem.new
            item.next_sibling.children.each do |node|
              def_item.incorporate(node)
            end
            assert(def_item.appears_complete?)
            print_def_item(def_item, nil)
          end
        else
          prev_def_item = nil
          def_item = DefItem.new

          item.at_css('> p').children.each do |node|
            if (node_has_class(node, ['sense', 'sub', 'intro-colon']) or
                node_is_nonstandard_intro_colon(node))
              def_item.incorporate(node)
            elsif node.name == 'span'
              # Recurse another level.
              node.children.each do |node_inner|
                if (node_has_class(node_inner,
                                   ['sense', 'sub', 'intro-colon']) or
                    node_is_nonstandard_intro_colon(node_inner))
                  if def_item.appears_complete?
                    print_def_item(def_item, prev_def_item)
                    prev_def_item = def_item.clone
                    def_item.text = ''
                  end
                  def_item.incorporate(node_inner)
                else
                  def_item.incorporate(node_inner)
                end
              end
              # Last entry in sub-list.
              assert(def_item.appears_complete?)
              print_def_item(def_item, prev_def_item)
              prev_def_item = def_item.clone
              def_item.text = ''
            else  # text
              def_item.incorporate(node)
            end
          end

          # Last entry.
          if def_item.appears_complete?
            print_def_item(def_item, prev_def_item)
          end
        end
      end
    end
  end
end

class DefItem
  attr_accessor :sense_num, :sub_alpha, :sub_num, :colon, :text

  def initialize
    @text = ''
  end

  def incorporate(node)
    if node_has_class(node, 'sub')
      assert(node_has_class(node, ['alp', 'num']),
             'Sub-definition missing expected class')
      if node_has_class(node, 'alp')
        @sub_alpha = node.content.strip_nbsp
      else # node_has_class(node, 'num')
        @sub_num = node.content.strip_nbsp
      end
    elsif node_has_class(node, 'sense')
      @sense_num = node.content.strip_nbsp
    elsif (node_has_class(node, 'intro-colon') or 
           node_is_nonstandard_intro_colon(node))
      @colon = node.content.strip_nbsp
      assert(@colon == ':')
    else
      @text += node.content
    end
  end

  def appears_complete?
    # Some definitions do not include an intro-colon, or we would check that
    # here as well.
    return !(@text.nil? or @text.empty? or @text.strip_nbsp.empty?)
    #return !(@colon.nil? or @colon.empty? or
    #         @text.nil? or @text.empty? or @text.strip_nbsp.empty?)
  end
end

def print_def_item(def_item, prev_def_item)
  # Assumed order of definition prefixes.
  # TODO: actually keep track of the order of consumption.
  prefix = [def_item.sense_num,
            def_item.sub_alpha,
            def_item.sub_num,
            def_item.colon]
  if prev_def_item
    if def_item.sense_num and (def_item.sense_num == prev_def_item.sense_num)
      prefix[0] = ' '
    end
    if def_item.sub_alpha and (def_item.sub_alpha == prev_def_item.sub_alpha)
      prefix[1] = ' '
    end
    if def_item.sub_num and (def_item.sub_num == prev_def_item.sub_num)
      prefix[2] = ' '
    end
  end
  prefix_str = prefix.compact.join(' ') + ' '
  wrapped = wrap_text("#{def_item.text.strip_nbsp}",
                      " " + " " * prefix_str.length)
  wrapped[0..(1 + prefix_str.length - 1)] = " #{prefix_str}"
  puts wrapped
end

main()

