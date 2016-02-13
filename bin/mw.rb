#!/usr/bin/ruby2.1
#
# Merriam-Webster.com scraper
#
# Setup:
#  sudo aptitude install libruby ruby-dev rubygems libxml2 libxslt-dev
#  sudo aptitude install ruby-nokogiri
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
  doc = parse_and_sanitize_doc(open(url))

  # TODO does this happen in practice?  Looks like we get a 404
  if doc.at_css('body.definitions-page').nil?
    puts "No definition found for '#{ARGV[0]}'"
    exit 1
  end

  just_parsed_quick_def = false
  just_parsed_full_def = false

  doc.css('div.card-box').each do |card_box|
    classes = card_box.attributes['class'].to_s.split
    if classes.include?('examples-box')
      puts 'Examples...'
      card_box.css('.definition-list li').each do |e|
        wrapped = wrap_text("#{e.inner_text.strip}", "   ")
        puts wrapped.sub(/^  /," -")
      end
    elsif classes.include?('origin-box')
      puts 'Origin...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.inner_text.strip, " ")
      end
    elsif classes.include?('rhymes-with-box')
      # (Elided.)
    elsif classes.include?('first-use-box')
      # TODO put all on one line?
      puts 'First known use...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.inner_text.strip, " ")
      end
    elsif classes.include?('related-box')
      puts 'Related...'
      parse_and_print_synonym_box(card_box)
    elsif classes.include?('other-x-terms-box')
      puts 'Other field terms...'
      card_box.css('div.card-primary-content p').each do |p|
        puts wrap_text(p.inner_text.strip, " ")
      end
    elsif classes.include?('related-phrases-box')
      puts 'Related phrases...'
      card_box.css('div.card-primary-content li').each do |li|
        wrapped = wrap_text("#{li.inner_text.strip}", "   ")
        puts wrapped.sub(/^  /," -")
      end

    elsif classes.include?('synonym-discussion-box')
      puts 'Synonym discussion...'
      card_box.css('div.card-primary-content .definition-block').each do |li|
        puts wrap_text(li.inner_text.strip, " ")
      end

    elsif classes.include?('quick-def-box')
      puts if just_parsed_quick_def or just_parsed_full_def
      parse_and_print_quick_def_box(card_box)
      just_parsed_quick_def = true
      just_parsed_full_def = false

    elsif classes.include?('full-def-box')
      puts if just_parsed_full_def or not just_parsed_quick_def

      term = card_box.at_css('h1, h2 i') or card_box.at_css('h1, h2')
      # TODO double check this ever occurs
      function = card_box.at_css('.word-attributes .main-attr')
      pronunciation = card_box.at_css('.word-attributes .pr')
      puts "Full: #{term.inner_text.strip}" if term
      puts "Function: #{function.inner_text.strip}" if function
      puts "Pronunciation: #{pronunciation.inner_text.strip}" if pronunciation

      card_box.css('.definition-list li').each do |e|
        wrapped = wrap_text("#{e.inner_text.strip}", "   ")
        puts wrapped.sub(/^  /," :")
      end

      just_parsed_quick_def = false
      just_parsed_full_def = true

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

def parse_and_print_synonym_box(card_box_node)
  heading = nil
  text = ''
  card_box_node.at_css('.definition-block').children.each do |node|
    assert(node.element? || node.text?)
    case node.name
    when 'h6'
      unless heading.nil?
        puts wrap_text(text.strip, "   ")
      end
      heading = node.inner_text.strip
      puts wrap_text("#{heading}:", " ")
      text = ''
    else  # In practice, 'text', 'a', and 'em'.
      text += node.inner_text
    end
  end
  puts wrap_text(text.strip, "   ")
end


def parse_and_print_quick_def_box(card_box_node)
  term = card_box_node.at_css('h1, h2 i') or card_box_node.at_css('h1, h2')
  function = card_box_node.at_css('.word-attributes .main-attr')
  pronunciation = card_box_node.at_css('.word-attributes .pr')
  puts "Quick: #{term.inner_text.strip}" if term
  puts "Function: #{function.inner_text.strip}" if function
  puts "Pronunciation: #{pronunciation.inner_text.strip}" if pronunciation

  card_box_node.css('.definition-list .definition-inner-item > span').each \
  do |outer_span|
    prefix = ''
    text = ''
    outer_span.children.each do |node|
      assert(node.element? || node.text?)
      if node.attributes['class'].to_s =~ /intro-colon/
        prefix += "#{node.inner_text.strip} "
      else
        text += node.inner_text
      end
    end

    wrapped = wrap_text("#{text.strip}", " " + " " * prefix.length)
    wrapped[0..(1 + prefix.length - 1)] = " #{prefix}"
    puts wrapped
  end
end

main()

