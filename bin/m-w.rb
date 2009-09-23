#!/usr/bin/ruby
#
# Merriam-Webster.com scraper
#
# Works okay on the 377 words I tried
#
# (Really, really hacky and as soon they change their html slightly
# everything will break.)
#
# Setup:
#  sudo aptitude install libruby ruby-dev rubygems libxml2 libxslt-dev
#  sudo gem install hpricot
#

# TODO:
#  - order of modifier, letter for test[1]
#

require 'uri'
require 'rubygems'
require 'hpricot'
require 'open-uri'

$debug = nil

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

  if entries.empty?
    parse_entry(doc)
  else
    entries.each do |entry|
      url = uri_escape("http://www.merriam-webster.com#{entry}")
      parse_entry(Hpricot(open(url)))
      puts
    end
  end
end

module Hpricot
  class Elem
    def next_nonempty_node
      node = self.next_node
      while node && node.class != Hpricot::Elem && node.inner_text =~ /^\s*$/
        node = node.next_node
      end

      return node
    end

    def previous_nonempty_node
      node = self.previous_node
      while node && node.class != Hpricot::Elem && node.inner_text =~ /^\s*$/
        node = node.previous_node
      end

      return node
    end
  end
end

# Debug print
def d(str)
  if $debug
    $stderr.puts str
  end
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

def parse_entry(doc)
  doc.search("//div[@id = 'mwEntryData']") do |div|
    # Leading headers
    div.search("//ul/li") do |li|
      text = li.inner_text.strip.sub("\t", " ").squeeze(" ")
      case text
      when /^Pronunciation/
        # skip -- I'd rather hear it
      when /Etymology/
        puts "Etymology..."
        content = text.sub(/^Etymology:\s+/, "")
        puts wrap_text(content, " ")
      else
        puts text
      end
    end

    div.search("p[@class = 'd']") do |p|
      #
      # Separators are in <strong> -- these don't follow a neat
      # list/indentation structure that translates well to plain
      # text, so we need to make one up via an adhoc token stream.
      #
      p.search("strong") do |strong|
        content = strong.inner_text.strip
        # This case statement is repeated below, kind-of
        case content
        when /^(\d+)\s*([[:alpha:]])$/  # "2 a"
          strong.swap("<strong>#!#{$1}-#{$2}!#</strong>")
        when /^(\d+)$/  # "2"
          strong.swap("<strong>#!#{$1}!#</strong>")
        when /^([[:alpha:]])$/
          strong.swap("<strong>#!#{$1}!#</strong>")
        when /^:$/
          strong.swap("<strong>#!:!#</strong>")
        when /synonyms/
          strong.swap("<strong>#!SYNONYMS!#</strong>")
        when /usage/
          strong.swap("<strong>#!USAGE!#</strong>")
        else
          d("Unknown def. separator: #{content}")
        end
      end

      # Sometimes verbs are separated into e.g. transitive and
      # intransitive sections
      p.search("em[@class = 'v']") do |em|
        em.swap("#!TYPE #{em.inner_text.strip}!#")
      end

      # em@uni is for unicode characters -- sometimes confuses
      # the parser
      # STEVE
#      p.search("em[@class = 'uni']") do |em|
#        d("removing: #{em.inner_text}")
#        em.swap("")
#      end

      # Catch e.g. "archaic"
      p.search("em") do |em|
        if em.at("..").name != "span"  # actually, span w/ class=vi
          content = em.inner_text.strip
          next_node = em.next_nonempty_node
          prev_node = em.previous_nonempty_node
          if ((next_node.class == Hpricot::Elem && next_node.name = "strong") || \
              (prev_node.class == Hpricot::Elem && prev_node.name = "strong"))
            content = em.inner_text.strip
            em.swap("#!MODIFIER #{content}!#")
          else
            d("modifier: #{content}?")
          end
        end
      end

      # Matches a sequence of "#!something!#"
      lines = p.inner_text.gsub(/(:?#![^!]*!#\s*)+/, "\n\\&")

      reformatted = []
      type = " "
      number = " "
      letter = " "
      colon = " "
      modifier = " "
      lines.each do |line|
        if line =~ /((?:#![^!]*!#\s*)+)(.*)/
          tokens = $1
          content = $2
          tokens.split(/!#/).each do |token|
            token = token.strip.sub(/^#!/,'')
            case token
            when /^TYPE (.*)/
              type = "#{$1} "
              number = " "
              letter = " "
              colon = " "
              modifier = " "
            when /^(\d+)-([[:alpha:]])$/  # e.g. "1 a"
              number = $1
              letter = $2
              colon = " "
              modifier = ""
            when /^(\d+)$/  # e.g. "2"
              number = $1
              letter = " "
              colon = " "
              modifier = ""
            when /^([[:alpha:]])$/  # e.g. "b"
              letter = $1
              modifier = ""   # STEVE: is this always the right thing?
            when /^:$/
              colon = ":"
            when /^MODIFIER (.*)/
              modifier += "#{$1} "
            when /^SYNONYMS/
              number = " "
              letter = " "
              modifier = "Synonyms"
              colon = ":"
            when /^USAGE/
              number = " "
              letter = " "
              modifier = "Usage"
              colon = ":"
            when /^\s*$/
              nil
            else
              d("unknown token: #{token}")
            end
          end
          lead = " #{type} #{number} #{modifier} #{letter} #{colon} ".squeeze(" ")
          indent = " " * lead.length
          content_wrapped = wrap_text(content, indent)
          reformatted << (lead + content_wrapped[lead.length..content_wrapped.length])

        elsif line !~ /^\s*$/
          # Assume this is okay -- usually a note on a proper noun
          d("no match: <<#{line}>>")
          puts wrap_text(line.strip, " ")
        end
      end

      reformatted.each do |definition|
        puts definition
      end
    end

    # Different forms of the word, e.g. adverb or noun for a verb
    div.search("p[@class = 'r']") do |p|
      puts wrap_text(p.inner_text, "")
    end
  end
end

main()

