#!/usr/bin/ruby
#
# Thesaurus
#
# Setup:
#  sudo aptitude install libruby ruby-dev rubygems libxml2 libxslt-dev
#  sudo gem install hpricot
#
# Tried to do this using scrubyt, didn't work out.
#

require 'uri'
require 'rubygems'
require 'hpricot'
require 'open-uri'

$stifle_did_you_mean = false

if ARGV[0] == '-q'
  $stifle_did_you_mean = true
  ARGV.shift
end

if ARGV[0].nil?
  $stderr.puts "Usage: thes [-q] <word>"
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
  stty_width = %x{stty size}.split[1].to_i - 2
  stty_width < 0 ? 78 : stty_width
end

def uri_escape(str)
  # Square brackets are not caught as invalid characters...
  URI.escape(str).gsub("[", "%5B") \
                 .gsub("]", "%5D")
end

rd, wr = IO.pipe

if fork()
  wr.close
  $stdin.reopen(rd)
  exec "less"
end

rd.close

term = uri_escape(ARGV[0])
doc = Hpricot(open("http://thesaurus.com/browse/#{term}",
                   "User-Agent" => "Ruby/#{RUBY_VERSION}"))

doc.search("//table[@class = 'the_content']") do |table|

  table.search("//td[@valign = 'top']") do |e|
    clause = e.innerText

    case clause
    when /Entry/
      # The first entry has some extra stuff that we need to avoid
      # printing.
      top_entry = e.next_sibling.search("//span[@id = 'queryn']").first
      if top_entry
        wr.puts "Entry: #{top_entry.innerText.strip}"
      else
        wr.puts "Entry: #{e.next_sibling.innerText.strip}"
      end

    when /Speech/
      e.next_sibling.search("span") do |speech|
        wr.puts "Type: #{speech.innerText}"
      end

    when /Synonyms/
      wr.puts "Synonyms..."

      words = e.next_sibling.innerText.gsub(/\s+/, " ").strip.split(",")
      wr.puts to_terminal_rows(words)

    when /Antonyms/
      wr.puts "Antonyms..."

      words = e.next_sibling.innerText.gsub(/\s+/, " ").strip.split(",")
      wr.puts to_terminal_rows(words)

    when /Definition/
      wr.puts "Definition: #{e.next_sibling.innerText}"

    when /Notes/
      wr.puts "Notes..."
      wr.puts "#{wrap_text(e.next_sibling.innerText)}"

    when /Related/
      wr.puts "Related..."
      wr.puts "#{wrap_text(e.next_sibling.innerText.strip)}"

    when /Concept/
      wr.puts "Concept: #{e.next_sibling.innerText}"

    when /Category/
      wr.puts "Category: #{e.next_sibling.innerText}"

    else
      wr.puts "Unknown::"
      wr.puts clause

    end
  end
  
  wr.puts
end

unless $stifle_did_you_mean
  wr.puts
  wr.puts "-------------------------"
  wr.puts

  doc.search("//div[@class = 'padnearby']") do |nearby|
    wr.puts "Did you mean..."
    words = []
    nearby.search("div/a") do |word|
      words << word.innerText
    end

    wr.puts to_terminal_rows(words)
    wr.puts
  end
end

