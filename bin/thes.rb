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

require 'rubygems'
require 'hpricot'
require 'open-uri'

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

# From:
# http://blog.macromates.com/2006/wrapping-text-with-regular-expressions/
def wrap_text(text)
  col = terminal_width()
  text.gsub(/(.{1,#{col}})( +|$)\n?|(.{#{col}})/, "  \\1\\3\n")
end

def terminal_width
  stty_width = %x{stty size}.split[1].to_i - 2
  stty_width < 0 ? 78 : stty_width
end

rd, wr = IO.pipe

if fork()
  wr.close
  $stdin.reopen(rd)
  exec "less"
end

rd.close

term = ARGV[0].gsub(" ", "+")
doc = Hpricot(open("http://thesaurus.reference.com/browse/#{term}"))

doc.search("//table[@class = 'the_content']") do |table|


  table.search("//td[@valign = 'top']") do |e|
    clause = e.innerText

    case clause
    when /Entry/
      wr.puts "Entry: #{e.next_sibling.innerText}"

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

    else
      wr.puts "Unknown::"
      wr.puts clause

    end

  end
  
  wr.puts
end

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

