#!/usr/bin/ruby -w
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

entries = []

def to_terminal_rows(words)
  rows = []
  rows << words.inject do |x,y|
    if (x + ', ' + y).length < 80
      x + ', ' + y
    else
      rows << x
      y.strip
    end
  end

  rows.join("\n")
end

term = ARGV[0].gsub(" ", "+")
doc = Hpricot(open("http://thesaurus.reference.com/browse/#{term}"))

doc.search("//div[@class = 'padnearby']") do |nearby|
  puts "Did you mean..."
  words = []
  nearby.search("div/a") do |word|
    words << word.innerText
  end

  puts to_terminal_rows(words)
  puts
end

doc.search("//table[@class = 'the_content']") do |table|


  table.search("//td[@valign = 'top']") do |e|

    case e.search("b/text()").to_s
    when /Entry/
      puts "Entry: #{e.next_sibling.innerText}"

    when /Speech/
      e.next_sibling.search("span") do |speech|
        puts "Type: #{speech.innerText}"
      end

    when /Synonyms/
      puts "Synonyms..."

      words = e.next_sibling.innerText.gsub(/\s+/, " ").strip.split(",")
      puts to_terminal_rows(words.map{ |w| w.strip })

    when /Antonyms/
      puts "Antonyms..."

      words = e.next_sibling.innerText.gsub(/\s+/, " ").strip.split(",")
      puts to_terminal_rows(words.map{ |w| w.strip })

    when /Definition/
      puts "Definition: #{e.next_sibling.innerText}"

    else
      puts "Unknown::"
      puts e.search("b/text()").to_s

    end

  end
  
  puts
end

