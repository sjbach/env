#!/usr/bin/ruby1.9.1
#
# Thesaurus.com scraper
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

$rd, $wr = IO.pipe

if fork()
  $wr.close
  $stdin.reopen($rd)
  exec "less"
end

$rd.close

def main
  term = uri_escape(ARGV[0])
  doc = Nokogiri::HTML(
    open("http://thesaurus.com/browse/#{term}"))

  # "Synonims" -- irony?
  doc.css("div.synonims").each do |div|
    $wr.puts "Entry: #{div.at_css('strong.ttl').inner_text.strip} (#{div.at_css('em.txt').inner_text.strip})"
    $wr.puts 'Synonyms...'
    $wr.puts to_terminal_rows(div.css('.relevancy-list span.text').map { |el| el.inner_text.strip })
    $wr.puts 'Antonyms...'
    $wr.puts to_terminal_rows(div.css('.antonyms span.text').map { |el| el.inner_text.strip })
    $wr.puts
  end

  doc.css("div.syn_of_syns").each do |div|
    $wr.puts "Entry: #{div.at_css('.subtitle a').inner_text.strip} (#{div.at_css('.def').inner_text.strip})"
    $wr.puts 'Synonyms...'
    $wr.puts to_terminal_rows(div.css('li a').map { |el| el.inner_text })
    # No Antonyms available in content. :-(
    $wr.puts
  end


  if !doc.css('div#example-sentences p').empty?
    $wr.puts 'Example Sentences:'
    doc.css("div#example-sentences p").each do |p|
      $wr.puts wrap_text(p.inner_text.strip).sub(/^  /,"- ")
    end
    $wr.puts
  end

  if !doc.css('div#word-origin p').empty?
    $wr.puts 'Word Origin & History:'
    doc.css("div#word-origin p").each do |p|
      $wr.puts wrap_text(p.inner_text.strip.gsub(/\s+/, ' ')).sub(/^  /,"- ")
    end
  end


end

main()

