#!/usr/bin/ruby -w
#
# Boy, do I hate parking tickets.
# cron: no-auto-deploy
#

require 'net/smtp'

$from = ARGV[0]
$to = ARGV[1]

if $from.nil? or $to.nil?
  $stderr.puts "Addresses unspecified"
  exit 1
end

day_of_week = `date -d tomorrow +%A`.chomp
day_of_month = `date -d tomorrow +%d`.chomp.to_i

first_week = day_of_month >= 1 && day_of_month <= 7
second_week = day_of_month >= 8 && day_of_month <= 14
third_week = day_of_month >= 15 && day_of_month <= 21
fourth_week = day_of_month >= 22 && day_of_month <= 28

def send_message(side)

  msg = <<END_OF_MESSAGE
From: #{$from}
To: #{$to}
Subject: Street sweeping tomorrow on #{side} side of street

Move your car!
END_OF_MESSAGE

  IO.popen("/usr/sbin/ssmtp -t", mode='r+') do |io|
    io.write msg
    io.close_write
    result = io.read
  end
end


if day_of_week == "Tuesday"
  if first_week || third_week
    send_message("odd")
  end
elsif day_of_week == "Wednesday"
  if second_week || fourth_week
    send_message("even")
  end
end

