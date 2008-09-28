#!/usr/bin/perl -w
#
# Command-line prefix to put standard input into a temporary ordinary file
# and use its name as the last argument.
#
# http://conway.rutgers.edu/~ccshan/wiki/blog/posts/Higher-order_shell/tmp

use strict;
use IO::Handle;
use File::Temp 'tempfile';
use File::Copy 'copy';

@ARGV
    or die "tmp: please provide a command to invoke\n";
my ($fh, $tmpname) = tempfile(UNLINK => 1)
    or die "tmp: cannot create temporary file: $!\n";
copy(\*STDIN, $fh)
    or die "tmp: cannot copy standard input to temporary file `$tmpname': $!\n";

my $program = $ARGV[0];
my $status = system $program (@ARGV, $tmpname);
if ($status & 255)
{
    die "tmp: $program terminated abnormally: $?\n";
}
else
{
    my $code = $? >> 8;
    exit $code;
}

