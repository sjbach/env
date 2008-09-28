#!/usr/bin/perl
#
# Keep running a command every time any file it looks at is changed.
#
# http://conway.rutgers.edu/~ccshan/wiki/blog/posts/Higher-order_shell/keep

use strict;
use Getopt::Std;
use Cwd 'cwd';
use File::Spec;
use Linux::Inotify2;
use Fcntl;

our $opt_d; # Watch for creation, deletion, and movement as well
our $opt_r; # Watch parent directories (up to current directory) as well
our $opt_s; # Watch even paths that were only stat'ed by the program
our $opt_v; # Print paths that triggered each run to standard error
getopts('drsv');

# Compute command-line options to pass to strace
my @strace = qw(strace -e file);
push @strace, qw(-e !stat,lstat,stat64,lstat64) unless $opt_s;

# Compute mask to use for inotify
my $mask = IN_MODIFY | IN_ATTRIB | IN_CLOSE_WRITE;
$mask |= IN_MOVE | IN_CREATE | IN_DELETE | IN_DELETE_SELF | IN_MOVE_SELF
    if $opt_d;

while ()
{
    # Fork strace as a child process, writing trace to a pipe
    pipe(CHILD, PARENT) or die "Cannot create pipe: $!\n";
    my $pid = fork;
    unless ($pid)
    {
        # We're the child, so exec strace
        close CHILD;
        fcntl(PARENT, F_SETFD, fcntl(PARENT, F_GETFD, 0) & ~FD_CLOEXEC);
        exec @strace, "-o/dev/fd/".fileno(PARENT), @ARGV;
    }

    # We're the parent, so read the trace from the child for paths to watch
    close PARENT;
    my @cwd = File::Spec->splitdir(cwd());
    my %path;
    while (<CHILD>)
    {
        my ($call, $path) = /^(\w+)\("((?:[^"\\]|\\.)+)"/ or next;
        my @path = File::Spec->splitdir(File::Spec->canonpath($path));
        @cwd = @path and last if $call eq 'chdir';
        my @root = length $path[0] ? @cwd : shift @path;
        do { $path{File::Spec->catdir(@root, @path)} = undef }
            while $opt_r and defined(pop @path);
    }
    close CHILD or die "Cannot close child: $!\n";
    waitpid($pid, 0);
    exit($? >> 8) if $?;
    %path or die "Deadlock\n";

    # Watch for any change to any path
    my $inotify = new Linux::Inotify2 or die "Cannot create inotify: $!\n";
    my $watch;
    while (defined(my $name = each %path))
    {
        if (defined($inotify->watch($name, $mask)))
        {
	    # print "Watching ", $name, "\n";
            $watch = 1;
        }
        else
        {
	    # print "Not watching ", $name, "\n";
        }
    }
    $watch or die "Cannot watch inotify: $!\n";
    my @events = $inotify->read or die "Cannot read inotify: $!\n";
    if ($opt_v) { print STDERR $_->fullname, "\n" foreach @events }
}

