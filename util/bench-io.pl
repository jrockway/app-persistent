#!/usr/bin/env perl

use strict;
use warnings;
use feature ':5.10';

use Time::HiRes qw(time);

open my $fh, '-|', @ARGV or die "Failed to exec @{ARGV}: $!";
my $start = time;
sysread $fh, my $buf, 1;
my $first = time;
say {*STDERR} "Time to first byte: ", $first-$start;
print $buf;
while( sysread $fh, my $buf, 8192 ){
    print $buf;
}
my $last = time;
say {*STDERR} "Time till last: ", $last-$start;
say {*STDERR} "Running time: ", $last-$first;
