#!/usr/bin/env perl

use strict;
use warnings;
use feature ':5.10';

use FindBin qw($Bin);
use lib "$Bin/lib";

use EV;
use App::Persistent::Server;

my $server = App::Persistent::Server->new(
    code => sub {
        use DDS;

        say Dump(\@_);
        say "HELLO from $$";

        local $| = 1;
        use Term::ReadLine;
        my $term = Term::ReadLine->new('pserver', \*STDIN, \*STDOUT);

        while(my $line = $term->readline('> ')){
            print "You said: $line\n";
        }

        exit 0;
    },
);

$server->start;
exit $server->completion_condvar->recv;

