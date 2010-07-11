#!/usr/bin/env perl

use strict;
use warnings;
use feature ':5.10';

use FindBin qw($Bin);
use lib "$Bin/lib";

use EV;
use App::Persistent::Server;

my $server = App::Persistent::Server->new(
    name => 'less',
    code => sub {
        print "command>";
        my $cmd = <>;
        chomp $cmd;
        print "running $cmd\n";
        exec $cmd;
    },
);

$server->start;
exit $server->completion_condvar->recv;

