#!/usr/bin/env perl

use strict;
use warnings;
use feature ':5.10';

use AnyEvent;
use AnyEvent::Handle;
use AnyEvent::Socket;

my $done = AnyEvent->condvar;

tcp_connect 'localhost', 1235, sub {
    my ($fh) = @_;

    my $h = AnyEvent::Handle->new( fh => $fh );
    $h->push_write( json => { type => 'Exit' } );
    $h->on_drain( sub { $done->send } );
};

$done->wait;
exit 0;
