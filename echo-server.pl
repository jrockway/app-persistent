#!/usr/bin/env perl

use strict;
use warnings;
use feature ':5.10';

use AnyEvent;
use AnyEvent::Socket;
use AnyEvent::Handle;

my $exit = AnyEvent->condvar;

my $s = tcp_server undef, 1234, sub {
    my ($fh, $host, $port) = @_;

    my $handle = AnyEvent::Handle->new(
        fh => $fh,
    );

    my $buf = "";
    my $reader;
    $reader = sub {
        my $handle = shift;
        my $obj = shift;
        my ($type, $value) = %$obj;

        { no warnings 'uninitialized';
          use JSON::XS;
          say "Got message.  Type: $type, value:";
          say JSON::XS->new->pretty->allow_nonref->encode($value);
        }

        given($type){
            when('KeyPress') {
                my $char = $value;
                given($char){
                    when("\n"){
                        push_write($handle, 'NormalOutput', "$buf\n");
                        $buf = "";
                    }
                    default {
                        $buf .= $char;
                    }
                }
            }
            when('EndOfFile'){
                push_write($handle, 'NormalOutput', 'Exiting...');
                push_write($handle, 'Exit', 0);
            }
        }

        $handle->push_read( json => $reader );
    };

    $handle->push_read( json => $reader );
};

sub push_write {
    my ($handle, $t, $v) = @_;
    $handle->push_write( json => {
        $t => $v,
    });
    $handle->push_write( "\r\n" );
}

exit $exit->recv;
