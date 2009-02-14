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

    my $line_count = 0;
    my $buf = "";
    my $reader;
    $reader = sub {
        my $handle = shift;
        my $obj = shift;
        my ($type, $value) = map { $obj->{$_} } qw/type value/;

        { no warnings 'uninitialized';
          say "Got message.  Type: $type, value '$value'";
        }

        given($type){
            when('keyPress') {
                my $char = chr $value;
                given($char){
                    when("\n"){
                        push_write($handle, 'normalOutput', "$buf\n");
                        $buf = "";
                        $line_count++;
                    }
                    default {
                        $buf .= chr $value;
                    }
                }

                $handle->push_read( json => $reader );
            }
        }

        if($line_count > 5){
            # fake an exit
            push_write($handle, 'exit', 0);
        }

        $handle->push_read( json => $reader );
    };

    $handle->push_read( json => $reader );
};

sub push_write {
    my ($handle, $t, $v) = @_;
    $handle->push_write( json => {
        type  => $t,
        value => $v,
    });
    $handle->push_write( "\r\n" );
}

exit $exit->recv;
