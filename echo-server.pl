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
        my ($type, $value) = map { $obj->{$_} } qw/type value/;

        { no warnings 'uninitialized';
          say "Got message.  Type: $type, value '$value'";
        }

        given($type){
            when('exit'){
                $exit->send(0);
            }
            when('keyPress') {
                my $char = chr $value;
                given($char){
                    when("\n"){
                        $handle->push_write( json => {
                            type  => 'normalOutput',
                            value => "$buf\n",
                        });
                        $handle->push_write( "\r\n" );
                        $buf = "";
                    }
                    default {
                        $buf .= chr $value;
                    }
                }

                $handle->push_read( json => $reader );
            }
            default {
                $handle->push_write( json => {
                    type  => 'errorOutput',
                    value => 'unknown command',
                });
                $handle->push_write( "\r\n" );
                $handle->push_read( json => $reader );
            }
        }
    };

    $handle->push_read( json => $reader );
};

exit $exit->recv;
