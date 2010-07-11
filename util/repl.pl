#!/usr/bin/env perl

use strict;
use warnings;
use feature ':5.10';

use Devel::REPL::Script;
use App::Persistent::Server;

# NOTE: Devel::REPL needs its Term::ReadLine to be lazy for this to
# work.  Edit Devel/REPL.pm and make "term" lazy.

# To easily use this, add this to your .bashrc:
#
#    alias repl="pclient +PC --name=repl -PC"
#
# Then "repl" will get you a fresh Devel::REPL session almost
# instantly!

my $repl = Devel::REPL::Script->new_with_options; # load

my $server = App::Persistent::Server->new(
    name => 'repl',
    code => sub {
        $repl->run;
    },
);

$server->start;
exit $server->completion_condvar->recv;

