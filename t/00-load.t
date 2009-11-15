#!/usr/bin/env perl

use strict;
use warnings;

use Test::Most 'bail';

BEGIN {
    plan tests => 1;
    use_ok "App::Persistent::Server";
}
