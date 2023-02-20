#!/usr/bin/perl -Ilib
#-------------------------------------------------------------------------------
# Test Zero Emulator
# Philip R Brenan at appaapps dot com, Appa Apps Ltd Inc., 2023
#-------------------------------------------------------------------------------
use v5.30;
use warnings FATAL => qw(all);
use strict;
use Zero::Emulator;
use Test::More qw(no_plan);

package Zero::Emulator;

my $r = emulate
 ([instruction(action=>'set', source=>[10..19], target=>[0..9]),                #0 Create and load some memory
   instruction(action=>'min', source=>[0..9],   target=>0),                     #1 Minimum
   instruction(action=>'out', source=>[0]),                                     #2 Print
 ]);
Test::More::is_deeply $r->out, [10];
