#!/usr/bin/perl -Ilib
#-------------------------------------------------------------------------------
# Test Zero Emulator
# Philip R Brenan at appaapps dot com, Appa Apps Ltd Inc., 2023
#-------------------------------------------------------------------------------
use v5.30;
use warnings FATAL => qw(all);
use strict;
use Zero::Emulator qw(:all);
use Test::More qw(no_plan);

if (1)
 {Start 1;
  Out "hello World";
  ok Execute(out=>["hello World"]);
 }

done_testing;
