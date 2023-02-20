#!/usr/bin/perl -Ilib
#-------------------------------------------------------------------------------
# Test Zero Emulator
# Philip R Brenan at appaapps dot com, Appa Apps Ltd Inc., 2023
#-------------------------------------------------------------------------------
use v5.30;
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use Test::More qw(no_plan);

my $home   = currentDirectory;                                                  # Home folder
my $input  = fpe $home, qw(input txt);                                          # Input file
my $output = fpe $home, qw(output txt);                                         # Output file
