#!/usr/bin/perl -I/home/phil/perl/zero/lib/ -Ilib
#-------------------------------------------------------------------------------
# Zero assembler language implemention of a generic N-Way tree.
# Philip R Brenan at appaapps dot com, Appa Apps Ltd Inc., 2023
#-------------------------------------------------------------------------------
use v5.30;
use warnings FATAL => qw(all);
use strict;
use Zero::NWayTree qw(:all);
use Zero::Emulator qw(:all);
use Test::More tests=>3;

if (1)                                                                          # Dimension of the tree
 {my $W = 3; my $N = 15;

  Start 1;
  my $t = New($W);

  For $N, sub                                                                   # Create tree
   {my ($k) = @_;
    my $d = Add $k, $k;
    Insert($t, $k, $d);
   };

  Iterate $t, sub                                                               # Iterate tree
   {my ($find) = @_;                                                            # Find result
    my $k = FindResult_key ($find);
    my $d = FindResult_data($find);
    AssertEq Add($k, $k), $d;
    Out $k;
   };

  my $e = Execute(suppressOutput=>1);                                           # Execute assembler program

  is_deeply $e->out, [0..$N-1];                                                 # Check output

  is_deeply printTreeKeys($e->memory), <<END;                                   # Check keys in memory
           3           7
     1           5           9    11    13
  0     2     4     6     8    10    12    14
END

  is_deeply printTreeData($e->memory), <<END;                                   # Check data in memory
           6          14
     2          10          18    22    26
  0     4     8    12    16    20    24    28
END
 }
