#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/
#-------------------------------------------------------------------------------
# Emulate the zero programming language where all instructions operate on arrays
# Philip R Brenan at appaapps dot com, Appa Apps Ltd Inc., 2023
#-------------------------------------------------------------------------------
use v5.30;
package Zero::Emulator;
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use Test::More qw(no_plan);

makeDieConfess;

my $home   = currentDirectory;                                                  # Home folder
my $input  = fpe $home, qw(input txt);                                          # Input file
my $output = fpe $home, qw(output txt);                                         # Output file

sub instruction(%)                                                              # Create a new instruction
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::Instruction", %options);                             # Instruction details
 }

sub type(%)                                                                     # Describe some data
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::Type", %options);                                    # Data type
 }

sub isScalar($)                                                                 # Check whether an element is a scalar or an array
 {my ($value) = @_;                                                             # Parameters
  ! ref $value;
 }

sub emulate($%)                                                                 # Emulate an array of code
 {my ($code, %options) = @_;                                                    # Parameters

  my @out;                                                                      # Output channel
  my %counts;                                                                   # Instruction counts
  my $count;                                                                    # Instruction count
  my @memory;                                                                   # Memory

  my %instructions =
   (add      => sub
     {my ($i) = @_;                                                             # Instruction
      my $s1 = $i->source_1;
      my $s2 = $i->source_2;
      my $t  = $i->target;

      if (isScalar $s2)
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $memory[$$s1[$i]] + $s2;
         }
       }
      else
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $memory[$$s1[$i]] + $memory[$$s2[$i]];
         }
       }
     },
    compare => sub
     {my ($i) = @_;                                                             # Instruction
     },

    move     => sub                                                             # Move data moves data from one part of memory to another - "set", by contrast, sets variables from constant values
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source;
      my $t  = $i->target;

      if (isScalar $s)
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $s;
         }
       }
      else
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $memory[$$s[$i]];
         }
       }
     },
    out     => sub                                                              # Accumulate output as an array of words
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source;
      if (isScalar $s)
       {push @out, $i->source;
       }
      else
       {for my $j(keys @$s)
         {push @out, $memory[$$s[$j]];
         }
       }
     },
    set     => sub                                                              # Set data
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source;
      my $t  = $i->target;

      if (isScalar $s)
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $s;
         }
       }
      else
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $$s[$i];
         }
       }
     },
   );

  for my $instruction(@$code)                                                   # Each instruction in the code
   {if (my $a = $instruction->action)
     {$counts{$a}++; $count++;
      $instructions{$a}->($instruction);
     }
   }
  genHash("Zero::Emulator::Results",
    out    => [@out],
    counts => {%counts},
    count  => $count,
    memory => [@memory],
   );
 }

is_deeply emulate([instruction(action=>'out', source=>"hello World")])->out,    # Hello World
          ["hello World"];

if (0)                                                                          # Move
 {my $r = emulate
   ([instruction(action=>'move', source=>1, target=>[0..2]),
     instruction(action=>'out',  source=>[0..1]),
   ]);
  is_deeply $r->out, [1,1];
 }

if (1)                                                                          # Move
 {my $r = emulate
   ([instruction(action=>'set',  source=>[1,2], target=>[0,1]),
     instruction(action=>'move', source=>[1],   target=>[0]),
     instruction(action=>'out',  source=>[0]),
   ]);
  is_deeply $r->out->[0], 2;
  is_deeply $r->count,    3;
 }

if (1)                                                                          # 1+2 -> 3
 {my $r = emulate
   ([instruction(action=>'set', source=>[1,2], target=>[0,1]),
     instruction(action=>'add',  source_1=>[0], source_2=>[1], target=>[0]),
     instruction(action=>'out',  source=>[0]),
   ]);
  is_deeply $r->out->[0], 3;
  is_deeply $r->count,    3;
 }
