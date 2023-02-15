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

  my $instructionPointer = 0;                                                   # Instruction pointer
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
    compare => sub                                                              # Compare two arrays or one array and a constant and write the result as a mask
     {my ($i) = @_;                                                             # Instruction
      my $s1 = $i->source_1;
      my $s2 = $i->source_2;
      my $t  = $i->target;

      if (isScalar $s2)
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $memory[$$s1[$i]] <=> $s2;
         }
       }
      else
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $memory[$$s1[$i]] <=> $memory[$$s2[$i]];
         }
       }
     },
    jump      => sub                                                            # Jump to the target location
     {my ($i) = @_;                                                             # Instruction
      my $t   = $i->target;
      if (isScalar($t))
       {$instructionPointer = $t;
       }
      else
       {$instructionPointer = $memory[$$t[0]];
       }
     },
    jumpEq    => sub                                                            # Jump to all the target locations for which the corresponding source location is set to zero for equal
     {my ($i) = @_;                                                             # Instruction
      my $s   = $i->source;
      my $t   = $i->target;
      if ($memory[$s] == 0)
       {if (isScalar($t))
         {$instructionPointer = $t;
         }
        else
         {$instructionPointer = $memory[$$t[0]];
         }
       }
     },
    load      => sub                                                            # Load data from the locations addressed by the source array into the target array
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source;
      my $t  = $i->target;

      if (!isScalar $s)                                                         # Load from specified locations
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $memory[$memory[$$s[$i]]];
         }
       }
     },
    move     => sub                                                             # Move data moves data from one part of memory to another - "set", by contrast, sets variables from constant values
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source;
      my $t  = $i->target;

      if (isScalar $s)                                                          # Broadcast location of
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
    out     => sub                                                              # Write source as output to an array of words
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source;
      if (isScalar $s)                                                          # Write a string
       {push @out, $i->source;
       }
      else                                                                      # Write memory locations
       {for my $j(keys @$s)
         {push @out, $memory[$$s[$j]];
         }
       }
     },
    set     => sub                                                              # Place data into memory
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source;
      my $t  = $i->target;

      if (isScalar $s)                                                          # Broadcast one value
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $s;
         }
       }
      else                                                                      # Set multiple values
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $$s[$i];
         }
       }
     },
   );

  for(;;)                                                                       # Each instruction in the code until we hit an undefined instruction
   {my $i = $$code[$instructionPointer++];
    last unless $i;
    if (my $a = $i->action)                                                     # Action
     {$counts{$a}++; $count++;                                                  # Execution counts
      $instructions{$a}->($i);                                                  # Execute instruction
     }
   }

  genHash("Zero::Emulator::Results",                                            # Execution results
    out    => [@out],
    counts => {%counts},
    count  => $count,
    memory => [@memory],
   );
 }

#goto latest;

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

if (1)                                                                          # Load
 {my $r = emulate
   ([instruction(action=>'set',  source=>[1..4], target=>[1..4]),
     instruction(action=>'load', source=>[4],    target=>[3]),
     instruction(action=>'load', source=>[3..4], target=>[1..2]),
     instruction(action=>'out',  source=>[1..4]),
   ]);
  is_deeply $r->out, [(4) x 4];
  is_deeply $r->count,    4;
 }

if (1)                                                                          # 1+2 -> 3
 {my $r = emulate
   ([instruction(action=>'set',  source=>[1,2], target=>[0,1]),
     instruction(action=>'add',  source_1=>[0], source_2=>[1], target=>[0]),
     instruction(action=>'out',  source=>[0]),
   ]);
  is_deeply $r->out->[0], 3;
  is_deeply $r->count,    3;
 }

if (1)                                                                          # For loop with direct jump targets
 {my @i;
  my %label;
  my $r = emulate                             #0 1 2 3 4 5 6
   ([instruction(action=>'set',     source  =>[0,1,3,0,1,6],      target=>[0..6]), #0 Count 1,2,3
     instruction(action=>'add',     source_1=>[0], source_2=>[1], target=>[0]), #1 Increment
     instruction(action=>'out',     source  =>[0]),                             #2 Print
     instruction(action=>'compare', source_1=>[0], source_2=>[2], target=>[3]), #3 Compare result to m[3]
     instruction(action=>'jumpEq',  source  => 3,                 target=> 6),  #4 Goto end of loop
     instruction(action=>'jump',                                  target=> 1),  #5 Restart loop
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,   15;
 }

if (1)                                                                          # For loop with indirect jump targets
 {my @i;
  my %label;
  my $r = emulate                             #0 1 2 3 4 5 6
   ([instruction(action=>'set',     source  =>[0,1,3,0,1,6],      target=>[0..6]), #0 Count 1,2,3
     instruction(action=>'add',     source_1=>[0], source_2=>[1], target=>[0]), #1 Increment
     instruction(action=>'out',     source  =>[0]),                             #2 Print
     instruction(action=>'compare', source_1=>[0], source_2=>[2], target=>[3]), #3 Compare result to m[3]
     instruction(action=>'jumpEq',  source  => 3,                 target=>[5]), #4 m[5] contains location of end of loop
     instruction(action=>'jump',                                  target=>[4]), #5 m[4] contains location of start of loop
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,   15;
 }
