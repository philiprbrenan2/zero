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

sub maximumInstructionsToExecute {100}                                          # Maximum number of subroutines to execute

sub dataStructure(@)                                                            # Describe a data structure
 {my ($name, @fields) = @_;                                                     # Structure name, fields names

  my $d = genHash("Zero::Emulator::DataStructure",                              # Description of a data structure
    name  => $name,                                                             # Name of the structure
    order => [],                                                                # Order of the elements in the structure, in effect, giving the offset of each element in the data structure
    names => {},                                                                # Maps the names of the fields to their offsets in the structure
   );
  $d->field($_) for @fields;                                                    # Add the field descriptions
  $d
 }

sub Zero::Emulator::DataStructure::field($$)                                    # Add a field to a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  if (!$d->names->{$name})
   {$d->names->{$name} = $d->order->@*;
    push $d->order->@*, $name;
   }
  else
   {confess "Duplicate name: $name in structure: ".$d->name;
   }
 }

sub Zero::Emulator::DataStructure::offset($$)                                   # Offset of a field
 {my ($d, $name) = @_;                                                          # Parameters
  $d->names->{$name}
 }

sub callEntry(%)                                                                # Describe an entry on the call stack: the return address, the parameter list length, the parameter list location, the line of code from which the call was made, the file number of the file from which the call was made
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::CallEntry",                                          # Description of a call stack entry
    target   => $options{target},                                               # The location of the subroutine being called
    call     => $options{call},                                                 # The location of the call instruction making the call
    params   => $options{params},                                               # The location at which the parameter list starts in memory. Note that there is no return area - if the subroutine returns values then they go somewhere in the parameter list at locations determined by the called subroutine.
    line     => $options{line},                                                 # The line number from which the call was made
    file     => $options{file},                                                 # The file number from which the call was made - this could be folded into the line number but for reasons best known to themselves people who cannot program very well often scatter projects across several files a practice that is completely pointless in this day of git and so can only lead to chaos and confusion
   );
 }

sub instruction(%)                                                              # Create a new instruction
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::Instruction",                                        # Instruction details
    number => undef,
    %options);
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
  my @calls;                                                                    # Call stack of calls made

  my sub jumpOp($$)                                                             # Jump to the target location if the tested memory area if the condition is matched
   {my ($i, $check) = @_;                                                       # Instruction, check
    my $T = $i->target; my $t  = isScalar($T) ? $T : $memory[$$T[0]];
    $instructionPointer = $t if &$check;                                        # Check if condition is met
   }

  my %instructions =                                                            # Instruction definitions
   (add       => sub                                                            # Add two arrays to make a third array
     {my ($i) = @_;                                                             # Instruction
      my $s1  = $i->source_1;
      my $s2  = $i->source_2;
      my $t   = $i->target;

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

    call => sub                                                                 # Call a subroutine
     {my ($i) = @_;                                                             # Instruction
      my $S = $i->source // 0; my $s = isScalar($S) ? $S : $memory[$S];         # Parameter list
      my $T = $i->target;      my $t = isScalar($T) ? $T : $memory[$T];         # Target subroutine to call

      push @calls,  callEntry(target=>$t, call=>$i->number, params=>$s);
      $instructionPointer = $t;
     },

    parameters => sub                                                           # Locate the parameter list for the current subroutine call
     {my ($i)  = @_;                                                             # Instruction
      @calls or confess "Not in a subroutine";
      my $c = $calls[-1];
      $memory[$i->target] = $c->params;
     },

    return    => sub                                                            # Call a subroutine
     {my ($i) = @_;                                                             # Instruction
      @calls or confess "The call stack is empty so I do not know where to return to";
      my $c = pop @calls;
      $instructionPointer = $c->call+1;
     },

    compare   => sub                                                            # Compare two arrays or one array and a constant and write the result as a mask
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

    inc       => sub                                                            # Increment locations in memory. The first location is incremented by 1, the next by two, etc.
     {my ($i) = @_;                                                             # Instruction
      my $t   = $i->target;

      for my $i(keys @$t)
       {$memory[$$t[$i]] += $i + 1;
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

    jumpEq    => sub                                                            # Conditional jumps
     {my ($i) = @_;
      jumpOp($i, sub{$memory[$i->source] == 0});
     },
    jumpNe    => sub
     {my ($i) = @_;
      jumpOp($i, sub{$memory[$i->source] != 0});
     },
    jumpLe    => sub
     {my ($i) = @_;
      jumpOp($i, sub{$memory[$i->source] <= 0});
     },
    jumpLt    => sub
     {my ($i) = @_;
      jumpOp($i, sub{$memory[$i->source] <  0});
     },
    jumpGe    => sub
     {my ($i) = @_;
      jumpOp($i, sub{$memory[$i->source] <= 0});
     },
    jumpGt    => sub
     {my ($i) = @_;
      jumpOp($i, sub{$memory[$i->source] <  0});
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

    max => sub                                                                  # Maximum element in source block to target
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source;                                                       # Array of locations containing the values to be summed
      my $T = $i->target; my $t = isScalar($T) ? $T : $memory[$T];              # Dereference target if necessary

      my $x; $x = !defined($x) || $x < $memory[$_] ? $memory[$_] : $x for @$s;  # Maximum element
      $memory[$t] = $x;                                                         # Save maximum
     },

    min => sub                                                                  # Minimum element in source block to target
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source;                                                       # Array of locations containing the values to be summed
      my $T = $i->target; my $t = isScalar($T) ? $T : $memory[$T];              # Dereference target if necessary

      my $x; $x = !defined($x) || $x > $memory[$_] ? $memory[$_] : $x for @$s;  # Minimum element
      $memory[$t] = $x;                                                         # Save maximum
     },

    move     => sub                                                             # Move data moves data from one part of memory to another - "set", by contrast, sets variables from constant values
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source;
      my $t  = $i->target;

      if (isScalar $s)                                                          # Broadcast source value
       {for my $i(keys @$t)
         {$memory[$$t[$i]] = $s;
         }
       }
      else
       {for my $i(keys @$t)                                                     # Look up source values in memory
         {$memory[$$t[$i]] = $memory[$$s[$i]];
         }
       }
     },

    moveBlock => sub                                                            # Move a block of data from the first source operand to the target operand.  The length of the move is determined by the second source operand.  The source block and the target block may overlap.
     {my ($i) = @_;                                                             # Instruction
      my $S1 = $i->source_1; my $s1 = isScalar($S1) ? $S1 : $memory[$S1];
      my $S2 = $i->source_2; my $s2 = isScalar($S2) ? $S2 : $memory[$S2];
      my $T  = $i->target;   my $t  = isScalar($T)  ? $T  : $memory[$T];

      my @b;                                                                    # Buffer the data being moved to avoid overwrites
      push @b, $memory[$s1+$_] for 0..$s2-1;
      $memory[$t+$_] = $b[$_]  for 0..$s2-1;
     },

    nop       => sub                                                            # No operation
     {my ($i) = @_;                                                             # Instruction
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

    set     => sub                                                              # Place constant data into memory
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

    shiftBlockLeft => sub                                                       # Move a block of longs referenced by the target operand of length the source operand one long to the left
     {my ($i) = @_;                                                             # Instruction
      my $S = $i->source; my $s = isScalar($S) ? $S : $memory[$S];              # Dereference length if necessary
      my $T = $i->target; my $t = isScalar($T) ? $T : $memory[$T];              # Dereference target if necessary

      for my $i(0..$s-2)                                                        # Move block
       {$memory[$t+$i] = $memory[$t+$i+1];
       }
     },

    shiftBlockRight => sub                                                      # Move a block of longs referenced by the target operand of length the source operand one long to the right
     {my ($i) = @_;                                                             # Instruction
      my $S = $i->source; my $s = isScalar($S) ? $S : $memory[$S];              # Dereference length if necessary
      my $T = $i->target; my $t = isScalar($T) ? $T : $memory[$T];              # Dereference target if necessary

      for my $i(reverse 0..$s-2)                                                # Move block
       {$memory[$t+$i+1] = $memory[$t+$i];
       }
     },

    sum => sub                                                                  # Sum the source block and place it in the target
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source;                                                       # Array of locations containing the values to be summed
      my $T = $i->target; my $t = isScalar($T) ? $T : $memory[$T];              # Dereference target if necessary

      my $x = 0; $x += $memory[$_] for @$s;                                     # Each location whose contents are to be summed
      $memory[$t] = $x;                                                         # Save sum
     },
   );

# Assemble
  my %labels;                                                                   # Load labels

  for my $c(keys @$code)                                                        # Each instruction
   {my $i = $$code[$c];
    $i->number = $c;
    next unless $$i{label};
    if (my $l = $i->label)                                                      # Label
     {$labels{$l} = $c;                                                         # Point label to instruction
     }
   }

  for my $c(keys @$code)                                                        # Each instruction
   {my $i = $$code[$c];
    next unless $i->action =~ m(\A(call|jump))i;
    if (my $l = $i->target)                                                     # Label
     {next unless isScalar($l);                                                 # Not an array
      next if $l =~ m/\A[-+]?\d+\Z/;                                            # Not an integer
      $i->target = $labels{$l};                                                 # Point target keyword to target instruction
     }
   }

# Execute
  for my $j(1..maximumInstructionsToExecute)                                    # Each instruction in the code until we hit an undefined instruction
   {my $i = $$code[$instructionPointer++];
    last unless $i;
    if (my $a = $i->action)                                                     # Action
     {$counts{$a}++; $count++;                                                  # Execution counts
      confess qq(Invalid instruction: "$a"\n) unless my $c = $instructions{$a};
      $c->($i);                                                                 # Execute instruction
     }
    confess "Out of instructions after $j" if $j >= maximumInstructionsToExecute;
   }

  genHash("Zero::Emulator::Results",                                            # Execution results
    out    => [@out],
    counts => {%counts},
    count  => $count,
    labels => {%labels},
    memory => [@memory],
   );
 }

goto latest unless &allTests;

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

if (1)                                                                          # Inc
 {my $r = emulate
   ([instruction(action=>'inc',     target=>[0..2]),
     instruction(action=>'out',     source=>[0..2]),
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,     2;
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
 {my $r = emulate                             #0 1 2 3
   ([instruction(action=>'set',     source  =>[0,1,3,0],       target=>[0..3]), #0 Count 1,2,3
     instruction(action=>'add',     source_1=>[0], source_2=>[1], target=>[0]), #1 Increment at start of loop
     instruction(action=>'out',     source  =>[0]),                             #2 Print
     instruction(action=>'compare', source_1=>[0], source_2=>[2], target=>[3]), #3 Compare result to m[3]
     instruction(action=>'jumpEq',  source  => 3,                 target=> 6),  #4 Goto end of loop
     instruction(action=>'jump',                                  target=> 1),  #5 Restart loop
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,   15;
 }

if (1)                                                                          # For loop with indirect jump targets
 {my $r = emulate                             #0 1 2 3 4 5 6
   ([instruction(action=>'set',     source  =>[0,1,3,0,1,6],   target=>[0..6]), #0 Count 1,2,3
     instruction(action=>'add',     source_1=>[0], source_2=>[1], target=>[0]), #1 Increment at start of loop
     instruction(action=>'out',     source  =>[0]),                             #2 Print
     instruction(action=>'compare', source_1=>[0], source_2=>[2], target=>[3]), #3 Compare result to m[3]
     instruction(action=>'jumpEq',  source  => 3,                 target=>[5]), #4 m[5] contains location of end of loop
     instruction(action=>'jump',                                  target=>[4]), #5 m[4] contains location of start of loop
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,   15;
 }

if (1)                                                                          # For loop with labels
 {my $r = emulate                             #0 1 2 3
   ([instruction(action=>'set',     source  =>[0,1,3,0],   target=>[0..4]),     #0 Count 1,2,3
     instruction(action=>'inc',     target  =>[0], label=>"loop"),              #1 Increment at start of loop
     instruction(action=>'out',     source  =>[0]),                             #2 Print
     instruction(action=>'compare', source_1=>[0], source_2=>[2], target=>[3]), #3 Compare result into m[3]
     instruction(action=>'jumpEq',  source  => 3,  target=>"loopEnd"),          #4 Jump to end of loop at end of loop
     instruction(action=>'jump',                   target=>"loop"),             #5 Restart loop
     instruction(action=>'nop',                    label =>"loopEnd"),          #6 End of loop
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,   16;
 }

if (1)                                                                          # For loop with labels
 {my $r = emulate                             #0 1 2
   ([instruction(action=>'set',     source  =>[0,3,0], target=>[0..2]),         #0 Index, limit, compare
     instruction(action=>'inc',     target  =>[0], label=>"loop"),              #1 Increment at start of loop
     instruction(action=>'out',     source  =>[0]),                             #2 Print
     instruction(action=>'compare', source_1=>[0], source_2=>[1], target=>[2]), #3 Compare result into m[3]
     instruction(action=>'jumpLt',  source  => 2,  target=>"loop"),             #4 Iterate
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,   13;
 }

if (1)                                                                          # For loop with labels
 {my $r = emulate
   ([instruction(action=>'set', source =>[0..3], target=>[0..3]),               #0 Block to move
     instruction(action=>'shiftBlockLeft', source=>3, target =>0),              #1 Shift left
     instruction(action=>'out', source =>[0..3]),                               #2 Print
   ]);
  is_deeply $r->out, [1,2,2,3];
 }

if (1)                                                                          # For loop with labels
 {my $r = emulate
   ([instruction(action=>'set', source =>[0..3], target=>[0..3]),               #0 Block to move
     instruction(action=>'shiftBlockRight', source=>3, target =>0),             #1 Shift left
     instruction(action=>'out', source =>[0..3]),                               #2 Print
   ]);
  is_deeply $r->out, [0,0,1,3];
 }

if (1)                                                                          # For loop with labels
 {my $r = emulate
   ([instruction(action=>'set', source =>[0..3], target=>[0..3]),               #0 Block to move
     instruction(action=>'moveBlock', source_1=>1, source_2=>2, target=>0),     #1 Shift left
     instruction(action=>'out', source =>[0..3]),                               #2 Print
   ]);
  is_deeply $r->out, [1,2,2,3];
 }

if (1)                                                                          # Call a subroutine and return
 {my $r = emulate
   ([instruction(action=>'jump',   target => "end_sub"),                        #0 Jump over subroutine
     instruction(action=>'out',    source => "Hello World", label => "sub"),    #1 Print
     instruction(action=>'return'),                                             #2 Return
     instruction(action=>'nop',    label => "end_sub"),                         #3 End of subroutine
     instruction(action=>'call',   target => "sub"),                            #4 Call subroutine
   ]);
  is_deeply $r->out, ["Hello World"];
 }

if (1)                                                                          # Call a subroutine passing it a parameter
 {my $r = emulate
   ([instruction(action=>'set',    source => [0..9], target => [0..9]),         #0 Create and load some memory
     instruction(action=>'jump',   target => "end_sub"),                        #1 Jump over subroutine
     instruction(action=>'parameters', target => 0, label => "sub"),            #2 Parameters
     instruction(action=>'out',        source => [0]),                          #3 Print
     instruction(action=>'return'),                                             #4 Return
     instruction(action=>'nop',    label  => "end_sub"),                        #5 End of subroutine
     instruction(action=>'call',   source => 2, target => "sub"),               #6 Call subroutine
   ]);
  is_deeply $r->out, [2];
 }

if (1)                                                                          # Sum of a block
 {my $r = emulate
   ([instruction(action=>'set', source => [0..9], target => [0..9]),            #0 Create and load some memory
     instruction(action=>'sum', source => [0..9], target => 0),                 #1 Sum
     instruction(action=>'out', source => [0]),                                 #2 Print
   ]);
  is_deeply $r->out, [45];
 }

if (1)                                                                          # Maximum of a block
 {my $r = emulate
   ([instruction(action=>'set', source => [0..9], target => [0..9]),            #0 Create and load some memory
     instruction(action=>'max', source => [0..9], target => 0),                 #1 Maximum
     instruction(action=>'out', source => [0]),                                 #2 Print
   ]);
  is_deeply $r->out, [9];
 }

latest:;
if (1)                                                                          # Minimum of a block
 {my $r = emulate
   ([instruction(action=>'set', source => [10..19], target => [0..9]),          #0 Create and load some memory
     instruction(action=>'min', source => [0..9],   target => 0),               #1 Minimum
     instruction(action=>'out', source => [0]),                                 #2 Print
   ]);
  is_deeply $r->out, [10];
 }

sub allTests{0 or !-d "/home/phil/"}
