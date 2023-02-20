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

sub maximumInstructionsToExecute {20}                                           # Maximum number of subroutines to execute

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
    number        => undef,
    owner         => undef,
    source        => undef,
    source_area   => undef,
    source_1      => undef,
    source_2      => undef,
    source_1_area => undef,
    source_2_area => undef,
    target        => undef,
    target_area   => undef,
    %options);
 }

sub isScalar($)                                                                 # Check whether an element is a scalar or an array
 {my ($value) = @_;                                                             # Parameters
  ! ref $value;
 }

sub code(%)                                                                     # A block of code
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::Code",                                               # Description of a call stack entry
    assembled => undef,                                                         # Needs to be assembled unless this field is true
    code      => [],                                                            # An array of instructions
    labels    => {},                                                            # Label name to instruction
    files     => [],                                                            # File number to file name
    %options,
   );
 }


sub emulate($%)                                                                 # Emulate an array of code
 {my ($code, %options) = @_;                                                    # Block of code, options

  my $c = code(code => $code);
  my $r = $c->execute(%options);
  $r
 }

sub Zero::Emulator::Code::assemble($%)                                          # Assemble a block of code to prepare it for execution
 {my ($Code, %options) = @_;                                                    # Code block, assembly options
  return $Code if $Code->assembled;                                             # Already assembled

  my $code = $Code->code;                                                       # The code to be assembled
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
      $i->target = $labels{$l} - $c;                                            # Relative jump
     }
   }
  $Code->labels = {%labels};                                                    # Labels created during assembly
  $Code->assembled = time;                                                      # Time of assembly
  $Code
 }

sub Zero::Emulator::Code::execute($%)                                           # Execute a block of code
 {my ($Code, %options) = @_;                                                    # Block of code, execution options
  $Code->assemble;                                                              # Assemble if necessary
  my $code = $Code->code;

  my $instructionPointer = 0;                                                   # Instruction pointer
  my @out;                                                                      # Output channel
  my %counts;                                                                   # Instruction counts
  my $count;                                                                    # Instruction count
  my @calls;                                                                    # Call stack of calls made
  my %memory;                                                                   # Memory
  my %owner;                                                                    # Who owns the privilege of writing to the corresponding block of memory. undef means that any-one can write, otherwise the instruction must have the matching owner id

  my sub getMemory($$)                                                          # Get a memory location from a memory arena
   {my ($area, $at) = @_;                                                       # Memory arena, memory location
    $memory{$area}[$at]                                                         # Value
   }

  my sub setMemory($$$$)                                                        # Set a memory location checking that the instruction owns the memory concerned
   {my ($i, $area, $at, $value) = @_;                                           # Instruction, memory arena, memory location, value to set
    if (my $o = $owner{$area}[$at])
     {if (my $O = $i->owner)
       {if ($o == $O)
         {$memory{$area}[$at] = $value;                                         # Memory owned by this instruction
          say STDERR sprintf "%4d:%4d = %d", $area, $at, $value if $options{trace};
         }
        else
         {confess "Owner mismatch memory: $area:$at, wanted: $o, got: $O\n";
         }
       }
      else
       {confess "Memory $area:$at owned with key: $o but no key provided\n";
       }
     }
    else                                                                        # Memory is open
     {my $v = $memory{$area}[$at] = $value;                                     # Set value
      my $o = $owner {$area}[$at] = $i->owner // 0;                             # Take ownership if an ownership key is supplied
      say STDERR sprintf "%4d:%4d:%4d = %d", $o, $area, $at, $v if $options{trace};
     }
   }

  my sub sourceValue($)                                                         # The value of the source operand interpreting a scalar as a direct reference and an array reference as an indirect reference
   {my ($i) = @_;                                                               # Instruction
    my $s = $i->source;
    my $a = $i->source_area // 0;
    return ($s, $a) if isScalar $s;                                             # (value, memory block)
    (getMemory($a, $$s[0]), $a);
   }

  my sub targetValue($)                                                         # The value of the target operand interpreting a scalar as a direct reference and an array reference as an indirect reference
   {my ($i) = @_;                                                               # Instruction
    my $t = $i->target;
    my $a = $i->target_area // 0;
    return ($t, $a) if isScalar $t;                                             # (value, memory block)
    (getMemory($a, $$t[0]), $a);
   }

  my sub jumpOp($$)                                                             # Jump to the target location if the tested memory area if the condition is matched
   {my ($i, $check) = @_;                                                       # Instruction, check
    my ($t, $ta) = targetValue($i);
    $instructionPointer = $i->number + $t if &$check;                           # Check if condition is met
   }

  my %instructions =                                                            # Instruction definitions
   (add       => sub                                                            # Add two arrays to make a third array
     {my ($i) = @_;                                                             # Instruction
      my $s1  = $i->source_1; my $sa1 = $i->source_1_area // 0;
      my $s2  = $i->source_2; my $sa2 = $i->source_2_area // 0;
      my $t   = $i->target;   my $ta  = $i->target_area   // 0;

      if (isScalar $s2)
       {for my $j(keys @$t)
         {setMemory($i, $ta, $$t[$j], getMemory($sa1, $$s1[$j]) + $s2);
         }
       }
      else
       {for my $j(keys @$t)
         {setMemory($i, $ta, $$t[$j], getMemory($sa1, $$s1[$j]) +
                                      getMemory($sa2, $$s2[$j]));
         }
       }
     },

    call => sub                                                                 # Call a subroutine
     {my ($i) = @_;                                                             # Instruction
      my ($s) = sourceValue($i);                                                # Parameter list location
      my ($t) = targetValue($i);                                                # Subroutine to call

      if (isScalar($i->target))
       {$instructionPointer = $i->number + $t;                                  # Relative call if we know where the subroutine is relative to the call instruction
       }
      else
       {$instructionPointer = $t;                                               # Absolute call
       }
      my $ti = $code->[$instructionPointer];
      push @calls, callEntry(target=>$ti, call=>$i, params=>$s);
     },

    parameters => sub                                                           # Locate the parameter list for the current subroutine call
     {my ($i)  = @_;                                                            # Instruction
      @calls or confess "Not in a subroutine";
      my $c = $calls[-1];
      setMemory($i, $i->target_area//0, $i->target, $c->params);
     },

    return    => sub                                                            # Call a subroutine
     {my ($i) = @_;                                                             # Instruction
      @calls or confess "The call stack is empty so I do not know where to return to";
      my $c = pop @calls;
      $instructionPointer = $c->call+1;
     },

    compare   => sub                                                            # Compare two arrays or one array and a constant and write the result as a mask
     {my ($i) = @_;                                                             # Instruction
      my $s1 = $i->source_1; my $sa1 = $i->source_1_area // 0;
      my $s2 = $i->source_2; my $sa2 = $i->source_2_area // 0;
      my $t  = $i->target;   my $ta  = $i->target_area   // 0;

      if (isScalar $s2)
       {for my $j(keys @$t)
         {setMemory($i, $ta, $$t[$j], getMemory($sa1, $$s1[$j]) <=> $s2);
         }
       }
      else
       {for my $j(keys @$t)
         {setMemory($i, $ta, $$t[$j], getMemory($sa1, $$s1[$j]) <=>
                                      getMemory($sa2, $$s2[$j]));
         }
       }
     },

    confess => sub                                                              # Print the current call stack and stop
     {my ($i) = @_;                                                             # Instruction
      push @out, "Stack trace";
      for my $j(reverse keys @calls)
       {my $c = $calls[$j];
        my $i = $c->call;
        my $l = $i->label // '';
        my $n = $i->number;
        my $t = $c->target;
        my $L = $t->label // '';
        my $N = $t->number;
        push @out, sprintf "%4d %4d->%4d  %12s->%12s", $j+1, $n, $N, $l, $L;
       }
      $instructionPointer = @$code;                                             # Execution terminates as soon as undefined instuction is encountered
     },

    inc       => sub                                                            # Increment locations in memory. The first location is incremented by 1, the next by two, etc.
     {my ($i) = @_;                                                             # Instruction
      my $t   = $i->target; my $ta = $i->target_area // 0;

      for my $j(keys @$t)
       {setMemory($i, $ta, $$t[$j], getMemory($ta, $$t[$j]) + $j + 1);
       }
     },

    jump      => sub                                                            # Jump to the target location
     {my ($i) = @_;                                                             # Instruction
      my ($t, $ta) = targetValue($i);
      if (isScalar($t))
       {$instructionPointer = $i->number + $t;
       }
      else
       {$instructionPointer = getMemory($ta, $t);
       }
     },

    jumpEq => sub {my ($i) = @_; jumpOp($i, sub{(sourceValue($i))[0] == 0})},   # Conditional jumps
    jumpNe => sub {my ($i) = @_; jumpOp($i, sub{(sourceValue($i))[0] != 0})},
    jumpLe => sub {my ($i) = @_; jumpOp($i, sub{(sourceValue($i))[0] <= 0})},
    jumpLt => sub {my ($i) = @_; jumpOp($i, sub{(sourceValue($i))[0] <  0})},
    jumpGe => sub {my ($i) = @_; jumpOp($i, sub{(sourceValue($i))[0] <= 0})},
    jumpGt => sub {my ($i) = @_; jumpOp($i, sub{(sourceValue($i))[0] <  0})},

    load      => sub                                                            # Load data from the locations addressed by the source array into the target array
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source; my $sa = $i->source_area // 0;
      my $t = $i->target; my $ta = $i->target_area // 0;

      if (!isScalar $s)                                                         # Load from specified locations
       {for my $j(keys @$t)
         {setMemory($i, $ta, $$t[$j], getMemory($sa, getMemory($sa, $$s[$j])));
         }
       }
     },

    max => sub                                                                  # Maximum element in source block to target
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source; my $sa = $i->source_area//0;                         # Array of locations containing the values to be summed
      my ($t, $ta) = targetValue($i);

      my $x;                                                                    # Maximum element
      for my $a(@$s)
       {my $S = getMemory($sa, $a);
        if (!defined($x) || $x < $S)
         {$x = $S;
         }
       }
      setMemory($i, $ta, $t, $x);                                               # Save maximum
     },

    memorySize => sub                                                           # Set the target location to the size of the memory area described by the source operand.
     {my ($i) = @_;                                                             # Instruction
      my ($s)      = sourceValue($i);                                           # Number of memory area
      my ($t, $ta) = targetValue($i);                                           # Set target to length of memory area
      setMemory($i, $ta, $t, scalar $memory{$s}->@*);
     },

    min => sub                                                                  # Minimum element in source block to target
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source; my $sa = $i->source_area//0;                         # Array of locations containing the values to be summed
      my ($t, $ta) = targetValue($i);

      my $x;                                                                    # Minimum element
      for my $a(@$s)
       {my $S = getMemory($sa, $a);
        if (!defined($x) || $x > $S)
         {$x = $S;
         }
       }
      setMemory($i, $ta, $t, $x);                                               # Save minimum
     },

    move     => sub                                                             # Move data moves data from one part of memory to another - "set", by contrast, sets variables from constant values
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source; my $sa = $i->source_area // 0;
      my $t = $i->target; my $ta = $i->target_area // 0;

      if (isScalar $s)                                                          # Broadcast source value
       {for my $i(keys @$t)
         {setMemory($i, $ta, $$t[$i], $s);
         }
       }
      else
       {for my $j(keys @$t)                                                     # Look up source values in memory
         {setMemory($i, $ta, $$t[$j], getMemory($sa, $$s[$j]));
         }
       }
     },

    moveBlock => sub                                                            # Move a block of data from the first source operand to the target operand.  The length of the move is determined by the second source operand.  The source block and the target block may overlap.
     {my ($i) = @_;                                                             # Instruction
      my $S1 = $i->source_1; my $sa1 = $i->source_1_area // 0; my $s1 = isScalar($S1) ? $S1 : getMemory($sa1, $S1);
      my $S2 = $i->source_2; my $sa2 = $i->source_2_area // 0; my $s2 = isScalar($S2) ? $S2 : getMemory($sa2, $S2);
      my ($t, $ta) = targetValue($i);

      my @b;                                                                    # Buffer the data being moved to avoid overwrites
      push @b, getMemory($sa1, $s1+$_)  for 0..$s2-1;
      setMemory($i, $ta, $t+$_, $b[$_]) for 0..$s2-1;
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
         {push @out, getMemory($i->source_area//0, $$s[$j]);
         }
       }
     },

    ownerClear => sub                                                           # Clear ownership for a range of memory. The target designates the location at which to start, source gives th length of the clear.  The ownership of the memory in this range is reset to zero so that any-one can claim it.
     {my ($i) = @_;                                                             # Instruction
      my ($s) = sourceValue($i);
      my ($t, $ta) = targetValue($i);
      for my $j(0..$s-1)                                                        # Move block
       {$owner{$ta}[$t + $j] = 0;                                               # Zero the ownership of the memory in the range
       }
     },

    set     => sub                                                              # Place constant data into memory
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source;
      my $t = $i->target; my $ta = $i->target_area // 0;                        # Target of set

      if (isScalar $s)                                                          # Broadcast one value
       {for my $j(keys @$t)
         {setMemory($i, $ta, $$t[$j], $s);
         }
       }
      else                                                                      # Set multiple values
       {for my $j(keys @$t)
         {setMemory($i, $ta, $$t[$j], $$s[$j]);
         }
       }
     },

    shiftBlockLeft => sub                                                       # Move a block of longs referenced by the target operand of length the source operand one long to the left
     {my ($i) = @_;                                                             # Instruction
      my ($s) = sourceValue($i);
      my $T = $i->target; my $ta = $i->target_area // 0; my $t = isScalar($T) ? $T : getMemory($ta, $T);

      for my $j(0..$s-2)                                                        # Move block
       {setMemory($i, $ta, $t+$j, getMemory($ta, $t+$j+1));
       }
     },

    shiftBlockRight => sub                                                      # Move a block of longs referenced by the target operand of length the source operand one long to the left
     {my ($i) = @_;                                                             # Instruction
      my ($s) = sourceValue($i);
      my $T = $i->target; my $ta = $i->target_area // 0; my $t = isScalar($T) ? $T : getMemory($ta, $T);

      for my $j(reverse 0..$s-2)                                                # Move block
       {setMemory($i, $ta, $t+$j+1, getMemory($ta, $t+$j));
       }
     },

    sum => sub                                                                  # Sum the source block and place it in the target
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source; my $sa = $i->source_area // 0;
      my ($t, $ta) = targetValue($i);

      my $x = 0; $x += getMemory($sa, $_) for @$s;                              # Each location whose contents are to be summed
      setMemory($i, $ta, $t, $x);                                               # Save sum
     },
   );

  for my $j(1..maximumInstructionsToExecute)                                    # Each instruction in the code until we hit an undefined instruction
   {my $i = $$code[$instructionPointer++];
    last unless $i;
    if (my $a = $i->action)                                                     # Action
     {$counts{$a}++; $count++;                                                  # Execution counts
      confess qq(Invalid instruction: "$a"\n) unless my $c = $instructions{$a};
      say STDERR sprintf "%4d  %4d  %12s", $j, $i->number, $i->action if $options{trace};
      $c->($i);                                                                 # Execute instruction
     }
    confess "Out of instructions after $j" if $j >= maximumInstructionsToExecute;
   }

  genHash("Zero::Emulator::Execution",                                          # Execution results
    code   => $code,                                                            # Code executed
    count  => $count,                                                           # Executed instructions count
    counts => {%counts},                                                        # Executed instructions by name counts
    memory => {%memory},                                                        # Memory contents at the end of execution
    out    => [@out],                                                           # The out channel
    owner  => {%owner},                                                         # Memory ownership
   );
 }

eval {goto latest};

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
   ([instruction(action=>'set', source=>0, target=>[0..2]),
     instruction(action=>'inc', target=>[0..2]),
     instruction(action=>'out', source=>[0..2]),
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,    3;
 }

if (1)                                                                          # 1+2 -> 3
 {my $r = emulate
   ([instruction(action=>'set', source=>[1,2], target=>[0,1]),
     instruction(action=>'add', source_1=>[0], source_2=>[1], target=>[0]),
     instruction(action=>'out', source=>[0]),
   ]);
  is_deeply $r->out->[0], 3;
  is_deeply $r->count,    3;
 }

if (1)                                                                          # For loop with direct jump targets
 {my $r = emulate                             #0 1 2 3
   ([instruction(action=>'set',     source  =>[0,1,3,0], target=>[0..3]),       #0 Count 1,2,3
     instruction(action=>'nop',     label=>'start'),                            #1 Start of loop
     instruction(action=>'add',     source_1=>[0], source_2=>[1], target=>[0]), #2 Increment at start of loop
     instruction(action=>'out',     source  =>[0]),                             #3 Print
     instruction(action=>'compare', source_1=>[0], source_2=>[2], target=>[3]), #4 Compare result to m[3]
     instruction(action=>'jumpEq',  source  =>[3], target=>"end"),              #5 Goto end of loop
     instruction(action=>'jump',                   target=>"start"),            #6 Restart loop
     instruction(action=>'nop',     label=>'end'),                              #7 End
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,   19;
 }

if (1)                                                                          # For loop with indirect jump targets
 {my $r = emulate                             #0 1 2 3 4 5 6
   ([instruction(action=>'set',     source  =>[0,1,3,0,-4,+2],    target=>[0..5]), #0 Count 1,2,3
     instruction(action=>'add',     source_1=>[0], source_2=>[1], target=>[0]), #1 Increment at start of loop
     instruction(action=>'out',     source  =>[0]),                             #2 Print
     instruction(action=>'compare', source_1=>[0], source_2=>[2], target=>[3]), #3 Compare result to m[3]
     instruction(action=>'jumpEq',  source  =>[3],                target=>[5]), #4 m[5] contains location of end of loop
     instruction(action=>'jump',                                  target=>[4]), #5 m[4] contains location of start of loop
   ], trace=>0);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,   15;
 }

if (1)                                                                          # For loop with labels
 {my $r = emulate                             #0 1 2 3
   ([instruction(action=>'set',     source  =>[0,1,3,0],   target=>[0..4]),     #0 Count 1,2,3
     instruction(action=>'inc',     target  =>[0], label=>"loop"),              #1 Increment at start of loop
     instruction(action=>'out',     source  =>[0]),                             #2 Print
     instruction(action=>'compare', source_1=>[0], source_2=>[2], target=>[3]), #3 Compare result into m[3]
     instruction(action=>'jumpEq',  source  =>[3], target=>"loopEnd"),          #4 Jump to end of loop at end of loop
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
     instruction(action=>'jumpLt',  source  =>[2],  target=>"loop"),            #4 Iterate
   ]);
  is_deeply $r->out, [1,2,3];
  is_deeply $r->count,   13;
 }

if (1)                                                                          # For loop with labels
 {my $r = emulate
   ([instruction(action=>'set',            source =>[0..3], target=>[0..3]),    #0 Block to move
     instruction(action=>'shiftBlockLeft', source => 3,     target=>0),         #1 Shift left
     instruction(action=>'out',            source =>[0..3]),                    #2 Print
   ]);
  is_deeply $r->out, [1,2,2,3];
 }

if (1)                                                                          # For loop with labels
 {my $r = emulate
   ([instruction(action=>'set',             source =>[0..3], target=>[0..3]),   #0 Block to move
     instruction(action=>'shiftBlockRight', source => 3,     target=>0),        #1 Shift left
     instruction(action=>'out',             source =>[0..3]),                   #2 Print
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

if (1)                                                                          # Indirect call to an absolute address
 {my $r = emulate
   ([instruction(action=>'set',    source => [0..9], target => [0..9]),         #0 Create and load some memory
     instruction(action=>'jump',   target => "end_sub"),                        #1 Jump over subroutine
     instruction(action=>'parameters', target => 0, label => "sub"),            #2 Parameters
     instruction(action=>'out',        source => [0]),                          #3 Print
     instruction(action=>'return'),                                             #4 Return
     instruction(action=>'nop',    label  => "end_sub"),                        #5 End of subroutine
     instruction(action=>'call',   source => 2, target => [2]),                 #6 Call subroutine  indirectly
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

if (1)                                                                          # Minimum of a block
 {my $r = emulate
   ([instruction(action=>'set', source => [10..19], target => [0..9]),          #0 Create and load some memory
     instruction(action=>'min', source => [0..9],   target => 0),               #1 Minimum
     instruction(action=>'out', source => [0]),                                 #2 Print
   ]);
  is_deeply $r->out, [10];
 }

if (1)                                                                          # Ownership of memory
 {my $r = eval {emulate
   ([instruction(action=>'set', source => [10..19], owner=>1, target => [0..9]),#0 Create and load some memory with for one owner
     instruction(action=>'min', source => [0..9],   owner=>2, target => 0),     #1 Minimum but with different owner
   ])};
  ok $@ =~ m(Owner mismatch memory: 0:0, wanted: 1, got: 2);
 }

if (1)                                                                          # Clear ownership of memory
 {my $r = eval {emulate
   ([instruction(action=>'set',        source => [0..9], owner=>1, target => [0..9]),  #0 Create and load some memory with for one owner
     instruction(action=>'ownerClear', source => 5,     target => 0),           #1 Clear owner ship of some of the memory
   ])};
  is_deeply $r->owner, { "0" => [0, 0, 0, 0, 0, 1, 1, 1, 1, 1] };
 }

if (1)                                                                          # Clear ownership of memory
 {my $r = eval {emulate
   ([instruction(action=>'set', source=>1, target=>[0..9], target_area=>22),    #0 Create and load some memory with for one owner
     instruction(action=>'memorySize', source=>22, target=>0),                  #1 Memory area size
   ])};
  is_deeply $r->memory->{0}, [10];
 }

#latest:;
if (1)                                                                          # Confess
 {my $r = emulate
    [instruction(action=>'call',    target=>"sub1"),                            #0 Call subroutine
     instruction(action=>'call',    label =>"sub1", target => "sub2"),          #1 Call subroutine
     instruction(action=>'confess', label =>"sub2"),                            #2 Print call stack
    ];

  is_deeply $r->out,
   ["Stack trace",
    "   2    1->   2          sub1->        sub2",
    "   1    0->   1              ->        sub1",
   ];
 }
