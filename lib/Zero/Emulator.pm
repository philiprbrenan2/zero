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
eval "use Test::More qw(no_plan)" unless caller;

=pod

All instructions except "move" operate on the local stack segment target=>1
means location 1. \1 means the location addressed by location 1

Memory is addressed in areas.  Each method has its own current stack area,
parameter area and return results area.  Each area can grow a much as is needed
to hold data. Memory areas can be sparse.  Additional memory areas can be
allocated and freed as necessary.

=cut

makeDieConfess;

sub maximumInstructionsToExecute {20}                                           # Maximum number of subroutines to execute

sub AreaStructure(@)                                                            # Describe a data structure mapping a memory area
 {my ($name, @fields) = @_;                                                     # Structure name, fields names

  my $d = genHash("Zero::Emulator::AreaStructure",                              # Description of a data structure mapping a memory area
    name  => $name,                                                             # Name of the structure
    order => [],                                                                # Order of the elements in the structure, in effect, giving the offset of each element in the data structure
    names => {},                                                                # Maps the names of the fields to their offsets in the structure
   );
  $d->field($_) for @fields;                                                    # Add the field descriptions
  $d
 }

sub Zero::Emulator::AreaStructure::field($$)                                    # Add a field to a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  if (!$d->names->{$name})
   {$d->names->{$name} = $d->order->@*;
    push $d->order->@*, $name;
   }
  else
   {confess "Duplicate name: $name in structure: ".$d->name;
   }
 }

sub Zero::Emulator::AreaStructure::offset($$)                                   # Offset of a field
 {my ($d, $name) = @_;                                                          # Parameters
  $d->names->{$name}
 }

sub callEntry(%)                                                                # Describe an entry on the call stack: the return address, the parameter list length, the parameter list location, the line of code from which the call was made, the file number of the file from which the call was made
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::CallEntry",                                          # Description of a call stack entry
    target   => $options{target},                                               # The location of the subroutine being called
    call     => $options{call},                                                 # The location of the call instruction making the call
    data     => $options{data},                                                 # Memory area containing data for this method
    params   => $options{params},                                               # Memory area containing paramter list
    return   => $options{return},                                               # Memory area conmtaining returned result
    line     => $options{line},                                                 # The line number from which the call was made
    file     => $options{file},                                                 # The file number from which the call was made - this could be folded into the line number but for reasons best known to themselves people who cannot program very well often scatter projects across several files a practice that is completely pointless in this day of git and so can only lead to chaos and confusion
  );
 }

sub Zero::Emulator::Code::instruction(%)                                        # Create a new instruction
 {my ($code, %options) = @_;                                                    # Code, options

  push $code->code->@*, genHash("Zero::Emulator::Instruction",                  # Instruction details
    action        => $options{action       },                                   # Instruction name
    number        => $options{number       },                                   # Instruction sequence number
    owner         => $options{owner        },
    source        => $options{source       },                                   # Source memory location
    source2       => $options{source2      },                                   # Secondary source memory location
    target        => $options{target       },                                   # Target memory location
    source_area   => $options{source_area  },
    source2_area  => $options{source2_area },
    target_area   => $options{target_area  },
  );
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

  my sub stackArea()                                                            # Memory area associated with this method invocation
   {$calls[-1]->data;                                                           # Stack area
   }

  my sub allocMemory()                                                          # Create the name of a new memory area
   {keys %memory
   }

  my sub getConstant($)                                                         # Get a memory location
   {my ($at) = @_;                                                              # Location
    my $s = stackArea;
    if (isScalar($at))                                                          # Constant
     {$at
     }
    elsif (isScalar($$at))                                                      # Direct
     {$memory{$s}[$$at]
     }
    else                                                                        # Indirect
     {$memory{$s}[$memory{$s}[$$$at]]
     }
   }

  my sub getMemory($)                                                           # Get a memory location
   {my ($at) = @_;                                                              # Location
    my $s = stackArea;
    if (isScalar($at))
     {$memory{$s}[$at]                                                          # Direct
     }
    elsif (isScalar($$at))
     {$memory{$s}[$memory{$s}[$$at]]                                            # Indirect 1
     }
    else
     {$memory{$s}[$memory{$s}[$$at[$memory{$s}[$$at]]]]                         # Indirect 2
     }
   }

  my sub setMemory($$)                                                          # Set a memory location to a specified value
   {my ($t, $value) = @_;                                                       # Target, value
    my $s = stackArea;
    if (isScalar($t))
     {$memory{$s}[$t] = $value;                                                 # Set memory directly
     }
    elsif (isScalar($$t))
     {$memory{$s}[getMemory($$t)]= $value;                                      # Set memory indirectly 1
     }
    else
     {$memory{$s}[$memory{$s}[getMemory($$t)]]= $value;                         # Set memory indirectly 2
     }
   }

  my sub sourceValue($)                                                         # The value of the source operand interpreting a scalar as a direct reference and an array reference as an indirect reference
   {my ($i) = @_;                                                               # Instruction
    my $s = $i->source;
    my $a = $i->source_area // 0;
    return ($s, $a) if isScalar $s;                                             # (value, memory block)
#    (getMemory($a, $$s[0]), $a);
undef
   }

  my sub targetValue($)                                                         # The value of the target operand interpreting a scalar as a direct reference and an array reference as an indirect reference
   {my ($i) = @_;                                                               # Instruction
    my $t = $i->target;
    my $A = $i->target_area // 0;
#    my $a = isScalar($A) ? $A : getMemory(0, $$A[0]);
#    return ($t, $a) if isScalar $t;                                             # (value, memory block)
#    (getMemory($a, $$t[0]), $a);
undef
   }

  my sub jumpOp($$)                                                             # Jump to the target location if the tested memory area if the condition is matched
   {my ($i, $check) = @_;                                                       # Instruction, check
    my ($t, $ta) = targetValue($i);
    $instructionPointer = $i->number + $t if &$check;                           # Check if condition is met
   }

  my %instructions =                                                            # Instruction definitions
   (add       => sub                                                            # Add two arrays to make a third array
     {my ($i) = @_;                                                             # Instruction
      setMemory($i->target, getMemory($i->source) + getMemory($i->source2));
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
      #setMemory($i, $i->target_area//0, $i->target, $c->params);
     },

    return    => sub                                                            # Call a subroutine
     {my ($i) = @_;                                                             # Instruction
      @calls or confess "The call stack is empty so I do not know where to return to";
      my $c = pop @calls;
      $instructionPointer = $c->call+1;
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
        my $P = $c->params // 0;
        push @out, sprintf "%4d %4d %12s  %4d", $j+1, $N, $L, $P;
       }
      $instructionPointer = @$code;                                             # Execution terminates as soon as undefined instuction is encountered
     },

    inc       => sub                                                            # Increment locations in memory. The first location is incremented by 1, the next by two, etc.
     {my ($i) = @_;                                                             # Instruction
      setMemory($i->target, getMemory($i->target) + 1);
     },

    jump      => sub                                                            # Jump to the target location
     {my ($i) = @_;                                                             # Instruction
      my ($t, $ta) = targetValue($i);
      if (isScalar($t))
       {$instructionPointer = $i->number + $t;
       }
      else
       {#$instructionPointer = getMemory($ta, $t);
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
         {#setMemory($i, $ta, $$t[$j], getMemory($sa, getMemory($sa, $$s[$j])));
         }
       }
     },

    max => sub                                                                  # Maximum element in source block to target
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source; my $sa = $i->source_area//0;                         # Array of locations containing the values to be summed
      my ($t, $ta) = targetValue($i);

      my $x;                                                                    # Maximum element
      for my $a(@$s)
       {#my $S = getMemory($sa, $a);
        #if (!defined($x) || $x < $S)
        # {$x = $S;
        # }
       }
      #setMemory($i, $ta, $t, $x);                                               # Save maximum
     },

    memoryAllocate => sub                                                       # Allocate a new block of memory and write its key to the specified target
     {my ($i) = @_;                                                             # Instruction
      my ($t, $ta) = targetValue($i);                                           # Set target to length of memory area
      $memory{$ta}[$t] = 1e6 + keys %memory;                                    # Keys below 1e6 are "well known", above that they are automatically generated
     },

    memoryFree => sub                                                           # Free the memory area specified by the target operand
     {my ($i) = @_;                                                             # Instruction
      my ($t, $ta) = targetValue($i);                                           # Set target to length of memory area
      $memory{$ta} = undef;
     },

    memorySize => sub                                                           # Set the target location to the size of the memory area described by the source operand.
     {my ($i) = @_;                                                             # Instruction
      my ($s)      = sourceValue($i);                                           # Number of memory area
      my ($t, $ta) = targetValue($i);                                           # Set target to length of memory area
      #setMemory($i, $ta, $t, scalar $memory{$s}->@*);
     },

    min => sub                                                                  # Minimum element in source block to target
     {my ($i) = @_;                                                             # Instruction
      my $s  = $i->source; my $sa = $i->source_area//0;                         # Array of locations containing the values to be summed
      my ($t, $ta) = targetValue($i);

      my $x;                                                                    # Minimum element
      for my $a(@$s)
       {#my $S = getMemory($sa, $a);
        #if (!defined($x) || $x > $S)
        # {$x = $S;
        # }
       }
      #setMemory($i, $ta, $t, $x);                                               # Save minimum
     },

    move     => sub                                                             # Move data moves data from one part of memory to another - "set", by contrast, sets variables from constant values
     {my ($i) = @_;                                                             # Instruction
      setMemory($i->target, getMemory($i->source));
     },

    moveBlock => sub                                                            # Move a block of data from the first source operand to the target operand.  The length of the move is determined by the second source operand.  The source block and the target block may overlap.
     {my ($i) = @_;                                                             # Instruction
      #my $S1 = $i->source_1; my $sa1 = $i->source_1_area // 0; my $s1 = isScalar($S1) ? $S1 : getMemory($sa1, $S1);
      #my $S2 = $i->source_2; my $sa2 = $i->source_2_area // 0; my $s2 = isScalar($S2) ? $S2 : getMemory($sa2, $S2);
      #my ($t, $ta) = targetValue($i);

      #my @b;                                                                    # Buffer the data being moved to avoid overwrites
      #push @b, getMemory($sa1, $s1+$_)  for 0..$s2-1;
      #setMemory($i, $ta, $t+$_, $b[$_]) for 0..$s2-1;
     },

    nop       => sub                                                            # No operation
     {my ($i) = @_;                                                             # Instruction
     },

    out     => sub                                                              # Write source as output to an array of words
     {my ($i) = @_;                                                             # Instruction
      push @out, getMemory($i->source);
     },

    outString => sub                                                            # Write a string to output
     {my ($i) = @_;                                                             # Instruction
      push @out, $i->source;
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
      setMemory($i->target, $i->source);
     },

    shiftBlockLeft => sub                                                       # Move a block of longs referenced by the target operand of length the source operand one long to the left
     {my ($i) = @_;                                                             # Instruction
      my ($s) = sourceValue($i);
      #my $T = $i->target; my $ta = $i->target_area // 0; my $t = isScalar($T) ? $T : getMemory($ta, $T);

      for my $j(0..$s-2)                                                        # Move block
       {#setMemory($i, $ta, $t+$j, getMemory($ta, $t+$j+1));
       }
     },

    shiftBlockRight => sub                                                      # Move a block of longs referenced by the target operand of length the source operand one long to the left
     {my ($i) = @_;                                                             # Instruction
      my ($s) = sourceValue($i);
      #my $T = $i->target; my $ta = $i->target_area // 0; my $t = isScalar($T) ? $T : getMemory($ta, $T);

      for my $j(reverse 0..$s-2)                                                # Move block
       {#setMemory($i, $ta, $t+$j+1, getMemory($ta, $t+$j));
       }
     },

    small => sub                                                                # Compare two constants or variables
     {my ($i) = @_;                                                             # Instruction
      my $s1 = getConstant($i->source);
      my $s2 = getConstant($i->source2);
      my $t  = $i->target;

      if (isScalar $t)
       {setMemory($t, $s1 == $s2 ? 0 : $s1 < $s2 ? 1 : 2);
       }
      else
       {setMemory($$t, $s1 == $s2 ? 0 : $s1 < $s2 ? 1 : 2);
       }
     },

    sum => sub                                                                  # Sum the source block and place it in the target
     {my ($i) = @_;                                                             # Instruction
      my $s = $i->source; my $sa = $i->source_area // 0;
      my ($t, $ta) = targetValue($i);

      #my $x = 0; $x += getMemory($sa, $_) for @$s;                              # Each location whose contents are to be summed
      #setMemory($i, $ta, $t, $x);                                               # Save sum
     },
   );

  push @calls, callEntry(                                                       # Initial stack entry
    data => scalar allocMemory,                                                 # Allocate data segment for current method
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

my $assembly = code;                                                            # The current assembly

sub start()                                                                     # Start the current assembly
 {$assembly = code;                                                             # The current assembly
 }

sub Add($$$)                                                                    # Copy the contents of the source location to the target location
 {my ($target, $s1, $s2) = @_;                                                  # Target location, source one, source two
  $assembly->instruction(action=>"add",
    target=>$target, source=>$s1, source2=>$s2);
 }

sub Inc($)                                                                      # Increment the target
 {my ($target) = @_;                                                            # Target
  $assembly->instruction(action=>"inc", target=>$target);
 }

sub Move($$)                                                                    # Copy the contents of the source location to the target location
 {my ($target, $source) = @_;                                                   # Target locations, source constants
  $assembly->instruction(action=>"move", target=>$target, source=>$source);
 }

sub Nop()                                                                       # Do nothing (but do it well!)
 {$assembly->instruction(action=>"nop");
 }

sub Out($)                                                                      # Write memory contents to out
 {my ($source) = @_;                                                            # Memory location to output
  $assembly->instruction(action=>"out", source=>$source);
 }

sub OutString($)                                                                # Output a string
 {my ($source) = @_;                                                            # Source string
  $assembly->instruction(action=>"outString", source=>$source);
 }

sub Set($$)                                                                     # Set the contents of the target from source constants
 {my ($target, $source) = @_;                                                   # Target locations, source constants
  $assembly->instruction(action=>"set", target=>$target, source=>$source);
 }

sub Small($$$)                                                                  # Compare the source operands and put 0 in the target if the operands are equal, 1 if the first operand is the smaller, or 2 if the second operand is the smaller
 {my ($target, $s1, $s2) = @_;                                                  # Target location, source one, source two
  $assembly->instruction(action=>"small",
    target=>$target, source=>$s1, source2=>$s2);
 }

sub execute(%)                                                                  # Execute the current assembly
 {my (%options) = @_;                                                           # Options
  my $r = $assembly->execute;                                                   # Execute the code in the current assembly
  if (my $out = $options{out})
   {my $c = compareArraysAndExplain $r->out, $out;
    lll $c if $c;
    return !$c;
   }
 }

use Exporter qw(import);
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

@ISA         = qw(Exporter);
@EXPORT      = qw();
@EXPORT_OK   = qw(emulate instruction);
%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

return 1 if caller;

eval {goto latest};
sub is_deeply;
sub ok($;$);

latest:;
if (1)
 {start;
  OutString "hello World";
  ok execute(out=>["hello World"]);
 }

if (1)                                                                          # Nop
 {start;
  Nop;
  ok execute(out=>[]);
 }

if (1)                                                                          # Out
 {start;
  Set 1, 2;
  Out 1;
  ok execute(out=>[2]);
 }

if (1)
 {start;                                                                        # Set
  Set  2, 1;
  Set \2, 2;
  Out \2;
  Out  1;
  ok execute(out=>[2, 2]);
 }

if (1)                                                                          # Move
 {start;
  Set  1, 1;
  Move 2, 1;
  Out  2;
  ok execute(out=>[1]);
 }

if (1)                                                                          # Add
 {start;
  Set  1, 1;
  Set  2, 2;
  Set  3, 0;
  Set  4, 3;
  Add  \4, \1, \2;
  Out  3;
  ok execute(out=>[3]);
 }

if (1)                                                                          # Inc
 {start;
  Set  1, 1;
  Set  2, 1;
  Inc \2;
  Out  1;
  ok execute(out=>[2]);
 }

if (1)                                                                          # Small - constants
 {start;
  Small 1, 1, 2;
  Out   1;
  ok execute(out=>[1]);
 }

if (1)                                                                          # Small - constants
 {start;
  Set   1, 1;
  Set   2, 2;
  Small 3, \1, \2;
  Out   3;
  ok execute(out=>[1]);
 }
exit;


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

if (1)                                                                          # Confess
 {my $r = emulate
    [instruction(action=>'nop'),                                                #0 Do nothing
     instruction(action=>'call',    target=>"sub1", source=>[4]),               #1 Call subroutine
     instruction(action=>'call',    label =>"sub1", source=>[3], target => "sub2"),          #2 Call subroutine
     instruction(action=>'confess', label =>"sub2"),                            #3 Print call stack
    ];

# say STDERR dump($r->out);
  is_deeply $r->out,
["Stack trace",
 "   2    3         sub2     0",
 "   1    2         sub1     0",
];
 }

if (1)                                                                          # Allocate and free memory
 {my $r = emulate
   ([instruction(action=>'memoryAllocate', target=>0),                           #0 Allocate
     instruction(action=>'set', source=>1, target_area=>[0], target=>[0..9]),   #1 Set the memory
     instruction(action=>'out', source=>[0..9], source_area=>[0]),              #2 Print
#     instruction(action=>'memoryFree', source=>[0]),                            #3 Call subroutine
    ]);
  say STDERR dump($r->out);
  say STDERR dump($r->memory);
 }
