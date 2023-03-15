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

sub maximumInstructionsToExecute (){100}                                        # Maximum number of subroutines to execute
sub wellKnownMemoryAreas         (){1e6}                                        # Memory areas with a number less than this are well known. They can be used globally but cannot be freed

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

sub stackFrame(%)                                                               # Describe an entry on the call stack: the return address, the parameter list length, the parameter list location, the line of code from which the call was made, the file number of the file from which the call was made
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::StackFrame",                                         # Description of a stack frame. A stack frame provides the context in which a method runs.
    target    => $options{target},                                              # The location of the subroutine being called
    call      => $options{call},                                                # The location of the call instruction making the call
    stackArea => $options{stackArea},                                           # Memory area containing data for this method
    params    => $options{params},                                              # Memory area containing paramter list
    return    => $options{return},                                              # Memory area conmtaining returned result
    line      => $options{line},                                                # The line number from which the call was made
    file      => $options{file},                                                # The file number from which the call was made - this could be folded into the line number but for reasons best known to themselves people who cannot program very well often scatter projects across several files a practice that is completely pointless in this day of git and so can only lead to chaos and confusion
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
    next unless $i->action eq "label";
    $labels{$i->source} = $i;                                                   # Point label to instruction
   }

  for my $c(keys @$code)                                                        # Each jump or call instruction
   {my $i = $$code[$c];
    next unless $i->action =~ m(\Aj|\Acall);
    if (my $l = $i->target)                                                     # Label
     {if (my $t = $labels{$l})                                                  # Found label
       {$i->target = $t->number - $c;                                           # Relative jump
       }
      else
       {my $a = $i->action;
         confess "No target for $a to label: $l";
       }
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
   {$calls[-1]->stackArea;                                                      # Stack area
   }

  my sub allocMemory()                                                          # Create the name of a new memory area
   {my $a = &wellKnownMemoryAreas + scalar(keys %memory);
    $memory{$a} = [];
    $a
   }

  my sub right($)                                                               # Get a constant or a memory location
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

  my sub setMemory($$)                                                          # Set a memory location to a specified value
   {my ($t, $value) = @_;                                                       # Target, value
    my $s = stackArea;
    if (isScalar($t))
     {$memory{$s}[$t] = $value;                                                 # Set memory directly
     }
    elsif (isScalar($$t))
     {$memory{$s}[right($$t)]= $value;                                          # Set memory indirectly 1
     }
    else
     {$memory{$s}[$memory{$s}[right($$t)]]= $value;                             # Set memory indirectly 2
     }
   }

  my sub jumpOp($$)                                                             # Jump to the target location if the tested memory area if the condition is matched
   {my ($i, $check) = @_;                                                       # Instruction, check
    $instructionPointer = $i->number + right($i->target) if &$check;            # Check if condition is met
   }

  my %instructions =                                                            # Instruction definitions
   (add       => sub                                                            # Add two arrays to make a third array
     {my ($i) = @_;                                                             # Instruction
      setMemory $i->target, right($i->source) + right($i->source2);
     },

    call => sub                                                                 # Call a subroutine
     {my ($i) = @_;                                                             # Instruction
      my $t = $i->target;                                                       # Subroutine to call

      if (isScalar($t))
       {$instructionPointer = $i->number + $t;                                  # Relative call if we know where the subroutine is relative to the call instruction
       }
      else
       {$instructionPointer = $t;                                               # Absolute call
       }
      push @calls, stackFrame(target=>$code->[$instructionPointer], call=>$i,    # Create a new call stack entry
        stackArea=>allocMemory, params=>allocMemory, return=>allocMemory);
     },

    return    => sub                                                            # Return from a subrotuine call via the call stack
     {my ($i) = @_;                                                             # Instruction
      @calls or confess "The call stack is empty so I do not know where to return to";
      my $c = pop @calls;
      $instructionPointer = $c->call->number+1;
      $c->params = $c->return = undef;
     },

    confess => sub                                                              # Print the current call stack and stop
     {my ($i) = @_;                                                             # Instruction
      push @out, "Stack trace";
      for my $j(reverse keys @calls)
       {my $c = $calls[$j];
        if (my $I = $c->call)
         {push @out, sprintf "%4d Call", $j+1;
         }
        else
         {push @out, sprintf "%4d ????", $j+1;
         }
       }
      $instructionPointer = @$code;                                             # Execution terminates as soon as undefined instuction is encountered
     },

    inc       => sub                                                            # Increment locations in memory. The first location is incremented by 1, the next by two, etc.
     {my ($i) = @_;                                                             # Instruction
      setMemory $i->target, right($i->target) + 1;
     },

    jmp       => sub                                                            # Jump to the target location
     {my ($i) = @_;                                                             # Instruction
      $instructionPointer = $i->number + right($i->target);
     },
                                                                                # Conditional jumps
    jEq => sub {my ($i) = @_; jumpOp($i, sub{right($i->source) == right($i->source2)})},
    jNe => sub {my ($i) = @_; jumpOp($i, sub{right($i->source) != right($i->source2)})},
    jLe => sub {my ($i) = @_; jumpOp($i, sub{right($i->source) <= right($i->source2)})},
    jLt => sub {my ($i) = @_; jumpOp($i, sub{right($i->source) <  right($i->source2)})},
    jGe => sub {my ($i) = @_; jumpOp($i, sub{right($i->source) >= right($i->source2)})},
    jGt => sub {my ($i) = @_; jumpOp($i, sub{right($i->source) >  right($i->source2)})},

    label     => sub                                                            # Label - no operation
     {my ($i) = @_;                                                             # Instruction
     },

    memoryAllocate => sub                                                       # Allocate a new block of memory and write its key to the specified target
     {my ($i) = @_;                                                             # Instruction
      my ($t, $ta) = targetValue($i);                                           # Set target to length of memory area
      $memory{$ta}[$t] = wellKnownMemoryAreas + keys %memory;                   # Create the area above the well known areas
     },

    memoryFree => sub                                                           # Free the memory area specified by the target operand
     {my ($i) = @_;                                                             # Instruction
      my ($t, $ta) = targetValue($i);                                           # Set target to length of memory area
      $ta > wellKnownMemoryAreas or confess "Cannot free global area $ta";
      $memory{$ta} = undef;
     },

    memorySize => sub                                                           # Set the target location to the size of the memory area described by the source operand.
     {my ($i) = @_;                                                             # Instruction
      my ($s)      = sourceValue($i);                                           # Number of memory area
      my ($t, $ta) = targetValue($i);                                           # Set target to length of memory area
      #setMemory($i, $ta, $t, scalar $memory{$s}->@*);
     },

    mov       => sub                                                            # Move data moves data from one part of memory to another - "set", by contrast, sets variables from constant values
     {my ($i) = @_;                                                             # Instruction
      setMemory $i->target, right($i->source);
     },

    get       => sub                                                            # Move one word from the area identified by the first source operand at the location identified by the second source operand to the target location on the current area.
     {my ($i) = @_;                                                             # Instruction
      setMemory $i->target, $memory{right($i->source)}[right($i->source2)];
     },

    put       => sub                                                            # Move one word from the current area to the area identified by the first source operand at the location identified by the second source operand to the target location on the current area.
     {my ($i) = @_;                                                             # Instruction
      $memory{right($i->source)}[right($i->source2)] = right($i->target);
     },

    paramsGet => sub                                                            # Get a parameter from the previous parameter block - this means that we must always have two entries on teh call stack - one representing the caller of the program, the second representing the current context of the program
     {my ($i) = @_;                                                             # Instruction
      my $p = $calls[-2]->params;
      setMemory ${$i->target}, $memory{$p}[right($i->source)];
     },

    paramsPut => sub                                                            # Place a parameter in the current parameter block
     {my ($i) = @_;                                                             # Instruction
      my $p = $calls[-1]->params;
      $memory{$p}[right($i->target)] = right($i->source);
     },

    returnGet => sub                                                            # Get a word from the return area
     {my ($i) = @_;                                                             # Instruction
      my $p = $calls[-1]->return;
      setMemory ${$i->target}, $memory{$p}[right($i->source)];
     },

    returnPut => sub                                                            # Put a word ino the return area
     {my ($i) = @_;                                                             # Instruction
      my $p = $calls[-2]->return;
      $memory{$p}[right($i->target)] = right($i->source);
     },

    nop       => sub                                                            # No operation
     {my ($i) = @_;                                                             # Instruction
     },

    out     => sub                                                              # Write source as output to an array of words
     {my ($i) = @_;                                                             # Instruction
      push @out, right($i->source);
     },

    smaller => sub                                                              # Compare two constants or variables then indicate which is smaller: 0 - they are both qual, 1- the first one is smaller, 2 - the second one is smaller
     {my ($i) = @_;                                                             # Instruction
      my $s1 = right($i->source);
      my $s2 = right($i->source2);
      my $t  = $i->target;

      if (isScalar $t)
       {setMemory $t,  $s1 == $s2 ? 0 : $s1 < $s2 ? 1 : 2;
       }
      else
       {setMemory $$t, $s1 == $s2 ? 0 : $s1 < $s2 ? 1 : 2;
       }
     },
   );

  push @calls, stackFrame(                                                       # Initial stack entries
    stackArea => allocMemory,                                                      # Allocate data segment for current method
    params    => allocMemory,
    return    => allocMemory,
  ) for 1..2;

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

sub Call($)                                                                     # Call the subroutine at the target address
 {my ($target) = @_;                                                            # Target
  $assembly->instruction(action=>"call", target=>$target);
 }

sub Confess()                                                                   # Confess
 {$assembly->instruction(action=>"confess");
 }

sub Inc($)                                                                      # Increment the target
 {my ($target) = @_;                                                            # Target
  confess "Reference required for Inc" if isScalar $target;
  $assembly->instruction(action=>"inc", target=>$target);
 }

sub Jmp($)                                                                      # Jump to a label
 {my ($target) = @_;                                                            # Target
  $assembly->instruction(action=>"jmp", target=>$target);
 }

sub JLe($$$)                                                                    # Jump to a target label if the first source field is less than or equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jLe", target=>$target, source=>$source, source2=>$source2);
 }

sub JLt($$$)                                                                    # Jump to a target label if the first source field is less than the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jLt", target=>$target, source=>$source, source2=>$source2);
 }

sub JGe($$$)                                                                    # Jump to a target label if the first source field is greater than or equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jGe", target=>$target, source=>$source, source2=>$source2);
 }

sub JGt($$$)                                                                    # Jump to a target label if the first source field is greater than the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jGt", target=>$target, source=>$source, source2=>$source2);
 }

sub JEq($$$)                                                                    # Jump to a target label if the first source field is equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jEq", target=>$target, source=>$source, source2=>$source2);
 }

sub JNe($$$)                                                                    # Jump to a target label if the first source field is not equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jNe", target=>$target, source=>$source, source2=>$source2);
 }

sub Label($)                                                                    # Create a lable
 {my ($source) = @_;                                                            # Name of label
  $assembly->instruction(action=>"label", source=>$source);
 }

sub Mov($$)                                                                     # Copy a constant or memory location to the target location
 {my ($target, $source) = @_;                                                   # Target locations, source constants
  $assembly->instruction(action=>"mov", target=>$target, source=>$source);
 }

sub Nop()                                                                       # Do nothing (but do it well!)
 {$assembly->instruction(action=>"nop");
 }

sub ParamsGet($$)                                                               # Get a word from the parameters in the previous frame and store it in the local stack frame
 {my ($target, $source) = @_;                                                   # Memory location to place parameter in, parameter to get
  $assembly->instruction(action=>"paramsGet", target=>$target, source=>$source);
 }

sub ParamsPut($$)                                                               # Put a parameter into the current frame
 {my ($target, $source) = @_;                                                   # Offset in parameter area to write to, memory location whose contents are to be used as a parameter
  $assembly->instruction(action=>"paramsPut", target=>$target, source=>$source);
 }

sub Out($)                                                                      # Write memory contents to out
 {my ($source) = @_;                                                            # Memory location to output
  $assembly->instruction(action=>"out", source=>$source);
 }

sub Return()                                                                    # Return from a procedure via the call stack
 {$assembly->instruction(action=>"return");
 }

sub ReturnGet($$)                                                               # Get a word from the return area
 {my ($target, $source) = @_;                                                   # Memory location to place return value in, return value to get
  $assembly->instruction(action=>"returnGet", target=>$target, source=>$source);
 }

sub ReturnPut($$)                                                               # Put a word ino the return area
 {my ($target, $source) = @_;                                                   # Offset in return area to write to, memory location whose contents are to be placed in the return area
  $assembly->instruction(action=>"returnPut", target=>$target, source=>$source);
 }

sub Smaller($$$)                                                                # Compare the source operands and put 0 in the target if the operands are equal, 1 if the first operand is the smaller, or 2 if the second operand is the smaller
 {my ($target, $s1, $s2) = @_;                                                  # Target location, source one, source two
  $assembly->instruction(action=>"smaller",
    target=>$target, source=>$s1, source2=>$s2);
 }

sub Get($$$)                                                                    # Move one word from the area identified by the first source operand at the location identified by the second source operand to the target location on the current area.
 {my ($target, $s1, $s2) = @_;                                                  # Target location, source one, source two
  $assembly->instruction(action=>"get",
    target=>$target, source=>$s1, source2=>$s2);
 }

sub Put($$$)                                                                    # Move one word from the current area to the area identified by the first source operand at the location identified by the second source operand to the target location on the current area.
 {my ($target, $s1, $s2) = @_;                                                  # Target location, source one, source two
  $assembly->instruction(action=>"put",
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
  return $r;
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
  Out "hello World";
  ok execute(out=>["hello World"]);
 }

if (1)                                                                          # Nop
 {start;
  Nop;
  ok execute(out=>[]);
 }

if (1)                                                                          # Out
 {start;
  Mov 1, 2;
  Out \1;
  ok execute(out=>[2]);
 }

if (1)
 {start;                                                                        # Mov
  Mov  1, 3;
  Mov  2, 1;
  Mov  3, \\2;
  Out \3;
  ok execute(out=>[3]);
 }

if (1)                                                                          # Add constants
 {start;
  Add  \1, 3, 2;
  Out  \1;
  ok execute(out=>[5]);
 }

if (1)                                                                          # Add
 {start;
  Mov   1, 1;
  Mov   2, 2;
  Mov   3, 0;
  Mov   4, 3;
  Add  \4, \1, \2;
  Out  3;
  ok execute(out=>[3]);
 }

if (1)                                                                          # Inc
 {start;
  Mov  1, 1;
  Inc \1;
  Out \1;
  ok execute(out=>[2]);
 }

if (1)                                                                          # Small - constants
 {start;
  Smaller 1, 1, 2;
  Out   1;
  ok execute(out=>[1]);
 }

if (1)                                                                          # Small - variables
 {start;
  Mov   1, 1;
  Mov   2, 2;
  Smaller 3, \1, \2;
  Out  \3;
  ok execute(out=>[1]);
 }

if (1)                                                                          # Jump
 {start;
  Jmp 'a';
    Out  1;
    Jmp 'b';
  Label 'a';
    Out  2;
  Label 'b';
  ok execute(out=>[2]);
 }

if (1)                                                                          # Jump less than
 {start;
  Mov 0, 1;
  JLt 'a', \0, 2;
    Out  1;
    Jmp 'b';
  Label 'a';
    Out  2;
  Label 'b';

  JGt 'c', \0, 3;
    Out  1;
    Jmp 'd';
  Label 'c';
    Out  2;
  Label 'd';
  ok execute(out=>[2,1]);
 }

if (1)                                                                          # For loop
 {start;
  Mov 0, 0;
  Label 'a';
    Out \0;
    Inc \0;
  JLt 'a', \0, 10;
  ok execute(out=>[0..9]);
 }

if (1)                                                                          # Move between areas
 {start;
  Put  1, 0, 0;
  Get \0, 0, 0;
  Out \0;
  ok execute(out=>[1]);
 }

if (1)                                                                          # Call a subroutine with no parmeters
 {start;
  Jmp 'start';
  Label 'write';
    Out 'aaa';
  Return;
  Label 'start';
    Call 'write';
  ok execute(out=>['aaa']);
 }

if (1)                                                                          # Call a subroutine with one parmeter
 {start;
  Jmp 'start';
  Label 'write';
    ParamsGet \0, 0;
    Out \0;
  Return;
  Label 'start';
    ParamsPut 0, 'bbb';
    Call 'write';
  ok execute(out=>['bbb']);
 }

if (1)                                                                          # Call a subroutine returning one value
 {start;
  Jmp 'start';
  Label 'load';
    ReturnPut 0, "ccc";
  Return;
  Label 'start';
    Call 'load';
    ReturnGet \0, 0;
    Out \0;
  ok execute(out=>['ccc']);
 }

if (1)                                                                          # Call a subroutine which confesses
 {start;
  Jmp 'start';
  Label 'ccc';
    Confess;
  Return;
  Label 'start';
    Call 'ccc';
  ok execute(out=>["Stack trace", "   3 Call", "   2 ????", "   1 ????"]);
 }
