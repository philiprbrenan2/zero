#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/
#-------------------------------------------------------------------------------
# Emulate the zero assembly programming language.
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

Memory is addressed in areas.  Each method has its own current stack area,
parameter area and return results area.  Each area can grow a much as is needed
to hold data. Memory areas can be sparse.  Additional memory areas can be
allocated and freed as necessary.

Well known locations are represented by negative area ids

=cut

makeDieConfess;

sub maximumInstructionsToExecute (){100}                                        # Maximum number of subroutines to execute

sub areaStructure($@)                                                           # Describe a data structure mapping a memory area
 {my ($structureName, @names) = @_;                                             # Structure name, fields names

  my $d = genHash("Zero::Emulator::areaStructure",                              # Description of a data structure mapping a memory area
    structureName => $structureName,                                            # Name of the structure
    fieldOrder    => [],                                                        # Order of the elements in the structure, in effect, giving the offset of each element in the data structure
    fieldNames    => {},                                                        # Maps the names of the fields to their offsets in the structure
   );
  $d->field($_) for @names;                                                     # Add the field descriptions
  $d
 }

sub Zero::Emulator::areaStructure::temporary($;$)                               # Create one or more temporary variables. Need to reuse temporaries no longer in use
 {my ($d, $count) = @_;                                                         # Parameters
  if (!defined($count))
   {my $o = $d->fieldOrder->@*;
    push $d->fieldOrder->@*, undef;
    return \$o;                                                                 # One temporary
   }
  map {__SUB__->($d)} 1..$count;                                                # Array of temporaries
 }

sub Zero::Emulator::areaStructure::name($$)                                     # Add a field to a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  if (!$d->fieldNames->{$name})
   {$d->fieldNames->{$name} = $d->fieldOrder->@*;
    push $d->fieldOrder->@*, $name;
   }
  else
   {confess "Duplicate name: $name in structure: ".$d->name;
   }
  \($d->fieldNames->{$name})
 }

sub Zero::Emulator::areaStructure::names($@)                                    # Add fields to a data structure
 {my ($d, @names) = @_;                                                         # Parameters
  map {$d->name($_)} @names;
 }

sub Zero::Emulator::areaStructure::offset($$)                                   # Offset of a field in a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  if (my $n = $d->fieldNames->{$name}){return $n}
  confess "No such name: '$name' in structure: ".$d->structureName;
 }

sub Zero::Emulator::areaStructure::address($$)                                  # Address of a field in a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  if (defined(my $n = $d->fieldNames->{$name})){return \$n}
  confess "No such name: '$name' in structure: ".$d->structureName;
 }

sub procedure($%)                                                               # Describe a procedure
 {my ($label, %options) = @_;                                                   # Start label of procedure, options describing procedure

  genHash("Zero::Emulator::Procedure",                                          # Description of a procedure
    target       => $label,                                                     # Label to call to call this procedure
    variables    => areaStructure("Procedure"),                                 # Variables local to this procedure
  );
 }

sub Zero::Emulator::Procedure::call($)                                          # Call a procedure.  Arguments are supplied by the ParamsPut and Get commands, return values are supplied by the ReturnPut and Get commands.
 {my ($procedure) = @_;                                                         # Procedure description
  Zero::Emulator::Call($procedure->target);
 }

sub stackFrame(%)                                                               # Describe an entry on the call stack: the return address, the parameter list length, the parameter list location, the line of code from which the call was made, the file number of the file from which the call was made
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::StackFrame",                                         # Description of a stack frame. A stack frame provides the context in which a method runs.
    target      => $options{target},                                            # The location of the subroutine being called
    instruction => $options{call},                                              # The location of the instruction making the call
    stackArea   => $options{stackArea},                                         # Memory area containing data for this method
    params      => $options{params},                                            # Memory area containing paramter list
    return      => $options{return},                                            # Memory area conmtaining returned result
    line        => $options{line},                                              # The line number from which the call was made
    file        => $options{file},                                              # The file number from which the call was made - this could be folded into the line number but for reasons best known to themselves people who cannot program very well often scatter projects across several files a practice that is completely pointless in this day of git and so can only lead to chaos and confusion
  );
 }

sub Zero::Emulator::Code::instruction($%)                                       # Create a new instruction
 {my ($block, %options) = @_;                                                   # Block of code desctriptor, options

  my ($package, $fileName, $line) = caller($options{level} // 1);

  if ($options{action} =~ m(\Avariable\Z)i)                                     # Variable
   {$block->variables->{$options{target}};
   }
  else
   {push $block->code->@*, genHash("Zero::Emulator::Code::Instruction",         # Instruction details
      action        => $options{action      },                                  # Instruction name
      number        => $options{number      },                                  # Instruction sequence number
      source        => $options{source      },                                  # Source memory location
      source2       => $options{source2     },                                  # Secondary source memory location
      sourceArea    => $options{sourceArea  },                                  # Source memory area
      target        => $options{target      },                                  # Target memory location
      target2       => $options{target2     },                                  # Secondary target memory location
      targetArea    => $options{targetArea  },                                  # Target secondary memory location
      source_area   => $options{source_area },                                  # Source area
      target_area   => $options{target_area },                                  # Target area
      line          => $line,                                                   # Line in source file at which this instruction was encoded
      file          => fne $fileName,                                           # Source file in which instruction was encoded
    );
   }
 }

sub isScalar($)                                                                 # Check whether an element is a scalar or an array
 {my ($value) = @_;                                                             # Parameters
  ! ref $value;
 }

sub block(%)                                                                    # A block of code
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::Code",                                               # Description of a call stack entry
    assembled    => undef,                                                      # Needs to be assembled unless this field is true
    code         => [],                                                         # An array of instructions
    variables    => areaStructure("Block"),                                     # Variables in this block of code
    labels       => {},                                                         # Label name to instruction
    labelCounter => 0,                                                          # Label counter used to generate unique labels
    files        => [],                                                         # File number to file name
    procedures   => {},                                                         # Procdures defined in this block of code
    %options,
   );
 }

sub emulate($%)                                                                 # Emulate an array of code
 {my ($code, %options) = @_;                                                    # Block of code, options

  my $c = code(code => $code);
  my $r = $c->Execute(%options);
  $r
 }

sub Zero::Emulator::Code::assemble($%)                                          # Assemble a block of code to prepare it for execution
 {my ($Block, %options) = @_;                                                   # Code block, assembly options
  return $Block if $Block->assembled;                                           # Already assembled
  my $code = $Block->code;                                                      # The code to be assembled
  my $vars = $Block->variables;                                                 # The varaibles refernced by the code

  my %labels;                                                                   # Load labels
  my $stackFrame = areaStructure("Stack");                                      # The current stack frame we are creating variables in

  for my $c(keys @$code)                                                        # Labels
   {my $i = $$code[$c];
    $i->number = $c;
    next unless $i->action eq "label";
    $labels{$i->source} = $i;                                                   # Point label to instruction
   }

  for my $c(keys @$code)                                                        # Target jump / call instructions
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

  $Block->labels = {%labels};                                                   # Labels created during assembly
  $Block->assembled = time;                                                     # Time of assembly
  $Block
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

  my sub stackTraceAndExit($)                                                   # Print a stack trace and exit
   {my ($i) = @_;                                                               # Instruction trace occurred at
    my $s = $options{suppressStackTracePrint};

    push my @s, "Stack trace\n";
    for my $j(reverse keys @calls)
     {my $c = $calls[$j];
      my $i = $c->instruction;
      push @s, sprintf "%5d  %4d %s\n", $j+1, $i->number+1, $i->action if $s;
      push @s, sprintf "%5d  %4d %-16s  at %s line %d\n", $j+1, $i->number+1, $i->action, $i->file, $i->line, unless $s;
     }
    say STDERR join "\n", @s unless $s;
    push @out, @s;
    $instructionPointer = undef;                                                # Execution terminates as soon as undefined instuction is encountered
   };

  my sub stackArea()                                                            # Memory area associated with this method invocation
   {$calls[-1]->stackArea;                                                      # Stack area
   }

  my sub allocMemory()                                                          # Create the name of a new memory area
   {my $a = scalar(keys %memory);
    $memory{$a} = [];
    $a
   }

  my sub left($;$)                                                              # Address a memory location
   {my ($A, $area) = @_;                                                        # Location, optional area
    my $a = $A;
    $a = \$A if isScalar $a;                                                    # Interpret constants as direct memory locations
    if (isScalar $$a)
     {if (!defined($area))                                                      # Current stack frame
       {return \$memory{&stackArea}[$$a]                                        # Stack frame
       }
      elsif (isScalar($area))
       {return \$memory{$area}[$$a]                                             # Specified constant area
       }
      elsif (isScalar($$area))
       {return \$memory{$memory{&stackArea}[$$area]}[$$a]                       # Indirect area
       }
     }
    if (isScalar $$$a)
     {if (!defined($area))                                                      # Current stack frame
       {return \$memory{&stackArea}[$memory{&stackArea}[$$$a]]                  # Stack frame
       }
      elsif (isScalar($area))
       {return \$memory{$area}[$memory{&stackArea}[$$$a]]                       # Specified constant area
       }
      elsif (isScalar($$area))
       {return \$memory{$memory{&stackArea}[$$area]}[$memory{&stackArea}[$$$a]] # Indirect area
       }
     }
    my $i = $calls[-1]->instruction;
    my $l = $i->line;
    my $f = $i->file;
    die "Invalid left area.address: ".dump([$area, $a])
     ." at $f line $l\n".dump(\%memory);
   }

  my sub right($;$)                                                             # Get a constant or a memory location
   {my ($a, $area) = @_;                                                        # Location, optional area
    return $a if isScalar($a);                                                  # Constant
    my $r;
    if (isScalar($$a))                                                          # Direct
     {if (!defined($area))
       {$r = $memory{&stackArea}[$$a]                                           # Direct from stack area
       }
      elsif (isScalar($area))
       {$r = $memory{$area}[$$a]                                                # Direct from constant area
       }
      elsif (isScalar($$area))
       {if (defined(my $i = $memory{&stackArea}[$$area]))
         {$r = $memory{$i}[$$a]                                                 # Direct from indirect area
         }
       }
     }
    elsif (isScalar($$$a))                                                      # Indirect
     {if (!defined($area))
       {$r = $memory{&stackArea}[$memory{&stackArea}[$$$a]]                     # Indirect from stack area
       }
      elsif (isScalar($area))
       {if (defined(my $i = $memory{&stackArea}[$$$a]))
         {$r = $memory{$area}[$i]                                               # Indirect from constant area
         }
       }
      elsif (isScalar($$area))
       {if (defined(my $i = $memory{&stackArea}[$$$a]))
         {if (defined(my $j = $memory{&stackArea}[$$$area]))
           {$r = $memory{$j}[$i]                                                # Indirect from indirect area
           }
         }
       }
     }
    if (!defined($r))
     {my $i = $calls[-1]->instruction;
      my $l = $i->line;
      my $f = $i->file;
      die "Invalid right area.address: "
       .dump([$area, $a])." at $f line $l\n".dump(\%memory);
     }
    $r
   }

  my sub setMemory($$;$)                                                        # Set a memory location in the current stack frame to a specified value
   {my ($target, $value, $area) = @_;                                           # Target, value, optional area
    my $a = left($target, $area//stackArea);
    $$a   = $value;
   }

  my sub jumpOp($$)                                                             # Jump to the target location if the tested memory area if the condition is matched
   {my ($i, $check) = @_;                                                       # Instruction, check
    $instructionPointer = $i->number + right($i->target) if &$check;            # Check if condition is met
   }

  my %instructions =                                                            # Instruction definitions
   (add       => sub                                                            # Add two arrays to make a third array
     {my $i = $calls[-1]->instruction;
      setMemory $i->target, right($i->source) + right($i->source2), $i->targetArea;
     },

    alloc     => sub                                                            # Create a new memory area and write its number into the location named by the target operand
     {my $i = $calls[-1]->instruction;
      my $a = allocMemory;
      setMemory $i->target, $a, $i->targetArea;
     },

    assertEq  => sub                                                            # Assert equals
     {my $i = $calls[-1]->instruction;
      my ($a, $b) = (right($i->source), right($i->sourceArea));
      unless(right($a) == right($b))
       {say STDERR "Assert $a == $b failed" unless $options{suppressStackTracePrint};
        stackTraceAndExit($i);
       }
     },

    assertNe  => sub                                                            # Assert not equals
     {my $i = $calls[-1]->instruction;
      my ($a, $b) = (right($i->source), right($i->sourceArea)) unless $options{suppressStackTracePrint};
      unless(right($a) != right($b))
       {say STDERR "Assert $a != $b failed";
        stackTraceAndExit($i);
       }
     },

    assertLt  => sub                                                            # Assert less than
     {my $i = $calls[-1]->instruction;
      my ($a, $b) = (right($i->source), right($i->sourceArea)) unless $options{suppressStackTracePrint};
      unless(right($a) <  right($b))
       {say STDERR "Assert $a <  $b failed";
        stackTraceAndExit($i);
       }
     },

    assertLe  => sub                                                            # Assert less than or equal
     {my $i = $calls[-1]->instruction;
      my ($a, $b) = (right($i->source), right($i->sourceArea)) unless $options{suppressStackTracePrint};
      unless(right($a) <= right($b))
       {say STDERR "Assert $a <= $b failed";
        stackTraceAndExit($i);
       }
     },

    assertGt  => sub                                                            # Assert greater than
     {my $i = $calls[-1]->instruction;
      my ($a, $b) = (right($i->source), right($i->sourceArea)) unless $options{suppressStackTracePrint};
      unless(right($a) > right($b))
       {say STDERR "Assert $a >  $b failed";
        stackTraceAndExit($i);
       }
     },

    assertGe  => sub                                                            # Assert greater
     {my $i = $calls[-1]->instruction;
      my ($a, $b) = (right($i->source), right($i->sourceArea)) unless $options{suppressStackTracePrint};
      unless(right($a) >= right($b))
       {say STDERR "Assert $a >= $b failed";
        stackTraceAndExit($i);
       }
     },

    free      => sub                                                            # Free the memory area named by the source operand
     {my $i = $calls[-1]->instruction;
      delete $memory{right($i->source, $i->sourceArea)};
     },

    call      => sub                                                            # Call a subroutine
     {my $i = $calls[-1]->instruction;
      my $t = $i->target;                                                       # Subroutine to call

      if (isScalar($t))
       {$instructionPointer = $i->number + $t;                                  # Relative call if we know where the subroutine is relative to the call instruction
       }
      else
       {$instructionPointer = $t;                                               # Absolute call
       }
      push @calls, stackFrame(target=>$code->[$instructionPointer],             # Create a new call stack entry
        instruction=>$i,
        stackArea=>allocMemory, params=>allocMemory, return=>allocMemory);
     },

    return    => sub                                                            # Return from a subroutine call via the call stack
     {my $i = $calls[-1]->instruction;
      @calls or confess "The call stack is empty so I do not know where to return to";
      my $C = pop @calls;
      if (@calls)
       {my $c = $calls[-1];
        $instructionPointer = $c->instruction->number+1;
       }
      else
       {$instructionPointer = undef;
       }
      $C->params = $C->return = undef;
     },

    confess => sub                                                              # Print the current call stack and stop
     {my $i = $calls[-1]->instruction;
      stackTraceAndExit($i);
     },

    dump    => sub                                                              # Dump memory
     {my $i = $calls[-1]->instruction;
      say STDERR $i->source, dump(\%memory);
     },

    dec     => sub                                                              # Decrement locations in memory. The first location is incremented by 1, the next by two, etc.
     {my $i = $calls[-1]->instruction;
      my $t1 = $i->target;
      my $t2 = $i->targetArea;
      setMemory $i->target, right($t1, $t2) - 1, $t2;
     },

    inc       => sub                                                            # Increment locations in memory. The first location is incremented by 1, the next by two, etc.
     {my $i = $calls[-1]->instruction;
      my $t1 = $i->target;
      my $t2 = $i->targetArea;
      setMemory $i->target, right($t1, $t2) + 1, $t2;
     },

    jmp       => sub                                                            # Jump to the target location
     {my $i = $calls[-1]->instruction;
      $instructionPointer = $i->number + right($i->target);
     },
                                                                                # Conditional jumps
    jEq => sub {my $i = $calls[-1]->instruction; jumpOp($i, sub{right($i->source) == right($i->source2)})},
    jNe => sub {my $i = $calls[-1]->instruction; jumpOp($i, sub{right($i->source) != right($i->source2)})},
    jLe => sub {my $i = $calls[-1]->instruction; jumpOp($i, sub{right($i->source) <= right($i->source2)})},
    jLt => sub {my $i = $calls[-1]->instruction; jumpOp($i, sub{right($i->source) <  right($i->source2)})},
    jGe => sub {my $i = $calls[-1]->instruction; jumpOp($i, sub{right($i->source) >= right($i->source2)})},
    jGt => sub {my $i = $calls[-1]->instruction; jumpOp($i, sub{right($i->source) >  right($i->source2)})},

    label     => sub                                                            # Label - no operation
     {my ($i) = @_;                                                             # Instruction
     },

    mov       => sub                                                            # Move data moves data from one part of memory to another - "set", by contrast, sets variables from constant values
     {my $i = $calls[-1]->instruction;
      setMemory $i->target, right($i->source, $i->sourceArea), $i->targetArea;
     },

    paramsGet => sub                                                            # Get a parameter from the previous parameter block - this means that we must always have two entries on teh call stack - one representing the caller of the program, the second representing the current context of the program
     {my $i = $calls[-1]->instruction;
      my $p = $i->sourceArea // $calls[-2]->params;
      my $t = left($i->target, $i->targetArea);
      my $s = right($i->source, $p);
      $$t = $s;
     },

    paramsPut => sub                                                            # Place a parameter in the current parameter block
     {my $i = $calls[-1]->instruction;
      my $p = $i->targetArea // $calls[-1]->params;
      my $t = left($i->target, $p);
      my $s = right($i->source, $i->sourceArea);
      $$t = $s;
     },

    returnGet => sub                                                            # Get a word from the return area
     {my $i = $calls[-1]->instruction;
      my $p = $calls[-1]->return;
      my $t = left($i->target, $i->targetArea);
      my $s = right($i->source, $p);
      $$t = $s;
     },

    returnPut => sub                                                            # Put a word ino the return area
     {my $i = $calls[-1]->instruction;
      my $p = $calls[-2]->return;
      my $t = left($i->target, $p);
      my $s = right($i->source, $i->sourceArea);
      $$t = $s;
     },

    nop       => sub                                                            # No operation
     {my ($i) = @_;                                                             # Instruction
     },

    out     => sub                                                              # Write source as output to an array of words
     {my $i = $calls[-1]->instruction;
      push @out, right($i->source, $i->sourceArea);
     },

    pop => sub                                                                  # Pop a value from the specified memory area if possible else confess
     {my $i = $calls[-1]->instruction;
      my $p = right($i->source);
      if (!defined($memory{$p}) or !$memory{$p}->@*)                            # Stack no poppable
       {confess($i);
       }
      setMemory right($i->target, $i->targetArea), pop $memory{$p}->@*;         # Pop from memory area into current stack frame
     },

    push => sub                                                                 # Push a value onto the specified memory area
     {my $i = $calls[-1]->instruction;
      push $memory{right($i->target)}->@*, right($i->source, $i->sourceArea);
     },

    smaller => sub                                                              # Compare two constants or variables then indicate which is smaller: 0 - they are both equal, 1 - the first one is smaller, 2 - the second one is smaller
     {my $i = $calls[-1]->instruction;
      my $s1 = right($i->source);
      my $s2 = right($i->sourceArea);
      my $t  = $i->target;
      my $a  = $i->targetArea;

      if (isScalar $t)
       {setMemory $t,  $s1 == $s2 ? 0 : $s1 < $s2 ? 1 : 2, $a;
       }
      else
       {setMemory $$t, $s1 == $s2 ? 0 : $s1 < $s2 ? 1 : 2, $a;
       }
     },
   );

  push @calls, stackFrame(                                                      # Initial stack entries
    stackArea => allocMemory,                                                   # Allocate data segment for current method
    params    => allocMemory,
    return    => allocMemory,
  ) for 1..1;

  for my $j(1..maximumInstructionsToExecute)                                    # Each instruction in the code until we hit an undefined instruction
   {last unless defined($instructionPointer);
    my $i = $calls[-1]->instruction = $$code[$instructionPointer++];
    last unless $i;
    if (my $a = $i->action)                                                     # Action
     {$counts{$a}++; $count++;                                                  # Execution counts
      confess qq(Invalid instruction: "$a"\n) unless my $c = $instructions{$a};
      if ($options{trace})
       {say STDERR sprintf "%4d  %4d  %12s", $j, $i->number, $i->action;
       }
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
   );
 }

my $assembly = block;                                                           # The current assembly

sub label()                                                                     # Next unique label
 {++$assembly->labelCounter;
 }

sub setLabel(;$)                                                                # Set and return a label
 {my ($l) = @_;                                                                 # Optional preset label
  $l //= label;                                                                 # Create label if none supplied
  Label($l);                                                                    # Set label
  $l                                                                            # return (new) label
 }

sub Start($)                                                                    # Start the current assembly using the specified version of the Zero languiage.  At  the moment only version 1 works.
 {my ($version) = @_;                                                           # Version desired - at the moment only 1
  $version == 1 or confess "Version 1 is currently the only version available\n";
  $assembly = block;                                                            # The current assembly
 }

sub Add($$$;$)                                                                  # Copy the contents of the source location to the target location
 {if (@_ == 3)
   {my ($target, $s1, $s2) = @_;                                                # Target location, source one, source two
    $assembly->instruction(action=>"add",
      target=>$target, source=>$s1, source2=>$s2);
   }
  else
   {my ($t1, $t2, $s1, $s2) = @_;                                               # Target location, target area, source one, source two
    $assembly->instruction(action=>"add",
      target=>$t1, targetArea=>$t2, source=>$s1, source2=>$s2);
   }
 }

sub Alloc($)                                                                    # Create a new memory area and write its number into the location named by the target operand
 {my ($target) = @_;                                                            # Target location to palce number of area created
  $assembly->instruction(action=>"alloc", target=>$target);
 }

sub Free($)                                                                     # Free the memory area named by the source operand
 {my ($source) = @_;                                                            # Source location containing number of area to free
  $assembly->instruction(action=>"free", source=>$source);
 }

sub Call($)                                                                     # Call the subroutine at the target address
 {my ($target) = @_;                                                            # Target
  $assembly->instruction(action=>"call", target=>$target);
 }

sub Confess()                                                                   # Confess
 {$assembly->instruction(action=>"confess");
 }

sub Dump($)                                                                     # Dump memory
 {my ($title) = @_;                                                             # Title
  $assembly->instruction(action=>"dump", source=>$title);
 }

sub Dec($;$)                                                                    # Decrement the target
 {my ($target, $targetArea) = @_;                                               # Target location, target area
  confess "Reference required for Dec" if isScalar $target;
  $assembly->instruction(action=>"dec", target=>$target, targetArea=>$targetArea);
 }

sub Inc($;$)                                                                    # Increment the target
 {my ($target, $targetArea) = @_;                                               # Target location, target area
  confess "Reference required for Inc" if isScalar $target;
  $assembly->instruction(action=>"inc", target=>$target, targetArea=>$targetArea);
 }

sub Jmp($)                                                                      # Jump to a label
 {my ($target) = @_;                                                            # Target
  $assembly->instruction(action=>"jmp", target=>$target);
 }

sub Jle($$$)                                                                    # Jump to a target label if the first source field is less than or equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jLe", target=>$target, source=>$source, source2=>$source2);
 }

sub Jlt($$$)                                                                    # Jump to a target label if the first source field is less than the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jLt", target=>$target, source=>$source, source2=>$source2);
 }

sub Jge($$$)                                                                    # Jump to a target label if the first source field is greater than or equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jGe", target=>$target, source=>$source, source2=>$source2);
 }

sub Jgt($$$)                                                                    # Jump to a target label if the first source field is greater than the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jGt", target=>$target, source=>$source, source2=>$source2);
 }

sub Jeq($$$)                                                                    # Jump to a target label if the first source field is equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jEq", target=>$target, source=>$source, source2=>$source2);
 }

sub Jne($$$)                                                                    # Jump to a target label if the first source field is not equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jNe", target=>$target, source=>$source, source2=>$source2);
 }

sub Label($)                                                                    # Create a lable
 {my ($source) = @_;                                                            # Name of label
  $assembly->instruction(action=>"label", source=>$source);
 }

sub Mov($$;$$)                                                                  # Copy a constant or memory location to the target location
 {if    (@_ == 2)
   {my ($target, $source) = @_;                                                 # Target location, source location
    $assembly->instruction(action=>"mov", target=>$target, source=>$source);
   }
  elsif (@_ == 3)
   {my ($target, $source, $sourceArea) = @_;                                    # Target location, source location, source area
    $assembly->instruction(action=>"mov", target=>$target, source=>$source, sourceArea=>$sourceArea);
   }
  else
   {my ($target, $targetArea, $source, $sourceArea) = @_;                       # Target location, target area, source location, source area
    $assembly->instruction(action=>"mov", target=>$target, targetArea=>$targetArea, source=>$source, sourceArea=>$sourceArea);
   }
 }

sub Nop()                                                                       # Do nothing (but do it well!)
 {$assembly->instruction(action=>"nop");
 }

sub Out($;$)                                                                    # Write memory contents to out
 {my ($source, $sourceArea) = @_;                                               # Memory location to output, memory area containing source operand

  $assembly->instruction(action=>"out", source=>$source, sourceArea=>$sourceArea);
 }

sub Procedure($$)                                                               # Define a procedure
 {my ($name, $source) = @_;                                                     # Name of procedure, source code as a subroutine# $assembly->instruction(action=>"procedure", target=>$target, source=>$source);
  if ($name and my $n = $assembly->procedures->{$name})                         # Reuse existing named procedure
   {return $n;
   }

  Jmp(my $end = label);                                                         # Jump over the code of the procedure body
  my $start = setLabel;
  &$source(procedure($start));                                                  # Code of procedure called with start label as a parameter
  setLabel $end;

  $assembly->procedures->{$name} = $start                                       # Return the start of the procedure
 }

sub ParamsGet($$;$)                                                             # Get a word from the parameters in the previous frame and store it
 {if (@_ == 2)
   {my ($target, $source) = @_;                                                 # Memory location to place parameter in, parameter to get from parameter area
    $assembly->instruction(action=>"paramsGet", target=>$target, source=>$source);
   }
  else
   {my ($target, $targetArea, $source) = @_;                                    # Memory location to place parameter in, memory area to place parameter in, parameter to get from parameter area
    $assembly->instruction(action=>"paramsGet", target=>$target, target=>$targetArea, source=>$source);
   }
 }

sub ParamsPut($$;$)                                                             # Put a parameter from the specified location and area into the parameters area
 {my ($target, $source, $sourceArea) = @_;                                      # Offset in parameter area to write to, memory location whose contents are to be used as a parameter, memory area containing contents
  $assembly->instruction(action=>"paramsPut", target=>$target, source=>$source, sourceArea=>$sourceArea);
 }

sub Return()                                                                    # Return from a procedure via the call stack
 {$assembly->instruction(action=>"return");
 }

sub ReturnGet($$;$)                                                             # Get a word from the return area and save it
 {if (@_ == 2)
   {my ($target, $source) = @_;                                                 # Memory location to place return value in, return value to get
    $assembly->instruction(action=>"returnGet", target=>$target, source=>$source);
   }
  else
   {my ($target, $targetArea, $source) = @_;                                    # Memory location to place return value in, memory area to place return value in, return value to get
    $assembly->instruction(action=>"returnGet", target=>$target, targetArea=>$targetArea, source=>$source);
   }
 }

sub ReturnPut($$;$)                                                             # Put a word into the return area
 {my ($target, $source, $sourceArea) = @_;                                      # Offset in return area to write to, memory location whose contents are to be placed in the return area
  $assembly->instruction(action=>"returnPut", target=>$target, source=>$source, sourceArea=>$sourceArea);
 }

sub Pop($$;$)                                                                   # Pop the memory area specified by the source operand into the location in the current stack frame identified by the target operand.
 {if (@_ == 2)
   {my ($target, $source) = @_;                                                 # Memory location to pop to, memory area to pop from
    $assembly->instruction(action=>"pop", target=>$target, source=>$source);
   }
  else
   {my ($target, $targetArea, $source) = @_;                                    # Memory location to pop to, memory area to pop to, memory area to pop from
    $assembly->instruction(action=>"pop", target=>$target, targetArea=>$targetArea, source=>$source);
   }
 }

sub Push($$;$)                                                                  # Push the value in the current stack frame specified by the source operand onto the memory area identified by the target operand.
 {my ($target, $source, $sourceArea) = @_;                                      # Memory area to push to, memory location containing value to push, memory area containing value to push
  $assembly->instruction(action=>"push", target=>$target, source=>$source);
 }

sub Smaller($$$)                                                                # Compare the source operands and put 0 in the target if the operands are equal, 1 if the first operand is the smaller, or 2 if the second operand is the smaller
 {my ($target, $s1, $s2) = @_;                                                  # Target location, source one, source two
  $assembly->instruction(action=>"smaller",
    target=>$target, source=>$s1, sourceArea=>$s2);
 }

sub Then(&)                                                                     # Then block
 {my ($t) = @_;                                                                 # Then block subroutine
  (then => $t)
 }

sub Else(&)                                                                     # Else block
 {my ($e) = @_;                                                                 # Else block subroutine
  (else => $e)
 }

sub Ifx($$$%)                                                                   # Execute then or else clause depending on whether two memory lcoations are equal.
 {my ($cmp, $a, $b, %options) = @_;                                             # Comparison, first memory location, second memory location, then block, else block
  confess "Then required" unless $options{then};
  if ($options{else})
   {my $else = label;
    my $end  = label;
    &$cmp($else, $a, $b);
      &{$options{then}};
      Jmp $end;
    setLabel($else);
      &{$options{else}};
    setLabel($end);
   }
  else
   {my $end  = label;
    &$cmp($end, $a, $b);
      &{$options{then}};
    setLabel($end);
   }
 }

sub IfEq($$%)                                                                   # Execute then or else clause depending on whether two memory locations are equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location, then block, else block
  Ifx(\&Jne, $a, $b, %options);
 }

sub IfNe($$%)                                                                   # Execute then or else clause depending on whether two memory locations are not equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location, then block, else block
  Ifx(\&Jeq, $a, $b, %options);
 }

sub IfLt($$%)                                                                   # Execute then or else clause depending on whether two memory locations are less than.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location, then block, else block
  Ifx(\&Jge, $a, $b, %options);
 }

sub IfLe($$%)                                                                   # Execute then or else clause depending on whether two memory locations are less than or equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location, then block, else block
  Ifx(\&Jgt, $a, $b, %options);
 }

sub IfGt($$%)                                                                   # Execute then or else clause depending on whether two memory locations are greater than.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location, then block, else block
  Ifx(\&Jge, $a, $b, %options);
 }

sub IfGe($$%)                                                                   # Execute then or else clause depending on whether two memory locations are greater than or equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location, then block, else block
  Ifx(\&Jgt, $a, $b, %options);
 }

sub Assert($$$)                                                                 # Assert
 {my ($op, $a, $b) = @_;                                                        # Operation, First memory location, second memory location
  $assembly->instruction(action=>"assert$op", source=>$a, sourceArea=>$b, level=>2);
 }

sub AssertEq($$%)                                                               # Assert two memory locations are equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  Assert("Eq", $a, $b);
 }

sub AssertNe($$%)                                                               # Assert two memory locations are not equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  Assert("Ne", $a, $b);
 }

sub AssertLt($$%)                                                               # Assert two memory locations are less than.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  Assert("Lt", $a, $b);
 }

sub AssertLe($$%)                                                               # Assert two memory locations are less than or equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  Assert("Le", $a, $b);
 }

sub AssertGt($$%)                                                               # Assert two memory locations are greater than.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  Assert("Gt", $a, $b);
 }

sub AssertGe($$%)                                                               # Assert are greater than or equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  Assert("Ge", $a, $b);
 }

sub For(%)                                                                      # For loop with initial, check, next clauses
 {my (%options) = @_;                                                           # Options
  if (my $start = $options{start})
   {&$start;
   }
  my ($Check, $Next, $End) = (label, label, label);
  if (my $check = $options{check})
   {setLabel($Check);
     &$check($End);
   }
  if (my $block = $options{block})
   {&$block($Check, $Next, $End);
   }
  if (my $next = $options{next})
   {setLabel($Next);
    &$next;
   }
  Jmp $Check;
  setLabel($End);
 }

sub Execute(%)                                                                  # Execute the current assembly
 {my (%options) = @_;                                                           # Options
  my $r = $assembly->execute(%options);                                         # Execute the code in the current assembly
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
@EXPORT_OK   = qw(areaStructure Add Alloc Call Confess Else Execute For Free AssertEq AssertGe AssertGt AssertLe AssertLt AssertNe Dec Dump IfEq IfGe IfGt IfLe IfLt IfNe Ifx Inc Jeq Jge Jgt Jle Jlt Jmp Jne Label Mov Nop Out ParamsGet ParamsPut Pop Procedure Push Return ReturnGet ReturnPut Smaller Start Then);
%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

return 1 if caller;

eval {goto latest};
sub is_deeply;
sub ok($;$);

#latest:;
if (1)                                                                          #TOut #TStart
 {Start 1;
  Out "hello World";
  ok Execute(out=>["hello World"]);
 }

#latest:;
if (1)                                                                          #TNop
 {Start 1;
  Nop;
  ok Execute(out=>[]);
 }

#latest:;
if (1)                                                                          #TMov
 {Start 1;
  Mov 1, 2;
  Out \1;
  ok Execute(out=>[2]);
 }

#latest:;
if (1)
 {Start 1;                                                                      #TMov
  Mov  1, 3;
  Mov  2, 1;
  Mov  3, \\2;
  Out \3;
  ok Execute(out=>[3]);
 }

#latest:;
if (1)                                                                          #TAdd
 {Start 1;
  Add  \1, 3, 2;
  Out  \1;
  ok Execute(out=>[5]);
 }

#latest:;
if (1)                                                                          #TAdd
 {Start 1;
  Mov   1, 1;
  Mov   2, 2;
  Mov   3, 0;
  Mov   4, 3;
  Add  \4, \1, \2;
  Out  3;
  ok Execute(out=>[3]);
 }

#latest:;
if (1)                                                                          #TDec
 {Start 1;
  Mov  1, 3;
  Dec \1;
  Out \1;
  ok Execute(out=>[2]);
 }

#latest:;
if (1)                                                                          #TInc
 {Start 1;
  Mov  1, 1;
  Inc \1;
  Out \1;
  Mov  1, 0, 2, undef;
  Inc \1, 0;
  Mov  0, \1, 0;
  Out \0;
  ok Execute(out=>[2,3]);
 }

#latest:;
if (1)                                                                          #TSmaller
 {Start 1;
  Smaller 1, 1, 2;
  Out   1;
  ok Execute(out=>[1]);
 }

#latest:;
if (1)                                                                          #TSmaller
 {Start 1;
  Mov   1, 1;
  Mov   2, 2;
  Smaller 3, \1, \2;
  Out  \3;
  ok Execute(out=>[1]);
 }

#latest:;
if (1)                                                                          #TJmp
 {Start 1;
  Jmp (my $a = label);
    Out  1;
    Jmp (my $b = label);
  setLabel($a);
    Out  2;
  setLabel($b);
  ok Execute(out=>[2]);
 }

#latest:;
if (1)                                                                          #TJLt #TLabel
 {Start 1;
  Mov 0, 1;
  Jlt ((my $a = label), \0, 2);
    Out  1;
    Jmp (my $b = label);
  setLabel($a);
    Out  2;
  setLabel($b);

  Jgt ((my $c = label), \0, 3);
    Out  1;
    Jmp (my $d = label);
  setLabel($c);
    Out  2;
  setLabel($d);
  ok Execute(out=>[2,1]);
 }

#latest:;
if (1)                                                                          #TLabel
 {Start 1;
  Mov 0, 0;
  my $a = setLabel;
    Out \0;
    Inc \0;
  Jlt $a, \0, 10;
  ok Execute(out=>[0..9]);
 }

#latest:;
if (1)                                                                          #TMov #TMov
 {Start 1;
  Mov  0, 0,  1, undef;
  Mov  0, \0, 0;
  Out \0;
  ok Execute(out=>[1]);
 }

#latest:;
if (1)                                                                          #TCall Call a subroutine with no parameters
 {Start 1;
  my $w = Procedure 'write', sub
   {Out 'aaa';
    Return;
   };
  Call $w;
  ok Execute(out=>['aaa']);
 }

#latest:;
if (1)                                                                          #TCall Call a subroutine with one parameter
 {Start 1;
  Jmp (my $start = label());
  my $w = setLabel 'write';
    ParamsGet \0, \0;
    Out \0;
  Return;
  setLabel $start;
    ParamsPut 0, 'bbb';
    Call $w;
  ok Execute(out=>['bbb']);
 }

#latest:;
if (1)                                                                          #TCall Call a subroutine returning one value
 {Start 1;
  Jmp (my $start = label());
  my $l = setLabel 'write';
    ReturnPut 0, "ccc";
  Return;
  setLabel $start;
    Call $l;
    ReturnGet \0, \0;
    Out \0;
  ok Execute(out=>['ccc']);
 }

#latest:;
if (1)                                                                          #TProcedure
 {Start 1;
  my $add = Procedure 'add2', sub
   {my ($p) = @_;                                                               # Procedure description
    my ($a, $b) = $p->variables->names(qw(a b));
    ParamsGet $a, \0;
    Add $b, $a, 2;
    ReturnPut 0, $b;
    Return;
   };
  ParamsPut 0, 2;
  Call $add;
  ReturnGet \0, \0;
  Out \0;
  ok Execute(out=>[4]);
 }

#latest:;
if (1)                                                                          #TConfess
 {Start 1;
  Jmp (my $start = label());
  my $c = setLabel 'write';
    Confess;
  Return;
  Label $start;
    Call $c;
  ok Execute(suppressStackTracePrint=>1, out=>
[ "Stack trace\n",
  "    2     3 confess\n",
  "    1     6 call\n"]);
 }

#latest:;
if (1)                                                                          #TPush
 {Start 1;
  Push 1, 1;
  Push 1, 2;
  is_deeply Execute()->memory->{1}, [1..2];
 }

#latest:;
if (1)                                                                          #TPop
 {Start 1;
  Push -1, 1;
  Push -1, 2;
  Pop  0, -1;
  my $r = Execute();
  is_deeply $r->memory, { "-1" => [1], "0" => [2], "1" => [], "2" => [] };
 }

#latest:;
if (1)                                                                          #TPush #TPop
 {Start 1;
  Push 1, 1;
  Push 1, 2;
  Pop  0, 1;
  Pop  1, 1;
  my $r = Execute;
  is_deeply $r->memory, { "0" => [2, 1], "1" => [], "2" => [] };
 }

#latest:;
if (1)                                                                          #TAlloc #TMov #TMov
 {Start 1;
  Alloc 0;
  Mov 1, \0,  1, undef;
  Mov 2, \0,  2, undef;
  Mov 1, \1, \0;
  Mov 2, \2, \0;
  my $r = Execute;
  is_deeply $r->memory, { "0" => [3, 1, 2], "1" => [], "2" => [], "3" => [undef, 1, 2] };
 }

#latest:;
if (1)                                                                          #TMov
 {Start 1;
  Mov 0, 0, 1, undef;
  Mov 0, 1, \0, 0;
  Mov 1, 1, \0;
  my $r = Execute;
  is_deeply $r->memory, { "0" => [1, 1], "1" => [1], "2" => [] };
 }

#latest:;
if (1)                                                                          #TFree
 {Start 1;
  Alloc 0;
  Out \0;
  Free \0;
  my $r = Execute;
  is_deeply $r->memory, { "0" => [3], "1" => [], "2" => [] };
 }

#latest:;
if (1)                                                                          # Layout
 {my $s = Start 1;
  my ($a, $b, $c) = $s->variables->names(qw(a b c));
  Mov $a, 'A';
  Mov $b, 'B';
  Mov $c, 'C';
  Out $c;
  Out $b;
  Out $a;
  ok Execute(out=>[qw(C B A)]);
 }

#latest:;
if (1)                                                                          #TIfEq
 {my $s = Start 1;
  my ($a, $b, $c) = $s->variables->names(qw(a b c));
  Mov $a, 1;
  Mov $b, 2;
  IfEq $a, $b,
  Then
   {Out 'Equal';
   };
  ok Execute(out=>[]);
 }

#latest:;
if (1)                                                                          #TFor
 {my $s = Start 1;
  my ($a) = $s->variables->names(qw(a));
  For start => sub{Mov $a, 0},
      check => sub{Jge  $_[0], $a, 10},
      next  => sub{Inc $a},
      block => sub{Out $a};
  ok Execute(out=>[0..9]);
 }

#latest:;
if (1)                                                                          #TAssertEq
 {my $s = Start 1;
  Mov 0, 1;
  AssertEq \0, 2;
  my $r = Execute(suppressStackTracePrint=>1);
  is_deeply $r->out, [
"Stack trace\n",
  "    1     2 assertEq\n",
];
 }

#latest:;
if (1)                                                                          # Temporary variable
 {my $s = Start 1;
  my ($a) = $s->variables->temporary;
  my ($b) = $s->variables->name(q(b));
  Mov $a, 1;
  Mov $b, 2;
  Out $a;
  Out $b;
  ok Execute(out=>[1..2]);
 }
