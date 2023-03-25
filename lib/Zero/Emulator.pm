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

=cut

makeDieConfess;

sub maximumInstructionsToExecute (){100}                                        # Maximum number of subroutines to execute
sub wellKnownMemoryAreas         (){1e6}                                        # Memory areas with a number less than this are well known. They can be used globally but cannot be freed

sub areaStructure($@)                                                           # Describe a data structure mapping a memory area
 {my ($name, @fields) = @_;                                                     # Structure name, fields names

  my $d = genHash("Zero::Emulator::areaStructure",                              # Description of a data structure mapping a memory area
    name  => $name,                                                             # Name of the structure
    order => [],                                                                # Order of the elements in the structure, in effect, giving the offset of each element in the data structure
    names => {},                                                                # Maps the names of the fields to their offsets in the structure
   );
  $d->field($_) for @fields;                                                    # Add the field descriptions
  $d
 }

sub Zero::Emulator::areaStructure::field($$)                                    # Add a field to a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  if (!$d->names->{$name})
   {$d->names->{$name} = $d->order->@*;
    push $d->order->@*, $name;
   }
  else
   {confess "Duplicate name: $name in structure: ".$d->name;
   }
  \($d->names->{$name})
 }

sub Zero::Emulator::areaStructure::fields($@)                                   # Add fields to a data structure
 {my ($d, @names) = @_;                                                         # Parameters
  map {$d->field($_)} @names;
 }

sub Zero::Emulator::areaStructure::offset($$)                                   # Offset of a field in a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  if (my $n = $d->names->{$name}){return $n}
  confess "No such name: $name in structure: ".$d->name;
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
    target    => $options{target},                                              # The location of the subroutine being called
    call      => $options{call},                                                # The location of the call instruction making the call
    stackArea => $options{stackArea},                                           # Memory area containing data for this method
    params    => $options{params},                                              # Memory area containing paramter list
    return    => $options{return},                                              # Memory area conmtaining returned result
    line      => $options{line},                                                # The line number from which the call was made
    file      => $options{file},                                                # The file number from which the call was made - this could be folded into the line number but for reasons best known to themselves people who cannot program very well often scatter projects across several files a practice that is completely pointless in this day of git and so can only lead to chaos and confusion
  );
 }

sub Zero::Emulator::Code::instruction($%)                                       # Create a new instruction
 {my ($block, %options) = @_;                                                   # Block of code desctriptor, options

  if ($options{action} =~ m(\Avariable\Z)i)                                     # Variable
   {$block->variables->{$options{target}};
   }
  else
   {push $block->code->@*, genHash("Zero::Emulator::Code::Instruction",         # Instruction details
      action        => $options{action      },                                  # Instruction name
      number        => $options{number      },                                  # Instruction sequence number
      owner         => $options{owner       },
      source        => $options{source      },                                  # Source memory location
      source2       => $options{source2     },                                  # Secondary source memory location
      target        => $options{target      },                                  # Target memory location
      target2       => $options{target2     },                                  # Target secondary memory location
      source_area   => $options{source_area },                                  # Source area
      target_area   => $options{target_area },                                  # Target area
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
  my %owner;                                                                    # Who owns the privilege of writing to the corresponding block of memory. undef means that any-one can write, otherwise the instruction must have the matching owner id

  my sub stackTraceAndExit($)                                                   # Print a stack trace and exit
   {my ($i) = @_;                                                               # Instruction trace occurred at
    push @out, "Stack trace\n";
    for my $j(reverse keys @calls)
     {my $c = $calls[$j];
      if (my $I = $c->call)
       {push @out, sprintf "%4d Call\n", $j+1;
       }
      else
       {push @out, sprintf "%4d ????\n", $j+1;
       }
     }
    $instructionPointer = @$code;                                               # Execution terminates as soon as undefined instuction is encountered
   };

  my sub stackArea()                                                            # Memory area associated with this method invocation
   {$calls[-1]->stackArea;                                                      # Stack area
   }

  my sub allocMemory()                                                          # Create the name of a new memory area
   {my $a = &wellKnownMemoryAreas + scalar(keys %memory);
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
    die "Invalid left area.address: ".dump([$area, $a])."\n";
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
    die "Invalid right area.address: ".dump([$area, $a])."\n" unless defined $r;
    $r
   }

  my sub setMemory($$;$)                                                        # Set a memory location in the current stack frame to a specified value
   {my ($target, $value, $area) = @_;                                           # Target, value, optional area
    my $a = left($target, $area);
    $$a   = $value;
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

    alloc     => sub                                                            # Create a new memory area and write its number into the location named by the target operand
     {my ($i) = @_;                                                             # Instruction
      setMemory right($i->target), allocMemory;
     },

    free      => sub                                                            # Free the memory area named by the source operand
     {my ($i) = @_;                                                             # Instruction
      delete $memory{right($i->source)};
     },

    call      => sub                                                            # Call a subroutine
     {my ($i) = @_;                                                             # Instruction
      my $t = $i->target;                                                       # Subroutine to call

      if (isScalar($t))
       {$instructionPointer = $i->number + $t;                                  # Relative call if we know where the subroutine is relative to the call instruction
       }
      else
       {$instructionPointer = $t;                                               # Absolute call
       }
      push @calls, stackFrame(target=>$code->[$instructionPointer], call=>$i,   # Create a new call stack entry
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
      stackTraceAndExit($i);
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

    mov       => sub                                                            # Move data moves data from one part of memory to another - "set", by contrast, sets variables from constant values
     {my ($i) = @_;                                                             # Instruction
      setMemory $i->target, right($i->source);
     },

    get       => sub                                                            # Copy one word from the area identified by the first source operand at the location identified by the second source operand to the target location on the current area.
     {my ($i) = @_;                                                             # Instruction
      my $s = right($i->source2, $i->source);
      my $t = left($i->target);
      $$t = $s;
     },

    put       => sub                                                            # Copy one word from the current area to the area identified by the first source operand at the location identified by the second source operand to the target location on the current area.
     {my ($i) = @_;                                                             # Instruction
      my $t = left($i->target2, $i->target);
      my $s = right($i->source);
      $$t = $s;
     },

    copy      => sub                                                            # Move one word from the area identified by the first source operand at the location identified by the second source operand to the area indic ated by the irst target operand at the location specified bythe second target operand.
     {my ($i) = @_;                                                             # Instruction
      my $s = right($i->source2, $i->source);
      my $t = left ($i->target2, $i->target);
      $$t = $s;
     },

    paramsGet => sub                                                            # Get a parameter from the previous parameter block - this means that we must always have two entries on teh call stack - one representing the caller of the program, the second representing the current context of the program
     {my ($i) = @_;                                                             # Instruction
      my $p = $calls[-2]->params;
     #setMemory ${$i->target}, $memory{$p}[right($i->source)];
      my $t = left($i->target);
      my $s = right($i->source, $p);
      $$t = $s;
     },

    paramsPut => sub                                                            # Place a parameter in the current parameter block
     {my ($i) = @_;                                                             # Instruction
      my $p = $calls[-1]->params;
      #$memory{$p}[right($i->target)] = right($i->source);
      my $t = left($i->target, $p);
      my $s = right($i->source);
      $$t = $s;
     },

    returnGet => sub                                                            # Get a word from the return area
     {my ($i) = @_;                                                             # Instruction
      my $p = $calls[-1]->return;
      #setMemory ${$i->target}, $memory{$p}[right($i->source)];
      my $t = left($i->target);
      my $s = right($i->source, $p);
      $$t = $s;
     },

    returnPut => sub                                                            # Put a word ino the return area
     {my ($i) = @_;                                                             # Instruction
      my $p = $calls[-2]->return;
      #$memory{$p}[right($i->target)] = right($i->source);
      my $t = left($i->target, $p);
      my $s = right($i->source);
      $$t = $s;
     },

    nop       => sub                                                            # No operation
     {my ($i) = @_;                                                             # Instruction
     },

    out     => sub                                                              # Write source as output to an array of words
     {my ($i) = @_;                                                             # Instruction
      push @out, right($i->source);
     },

    pop => sub                                                                  # Pop a value from the specified memory area if possible else confess
     {my ($i) = @_;                                                             # Instruction
      my $p = right($i->source);
      if (!defined($memory{$p}) or !$memory{$p}->@*)                            # Stack no poppable
       {confess($i);
       }
      setMemory right($i->target), pop $memory{$p}->@*;                         # Pop from memory area into current stack frame
     },

    push => sub                                                                 # Push a value onto the specified memory area
     {my ($i) = @_;                                                             # Instruction
      push $memory{right($i->target)}->@*, right($i->source);
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

  push @calls, stackFrame(                                                      # Initial stack entries
    stackArea => allocMemory,                                                   # Allocate data segment for current method
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
      eval {$c->($i)};                                                          # Execute instruction
      if ($@)                                                                   # Handle any errror produced during subroutine execution
       {say STDERR $@;
        last;
       }
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

sub Add($$$)                                                                    # Copy the contents of the source location to the target location
 {my ($target, $s1, $s2) = @_;                                                  # Target location, source one, source two
  $assembly->instruction(action=>"add",
    target=>$target, source=>$s1, source2=>$s2);
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

sub Out($)                                                                      # Write memory contents to out
 {my ($source) = @_;                                                            # Memory location to output
  $assembly->instruction(action=>"out", source=>$source);
 }

sub Procedure($$)                                                               # Define a procedure
 {my ($name, $source) = @_;                                                     # Name of procedure, source code as a subroutine# $assembly->instruction(action=>"procedure", target=>$target, source=>$source);
  if ($name and $assembly->procedures->{$name})
   {confess "Procedure already defined with name: $name\n";
   }

  Jmp(my $end = label);                                                         # Jump over the code of the procedure body
  my $start = setLabel;
  &$source;                                                                     # Code of procedure
  setLabel $end;

  procedure($start)                                                             # Return a description of the procedure
 }

sub ParamsGet($$)                                                               # Get a word from the parameters in the previous frame and store it in the local stack frame
 {my ($target, $source) = @_;                                                   # Memory location to place parameter in, parameter to get
  $assembly->instruction(action=>"paramsGet", target=>$target, source=>$source);
 }

sub ParamsPut($$)                                                               # Put a parameter into the current frame
 {my ($target, $source) = @_;                                                   # Offset in parameter area to write to, memory location whose contents are to be used as a parameter
  $assembly->instruction(action=>"paramsPut", target=>$target, source=>$source);
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

sub Pop($$)                                                                     # Pop the memory area specified by the source operand into the location in the current stack frame identified by the target operand.
 {my ($target, $source) = @_;                                                   # Memory area, value
  $assembly->instruction(action=>"pop", target=>$target, source=>$source);
 }

sub Push($$)                                                                    # Push the value in the current stack frame specified by the source operand onto the memory area identified by the target operand.
 {my ($target, $source) = @_;                                                   # Memory area, value
  $assembly->instruction(action=>"push", target=>$target, source=>$source);
 }

sub Smaller($$$)                                                                # Compare the source operands and put 0 in the target if the operands are equal, 1 if the first operand is the smaller, or 2 if the second operand is the smaller
 {my ($target, $s1, $s2) = @_;                                                  # Target location, source one, source two
  $assembly->instruction(action=>"smaller",
    target=>$target, source=>$s1, source2=>$s2);
 }

sub Get($$$)                                                                    # Copy one word from another memory area to the curent stack area
 {my ($target, $s1, $s2) = @_;                                                  # Target location, source area, source location
  $assembly->instruction(action=>"get",
    target=>$target, source=>$s1, source2=>$s2);
 }

sub Put($$$)                                                                    # Copy one word from the current stack area to another memory area
 {my ($t1, $t2, $source) = @_;                                                  # Target location, source area, source location
  $assembly->instruction(action=>"put",
    target=>$t1, target2=>$t2, source=>$source);
 }

sub Copy($$$$)                                                                  # Copy one word from one area to another area
 {my ($t1, $t2, $s1, $s2) = @_;                                                 # Target area, target location, source area, source location
  $assembly->instruction(action=>"copy",
    target=>$t1, target2=>$t2, source=>$s1, source2=>$s2);
 }

sub Variable($)                                                                 # Create a variable in the cirent stack frame during assembly
 {my ($name) = @_;                                                              # Variable name
  $assembly->instruction(action=>"variable", target=>$name);
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
@EXPORT_OK   = qw(areaStructure Start Add Call Confess Inc Jmp JLe JLt JGe JGt JEq JNe Label Mov Nop Out ParamsGet ParamsPut Return ReturnGet ReturnPut Pop Push Smaller Get Put Execute);
%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

return 1 if caller;

eval {goto latest};
sub is_deeply;
sub ok($;$);

if (1)                                                                          #TOut #TStart
 {Start 1;
  Out "hello World";
  ok Execute(out=>["hello World"]);
 }

if (1)                                                                          #TNop
 {Start 1;
  Nop;
  ok Execute(out=>[]);
 }

if (1)                                                                          #TMov
 {Start 1;
  Mov 1, 2;
  Out \1;
  ok Execute(out=>[2]);
 }

if (1)
 {Start 1;                                                                      #TMov
  Mov  1, 3;
  Mov  2, 1;
  Mov  3, \\2;
  Out \3;
  ok Execute(out=>[3]);
 }

if (1)                                                                          #TAdd
 {Start 1;
  Add  \1, 3, 2;
  Out  \1;
  ok Execute(out=>[5]);
 }

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

if (1)                                                                          #TInc
 {Start 1;
  Mov  1, 1;
  Inc \1;
  Out \1;
  ok Execute(out=>[2]);
 }

if (1)                                                                          #TSmaller
 {Start 1;
  Smaller 1, 1, 2;
  Out   1;
  ok Execute(out=>[1]);
 }

if (1)                                                                          #TSmaller
 {Start 1;
  Mov   1, 1;
  Mov   2, 2;
  Smaller 3, \1, \2;
  Out  \3;
  ok Execute(out=>[1]);
 }

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

if (1)                                                                          #TJLt #TLabel
 {Start 1;
  Mov 0, 1;
  JLt ((my $a = label), \0, 2);
    Out  1;
    Jmp (my $b = label);
  setLabel($a);
    Out  2;
  setLabel($b);

  JGt ((my $c = label), \0, 3);
    Out  1;
    Jmp (my $d = label);
  setLabel($c);
    Out  2;
  setLabel($d);
  ok Execute(out=>[2,1]);
 }

if (1)                                                                          #TLabel
 {Start 1;
  Mov 0, 0;
  my $a = setLabel;
    Out \0;
    Inc \0;
  JLt $a, \0, 10;
  ok Execute(out=>[0..9]);
 }

if (1)                                                                          #TPut #TGet
 {Start 1;
  Put  0, 0,  1;
  Get  0, 0,  \0;
  Out \0;
  ok Execute(out=>[1]);
 }

if (1)                                                                          #TCall Call a subroutine with no parameters
 {Start 1;
  Jmp (my $start = label());
  my $w = setLabel 'write';
    Out 'aaa';
  Return;
  setLabel $start;
    Call $w;
  ok Execute(out=>['aaa']);
 }

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

if (1)                                                                          #TProcedure
 {Start 1;
  my $add = Procedure 'add2', sub
   {ParamsGet 0, \0;
    Add 0, \0, 2;
    ReturnPut 0, \0;
    Return;
   };
  ParamsPut 0, 2;
  $add->call;
  ReturnGet \0, \0;
  Out \0;
  ok Execute(out=>[4]);
 }

if (1)                                                                          #TConfess
 {Start 1;
  Jmp (my $start = label());
  my $c = setLabel 'write';
    Confess;
  Return;
  Label $start;
    Call $c;
  ok Execute(out=>["Stack trace\n", "   3 Call\n", "   2 ????\n", "   1 ????\n"]);
 }

if (1)                                                                          #TPush
 {Start 1;
  Push 1, 1;
  Push 1, 2;
  is_deeply Execute()->memory->{1}, [1..2];
 }

if (1)                                                                          #TPop
 {Start 1;
  Push 1, 1;
  Push 1, 2;
  Pop  0, 1;
  my $r = Execute();
  is_deeply $r->memory->{1}, [1];
  is_deeply $r->memory->{1000003}, [2];
 }

if (1)                                                                          #TPush #TPop
 {Start 1;
  Push 1, 1;
  Push 1, 2;
  Pop  0, 1;
  Pop  1, 1;
  my $r = Execute;
  is_deeply $r->memory->{1}, [];
  is_deeply $r->memory->{1000003}, [2, 1];
 }

if (1)                                                                          #TAlloc #TGet #TPut
 {Start 1;
  Alloc 0;
  Put \0, 1, 1;
  Put \0, 2, 2;
  Get 1, \0, \1;
  Get 2, \0, \2;
  my $r = Execute;
  is_deeply $r->memory->{1000003}, [1000006, 1, 2];
  is_deeply $r->memory->{1000006}, [undef, 1,2];
 }

if (1)                                                                          #TCopy
 {Start 1;
  Put  0, 0, 1;
  Copy 1, 0, 0, \0;
  Get  1, 1, \0;
  my $r = Execute;
  is_deeply $r->memory, {
  "0" => [1],
  "1" => [1],
  "1000000" => [],
  "1000001" => [],
  "1000002" => [],
  "1000003" => [undef, 1],
  "1000004" => [],
  "1000005" => [],
};
 }

#latest:;
if (1)                                                                          #TFree
 {Start 1;
  Alloc 0;
  Out \0;
  Free \0;
  my $r = Execute;
  is_deeply $r->memory, {
              1000000 => [],
              1000001 => [],
              1000002 => [],
              1000003 => [1000006],
              1000004 => [],
              1000005 => [],
            };
 }

#latest:;
if (1)                                                                          # Layout
 {my $s = Start 1;
  my ($a, $b, $c) = $s->variables->fields(qw(a b c));
  Mov $a, 'A';
  Mov $b, 'B';
  Mov $c, 'C';
  Out $c;
  Out $b;
  Out $a;
  ok Execute(out=>[qw(C B A)]);
 }
