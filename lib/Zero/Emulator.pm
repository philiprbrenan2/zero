#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/
#-------------------------------------------------------------------------------
# Assemble and execute the Zero programming language. Examples at the end.
# Philip R Brenan at appaapps dot com, Appa Apps Ltd Inc., 2023
#-------------------------------------------------------------------------------
# Pointless adds and subtracts by 0. Perhaps we should flag adds and subtracts by 1 as well so we can have an instruction optimized for these variants.
use v5.30;
package Zero::Emulator;
use warnings FATAL => qw(all);
use strict;
use Carp qw(cluck confess);
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
eval "use Test::More tests=>53" unless caller;

makeDieConfess;

my sub maximumInstructionsToExecute {1e5}                                       # Maximum number of subroutines to execute

my sub Code(%)                                                                  # A block of code
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::Code",                                               # Description of a call stack entry
    assembled    => undef,                                                      # Needs to be assembled unless this field is true
    code         => [],                                                         # An array of instructions
    variables    => AreaStructure("Variables"),                                 # Variables in this block of code
    labels       => {},                                                         # Label name to instruction
    labelCounter => 0,                                                          # Label counter used to generate unique labels
    files        => [],                                                         # File number to file name
    procedures   => {},                                                         # Procedures defined in this block of code
    %options,
   );
 }

my sub stackFrame(%)                                                            # Describe an entry on the call stack: the return address, the parameter list length, the parameter list location, the line of code from which the call was made, the file number of the file from which the call was made
 {my (%options) = @_;                                                           # Parameters

  genHash("Zero::Emulator::StackFrame",                                         # Description of a stack frame. A stack frame provides the context in which a method runs.
    target      => $options{target},                                            # The location of the subroutine being called
    instruction => $options{call},                                              # The location of the instruction making the call
    stackArea   => $options{stackArea},                                         # Memory area containing data for this method
    params      => $options{params},                                            # Memory area containing paramter list
    return      => $options{return},                                            # Memory area conmtaining returned result
    line        => $options{line},                                              # The line number from which the call was made
    file        => $options{file},                                              # The file number from which the call was made - this could be folded into the line number but for reasons best known to themselves people who cannot program very well often scatter projects across several files a practice that is completely pointless in this day of git and so can only lead to chaos and confusion
    variables   => $options{variables},                                         # Variables local to this stack frame
  );
 }

sub Zero::Emulator::Code::instruction($%)                                       # Create a new instruction
 {my ($block, %options) = @_;                                                   # Block of code desctriptor, options

  my ($package, $fileName, $line) = caller($options{level} // 1);

  my sub stackTrace()                                                           # File numbers and line numbers of callers
   {my @s;
    for my $c(1..99)
     {my @c = caller($c);
      last unless @c;
      push @s, [$c[1], $c[2]];
     }
    \@s
   };

  if ($options{action} !~ m(\Avariable\Z)i)                                     # Non variable
   {push $block->code->@*, my $i = genHash("Zero::Emulator::Code::Instruction", # Instruction details
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
#     variables     => $block->variables,                                       # The Area Structure currently in use when this instruction was created
      context       => stackTrace(),                                            # The call context in which this instruction was created
      executed      => 0,                                                       # The number of times this instruction was executed
    );
    return $i;
   }
 }

sub Zero::Emulator::Code::Instruction::contextString($;$)                       # Stack trace back for this instruction
 {my ($i, $title) = @_;                                                         # Instruction, options
  @_ == 1 or @_ == 2 or confess "One or Two parameters";
  my @s;
  push @s, $title if defined $title;
  for my $c($i->context->@*)
   {push @s, sprintf "    at %s line %d", $$c[0], $$c[1];
   }
  join "\n", @s
 }

sub AreaStructure($@)                                                           # Describe a data structure mapping a memory area
 {my ($structureName, @names) = @_;                                             # Structure name, fields names

  my $d = genHash("Zero::Emulator::AreaStructure",                              # Description of a data structure mapping a memory area
    structureName => $structureName,                                            # Name of the structure
    fieldOrder    => [],                                                        # Order of the elements in the structure, in effect, giving the offset of each element in the data structure
    fieldNames    => {},                                                        # Maps the names of the fields to their offsets in the structure
    instructions3  => [],                                                        # The variable instruction associated with this variable
   );
  $d->field($_) for @names;                                                     # Add the field descriptions
  $d
 }

sub Zero::Emulator::AreaStructure::count($)                                     # Add a field to a data structure
 {my ($d) = @_;                                                                 # Area structure
  scalar $d->fieldOrder->@*
 }

sub Zero::Emulator::AreaStructure::name($$)                                     # Add a field to a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  @_ == 2 or confess "Two parameters";
  if (!$d->fieldNames->{$name})
   {$d->fieldNames->{$name} = $d->fieldOrder->@*;
    push $d->fieldOrder->@*, $name;
   }
  else
   {confess "Duplicate name: $name in structure: ".$d->name;
   }
  \($d->fieldNames->{$name})
 }

my sub procedure($%)                                                            # Describe a procedure
 {my ($label, %options) = @_;                                                   # Start label of procedure, options describing procedure

  genHash("Zero::Emulator::Procedure",                                          # Description of a procedure
    target       => $label,                                                     # Label to call to call this procedure
    variables    => AreaStructure("Procedure"),                                 # Registers local to this procedure
  );
 }

sub Zero::Emulator::AreaStructure::registers($)                                 # Create one or more temporary variables. Need to reuse registers no longer in use
 {my ($d, $count) = @_;                                                         # Parameters
  @_ == 1 or confess "One parameter";
  if (!defined($count))
   {my $o = $d->fieldOrder->@*;
    push $d->fieldOrder->@*, undef;
    return \$o;                                                                 # One temporary
   }
  map {__SUB__->($d)} 1..$count;                                                # Array of temporaries
 }

sub Zero::Emulator::AreaStructure::offset($$)                                   # Offset of a field in a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  @_ == 2 or confess "Two parameters";
  if (defined(my $n = $d->fieldNames->{$name})){return $n}
  confess "No such name: '$name' in structure: ".$d->structureName;
 }

sub Zero::Emulator::AreaStructure::address($$)                                  # Address of a field in a data structure
 {my ($d, $name) = @_;                                                          # Parameters
  @_ == 2 or confess "Two parameters";
  if (defined(my $n = $d->fieldNames->{$name})){return \$n}
  confess "No such name: '$name' in structure: ".$d->structureName;
 }

sub Zero::Emulator::Procedure::registers($)                                     # Allocate a register within a procedure
 {my ($procedure) = @_;                                                         # Procedure description
  @_ == 1 or confess "One parameter";
  $procedure->variables->registers();
 }

my sub Reference($)                                                             # Record a reference to memory
 {my ($r) = @_;                                                                 # Reference
  @_ == 1 or confess "One parameter";
  genHash("Zero::Emulator::Reference",
    area    => ref($r) =~ m(array)i ? $$r[0] : undef,
    address => ref($r) =~ m(array)i ? $$r[1] : $r,
  );
 }

sub Zero::Emulator::Reference::print($)                                         # Print the value of an address
 {my ($ref) = @_;                                                               # Reference specification
  @_ == 1 or confess "One parameter";
  my $a  = dump($ref->area);
  my $l  = dump($ref->address);
  my $s  = "Reference area: $a, address: $l";
  say STDERR $s;
 }

sub Zero::Emulator::Address::print($$)                                          # Print the value of an address in the current execution
 {my ($address, $exec) = @_;                                                    # Address specification
  @_ == 2 or confess "Two parameters";
  #my $e  = $address->exec;
  my $e  = $exec;
  my $m  = $e->memory;
  my $t  = $e->memoryType;
  my $a  = $address->area;
  my $l  = $address->location;
  my $s  = "Address area: $a";
     $s .= "(".($$t{$a} // "unknown")."), ";
     $s .= "location: $l";
 }

sub Zero::Emulator::Address::get($$)                                            # Get the value of an address at the specified location in memory in the specified execution environment
 {my ($address, $exec) = @_;                                                    # Address specification
  @_ == 2 or confess "Two parameters";
#  my $e = $address->exec;
  my $e = $exec;
  my $m = $e->memory;
  my $a = $address->area;
  my $l = $address->location;
  $$m{$a}[$l]
 }

sub Zero::Emulator::Address::at($$)                                             # Reference to the specified location in memory of current execution environment
 {my ($address, $exec) = @_;                                                    # Address specification
  @_ == 2 or confess "Two parameters";
# my $e = $address->exec;
  my $e = $exec;
  my $m = $e->memory;
  my $a = $address->area;
  my $l = $address->location;
  \$$m{$a}[$l]
 }

sub Zero::Emulator::Address::set($$$)                                           # Set the value of an address at the specified location in memory in the current execution environment
 {my ($address, $value, $exec) = @_;                                            # Address specification, value
  @_ == 3 or confess "Three parameters";
# my $e = $address->exec;
  my $e = $exec;
  my $m = $e->memory;
  my $a = $address->area;
  my $l = $address->location;
  $address->print($exec);
  $$m{$a}[$l] = $value
 }

sub Zero::Emulator::Address::areaContent($$)                                    # Content of an area containing a location in memiry in the specified execution
 {my ($address, $exec) = @_;                                                    # Address specification, execution environment
  @_ == 2 or confess "Two parameters";
# my $e = $address->exec;
  my $e = $exec;
  my $m = $e->memory;
  my $a = $address->area;
  my $A = $$m{$a};
  confess "Invalid area: ".dump($a)."\n".dump($e->memory) unless defined $A;
  @$A
 }

sub Zero::Emulator::Procedure::call($)                                          # Call a procedure.  Arguments are supplied by the ParamsPut and Get commands, return values are supplied by the ReturnPut and Get commands.
 {my ($procedure) = @_;                                                         # Procedure description
  @_ == 1 or confess "One parameter";
  Zero::Emulator::Call($procedure->target);
 }

my sub isScalar($)                                                              # Check whether an element is a scalar or an array
 {my ($value) = @_;                                                             # Parameters
  ! ref $value;
 }

sub Zero::Emulator::Code::registers($)                                          # Allocate registers
 {my ($code, $number) = @_;                                                     # Code block, number of registers required
  @_ == 1 or confess "One parameter";
  $code->variables->registers
 }

sub Zero::Emulator::Execution::dumpMemory($;$)                                  # Dump memory
 {my ($exec, $title) = @_;                                                      # Execution, Instruction, title
  @_ == 1 or @_ == 2 or confess "One or Two parameters";
  my %memory = $exec->memory->%*;
  my @m;
  for my $m(sort {$a <=> $b} keys %memory)
   {next if ref($m) =~ m(\Astack\Z)i;
    my $l = dump($memory{$m});
    $l = substr($l, 0, 100) if length($l) > 100;
    push @m, "$m=$l";
   }

  my $t = $title ? " $title" : '';

  "memory$t:\n". dump(\@m);
 }

sub Zero::Emulator::Code::assemble($%)                                          # Assemble a block of code to prepare it for execution
 {my ($Block, %options) = @_;                                                   # Code block, assembly options
  return $Block if $Block->assembled;                                           # Already assembled
  my $code = $Block->code;                                                      # The code to be assembled
  my $vars = $Block->variables;                                                 # The varaibles refernced by the code

  my %labels;                                                                   # Load labels
  my $stackFrame = AreaStructure("Stack");                                      # The current stack frame we are creating variables in

  for my $c(keys @$code)                                                        # Labels
   {my $i = $$code[$c];
    $i->number = $c;
    next unless $i->action eq "label";
    $labels{$i->source->address} = $i;                                          # Point label to instruction
   }

  for my $c(keys @$code)                                                        # Target jump and call instructions
   {my $i = $$code[$c];
    next unless $i->action =~ m(\A(j|call))i;
    if (my $l = $i->target->address)                                            # Label
     {if (my $t = $labels{$l})                                                  # Found label
       {$i->target = Reference($t->number - $c);                                # Relative jump
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

sub Zero::Emulator::Execution::analyzeExecutionResultsLeast($%)                 # Analyze execution results for least used code
 {my ($exec, %options) = @_;                                                    # Execution results, options

  my @c = $exec->code->code->@*;
  my %l;
  for my $i(@c)                                                                 # Count executions of each instruction
   {$l{$i->file}{$i->line} += $i->executed unless $i->action =~ m(\Aassert)i;
   }

  my @L;
  for   my $f(keys %l)
   {for my $l(keys $l{$f}->%*)
     {push @L, [$l{$f}{$l}, $f, $l];
     }
   }
  my @l = sort {$$a[0] <=> $$b[0]}                                              # By frequency
          sort {$$a[2] <=> $$b[2]} @L;                                          # By line number

  my $N = $options{least}//1;
  $#l = $N if @l > $N;
  map {sprintf "%4d at %s line %4d", $$_[0], $$_[1], $$_[2]} @l;
 }

sub Zero::Emulator::Execution::analyzeExecutionResultsMost($%)                  # Analyze execution results for most used code
 {my ($exec, %options) = @_;                                                    # Execution results, options

  my @c = $exec->code->code->@*;
  my %m;
  for my $i(@c)                                                                 # Count executions of each instruction
   {my $t =                                                                     # Traceback
     join "\n", map {sprintf "    at %s line %4d", $$_[0], $$_[1]} $i->context->@*;
    $m{$t} += $i->executed;
   }
  my @m = reverse sort {$$a[1] <=> $$b[1]} map {[$_, $m{$_}]} keys %m;          # Sort a hash into value order
  my $N = $options{most}//1;
  $#m = $N if @m > $N;
  map{sprintf "%4d\n%s", $m[$_][1], $m[$_][0]} keys @m;
 }

sub Zero::Emulator::Execution::analyzeExecutionNotRead($%)                      # Analyze execution results for variables never read
 {my ($exec, %options) = @_;                                                    # Execution results, options

  my @t;
  my $n = $exec->notRead;
  for my $areaK(sort keys %$n)
   {my $area = $$n{$areaK};
    for my $addressK(sort keys %$area)
     {my $address = $$area{$addressK};
      my $context = $exec->code->code->[$addressK]->contextString;
      push @t, "Not read from area: $areaK, address: $addressK in context\n$context";
     }
   }
  @t;
 }

sub Zero::Emulator::Execution::analyzeExecutionResultsDoubleWrite($%)           # Analyze execution results - double writes
 {my ($exec, %options) = @_;                                                    # Execution results, options

  my @r;

  my $W = $exec->doubleWrite;
  if (keys %$W)
   {for my $p(sort keys %$W)
     {for my $q(keys $$W{$p}->%*)
       {push @r, sprintf "Double write occured %d  times. ", $$W{$p}{$q};
        if ($p eq $q)
         {push @r, "First  and second write\n$p\n";
         }
        else
         {push @r, "First  write:\n$p\n";
          push @r, "Second write:\n$q\n";
         }
       }
     }
   }
  @r
 }

sub Zero::Emulator::Execution::analyzeExecutionResults($%)                      # Analyze execution results
 {my ($exec, %options) = @_;                                                    # Execution results, options

  my @r;

  if (1)
   {my @l = $exec->analyzeExecutionResultsLeast(%options);                      # Least/most executed
    my @m = $exec->analyzeExecutionResultsMost (%options);
    if (@l and $options{leastExecuted})
     {push @r, "Least executed:";
      push @r, @l;
     }
    if (@m and $options{mostExecuted})
     {push @r, "Most executed:";
      push @r, @m;
     }
   }

  if (my @n = $exec->analyzeExecutionNotRead(%options))                         # Variables not read
   {my $n = @n;
    @n = () unless $options{notRead};
    push @r, @n;
    push @r, sprintf "# %8d variables not read", $n;
   }

  if (my @d = $exec->analyzeExecutionResultsDoubleWrite(%options))              # Analyze execution results - double writes
   {my $d = @d;
    @d = () unless $options{doubleWrite};
    push @r, @d;
    push @r, sprintf "# %8d double writes", $d/2;
   }

  push @r,   sprintf "# %8d instructions executed", $exec->count;
  join "\n", @r;
 }

#D1 Execution                                                                   # Execute assembly code in the emulator

sub Zero::Emulator::Code::execute($%)                                           # Execute a block of code
 {my ($Code, %options) = @_;                                                    # Block of code, execution options
  $Code->assemble;                                                              # Assemble if necessary
  my $code = $Code->code;

  my $instructionPointer = 0;                                                   # Instruction pointer
  my @calls;                                                                    # Call stack of calls made
  my @out;                                                                      # Output area
  my %memory;                                                                   # Memory
  my %memoryType;                                                               # The reason for this allocation
  my %rw;                                                                       # Last action on each memory location, read or write: two writes with no intervening read is bad.  Writes are represented as stack trace backs, reasd by undef
  my %read;                                                                     # Whether a memory location was ever read allowing us to find all the unused locations
  my %notRead;                                                                  # Memory locations never read
  my %doubleWrite;                                                              # Double writes: earlier instruction number to later instruction number
  my %pointlessAssign;                                                          # Pointless assigns {instruction number} to count - location already has the specified value
  my $debug;                                                                    # Debug

  my $exec          =  genHash("Zero::Emulator::Execution",                     # Execution results
    calls           => \@calls,                                                 # Call stack
    code            => $Code,                                                   # Code executed
    count           => 0,                                                       # Executed instructions count
    counts          => {},                                                      # Executed instructions by name counts
    memory          => \%memory,                                                # Memory contents at the end of execution
    memoryType      => \%memoryType,                                            # Memory contents at the end of execution
    rw              => \%rw,                                                    # Read / write access to memory
    read            => \%read,                                                  # Records whether a memory location was ever read allowing us to find all the unused locations
    notRead         => \%notRead,                                               # Memory locations never read
    out             => \@out,                                                   # The out channel
    doubleWrite     => \%doubleWrite,                                           # Source of double writes {instruction number} to count - an existing value was overwritten before it was used
    pointlessAssign => \%pointlessAssign,                                       # Location already has the specified value
   );

  my sub currentInstruction()                                                   # Get the current instructionm
   {$calls[-1]->instruction;
   };

  my sub address($$)                                                            # Record a reference to memory
   {my ($area, $location) = @_;                                                 # Area, location in area, memory
    genHash("Zero::Emulator::Address",                                          # Address memory
      area     => $area,                                                        # Area in memory
      location => $location,                                                    # Location within area
     );
   }

  my sub stackTraceAndExit($;$)                                                 # Print a stack trace and exit
   {my ($i, $title) = @_;                                                       # Instruction trace occurred at, title
    my $s = $options{suppressErrors};
    my $d = $options{debug};
    my @s;

    push @s, $title // "Stack trace\n";
    push @s, $i->contextString("Context of failing instruction") if $d;
    for my $j(reverse keys @calls)
     {my $c = $calls[$j];
      my $i = $c->instruction;
      push @s, sprintf "%5d  %4d %s\n", $j+1, $i->number+1, $i->action if $s;
      push @s, sprintf "%5d  %4d %-16s at %s line %d\n",
        $j+1, $i->number+1, $i->action, $i->file, $i->line         unless $s;
     }

    say STDERR join "\n", @s unless $s;
    push @out, @s;
    $instructionPointer = undef;                                                # Execution terminates as soon as undefined instuction is encountered
   };

  my sub stackArea()                                                            # Memory area associated with this method invocation
   {$calls[-1]->stackArea;                                                      # Stack area
   }

  my $allocs = 0; my $allocsStacked = 0;                                        # Normal allcos made by the caller, stacked allcos made by to syupport subroutine calling, parameter passing, result returning.
  my sub allocMemory($;$)                                                       # Create the name of a new memory area
   {my ($name, $stacked) = @_;                                                  # Name of allocation, stacked if true
    if ($stacked)
     {my $a = $allocsStacked--;
      $memory{$a} = bless [], $name;
      $memoryType{$a} = $name;
      return $a
     }
    my $a = ++$allocs;
    $memory{$a} = bless [], $name;
    $memoryType{$a} = $name;
    $a
   }

  my sub notRead()                                                              # Record the unused memory locations in the current stack frame
   {my $area = &stackArea;
#    my @area = $memory{$area}->@*;                                             # Memory in area
#    my %r;                                                                     # Location in stack frame => instruction defining vasriable
#    for my $a(keys @area)
#     {if (my $I  = $calls[-1]->variables->instructions->[$a])
#       {$r{$a} = $I;                                                           # Number of instruction creating variable
#       }
#     }
#
#    if (my $r = $read{$area})                                                  # Locations in this area that have ben read
#     {delete $r{$_} for keys %$r;                                              # Delete locations that have been read from
#     }
#
#    $notRead{$area} = {%r} if keys %r;                                         # Record not read
   }

  my sub rwWrite($$)                                                            # Observe write to memory
   {my ($area, $address) = @_;                                                  # Area in memory, address within area
    my $P = $rw{$area}{$address};
    if (defined($P))
     {my $M = $memory{$area}[$address];                                         # If the memory location is zero we will assume that it has been cleared rather than set.
      if ($M)
       {my $Q = currentInstruction;
        $doubleWrite{$P->contextString}{$Q->contextString}++;
       }
     }
    $rw{$area}{$address} = currentInstruction;
   }

  my sub markAsRead($$)                                                         # Mark a memory location as having been read from
   {my ($area, $address) = @_;                                                  # Area in memory, address within area
    delete $rw{$area}{$address};                                                # Clear last write operation
   }

  my sub rwRead($$)                                                             # Observe read from memory
   {my ($area, $address) = @_;                                                  # Area in memory, address within area
    if (defined(my $a = $rw{$area}{$address}))                                  # Can only read from locations that actually have something in them
     {markAsRead $area, $address;                                               # Clear last write operation
           $read{$area}{$address}++;                                            # Track reads
     }
   }

  my sub left($;$)                                                              # Address a memory location
   {my ($ref, $extra) = @_;                                                     # Reference, an optional extra offset to add or subtract to the final memory address
    my $r    =  $ref->address;
    my $a    =  $r;
       $a    = \$r if isScalar $a;                                              # Interpret constants as direct memory locations
    my $area = $ref->area;
    my $x = $extra // 0;                                                        # Default is to use the address as supplied without locating a nearby address
    my $S = &stackArea;                                                         # Current stack frame

    my sub invalid()
     {my $i = currentInstruction;
      stackTraceAndExit($i);
      my $l = $i->line;
      my $f = $i->file;
      my $c = $i->contextString;
      die "Invalid left area: ".dump($area)
       ." address: ".dump($a)
       .(defined($extra) ? " + extra: ".dump($extra) : '')
       ." stack: $S at $f line $l\n$c\n";
     };

    my $M;                                                                      # Memory location
    if (isScalar $$a)
     {$M = $$a+$x
     }
    elsif (isScalar $$$a)
     {rwRead($S, $$$a);
      $M = $memory{$S}[$$$a]+$x
     }
    else
     {invalid
     }

    if ($M < 0)                                                                 # Disallow negative addresses because they mean something special to Perl
     {stackTraceAndExit(currentInstruction,
       "Negative address for area: ".dump($area)
       .", address: ".dump($a)
       ." extra:".dump($x));
     }
    elsif (!defined($area))                                                     # Current stack frame
     {rwWrite(        $S, $M);
      return  address($S, $M);                                                  # Stack frame
     }
    elsif (isScalar($area))
     {rwWrite(        $area, $M);
      return  address($area, $M)                                                # Specified constant area
     }
    elsif (isScalar($$area))
     {rwRead (        $S, $$area);
      my $A = $memory{$S}[$$area];
      rwWrite(        $A, $M);
      return  address($A, $M)                                                   # Indirect area
     }
    invalid;
   }

  my sub leftSuppress($)                                                        # Indicate that a memory location has been read
   {my ($ref) = @_;                                                             # Reference
    my $A     = $ref->address;
    my $area  = $ref->area;
    my $a = $A;
       $a = \$A if isScalar $a;                                                 # Interpret constants as direct memory locations

    my $m;
    if (isScalar $$a)                                                           # Direct
     {$m = $$a;
     }
    elsif (isScalar $$$a)                                                       # Indirect
     {rwRead      (&stackArea, $$$a);
      $m = $memory{&stackArea}[$$$a];
     }

    if (defined($m))
     {if (!defined($area))                                                      # Current stack frame
       {rwRead(&stackArea, $m);
       }
      elsif (isScalar($area))                                                   # Direct area
       {rwRead($area, $m);
       }
      elsif (isScalar($$area))                                                  # Indirect area
       {rwRead(        &stackArea,  $area);
        rwRead($memory{&stackArea}[$$area], $m);
       }
     }
   }

  my sub right($)                                                               # Get a constant or a memory location
   {my ($ref) = @_;                                                             # Location, optional area
    my $a    = $ref->address;
    my $area = $ref->area;
    my $r; my $e = 0; my $tAddress; my $tArea;

    my sub invalid()
     {my $i = currentInstruction;
      stackTraceAndExit($i);
      my $l = $i->line;
      my $f = $i->file;
      my $c = $i->contextString("Failing instruction:");
      die "Invalid right area: ".dump($area)
       ." address: "    .dump($a)
       ." stack: "      .&stackArea
       ." error: "      .dump($e)
       ." target Area: ".dump($tArea)
       ." address: "    .dump($tAddress)
       ." at $f line $l\n$c\n"
       .dump(\%memory);
     }

    if (isScalar($a))                                                           # Constant
     {#rwRead($area//&stackArea, $a) if $a =~ m(\A\-?\d+\Z);
      return $a if defined $a;                                                  # Attempting to read a location that has never been set is an error
     }

    my $m;
    if (isScalar($$a))                                                          # Direct
     {$m = $$a;
     }
    elsif (isScalar($$$a))                                                      # Indirect
     {rwRead(      &stackArea, $$$a);
      $m = $memory{&stackArea}[$$$a];
     }
    if (!defined($m))
     {invalid;
     }

    if (!defined($area))
     {rwRead(      &stackArea, $m);
      $r = $memory{&stackArea}[$m];                                             # Indirect from stack area
      $e = 1;  $tAddress = $m; $tArea = &stackArea;
     }
    elsif (isScalar($area))
     {rwRead(      $area, $m);
      $r = $memory{$area}[$m];                                                  # Indirect from constant area
      $e = 2;  $tAddress = $m; $tArea = $area;
     }
    elsif (isScalar($$area))
     {rwRead(&stackArea, $$area);                                               # Mark the location holding the area as having been read
      if (defined(my $j = $memory{&stackArea}[$$area]))
       {rwRead(      $j, $m);
        $r = $memory{$j}[$m];                                                   # Indirect from indirect area
        $e = 9; $tAddress = $m; $tArea = $j;
       }
     }
    invalid if !defined $r;
    $r
   }

  my sub jumpOp($$)                                                             # Jump to the target location if the tested memory area if the condition is matched
   {my ($i, $check) = @_;                                                       # Instruction, check
    $instructionPointer = $i->number + right($i->target) if &$check;            # Check if condition is met
   }

  my sub assert($$)                                                             # Assert generically
   {my ($test, $sub) = @_;                                                      # Text of test, subroutine of test
    my $i = currentInstruction;
    my ($a, $b) = (right($i->source), right($i->source2));
    unless($sub->($a, $b))
     {say STDERR "Assert $a $test $b failed" unless $options{suppressErrors};
      stackTraceAndExit($i);
     }
   }

  my sub assign($$)                                                             # Assign - check for pointless assignments
   {my ($target, $value) = @_;                                                  # Target of assign, value to assign
    ref($target) =~ m(Address)i or confess "Not an address: ".dump($target);
    defined($value) or confess "Cannot assign an undefined value";
    my $currently = $target->get($exec);
    if (defined($currently) and $currently == $value)
     {$pointlessAssign{currentInstruction->number}++;
      if ($options{stopOnError=>1})
       {my $a = $target->area;
        my $l = $target->location;
        stackTraceAndExit(currentInstruction(), "Pointless assign of: $currently to area: $a, at ") ;
       }
     }
    $target->set($value, $exec);
   }

  my sub allocateSystemAreas                                                    # Allocate system areas for a new stack frame
   {(stackArea  => allocMemory("stackArea", 1),
     params     => allocMemory("params",    1),
     return     => allocMemory("return",    1));
   }

  my sub freeSystemAreas($)                                                     # Free system areas for the specified stack frame
    {my ($c) = @_;                                                              # Parameters
     notRead;                                                                   # Record unread memory locations in the current stack frame
     delete $memory{$_} for $c->stackArea, $c->params, $c->return;
    $allocsStacked -= 3;
   }

  my %instructions =                                                            # Instruction definitions
   (add     => sub                                                              # Add the two source operands and store the result in the target
     {my $i = currentInstruction;
      my $t = left($i->target);
      assign($t, right($i->source) + right($i->source2));
     },
    subtract  => sub                                                            # Subtract the second source operand from the first and store the result in the target
     {my $i = currentInstruction;
      my $t = left($i->target);
      assign($t, right($i->source) - right($i->source2));
     },

    alloc     => sub                                                            # Create a new memory area and write its number into the location named by the target operand
     {my $i = currentInstruction;
      my $a = allocMemory($i->source);                                          # The reason for this allocation
      my $t = left($i->target);

      $memory{$a} = [];
      bless $memory{$a}, $i->source;                                            # Useful becuase dump then printsthe type of each area for us
      assign($t, $a);
      $a
     },

    assert    =>   sub                                                          # Assert
     {my $i = currentInstruction;
      say STDERR "Assert failed" unless $options{suppressErrors};
      stackTraceAndExit($i);
     },

    assertEq  =>   sub                                                          # Assert equals
     {assert("==", sub {my ($a, $b) = @_; $a == $b})
     },

    assertNe  =>   sub                                                          # Assert not equals
     {assert("!=", sub {my ($a, $b) = @_; $a != $b})
     },

    assertLt  =>   sub                                                          # Assert less than
     {assert("< ", sub {my ($a, $b) = @_; $a <  $b})
     },

    assertLe  =>   sub                                                          # Assert less than or equal
     {assert("<=", sub {my ($a, $b) = @_; $a <= $b})
     },

    assertGt  =>   sub                                                          # Assert greater than
     {assert("> ", sub {my ($a, $b) = @_; $a >  $b})
     },

    assertGe  =>   sub                                                          # Assert greater
     {assert(">=", sub {my ($a, $b) = @_; $a >= $b})
     },

    free      => sub                                                            # Free the memory area named by the source operand
     {my $i = currentInstruction;
      my $area = right($i->source);                                             # Area
      confess "Attemp to allocate non user area: $area" unless $area =~ m(\A\d+\Z);
      delete $memory{$area}
     },

    call      => sub                                                            # Call a subroutine
     {my $i = currentInstruction;
      my $t = $i->target->address;                                              # Subroutine to call

      if (isScalar($t))
       {$instructionPointer = $i->number + $t;                                  # Relative call if we know where the subroutine is relative to the call instruction
       }
      else
       {$instructionPointer = $t;                                               # Absolute call
       }
      push @calls, stackFrame(target=>$code->[$instructionPointer],             # Create a new call stack entry
        instruction=>$i, variables=>$i->source->variables,
        allocateSystemAreas());
     },

    return    => sub                                                            # Return from a subroutine call via the call stack
     {my $i = currentInstruction;
      @calls or confess "The call stack is empty so I do not know where to return to";
      freeSystemAreas(pop @calls);
      if (@calls)
       {my $c = $calls[-1];
        $instructionPointer = $c->instruction->number+1;
       }
      else
       {$instructionPointer = undef;
       }
     },

    confess => sub                                                              # Print the current call stack and stop
     {stackTraceAndExit(currentInstruction);
     },

    debug   => sub                                                              # Set debug
     {my $i = currentInstruction;
      my $s = right($i->source);
      $debug = !!$s;
      say STDERR "Debug $debug";
     },

    dump    => sub                                                              # Dump memory
     {my $i = currentInstruction;
      my $d = $exec->dumpMemory($i->source);
      say STDERR $d if $options{trace} or $options{debug};
      push @out, $d;
     },

    dec     => sub                                                              # Decrement locations in memory. The first location is incremented by 1, the next by two, etc.
     {my $i = currentInstruction;
      leftSuppress($i->target);                                                 # Make sure there is something to decrement
      my $t = left($i->target);
      ${$t->at($exec)}--;
     },

    inc       => sub                                                            # Increment locations in memory. The first location is incremented by 1, the next by two, etc.
     {my $i = currentInstruction;
      leftSuppress($i->target);                                                 # Make sure there is something to increment
      my $t = left($i->target);
      ${$t->at($exec)}++;
     },

    jmp       => sub                                                            # Jump to the target location
     {my $i = currentInstruction;
      my $n = $i->number;
      my $r = right($i->target);
      $instructionPointer = $n + $r;
     },
                                                                                # Conditional jumps
    jEq => sub {my $i = currentInstruction; jumpOp($i, sub{right($i->source) == right($i->source2)})},
    jNe => sub {my $i = currentInstruction; jumpOp($i, sub{right($i->source) != right($i->source2)})},
    jLe => sub {my $i = currentInstruction; jumpOp($i, sub{right($i->source) <= right($i->source2)})},
    jLt => sub {my $i = currentInstruction; jumpOp($i, sub{right($i->source) <  right($i->source2)})},
    jGe => sub {my $i = currentInstruction; jumpOp($i, sub{right($i->source) >= right($i->source2)})},
    jGt => sub {my $i = currentInstruction; jumpOp($i, sub{right($i->source) >  right($i->source2)})},

    label     => sub                                                            # Label - no operation
     {my ($i) = @_;                                                             # Instruction
     },

    clear     => sub                                                            # Clear the first bytes of an area as specified by the taregt operand
     {my $i = currentInstruction;
      my $n = left($i->target);
      for my $a(0..$n->location-1)
       {my $A = left(Reference([$i->target->area, $a]));
        $A->set(0, $exec);
       }
     },

    leAddress => sub                                                            # Load the address component of a reference
     {my $i = currentInstruction;
      my $s = left($i->source);
      my $t = left($i->target);
      assign($t, $s->location);
     },

    leArea    => sub                                                            # Load the area component of an address
     {my $i = currentInstruction;
      my $s = left($i->source);
      my $t = left($i->target);
      assign($t, $s->area);
     },

    mov       => sub                                                            # Move data moves data from one part of memory to another - "set", by contrast, sets variables from constant values
     {my $i = currentInstruction;
      my $s = right($i->source);
      my $t = left($i->target);
      assign($t, $s);
     },

    paramsGet => sub                                                            # Get a parameter from the previous parameter block - this means that we must always have two entries on the call stack - one representing the caller of the program, the second representing the current context of the program
     {my $i = currentInstruction;
      my $p = Reference([$calls[-2]->params, $i->source->address]);
      my $t = left ($i->target);
      leftSuppress ($p);                                                        # The source will be read from
      my $s = left ($p);                                                        # The source has to be a left hand side because we want to address a memory area not get a constant
      assign($t, $s->get($exec));
     },

    paramsPut => sub                                                            # Place a parameter in the current parameter block
     {my $i = currentInstruction;
      my $p = $i->targetArea // $calls[-1]->params;
      leftSuppress (Reference([$p, $i->target->address]));
      my $t = left (Reference([$p, $i->target->address]));
      my $s = right($i->source);
      assign($t, $s);
     },

    returnGet => sub                                                            # Get a word from the return area
     {my $i = currentInstruction;
      my $p = $calls[-1]->return;                                               # Memory area
      my $t = left ($i->target);
      leftSuppress(Reference([$p, \$i->source->address]));                      # The source will be read from
      my $s = left(Reference([$p,  $i->source->address]));                      # The source has to be a left hand side because we want to address a memory area not get a constant
      assign($t, $s->get($exec));
     },

    returnPut => sub                                                            # Put a word ino the return area
     {my $i = currentInstruction;
      my $p = $calls[-2]->return;
      my $t = left (Reference([$p, $i->target->address]));
      my $s = right($i->source);
      assign($t, $s);
     },

    nop       => sub                                                            # No operation
     {my ($i) = @_;                                                             # Instruction
     },

    out     => sub                                                              # Write source as output to an array of words
     {my $i = currentInstruction;
      my $t = right($i->source);
      lll $t if $options{debug} or $options{trace};
      push @out, $t;
     },

    pop => sub                                                                  # Pop a value from the specified memory area if possible else confess
     {my $i = currentInstruction;
      my $s = $i->source;
      my $area = $i->source ? right($i->source) : &stackArea;                   # Memory area to pop
      if (!defined($memory{$area}) or !$memory{$area}->@*)                      # Stack not poppable
        {confess "Cannot pop area $area";
        }
      my $t = left($i->target);
      my $p = pop $memory{$area}->@*;
      assign($t, $p);                                                           # Pop from memory area into indicated memory location
     },

    push => sub                                                                 # Push a value onto the specified memory area
     {my $i = currentInstruction;
      if ($i->target)
       {push $memory{right($i->target)}->@*, right($i->source);
       }
      else
       {push $memory{&stackArea}->@*, right($i->source);
       }
     },

    shiftLeft => sub                                                            # Shift left within an element
     {my $i = currentInstruction;
      leftSuppress ($i->target);                                                # Make sure there something to shift
      my $t = left ($i->target);
      my $s = right($i->source);
      assign($t, $t->get($exec) << $s);
     },

    shiftRight => sub                                                           # Shift right within an element
     {my $i = currentInstruction;
      leftSuppress ($i->target);                                                # Make sure there something to shift
      my $t = left ($i->target);
      my $s = right($i->source);
      assign($t, $t->get($exec) >> $s);
     },

    shiftUp => sub                                                              # Shift an element up in a memory area
     {my $i = currentInstruction;
      my $s = right($i->source);
      my $t = left($i->target);
      my $L = $t->areaContent($exec);                                           # Length of area
      my $l = $t->location;  # Wrong, is big like  4444 should be 4
#say STDERR "AAAA", dump($t);
      for my $j(reverse 1..$L-$l)
       {my $s = left($i->target, $j-1);
        my $t = left($i->target, $j);
        assign($t, $s->get($exec));
#say STDERR "BBBB", $exec->dumpMemory("BBBB");
       }
      assign($t, $s);
     },

    shiftDown => sub                                                            # Shift an element down in a memory area
     {my $i = currentInstruction;
      my $s = left($i->source)->get($exec);
      my $t = left($i->source);
      my $L = $t->areaContent($exec);                                           # Length of area
      my $l = $t->location;
      for my $j($l..$L-2)                                                       # Each element in specified range
       {my $s = left(Reference([$i->source->area, $j+1]));
        my $t = left(Reference([$i->source->area, $j]));
        assign($t, $s->get($exec));
       }
      pop $memory{$t->area}->@*;
      my $T = left($i->target);
      assign($T, $s);
     },
   );

  push @calls, stackFrame(variables=>$Code->variables, allocateSystemAreas);    # Variables in initial stack frame

  my $mi = $options{maximumInstructionsToExecute} //                            # Prevent run away executions
                    maximumInstructionsToExecute;
  for my $j(1..$mi)                                                             # Each instruction in the code until we hit an undefined instruction
   {last unless defined($instructionPointer);
    my $i = $calls[-1]->instruction = $$code[$instructionPointer++];
    last unless $i;
    if (my $a = $i->action)                                                     # Action
     {$exec->counts->{$a}++; $exec->count++;                                    # Execution instruction counts
      confess qq(Invalid instruction: "$a"\n) unless my $c = $instructions{$a};
      if ($options{trace})
       {say STDERR sprintf "%4d  %4d  %12s at %s line %d\n",
          $j, $i->number, $i->action, $i->file, $i->line;
       }
      ++$i->executed;
      $c->($i);                                                                 # Execute instruction
     }
    confess "Out of instructions after $j" if $j >= maximumInstructionsToExecute;
   }

  if (1)                                                                        # Free first stack frame
   {freeSystemAreas($calls[0]);                                                 # Free
   }

  $exec
 }                                                                              # Execution results

my $assembly;                                                                   # The current assembly

my sub label()                                                                  # Next unique label
 {++$assembly->labelCounter;
 }

my sub setLabel(;$)                                                             # Set and return a label
 {my ($l) = @_;                                                                 # Optional preset label
  $l //= label;                                                                 # Create label if none supplied
  Label($l);                                                                    # Set label
  $l                                                                            # return (new) label
 }

my sub xSource($)                                                               # Record a source argument
 {my ($s) = @_;                                                                 # Source expression - either a single location ion the currnt stack frame or a refernce to an array conatyaining anInstruction pair containing the area id followed by the location
  (q(source), Reference $s)
 }

my sub xSource2($)                                                              # Record a source argument
 {my ($s) = @_;                                                                 # Source expression - either a single location ion the currnt stack frame or a refernce to an array conatyaining anInstruction pair containing the area id followed by the location
  (q(source2), Reference $s)
 }

my sub xTarget($)                                                               # Record a target argument
 {my ($t) = @_;                                                                 # Target expression - either a single location ion the currnt stack frame or a refernce to an array conatyaining anInstruction pair containing the area id followed by the location
  (q(target), Reference $t)
 }

sub Start($)                                                                    # Start the current assembly using the specified version of the Zero languiage.  At  the moment only version 1 works.
 {my ($version) = @_;                                                           # Version desired - at the moment only 1
  $version == 1 or confess "Version 1 is currently the only version available\n";
  $assembly = Code;                                                             # The current assembly
 }

sub Add($$;$)                                                                   # Add the source locations together and store in the result in the target area
 {my ($target, $s1, $s2) = @_ == 2 ? (&Var(), @_) : @_;                         # Target location, source one, source two
  $assembly->instruction(action=>"add", xTarget($target),
    xSource($s1), xSource2($s2));
  $target
 }

sub Subtract($$;$)                                                              # Subtract the second source location from the first and store in the result in the target area
 {my ($target, $s1, $s2) = @_ == 2 ? (&Var(), @_) : @_;                         # Target location, source one, source two
  $assembly->instruction(action=>"subtract", xTarget($target),
    xSource($s1), xSource2($s2));
  $target
 }

sub Alloc($)                                                                    # Create a new memory area and write its number into the location named by the target operand
 {my ($name) = @_;                                                              # Name of allocation
  my $t = &Var();
  $assembly->instruction(action=>"alloc", target=>Reference($t), source=>$name);
  $t;
 }

sub Free($)                                                                     # Free the memory area named by the source operand
 {my ($source) = @_;                                                            # Source location containing number of area to free
  $assembly->instruction(action=>"free", xSource($source));
 }

sub Call($)                                                                     # Call the subroutine at the target address
 {my ($p) = @_;                                                                 # Procedure description
  $assembly->instruction(action=>"call",
    target=>Reference($p->target), source=>$p);
 }

sub Confess()                                                                   # Confess
 {$assembly->instruction(action=>"confess");
 }

sub Dump(;$)                                                                    # Dump memory
 {my ($title) = @_;                                                             # Titl
  $assembly->instruction(action=>"dump", source=>$title);
 }

sub Debug($)                                                                    # Debug
 {my ($source) = @_;                                                            # Debug setting, options
  $assembly->instruction(action=>"debug", xSource($source));
 }

sub Dec($)                                                                      # Decrement the target
 {my ($target) = @_;                                                            # Target address
  $assembly->instruction(action=>"dec", xTarget($target))
 }

sub Inc($)                                                                      # Increment the target
 {my ($target) = @_;                                                            # Target address
  $assembly->instruction(action=>"inc", xTarget($target))
 }

sub Jmp($)                                                                      # Jump to a label
 {my ($target) = @_;                                                            # Target address
  $assembly->instruction(action=>"jmp", xTarget($target));
 }

sub Jle($$$)                                                                    # Jump to a target label if the first source field is less than or equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jLe", xTarget($target), xSource($source), xSource2($source2));
 }

sub Jlt($$$)                                                                    # Jump to a target label if the first source field is less than the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jLt", xTarget($target), xSource($source), xSource2($source2));
 }

sub Jge($$$)                                                                    # Jump to a target label if the first source field is greater than or equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jGe", xTarget($target), xSource($source), xSource2($source2));
 }

sub Jgt($$$)                                                                    # Jump to a target label if the first source field is greater than the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jGt", xTarget($target), xSource($source), xSource2($source2));
 }

sub Jeq($$$)                                                                    # Jump to a target label if the first source field is equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jEq", xTarget($target), xSource($source), xSource2($source2));
 }

sub Jne($$$)                                                                    # Jump to a target label if the first source field is not equal to the second source field
 {my ($target, $source, $source2) = @_;                                         # Target label, source to test
  $assembly->instruction(action=>"jNe", xTarget($target), xSource($source), xSource2($source2));
 }

sub Label($)                                                                    # Create a lable
 {my ($source) = @_;                                                            # Name of label
  $assembly->instruction(action=>"label", xSource($source));
 }

sub Clear($)                                                                    # Clear the first bytes of an area.  The area is specified by the first element of the address, the number of locations to clear is specified by the second element of the target address.
 {my ($target) = @_;                                                            # Target location, source location
  $assembly->instruction(action=>"clear", xTarget($target));
 }

sub LeAddress($;$)                                                              # Load the address component
 {if (@_ == 1)
   {my ($source) = @_;                                                          # Target location, source location
    my $t = &Var();
    $assembly->instruction(action=>"leAddress", target=>Reference($t), xSource($source));
    return $t;
   }
  elsif (@ == 2)
   {my ($target, $source) = @_;                                                 # Target location, source location
    $assembly->instruction(action=>"leAddress", xTarget($target), xSource($source));
   }
  else
   {confess "One or two parameters required";
   }
 }

sub LeArea($;$)                                                                 # Load the address component
 {if (@_ == 1)
   {my ($source) = @_;                                                          # Target location, source location
    my $t = &Var();
    $assembly->instruction(action=>"leArea", target=>Reference($t), xSource($source));
    return $t;
   }
  elsif (@ == 2)
   {my ($target, $source) = @_;                                                 # Target location, source location
    $assembly->instruction(action=>"leArea", xTarget($target), xSource($source));
   }
  else
   {confess "One or two parameters required";
   }
 }

sub Mov($;$)                                                                    # Copy a constant or memory location to the target location
 {if (@_ == 1)
   {my ($source) = @_;                                                          # Target location, source location
    my $t = &Var();
    $assembly->instruction(action=>"mov", target=>Reference($t), xSource($source));
    return $t;
   }
  elsif (@ == 2)
   {my ($target, $source) = @_;                                                 # Target location, source location
    $assembly->instruction(action=>"mov", xTarget($target), xSource($source));
   }
  else
   {confess "One or two parameters required";
   }
 }

sub Nop()                                                                       # Do nothing (but do it well!)
 {$assembly->instruction(action=>"nop");
 }

sub Out($)                                                                      # Write memory contents to out
 {my ($source) = @_;                                                            # Either a scalar constant or memory address to output
  $assembly->instruction(action=>"out", xSource($source))
 }

sub Procedure($$)                                                               # Define a procedure
 {my ($name, $source) = @_;                                                     # Name of procedure, source code as a subroutine# $assembly->instruction(action=>"procedure", target=>$target, source=>$source);
  if ($name and my $n = $assembly->procedures->{$name})                         # Reuse existing named procedure
   {return $n;
   }

  Jmp(my $end = label);                                                         # Jump over the code of the procedure body
  my $start = setLabel;
  my $p = procedure($start);                                                    # Procedure description
  my $save_registers = $assembly->variables;
  $assembly->variables = $p->variables;
  &$source($p);                                                                 # Code of procedure called with start label as a parameter
  &Return;
  $assembly->variables = $save_registers;

  setLabel $end;
  $assembly->procedures->{$name} = $p;                                          # Return the start of the procedure
 }

sub ParamsGet($;$)                                                              # Get a word from the parameters in the previous frame and store it in the current frame
 {if (@_ == 1)
   {my ($source) = @_;                                                          # Memory location to place parameter in, parameter number
    my $p = &Var();
    $assembly->instruction(action=>"paramsGet", target=>Reference($p), xSource($source));
    return $p;
   }
  elsif (@_ == 2)
   {my ($target, $source) = @_;                                                 # Memory location to place parameter in, parameter number
    $assembly->instruction(action=>"paramsGet", xTarget($target), xSource($source));
   }
  else
   {confess "One or two parameters required";
   }
 }

sub ParamsPut($$)                                                               # Put a word into the parameters list to make it visible in a called procedure
 {my ($target, $source) = @_;                                                   # Parameter number, location to fetch paranter from
  $assembly->instruction(action=>"paramsPut", xTarget($target), xSource($source));
 }

sub Return()                                                                    # Return from a procedure via the call stack
 {$assembly->instruction(action=>"return");
 }

sub ReturnGet($;$)                                                              # Get a word from the return area and save it
 {if (@_ == 1)                                                                  # Create a variable
   {my ($source) = @_;                                                          # Memory location to place return value in, return value to get
    my $p = &Var();
    $assembly->instruction(action=>"returnGet", target=>Reference($p), xSource($source));
    return $p;
   }
  elsif (@_ == 2)
   {my ($target, $source) = @_;                                                 # Memory location to place return value in, return value to get
    $assembly->instruction(action=>"returnGet", xTarget($target), xSource($source));
   }
  else
   {confess "One or two parameters required";
   }
 }

sub ReturnPut($$)                                                               # Put a word into the return area
 {my ($target, $source) = @_;                                                   # Offset in return area to write to, memory location whose contents are to be placed in the return area
  $assembly->instruction(action=>"returnPut", xTarget($target), xSource($source));
 }

sub Pop(;$$)                                                                    # Pop the memory area specified by the source operand into the memory address specified by the target operand
 {if (@_ == 0)                                                                  # Pop current stack fram intoo a local variable
   {my $p = &Var();
    my $i = $assembly->instruction(action=>"pop", target=>Reference($p));
    $i->source = undef;
    return $p;
   }
  elsif (@_ == 1)                                                               # Pop indicated area into a local variable
   {my ($source) = @_;                                                          # Memory location to place return value in, return value to get
    my $p = &Var();
    $assembly->instruction(action=>"pop", target=>Reference($p), xSource($source));
    return $p;
   }
  elsif (@_ == 2)
   {my ($target, $source) = @_;                                                 # Pop indicated area into target location
    $assembly->instruction(action=>"pop", xTarget($target), xSource($source));
   }
  else
   {confess "Zero, One or two parameters required";
   }
 }

sub Push($;$)                                                                   # Push the value in the current stack frame specified by the source operand onto the memory area identified by the target operand.
 {if (@_ == 1)
   {my ($source) = @_;                                                          # Push a value onto the current stack frame
    $assembly->instruction(action=>"push", xSource($source));
   }
  elsif (@_ == 2)                                                               # Push a value onto the specified memory area
   {my ($target, $source) = @_;                                                 # Memory area to push to, memory containing value to push
    $assembly->instruction(action=>"push", xTarget($target), xSource($source));
   }
  else
   {confess "One or two parameters required";
   }
 }

sub ShiftLeft($;$)                                                              # Shift left within an element
 {my ($target, $source) = @_;                                                   # Target to shift, amount to shift
  $assembly->instruction(action=>"shiftLeft", xTarget($target), xSource($source));
  $target
 }

sub ShiftRight($;$)                                                             # Shift right with an element
 {my ($target, $source) = @_;                                                   # Target to shift, amount to shift
  $assembly->instruction(action=>"shiftRight", xTarget($target), xSource($source));
  $target
 }

sub ShiftUp($;$)                                                                # Shift an element up one in an area
 {my ($target, $source) = @_;                                                   # Target to shift, amount to shift
  $assembly->instruction(action=>"shiftUp", xTarget($target), xSource($source));
  $target
 }

sub ShiftDown($;$)                                                              # Shift an element down opne in an area
 {if (@_ == 1)                                                                  # Create a variable
   {my ($source) = @_;                                                          # Memory location to place return value in, return value to get
    my $p = &Var();
    $assembly->instruction(action=>"shiftDown", target=>Reference($p), xSource($source));
    return $p;
   }
  elsif (@_ == 2)
   {my ($target, $source) = @_;                                                 # Memory location to place return value in, return value to get
    $assembly->instruction(action=>"shiftDown", xTarget($target), xSource($source));
    return $target;
   }
  else
   {confess "One or two parameters required";
   }
 }

sub Then(&)                                                                     # Then block
 {my ($t) = @_;                                                                 # Then block subroutine
  @_ == 1 or confess "One parameter";
  (then => $t)
 }

sub Else(&)                                                                     # Else block
 {my ($e) = @_;                                                                 # Else block subroutine
  @_ == 1 or confess "One parameter";
  (else => $e)
 }

sub Ifx($$$%)                                                                   # Execute then or else clause depending on whether two memory locations are equal.
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

sub IfFalse($%)                                                                 # Execute then clause if the specified memory location is zero representing false
 {my ($a, %options) = @_;                                                       # Memory location, then block, else block
  Ifx(\&Jne, $a, 0, %options);
 }

sub IfTrue($%)                                                                  # Execute then clause if the specified memory location is not zero representing true
 {my ($a, %options) = @_;                                                       # Memory location, then block, else block
  Ifx(\&Jeq, $a, 0, %options);
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
  Ifx(\&Jle, $a, $b, %options);
 }

sub IfGe($$%)                                                                   # Execute then or else clause depending on whether two memory locations are greater than or equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location, then block, else block
  Ifx(\&Jlt, $a, $b, %options);
 }

sub AssertOp($$$)                                                               # Assert operation
 {my ($op, $a, $b) = @_;                                                        # Operation, First memory location, second memory location
  $assembly->instruction(action=>"assert$op", xSource($a), xSource2($b), level=>2);
 }

sub Assert(%)                                                                   # Assert regardless
 {my (%options) = @_;                                                           #
  $assembly->instruction(action=>"assert");
 }

sub AssertEq($$%)                                                               # Assert two memory locations are equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  AssertOp("Eq", $a, $b);
 }

sub AssertNe($$%)                                                               # Assert two memory locations are not equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  AssertOp("Ne", $a, $b);
 }

sub AssertLt($$%)                                                               # Assert two memory locations are less than.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  AssertOp("Lt", $a, $b);
 }

sub AssertLe($$%)                                                               # Assert two memory locations are less than or equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  AssertOp("Le", $a, $b);
 }

sub AssertGt($$%)                                                               # Assert two memory locations are greater than.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  AssertOp("Gt", $a, $b);
 }

sub AssertGe($$%)                                                               # Assert are greater than or equal.
 {my ($a, $b, %options) = @_;                                                   # First memory location, second memory location
  AssertOp("Ge", $a, $b);
 }

sub For($$%)                                                                    # For loop 0..range-1 or in reverse
 {my ($range, $block, %options) = @_;                                           # Limit, block, options
  if (!exists $options{reverse})                                                # Ascending order
   {my $s = 0; my $e = $range;                                                  # Start, end
    ($s, $e) = @$range if ref($e) =~ m(ARRAY);                                  # [start, end]

    my ($Start, $Check, $Next, $End) = (label, label, label, label);

    setLabel($Start);                                                           # Start
    my $i = Mov $s;
      setLabel($Check);                                                         # Check
      Jge  $End, $i, $e;
        &$block($i, $Check, $Next, $End);                                       # Block
      setLabel($Next);
      Inc $i;                                                                   # Next
      Jmp $Check;
    setLabel($End);                                                             # End
   }
  else
   {my $s = $range; my $e = 0;                                                  # Start, end
    ($e, $s) = @$range if ref($e) =~ m(ARRAY);                                  # [end, start]

    my ($Start, $Check, $Next, $End) = (label, label, label, label);

    setLabel($Start);                                                           # Start
    my $i = Subtract $s, 1;
    Subtract $i, $s;
      setLabel($Check);                                                         # Check
      Jlt  $End, $i, $e;
        &$block($i, $Check, $Next, $End);                                       # Block
      setLabel($Next);
      Dec $i;                                                                   # Next
      Jmp $Check;
    setLabel($End);                                                             # End
   }
 }

sub Good(&)                                                                     # A good ending
 {my ($good) = @_;                                                              # What to do on a good ending
  @_ == 1 or confess "One parameter";
  (good => $good)
 }

sub Bad(&)                                                                      # A bad ending
 {my ($bad) = @_;                                                               # What to do on a bad ending
  @_ == 1 or confess "One parameter";
  (bad => $bad)
 }

sub Block(&%)                                                                   # Block of code that can either be restarted or come to a good or a bad ending
 {my ($block, %options) = @_;                                                   # Block, options
  my ($Start, $Good, $Bad, $End) = (label, label, label, label);

  my $g = $options{good};
  my $b = $options{bad};

  setLabel($Start);                                                             # Start

  &$block($Start, $Good, $Bad, $End);                                           # Code of block

  if ($g)                                                                       # Good
   {Jmp $End;
    setLabel($Good);
    &$g;
   }

  if ($b)                                                                       # Bad
   {Jmp $End;
    setLabel($Bad);
    &$b;
   }
  setLabel($End);                                                               # End
 }

sub Var(;$)                                                                     # Create a variable initialized to the specified value
 {my ($value) = @_;                                                             # Value
  my $i = $assembly->registers;
  my $a = $assembly->variables;
  my $v = $a->count-1;
  my $c = $assembly->code->@*;
# $a->instructions->[$v] = $c;

  Mov $i, $value if defined $value;
  $i
 }

sub Execute(%)                                                                  # Execute the current assembly
 {my (%options) = @_;                                                           # Options
  my $r = $assembly->execute(%options);                                         # Execute the code in the current assembly
  if (my $out = $options{out})
   {my $c = compareArraysAndExplain $r->out, $out;
    lll $c if $c;
    say STDERR dump($r->out) if $c;
    return 0 if $c;
   }
  if (my $memory = $options{memory})
   {my $E = dump $memory;
    my $G = dump $r->memory;
    my $e = [split "\n", $E];
    my $g = [split "\n", $G];
    my $c = compareArraysAndExplain $g, $e;
    lll "$c\n$G\n" if $c;
    return 0 if $c;
   }
  return $r;
 }

use Exporter qw(import);
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

@ISA         = qw(Exporter);
@EXPORT      = qw();
@EXPORT_OK   = qw(AreaStructure Add Alloc Bad Block Call Clear Confess Debug Else Execute For Free Good Assert AssertEq AssertNe AssertGe AssertGt AssertLe AssertLt Dec Dump IfEq IfGe IfGt IfLe IfLt IfNe Ifx IfTrue IfFalse Inc Jeq Jge Jgt Jle Jlt Jmp Jne Label Mov Nop Out ParamsGet ParamsPut Pop Procedure Push Return ReturnGet ReturnPut ShiftLeft ShiftRight ShiftUp ShiftDown Start Subtract Then Var);
%EXPORT_TAGS = (all=>[@EXPORT, @EXPORT_OK]);

return 1 if caller;

eval {goto latest};
sub is_deeply;
sub ok($;$);

#D1 Examples

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
  my $a = Mov 2;
  Out $a;
  ok Execute(out=>[2]);
 }

#latest:;
if (1)
 {Start 1;                                                                      #TMov
  my $a = Mov  3;
  my $b = Mov  $$a;
  my $c = Mov  \$b;
  Out $c;
  ok Execute(out=>[3]);
 }

#latest:;
if (1)                                                                          #TAdd
 {Start 1;
  my $a = Add 3, 2;
  Out  $a;
  ok Execute(out=>[5]);
 }

#latest:;
if (1)                                                                          #TAdd #TSubtract
 {Start 1;
  my $a = Subtract 4, 2;
  Out $a;
  ok Execute(out=>[2]);
 }

#latest:;
if (1)                                                                          #TDec
 {Start 1;
  my $a = Mov 3;
  Dec $a;
  Out $a;
  ok Execute(out=>[2]);
 }

#latest:;
if (1)                                                                          #TInc
 {Start 1;
  my $a = Mov 3;
  Inc $a;
  Out $a;
  ok Execute(out=>[4]);
 }

#latest:;
if (1)                                                                          #TShiftLeft
 {Start 1;
  my $a = Mov 1;
  ShiftLeft $a, $a;
  Out $a;
  ok Execute(out=>[2]);
 }

#latest:;
if (1)                                                                          #TShiftRight
 {Start 1;
  my $a = Mov 4;
  ShiftRight $a, 1;
  Out $a;
  ok Execute(out=>[2]);
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
  my $e = Execute(out=>[2]);
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
  Mov     [-1,  1],  11;
  Mov  1, [-1, \1];
  Out \1;
  ok Execute(out=>[11]);
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
  my $w = Procedure 'write', sub
   {ParamsGet \0, 0;
    Out \0;
    Return;
   };
  ParamsPut 0, 'bbb';
  Call $w;
  ok Execute(out=>['bbb']);
 }

#latest:;
if (1)                                                                          #TCall Call a subroutine returning one value
 {Start 1;
  my $w = Procedure 'write', sub
   {ReturnPut 0, "ccc";
    Return;
   };
  Call $w;
  ReturnGet \0, 0;
  Out \0;
  ok Execute(out=>['ccc']);
 }

#latest:;
if (1)                                                                          #TProcedure
 {Start 1;
  my $add = Procedure 'add2', sub
   {my $a = ParamsGet 0;
    my $b = Add $a, 2;
    ReturnPut 0, $b;
    Return;
   };
  ParamsPut 0, 2;
  Call $add;
  my $c = ReturnGet 0;
  Out $c;
  ok Execute(out=>[4]);
 }

#latest:;
if (1)                                                                          #TConfess
 {Start 1;
  my $c = Procedure 'confess', sub
   {Confess;
   };
  Call $c;
  ok Execute(suppressErrors=>1, out=>
[
"Stack trace\n",
  "    2     3 confess\n",
  "    1     6 call\n"]);
 }

#latest:;
if (1)                                                                          #TPush #TPop
 {Start 1;
  my $a = Alloc "aaa";
  Push $a, 1;
  Push $a, 2;
  my $c = Pop $a;
  my $d = Pop $a;

  Out $c;
  Out $d;
  my $e = Execute;
  is_deeply $e->out,    [2, 1];
  is_deeply $e->memory, { 1 => []};
 }

#latest:;
if (1)                                                                          #TAlloc #TMov
 {Start 1;
  my $a = Alloc "alloc";
  my $b = Mov 99;
  my $c = Mov $a;
  Mov [$a, 0], $b;
  Mov [$c, 1], 2;
  ok Execute(memory => { 1 => bless([99, 2], "alloc") });
 }

#latest:;
if (1)                                                                          #TFree
 {Start 1;
  my $a = Alloc "node";
  Out $a;
  Mov [$a, 1], 1;
  Mov [$a, 2], 2;
  Mov 1, [$a, \1];
  Dump;
  Free $a;
  my $e = Execute;
 }

#latest:;
if (1)                                                                          # Layout
 {Start 1;
  my $a = Mov 'A';
  my $b = Mov 'B';
  my $c = Mov 'C';
  Out $c;
  Out $b;
  Out $a;
  ok Execute(out=>[qw(C B A)]);
 }

#latest:;
if (1)                                                                          #TIfEq  #TIfNe  #TIfLt #TIfLe  #TIfGt  #TIfGe
 {Start 1;
  my $a = Mov 1;
  my $b = Mov 2;
  IfEq $a, $a, Then {Out "Eq"};
  IfNe $a, $a, Then {Out "Ne"};
  IfLe $a, $a, Then {Out "Le"};
  IfLt $a, $a, Then {Out "Lt"};
  IfGe $a, $a, Then {Out "Ge"};
  IfGt $a, $a, Then {Out "Gt"};
  ok Execute(out=>["Eq", "Le", "Ge"]);
 }

#latest:;
if (1)                                                                          #TIfEq  #TIfNe  #TIfLt #TIfLe  #TIfGt  #TIfGe
 {Start 1;
  my $a = Mov 1;
  my $b = Mov 2;
  IfEq $a, $b, Then {Out "Eq"};
  IfNe $a, $b, Then {Out "Ne"};
  IfLe $a, $b, Then {Out "Le"};
  IfLt $a, $b, Then {Out "Lt"};
  IfGe $a, $b, Then {Out "Ge"};
  IfGt $a, $b, Then {Out "Gt"};
  ok Execute(out=>["Ne", "Le", "Lt"]);
 }

#latest:;
if (1)                                                                          #TIfEq  #TIfNe  #TIfLt #TIfLe  #TIfGt  #TIfGe
 {Start 1;
  my $a = Mov 1;
  my $b = Mov 2;
  IfEq $b, $a, Then {Out "Eq"};
  IfNe $b, $a, Then {Out "Ne"};
  IfLe $b, $a, Then {Out "Le"};
  IfLt $b, $a, Then {Out "Lt"};
  IfGe $b, $a, Then {Out "Ge"};
  IfGt $b, $a, Then {Out "Gt"};
  ok Execute(out=>["Ne", "Ge", "Gt"]);
 }

#latest:;
if (1)                                                                          #TIfTrue
 {Start 1;
  IfTrue 1,
  Then
   {Out 1
   },
  Else
   {Out 0
   };
  ok Execute(out=>[1]);
 }

#latest:;
if (1)                                                                          #TIfFalse
 {Start 1;
  IfFalse 1,
  Then
   {Out 1
   },
  Else
   {Out 0
   };
  ok Execute(out=>[0]);
 }

#latest:;
if (1)                                                                          #TForLoop
 {Start 1;
  For 10, sub
   {my ($i) = @_;
    Out $i;
   };
  my $e = Execute;
  is_deeply $e->out, [0..9];
 }

#latest:;
if (1)                                                                          #TForLoop
 {Start 1;
  For 10, sub
   {my ($i) = @_;
    Out $i;
   }, reverse=>1;
  ok Execute(out=>[reverse 0..9]);
 }

#latest:;
if (1)                                                                          #TForLoop
 {Start 1;
  For [2, 10], sub
   {my ($i) = @_;
    Out $i;
   };
  ok Execute(out=>[2..9]);
 }

#latest:;
if (1)                                                                          #TAssert
 {Start 1;
  Assert;
  my $r = Execute(suppressErrors=>1);
  is_deeply $r->out, [
"Stack trace\n",
  "    1     1 assert\n",
];
 }

#latest:;
if (1)                                                                          #TAssertEq
 {Start 1;
  Mov 0, 1;
  AssertEq \0, 2;
  my $r = Execute(suppressErrors=>1);
  is_deeply $r->out, [
"Stack trace\n",
  "    1     2 assertEq\n",
];
 }

#latest:;
if (1)                                                                          # Temporary variable
 {my $s = Start 1;
  my $a = Mov 1;
  my $b = Mov 2;
  Out $a;
  Out $b;
  ok Execute(out=>[1..2]);
 }

#latest:;
if (1)                                                                          #TAlloc #TMov #TCall
 {Start 1;
  my $a = Alloc "aaa";
  Dump;
  my $e = Execute;
  is_deeply $e->out, [  "memory:\n[\n  \"-2=bless([], \\\"return\\\")\",\n  \"-1=bless([], \\\"params\\\")\",\n  \"0=bless([1], \\\"stackArea\\\")\",\n  \"1=bless([], \\\"aaa\\\")\",\n]",];
 }

#latest:;
if (1)                                                                          #TAlloc #TMov #TCall
 {Start 1;
  my $a = Alloc "aaa";
  my $i = Mov 1;
  my $v = Mov 11;
  ParamsPut 0, $a;
  ParamsPut 1, $i;
  ParamsPut 2, $v;
  my $set = Procedure 'set', sub
   {my $a = ParamsGet 0;
    my $i = ParamsGet 1;
    my $v = ParamsGet 2;
    Mov [$a, \$i], $v;
    Return;
   };
  Call $set;
  my $V = Mov [$a, \$i];
  AssertEq $v, $V;
  Out [$a, \$i];
  my $e = Execute;
  is_deeply $e->out, [11];
 }

#latest:;
if (1)                                                                          #TAlloc #TClear
 {Start 1;
  my $a = Alloc "aaa";
  Clear [$a, 10];
  my $e = Execute;
  is_deeply $e->memory->{1}, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
 }

#latest:;
if (1)                                                                          #TBlock
 {Start 1;
  Block
   {my ($start, $good, $bad, $end) = @_;
    Out 1;
    Jmp $good;
   }
  Good
   {Out 2;
   },
  Bad
   {Out 3;
   };
  Out 4;
  ok Execute(out=>[1,2,4], analyze=>0);
 }

#latest:;
if (1)                                                                          #TBlock
 {Start 1;
  Block
   {my ($start, $good, $bad, $end) = @_;
    Out 1;
    Jmp $bad;
   }
  Good
   {Out 2;
   },
  Bad
   {Out 3;
   };
  Out 4;
  ok Execute(out=>[1,3,4]);
 }

#latest:;
if (1)                                                                          #TProcedure
 {Start 1;
  for my $i(1..10)
   {Out $i;
   };
  IfTrue 0,
  Then
   {Out 99;
   };
  my $e = Execute;
  is_deeply $e->out, [1..10];
  ok $e->analyzeExecutionResults(analyze=>3) =~ m(#       12 instructions executed);
 }

#latest:;
if (1)                                                                          #DdoubleWrite
 {Start 1;
  Mov 1, 1;
  Mov 2, 1;
  Mov 3, 1;
  Mov 3, 1;
  Mov 1, 1;
  my $e = Execute;
  ok keys($e->doubleWrite->%*) == 2;                                            # In area 0, variable 1 was first written by instruction 0 then again by instruction 1 once.
  #say STDERR $e->analyzeExecutionResultsDoubleWrite(doubleWrite=>1);
 }

#latest:;
if (1)                                                                          #DpointlessAssign
 {Start 1;
  Add 2,  1, 1;
  Add 2, \2, 0;
  my $e = Execute;
  is_deeply $e->pointlessAssign, { 1 => 1 };
 }

#latest:;
if (0)                                                                          #DnotRead
 {Start 1;
  my $a = Mov 1;
  my $b = Mov $a;
  my $e = Execute;
  ok $e->notRead->{0}{1} == 1;                                                  # Area 0 == stack, variable 1 == $b generated by instruction 1
 }

#latest:;
if (1)                                                                          #TAlloc #TMov #TCall
 {Start 1;
  my $set = Procedure 'set', sub
   {my $a = ParamsGet 0;
   };
  ParamsPut 0, 1;
  Call $set;
  ParamsPut 0, 1;
  Call $set;
  my $e = Execute;
  is_deeply $e->out, [];
 }

#latest:;
if (1)                                                                          # invalid address
 {Start 1;
  Mov 1, \0;
  my $e = eval {Execute suppressErrors=>1};
  ok $@ =~ m"Invalid right area: undef address: \\0 stack: 0 ";
 }

#latest:;
if (1)                                                                          #TShiftUp
 {Start 1;
  my $a = Alloc "array";
  Mov [$a, 0], 0;
  Mov [$a, 1], 1;
  Mov [$a, 2], 2;
  ShiftUp [$a, 1], 99;
  my $e = Execute;
  is_deeply $e->memory, {1=>[0, 99, 1, 2]};
 }

#latest:;
if (1)                                                                          #TShiftDown
 {Start 1;
  my $a = Alloc "array";
  Mov [$a, 0], 0;
  Mov [$a, 1], 99;
  Mov [$a, 2], 2;
  my $b = ShiftDown [$a, 1];
  Out $b;
  my $e = Execute;
  is_deeply $e->memory, {1=>[0, 2]};
  is_deeply $e->out,    [99];
 }

#latest:;
if (1)                                                                          #TLeArea #TLeAddress
 {Start 1;
  my $a = Alloc "array";
  my $b = Mov 2;
  my $c = Mov 5;
  my $d = LeAddress $c;
  my $f = LeArea    [$a, 0];
  Out $d;
  Out $f;
  Mov [$a, \$b], 22;
  Mov [$a, \$c], 33;
  Mov [$f, \$d], 44;
  my $e = Execute;
  is_deeply $e->out,    [2,1];
  is_deeply $e->memory, {1=>[undef, undef, 44, undef, undef, 33]};
 }

#latest:;
if (1)                                                                          #TAlloc #TMov
 {Start 1;
  my $a = Alloc "aaa";
  my $b = Alloc "bbb";
  Mov [$a, 0], $b;
  Mov [$b, 0], 99;
  For 3, sub
   {my ($i, $check, $next, $end) = @_;
    my $c = Mov [$a, \0];
    my $d = Mov [$c, \0];
    Jeq $next, $d, $d;
   };
  my $e = Execute;
  is_deeply $e->analyzeExecutionResults(doubleWrite=>3), "#       33 instructions executed";
  is_deeply $e->memory, { 1 => bless([2], "aaa"), 2 => bless([99], "bbb") };
 }

#latest:;
if (1)                                                                          #TAlloc #TMov
 {Start 1;
  my $a = Alloc "aaa";
  my $b = Mov 2;                                                                # Location to move to in a
  Mov [$b, 0], 99;
  For 3, sub
   {my ($i, $check, $next, $end) = @_;
    Mov [$a, \$b], 1;
    Jeq $next, [$a, \$b], 1;
   };
  my $e = Execute;
  is_deeply $e->analyzeExecutionResults(doubleWrite=>3), "#       29 instructions executed";
  is_deeply $e->memory, {1 => bless([undef, undef, 1], "aaa"), 2 => [99]};
 }
