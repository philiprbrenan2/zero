use v5.30;
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);

my @a = qw(
Start
Add
Alloc
Free
Call
Confess
Inc
Jmp
Jle
Jlt
Jge
Jgt
Jeq
Jne
Label
Mov
Nop
Out
Procedure
ParamsGet
ParamsPut
Return
ReturnGet
ReturnPut
Pop
Push
Smaller
Get
Put
Copy
Then
Else
Ifx
IfEq
IfNe
IfLt
IfLe
IfGt
IfGe
For
Execute);

say join ' ', sort @a;
