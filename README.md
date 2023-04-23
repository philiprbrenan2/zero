# Zero

![Test](https://github.com/philiprbrenan/zero/workflows/Test/badge.svg)

A minimal [assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler) and [emulator](https://en.wikipedia.org/wiki/Emulator) for the Zero programming language.

The goal is to implement N-Way [trees](https://en.wikipedia.org/wiki/Tree_(data_structure)) in Zero [assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler) [code](https://en.wikipedia.org/wiki/Computer_program), optimize the [assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler) [code](https://en.wikipedia.org/wiki/Computer_program) assiduously through exhaustive testing, then realize the
algorithm as an [integrated circuit](https://en.wikipedia.org/wiki/Integrated_circuit) rather than as software so that large, extremely
fast associative memories can be manufactured on an industrial scale.

Open the __Actions__ [tab](https://en.wikipedia.org/wiki/Tab_key) to see the [code](https://en.wikipedia.org/wiki/Computer_program) in action.

The initial idea is to produce a small CPU which implements just the
instructions needed to implement the algorithm.  The small CPU will then be
replicated across an [fpga](https://en.wikipedia.org/wiki/Field-programmable_gate_array) so that the [tree](https://en.wikipedia.org/wiki/Tree_(data_structure)) can be queried in parallel rather in
the same way that a gpu [processes](https://en.wikipedia.org/wiki/Process_management_(computing)) pixels.

Only one [tree](https://en.wikipedia.org/wiki/Tree_(data_structure)) will be used: typically mapping 64 [bit](https://en.wikipedia.org/wiki/Bit) keys into 64 [bit](https://en.wikipedia.org/wiki/Bit) data. It
will be useful to add additional data at the front of the keys such as data
length, data position, [process](https://en.wikipedia.org/wiki/Process_management_(computing)) id, [userid](https://en.wikipedia.org/wiki/User_identifier), time stamp etc. As the keys are
sorted in the [tree](https://en.wikipedia.org/wiki/Tree_(data_structure)) [trees](https://en.wikipedia.org/wiki/Tree_(data_structure)) with similar prefixes will tend to collect together so
we can compress out the common prefix of the keys in each node.

Strings longer than 64 bits can be processed in pieces with each piece prefixed
by the [string](https://en.wikipedia.org/wiki/String_(computer_science)) id and the position in the [string](https://en.wikipedia.org/wiki/String_(computer_science)).  Incoming [strings](https://en.wikipedia.org/wiki/String_(computer_science)) can be made
unique by assigning a unique 64 [bit](https://en.wikipedia.org/wiki/Bit) number to each prefix of the [string](https://en.wikipedia.org/wiki/String_(computer_science)) so that
a second such [string](https://en.wikipedia.org/wiki/String_(computer_science)) can be easily recognized.  Once such a long [string](https://en.wikipedia.org/wiki/String_(computer_science)) has
been represented by unique number it can be located in one descent through the [tree](https://en.wikipedia.org/wiki/Tree_(data_structure)) although a single traversal of the [tree](https://en.wikipedia.org/wiki/Tree_(data_structure)) will no longer yield such [strings](https://en.wikipedia.org/wiki/String_(computer_science)) in alphabetic order.

All communications with the chip will be done via [USB](https://en.wikipedia.org/wiki/USB) .  Incoming read requests
can be done in parallel as long as there are processors left to assign work to.
An update will have to wait for all existing finds to finish while stalling all
trailing actions until the update is complete.

Associative look ups are the 'sine qua non' of all [Turing](https://en.wikipedia.org/wiki/Alan_Turing) complete programming
languages. This arrangement should produce very fast associate look ups - much
faster than can be performed by any system reliant on external software. Usage
of power and [integrated circuit](https://en.wikipedia.org/wiki/Integrated_circuit) surface area should be reduced by having a minimal CPU to
perform the looks ups. Being able to deliver such looks up faster than can be
done with conventional software solutions might prove profitable in much the
same way as graphics chips and other chips used at scale.

If you would like to be involved with this project, please raise an issue
saying so!
