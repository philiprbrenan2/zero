# Zero

![Test](https://github.com/philiprbrenan/zero/workflows/Test/badge.svg)

A minimal [assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler) and [emulator](https://en.wikipedia.org/wiki/Emulator) for the Zero programming language.

The goal is to implement N-Way [trees](https://en.wikipedia.org/wiki/Tree_(data_structure)) in Zero [assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler) [code](https://en.wikipedia.org/wiki/Computer_program), optimize the [assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler) [code](https://en.wikipedia.org/wiki/Computer_program) assiduously through exhaustive testing, then realize the
algorithm as silicon hardware rather than as software so that large associative
memories can be manufactured on an industrial scale.

Open the __Actions__ [tab](https://en.wikipedia.org/wiki/Tab_key) to see the [code](https://en.wikipedia.org/wiki/Computer_program) in action.

The initial idea is to produce a small CPU which implements just the
instructions needed to implement the algorithm.  The small CPU will then be
replicated across an [fpga](https://en.wikipedia.org/wiki/Field-programmable_gate_array) so that the [tree](https://en.wikipedia.org/wiki/Tree_(data_structure)) can be processed in parallel rather
in the same way that a gpu [processes](https://en.wikipedia.org/wiki/Process_management_(computing)) pixels.

Only one [tree](https://en.wikipedia.org/wiki/Tree_(data_structure)) will be used: typically to map 64 [bit](https://en.wikipedia.org/wiki/Bit) keys into 64 [bit](https://en.wikipedia.org/wiki/Bit) data.
However, it might be useful to add additional data at the front of the keys
such as data length, data position, [process](https://en.wikipedia.org/wiki/Process_management_(computing)) id, [userid](https://en.wikipedia.org/wiki/User_identifier) etc. As the keys are
sorted in the [tree](https://en.wikipedia.org/wiki/Tree_(data_structure)) [trees](https://en.wikipedia.org/wiki/Tree_(data_structure)) with similar prefixes will tend to collect together so
we can compress out the common prefix of the keys in each node.

All communications with the chip will be done via usb .

If you would like to be involved with this project, please raise an issue
saying so!
