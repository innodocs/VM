# VM

<h2>Virtual Machine Development</h2>

The goal of this "VM" project is to build a language system complete
with virtual machines, compilers, assemblers and translators
(GAP to C etc).

The system is being constructed in small steps, gradually and
incrementally for the purpose of accessibility. The material
could also be useful in a CS introductory
lecture on language systems. The chapters are as follows:

* C1: abstract syntax for Straight Line Programs (SLP); compiler/emitter of VM code; and Virtual Machine for SLPs (C++, SML-NJ, Scala)
* C2: add emitter for assembly language to compiler; add simple assembler (Antlr, Java, Scala)
* C3: lexers/parsers for SLP (Antlr, ml-lex, ml-yacc)
* C4: replace SLP w/ GAP (Group, Algorithms and Programming) language, implement 'if' and 'while' statements
* C5: add 'for' and 'repeat' statements
* C6: add string type to compiler, add string pool, string constants, string printing to VM/Asm

<h3>Directory Structure</h3>

Each chapter directory is structured in the same way:

* vm: the actual virtual machine (C++)
* asm: the assembler (Scala, Antlr)
* comp: the compiler for SLP (C1-C3), and GAP; the 'scala'
  sub-directory holds the scala version of the compiler, 'sml'
  holds the SML-NJ version
* test: test programs

<h3>Building the System</h3>

Given our goal of accessibility, we have attempted to make building
and experimenting with the system as simple as possible. As
such we have refrained from using build systems with involved
directory structures, rules, learning curves, and have instead opted
to place all required files in a single directory together with a
simple ant build file. Even Ant is not required, as it is
simple enough to build the various (sub)systems manually.





