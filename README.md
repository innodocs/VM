# VM

<h2>Virtual Machine Development</h2>

The goal of this "VM" project is to build a language system complete
with virtual machines, compilers, assemblers and translators
(GAP to C etc).

The system is being constructed in small steps, gradually and
incrementally for the purpose of accessibility. The material
could also be useful in a CS introductory
lecture on language systems. The chapters are as follows:

* C1: abstract syntax for SLP (Straight Line Programs [APPL1998], pg 7 - 12);
  compiler/emitter of VM code; and Virtual Machine for SLPs (C++, SML-NJ, Scala)
* C2: add emitter for assembly language to compiler;
  add simple assembler (Antlr, Java, Scala)
* C3: lexers/parsers for SLP (Antlr, ml-lex, ml-yacc)
* C4: replace SLP w/ GAP language (Groups, Algorithms, and Programming [GAP2021]),
  implement 'if' and 'while' statements
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
simple enough to build the various (sub)systems by hand.

<h4>Building with Ant</h4>

Define the `SCALA_HOME`, `ANTLR_HOME` and `ANTLR_CLASSPATH` environment variables, e.g.:

        export SCALA_HOME="/usr/local/Cellar/scala/2.13.5/libexec/"
        export ANTLR_HOME="/usr/local/Cellar/antlr/4.9.2/"
        export ANTLR_CLASSPATH="$ANTLR_HOME/antlr-4.9.2-complete.jar"
        
        export PATH="$ANTLR_HOME/bin:$PATH"
        export CLASSPATH="$ANTLR_CLASSPATH:$CLASSPATH"

For each subsystem:

* cd to sub-system folder (`vm`, `asm`, `comp/scala`)
* run `ant`:

Task | Command
------------ | -------------
to build the system | `ant`
to build and test   | `ant test`
to cleanup          | `ant clean`

* if everything went OK, there will be`vm`, `vm-asm` and `vm-comp` binaries in
  the `bin` folder. To test/run:
  
      cd test
      ../bin/vm-asm asmtest.asm asmtest.vm
      ../bin/vm asmtest.vm
      
      ../bin/vm-comp test4.gap test4.vm
      ../bin/vm test4.vm


For the `sml` build:

    cd comp/sml
    sml sources.cm
    Test.run();


<h3>Bibliography</h3>

[APPL1998] Andrew W. Appel, *Modern Compiler Implementation in ML*; 1998. (https://www.cs.princeton.edu/~appel/modern/ml/)

[GAP2021] The GAP Group, *GAP -- Groups, Algorithms, and Programming, Version 4.11.1*; 2021. (https://www.gap-system.org)


