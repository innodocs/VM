//
// vm-asm 1.2
//
//GLOBALS 2
//STRINGS 2

// a := 5 + 3;
IPUSHC 5
IPUSHC 3
IADD
ISTOREG 0

// b := 0;
// while b <= 3 do
//   i := i + 1;
//   Print(i);
// od;
IPUSHC 0
ISTOREG 1
while1_start:
IPUSHC 3
ILOADG 1
IF_ICMPLT while1_end
ILOADG 1
IPUSHC 1
IADD
IDUP
ISTOREG 1
IPRINT 1
SPUSHC "\n"
SPRINT 1
GOTO while1_start
while1_end:

// b := 10 * a;
IPUSHC 10
ILOADG 0
IMULT
ISTOREG 1

// Print("10 * a = ", b)
SPUSHC "10 * a = "
SPRINT 1
ILOADG 1
IPRINT 1
SPUSHC "\n"
SPRINT 1
end:
HALT
