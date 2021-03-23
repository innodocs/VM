/*
**  test-01.asm
**
*A  Christina Podisor
*C  Copyright © 2019-2021 innodocs. All rights reserved.
**
**  a := 5 + 3;
**  b := 0;
**  while b <= 3 do
**    b := b + 1;
**    Print(b);
**  od;
**
**  b := 10 * a;
**  Print("a = ", a);
**  Print("10 * a = ", b);
*/

IPUSHC 5
IPUSHC 3
IADD
ISTOREG 0

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

IPUSHC 10
ILOADG 0
IMULT
ISTOREG 1
SPUSHC "a = "
SPRINT 1
ILOADG 0
IPRINT 1
SPUSHC "\n"
SPRINT 1
SPUSHC "10 * a = "
SPRINT 1
ILOADG 1
IPRINT 1
SPUSHC "\n"
SPRINT 1

