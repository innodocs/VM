/*
**  test-03.asm
**
*A  Christina Podisor
*C  Copyright © 2019-2021 innodocs. All rights reserved.
**
**  s := 0;
**  i := 0;
**  while i < 10 do
**    i := i + 1;
**    s := s + i;
**  od
**
*/

IPUSHC 0
IDUP
ISTOREG 0
ISTOREG 1
loop_start:
ILOADG 1
IPUSHC 10
IF_ICMPGE end
ILOADG 0
ILOADG 1
IADD
ILOADG 1
IPUSHC 1
IADD
ISTOREG 1
ISTOREG 0
GOTO loop_start
end:
ILOADG 0
IPRINT 1
SPUSHC "\n"
SPRINT 1
