/*
**  test-01.asm
**
*A  Christina Podisor
*C  Copyright © 2019-2021 innodocs. All rights reserved.
**
**  Print(1+2, 3+4)
*/

IPUSHC 1
IPUSHC 2
IADD
IPUSHC 3
IPUSHC 4
IADD
IPRINT 2
SPUSHC "\n"
SPRINT 1
