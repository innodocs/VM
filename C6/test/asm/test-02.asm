/*
**  test-02.asm
**
*A  Christina Podisor
*C  Copyright © 2019-2021 innodocs. All rights reserved.
**
**  Add all the numbers between 1 and 10, then print the result.
*/

IPUSHC 1
IPUSHC 2
IPUSHC 3
IPUSHC 4
IPUSHC 5
IPUSHC 6
IPUSHC 7
IPUSHC 8
IPUSHC 9
IADD
IADD
IADD
IADD
IADD
IADD
IADD
IADD
IPRINT 1
SPUSHC "\n"
SPRINT 1

