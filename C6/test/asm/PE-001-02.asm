/*
**  PE-001-02.asm
**
*A  Christina Podisor
*C  Copyright © 2019-2021 innodocs. All rights reserved.
**
**  Project Euler Problem 1
**
**  If we list all the natural numbers below 10 that are multiples of 3 or 5,
**  we get 3, 5, 6 and 9. The sum of these multiples is 23.
**
**  Find the sum of all the multiples of 3 or 5 below 1000.
*/

IPUSHC 0
IDUP
ISTOREG 0
ISTOREG 1
start_loop:
ILOADG 1
IPUSHC 1000
IF_ICMPGE end
ILOADG 1
IPUSHC 3
IMOD
ILOADG 1
IPUSHC 5
IMOD
IAND
IF_INE if_not_div_by_3_or_5
ILOADG 0
ILOADG 1
IADD
ISTOREG 0
if_not_div_by_3_or_5:
ILOADG 1
IPUSHC 1
IADD
ISTOREG 1
GOTO start_loop
end:
ILOADG 0
IPRINT 1
SPUSHC "\n"
SPRINT 1



