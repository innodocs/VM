/*
**  PE-001-03-stackerror.asm
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
ISTOREG 0
IPUSHC 0
ISTOREG 1

start_loop_5:
IPUSHC 1
ILOADG 0
IADD
ISTOREG 0
ILOADG 0
IPUSHC 5
IMULT
IDUP
IPUSHC 1000
IF_ICMPGE reset_loop
ILOADG 1
IADD
ISTOREG 1
GOTO start_loop_5

reset_loop:
IPOP
IPUSHC 0
ISTOREG 0

start_loop_3:
IPUSHC 1
ILOADG 0
IADD
ISTOREG 0
ILOADG 0
IPUSHC 3
IMULT
IDUP
IPUSHC 1000
IF_ICMPGE end
IDUP
IPUSHC 5
IMOD
IF_IEQ start_loop_3
ILOADG 1
IADD
ISTOREG 1
GOTO start_loop_3

end:
IPOP
ILOADG 1
IPRINT 1
SPUSHC "\n"
SPRINT 1
