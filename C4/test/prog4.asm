//
// vm-asm 1.2
//
GLOBALS 2
IPUSHC 5
IPUSHC 3
IADD
ISTOREG 0
IPUSHC 4
IPUSHC 2
ISUB
ISTOREG 1
If1Start:
ILOADG 0
ILOADG 1
IF_ICMPGE If1Else
ILOADG 0
IPRINT 1
GOTO If1End
If1Else:
ILOADG 1
IPRINT 1
If1End:
HALT