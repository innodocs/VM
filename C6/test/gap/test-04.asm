//
// vm-asm 1.3
//
GLOBALS 5
STRINGS 3
IPUSHC 5
IPUSHC 3
IADD
ISTOREG 0
IPUSHC 4
IPUSHC 2
ISUB
ISTOREG 1
if1_start:
ILOADG 0
ILOADG 1
IF_ICMPGE if1_else
ILOADG 0
IPRINT 1
SPUSHC "\n"
SPRINT 1
GOTO if1_end
if1_else:
ILOADG 1
IPRINT 1
SPUSHC "\n"
SPRINT 1
if1_end:
IPUSHC 1
ISTOREG 2
for1_start:
ILOADG 2
IDUP
IPUSHC 10
IF_ICMPGT for1_end
if2_start:
ILOADG 2
IPUSHC 2
IMOD
IPUSHC 0
IF_ICMPNE if2_end
ILOADG 2
IPRINT 1
SPUSHC "\n"
SPRINT 1
if2_end:
IPUSHC 1
IADD
ISTOREG 2
GOTO for1_start
for1_end:
IPOP
IPUSHC 1
ISTOREG 3
while1_start:
ILOADG 3
IPUSHC 10
IF_ICMPGT while1_end
if3_start:
ILOADG 3
IPUSHC 2
IMOD
IPUSHC 0
IF_ICMPNE if3_end
ILOADG 3
IPRINT 1
SPUSHC "\n"
SPRINT 1
if3_end:
ILOADG 3
IPUSHC 1
IADD
ISTOREG 3
GOTO while1_start
while1_end:
IPUSHC 1
ISTOREG 3
repeat1_start:
if4_start:
ILOADG 3
IPUSHC 2
IMOD
IPUSHC 0
IF_ICMPNE if4_end
ILOADG 3
IPRINT 1
SPUSHC "\n"
SPRINT 1
if4_end:
ILOADG 3
IPUSHC 1
IADD
ISTOREG 3
ILOADG 3
IPUSHC 10
IF_ICMPLE repeat1_start
repeat1_end:
IPUSHC 0
ISTOREG 2
IPUSHC 0
ISTOREG 4
while2_start:
ILOADG 4
IPUSHC 200
IF_ICMPGT while2_end
ILOADG 2
IPUSHC 1
IADD
ISTOREG 2
ILOADG 4
ILOADG 2
IPUSHC 2
IPOW
IADD
ISTOREG 4
GOTO while2_start
while2_end:
SPUSHC "i = "
SPRINT 1
ILOADG 2
IPRINT 1
SPUSHC ", s = "
SPRINT 1
ILOADG 4
IPRINT 1
SPUSHC "\n"
SPRINT 1
IPUSHC 0
ISTOREG 2
IPUSHC 0
ISTOREG 4
repeat2_start:
ILOADG 2
IPUSHC 1
IADD
ISTOREG 2
ILOADG 4
ILOADG 2
IPUSHC 2
IPOW
IADD
ISTOREG 4
ILOADG 4
IPUSHC 200
IF_ICMPLE repeat2_start
repeat2_end:
SPUSHC "i = "
SPRINT 1
ILOADG 2
IPRINT 1
SPUSHC ", s = "
SPRINT 1
ILOADG 4
IPRINT 1
SPUSHC "\n"
SPRINT 1
HALT
