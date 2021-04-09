##
##  test-05.gap
##
#A  Ovidiu Podisor
#C  Copyright © 2019-2021 innodocs. All rights reserved.
##
##  test for loop
##
##  in GAP, for loop variable is not affected by assignments in the loop block.
##  the output from the program below in GAP is
##
##      1, 11
##      2, 12
##      ...
##      10, 20
##      after loop: i = 10
##
##  and not
##
##      1, 11
##      after loop: i = 11
##
#

for i in [1 .. 10] do
  Print(i, ", ");
  i := i + 10;
  Print(i, "\n");
od;
Print("after for loop i = ", i, "\n");
