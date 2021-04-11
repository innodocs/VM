##
##  test-06.gap
##
#A  Ovidiu Podisor
#C  Copyright © 2019-2021 innodocs. All rights reserved.
##
##  test type inference
#

#
# this should work
#   a: Int
#   b: Int
#
a := b;
b := a;
c := a + b;
Print("a = ", a, ", b = ", b, "\n");

#
# this should fail
#   x: Any
#   y: Any
#   no suitable function for Print[Any]
#
x := y;
y := x;
Print("x = ", x, ", y = ", y, "\n");
