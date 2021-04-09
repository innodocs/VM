##
##  test-04.gap
##
#A  Christina Podisor
#C  Copyright © 2019-2021 innodocs. All rights reserved.
##

#
# test 'if'
#
a := 5+3;
b := 4-2;
if a < b then
  Print(a, "\n");
else
  Print(b, "\n");
fi;

#
# test 'for' loop
#
for i in [1 .. 10] do
  if i mod 2 = 0 then
    Print(i, "\n");
  fi;
od;

#
# test 'while' loop
#
c := 1;
while c <= 10 do
  if c mod 2 = 0 then
    Print(c, "\n");
  fi;
  c := c + 1;
od;

#
# test 'repeat' loop
#
c := 1;
repeat
  if c mod 2 = 0 then
    Print(c, "\n");
  fi;
  c := c + 1;
until c > 10;

#
# sum up the squares 1^2, 2^2, ... until the sum exceeds 200
#
i := 0;; s := 0;;
while s <= 200 do
  i := i + 1; s := s + i^2;
od;
Print("i = ", i, ", s = ", s, "\n");

#
# sum up the squares 1^2, 2^2, ... until the sum exceeds 200
#
i := 0;; s := 0;;
repeat
  i := i + 1; s := s + i^2;
until s > 200;
Print("i = ", i, ", s = ", s, "\n");

