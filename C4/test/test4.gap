a := 5+3;
b := 4-2;
if a < b then
  Print(a);
else
  Print(b);
fi;

c := 1;
while c <= 10 do
  if c mod 2 = 0 then
    Print(c);
  fi;
  c := c + 1;
od;

#
# sum up the squares 1^2, 2^2, ... until the sum exceeds 200
#
i := 0;; s := 0;;
while s <= 200 do
  i := i + 1; s := s + i^2;
od;
Print(i, s);
