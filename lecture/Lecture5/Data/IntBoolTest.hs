import Prelude hiding (LT, GT, EQ, id)
import IntBool
import IntBoolParse
import Base hiding (check)

--BEGIN:More19
t1 = "4"
t2 = "-4 - 6"
t3 = "if (3==6) -2; else -7"
t4 = "3*(8 + 5)"
t5 = "3 + 8 * 2"
--END:More19

--BEGIN:More98
t6 = "if (3 > 3*(8 + 5)) 1; else 0"
t7 = "2 + (if (3 <= 0) 9; else -5)"
--END:More98

t9 = "var x = 3; var y = -2; if (x > y) true; else false"

test1 = do
  test "execute" execute (parseExp t1)
  test "execute" execute (parseExp t2)
  test "execute" execute (parseExp t3)
  test "execute" execute (parseExp t4)
  test "execute" execute (parseExp t5)
  test "execute" execute (parseExp t6)
  test "execute" execute (parseExp t7)

--BEGIN:Type6
err1 = "if (3) 5; else 8"
err2 = "3 + true"
err3 = "3 || true"
err4 = "-true"
--END:Type6

test2 = do
  test "execute" execute (parseExp err1)
  test "execute" execute (parseExp err2)
  test "execute" execute (parseExp err3)
  test "execute" execute (parseExp err4)

main = do
  tagged "More23" test1
  tagged "Type6run" test2