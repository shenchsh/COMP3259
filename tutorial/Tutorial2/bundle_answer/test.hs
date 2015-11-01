import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Either(..))

import Data.List
import Data.Ord

import Interp
import Declare

main = defaultMain tests


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

getRight :: Either a b -> b
getRight (Right a) = a

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Tutorial 2" [q3test, q4test, q5test, q6test, q7test, q8test]


q3test = testGroup "Q3"
           [ testCase "t1" $ evaluate (Neg (Num 3)) @?= -3
           , testCase "t2" $ evaluate (Power (Num 2) (Num 3)) @?= 8
           ]

q4test = testGroup "Q4"
           [testCase "t1" $ calc "1 + 8 * 2" @?= 17, testCase "t2" $ calc "2 * (8 + -6) ^ 3" @?= 16]

q5test = testGroup "Q5"
           [ testCase "t1" $ show e1 @?= "(3 + 4)"
           , testCase "t2" $ show e2 @?= "(3 + ((4 - 5) * 7))"
           , testCase "t3" $ show e3 @?= "(((1 + 2) / 3) - ((5 - 6) * 8))"
           ]

q6test = testGroup "Q6"
           [ testCase "t1" $ getRight (evaluate2 (Add (Sub (Num 3) (Num 2)) (Mult (Num 2) (Num 3)))) @?= 7
           , testCase "t2" $ isLeft (evaluate2 (Div (Num 2) (Num 0))) @?= True
           , testCase "t3" $ isLeft (evaluate2 (Power (Num 2) (Num (-3)))) @?= True
           , testCase "t4" $ getRight (calc2 "2 ^ 3 + 1") @?= 9
           , testCase "t5" $ getRight (calc2 "6 / 3 + 6") @?= 8
           ]

q7test = testGroup "Q7"
           [ testCase "t1" $ getRight (bindE (Right 3) (\n -> Right (n + 1))) @?= 4
           , testCase "t2" $ isLeft (bindE (Left "bad things") (\n -> Right (n + 1))) @?= True
           ]

q8test = testGroup "Q8"
           [ testCase "t1" $ getRight (calc3 "1 + 8 * 2") @?= 17
           , testCase "t2" $ isLeft (calc3 "2 * (4 - 2) / (8 - 8)") @?= True
           , testCase "t3" $ isLeft (calc3 "2 * (4 - 2) ^ (6 - 8)") @?= True
           ]
