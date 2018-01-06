module Main
  where
import Vertices
import Data.Ratio

main :: IO ()
main = do
  let x = newVar 1
      y = newVar 2
      z = newVar 3
      vars = [x,y,z]
      c1 = linearCombination vars [(1,x), (1,y), (1,z)] .=. constant 0
      c2 = linearCombination vars [(1,z)] .<. constant (10)
      c3 = linearCombination vars [(1,x), (-1,y)] .>. constant (-10)
      c4 = linearCombination vars [(1,x), (1,y)] .<. constant (5%2)
      c5 = linearCombination vars [(1,x), (-1,y)] .<. constant 0
      c6 = linearCombination vars [(1,x), (-1,y)] .>. constant (-1)
  result <- getVertices [c1,c2,c3,c4,c5,c6]
  print result
