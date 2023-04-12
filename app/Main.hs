module Main (main) where

import Evaluation
import Syntax
import Typing

example1 :: Raw
example1 = RLet "id" (Function Unit Unit) (RLam "x" (RVar "x")) (RVar "id")

example2 :: Raw
example2 = RLet "id" (Function (Function Unit Unit) (Function Unit Unit)) (RLam "x" (RVar "x")) (RVar "id")

test :: Raw -> IO ()
test raw =
  case infer [] raw of
    Just (tm, ty) -> do
      putStrLn "Elaborated term:\n"
      print tm
      putStrLn "\nHas type:\n"
      print ty
      putStrLn "\nEvaluates to:\n"
      print (nbe tm)
    Nothing -> putStrLn "Type error!"

main :: IO ()
main = test example1
