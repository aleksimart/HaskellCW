module Main where

import           Lib

main :: IO ()
main = do
  putStrLn "Example usage of functions from challenges"
  putStrLn
    "============================================ Challenge 1 ============================================"
  putStrLn "Solving a 3x3 word search with a word 'ZIP' hidden in it"
  print $ solveWordSearch ["ZIP"] ["ZYL", "CIN", "EQP"]
  putStrLn
    "============================================ Challenge 2 ============================================"
  putStrLn
    "Creating your own word search with words: 'LIKE', 'PEACE', 'SPACE', with density 0.6"
  grid <- createWordSearch ["LIKE", "PEACE", "SPACE"] 0.6
  print grid
  putStrLn
    "============================================ Challenge 3 ============================================"
  let expr = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
  putStrLn $ "Pretty printing a lambda expression with macros: " ++ show expr
  print $ prettyPrint expr
  putStrLn
    "============================================ Challenge 4 ============================================"
  putStrLn "Parsing a lambda expression: \\x1 -> x1 x2"
  print $ parseLamMacro "\\x1 -> x1 x2"
  putStrLn
    "============================================ Challenge 5 ============================================"
  let expr1 = LamDef [("F", LamAbs 1 (LamVar 1))] (LamVar 2)
  putStrLn $ "CPS transformation of a lambda expression: " ++ show expr1
  print $ cpsTransform expr1
  putStrLn
    "============================================ Challenge 6 ============================================"
  let expr2 = LamDef [("F", (LamAbs 1 (LamVar 1)))] (LamMacro "F")
  putStrLn
    $  "Check number of inner and outer reduction in an expression: "
    ++ show expr2
    ++ " and it's csp transformed version with bound 10"
  print $ compareInnerOuter expr2 10
  putStrLn
    "====================================================================================================="
  putStrLn "All the other examples can be checked in the Spec.hs"



