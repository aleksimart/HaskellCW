{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
-- Changed Challenges to Lib
module Lib (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random


-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-- END OF CODE YOU MUST NOT MODIFY

-- ADD YOUR OWN CODE HERE

-- Challenge 1 --

-- Should work with no words as well
solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch ws css 
    |(not.checkGridValidity) css  = error "Invalid Grid, it mustn't be empty or not square"
    |(not.checkWordsValidity) ws  = error "Invalid list of words, a word cannot be an empty string"
    | otherwise                   = map (placementSearch css) tuples  
  where
    starts = map (startLetter css 0 0) ws
    tuples = zip ws starts

checkGridValidity :: WordSearchGrid -> Bool
checkGridValidity [] = False
checkGridValidity g@(cs:_) = length g == length cs

checkWordsValidity :: [ String ] -> Bool
checkWordsValidity [] = False
checkWordsValidity ([]:_) = False
checkWordsValidity [w] = True
checkWordsValidity (w:ws) = checkWordsValidity ws

-- Function searches for all locations where the first letter of the word
-- matches the letter in the cell or returns Nothing if none are found
-- w = word, c = cell, l = letter 
startLetter :: WordSearchGrid -> Int -> Int -> String ->Maybe [Posn]
startLetter  _ _ _ []= error "This game won't work with an empty string"
startLetter [] _ _ _ = Just []
startLetter ([]:css) row _ w = startLetter css (row + 1) 0 w
startLetter ((c:cs):css) row col w@(l:_) | l == c = fmap ((col,row):) nextcellSearch
  | otherwise = nextcellSearch
  where 
    leftcells = cs:css
    nextcellSearch = startLetter leftcells row (col + 1) w 

-- Function attempts to find the placement for a particular word
-- based on the list of positions passed as a parameter
-- All the directions are attempted for all the positions and if no placement is found
-- then return Nothing
-- w = word, p = position, c = cell
placementSearch :: WordSearchGrid -> (String, Maybe [Posn])  -> (String, Maybe Placement)
placementSearch _ (w, Nothing) = (w, Nothing)
placementSearch _ (w, Just[]) = (w, Nothing)
placementSearch css (w, Just (p:ps)) 
  | findWord css Forward w p          = (w, Just(p, Forward))
  | findWord css DownForward w p      = (w, Just(p, DownForward))
  | findWord css Down w p             = (w, Just(p, Down))
  | findWord css DownBack w p         = (w, Just(p, DownBack))
  | findWord css Back w p             = (w, Just(p, Back))
  | findWord css UpBack w p           = (w, Just(p, UpBack))
  | findWord css Up w p               = (w, Just(p, Up))
  | findWord css UpForward w p        = (w, Just(p, UpForward))
  | otherwise                         = placementSearch css (w, Just ps) 

-- Function attempt to compare all the word's letters with all the cells in a particular Orientation
-- returns True if in a particular Orientation we get a word and false
-- if at any point the position leaves the valid range or the letter of the words isn't the same as in
-- the cell
-- c = cell, l = letter, col = column, row = row, p = position
findWord :: WordSearchGrid -> Orientation -> String -> Posn -> Bool
findWord _ _ [] _ = True
findWord css Forward (l:ls) p@(col, row)
  | col > (length css - 1)                           = False
  | l == findLetter css p                            = findWord css Forward ls (col + 1, row)
  | otherwise = False
findWord css DownForward (l:ls) p@(col, row) 
  | col > (length css - 1) || row > (length css - 1) = False
  | l == findLetter css p                            = findWord css DownForward ls (col + 1, row + 1)
  | otherwise                                        = False
findWord css Down (l:ls) p@(col, row) 
  | row > (length css - 1)                           = False
  | l == findLetter css p                            = findWord css Down ls (col, row + 1)
  | otherwise                                        = False
findWord css DownBack (l:ls) p@(col, row) 
  | col < 0 || row > (length css - 1)                = False
  | l == findLetter css p                            = findWord css DownBack ls (col - 1, row + 1)
  | otherwise                                        = False
findWord css Back (l:ls) p@(col, row)
  | col < 0                                          = False
  | l == findLetter css p                            = findWord css Back ls (col - 1, row)
  | otherwise                                        = False
findWord css UpBack (l:ls) p@(col, row) 
  | col < 0 || row < 0                               = False
  | l == findLetter css p                            = findWord css UpBack ls (col - 1, row - 1)
  | otherwise                                        = False
findWord css Up (l:ls) p@(col, row) 
  | row < 0                                          = False
  | l == findLetter css p                            = findWord css Up ls (col, row - 1)
  | otherwise                                        = False
findWord css UpForward (l:ls) p@(col, row) 
  | col > (length css - 1) || row < 0                = False
  | l == findLetter css p                            = findWord css UpForward ls (col + 1, row - 1)
  | otherwise                                        = False 

-- TODO: Add Intermidiary Check for point validity (Possibly)
-- Function searches for a character located at a particular position
findLetter :: WordSearchGrid -> Posn -> Char 
findLetter css (col, row) = (css!!row)!!col

-- Two examples for you to try out, the first of which is in the instructions

exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]


-- Challenge 2 --

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch _ _ = return []


--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws



-- Challenge 3 --

prettyPrint :: LamMacroExpr -> String
prettyPrint _ = ""

-- examples in the instructions
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) 


-- Challenge 4 --

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro _ = Nothing 


-- Challenge 5

cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform _ = LamDef [] (LamVar 0)

-- Examples in the instructions
exId =  (LamDef [] (LamAbs 1 (LamVar 1)))
ex5'1 = (LamApp (LamVar 1) (LamVar 2))
-- ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
-- ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
-- ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))


-- Challenge 6

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing) 

-- Examples in the instructions

-- (\x1 -> x1 x2)
-- ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

-- --  def F = \x1 -> x1 in F  
-- ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

-- --  (\x1 -> x1) (\x2 -> x2)   
-- ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

-- --  (\x1 -> x1 x1)(\x1 -> x1 x1)  
-- wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
-- ex6'4 = LamDef [] (LamApp wExp wExp)

-- --  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
-- ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

-- --  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
-- ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- -- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1 ) (\x1 -> x1 x1)) ID
-- ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 
