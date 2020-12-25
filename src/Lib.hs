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
import Data.Maybe 


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
    |(not.checkGridValidity) grid  = error "Invalid Grid, it mustn't be empty or not square"
    |(not.checkWordsValidity) words  = error "Invalid list of words, a word cannot be an empty string"
    | otherwise                   = map (placementSearch grid) tuples  
  where
    words = upperCase ws
    grid = upperCase css
    starts = map (startLetter grid 0 0) words
    tuples = zip words starts

checkGridValidity :: WordSearchGrid -> Bool
checkGridValidity [] = False
checkGridValidity g@(cs:_) = length g == length cs

checkWordsValidity :: [ String ] -> Bool
checkWordsValidity [] = False
checkWordsValidity ([]:_) = False
checkWordsValidity [_] = True
checkWordsValidity (_:ws) = checkWordsValidity ws

upperCase :: [String] -> [String]
upperCase = map (map toUpper) 

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

-- Function attempts to compare all the word's letters with all the cells in a particular Orientation
-- returns True if in a particular Orientation we get a word and false
-- if at any point the position leaves the valid range or the letter of the words isn't the same as in
-- the cell
-- c = cell, l = letter, col = column, row = row, p = position
-- TODO combine two guards maybe
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

-- Challenge 2 --
createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch ws density
  | (not.isValidDensity) density = error "Invalid density, it must be between 0 and 1 (excluding them)"
  | (not.checkWordsValidity) ws = error "Invalid list of words, a word cannot be empty"
  | otherwise = do let words = upperCase ws
                   let gridN= gridlength words density
                   let gridEmpty = genEmptyGrid gridN
                   let charSet = availableChar words 
                   gridWithWords <- insertWords words gridEmpty
                   finalGrid <- fillGrid charSet gridWithWords
                   if findUniqueWords finalGrid words
                      then
                      return finalGrid
                      else
                      createWordSearch words density

-- Validity Checks
------------------------------------------------------------
isValidDensity :: Double -> Bool
isValidDensity density = density < 1 && density > 0
------------------------------------------------------------
  
-- Random Number generation 
------------------------------------------------------------
-- Function that generates 
-- Random value based on lower and upper bounds
randVal :: Int -> Int -> IO Int
randVal low up =  randomRIO (low, up)
------------------------------------------------------------

-- Orientation generation
------------------------------------------------------------
-- Function uses to map a randomly generated integer to 
-- An Orientation for word orientation's randomness
numToOrient :: Int -> Orientation
numToOrient index = [Forward, DownForward,
                     Down,    DownBack,
                     Back,    UpBack,
                     Up,      UpForward]!!index

-- Function takes a word and generates a random direction
-- For it using the helper function above
genWordOrientation :: String -> IO (String, Orientation)
genWordOrientation word = do num <- randVal 0 7
                             let orientation = numToOrient num
                             return (word, orientation)
------------------------------------------------------------

-- Empty Grid Generation
------------------------------------------------------------
-- Function that generates the length for a square grid
-- Based on the density value
-- TODO: Make it better
gridlength :: [String] -> Double -> Int
gridlength words density = ceiling((characterNo words / density)/3) 
  where
    characterNo = fromIntegral.length.concat

-- Create a grid filled with 0's
-- Used later on to first fill it up with hidden words
genEmptyGrid :: Int -> WordSearchGrid
genEmptyGrid l = replicate l (concat (replicate l "0"))
------------------------------------------------------------

-- Position Generation
------------------------------------------------------------
-- Function that generates the range for the column and row
-- Based on the orientation given and word & grid length
-- Used to improve efficiency and to make the first letter 
-- Position generation less likely to fail
posnRestrictions :: Orientation -> Int -> Int -> ((Int,Int), (Int, Int))
posnRestrictions Forward wordl gridl        = ( (0,        gridl- wordl), (0,         gridl-1)      )
posnRestrictions Back wordl gridl           = ( (wordl-1,  gridl-1 ),     (0,         gridl-1)      )
posnRestrictions Down wordl gridl           = ( (0,        gridl-1),      (0,         gridl- wordl) )
posnRestrictions Up wordl gridl             = ( (0,        gridl-1),      (wordl - 1, gridl - 1)    )
posnRestrictions DownForward wordl gridl    = ( (0,        gridl-wordl),  (0,         gridl-wordl)  )
posnRestrictions UpForward wordl gridl      = ( (0,        gridl-wordl),  (wordl - 1, gridl - 1)    )
posnRestrictions DownBack wordl gridl       = ( (wordl-1,  gridl-1),      (0,         gridl-wordl)  )
posnRestrictions UpBack wordl gridl         = ( (wordl-1,  gridl-1),      (wordl - 1, gridl - 1)    )

-- Function that uses the ranges generated by helper function
-- posnRestrictions and generates appropriate random row and column
genPosn :: Orientation -> Int -> Int ->  IO Posn
genPosn l wordl gridl = do let ((lowCol, upCol),(lowRow, upRow)) = posnRestrictions l wordl gridl
                           col <- randVal lowCol upCol
                           row <- randVal lowRow upRow
                           return (col,row)
------------------------------------------------------------
  
--Word Insertion
------------------------------------------------------------
-- Function replaces a letter in a given position in a grid
-- While keeping the rest the same
replaceLetter :: Posn -> Char -> [String] -> [String]
replaceLetter (0, 0) l ((_:cs):css) = (l:cs):css
replaceLetter (col, 0) l ((c:cs):css)= (c: head (replaceLetter (col-1, 0) l (cs:css))) : css
replaceLetter (col, row) l (cs:css) = cs : replaceLetter (col, row - 1) l css

-- Function attempts to insert a word into a particular area
-- Bases the insertion on orientation and position
-- TODO: Add a goforward go backward function that takes a position and changes it
updateInsert :: Orientation -> Posn -> String -> [String] -> Maybe [String]
updateInsert _ _ [] css = Just css
updateInsert Forward p@(col, row) (l:ls) css 
  |canReplaceLetter css l p = updateInsert Forward (col+1, row) ls (replaceLetter p l css)
  |otherwise = Nothing
updateInsert Back p@(col, row) (l:ls) css 
  |canReplaceLetter css l p = updateInsert Back (col-1, row) ls (replaceLetter p l css)  
  |otherwise = Nothing
updateInsert Up p@(col, row) (l:ls) css 
  |canReplaceLetter css l p = updateInsert Up (col, row - 1) ls (replaceLetter p l css)  
  |otherwise = Nothing
updateInsert Down p@(col, row) (l:ls) css 
  |canReplaceLetter css l p = updateInsert Down (col, row + 1) ls (replaceLetter p l css)
  |otherwise = Nothing
updateInsert DownForward p@(col, row) (l:ls) css 
  |canReplaceLetter css l p = updateInsert DownForward (col+1, row + 1) ls (replaceLetter p l css)  
  |otherwise = Nothing
updateInsert DownBack p@(col, row) (l:ls) css 
  |canReplaceLetter css l p = updateInsert DownBack (col-1, row + 1) ls (replaceLetter p l css)   
  |otherwise = Nothing
updateInsert UpBack p@(col, row) (l:ls) css 
  |canReplaceLetter css l p = updateInsert UpBack (col-1, row - 1) ls (replaceLetter p l css)   
  |otherwise = Nothing
updateInsert UpForward p@(col, row) (l:ls) css 
  |canReplaceLetter css l p = updateInsert UpForward (col+1, row - 1) ls (replaceLetter p l css)  
  |otherwise = Nothing

-- Function that acts as a guard for the updateInsert
-- Checks if the cell is free or has the same letter as the word
canReplaceLetter :: [String] -> Char -> Posn -> Bool 
canReplaceLetter css l p = findLetter css p == '0' || findLetter css p == l

-- Change to foldM
-- Function that takes an array of words and inserts them
-- In a given grid
insertWords :: [String] -> [String] -> IO [String]
insertWords [] css = return css
insertWords (w:ws) css = do first <- generateInsertWord w css
                            insertWords ws first

-- Function that takes a string and generates a successfull position
-- And orientation for it that results in it being inserted in a given grid
generateInsertWord :: String -> [String] -> IO [String]
generateInsertWord w css = do (_, orientation) <- genWordOrientation w
                              position <- genPosn orientation (length w) (length css)
                              let insertInGrid = updateInsert orientation position w css
                              if isJust insertInGrid
                                 then 
                                   do let Just grid = insertInGrid
                                      return grid 
                                 else 
                                   generateInsertWord w css
------------------------------------------------------------
  
-- Finish Off the Grid
------------------------------------------------------------
-- Function that fills up the grid
-- With already hidden words with other random characters
-- Based on the words given
fillGrid :: String -> [String] -> IO[String]
fillGrid _ [] = return []
fillGrid ls (cs:css) = do line <- fillLine ls cs
                          rest <- fillGrid ls css
                          return (line:rest)

-- Function that fills up one specific
-- Line with random characters, except for cells taken
-- By hidden walls
fillLine :: String -> String -> IO String
fillLine _ [] = return []
fillLine ls (c:cs) = do character <- randomChar ls
                        let r = tryReplace character c
                        rest <- fillLine ls cs
                        return (r:rest)

-- Function that generates a set of 
-- Available characters from given words
-- https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
-- Removes duplicates from a given array with O(nlogn) time
-- TODO:: Maybe change it to the other stable solution (check link, comments under the solution i used)
availableChar :: [String] -> String
availableChar = (head.group.sort).concat
            
-- Function that returns either the original character
-- Or the new one, depending on whether the cell is free or not
tryReplace :: Char -> Char -> Char
tryReplace l c  | c == '0' = l
  | otherwise = c

-- Function that generates a random character
-- Based on the random number given
randomChar :: String -> IO Char
randomChar ls = do index <- randVal 0 (length ls - 1)
                   return (ls!!index)
------------------------------------------------------------

-- Verify Uniqueness of the Grid
------------------------------------------------------------
startLetters :: WordSearchGrid -> [String] -> Maybe [[Posn]]
startLetters css [] = Just []
startLetters css (w:ws) | isJust positionList = let (Just posn) = positionList
                                                      in fmap (posn:) (startLetters css ws)
                        | otherwise = Nothing
    where
      positionList = startLetter css 0 0 w

uniqueWord :: WordSearchGrid -> Bool -> (String, [Posn]) -> Bool
uniqueWord css f (w,[]) = f
uniqueWord css f (w ,p:ps)
  | existWord && not f = uniqueWord css True (w, ps) 
  | existWord && f = False
  | otherwise = uniqueWord css f (w, ps)
  where
    orientations = [Forward, Back, Down , Up, UpForward, DownForward, DownBack, UpBack]
    existWord = uniqueOrientation css False w p orientations
-- found = map (uncurry.uncurry $ findWord css) words

uniqueOrientation :: WordSearchGrid -> Bool -> String -> Posn -> [Orientation] -> Bool
uniqueOrientation css f w p [] = f 
uniqueOrientation css f w p (o:os)
  | existWord && not f = uniqueOrientation css True w p os 
  | existWord && f = False
  | otherwise = uniqueOrientation css f w p os
    where
      existWord = findWord css o w p

findUniqueWords :: WordSearchGrid -> [String] -> Bool
findUniqueWords css ws  = and $ map (uniqueWord css False) tuples
 where
   Just positions = startLetters css ws
   tuples :: [(String, [Posn])]
   tuples = zip ws positions
  
                                                         
------------------------------------------------------------
  
-- Debug is a function that only puts in the hidden words
-- in the grid without any random characters
-- (the rest of the cells just have '0' in them) 
debug :: [String] -> Double -> IO WordSearchGrid
debug ws density = insertWords ws gridEmpty
  where
    gridN= gridlength ws density
    gridEmpty = genEmptyGrid gridN

 
--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- debug words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

-- Changed the functions bellow to improve the prettyPrint of the grid
printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do printLine w
                      printGrid ws

-- Function that pretty-prints one line of a grid printLine :: String -> IO () printLine [] = putChar '\n' 
printLine (l:ls) = do putChar l
                      putChar ' '
                      printLine ls

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
