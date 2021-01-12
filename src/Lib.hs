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

-- END OF CODE YOU MUST NOT MODIFY

-- ADD YOUR OWN CODE HERE

-- AUTHOR: Aleksei Martirosov
-- Solutions to challenges from 1 to 6

-- Challenge 1 --

-- Should work with no words as well
solveWordSearch :: [ String ] -> WordSearchGrid -> [ (String,Maybe Placement) ]
solveWordSearch ws css 
    |(not.checkGridValidity) grid  = error "Invalid Grid, it must be square"
    | null grid = solveEmptyGrid ws
    |(not.checkWordsValidity) words  = error "Invalid list of words, a word cannot be an empty string or contain a null character"
    | otherwise                   = map (placementSearch grid) tuples  
  where
    words = upperCase ws
    grid = upperCase css
    starts = map (startLetter grid 0 0) words
    tuples = zip words starts

solveEmptyGrid :: [String] -> [(String, Maybe Placement)]
solveEmptyGrid words = [(w,Nothing) | w <- words]

checkGridValidity :: WordSearchGrid -> Bool
checkGridValidity [] = True
checkGridValidity g@(cs:_) = length g == length cs

checkWordsValidity :: [String] -> Bool
checkWordsValidity [] = False
checkWordsValidity ([] : _) = False
checkWordsValidity [w] = '\0' `notElem` w
checkWordsValidity (w:ws) = '\0' `notElem` w && checkWordsValidity ws

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
-- TODO clean up this mess and optimise by breaking down the check into a separate function (can keep this one and the rest that can be recursive should be called later)
createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch ws density
  | (not.isValidDensity) density = error "Invalid density, it must be between 0 and 1 (excluding them)"
  | (not.checkWordsValidity) ws = error "Invalid list of words, a word cannot be empty or contain a null character"
  | otherwise = do let words = upperCase ws
                   let gridN = max (maxWordLength words 0) (gridlength words density)
                   let gridEmpty = genEmptyGrid gridN
                   let charSet = availableChar words 
                   gridWithWords <- insertWords words gridEmpty
                   if null gridWithWords
                      then
                        createWordSearch words density
                      else 
                        do finalGrid <- fillGrid charSet gridWithWords
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
gridlength words density = ceiling(sqrt (characterNo words / density) + 1) 
  where
    characterNo = fromIntegral . length . concat

maxWordLength :: [String] -> Int -> Int
maxWordLength [] max = max
maxWordLength (word:words) max 
  | max > worLen = maxWordLength words max
  | otherwise    = maxWordLength words worLen 
  where
    worLen = length word

-- Create a grid filled with Null characters
-- This program has to assume that null charcter will not be used
-- Used later on to first fill it up with hidden words
genEmptyGrid :: Int -> WordSearchGrid
genEmptyGrid l = replicate l (concat (replicate l "\0"))
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
canReplaceLetter css l p = findLetter css p == '\0' || findLetter css p == l

-- Change to foldM
-- Function that takes an array of words and inserts them
-- In a given grid
insertWords :: [String] -> [String] -> IO [String]
insertWords [] css = return css
insertWords (w:ws) css = do first <- generateInsertWord w css 0
                            if null first
                               then 
                                  return []
                                else
                                  insertWords ws first

-- Function that takes a string and generates a successfull position
-- And orientation for it that results in it being inserted in a given grid
generateInsertWord :: String -> [String] -> Int -> IO [String]
generateInsertWord _ _ 15 = return []
generateInsertWord w css counter = do (_, orientation) <- genWordOrientation w
                                      position <- genPosn orientation (length w) (length css)
                                      let insertInGrid = updateInsert orientation position w css
                                      if isJust insertInGrid
                                        then 
                                          do let Just grid = insertInGrid
                                             return grid 
                                        else 
                                          generateInsertWord w css (counter + 1)
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
availableChar ws = map head . group . sort $ concat ws 
            
-- Function that returns either the original character
-- Or the new one, depending on whether the cell is free or not
tryReplace :: Char -> Char -> Char
tryReplace l c  | c == '\0' = l
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
startLetters _ [] = Just []
startLetters css (w:ws) | isJust positionList = let (Just posn) = positionList
                                                      in fmap (posn:) (startLetters css ws)
                        | otherwise = Nothing
    where
      positionList = startLetter css 0 0 w

uniqueWord :: WordSearchGrid -> Bool -> (String, [Posn]) -> Bool
uniqueWord _ f (w,[]) = f
uniqueWord css f (w ,p:ps)
  | existWord && not f = uniqueWord css True (w, ps) 
  | existWord && f = False
  | otherwise = uniqueWord css f (w, ps)
  where
    orientations = [Forward, Back, Down , Up, UpForward, DownForward, DownBack, UpBack]
    existWord = uniqueOrientation css False w p orientations
-- found = map (uncurry.uncurry $ findWord css) words

uniqueOrientation :: WordSearchGrid -> Bool -> String -> Posn -> [Orientation] -> Bool
uniqueOrientation _ f _ _ [] = f 
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
-- Debug is a function that only puts in the hidden words
-- in the grid without any random characters
-- (the rest of the cells just have '0' in them) 
-- debug :: [String] -> Double -> IO WordSearchGrid
-- debug ws density = insertWords ws gridEmpty
--   where
--     gridN= gridlength ws density
--     gridEmpty = genEmptyGrid gridN

 
-- --- Convenience functions supplied for testing purposes
-- createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
-- createAndSolve words maxDensity =   do g <- debug words maxDensity
--                                        let soln = solveWordSearch words g
--                                        printGrid g
--                                         return soln

-- -- createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
-- -- createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
-- --                                        let soln = solveWordSearch words g
-- --                                        printGrid g
-- --                                        return soln
                                       
-- -- Changed the functions bellow to improve the prettyPrint of the grid
-- printGrid :: WordSearchGrid -> IO ()
-- printGrid [] = return ()
-- printGrid (w:ws) = do printLine w
--                       printGrid ws

-- -- Function that pretty-prints one line of a grid printLine :: String -> IO () printLine [] = putChar '\n' 
-- printLine (l:ls) = do putChar l
--                       putChar ' '
--                       printLine ls

-- Challenge 3 --

-- Function to pretty print the given LamMacroExpr
-- First it simplifies the macros
-- Happens when an inner macro has a sub-expression which is an outer macro
-- Then subsitutes the macros in the expression multiple times
-- Until there is no change in expression.
-- Done more than once since a bigger macro might not be recognised at first
-- After it has been simplified.
-- Finally, it formats the LamMacroExp as specified
prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef [] e) = formatLamExpr e False
prettyPrint (LamDef m  e) 
  | illegalMacros $ reverse m       = error "A macro contains Illegal Macros. It cannot contain itself, any undefined ones or the ones which were not defined before itself"
  | otherwise = formatMacros macros ++ formatLamExpr final False
  where
    macros     = simplifyMacros m 
    expression = iterate (exploreExpr macros) e
    -- Lines taken from Lecture "Implementing Evaluation"
    list       = zip expression (tail expression)
    final      = (fst . head . dropWhile(uncurry(/=))) list

-- TODO : Good convention is to first do something in the wrong case (when talking about guards) and then otherwise the "good one" (The one where we succeed)
-- Ask dom about it, it might not work with haskell
-- Error Check
------------------------------------------------------------
illegalMacros :: [(String, LamExpr)] -> Bool
illegalMacros []                         = False
illegalMacros ((_, e) : ms) 
  | null illegal                   = illegalMacros ms
  | otherwise                            = True
   where
     allowedMacros = [n | (n, _) <- ms]
     containedMacros = containsMacros e []  
     illegal = filter (`notElem` allowedMacros) containedMacros

containsMacros :: LamExpr -> [String] -> [String]
containsMacros (LamVar _)     ms = ms 
containsMacros (LamMacro m)   ms = m:ms
containsMacros (LamAbs _ e)   ms = containsMacros e ms
containsMacros (LamApp e1 e2) ms = ms ++ containsMacros e1 [] ++ containsMacros e2 []
------------------------------------------------------------
  
-- Formatting Macros and the Expression to a specified format
------------------------------------------------------------
-- Function that creates the string out of the given list of macros definitions
-- First part of the LanMacroExpr type
formatMacros :: [(String, LamExpr)] -> String
formatMacros []          = ""
formatMacros ((l, e):ms) = "def " ++ l ++ " = " ++ formatLamExpr e False ++ " in " ++ formatMacros ms 

-- Function that creates a pretty string out of the given expression
-- Second part of the LanMacroExpr type
formatLamExpr :: LamExpr -> Bool -> String
formatLamExpr (LamMacro v)   innerBool = v 
formatLamExpr (LamAbs num e) innerBool
  | checkVar num && innerBool = "(\\" ++ formatLamExpr (LamVar num) False ++ " -> " ++ formatLamExpr e False ++ ")"
  | checkVar num = "\\" ++ formatLamExpr (LamVar num) innerBool ++ " -> " ++ formatLamExpr e innerBool
formatLamExpr (LamApp e1 e2) innerBool 
  |isAbstraction e1 && isApplication e2 = formatLamExpr e1 True  ++ " (" ++ formatLamExpr e2 innerBool ++ ")"
  |isAbstraction e1          = formatLamExpr e1 True ++ " " ++ formatLamExpr e2 innerBool
  |isApplication e2          = formatLamExpr e1  innerBool ++ " (" ++ formatLamExpr e2 innerBool ++ ")"
  |otherwise                 = formatLamExpr e1 innerBool ++ " " ++ formatLamExpr e2 innerBool
formatLamExpr (LamVar num) innerBool  | checkVar num = "x" ++ show num

-- formatInnerLam :: LamExpr -> String 
-- formatInnerLam (LamMacro v) = v
-- formatInnerLam (LamAbs num e) | checkVar num = "(" ++ "\\" ++ formatLamExpr (LamVar num) ++ " -> " ++ formatLamExpr e ++ ")"
-- formatInnerLam (LamApp e1 e2) 
--   |isAbstraction e1 && isApplication e2 = "(" ++ formatInnerLam e1 ++ ") " ++ " (" ++ formatInnerLam e2 ++ ")"
--   |isAbstraction e1          = formatInnerLam e1 ++ formatInnerLam e2
--   |isApplication e2          = formatInnerLam e1 ++ " (" ++ formatInnerLam e2 ++ ")"
--   |otherwise                 = formatInnerLam e1 ++ " " ++ formatInnerLam e2
-- formatInnerLam (LamVar num)   | checkVar num = "x" ++ show num

checkVar :: Int -> Bool
checkVar val | val >= 0 = True
  | otherwise = error "Error: encountered negative value for variable value"
------------------------------------------------------------

-- Simplifying Macros and the Expression using other Macros
------------------------------------------------------------
-- Function that searches the expressions of the macros for any sub-expressions
-- That are defined in the outer macros
-- Note that this function requires an inverse input of macros (from innermost to outermos)
-- This is for the ease of recursion (inner one can be expressed as any of the outer ones) 
exploreMacros :: [(String, LamExpr)] -> [(String, LamExpr)]
exploreMacros [] = []
exploreMacros (m@(name, e):ms) 
  | e /=new   = (name, new) : exploreMacros ms 
  | otherwise = m : exploreMacros ms
  where
    new = exploreExpr ms e

-- Function that searches the expression for any sub-expressions
-- That are defined as given macros
exploreExpr :: [(String, LamExpr)] -> LamExpr -> LamExpr
exploreExpr ms e | isJust m   = LamMacro name 
  where
    m = check ms e
    Just name = fmap fst m
exploreExpr _ (LamMacro v)    = LamMacro v
exploreExpr _ (LamVar num)    = LamVar num
exploreExpr ms (LamAbs num e) = LamAbs num (exploreExpr ms e) 
exploreExpr ms (LamApp e1 e2) = LamApp (exploreExpr ms e1) (exploreExpr ms e2)

-- Function that checks particular expression against available 
-- Macros and returns a macro it matches to or Nothing 
-- If there isn't such
check :: [(String, LamExpr)] -> LamExpr -> Maybe (String, LamExpr)
check [] _ = Nothing
check (m@(_, exp):ms) e 
  | exp == e  = Just m
  | otherwise = check ms e
------------------------------------------------------------

-- Additional Helper Functions
------------------------------------------------------------
-- Function that checks if an expression is an Abstraction
-- Used for bracketing purposes
isAbstraction :: LamExpr -> Bool
isAbstraction (LamAbs _ _) = True
isAbstraction (LamApp _ e) = isAbstraction e
isAbstraction _            = False

-- Function that checks if an expression is an Application
-- Used for bracketing purposes
isApplication :: LamExpr -> Bool
isApplication (LamApp _ _) = True
isApplication _            = False

-- Function that reverses the macros table, feeds it to exploreMacros
-- And reverses it back to how it was
-- Check exploreMacros for the reason behind reversing it
simplifyMacros :: [(String, LamExpr)] -> [(String, LamExpr)]
simplifyMacros = reverse.exploreMacros.reverse
------------------------------------------------------------

-- TODO: INTERESTING THING TO POINT OUT
-- If the guards for one part fail, it will continue pattern matching for the others

-- examples in the instructions
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) 
ex3'5 = LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamApp(LamAbs 1 (LamVar 1)) (LamVar 2))] (LamAbs 1 (LamVar 1))
ex3'6 = LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamAbs 2 (LamAbs 1 (LamVar 1)))] (LamApp(LamAbs 2 (LamAbs 1 (LamVar 1))) (LamVar 2))
ex3'7 = LamDef [] (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))) (LamAbs 4 (LamApp (LamAbs 7 (LamApp (LamVar 7)  (LamVar 2))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3)))))))
ex3'8 = LamDef [("F", LamAbs 2 (LamAbs 1 (LamVar 1))),("G", LamAbs 1 (LamVar 1))] (LamApp (LamVar 3) (LamApp (LamVar 1) (LamVar 2)))
ex3'9 = LamDef []  (LamApp(LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 2))
ex3'10 = LamDef [] (LamAbs 2(LamAbs 1(LamAbs 0 (LamVar 0))))

-- Challenge 4 --

-- Function Parses the given String and returns LamMacroExpr, if the syntax is correct
-- First it checks if the macros are defined correctly (all the rules are followed) by parsing the definitions
-- Then it parses the expression.
-- Will return nothing if:
--    1. Invalid macros definitions (check functions below for the rules)
--    2. Invalid expression definition 
parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro exp
  | not $ macrosCheck exp =  Nothing
  | null expression = Nothing
  | not $ null unparsed  = Nothing
  | otherwise = Just parsed
  where
    expression = parse macrosExpr exp
    unparsed = snd $ head expression
    parsed = fst $ head expression 

-- Checks for Macros
------------------------------------------------------------
-- Function that checks the parse macros list against the same macros list
-- filtered according to rules defined in filterMacros function
macrosCheck :: String -> Bool
macrosCheck exp = macros == filterMacros macros
  where macrosParsed = parse (many macrosMap) exp
        macros = fst $ head macrosParsed

-- Parser that parses all the macros definitions with extra restrictions
-- 1. If there are free variables in any of the macros, it will return []
-- 2. If the same macro is defined twice, it will also return []
filterMacros :: [(String, LamExpr)] -> [(String, LamExpr)]
filterMacros ms 
  | length names /= length unique = []
  | free = []
  | otherwise = ms
  where 
    names = map fst ms 
    unique = nub names
    free = any (checkFreeVariables.snd) ms
-- free = or $ map (checkFreeVariables.snd) ms 

-- Check if all the variables in the given expression are closed
checkFreeVariables :: LamExpr -> Bool                         
checkFreeVariables e = let vars = getVariables e in
                           any (free e) vars
-- or $ map (free e)  vars

-- Gets all the variables out of an expression
getVariables :: LamExpr -> [Int]
getVariables (LamVar y) = [y]
getVariables (LamMacro _) = []
getVariables (LamAbs y e) = [y] ++ getVariables e
getVariables (LamApp e1 e2) = getVariables e1 ++ getVariables e2 

-- From Prog3 Lectures
-- Checks if a given variable is free in a given expression
free ::  LamExpr -> Int -> Bool
free (LamVar y) x= x == y
-- Cheeky
free (LamMacro _) _ = False
free (LamAbs y e) x
  | x == y = False
  | x /= y = free e x
free (LamApp e1 e2) x = free e1 x || free e2 x
------------------------------------------------------------

-- Parsers for Macros
------------------------------------------------------------
  
-- Parser that first parses all the macros definitions and then the actual expression
macrosExpr :: Parser LamMacroExpr 
macrosExpr = LamDef <$> many macrosMap <*> expr

-- Parser that parses all the macros definitions
macrosMap :: Parser (String, LamExpr)
macrosMap = do token (string "def")
               name <- some upper
               token (char '=')
               e <- expr
               token (string "in")
               return (name, e)
------------------------------------------------------------

-- Parsers for each part of the Grammar
--
-- The update in the given grammar is:
--    Expr ::= abs int Expr   | Term
--    Term ::= app term Fact  | Fact
--    Fact ::= '(' expr ')'   | Var  | MacroName
--
-- The rest is the same as defined in the challenge
-- This update ensures that the precendence and associativity rules are being followed
------------------------------------------------------------
  
-- Parser for Expr in given grammar
expr :: Parser LamExpr
expr = abst <|> term

-- Parser for Term in given grammar
-- Uses left-associativity parsing for app
term :: Parser LamExpr
term =  fact `chainl1` app

-- Parser for fact in given grammar
fact :: Parser LamExpr
fact = do token(char '(')
          e <- expr
          space
          char ')'
          return e 
      <|> abst <|> macro <|> var

-- Parser for app in given grammar
-- Only returns the function, because parsing it requires
-- Left associativity rules (using function chainl1)
app  :: Parser (LamExpr -> LamExpr -> LamExpr)
-- TODO: Added space
app  = do char ' '
          return LamApp

-- Parser for abs int Expr in given grammar  
abst :: Parser LamExpr
abst = do string "\\x"
          val <- some digit 
          token (string "->")
          LamAbs (read val) <$> expr

-- Parser for MacroName in given grammar
macro :: Parser LamExpr
macro = do space
           LamMacro <$> some upper 

-- Parser for Var in given grammar
var :: Parser LamExpr
var = do space
         char 'x'
         LamVar . read <$> some digit

-- TODO use that in code
digits :: Parser Int 
digits = read <$> some digit
-- digits :: Parser String
-- digits =    do  e <- digit 
--                 return [e]
--         <|> do  e <- digit 
--                 t <- digits
--                 return (e:t)

-- Function from the paper by Graham Hutton and Erik Meijer
-- Parses repeated applications of a parser p separated by applications
-- Of op whose result is the operator 'app' (for this particular scenario), defined above.
-- Specifically created for operations that associate to the left
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where
                   rest a = (do f <- op
                                b <- p
                                rest (f a b)) 
                            <|>return a
------------------------------------------------------------
  
-- Examples
ex4'1 = "x1 (x2 x3)"
ex4'2 = "x1 x2 F"
ex4'3 = "def F = \\x1-> x1 in \\x2 -> x2 F"
ex4'4 = "def F = \\x1 -> x1 (def G= \\x1 -> x1 in x1)in \\x2 -> x2"
ex4'5 = "def F = \\x1 -> x1 in def F = \\x2 -> x2 x1 in x1"
ex4'6 = "def F = x1 in F"

-- Challenge 5

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform me@(LamDef ms e) = LamDef macros (snd expr)
  where 
    max = getValue me
    macros = transitionMacros (fst expr, ms)
    expr = transitionExpr (inc max, e)

getValue :: LamMacroExpr -> Int
getValue (LamDef m e) | macro > expr = macro
  | otherwise = expr
  where
    macro = maxMacroVal 0 m
    expr = getMax $ getVariables e


maxMacroVal :: Int -> [(String, LamExpr)] -> Int
maxMacroVal v [] = v
maxMacroVal v ((_, expr): ms)  
  |v > local =  maxMacroVal v ms
  | otherwise = maxMacroVal local ms
  where
    local = getMax $ getVariables expr

getMax :: [Int] -> Int
getMax = foldr (\x y -> if x >= y then x else y) 0

transitionMacros :: (Int, [(String, LamExpr)]) -> [(String, LamExpr)]
transitionMacros (_, []) = []
transitionMacros (val, (n, e):ms)= (n, snd expression) : transitionMacros (fst expression, ms)
  where
    expression = transitionExpr (val, e)

-- HMMM Maybe also (Int, LamExpr)
transitionExpr :: (Int, LamExpr) -> (Int, LamExpr)
transitionExpr (val, v@(LamVar _)) = (val+1, LamAbs val (LamApp (LamVar val) v))
transitionExpr (val, m@(LamMacro _))= (val, m)
transitionExpr (val, LamAbs x e) = (available, csp)
  where
    expr = transitionExpr (val+1, e) 
    available = fst expr
    evaluated = snd expr
    csp = LamAbs val (LamApp (LamVar val) (LamAbs x evaluated)) 

transitionExpr (k, LamApp e1 e2) = (fst expr2 ,csp)
  where
    expr1 = transitionExpr (inc e, e1)
    expr2 = transitionExpr (fst expr1 , e2)
    f = inc k 
    e = inc f
    csp = LamAbs k (LamApp (snd expr1) (LamAbs f (LamApp (snd expr2) (LamAbs e (LamApp (LamApp (LamVar f) (LamVar e)) (LamVar k))))))
   
inc :: Int -> Int
inc a = a+1

-- Examples in the instructions
exId =  (LamAbs 1 (LamVar 1))
ex5'1 = (LamDef [] (LamApp (LamVar 1) (LamVar 2)))
ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))


-- Challenge 6

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 (LamDef _ (LamVar _)) = Nothing
innerRedn1 (LamDef ms (LamMacro x)) = Just (LamDef ms (macroLookup ms x))
-- innerRedn1 (LamDef ms  (LamAbs x e)) | isJust inner = 
--   where
--     inner = innerRedn1 e


outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter expr val = (reductions innerRedn expr val, reductions outerRedn expr val, reductions innerRedn (toCps (getValue expr) expr) val, reductions outerRedn (toCps (getValue expr) expr) val ) 

-- Beta-reduction functions
------------------------------------------------------------

reductions :: (Int -> LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> Int -> Maybe Int
reductions strat e 0   = case strat (inc $ getValue e) e of
                           Nothing -> Just 0
                           Just _  -> Nothing
reductions strat e max = case strat (inc $ getValue e) e of
                           Just reduced -> (1+) <$> reductions strat reduced (max - 1)
                           Nothing      -> Just 0


            

-- reductions strat e 0 
--   | isJust reduced = Nothing
--   | otherwise = Just 0
--   where
--     nextFree = inc $ getValue e
--     reduced = strat nextFree e
-- reductions strat e max 
--   | isJust reduced = fmap (1+) (reductions strat a (max - 1))
--   | otherwise = Just 0
--   where
--     nextFree = inc $ getValue e
--     reduced = strat nextFree e
--     Just a = reduced


-- alt :: Maybe (Int -> LamMacroExpr -> Maybe LamMacroExpr) -> Maybe a -> Maybe a
-- alt a b = \inp -> case a inp of
--                     Nothing -> b inp
--                     Just c -> Just c

-- reductions strat e = [p | p <- zip evals (tail evals)]
--   where
--     evals = iterate $ strat nextFree 
--     nextFree = inc $ getValue e



-- Todo, maybe return max
-- innerRedn :: Int -> LamMacroExpr -> Maybe LamMacroExpr
-- innerRedn max (LamDef macros expr) 
--   | expr == innerExpr && null macros       = Nothing 
--   | expr == innerExpr && (not.null) macros = Just (subMacros (reverse macros) expr)
--   | otherwise                              = Just (LamDef macros innerExpr)
--   where
--     innerExpr = innerExprRedn max expr

innerRedn :: Int -> LamMacroExpr -> Maybe LamMacroExpr
innerRedn max (LamDef macros expr) = do inner <- innerExprRedn max expr
                                        return (LamDef macros inner)
                                    <|> if null macros
                                           then
                                           Nothing
                                           else
                                           Just (LamDef (init macros) (subMacro (last macros) expr))

outerRedn :: Int -> LamMacroExpr -> Maybe LamMacroExpr
outerRedn max (LamDef macros expr) = if null macros
                                        then
                                        do inner <- outerExprRedn max expr
                                           return (LamDef macros inner)
                                        else
                                        Just (LamDef (tail macros) (subMacro (head macros) expr))


toCps ::Int -> LamMacroExpr -> LamMacroExpr
toCps nextFree expr = LamDef macros (LamApp transformed (LamAbs nextFree (LamVar nextFree)))
  where
    LamDef macros transformed = cpsTransform expr
-- | expr == innerExpr && null macros       = Nothing 
-- | expr == innerExpr && (not.null) macros = Just (subMacros (reverse macros) expr)
-- | otherwise                              = Just (LamDef macros innerExpr)
-- where
--   innerExpr = innerExprRedn max expr

-- Actually don't need that because even if the macro is empty, I still count it as reduction
subMacros :: [(String, LamExpr)] -> LamExpr -> LamMacroExpr
subMacros [] expr = LamDef [] expr
subMacros (macro:macros) expr 
  | expr == innerExpr = subMacros macros expr
  | otherwise         = LamDef (reverse macros) innerExpr
  where
    innerExpr = subMacro macro expr

subMacro :: (String, LamExpr) -> LamExpr -> LamExpr
subMacro macro expr@(LamVar _)           = expr
subMacro (name, mexpr) expr@(LamMacro x) 
  | name == x                            = mexpr
  | otherwise                            = expr
subMacro macro (LamAbs val expr)         = LamAbs val (subMacro macro expr)
subMacro macro (LamApp e1 e2)            = LamApp (subMacro macro e1) (subMacro macro e2)

-- innerRedn _ ms (LamMacro x) = Lam
-- innerRedn _ ms e@(LamVar x) = LamDef ms e
-- innerRedn _ ms e@(LamAbs _ _) =  LamDef ms e
-- innerRedn max ms (LamApp (LamAbs x e1) e@(LamAbs y e2)) = LamDef ms (snd $ subst (inc max) e1 x e)
-- innerRedn max ms (LamApp (LamAbs x e1) e@(LamVar y)) = LamDef ms (snd $ subst (inc max) e1 x e)
-- innerRedn max ms (LamApp e@(LamAbs x e1) e2) = LamDef macros (LamApp e expr)
--   where
--     LamDef macros expr = eval1cbv max ms e2
-- eval1cbv max ms (LamApp e1 e2) = LamDef macros (LamApp expr e2)
--   where
--     LamDef macros expr = eval1cbv max ms e1

-- innerExprRedn :: Int -> LamExpr -> LamExpr
-- innerExprRedn _ expr@(LamMacro _)      = expr
-- innerExprRedn _ expr@(LamVar _)        = expr
-- innerExprRedn max (LamAbs val expr)    = LamAbs val (innerExprRedn max expr)
-- innerExprRedn max (LamApp (LamAbs val expr1) expr2) 
--   | expr1 == leftInner                 = snd $ subst max expr1 val expr2
--   | otherwise                          = LamApp leftInner expr2
--   where
--     leftInner = innerExprRedn max expr1
-- innerExprRedn max (LamApp expr1 expr2) 
--   | expr1 == leftInner                 = LamApp expr1 (innerExprRedn max expr2) 
--   | otherwise                          = LamApp leftInner expr2
--   where
--     leftInner = innerExprRedn max expr1

innerExprRedn :: Int -> LamExpr -> Maybe LamExpr
innerExprRedn _ expr@(LamMacro _)      = Nothing
innerExprRedn _ expr@(LamVar _)        = Nothing
innerExprRedn max (LamAbs val expr)    = do inner <- innerExprRedn max expr
                                            return (LamAbs val inner)
innerExprRedn max (LamApp lam@(LamAbs val expr1) expr2) = do inner <- innerExprRedn max expr1
                                                             return (LamApp (LamAbs val inner) expr2)
                                                      <|> do inner <- innerExprRedn max expr2
                                                             return (LamApp lam inner)
                                                      <|>return (snd $ subst max expr1 val expr2)
-- innerExprRedn max (LamApp expr2@(LamVar _) (LamAbs val expr1)) = do inner <- innerExprRedn max expr1
--                                                                     return (LamApp expr2 (LamAbs val inner))
--                                                                  <|>return (snd $ subst max expr1 val expr2)
innerExprRedn max (LamApp expr1 expr2) = do inner <- innerExprRedn max expr1
                                            return (LamApp inner expr2)
                                         <|>do inner <- innerExprRedn max expr2
                                               return (LamApp expr1 inner)

outerExprRedn :: Int -> LamExpr -> Maybe LamExpr
outerExprRedn _ expr@(LamMacro _)      = Nothing
outerExprRedn _ expr@(LamVar _)        = Nothing
outerExprRedn max (LamAbs val expr)    = do inner <- outerExprRedn max expr
                                            return (LamAbs val inner)
outerExprRedn max (LamApp (LamAbs val expr1) expr2) = return (snd $ subst max expr1 val expr2)
outerExprRedn max (LamApp expr1 expr2) = do inner <- outerExprRedn max expr1
                                            return (LamApp inner expr2)
                                         <|>do inner <- outerExprRedn max expr2
                                               return (LamApp expr1 inner)

subst :: Int -> LamExpr -> Int -> LamExpr -> (Int, LamExpr)  
subst max (LamVar x) y expr 
  | x == y                                      = (max, expr)
  | x /= y                                      = (max, LamVar x)
subst max (LamMacro x) _ _                      = (max, LamMacro x)
subst max (LamAbs x e1) y e 
  | x /= y && not (free e x)                    = (max, LamAbs x inner1)
  | x /= y && free e x                          = subst (fst inner2Tuple) (LamAbs max (snd inner2Tuple)) y e
  | x == y                                      = (max, LamAbs x e1)
  where
    inner1 = snd $ subst max e1 y e
    inner2Tuple = subst (inc max) e1 x (LamVar max)
subst x' (LamApp e1 e2) y e                     = (fst expr2Tuple, LamApp (snd expr1Tuple) (snd expr2Tuple))
  where
    expr1Tuple = subst x' e1 y e
    expr2Tuple = subst (fst expr1Tuple) e2 y e

-- substMacro :: [(String, LamExpr)] -> LamExpr -> LamExpr
-- substMacro ms (LamMacro m) = macroLookup m

-- subst ms (LamMacro x) _ _  = macroLookup ms x
macroLookup :: [(String, LamExpr)] -> String -> LamExpr
macroLookup ms name = getExpr $ dropWhile (\(x,_) -> x /= name)  ms
  where
    getExpr = snd . head

------------------------------------------------------------
  
-- Examples in the instructions

--  (\x1 -> x1 x2)
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F  
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)   
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)  
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1 ) (\x1 -> x1 x1)) ID
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 

helper :: LamMacroExpr -> ([(String, LamExpr)], LamExpr)
helper (LamDef ms e) = (ms, e)
