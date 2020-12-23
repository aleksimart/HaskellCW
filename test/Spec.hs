import Lib
import Test.HUnit
import Control.Exception
import Control.Monad
import Data.Char


assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)
 
assertError ex f = 
    TestCase $ assertException (ErrorCall ex) $ evaluate f


main :: IO ()
main = do 
    -- For some reason in prints first line on the same line as the previous one
    putStrLn ""
    putStrLn "Testing Trivial Errors"
    putStrLn "----------------------------------------------------"
    runTestTT testErrors
    putStrLn "----------------------------------------------------"
    putStrLn ""
    putStrLn "Testing the Given Examples"
    putStrLn "----------------------------------------------------"
    runTestTT testExamples
    putStrLn "----------------------------------------------------"
    putStrLn ""
    putStrLn "Testing Lower case same Examples"
    putStrLn "----------------------------------------------------"
    runTestTT testLowerCases
    putStrLn "----------------------------------------------------"
    
-- TODO More square grid tests
-- Testing the Errors
testGrid1 = assertError "Empty Grid Mustn't be allowed" (solveWordSearch ["TEST"] [])
testGrid2 = assertError "Non-square Grdi Mustn't be allowed" (solveWordSearch ["TEST"] ["AAA","AA"])
testGrid3 = assertError "Empty String Mustn't be allowed" (solveWordSearch ["TEST", "UNREAL", "POSSIBLE", ""] ["UUUUUU","UUUUUU","UUUUUU","UUUUUU","UUUUUU","UUUUUU"])
testGrid4 = assertError "Empty String Mustn't be allowed" (solveWordSearch ["", "UNREAL", "POSSIBLE", "IMPOSSIBLE"] ["UUUUUU","UUUUUU","UUUUUU","UUUUUU","UUUUUU","UUUUUU"])
testGrid5 = assertError "Empty String Mustn't be allowed" (solveWordSearch ["OKAY", "", "POSSIBLE", "IMPOSSIBLE"] ["UUUUUU","UUUUUU","UUUUUU","UUUUUU","UUUUUU","UUUUUU"])
testErrors = TestList [TestLabel "Checking the Empty grid exception" testGrid1, 
                       TestLabel "Checking non-square Grid exception" testGrid2,
                       TestLabel "Checking for Empty string exception 1" testGrid3,
                       TestLabel "Checking for Empty strings exception 2" testGrid4,
                       TestLabel "Checking for Empty strings exception 3" testGrid5]

-- Testing Predefined Examples
testGivenExample1 = TestCase (assertEqual "Error in the First Example" (solveWordSearch exWords1'1 exGrid1'1 ) exampleAnswer1)
exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]
exampleAnswer1 = [("HASKELL",Just((0,0),DownForward)),
               ("STRING",Just((7,0),Back)),
               ("STACK",Just((2,2),Forward)),
               ("MAIN",Just((2,7),Up)),
               ("METHOD",Just((4,3),Down))]

testGivenExample2 = TestCase (assertEqual "Error in the Second Example" (solveWordSearch exWords1'2 exGrid1'2 ) exampleAnswer2)
exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
exampleAnswer2 =  [("BANANA",Just ((5,6),UpBack)),
                ("ORANGE",Just ((1,0),DownForward)),
                ("MELON",Just ((7,8),Up)),
                ("RASPBERRY",Just ((8,0),DownBack)),
                ("APPLE",Just ((2,8),UpForward)),
                ("PLUM",Just ((5,1),DownBack)),
                ("GRAPE",Just ((8,6),Up))]

testExamples = TestList [TestLabel "First Example:" testGivenExample1,
                         TestLabel "Second Example:" testGivenExample2]


-- Testing LowerCase
testLowerCase1 = TestCase (assertEqual "Lower Case game example 1 don't result in the same answer" 
                            (upperCase (solveWordSearch lowerCaseWordsExample1 lowerCaseGridExample1) ) lowerCaseAnswer1)
lowerCaseGridExample1 = [ "hagnirtsh" , "sacagetak", "gcstackel","mghkmilki","eknletgcn","tnirtlete","iraahclsr","mamrosagd","gizkddnrg" ] 
lowerCaseWordsExample1 = [ "haskell","string","stack","main","method"]
lowerCaseAnswer1 = [("HASKELL",Just((0,0),DownForward)),
               ("STRING",Just((7,0),Back)),
               ("STACK",Just((2,2),Forward)),
               ("MAIN",Just((2,7),Up)),
               ("METHOD",Just((4,3),Down))]

testLowerCase2 = TestCase (assertEqual "Mixed Lower Case for example 2 doesn't result in the same answer" (solveWordSearch exWords1'2 exGrid1'2 ) exampleAnswer2)
lowerCaseGridExample2 = ["ROBREUmbR","AURpepsaN","UNLALMSEE","ygaunpyyP","NLmnbgeNA","NBlealeoR","ALrypBBLG","nrepBEBEP","YgayARoMr"]
lowerCaseWordsExample2 = [ "Banana", "ORanGE", "MELON", "RASPBerRY","APplE","PluM","GRapE" ]
lowerCaseAnswer2 =  [("BANANA",Just ((5,6),UpBack)),
                ("ORANGE",Just ((1,0),DownForward)),
                ("MELON",Just ((7,8),Up)),
                ("RASPBERRY",Just ((8,0),DownBack)),
                ("APPLE",Just ((2,8),UpForward)),
                ("PLUM",Just ((5,1),DownBack)),
                ("GRAPE",Just ((8,6),Up))]

testLowerCases = TestList [TestLabel "Everything is lower case " testLowerCase1,
                           TestLabel "Everything is mixed case " testLowerCase2]





-- Helper Function
upperCase :: [(String, Maybe Placement)] -> [(String, Maybe Placement)]
upperCase xs = zip (map (map toUpper.fst) xs) (map snd xs)

-- exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
-- exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
-- testAnswer2 =  [("BANANA",Just ((5,6),UpBack)),("ORANGE",Just ((1,0),DownForward)),
--                 ("MELON",Just ((7,8),Up)),
--                 ("RASPBERRY",Just ((8,0),DownBack)),
--                 ("APPLE",Just ((2,8),UpForward)),
--                 ("PLUM",Just ((5,1),DownBack)),
--                 ("GRAPE",Just ((8,5),Up))]

-- testExamples = TestList [TestLabel "First Example:" testGivenExample1,
--                          TestLabel "Second Example:" testGivenExample2]

-- Test the grid size is NxN and isn't empty
-- Test the the word isn't empty
