import Lib
import Test.HUnit
import Control.Exception
import Control.Monad
import Data.Char
import Data.Maybe
import System.CPUTime
import Test.QuickCheck

-- https://stackoverflow.com/questions/13350164/how-do-i-test-for-an-error-in-haskell
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)
 
assertError ex f = 
    TestCase $ assertException (ErrorCall ex) $ evaluate f

assertIOError ex f = 
    TestCase $ assertException (ErrorCall ex) $ f

benchmark :: IO a -> IO Double
benchmark prog = do start <- getCPUTime
                    _ <- prog
                    end <- getCPUTime
                    let result = (fromIntegral (end - start) / 10^12) :: Double
                    return result

timeout :: Double -> Bool
timeout = (15.0 >)

assertTrue val = assertEqual "True" $ show val

main :: IO ()
main = do 
    -- For some reason in prints first line on the same line as the previous one
    start 1
    stage "erroneous input" (runTestTT testErrors)
    stage "given examples" (runTestTT testExamples)
    stage "special cases" (runTestTT testSpecialCases)
    stage "larger inputs" testLarge1

    start 2
    stage "erroneous input" (runTestTT testErr2)
    stage "simple cases" simpleTest2'1
    stage "lower case input" exCasing2'1 
    stage "extreme cases" extremeTest2'1

    start 3
    stage "simple cases" (runTestTT simpleTests3'1)
    stage "extreme cases" (runTestTT edgeTests3'1)

    start 4
    stage "simple grammar rules (only positive integers for variables and integers,\n capital letters for macros and only x for variables)" simpleParsingRules
    stage "application associativity" (runTestTT simpleAppsTest)
    stage "application/abstraction precedence" (runTestTT mixTest)
    stage "original examples given" (runTestTT casesTest)
 
    start 5
    stage "simple cps transformation rules" (runTestTT simpleTest5)
    stage "original examples given" (runTestTT casesTest5)


isUpperLetters :: String -> Bool
isUpperLetters [] = False
isUpperLetters (' ':rest) = isUpperLetters rest
isUpperLetters word = isUpperLetters1 word

isUpperLetters1 [] = False
isUpperLetters1 word = all isLetter word  && all isUpper word


start number = do putStrLn ""
                  putStrLn ("==================== Challenge "++ show number ++ " ====================")
                  putStrLn ""

stage :: String -> IO a -> IO()
stage name test = do putStrLn ("Testing " ++ name)
                     putStrLn "-----------------------------------------------------"
                     test
                     putStrLn "-----------------------------------------------------"
                     putStrLn ""

-- intPropTest :: Int -> Bool
-- intPropTest val =  parseLamMacro ("x" ++ show s) == (LamVar (s))
-- TODO More square grid tests

-- Challenge 1
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Testing for the error cases
------------------------------------------------------------
-- exEr1'1 = assertError "Empty Grid Mustn't be allowed"      wrongEx1
exEr1'2 = assertError "Non-square Grid Mustn't be allowed" wrongEx2 
exEr1'3 = assertError "Empty String Mustn't be allowed (Last position)"    wrongEx3 
exEr1'4 = assertError "Words containing non-alphabetic characters are not allowed" wrongEx4 
testErrors = TestList [TestLabel "Checking non-square Grid exception" exEr1'2,
                       TestLabel "Checking for Empty string exception" exEr1'3,
                       TestLabel "Checking words for invalid character" exEr1'4]

wrongEx1 = solveWordSearch ["TEST"] []
wrongEx2 = solveWordSearch ["TEST"] ["AAA","AA"]
wrongEx3 = solveWordSearch (removeWord (head emptyStringWords) emptyStringWords) emptyStringGrid
wrongEx4 = solveWordSearch ["JFOL", "ADFA\0f"] emptyStringGrid

emptyStringWords = ["TEST", "UNREAL", "POSSIBLE", "IMPOSSIBLE"] 
emptyStringGrid  = ["UUUUUU","UUUUUU","UUUUUU","UUUUUU","UUUUUU","UUUUUU"]

------------------------------------------------------------
-- Testing Predefined Examples
------------------------------------------------------------
testGivenExample1 = TestCase (assertEqual "Error in the First Example" (solveWordSearch exWords1'1 exGrid1'1 ) exAnsw1)
exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]
exAnsw1 = [("HASKELL",Just((0,0),DownForward)),
               ("STRING",Just((7,0),Back)),
               ("STACK",Just((2,2),Forward)),
               ("MAIN",Just((2,7),Up)),
               ("METHOD",Just((4,3),Down))]

testGivenExample2 = TestCase (assertEqual "Error in the Second Example" (solveWordSearch exWords1'2 exGrid1'2 ) exAnsw2)
exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]
exAnsw2 =  [("BANANA",Just ((5,6),UpBack)),
                ("ORANGE",Just ((1,0),DownForward)),
                ("MELON",Just ((7,8),Up)),
                ("RASPBERRY",Just ((8,0),DownBack)),
                ("APPLE",Just ((2,8),UpForward)),
                ("PLUM",Just ((5,1),DownBack)),
                ("GRAPE",Just ((8,6),Up))]

testExamples = TestList [TestLabel "First Example:" testGivenExample1,
                         TestLabel "Second Example:" testGivenExample2]
------------------------------------------------------------
    
-- Testing Special cases
------------------------------------------------------------
testLowerCase1 = TestCase (assertEqual "Lower case grid/words must still work" 
                            (upperCase (solveWordSearch lowerCaseWordsExample1 lowerCaseGridExample1) ) exAnsw1)
lowerCaseGridExample1 = [ "hagnirtsh" , "sacagetak", "gcstackel","mghkmilki","eknletgcn","tnirtlete","iraahclsr","mamrosagd","gizkddnrg" ] 
lowerCaseWordsExample1 = [ "haskell","string","stack","main","method"]

testLowerCase2 = TestCase (assertEqual "Mixed case grid/words must still work" (solveWordSearch exWords1'2 exGrid1'2) exAnsw2)
lowerCaseGridExample2 = ["ROBREUmbR","AURpepsaN","UNLALMSEE","ygaunpyyP","NLmnbgeNA","NBlealeoR","ALrypBBLG","nrepBEBEP","YgayARoMr"]
lowerCaseWordsExample2 = [ "Banana", "ORanGE", "MELON", "RASPBerRY","APplE","PluM","GRapE" ]

testEmptyGrid1 = TestCase (assertEqual "Empty Grid should just return nothing for all words" (solveWordSearch emptyGridWords []) emptyGridAnswer)
emptyGridWords = ["BEAUTIFUL", "INTERESTING", "CAPABLE", "REALISTIC"] 
emptyGridAnswer = [(x, Nothing) | x <- emptyGridWords]

testSpecialCases = TestList [TestLabel "Everything is lower case " testLowerCase1,
                            TestLabel "Everything is mixed case " testLowerCase2,
                            TestLabel "Empty Grid Still works but no words are found" testEmptyGrid1]
------------------------------------------------------------
    
    
-- Testing Larger input (printing time taken below it)
------------------------------------------------------------
exGrid1'3 = ["ZEZDWEHIZYXDEAWTYBGRHXBJALNSWIRHLYZDXXLTCOZEETROWB",
            "SXUYKUFMLWSDJBSSHLHYBDLUKMONMMSPTQZCSRGGMZWHCWZPLM",
            "TWBMHVQMCAKLTWXHRAZYLWCMBOYROUYQCKOQWWHPQXLIFARGQI",
            "FMANXUCMXJTLKHDFONNYWFWVPBLERZRTMFHKJITUAWVRHXQXCB",
            "MIWTQLYNSNIVOXUANPBLVDIACRDVIEFDBDWPSLMQGFGFAKEVAH",
            "MPCDWUTOMNBQXRNNIEWKFHKKXOYZSOAQEEIMPCEOWJOZFPEQWW",
            "XAGKKFWGAHJCWVRPIVRUPUUUQWFTGRZCOZOXOWRSBXFJUZNGWV",
            "AUHJVDBXXBCTFGQXFRDLVIVOGGBGFORCSBPNNWQVIMXAAUYMFQ",
            "LEXODWNRAUIXQJUKSJIKDFMWBQBBFIPRSEJHSTNCNXUJSLFGVZ",
            "BVCIULETUMHCLRGPUHMEYSAJWJYJDFQTOLKAADFGIPRMWNRPJQ",
            "SRBLDEQSJHVKNKAPBRYZVBBJMUDLIIJQFHATZRTAZKXZBKJJUV",
            "JXYILEXJVWSRUKADKYNQLLHMPDYUAYMKWMSERTJAREYUVMVVTR",
            "OVQABGVDUBSTFCUIOCVCRVTIHKDEWKZRAWERRALVORTXWOBHXB",
            "WDXXCKJXHJFMLVLCZEPIIEWZEIGEJPMZZCXIHNWLDGNLVDUFLU",
            "KVEGNFAWMIYIJAHIOBMBTNCHZJFMCCIETJHIJNUCEIKKMIHNFY",
            "UXRMCSJAOJOFCQFZSCYRHWTIKMEHWNGUBAOSHFSPNKFCKNDZAY",
            "HLXSAMQIWZZYLEXVAAFZRWDZUTIDGHAKBZUYIRVMLHSOQFUAXV",
            "WDLTUTVBSMJOZSVXCFCFHPHUEHGDURVVISPTLAIEIVXAUVRPIN",
            "YAOJWIRHOYTGJUXFKELPKSHZWVSVHVFWDTUUPGXETHTEHLNKFQ",
            "DCXSYWIGZBZDKXGXIQZKENNLDKPCANARTAHOFAADGLFEJPSFUW",
            "ECKECGTDPXOTRZNKJFDWJGZTYHCDVLOVEUSMLUFNTYCBUVPDZJ",
            "XVECZWSAXMYCJEWQLGVIRUYDEWGVHWZBNSHJVUBQQFVBOEXILN",
            "BFZQSNMFBUBNJGABSPWYXQEDNQUKIIOBIAUVIYCZTPJDGMVSUG",
            "UMYOLSQNBZCRBSSQANNTHJKYEGHDNXPBLABDCNPIAHSEAIWDZF",
            "JNPGMJPDTHSWDMOPTNZZVWLITKCTNHLIQGZOIJPGDAJMCDMCQO",
            "FAZOFFWOMVWKUUWWBAZHSSLOIYWHJEKWHRRRHLIJXIFPVRLIPV",
            "BVJQGLFRWVQTNCHRLRQPHUXFEWNZINNILPGSVIPFHTRHYZUTZP",
            "VEIUNHPLRUNREBSJPJEYEDOCXMGHXCYZBIJJYNKNZEOIFTPEVT",
            "KRSDLGGZODDRTESLHFSICKGJGLAWUYPEVGMKYPPTTRURDYWJST",
            "GFKGMSQRDCLLIGPEULPVIRAPAUCGPQYTGMIDZBPUNMOFVXWSNT",
            "ALVMEEMLSSPAQDYGGOAKCOUYYUEWHTKYAZWOFEPCIISKYBCUQR",
            "NBIMVEDVQNYZQJCSBKMDGTZNVYVUSMSVSKQLCMCJOTOEPPWTSW",
            "JNANEEXNGVPQOHINBMSOSKNRWWSLRBMQFUTPOFMZJTJPQZATHB",
            "IRIRKWSXPRIZXJOLHJHPDXRPWAVBYODQCSDCJSNTNEQXKNAQRJ",
            "PXGZCUUBYGLBHIHPBKEROXASDDSNBOELVZUYOLSKRNXEBYKWKP",
            "PDOVFQCLDZAWIEEILNNJGPDKSQBWACREZPCLUTCZSTUAIIXBOA",
            "DTMKOVPLMCXLCWDKGHHWPIDPWPKULRXMGRUGUZQYKEAZZNYQTF",
            "HUGHJPVTAZAOTQOIFALDXYBKBXZFYLUVITXNANHNZFSENHSSQA",
            "QVUFBSYNFKPQYFICTOODAXHTTCQEIWYNPOUPCQBVSHYYKFCWYI",
            "FOZVWGWMCCWCVKYFUVXZWAPKEKLWALDUNQULXJAMWSRZEHKNQX",
            "UXEWQJDAIFDQTHVWIDBHBAFTJNXJNDXZYHGPIPOKBDTPHHZYPT",
            "RDONXXYOGUWKKRKCLJFMGGTOENHFSMSFZCZKUVMBNSXTVSEFZH",
            "VLAEJJPNGKVEQKHHXAQEZXZIEQBSHMDORWPWZMKTTHCQLHXXWV",
            "EPLAXSGKUJUXGHJEUZQUUKHGHODSBBJGFUZHPDQLWGRINRITJD",
            "HGCPKUEPUPBYZQNJOQIRWGBCARIWEADAASONQFPKJWFSEKYZFY",
            "JMMECNWWWAKKWIXCNHNYGQYLTNOZEIAEATAPWFETLHOHIQWJFC",
            "OKZYNEYNPTBAZEOQYZAUBIJUDBHQAIXBHAOZEADWWEITCVGHXR",
            "LYMKXTSEPZKYXKHETKXWPSWPUELDKVIDWHNYITAYJZDMFAHJWS",
            "POFDLGXAYGLUTBOEWXNWZEJVUYSCTDASDINFCLQXDDIUXWHMBZ",
            "ROMASZUOZZZWOWLFUQAEACRHOXBAGNKTXJXIBMBPFFJJCGILFX"]

exWords1'3 =  ["ADVANCED",
    "AMAZING",
    "BEAUTIFUL",
    "COMPUTER",
    "HASKELL",
    "HATE",
    "ITERMITTENT",
    "JOINT",
    "LOVE",
    "OUT",
    "POINT",
    "POSSIBLE",
    "RIDICULOUS",
    "UNREAL",
    "WORDS"]
exAnsw1'3 = [("ADVANCED",Just ((33,19),UpBack)),("AMAZING",Just ((34,10),DownBack)),("BEAUTIFUL",Just ((31,21),UpForward)),("COMPUTER",Just ((35,33),UpForward)),("HASKELL",Just ((44,18),UpBack)),
              ("HATE",Just ((35,8),Down)),("ITERMITTENT",Just ((41,25),Down)),("JOINT",Just ((40,32),Up)),("LOVE",Just ((29,20),Forward)),("OUT",Just ((35,19),Up)),("POINT",Just ((43,32),UpBack)),
              ("POSSIBLE",Just ((36,18),DownBack)),("RIDICULOUS",Just ((42,26),UpBack)),("UNREAL",Just ((38,14),UpBack)),("WORDS",Just ((29,21),UpForward))]

exWords1'4 = ["UNKNOWN", "INCAPABLE", "OKAY", "THEORETICAL"]
exAnsw1'4  = [("UNKNOWN",Nothing),("INCAPABLE",Nothing),("OKAY",Nothing),("THEORETICAL",Nothing)]

testLarge1 = do time1 <- benchmark (evaluate $ solveWordSearch exWords1'3 exGrid1'3)
                time2 <- benchmark (evaluate $ solveWordSearch exWords1'4 exGrid1'3)
                let test1 = TestCase (do assertEqual "Error in test case 1'3" (solveWordSearch exWords1'3 exGrid1'3) exAnsw1'3
                                         assertBool  "Test Timed out (more than 15 seconds to do it)" (timeout time1))
                let test2 = TestCase (do assertEqual "Error in test case 1'4" (solveWordSearch exWords1'4 exGrid1'3) exAnsw1'4
                                         assertBool "Test Timed out (more than 15 seconds to do it)" (timeout time2 ))
                let total = TestList [ TestLabel "Benchmark and check 50x50 grid with all words found" test1,
                                       TestLabel "Benchmark and check 50x50 grid with none of the words found" test2]
                runTestTT total
------------------------------------------------------------

-- Helper Function
upperCase :: [(String, Maybe Placement)] -> [(String, Maybe Placement)]
upperCase xs = zip (map (map toUpper.fst) xs) (map snd xs)

removeWord :: String -> [String] -> [String]
removeWord word = foldr (\x -> if x == word then ("":) else (x:)) []


-- Challenge 2

-- Testing for the error cases
------------------------------------------------------------
errCase2'1 = assertIOError "Grid can't have density of 0"  exErr2'1
errCase2'2 = assertIOError "Grid can't have density of 1" exErr2'2
errCase2'3 = assertIOError "Words can only have alphabet characters" exErr2'3
errCase2'4 = assertIOError "Cannot have empty words" exErr2'4

exErr2'1 =  createWordSearch ["JUST", "LITTLE", "TEST"] 0
exErr2'2 =  createWordSearch ["JUST", "LITTLE", "TEST"] 1
exErr2'3 =  createWordSearch ["USE", "WEIRD", "SYMBOL\0"] 0.6
exErr2'4 =  createWordSearch (removeWord (last emptyStringWords) emptyStringWords) 0.8

testErr2 = TestList [TestLabel "Checking wrong density exception (0)" errCase2'1,
                       TestLabel "Checking wrong density exception (1)" errCase2'2,
                       TestLabel "Checking words with invalid symbol" errCase2'3,
                       TestLabel "Checking empty words exception" errCase2'4]
------------------------------------------------------------

-- Testing Trivial Cases
------------------------------------------------------------
ex2'1 = createWordSearch exWords2'1 exDensity2'1
exWords2'1 = ["HERE", "PROVIDE", "TEST", "SIMPLE"]
exDensity2'1 = 0.8

ex2'2 = createWordSearch exWords2'2 exDensity2'2
exWords2'2 = ["OKAY", "ANOTHER", "CHALLENGE", "PERFORMING", "MIRACLE", "POSSIBILITY"] 
exDensity2'2 = 0.95

verifyShape :: WordSearchGrid -> Bool
verifyShape [] = False
verifyShape grid@(row:_) = length row == length grid

verifyDensity :: WordSearchGrid -> Int -> Double -> Bool
verifyDensity grid total density = (dTotal / (dLength^2 )) < density
    where
        dTotal = fromIntegral total :: Double
        dLength = fromIntegral (length grid) :: Double
    
getFoundWords :: [(String, Maybe Placement)] -> [String]
getFoundWords [] = []
getFoundWords ((_, Nothing):rest) = getFoundWords rest
getFoundWords ((word,Just _):rest) = word : getFoundWords rest

simpleTest2'1 = do grid1 <- ex2'1
                   let solution1 = solveWordSearch exWords2'1 grid1
                   time2 <- benchmark (createWordSearch exWords2'1 exDensity2'1)
                   let test1 = TestCase (do assertBool  "Shape Must be Square" (verifyShape grid1)
                                            assertBool  "Density must be strictly less" (verifyDensity grid1 (length exWords2'1) exDensity2'1)
                                            assertEqual "Not all the words were found" (getFoundWords solution1) exWords2'1
                                            assertBool  "Test Timed out (more than 15 seconds to do it)" (timeout time2))
                   
                   grid2 <- ex2'2
                   let solution2 = solveWordSearch exWords2'2 grid2
                   time2 <- benchmark (createWordSearch exWords2'2 exDensity2'2)
                   let test2 = TestCase (do assertBool  "Shape Must be Square" (verifyShape grid2)
                                            assertBool  "Density must be strictly less" (verifyDensity grid2 (length exWords2'2) exDensity2'2)
                                            assertEqual "Not all the words were found" (getFoundWords solution2) exWords2'2
                                            assertBool  "Test Timed out (more than 15 seconds to do it)" (timeout time2))

                   let testList = TestList [TestLabel "First Example" test1,
                                            TestLabel "Second Example" test2]
                   runTestTT testList

-- Testing Lower Case 
------------------------------------------------------------
ex2'3 = createWordSearch exWords2'3 exDensity2'3
exWords2'3 = ["OkaY", "AnoTHeR", "CHalLenGE", "PeRfoRMiNG", "MiraCLe", "PosSIbiLITy"] 
exDensity2'3 = 0.95

exCasing2'1 = do grid1 <- ex2'3
                 let solution1 = solveWordSearch exWords2'3 grid1
                 time2 <- benchmark (createWordSearch exWords2'3 exDensity2'3)
                 let test1 = TestCase (do assertBool  "Shape Must be Square" (verifyShape grid1)
                                          assertBool  "Density must be strictly less" (verifyDensity grid1 (length exWords2'2) exDensity2'2)
                                          assertEqual "Not all the words were found"  (getFoundWords $ upperCase solution1) exWords2'2
                                          assertBool  "Test Timed out (more than 15 seconds to do it)" (timeout time2))
                 runTestTT test1
------------------------------------------------------------

-- Testing a few Extreme Cases
------------------------------------------------------------
ex2'4 = createWordSearch exWords2'4 exDensity2'4
exWords2'4 = ["ANOTHER", "BITES", "DUST", "QUEEN", "IMPOSSIBLE", "UNREAL"] 
exDensity2'4 = 0.99

ex2'5 = createWordSearch exWords2'5 exDensity2'5
exWords2'5 = ["HEREISAVERYLONGWORDTOPUTIN", "SMALL"] 
exDensity2'5 = 0.99

extremeTest2'1 = do grid1 <- ex2'4
                    let solution1 = solveWordSearch exWords2'4 grid1
                    time2 <- benchmark (createWordSearch exWords2'4 exDensity2'4)
                    let test1 = TestCase (do assertBool  "Shape Must be Square" (verifyShape grid1)
                                             assertBool  "Density must be strictly less" (verifyDensity grid1 (length exWords2'4) exDensity2'4)
                                             assertEqual "Not all the words were found" (getFoundWords solution1) exWords2'4
                                             assertBool  "Test Timed out (more than 15 seconds to do it)" (timeout time2))
                   
                    grid2 <- ex2'5
                    let solution2 = solveWordSearch exWords2'5 grid2
                    time2 <- benchmark (createWordSearch exWords2'5 exDensity2'5)
                    let test2 = TestCase (do assertBool  "Shape Must be Square" (verifyShape grid2)
                                             assertBool  "Density must be strictly less" (verifyDensity grid2 (length exWords2'5) exDensity2'5)
                                             assertEqual "Not all the words were found" (getFoundWords solution2) exWords2'5
                                             assertBool  "Test Timed out (more than 15 seconds to do it)" (timeout time2))

                    let testList = TestList [TestLabel "Very dense grid (0.99)" test1,
                                            TestLabel "One very long word" test2]
                    runTestTT testList
------------------------------------------------------------
-- Challenge 3

-- Testing for the error cases
------------------------------------------------------------
errCase3'1 = assertError "Negative numbers are not allowed in variables" (prettyPrint errExpr3'1)
errExpr3'1 = LamDef [] (LamVar (-3))

errCase3'2 = assertError "Negative numbers are not allowed in variables" (prettyPrint errExpr3'2)
errExpr3'2 = LamDef [] (LamAbs (-1) (LamVar 3)) 
------------------------------------------------------------
simpleTest3'1 = TestCase (assertEqual "Wrong pretty print output" (prettyPrint ex3'1) "(\\x1 -> x1) \\x1 -> x1")
simpleTest3'2 = TestCase (assertEqual "Wrong pretty print output" (prettyPrint ex3'2) "\\x1 -> x1 \\x1 -> x1")
simpleTest3'3 = TestCase (assertEqual "Wrong pretty print output" (prettyPrint ex3'3) "def F = \\x1 -> x1 in \\x2 -> x2 F")
simpleTest3'4 = TestCase (assertEqual "Wrong pretty print output" (prettyPrint ex3'4) "def F = \\x1 -> x1 in \\x2 -> F x2")

ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) 
-- simpleTest3'5 = assertEqual "Wrong pretty print output" (prettyPrint ex3'1) "(\\x1 -> x1) \\x1 -> x1"
-- simpleTest3'6 = assertEqual "Wrong pretty print output" (prettyPrint ex3'1) "(\\x1 -> x1) \\x1 -> x1"
-- simpleTest3'7 = assertEqual "Wrong pretty print output" (prettyPrint ex3'1) "(\\x1 -> x1) \\x1 -> x1"
-- simpleTest3'8 = assertEqual "Wrong pretty print output" (prettyPrint ex3'1) "(\\x1 -> x1) \\x1 -> x1"
-- simpleTest3'9 = assertEqual "Wrong pretty print output" (prettyPrint ex3'1) "(\\x1 -> x1) \\x1 -> x1"
-- simpleTest3'10 = assertEqual "Wrong pretty print output" (prettyPrint ex3'1) "(\\x1 -> x1) \\x1 -> x1"

-- ex3'5 = LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamApp(LamAbs 1 (LamVar 1)) (LamVar 2))] (LamAbs 1 (LamVar 1))
-- ex3'6 = LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamAbs 2 (LamAbs 1 (LamVar 1)))] (LamApp(LamAbs 2 (LamAbs 1 (LamVar 1))) (LamVar 2))
-- ex3'7 = LamDef [] (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))) (LamAbs 4 (LamApp (LamAbs 7 (LamApp (LamVar 7)  (LamVar 2))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3)))))))
-- ex3'8 = LamDef [("F", LamAbs 2 (LamAbs 1 (LamVar 1))),("G", LamAbs 1 (LamVar 1))] (LamApp (LamVar 3) (LamApp (LamVar 1) (LamVar 2)))
-- ex3'9 = LamDef []  (LamApp(LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 2))
-- ex3'10 = LamDef [] (LamAbs 2(LamAbs 1(LamAbs 0 (LamVar 0))))

simpleTests3'1 = TestList [TestLabel ("Simple Test 1: " ++ show ex3'1)  simpleTest3'1,
                           TestLabel ("Simple Test 2: " ++ show ex3'2)  simpleTest3'2,
                           TestLabel ("Simple Test 3: " ++ show ex3'3)  simpleTest3'3,
                           TestLabel ("Simple Test 4: " ++ show ex3'4)  simpleTest3'4]

-- Testing Edge Cases
------------------------------------------------------------
edgeEx3'5 = TestCase (assertEqual "Expressions in macros and the actual expression with macros isn't unexpanded" "def F = \\x1 -> x1 in def G = \\x2 -> F in G" (prettyPrint ex3'5))
ex3'5 = LamDef [("F",LamAbs 1 (LamVar 1)),("G",LamAbs 2 (LamAbs 1 (LamVar 1)))] (LamAbs 2 (LamAbs 1 (LamVar 1)))

edgeEx3'6 = TestCase (assertEqual "Expression in the outer macro simplified by the inner one" "def G = \\x2 -> \\x1 -> x1 in def F = \\x1 -> x1 in G" (prettyPrint ex3'6))
ex3'6 = LamDef [("G",LamAbs 2 (LamAbs 1 (LamVar 1))), ("F",LamAbs 1 (LamVar 1))] (LamAbs 2 (LamAbs 1 (LamVar 1)))

edgeEx3'7 = TestCase (assertEqual "If two macros are the same, the one defined earlier should be used" "def F = \\x1 -> x1 in def G = F in F" (prettyPrint ex3'7)) 
ex3'7 = LamDef [("F",LamAbs 1 (LamVar 1)),("G",LamAbs 1 (LamVar 1))] (LamAbs 1 (LamVar 1))

edgeEx3'8 = TestCase (assertEqual "Invalid bracketing, something to do with conflicting application and abstraction associativity" "x1 (\\x2 -> x2) (\\x3 -> x3) (\\x4 -> x4) \\x5 -> x5" (prettyPrint ex3'8)) 
ex3'8 = LamDef [] (LamApp (LamApp (LamApp (LamApp (LamVar 1) (LamAbs 2 (LamVar 2))) (LamAbs 3 (LamVar 3))) (LamAbs 4 (LamVar 4))) (LamAbs 5 (LamVar 5)))

edgeEx3'9 = TestCase (assertEqual "Invalid bracketing, something to do with conflicting application and abstraction associativity" "x1 x2 (x3 x4) \\x5 -> x5 x6" (prettyPrint ex3'9)) 
ex3'9 = LamDef [] (LamApp (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamApp (LamVar 3) (LamVar 4))) (LamAbs 5 (LamApp (LamVar 5) (LamVar 6))))

edgeEx3'10 = TestCase (assertEqual "Bigger macro should be used and outer macro shouldn't have inner macros" "def F = \\x2 -> x2 \\x1 -> x1 in def G = \\x1 -> x1 in F" (prettyPrint ex3'10)) 
ex3'10 = LamDef [("F",LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamVar 1)))),("G",LamAbs 1 (LamVar 1))] (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamVar 1))))

edgeTests3'1 = TestList [ TestLabel "Unexpanding Macros within expressions and within macros themselves" edgeEx3'5,
                        TestLabel "Outer macro can't have inner macros unexpanded" edgeEx3'6,
                        TestLabel "Outer Macro should be be used if the inner one is identical" edgeEx3'7,
                        TestLabel "Appropriate bracketing for abstraction and application 1" edgeEx3'8,
                        TestLabel "Appropriate bracketing for abstraction and application 2" edgeEx3'9,
                        TestLabel "Use of the biggest macro for unexpanding" edgeEx3'10]

------------------------------------------------------------
-- Challenge 4


-- Checking simple input Parsing rules
------------------------------------------------------------
simpleParsingRules = do quickCheck (\s -> if s < 0  then isNothing $ parseLamMacro ("x" ++ show (s :: Int)) else parseLamMacro ("x" ++ show (s::Int)) == Just (LamDef [] (LamVar s)))
                        quickCheck (\s -> if s /= 'x' then isNothing $ parseLamMacro ((s :: Char) : "1") else parseLamMacro "x1" == Just (LamDef [] (LamVar 1)))
                        quickCheck (\s -> if not $ isUpperLetters s  then isNothing $ parseLamMacro s else parseLamMacro s == Just (LamDef [] (LamMacro s)) )
                        quickCheck (\s -> if s < 0 then isNothing $ parseLamMacro ("\\x" ++ show (s :: Int) ++ " -> x2") else parseLamMacro ("\\x" ++ show (s :: Int) ++ " -> x2") == Just (LamDef [] (LamAbs s (LamVar 2))))

-- Testing simple applicative properties
------------------------------------------------------------
simpleApp4'1 = TestCase (assertEqual "Application is left associative" ex4'1 (parseLamMacro "x1 x2 x3")) 
ex4'1 = Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))

simpleApp4'2 = TestCase (assertEqual "Application is left associative" ex4'2 (parseLamMacro "x1 (x2 x3)")) 
ex4'2 = Just (LamDef [] (LamApp (LamVar 1) (LamApp (LamVar 2) (LamVar 3))))

simpleApp4'3 = TestCase (assertEqual "Application is left associative but extra brackets should still be accepted" ex4'3 (parseLamMacro "(x1 x2) x3")) 
ex4'3 = Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)))

simpleAppsTest = TestList [ TestLabel "Application associativity check" simpleApp4'1,
                            TestLabel "Application associativity check" simpleApp4'2,
                            TestLabel "Application associativity check (with unnecessary brackets)" simpleApp4'3 ]
-- Testing Mix of Abstraction and Application
------------------------------------------------------------
mixAppAbs4'4 = TestCase (assertEqual "Application is inside abstraction" ex4'4 (parseLamMacro "\\x1 -> x1 x2"))
ex4'4 = Just (LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2))))

mixAppAbs4'5 = TestCase (assertEqual "Application outside abstraction" ex4'5 (parseLamMacro "(\\x1 -> x1) x2"))
ex4'5 = Just (LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))

mixAppAbs4'6 = TestCase (assertEqual "Application is inside abstraction, but extra brackets should be still accepted" ex4'6 (parseLamMacro "\\x1 -> (x1 x2)"))
ex4'6 = Just (LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2))))

mixTest = TestList [ TestLabel "Application/abstraction precedence check" mixAppAbs4'4,
                     TestLabel "Application/abstraction precedence check" mixAppAbs4'5,
                     TestLabel "Application/abstraction precedence check (with unnecessary brackets)" mixAppAbs4'6 ]


-- Testing Examples provided
------------------------------------------------------------
case4'7 = TestCase (assertEqual ("Incorrect Parsing of expression " ++ ex4'7) ans4'7 (parseLamMacro ex4'7))
ex4'7 = "x1 x2 F"
ans4'7 = Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamMacro "F")))

case4'8 = TestCase (assertEqual ("Incorrect Parsing of expression " ++ ex4'8) ans4'8 (parseLamMacro ex4'8))
ex4'8 = "def F = \\x1-> x1 in \\x2 -> x2 F"
ans4'8 = Just (LamDef [("F",LamAbs 1 (LamVar 1))] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F"))))

case4'9 = TestCase (assertEqual ("Incorrect Parsing of expression " ++ ex4'9) Nothing (parseLamMacro ex4'9))
ex4'9 = "def F = \\x1 -> x1 (def G= \\x1 -> x1 in x1)in \\x2 -> x2"

case4'10 = TestCase (assertEqual ("Incorrect Parsing of expression " ++ ex4'10) Nothing (parseLamMacro ex4'10))
ex4'10 = "def F = \\x1 -> x1 in def F = \\x2 -> x2 x1 in x1"

case4'11 = TestCase (assertEqual ("Incorrect Parsing of expression " ++ ex4'11) Nothing (parseLamMacro ex4'11))
ex4'11 = "def F = x1 in F"

casesTest = TestList [ TestLabel "Given example 2" case4'7,
                       TestLabel "Given example 3" case4'8,
                       TestLabel "Given example 4" case4'9,
                       TestLabel "Given example 5" case4'10,
                       TestLabel "Given example 6" case4'11 ]

-- Testing a few More Scenarios
------------------------------------------------------------
case4'12 = TestCase (assertEqual ("Incorrect Parsing of expression " ++ ex4'12) Nothing (parseLamMacro ex4'12))
ex4'12 = "def F = \\x1 -> x1 \\x3 -> x2 x3 in F"

-- TODO verify the spacing later
-- Check macro within expression
    
-- Challenge 5
------------------------------------------------------------

-- Testing Simple Cases
------------------------------------------------------------
case5'1 = TestCase (assertEqual "Incorrect cps transformation of a variable" ans5'1 (cpsTransform ex5'1))
ex5'1 = LamDef [] (LamVar 3)
ans5'1 = LamDef [] (LamAbs 4 (LamApp (LamVar 4) (LamVar 3)))

case5'2 = TestCase (assertEqual "Incorrect cps transformation of abstraction" ans5'2 (cpsTransform ex5'2))
ex5'2 = LamDef [] (LamAbs 1 (LamVar 1))
ans5'2 = LamDef [] (LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 3 (LamApp (LamVar 3) (LamVar 1))))))

case5'3 = TestCase (assertEqual "Incorrect cps transformation of application" ans5'3 (cpsTransform ex5'3))
ex5'3 = LamDef[] (LamApp (LamVar 1) (LamVar 2))
ans5'3 = LamDef [] (LamAbs 3 (LamApp (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))) (LamAbs 4 (LamApp (LamAbs 7 (LamApp (LamVar 7) (LamVar 2))) (LamAbs 5 (LamApp (LamApp (LamVar 4) (LamVar 5)) (LamVar 3)))))))

case5'4 = TestCase (assertEqual "Incorrect cps transformation of macro" ex5'4 (cpsTransform ex5'4))
ex5'4 = LamDef [] (LamMacro "F")

simpleTest5 = TestList [
                        TestLabel "Checking cps variable transformation" case5'1,
                        TestLabel "Checking cps abstraction transformation" case5'2,
                        TestLabel "Checking cps application transformation" case5'3,
                        TestLabel "Checking cps macro transformation" case5'4
                       ]
-- Testing Given Examples
------------------------------------------------------------
exId5 =  (LamAbs 1 (LamVar 1))

case5'5 = TestCase (assertEqual "Incorrect cps transformation of a variable" ans5'5 (cpsTransform ex5'5))
ex5'5 = (LamDef [ ("F", exId5) ] (LamVar 2) )
ans5'5 = LamDef [("F",LamAbs 4 (LamApp (LamVar 4) (LamAbs 1 (LamAbs 5 (LamApp (LamVar 5) (LamVar 1))))))] (LamAbs 3 (LamApp (LamVar 3) (LamVar 2)))

case5'6 = TestCase (assertEqual "Incorrect cps transformation of a variable" ans5'6 (cpsTransform ex5'6))
ex5'6 = LamDef [ ("F", exId5) ] (LamMacro "F") 
ans5'6 = LamDef [("F",LamAbs 2 (LamApp (LamVar 2) (LamAbs 1 (LamAbs 3 (LamApp (LamVar 3) (LamVar 1))))))] (LamMacro "F")

case5'7 = TestCase (assertEqual "Incorrect cps transformation of a variable" ans5'7 (cpsTransform ex5'7))
ex5'7 = (LamDef [ ("F", exId5) ] (LamApp (LamMacro "F") (LamMacro "F")))
ans5'7 = LamDef [("F",LamAbs 5 (LamApp (LamVar 5) (LamAbs 1 (LamAbs 6 (LamApp (LamVar 6) (LamVar 1))))))] (LamAbs 2 (LamApp (LamMacro "F") (LamAbs 3 (LamApp (LamMacro "F") (LamAbs 4 (LamApp (LamApp (LamVar 3) (LamVar 4)) (LamVar 2)))))))

casesTest5 = TestList [ TestLabel "Given example 2" case5'5,
                        TestLabel "Given example 3" case5'6,
                        TestLabel "Given example 4" case5'7]

-- Challenge 6
------------------------------------------------------------
