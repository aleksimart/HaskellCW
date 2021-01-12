import Lib
import Test.HUnit
import Control.Exception
import Control.Monad
import Data.Char
import System.CPUTime
import Text.Printf

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
    putStrLn ""
    putStrLn "Testing Challenge 1"
    putStrLn "++++++++++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn "Testing for errors"
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
    putStrLn ""
    putStrLn "Testing Larger Inputs"
    putStrLn "----------------------------------------------------"
    testLarge1
    putStrLn "----------------------------------------------------"
    putStrLn ""
    putStrLn "Testing Challenge 2"
    putStrLn "++++++++++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn "Testing for errors"
    putStrLn "----------------------------------------------------"
    runTestTT testErr2
    putStrLn "Testing Simple Examples"
    putStrLn "----------------------------------------------------"
    simpleTest2'1
    putStrLn "----------------------------------------------------"
    putStrLn ""
    putStrLn "Testing Lower case"
    putStrLn "----------------------------------------------------"
    exCasing2'1 
    putStrLn "----------------------------------------------------"
    putStrLn "Extreme test case"
    putStrLn "----------------------------------------------------"
    extremeTest2'1
    putStrLn "----------------------------------------------------"
    
-- TODO More square grid tests

-- Challenge 1
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Testing for the error cases
------------------------------------------------------------
exEr1'1 = assertError "Empty Grid Mustn't be allowed"      wrongEx1
exEr1'2 = assertError "Non-square Grid Mustn't be allowed" wrongEx2 
exEr1'3 = assertError "Empty String Mustn't be allowed (Last position)"    wrongEx3 
exEr1'4 = assertError "Words containing non-alphabetic characters are not allowed" wrongEx4 
testErrors = TestList [TestLabel "Checking the Empty grid exception" exEr1'1, 
                       TestLabel "Checking non-square Grid exception" exEr1'2,
                       TestLabel "Checking for Empty string exception" exEr1'3,
                       TestLabel "Checking for Invalid words expection" exEr1'4]

wrongEx1 = solveWordSearch ["TEST"] []
wrongEx2 = solveWordSearch ["TEST"] ["AAA","AA"]
wrongEx3 = solveWordSearch (removeWord (head emptyStringWords) emptyStringWords) emptyStringGrid
wrongEx4 = solveWordSearch ["JFO L", "SDF#EW$"] emptyStringGrid

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
    
-- Testing Case sensitivity
------------------------------------------------------------
testLowerCase1 = TestCase (assertEqual "Lower case grid/words must still work" 
                            (upperCase (solveWordSearch lowerCaseWordsExample1 lowerCaseGridExample1) ) exAnsw1)
lowerCaseGridExample1 = [ "hagnirtsh" , "sacagetak", "gcstackel","mghkmilki","eknletgcn","tnirtlete","iraahclsr","mamrosagd","gizkddnrg" ] 
lowerCaseWordsExample1 = [ "haskell","string","stack","main","method"]

testLowerCase2 = TestCase (assertEqual "Mixed case grid/words must still work" (solveWordSearch exWords1'2 exGrid1'2) exAnsw2)
lowerCaseGridExample2 = ["ROBREUmbR","AURpepsaN","UNLALMSEE","ygaunpyyP","NLmnbgeNA","NBlealeoR","ALrypBBLG","nrepBEBEP","YgayARoMr"]
lowerCaseWordsExample2 = [ "Banana", "ORanGE", "MELON", "RASPBerRY","APplE","PluM","GRapE" ]

testLowerCases = TestList [TestLabel "Everything is lower case " testLowerCase1,
                           TestLabel "Everything is mixed case " testLowerCase2]
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
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Testing for the error cases
------------------------------------------------------------
errCase2'1 = assertIOError "Grid can't have density of 0"  exErr2'1
errCase2'2 = assertIOError "Grid can't have density of 1" exErr2'2
errCase2'3 = assertIOError "Words can only have alphabet characters" exErr2'3
errCase2'4 = assertIOError "Cannot have empty words" exErr2'4

exErr2'1 =  createWordSearch ["JUST", "LITTLE", "TEST"] 0
exErr2'2 =  createWordSearch ["JUST", "LITTLE", "TEST"] 1
exErr2'3 =  createWordSearch ["USE#$#", "  WEIRD %^", "SYMBOL(*S)"] 0.6
exErr2'4 =  createWordSearch (removeWord (last emptyStringWords) emptyStringWords) 0.8

testErr2 = TestList [TestLabel "Checking wrong density exception (0)" errCase2'1,
                       TestLabel "Checking wrong density exception (1)" errCase2'2,
                       TestLabel "Checking words with invalid symbols" errCase2'3,
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

                    let testList = TestList [TestLabel "First Example" test1,
                                            TestLabel "Second Example" test2]
                    runTestTT testList
------------------------------------------------------------

-- Test the grid size is NxN and isn't empty
-- Test the the word isn't empty
