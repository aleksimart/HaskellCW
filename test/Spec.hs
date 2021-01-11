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

benchmark :: IO a -> IO Double
benchmark prog = do start <- getCPUTime
                    v <- prog
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
    
-- TODO More square grid tests

-- Challenge 1
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Testing for the error cases
------------------------------------------------------------
testGrid1 = assertError "Empty Grid Mustn't be allowed"      wrongEx1
testGrid2 = assertError "Non-square Grid Mustn't be allowed" wrongEx2 
testGrid3 = assertError "Empty String Mustn't be allowed (Last position)"    wrongEx3 
testGrid4 = assertError "Empty String Mustn't be allowed (Start position)" wrongEx4 
testErrors = TestList [TestLabel "Checking the Empty grid exception" testGrid1, 
                       TestLabel "Checking non-square Grid exception" testGrid2,
                       TestLabel "Checking for Empty string exception 1" testGrid3,
                       TestLabel "Checking for Empty strings exception 2" testGrid4]

wrongEx1 = solveWordSearch ["TEST"] []
wrongEx2 = solveWordSearch ["TEST"] ["AAA","AA"]
wrongEx3 = solveWordSearch (removeWord (head emptyStringWords) emptyStringWords) emptyStringGrid
wrongEx4 = solveWordSearch (removeWord (last emptyStringWords) emptyStringWords) emptyStringGrid

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
                let total = TestList [ TestLabel "Benchmarking above test" test1,
                                       TestLabel "Benchmarking above test" test2]
                runTestTT total
------------------------------------------------------------

-- Helper Function
upperCase :: [(String, Maybe Placement)] -> [(String, Maybe Placement)]
upperCase xs = zip (map (map toUpper.fst) xs) (map snd xs)

removeWord :: String -> [String] -> [String]
removeWord word = foldr (\x -> if x == word then ("":) else (x:)) []

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
