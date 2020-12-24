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

-- Two examples for you to try out, the first of which is in the instructions

exGrid1'1 = [ "HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG" ] 
exWords1'1 = [ "HASKELL","STRING","STACK","MAIN","METHOD"]

exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 = [ "BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE" ]


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

exGrid1'4 = ["BVMGKKLPUDNSAELXQEJTKZRWEQLXOJXTJBMDAYRJDILJPYVVSE",
  "RYRTWXGQNFUERPIOZXDGWPICWVZLGTDJSBJLORLQYVIKPFICNQ",
  "QPCUWGLCDYZDJLFEKHXSDLJIUBFWBVLWJLHPNBOVGYRCPTHUGS",
  "QCNSRIDYZQSFLPCMQNHJUQOHSIATOSQCJETGQPGQXUYARWVKWP",
  "VYQFSOHEEEQQGUOSXNPFZWCVSPKFMFJXSUFDBYEGFKMLIZYOVG",
  "JDXYPRTFPJRRWVRWBEEAWIDXEETEUDLAFQKDEFWSYOWQUQDUTU",
  "RFLUQNBPOGSNQWTVJUVJPSQOHLLNEJMXKAMHMKTGLBJXCLMESM",
  "SMEMXNFYCQCXRZKMUFBQFMTDCABDNLXMYGLFAMQEMZVCSQYOCK",
  "ERSDPQDCIVRLRWRDQEHRWVQGPESAILZXAPDBDACWWLYDRIPHFD",
  "EZCPPMPPOTNCRCREUAJESPKLLRZKPZSONFOQBDPENXIEMLOGBJ",
  "WDHLRWZYROXLRGZXKBZHWTNIINAHEPARROTHAXXBGYTPSWNDOW",
  "VFNVZHCWUDFBYYCNJHGTNZEOTUSUTLOEYHSPIMKDKLFCWKPGRM",
  "DPDVDOGMKMLJVHGTLJRADVANCEDUARLTXLUGYIUTIXCXQTJCSB",
  "FDTOZLJDDKKXWTEGBRDFAMIGQORIHBLUSGOYNEBBGVQSOMFSRA",
  "INEDEOKOZIUKOIUZKHTBSOOZIHOMIGWPDNLWEKIGHNSWZXIYGA",
  "XZBAWOGVOKJDYLVXHRLNPWJQJOWSITLMJIUJHHSYBSIVMLNLLL",
  "YBDEVIPBLXCJRFEZEEVNAJOLADSIITXOFUCBTLQYKORATILBVS",
  "OYPGSMHNEPQPSYIUNNSLSZRAUOMZQNTCQGIRUPQAZOLAAQTMQX",
  "XMMXNGVLJJMPQOEXCLXRYOXYPPSODVOEOIDFOPFMSSTQIVRGOD",
  "SFOYMXVYXJIDMLESAOACIIPQUXOGCAFRNRIEOUVEMNONIVPTQW",
  "XWFPPLUARGRXDBCKJNQLQOZBAXRZJSTNETRTSOSVXLECERQJNI",
  "VJWKJQTGUWZCZQXTXHQSRAPMNHNWLNFZUNVVGAJKIUIRCBBBIJ",
  "OXEYMLWDJLHXBOJLMNDILBNSAHZXXAMAZINGOTCCDCJWKVYAGG",
  "UJVDIFGFJPWNNHNJPJLKMWUZASDQKYEPYOPIFPSDKUCEQCLTMD",
  "VFJXKBJQNZZYLBOECHPDEKNYMVOBMBZBXJCTWVIXJXMKHNDMLF",
  "VJYJTTQXLZGFATRKQUHQXCYZCFXHYUBIHKOLRPJZBHOOGLJXBO",
  "USZCHYFRXDRQNIJVFSFUEEQBUKBRQAZBIYVBYFSKIAPCGQWIWC",
  "QOOXYYYUSQACFYXFSELFGCFVAJWPNOQNGBBNTOAMMMZPNXPMWP",
  "HISINLFPWRDDNWILFMUQCNRJDWQPKRRLCZVWJULXTHMUNBEUND",
  "YTFSNLYJKPJPIXIOKGTUANCTJDRZAJNZXPJZJWZLORNGEDMLFG",
  "SJENLPNVBODHUIMWQHLUSOYKWMLGEDALGJGDHCTNRSXRUQLEZM",
  "BJUDGLMGMLXYQMLSMMEUWWOHMSCHQDMMMTQNINJKMXFHUCGJES",
  "PZQNSGFPGSTNEFZDUDZFOXUSPQIWUEPZEDVEFNKKTIQWBOWPWK",
  "XFCPQBPIAMBAAMIPFLBXHIEMBXELKJISYEALJDTVPPBXTISWOA",
  "HLEOZQTHOTXPSMVUIIFAQMWXWRBSTGSNYXIFTEOQHLPKOCKQCE",
  "MYSTQZMFFTAFKQAIALANFQIJUZJALKHWLBNARNYMNPBELAQNOK",
  "HBOXSLULTPRTAHKLQRTFHIGMJMOHSKFHCEIDNOVZEGXJGKPIOD",
  "QQNSRMNPPDJYHFAIHZSRFSEKXWBBIWVVDOSNBUAXIFPZAJNXGC",
  "KQFWHMNYLFWEZDANXQTNKUMDKMSXEHZCOKDNBVLFLTNLNLBGBA",
  "ZXFSWQWHQYNXHSGWFXAWMIDPOUAFBKHNAAYAHIZJCRBEYNNXUC",
  "SAMZMPGLZVUPOCECEDNWVOIGIMRGILCHPSDUOSXMYRANYPSFKM",
  "IXKVRUWFJNTKLRHIGMJWTCLEOULYYPEMMCBKMFVRZWQCIHHQQY",
  "XDLZTDCDCDJAMUDVDTNWSMIPDJMCZBXJTHRDTPCZXNRMYAPUAB",
  "ZJGOECTMGIUIAPJNYKRSLBHLSBGYGPOOCHWQEHJUVKEDPSPDKJ",
  "VFUKDEPHUJQHROZDYNHYMIYNJGTJIFMSXUAKYMTABNXIHULXZI",
  "BJYXLFSSSOMOIQPCZZCOXIVMLIDRSFVUROJTKSBPQDUOKCBJHS",
  "XZJBKEZRHVNEVBEJCAKJBOOYPJHLBHYPPWOSOHOFBJJGUTNTJV",
  "ZPPGNYDEMCJLAMNKCQEMDWSLUVLOVQMJQULJNZTCNMUOOHYTNG",
  "XKVNOGIIODCWTUXKINPARIYZCHOXCAHYQSPDWYMEZDIJKGIVBJ",
  "VYEZGZBFBHHLZECBNPXQVTLFQSXLLKPFQOGAYMCCHVCDHBCQUZ"]

exWords1'4 = ["ADVANCED",
  "AMAZING",
  "BEAUTIFUL",
  "CHESS",
  "COMPUTER",
  "FATHER",
  "HASKELL",
  "HATE",
  "INTRIGUING",
  "ITERMITTENT",
  "JOINT",
  "LION",
  "LOVE",
  "LUDICROUS",
  "OUT",
  "PARROT",
  "POINT",
  "POSSIBLE",
  "RIDICULOUS",
  "SON",
  "UNBELIEVABLE",
  "UNREAL",
  "UNSTOPPABLE",
  "WINNER",
  "WORDS"]
-- Challenge 2 --

createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch ws density = gridFinal
  where
    gridN= gridlength ws density
    gridEmpty = genEmptyGrid gridN
    charSet = availableChar ws 
    gridFinal = do gridWithWords <- insertWords ws gridEmpty
                   fillGrid charSet gridWithWords

debug :: [String] -> Double -> IO WordSearchGrid
debug ws density = insertWords ws gridEmpty
  where
    gridN= gridlength ws density
    gridEmpty = genEmptyGrid gridN
    charSet = availableChar ws 
-- createWordSearch ws den = 
--   where
--     wordsOrientation = genWordOrientation ws
--     length = gridlength ws den
--     emptyGrid = genEmptyGrid length


-- Random Number generation 
------------------------------------------------------------
randVal :: Int -> Int -> IO Int
randVal low up =  randomRIO (low, up)

randList :: Int -> Int -> Int -> IO [Int]
randList 0 _ _= return []
randList size low up = do n <- randVal low up 
                          ns <- randList (size-1) low up
                          return (n:ns)
------------------------------------------------------------

-- Orientation generation
------------------------------------------------------------
numToOrient :: Int -> Orientation
numToOrient index = [Forward, DownForward,
                     Down,    DownBack,
                     Back,    UpBack,
                     Up,      UpForward]!!index

getDirections :: [Int] -> [Orientation]
getDirections = map numToOrient

-- genWordOrientation :: [String] -> IO [(String, Orientation)]
-- genWordOrientation ws = do let size = length ws 
--                            numList <- randList size 0 7
--                            let directionList = getDirections numList 
--                            let tuples = zip ws directionList
--                            return tuples

genWordOrientation :: String -> IO (String, Orientation)
genWordOrientation w = do num <- randVal 0 7
                          let orientation = numToOrient num
                          return (w, orientation)

------------------------------------------------------------

-- Empty Grid Generation
------------------------------------------------------------
gridlength :: [String] -> Double -> Int
gridlength ws density = ceiling((characterNo ws / density)/3) 
  where
    characterNo = fromIntegral.length.concat

-- So far not needed
genLine :: [String] -> Int -> IO String
genLine ws ll = do let letters = (rmdups.concat) ws 
                   let size = length letters
                   values <- randList ll 0 (size - 1)
                   let line = map (letters!!) values
                   return line

-- genEmptyGrid :: [String] -> Int -> IO [String]
-- genEmptyGrid ws l = let line = genLine ws l
--                      in sequenceIO $ replicate l line

genEmptyGrid :: Int -> [String]
genEmptyGrid l = replicate l (concat (replicate l "0"))
------------------------------------------------------------

-- Position Generation
------------------------------------------------------------
genPosn :: Orientation -> Int -> Int ->  IO Posn
genPosn l wordl gridl = do let ((lowCol, upCol),(lowRow, upRow)) = posnRestrictions l wordl gridl
                           col <- randVal lowCol upCol
                           row <- randVal lowRow upRow
                           return (col,row)

posnRestrictions :: Orientation -> Int -> Int -> ((Int,Int), (Int, Int))
posnRestrictions Forward wordl gridl        = ((0, gridl- wordl), (0, gridl-1))
posnRestrictions Back wordl gridl           = ((wordl-1, gridl-1 ), (0, gridl-1))
posnRestrictions Down wordl gridl           = ((0, gridl-1), (0, gridl- wordl))
posnRestrictions Up wordl gridl             = ((0, gridl-1), (wordl - 1, gridl - 1))
posnRestrictions DownForward wordl gridl    = ((0, gridl-wordl), (0, gridl-wordl))
posnRestrictions UpForward wordl gridl      = ((0, gridl-wordl), (wordl - 1, gridl - 1))
posnRestrictions DownBack wordl gridl       = ((wordl-1, gridl-1), (0, gridl-wordl))
posnRestrictions UpBack wordl gridl         = ((wordl-1, gridl-1), (wordl - 1, gridl - 1))

------------------------------------------------------------
  
--Word Insertion
------------------------------------------------------------
tryInsertWord :: Orientation -> Posn -> String -> [String]  -> Bool
tryInsertWord _ _ [] _ = True 
tryInsertWord Forward p@(col, row) (l:ls) css 
  |findLetter css p == '0' = tryInsertWord Forward (col+1, row) ls css
  |findLetter css p == l = tryInsertWord Forward (col+1, row) ls css
  |otherwise = False
tryInsertWord Back p@(col, row) (l:ls) css 
  |findLetter css p == '0' = tryInsertWord Back (col-1, row) ls css
  |findLetter css p == l = tryInsertWord Back (col-1, row) ls css
  |otherwise = False
tryInsertWord Up p@(col, row) (l:ls) css 
  |findLetter css p == '0' = tryInsertWord Up (col, row - 1) ls css
  |findLetter css p == l = tryInsertWord Up (col, row - 1) ls css
  |otherwise = False
tryInsertWord Down p@(col, row) (l:ls) css 
  |findLetter css p == '0' = tryInsertWord Down (col, row + 1) ls css
  |findLetter css p == l = tryInsertWord Down (col, row + 1) ls css
  |otherwise = False
tryInsertWord DownForward p@(col, row) (l:ls) css 
  |findLetter css p == '0' = tryInsertWord DownForward (col+1, row + 1) ls css
  |findLetter css p == l = tryInsertWord DownForward (col+1, row + 1) ls css
  |otherwise = False
tryInsertWord DownBack p@(col, row) (l:ls) css 
  |findLetter css p == '0' = tryInsertWord DownBack (col-1, row + 1) ls css
  |findLetter css p == l = tryInsertWord DownBack (col-1, row + 1) ls css
  |otherwise = False
tryInsertWord UpBack p@(col, row) (l:ls) css 
  |findLetter css p == '0' = tryInsertWord UpBack (col-1, row - 1) ls css
  |findLetter css p == l = tryInsertWord UpBack (col-1, row - 1) ls css
  |otherwise = False
tryInsertWord UpForward p@(col, row) (l:ls) css 
  |findLetter css p == '0' = tryInsertWord UpForward (col+1, row - 1) ls css
  |findLetter css p == l = tryInsertWord UpForward (col+1, row - 1) ls css
  |otherwise = False

replaceLetter :: Posn -> Char -> [String] -> [String]
replaceLetter (0, 0) l ((c:cs):css) = (l:cs):css
replaceLetter (col, 0) l ((c:cs):css)= (c: head (replaceLetter (col-1, 0) l (cs:css))) : css
replaceLetter (col, row) l (cs:css) = cs : replaceLetter (col, row - 1) l css

insertWord :: Orientation -> Posn -> String -> [String] -> [String]
insertWord _ _ [] css = css
insertWord Forward p@(col,row) (l:ls) css = insertWord Forward (col+1, row) ls (replaceLetter p l css)
insertWord Back    p@(col,row) (l:ls) css = insertWord Back (col-1, row) ls (replaceLetter p l css)
insertWord Up p@(col,row) (l:ls) css = insertWord Up (col, row-1) ls (replaceLetter p l css)
insertWord Down p@(col,row) (l:ls) css = insertWord Down (col, row+1) ls (replaceLetter p l css)
insertWord UpForward p@(col,row) (l:ls) css = insertWord UpForward (col+1, row-1) ls (replaceLetter p l css)
insertWord DownForward p@(col,row) (l:ls) css = insertWord DownForward (col+1, row+1) ls (replaceLetter p l css)
insertWord UpBack p@(col,row) (l:ls) css = insertWord UpBack (col-1, row-1) ls (replaceLetter p l css)
insertWord DownBack p@(col,row) (l:ls) css = insertWord DownBack (col-1, row+1) ls (replaceLetter p l css)

-- successfullInsertWord :: Posn -> [String] -> [String] -> IO [String]
-- successfullInsertWord p (w:ws) css = do (_,orientation) <- genWordOrientation w
--                                         if tryInsertWord w orientation then
--                                                                        grid <- insertWord orientation p 

-- Change to foldM
insertWords :: [String] -> [String] -> IO [String]
insertWords [] css = return css
insertWords (w:ws) css = do first <- generateInsertWord w css
                            insertWords ws first

generateInsertWord :: String -> [String] -> IO [String]
generateInsertWord w css = do (_, orientation) <- genWordOrientation w
                              position <- genPosn orientation (length w) (length css)
                              if tryInsertWord orientation position w css 
                                 then 
                                   return (insertWord orientation position w css)
                                 else 
                                   generateInsertWord w css

-- canReplaceLetter :: [String] -> Char -> Posn -> Bool canReplaceLetter css l p = findLetter css p == '0' || findLetter css p == l

------------------------------------------------------------
  
--Finish Off the Grid
------------------------------------------------------------
fillGrid ::String -> [String] -> IO[String]
fillGrid _ [] = return []
fillGrid ls (cs:css) = do line <- fillLine ls cs
                          rest <- fillGrid ls css
                          return (line:rest)

fillLine :: String -> String -> IO String
fillLine _ [] = return []
fillLine ls (c:cs) = do character <- randomChar ls
                        let r = tryReplace character c
                        rest <- fillLine ls cs
                        return (r:rest)

availableChar :: [String] -> String
availableChar = rmdups.concat
            
tryReplace :: Char -> Char -> Char
tryReplace l c  | c == '0' = l
  | otherwise = c

randomChar :: String -> IO Char
randomChar ls = do index <- randVal 0 (length ls - 1)
                   return (ls!!index)
                        
------------------------------------------------------------
-- Additional Functions
------------------------------------------------------------
-- TODO: Possibly add it back to empty grid
sequenceIO :: [IO a] -> IO[a]
sequenceIO [] = return []
sequenceIO (x:xs) = do el <- x
                       els <- sequenceIO xs
                       return (el:els)

-- https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
-- Removes duplicates with O(nlogn) time
-- TODO:: Maybe change it to the other stable solution (check link, comments under the solution i used)
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
------------------------------------------------------------


 
--- Convenience functions supplied for testing purposes
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- debug words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do printLine w
                      printGrid ws

printLine :: String -> IO ()
printLine [] = putChar '\n' 
printLine (l:ls) = do putChar l
                      putChar ' '
                      printLine ls


cheekyHelp:: IO ()
cheekyHelp = do puzzle <- createWordSearch ["МАМА", "ПАПА", "ЮЛА", "БАБУШКА", "КАРТОШКА"] 0.9
                printGrid puzzle

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
