import Lib
import Test.HUnit
import Control.Exception
import Control.Monad


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
    putStrLn "Testing Trivial Errors"
    runTestTT testErrors
    putStrLn "Testing the Start Letter Function"
    
-- TODO More square grid tests
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

-- testWorking = TestCase (assertEqual )
-- Test the grid size is NxN and isn't empty
-- Test the the word isn't empty
