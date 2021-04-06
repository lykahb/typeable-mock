import Control.Monad (void)
import Data.Maybe (fromMaybe)
import TypeableMock

functionCallingMock mocks = do
  fromMaybe print (useMockM1 mocks "print") "some string"

main :: IO ()
main = do
  m1 <- makeMock "print" $ mockM1 ((\arg -> void $ print ("mocked " <> arg)) :: String -> IO ())
  m2 <- makeMock "myPrint" $ mockM1 ((\arg -> void $ print ("mocked myPrint with string " <> arg)) :: String -> IO ())
  m3 <- makeMock "myPrint" $ mockM1 ((\arg -> print ("mocked myPrint with int " <> show arg)) :: (Int, Int) -> IO ())
  m4 <- makeMock "myPrint2" $ mockM2 ((\_ _ -> putStrLn "mocking two arg function" >> pure ()) :: Int -> Int -> IO ())

  functionCallingMock (mocksToConfig [m1, m2, m3, m4])

  checkCalls m1 [ExpectedArg ("some string" :: String)]
  -- checkCalls m2 [ExpectedArg ("string" :: String)]
  -- checkCalls m3 [ExpectedArg (1 :: Int, 2 :: Int)]
  -- checkCalls m4 [(1 :: Int, 2 :: Int), (3, 4)]

-- checkCalls m4 [(1 :: Int, 2 :: Int)]  -- Exception: user error (Mock myPrint2 has unexpected calls: [["3","4"]])
-- checkCalls m4 ([] :: [ExpectedArg])  -- Exception: user error (Mock myPrint2 has unexpected calls: 2 calls)

