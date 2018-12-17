import Test.Hspec
import Test.QuickCheck
import TestUtil
import TestMessages

main :: IO ()
main = hspec $ do
    describe "SnakeBot tests" $ do
        utilTest
        messagesTest
