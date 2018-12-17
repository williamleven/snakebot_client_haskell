import Test.Hspec
import Test.QuickCheck
import TestUtil
import TestMessages

main :: IO ()
main = hspec $
    describe "SnakeBot tests" $ do
        utilTest
        messagesTest
