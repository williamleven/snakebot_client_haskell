import Test.Hspec
import Test.QuickCheck
import TestUtil

main :: IO ()
main = hspec $ do
    describe "SnakeBot tests" $ do
        utilTest
