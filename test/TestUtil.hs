{-# LANGUAGE DuplicateRecordFields #-}
module TestUtil (
    utilTest
) where
import Test.QuickCheck
import Test.Hspec
import Numeric.Natural
import Data.Maybe
import Types
import Util

instance Arbitrary Natural where
    arbitrary = suchThatMap genInt (Just . intToNatural)
        where genInt = arbitrary :: Gen Integer
              intToNatural i = (fromInteger (abs i)) :: Natural

instance Arbitrary Direction where
    arbitrary = elements [RIGHT, DOWN, LEFT, UP]

propDirTurn :: Direction -> Integer -> Bool
propDirTurn d n = (turnDir (turnDir d n) (-n)) == d

propConvertCoordPos :: Position -> Natural -> Property
propConvertCoordPos c mw = mw /= 0 ==> c == c'
    where c' = toPosition (toCoordinate c mw) mw

propAddCoordsCommutative :: Coordinate -> Coordinate -> Bool
propAddCoordsCommutative c1 c2 = r1 == r2
    where r1 = addCoords c1 c2
          r2 = addCoords c2 c1

propAddSubCoords :: Coordinate -> Coordinate -> Bool
propAddSubCoords c1 c2 = c1 == c1''
    where c1'' = subCoords c1' c2
          c1'  = addCoords c1  c2

propSymmetricDistances :: Integer -> Integer -> Bool
propSymmetricDistances x y = (mD z (x, y)) == (mD z (y, x))
                          && (eD z (x, y)) == (eD z (y, x))
    where eD = euclidianDist
          mD = manhattanDist
          z = (0, 0)

propDirToDelta :: Direction -> Bool
propDirToDelta d = d == d'
    where d' = dirFromDelta delta
          delta = dirToDelta d

propDirStep :: Direction -> Integer -> Integer -> Bool
propDirStep d x y = c'' == c
    where c'' = coordAddDelta c' d'
          c' = coordAddDelta c d
          c = (x, y)
          d' = reverseDir d

propSquareWithinItself :: Coordinate -> Coordinate -> Bool
propSquareWithinItself c1 c2 =
    and [isWithinSquare c1 c1 c2, isWithinSquare c2 c1 c2]

exMap :: Map
exMap = Map {
    width = 20,
    height = 16,
    worldTick = 1,
    snakeInfos = [
        SnakeInfo {
            id = "snake_1",
            name = "Snake #1",
            points = 42,
            positions = [p 2 3, p 3 3, p 3 4],
            tailProtectedForGameTicks = 0
        },
        SnakeInfo {
            id = "snake_2",
            name = "Snake #2",
            points = 13,
            positions = [p 19 1, p 19 2, p 19 3],
            tailProtectedForGameTicks = 2
        }],
    foodPositions = [p 6 6, p 9 15],
    obstaclePositions = [p 17 2, p 10 10]
} where p x y = toPosition (x, y) 20

utilTest :: Spec
utilTest = context "UtilTest" $ do
    it "Convert Coordinate -> Position -> Coordinate" $ do
        quickCheck propConvertCoordPos
    it "Test addCoords commutativity" $ do
        quickCheck propAddCoordsCommutative
    it "Test addCoords inverse subCoords" $ do
        quickCheck propAddSubCoords
    it "Distance Symmetry" $ do
        quickCheck propSymmetricDistances
    it "Manhattan Distance" $ do
        10 `shouldBe` (manhattanDist (1,1) (4, 8))
    it "Euclidian Distance" $ do
        10.0 `shouldBe` (euclidianDist (1,1) (7, 9))
    it "Add Coordinates" $ do
        (9, 3) `shouldBe` (addCoords (3, 8) (6, -5))
    it "Subtract Coordinates" $ do
        (-3, 13) `shouldBe` (subCoords (3, 8) (6, -5))
    it "Convert Direction -> Coodinate -> Direction" $ do
        quickCheck propDirToDelta
    it "Step Direction" $ do
        quickCheck propDirStep
    it "Coordinate Add Direction" $ do
        (-4, 3) `shouldBe` (coordAddDelta (-3, 3) LEFT)
        (-2, 3) `shouldBe` (coordAddDelta (-3, 3) RIGHT)
        (-3, 4) `shouldBe` (coordAddDelta (-3, 3) DOWN)
        (-3, 2) `shouldBe` (coordAddDelta (-3, 3) UP)
    it "Test isWithinSquare" $ do
        quickCheck propSquareWithinItself
    it "Test getSnakeBy*" $ do
        getSnakeById exMap "snake_1" `shouldBe` getSnakeByName exMap "Snake #1"
        getSnakeById exMap "snake_2" `shouldBe` getSnakeByName exMap "Snake #2"
    it "Test getTileAt" $ do
        -- Test get snake tiles
        getTileAt exMap (3, 4) `shouldBe`
            (SnakeTail $ fromJust $ getSnakeById exMap "snake_1")
        getTileAt exMap (3, 3) `shouldBe`
            (SnakeBody $ fromJust $ getSnakeById exMap "snake_1")
        getTileAt exMap (19, 1) `shouldBe`
            (SnakeHead $ fromJust $ getSnakeById exMap "snake_2")

        -- Test get obstacles
        getTileAt exMap (10, 10) `shouldBe` Obstacle
        getTileAt exMap (17,  2) `shouldBe` Obstacle

        -- Test get food
        getTileAt exMap (6,  6) `shouldBe` Food
        getTileAt exMap (9, 15) `shouldBe` Food

        -- Test get empty
        getTileAt exMap ( 0,  0) `shouldBe` Empty
        getTileAt exMap ( 4,  8) `shouldBe` Empty
        getTileAt exMap (19, 15) `shouldBe` Empty

        -- Test get wall
        getTileAt exMap ( -1, -1) `shouldBe` Wall
        getTileAt exMap (  5, 20) `shouldBe` Wall
        getTileAt exMap (999,  0) `shouldBe` Wall
    it "Test tileSafe snake tail" $ do
        tileSafe (getTileAt exMap (19, 3)) `shouldBe` False
        tileSafe (getTileAt exMap ( 3, 4)) `shouldBe` True
    it "Test snakeFacing" $ do
        snakeFacing (fromJust $ getSnakeById exMap "snake_1") (width exMap)
            `shouldBe` LEFT

        snakeFacing (fromJust $ getSnakeById exMap "snake_2") (width exMap)
            `shouldBe` UP


