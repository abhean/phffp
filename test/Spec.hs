import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import WordNumber (digitToWord, digits)

divisor :: (Arbitrary a, Num a, Eq a) => Gen a
divisor = arbitrary `suchThat` (/=0)

getNumeratorDivisorPair :: (Arbitrary a, Num a, Eq a) => Gen (a, a)
getNumeratorDivisorPair = do
  x <- arbitrary
  y <- divisor
  return (x, y)

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, True) = (Just y, True)
        go y (Just x, True)  = (Just y, x >= y)

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = (x + y) + z == x + (y + z)

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = (x + y) == (y + x)

productAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
productAssociative x y z = (x * y) * z == x * (y * z)

productCommutative :: (Eq a, Num a) => a -> a -> Bool
productCommutative x y = (x * y) == (y * x)

prop_quotRemTest :: Property
prop_quotRemTest = forAll (getNumeratorDivisorPair :: Gen (Int, Int)) (\(n, d) -> quot n d * d + rem n d == n)

prop_divModTest :: Property
prop_divModTest = forAll (getNumeratorDivisorPair :: Gen (Int, Int)) (\(n, d) -> div n d * d + mod n d == n)

powerAssociative :: Integral a => a -> a -> a -> Bool
powerAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

powerCommutative :: Integral a => a -> a -> Bool
powerCommutative x y = (x ^ y) == (y ^ x)

-- TODO[egarcia] use coarbitrary to generate random functions?
testFunctionApplication :: Int -> Bool
testFunctionApplication x = ((+1) x) == ((+1) $ x)

square :: Num a => a -> a
square x = x * x

squareIdentity :: Floating a => a -> a
squareIdentity = square . sqrt

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "QuickCheck exercises" $ do
    it "half" $ do
      property (\x -> (x ::Double) == halfIdentity x)
    it "sorted list" $ do
      property ((listOrdered . sort) :: [Int] -> Bool)
    it "plus associative" $ do
      property (plusAssociative :: Int -> Int -> Int -> Bool)
    it "plus commutative" $ do
      property (plusCommutative :: Int -> Int -> Bool)
    it "product associative" $ do
      property (productAssociative :: Int -> Int -> Int -> Bool)
    it "product commutative" $ do
      property (productCommutative :: Int -> Int -> Bool)
    it "quot rem" $ do
      property prop_quotRemTest
    it "div mod" $ do
      property prop_divModTest
    it "power associative" $ do
      property (powerAssociative :: Int -> Int -> Int -> Bool)
    it "power commutative" $ do
      property (powerCommutative :: Int -> Int -> Bool)
    it "double reverse list" $ do
      property ((\xs -> xs == (reverse . reverse $ xs)) :: [Int] -> Bool)
    it "function application" $ do
      property testFunctionApplication
    it "square identity" $ do
      property ((\x -> squareIdentity x == x) :: Float -> Bool)
