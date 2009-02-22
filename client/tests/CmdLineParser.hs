import Test.QuickCheck
import App.Persistent.Client
import Data.Char

instance Arbitrary Char where
    arbitrary = choose ('a', 'b')

main = do
  test prop_allArgsSelected
  test prop_noArgsSelected

prop_allArgsSelected xs = xs == (fst $ parseCmdLine ("+PC" : xs ++ ["-PC"]))
prop_noArgsSelected xs = xs == (snd $ parseCmdLine xs)
