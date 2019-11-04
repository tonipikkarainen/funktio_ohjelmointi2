module Main where
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Profunctor
import Data.Void

main :: IO ()
main = do
  putStrLn "hello world"
