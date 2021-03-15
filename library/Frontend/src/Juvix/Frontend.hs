module Juvix.Frontend where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as Types
import Juvix.Library
  ( (.),
    Applicative (pure),
    Either (..),
    FilePath,
    Functor (fmap),
    IO,
    Traversable (sequenceA, traverse),
    intern,
  )
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Parser (ParserError)
import qualified System.FilePath as FilePath
import Prelude (String)

-- we abuse laziness here
-- TODO ∷ add directory option
-- this will add top level to the thing, and properly handle paths
ofPath :: [FilePath] -> IO (Either ParserError [(NameSymbol.T, [Types.TopLevel])])
ofPath =
  -- fmap gets through the IO, so that sequenceA flips the either and list
  fmap sequenceA . traverse ofSingleFile

ofSingleFile :: FilePath -> IO (Either ParserError (NameSymbol.T, [Types.TopLevel]))
ofSingleFile file = do
  read <- ByteString.readFile file
  case Parser.parse read of
    Left x ->
      pure (Left x)
    Right (Types.Header name xs) ->
      pure (Right (name, xs))
    Right (Types.NoHeader xs) ->
      let toName =
            NameSymbol.fromSymbol . intern . toUpper . FilePath.takeBaseName
       in pure (Right (toName file, xs))

toUpper :: String -> String
toUpper (x : xs) = Char.toUpper x : xs
toUpper [] = []
