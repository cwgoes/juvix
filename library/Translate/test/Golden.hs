module Golden where

import qualified Data.ByteString as ByteString (readFile, writeFile)
import Data.ByteString.Char8 (pack)
import qualified Data.Text as Text
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Frontend.Types (TopLevel, extractTopLevel)
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.Silver.Advanced as T

--------------------------------------------------------------------------------
-- Contracts as a file (Golden tests)
--------------------------------------------------------------------------------
contractFiles :: T.TestTree
contractFiles =
  T.testGroup
    "Contract Files"
    [ T.testGroup
        "Contract Files Tests - Golden"
        [ idString,
          addition,
          token
        ]
    ]

resultToText :: Show a => a -> Text
resultToText = Text.pack . show

toByteString :: Show a => a -> ByteString
toByteString = Data.ByteString.Char8.pack . show

parsedContract :: FilePath -> IO [TopLevel]
parsedContract file = do
  rawContract <- ByteString.readFile file
  case Parser.prettyParse rawContract of
    Left err -> writeFile (file <> ".parsed") (toS err) *> pure []
    Right x -> pure $ extractTopLevel x

getGolden :: FilePath -> IO (Maybe [TopLevel])
getGolden file = do
  maybeBS <- T.readFileMaybe file
  return $ do
    bs <- maybeBS
    readMaybe $ Text.unpack $ decodeUtf8 bs

compareParsedGolden :: (Eq a, Show a) => a -> a -> T.GDiff
compareParsedGolden golden parsed
  | parsed == golden =
    T.Equal
  | otherwise =
    T.DiffText
      { T.gReason =
          Just $
            "Parsed output doesn't match golden file."
              <> "The parsed result is \n"
              <> show parsed
              <> "\n but the expected result is \n"
              <> show golden,
        T.gActual = resultToText parsed,
        T.gExpected = resultToText golden
      }

goldenTest :: T.TestName -> FilePath -> T.TestTree
goldenTest name file =
  let goldenFileName = file <> ".golden"
   in T.goldenTest1
        name
        (getGolden goldenFileName)
        (parsedContract file)
        compareParsedGolden
        -- show the golden/actual value, not working atm
        ( T.ShowText . Text.pack
            . const "this isn't doing anything?" -- (Prelude.unlines . map show))
              -- update the golden file, not working atm
        )
        ( ByteString.writeFile goldenFileName
            . const "this isn't either" -- ((encodeUtf8 . Text.pack) . ppShowList))
        )

idString :: T.TestTree
idString = goldenTest "Id-String" "../../test/examples/Id-Strings.ju"

addition :: T.TestTree
addition = goldenTest "Addition" "../../test/examples/Addition.ju"

token :: T.TestTree
token = goldenTest "Token" "../../test/examples/Token.ju"
