import           Control.Applicative                   (liftA2)
import qualified Data.ByteString.Char8                 as BS
import           Data.Char                             (isPrint)
import           Data.HashMap.Lazy                     (HashMap)
import qualified Data.HashMap.Lazy                     as Map
import           Data.Scientific
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Encoding                    (decodeUtf8, encodeUtf8)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Vector                           (Vector)
import qualified Data.Vector                           as Vector
import           Data.Yaml                             (Value (..),
                                                        decodeEither, encode)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

import           Text.Yaml.Pretty

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 100000 } $
      forAll arbitraryString $ \yamlString ->
      test (String yamlString) (Text.length yamlString * 2)
  quickCheckWith stdArgs { maxSuccess = 100000 } $
      forAll (choose (10,100)) $ \width ->
      forAll arbitraryYaml $ \yaml ->
      test yaml width

test :: Value -> Int -> Property
test yaml width = counterexample
    ( unlines
        [ "Expected:"
        , Text.unpack (decodeUtf8 (encode yaml))
        , ""
        , "Actual:"
        , Text.unpack (renderPretty width yaml)
        , either id (Text.unpack . decodeUtf8 . encode) actual ] )
    (actual == decodeEither (encode yaml))
  where
    renderPretty w = renderStrict . layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine w 1 } . prettyYaml
    actual = decodeEither (encodeUtf8 (renderPretty width yaml)) :: Either String Value

arbitraryYaml :: Gen Value
arbitraryYaml = sized $ \size -> frequency
    [ (3 * size, fmap String arbitraryString)
    , (size,     fmap Number arbitraryNumber)
    , (1,        fmap Array  arbitraryArray)
    , (1,        fmap Object arbitraryObject) ]

arbitraryString :: Gen Text
arbitraryString = fmap Text.pack $ listOf $ frequency
    [ (9, choose ('\0', '\127') `suchThat` isPrint)
    , (1,   arbitrary `suchThat` isPrint) ]

arbitraryNumber :: Gen Scientific
arbitraryNumber = liftA2 scientific arbitrary arbitrary

arbitraryArray :: Gen (Vector Value)
arbitraryArray = fmap Vector.fromList (listOf arbitraryYaml)

arbitraryObject :: Gen (HashMap Text Value)
arbitraryObject = fmap Map.fromList $ listOf $ do
    key <- arbitraryString
    value <- arbitraryYaml
    pure (key, value)
