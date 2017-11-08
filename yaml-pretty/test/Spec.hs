import qualified Data.ByteString.Char8                   as BS
import           Data.Char                               (isPrint)
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String
import           Data.Vector                             (Vector)
import qualified Data.Vector                             as Vector
import           Data.Yaml                               (Value (..),
                                                          decodeEither, encode)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

import           Text.Yaml.Pretty

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 100000 } $ forAll (choose (10,100)) $ \width ->
    forAll arbitraryYaml $ \yaml ->
    -- forAll (fmap String arbitraryString) $ \yaml ->
        test yaml width

test :: Value -> Int -> Property
test yaml width = counterexample
    ( unlines
        [ "Expected:"
        , BS.unpack (encode yaml)
        , ""
        , "Actual:"
        , BS.unpack (renderPretty width yaml)
        , either id (BS.unpack . encode) actual ] )
    (actual == decodeEither (encode yaml))
  where
    renderPretty w = BS.pack . renderString . layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine w 1 } . prettyYaml
    actual = decodeEither (renderPretty width yaml) :: Either String Value

arbitraryYaml :: Gen Value
arbitraryYaml = sized $ \size -> frequency
    [ (size, fmap String arbitraryString)
    , (1,    fmap Array  arbitraryArray) ]

arbitraryString :: Gen Text
arbitraryString = fmap Text.pack (listOf (choose ('\0', '\127') `suchThat` isPrint))

arbitraryArray :: Gen (Vector Value)
arbitraryArray = fmap Vector.fromList (listOf arbitraryYaml)
