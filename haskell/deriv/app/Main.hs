module Main where

import qualified Derive as D
import qualified FDerive as F

main :: IO ()
main = do {
    print $ D.match (D.Concat (D.Character 'a') (D.Character 'b')) "ab";
    print $ F.match (F.concat (F.character 'a') (F.character 'b')) "ab";
}
