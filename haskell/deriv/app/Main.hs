module Main where

import qualified Derive as D
import qualified FDerive as F
import qualified FDeriveTree as T

import qualified Data.Tree as Tree

main :: IO ()
main = do {
    print $ D.match (D.Concat (D.Character 'a') (D.Character 'b')) "ab";
    print $ F.match (F.concat (F.character 'a') (F.character 'b')) "ab";
    print $ T.match (T.node "a" (T.concat (T.node "b" T.empty) (T.node "c" (T.node "d" T.empty)))) (Tree.Node "a" [Tree.Node "b" [], Tree.Node "c" [Tree.Node "d" []]]); 
}
