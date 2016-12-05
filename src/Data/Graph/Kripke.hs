module Data.Graph.Kripke where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Logic.Propositional

succtx :: (Graph gr) => gr a b -> Context a b -> [Context a b]
succtx g = map (context g) . suc g . node'

contexts :: (Graph gr) => gr a b -> [Context a b]
contexts gr = map (context gr) (nodes gr)

type KrGr = Gr [Var] ()

type KrCtx = Context [Var] ()

newtype Kripke = Kripke { getKripke :: ([Int], KrGr) }

krGr = snd . getKripke

mkKripke :: KrGr -> [Int] -> Kripke
mkKripke gr is = Kripke (is,gr)

example :: Kripke
example = let nodes = [ (1,[Var 'p'])
                      , (2,[Var 'q']) ]
              edges = [ (1,2,())
                      , (1,1,())
                      , (2,2,()) ]
          in mkKripke (mkGraph nodes edges) [1]
          
kop :: (KrGr -> a) -> Kripke -> a
kop f (Kripke (_,g)) = f g

findCycles :: (Monad m) => (KrCtx -> m KrCtx) -> KrGr -> Path -> KrCtx -> m Path
findCycles p gr hist c = 
  if elem this hist
     then return (reverse hist')
     else p c >>= findCycles p gr hist'
  where this = node' c
        hist' = this : hist
