module Data.Graph.Kripke where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

succtx :: (Graph gr) => gr a b -> Context a b -> [Context a b]
succtx g = map (context g) . suc g . node'

contexts :: (Graph gr) => gr a b -> [Context a b]
contexts gr = map (context gr) (nodes gr)

type KrGr v = Gr [v] ()

type KrCtx v = Context [v] ()

newtype Kripke v = Kripke { getKripke :: ([Int], KrGr v) }

krGr = snd . getKripke

mkKripke :: KrGr v -> [Int] -> Kripke v
mkKripke gr is = Kripke (is,gr)

kop :: (KrGr v -> a) -> Kripke v -> a
kop f (Kripke (_,g)) = f g

findPaths :: (Monad m) 
          => (KrCtx v -> Path -> Bool) 
          -> (KrCtx v -> Path -> m (KrCtx v))
          -> KrGr v -> Path -> KrCtx v 
          -> m Path
findPaths end sucS gr hist c = 
  if end c hist
     then return (reverse hist')
     else sucS c hist >>= findPaths end sucS gr hist'
  where this = node' c
        hist' = this : hist

isCycle :: KrCtx v -> Path -> Bool
isCycle c = elem (node' c)

isNotCycle :: KrCtx v -> Path -> Bool
isNotCycle c = not . isCycle c

findCycles :: (Monad m) 
           => (KrCtx v -> Path -> m (KrCtx v)) 
           -> KrGr v -> Path -> KrCtx v 
           -> m Path
findCycles = findPaths isCycle
