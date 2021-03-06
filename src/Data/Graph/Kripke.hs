module Data.Graph.Kripke where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

succtx :: (Graph gr) => gr a b -> Context a b -> [Context a b]
succtx g = map (context g) . suc g . node'

contexts :: (Graph gr) => gr a b -> [Context a b]
contexts gr = map (context gr) (nodes gr)

type KrGr v = Gr [v] ()

type KrCtx v = Context [v] ()

newtype Kripke v = Kripke { getKripke :: ([Node], KrGr v) }

krGr = snd . getKripke

mkKripke :: [(Node,[v])] -> [(Node,Node)] -> [Node] -> Kripke v
mkKripke ns es is = Kripke (is,mkGraph ns (map (\(a,b) -> (a,b,())) es))

kop :: (KrGr v -> a) -> Kripke v -> a
kop f (Kripke (_,g)) = f g

kopk :: (KrGr v -> KrGr v) -> Kripke v -> Kripke v
kopk f (Kripke (is,g)) = Kripke (is,f g)

findPaths :: (Monad m) 
          => (KrCtx v -> Path -> Maybe Path)
          -> (KrCtx v -> Path -> m (KrCtx v))
          -> KrGr v -> Path -> KrCtx v 
          -> m Path
findPaths end sucS gr hist c =
  case end c hist of
    Just p -> return (reverse hist' ++ p)
    Nothing -> sucS c hist >>= findPaths end sucS gr hist'
  where this = node' c
        hist' = this : hist
        
isCycle :: KrCtx v -> Path -> Maybe Path
isCycle c p = if elem (node' c) p
                 then Just [node' c]
                 else Nothing

isNotCycle :: KrCtx v -> Path -> Bool
isNotCycle c p = case isCycle c p of
                   Just _ -> False
                   Nothing -> True

findCycles :: (Monad m) 
           => (KrCtx v -> Path -> m (KrCtx v)) 
           -> KrGr v -> Path -> KrCtx v 
           -> m Path
findCycles = findPaths isCycle

addLabel :: v -> KrCtx v -> KrGr v -> KrGr v
addLabel v (p,n,vs,s) gr = (p,n,v:vs,s) & gr
