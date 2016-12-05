module ModelChecking.CTL where

import Data.Map (fromList)
import Data.Maybe (listToMaybe, isJust, catMaybes)

import Data.Logic.Propositional
import Data.Graph.Inductive.Graph
import Data.Graph.Kripke

nodeMap :: Expr -> KrCtx -> Mapping
nodeMap e c = let vs = variables e
                  ps = lab' c
                  f v = (v, elem v ps)
              in fromList (map f vs)

data CTL = Atom Expr
         | Con CTL CTL
         | Dis CTL CTL
         | Neg CTL
         | EX CTL
         | EU CTL CTL
         | EG CTL 

check :: Kripke -> CTL -> Maybe [Int]
check = undefined

satPhi :: Expr -> KrCtx -> Bool
satPhi exp c = interpret exp (nodeMap exp c)

type OpCTL = KrGr -> Expr -> KrCtx -> Maybe Path

satOp :: OpCTL -> Kripke -> Expr -> [(KrCtx, Path)]
satOp op kr exp = catMaybes $ map runOp (kop contexts kr)
  where runOp c = fmap (\path -> (c,path)) (op (krGr kr) exp c)

sucEX :: KrGr -> Expr -> KrCtx -> [KrCtx]
sucEX gr exp c = filter (satPhi exp) (succtx gr c)

satEX :: OpCTL
satEX gr exp c = let res = sucEX gr exp c
                     mkp c' = [node' c, node' c']
                 in (fmap mkp . listToMaybe) res

satEG :: OpCTL
satEG gr exp c = 
  if satPhi exp c
     then listToMaybe $ findCycles (sucEX gr exp) gr [] c
     else Nothing

satEU :: OpCTL
satEU = undefined
