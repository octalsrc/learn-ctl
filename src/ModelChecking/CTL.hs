module ModelChecking.CTL where

import Data.Map (fromList)
import Data.Maybe (listToMaybe, isJust, catMaybes)

import Data.Logic.Propositional
import Data.Graph.Inductive.Graph
import Data.Graph.Kripke

type KripkeCTL = Kripke Var
type GrCTL = KrGr Var
type CtxCTL = KrCtx Var

nodeMap :: Expr -> CtxCTL -> Mapping
nodeMap e c = let vs = variables e
                  ps = lab' c
                  f v = (v, elem v ps)
              in fromList (map f vs)

data CTL = Atom Expr
         | Neg CTL
         | Con CTL CTL
         | Dis CTL CTL
         | EX CTL
         | EU CTL CTL
         | EG CTL

check :: KripkeCTL -> CTL -> [Node]
check (Kripke (is,gr)) ctl = 
  let sats = nodeVals $ checkNode gr ctl
  in filter (not . flip elem sats) is

nodeVals :: (GrCTL, Expr) -> [Node]
nodeVals = undefined  

checkNode :: GrCTL -> CTL -> (GrCTL, Expr)
checkNode gr ctl = 
  case ctl of
    Atom e -> (gr,e)
    Neg     c -> po1 Negation gr c
    Con c1 c2 -> po2 Conjunction gr c1 c2
    Dis c1 c2 -> po2 Disjunction gr c1 c2


po1 :: (Expr -> a) -> GrCTL -> CTL -> (GrCTL, a)
po1 f gr c = let (gr', e) = checkNode gr c
             in (gr',f e)

po2 :: (Expr -> Expr -> Expr) -> GrCTL -> CTL -> CTL -> (GrCTL, Expr)
po2 f gr c1 c2 = let (gr', f') = po1 f gr c1
                 in po1 f' gr' c2

satPhi :: Expr -> CtxCTL -> Bool
satPhi exp c = interpret exp (nodeMap exp c)

type OpCTL e = GrCTL -> e -> CtxCTL -> Maybe Path

satOp :: OpCTL e -> KripkeCTL -> e -> [(CtxCTL, Path)]
satOp op kr exp = catMaybes $ map runOp (kop contexts kr)
  where runOp c = fmap (\path -> (c,path)) (op (krGr kr) exp c)

satAtom :: OpCTL Expr
satAtom gr exp c = if satPhi exp c
                      then Just [node' c]
                      else Nothing

sucEX :: GrCTL -> Expr -> CtxCTL -> [CtxCTL]
sucEX gr exp c = filter (satPhi exp) (succtx gr c)

satEX :: OpCTL Expr
satEX gr exp c = let res = sucEX gr exp c
                     mkp c' = [node' c, node' c']
                 in (fmap mkp . listToMaybe) res

satEG :: OpCTL Expr
satEG gr exp c = 
  if satPhi exp c
     then listToMaybe $ findCycles (\c _ -> sucEX gr exp c) gr [] c
     else Nothing

satEU :: OpCTL (Expr,Expr)
satEU gr (psi,phi) c = 
  if satPhi fml c
     then listToMaybe $ findPaths end sucS gr [] c
     else Nothing
  where fml = Disjunction psi phi
        end c = const (satPhi phi c)
        sucS c hist = filter (\c -> isNotCycle c hist) (sucEX gr fml c)

testPrint :: [(CtxCTL,Path)] -> IO ()
testPrint = putStrLn 
            . concat 
            . map (\s -> show s ++ "\n") . map (\(c,p) -> (node' c,p))

example :: KripkeCTL
example = let nodes = [ (1,[Var 'p'])
                      , (2,[Var 'q']) ]
              edges = [ (1,2,())
                      , (1,1,())
                      , (2,2,()) ]
          in mkKripke (mkGraph nodes edges) [1]

example2 :: KripkeCTL
example2 = kopk (addLabel (Var 'w') ((kop context) example 2)) example
