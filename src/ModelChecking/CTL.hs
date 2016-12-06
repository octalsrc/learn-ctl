module ModelChecking.CTL where

import Data.Map (fromList)
import Data.List (union, intersect, (\\), nub)
import Data.Maybe (listToMaybe, isJust, catMaybes)

import Data.Graph.Inductive.Graph
import Data.Graph.Kripke

data Var = Var Char | VTrue | VFalse deriving (Show, Read, Eq, Ord)

type KripkeCTL = Kripke Var
type GrCTL = KrGr Var
type CtxCTL = KrCtx Var

data CTL' = Atom' Var
          | Neg' CTL'
          | Con' CTL' CTL'
          | Dis' CTL' CTL'
          
          | EX' CTL'
          | EG' CTL'
          | EU' CTL' CTL'
          
          | EF' CTL'
          | AX' CTL'
          | AG' CTL'
          | AF' CTL'
          
          | AU' CTL' CTL'
          | AR' CTL' CTL'
          | ER' CTL' CTL'

          deriving (Show, Read, Eq, Ord)

vtrue = Atom VTrue

expand :: CTL' -> CTL
expand ctl = 
  case ctl of
    Atom' v -> Atom v
    Neg' c -> r c Neg
    Con' c1 c2 -> r2 c1 c2 Con
    Dis' c1 c2 -> r2 c1 c2 Dis
    EX' c -> r c EX
    EG' c -> r c EG
    EU' c1 c2 -> r2 c1 c2 EU

    EF' c -> r c (\c' -> EU vtrue c')
    AX' c -> r c (\c' -> Neg (EX (Neg c')))
    AG' c -> expand (Neg' (EF' (Neg' c)))
    AF' c -> r c (\c' -> Neg (EG (Neg c')))
    
    AU' c1 c2 -> r2 c1 c2 (\c1' c2' -> Neg (Dis (EU (Neg c2') (Neg (Dis c2' c1'))) (EG (Neg c2'))))
    AR' c1 c2 -> r2 c1 c2 (\c1' c2' -> Neg (EU (Neg c1') (Neg c2')))
    ER' c1 c2 -> expand (Neg' (AU' (Neg' c1) (Neg' c2)))
  where r c f = f (expand c)
        r2 c1 c2 f = f (expand c1) (expand c2)

atom :: Char -> CTL
atom = Atom . Var

data CTL = Atom Var
         | Neg CTL
         | Con CTL CTL
         | Dis CTL CTL
         
         | EX CTL
         | EG CTL
         | EU CTL CTL
         
         deriving (Show, Read, Eq, Ord)

modelCheck :: KripkeCTL -> CTL -> [Node]
modelCheck (Kripke (is,gr)) ctl = 
  let sats = check gr ctl
  in filter (not . flip elem sats) is

check :: GrCTL -> CTL -> [Node]
check gr c = 
  case c of
    Atom v -> satOp opAtom gr v
    Neg c' -> rop (nodes gr \\) gr c'
    Con c1' c2' -> rop2 intersect gr c1' c2'
    Dis c1' c2' -> rop2 union gr c2' c2'
    EX c' -> rop (satOp opEX gr) gr c'
    EG c' -> rop (satOp opEG gr) gr c'
    EU c1' c2' -> rop2 (\a b -> satOp opEU gr (a,b)) gr c1' c2'

rop f gr c' = nub $ f (check gr c')
rop2 f2 gr c1 c2 = nub $ f2 (check gr c1) (check gr c2)

type OpCTL e = GrCTL -> e -> CtxCTL -> Maybe Path

satOp' :: OpCTL e
       -> GrCTL
       -> e 
       -> [(CtxCTL, Path)]
satOp' op gr phi = catMaybes $ map runOp (contexts gr)
  where runOp c = fmap (\path -> (c,path)) (op gr phi c)

satOp op kr phi = resTN $ satOp' op kr phi

resTN :: [(CtxCTL, Path)] -> [Node]
resTN = map node' . map fst

opAtom :: OpCTL Var
opAtom gr v c = case v of
                  VTrue -> Just [node' c]
                  VFalse -> Nothing
                  v -> if v `elem` lab' c
                          then Just [node' c]
                          else Nothing

sat :: [Node] -> CtxCTL -> Bool
sat phi = flip elem phi . node'

sucEX :: GrCTL -> [Node] -> CtxCTL -> [CtxCTL]
sucEX gr phi c = filter (sat phi) (succtx gr c)

opEX :: OpCTL [Node]
opEX gr phi c = let res = sucEX gr phi c
                    mkp c' = [node' c, node' c']
                 in (fmap mkp . listToMaybe) res

opEG :: OpCTL [Node]
opEG gr phi c = 
  if sat phi c
     then listToMaybe $ findCycles (\c _ -> sucEX gr phi c) gr [] c
     else Nothing

opEU :: OpCTL ([Node],[Node])
opEU gr (psi,phi) c = 
  if sat fml c
     then listToMaybe $ findPaths end sucS gr [] c
     else Nothing
  where fml = union psi phi
        end c = const (sat phi c)
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
