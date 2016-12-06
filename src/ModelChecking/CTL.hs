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

modelCheck :: KripkeCTL -> CTL -> Maybe Path
modelCheck (Kripke (is,gr)) ctl = 
  let sats = check gr ctl
      judge i = listToMaybe (filter ((== i) . fst) sats)
  in fmap snd . listToMaybe . catMaybes . map judge $ is

check :: GrCTL -> CTL -> [(Node, Path)]
check gr c = 
  case c of
    Atom v -> satOp opAtom gr v
    Neg c' -> rop (map (\a -> (a,[a]::Path)) . (nodes gr \\) . map fst) gr c'
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

satOp :: OpCTL e -> GrCTL -> e -> [(Node, Path)]
satOp op gr phi = resTN $ satOp' op gr phi

resTN :: [(CtxCTL, Path)] -> [(Node, Path)]
resTN = map (\(a,b) -> (node' a, b))

opAtom :: OpCTL Var
opAtom gr v c = case v of
                  VTrue -> Just [node' c]
                  VFalse -> Nothing
                  v -> if v `elem` lab' c
                          then Just [node' c]
                          else Nothing

sat :: [(Node,Path)] -> CtxCTL -> Maybe Path
sat phi c = (fmap snd . listToMaybe) (filter (\p -> fst p == node' c) phi)

sucEX :: GrCTL -> [(Node,Path)] -> CtxCTL -> [(CtxCTL,Path)]
sucEX gr phi c = catMaybes (map satphi $ succtx gr c)
  where satphi c = case sat phi c of
                     Just p -> Just (c,p)
                     Nothing -> Nothing

opEX :: OpCTL [(Node,Path)]
opEX gr phi c = let res = sucEX gr phi c
                    mkp c' = node' c : (snd c')
                 in (fmap mkp . listToMaybe) res

opEG :: OpCTL [(Node,Path)]
opEG gr phi c =
  case sat phi c of
    Just p -> (fmap (capp p) . listToMaybe) $ findCycles (\c _ -> map fst (sucEX gr phi c)) gr [] c
    Nothing -> Nothing

opEU :: OpCTL ([(Node, Path)],[(Node,Path)])
opEU gr (psi,phi) c = 
  case sat fml c of
    Just p -> (fmap (capp p) . listToMaybe) $ findPaths (end) sucS gr [] c
    Nothing -> Nothing
  where fml = union psi phi
        end c p = sat phi c
        sucS c hist = map fst $ filter (\c -> isNotCycle (fst c) hist) (sucEX gr fml c)

capp :: Path -> Path -> Path
capp p p' = p ++ p'

testPrint :: [(CtxCTL,Path)] -> IO ()
testPrint = putStrLn 
            . concat 
            . map (\s -> show s ++ "\n") . map (\(c,p) -> (node' c,p))

example :: KripkeCTL
example = let nodes = [ (1,[Var 'p'])
                      , (2,[Var 'q']) ]
              edges = [ (1,2)
                      , (1,1)
                      , (2,2) ]
          in mkKripke nodes edges [1]

example2 :: KripkeCTL
example2 = kopk (addLabel (Var 'w') ((kop context) example 2)) example
