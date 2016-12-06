{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import Data.Map (Map, fromList)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)

import ModelChecking.CTL
import Data.Graph.Inductive.Graph (Path)
import Data.Graph.Kripke
import Reflex.Dom

fullWidth :: Map Text Text
fullWidth = fromList [("style","width:100%; font-size: x-large;")]

main :: IO ()
main = mainWidget (do el "div" (do elAttr "p" fullWidth (text "Checking the negation of:") 
                                   t <- textInput $ def & textInputConfig_attributes .~ (constDyn fullWidth)
                                   elAttr "div" (fromList [("style","height: 30px;")]) blank
                                   let ctl = fmap tryExpand . _textInput_value $ t
                                       path :: (Text,Maybe CTL) -> [Int]
                                       path c = case snd c of
                                                  Just p -> case modelCheck exampleKr p of
                                                              Just p -> p
                                                              Nothing -> []
                                                  Nothing -> []
                                                  
                                       graph :: MonadWidget t m => (Text,Maybe CTL) -> m ()
                                       graph c = exampleSvg (path c)
                                   
                                    
                                   elAttr "div" (fromList [("style","font-size: x-large; height: 30px;")]) (dynText (fmap fst ctl))
                                   elAttr "div" (fromList [("style","font-size: x-large; height: 60px;")]) (dynText (fmap (decide . path) ctl))
                                   dyn (fmap graph ctl)
                                   return ()))

decide [] = "SAT"
decide as = pack ("UNSAT: " ++ show as)

simpleSvg :: MonadWidget t m => m ()
simpleSvg = el "div" (svgAttr "svg" (fromList [("width","100")
                                              ,("height","100")]) graph)

tryExpand :: Text -> (Text,Maybe CTL)
tryExpand mctl' = if mctl' == ""
                     then ("", Nothing)
                     else case readMaybe (unpack mctl') :: Maybe CTL' of
                            Just ctl' -> ((pack . ("Expanded form: " ++). show . Neg . expand) ctl', Just (expand ctl'))
                            Nothing -> ("(not yet a valid CTL formula...)", Nothing)

exampleKr = let states = [(1,map Var "p")
                         ,(2,map Var "st")
                         ,(3,map Var "q")
                         ,(4,map Var "qr")
                         ,(5,map Var "s")]
                edges = [(1,2),(1,3),(1,4)
                        ,(2,2),(2,1)
                        ,(3,2)
                        ,(5,5),(5,4)]
                initStates = [1,5]
            in mkKripke states edges initStates

svgSize :: [(Text,Text)]
svgSize = [("width","600"),("height","500")]

exampleSvg :: MonadWidget t m => [Int] -> m ()
exampleSvg ns = svgAttr "svg" (fromList svgSize) $ do 
  kloop ns (2,2) (60,60)
  kloop ns (5,5) (350,445)

  kstate ns 1 "p"   (380,100)
  kstate ns 2 "s t" (100,100)
  kstate ns 3 "q"   (120,300)
  kstate ns 4 "q r" (430,280)
  kstate ns 5 "s"   (300,430)
  
  kedge ns (0,1) "M 520 20 L 425 65" (425,65)
  kedge ns (0,5) "M 190 480 L 245 450" (245,450)
  
  kedge ns (3,2) "M 110 250 L 105 163" (105,163)
  kedge ns (1,3) "M 340 130 L 162 252" (162,252)
  kedge ns (5,4) "M 332 380 L 390 320" (390,320)
  kedge ns (2,1) "M 140  60 L 340  60" (340,60)
  kedge ns (1,2) "M 150  80 L 330  80" (150,80)
  kedge ns (1,4) "M 400 145 L 420 220" (420,220)

kedge :: MonadWidget t m => [Int] -> (Int,Int) -> Text -> (Int,Int) -> m ()
kedge ns (a,b) draw end = svgpath draw as >> circle (fromList (as ++ cs))
  where as = if (thisEdge ns (a,b))
                then [("stroke","red"),("stroke-width","5"),("fill","red")]
                else [("stroke","blue"),("stroke-width","3")]
        cs = [("cx",show' (fst end)),("cy",show' (snd end)),("r","6")]

kloop :: MonadWidget t m => [Int] -> (Int,Int) -> (Int,Int) -> m ()
kloop ns (a,b) (x,y) = circle (fromList as)
  where as = [("cx",show' x),("cy",show' y),("r","35"),("fill","white")]
             ++ if thisEdge ns (a,b)
                   then [("stroke","red"),("stroke-width","5")]
                   else [("stroke","blue"),("stroke-width","3")]

thisEdge :: [Int] -> (Int,Int) -> Bool
thisEdge [] _ = False
thisEdge ps (i,i') = if i == 0
                        then head ps == i'
                        else find (i,i') ps
  where find (a,b) (c:d:es) = if a == c && b == d
                                 then True
                                 else find (a,b) (d:es)
        find _ _ = False

show' :: Int -> Text
show' = pack . show

kstate :: (MonadWidget t m) => [Int] -> Int -> Text -> (Int,Int) -> m ()
kstate ns i ps (x,y) = circle (fromList as) 
                       >> svgtext (show' i) (fromList ts) 
                       >> svgtext ps (fromList ts')
  where as = [("cx",show' x),("cy",show' y),("r","40")
             ,("stroke",fst act),("stroke-width",snd act)
             ,("fill","white")] :: [(Text,Text)]
        ts  = [("x",show' (x - 12)),("y",show' (y + 17)),("font-size","45")]
        ts' = [("x",show' (x + 55)),("y",show' (y + 20)),("font-size","35")]
        act = if i `elem` ns
                 then ("red","5") :: (Text,Text)
                 else ("black","3") :: (Text,Text)

graph :: (MonadWidget t m) => m ()
graph = circle (fromList [("cx","50")
                         ,("cy","50")
                         ,("r","40")
                         ,("stroke","green")
                         ,("stroke-width","4")])

circle :: (MonadWidget t m) => Map Text Text -> m ()                                    
circle attrs = svgAttr "circle" attrs blank

svgpath :: (MonadWidget t m) => Text -> [(Text,Text)] -> m ()                                    
svgpath draw attrs = svgAttr "path" (fromList (("d",draw) : attrs)) blank

svgtext :: (MonadWidget t m) => Text -> Map Text Text -> m ()                                    
svgtext t attrs = svgAttr "text" attrs (text t)


svgDynAttr' :: forall t m a. MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
svgDynAttr' = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

svgDynAttr :: forall t m a. MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
svgDynAttr elementTag attrs child = snd <$> svgDynAttr' elementTag attrs child

svgAttr' :: forall t m a. MonadWidget t m => Text -> Map Text Text -> m a -> m (El t, a)
svgAttr' elementTag attrs child = svgDynAttr' elementTag (constDyn attrs) child

svgAttr :: forall t m a. MonadWidget t m => Text -> Map Text Text -> m a -> m a
svgAttr elementTag attrs child = svgDynAttr elementTag (constDyn attrs) child

svg' :: forall t m a. MonadWidget t m => Text -> m a -> m (El t, a)
svg' elementTag child = svgAttr' elementTag (Map.empty :: AttributeMap) child

svg :: forall t m a. MonadWidget t m => Text -> m a -> m a
svg elementTag child = svgAttr elementTag Map.empty child

svgClass :: forall t m a. MonadWidget t m => Text -> Text -> m a -> m a
svgClass elementTag c child = svgAttr elementTag ("class" =: c) child
