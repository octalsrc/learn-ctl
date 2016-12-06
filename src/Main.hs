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

main :: IO ()
main = mainWidget (do el "div" $ text "Welcome to Computer-Aided Verification!"
                      el "div" (do t <- textInput def
                                   let ctl = fmap tryExpand . _textInput_value $ t
                                       path :: (Text,Maybe CTL) -> [Int]
                                       path c = case snd c of
                                                  Just p -> case modelCheck exampleKr p of
                                                              Just p -> p
                                                              Nothing -> []
                                                  Nothing -> []
                                                  
                                       graph :: MonadWidget t m => (Text,Maybe CTL) -> m ()
                                       graph c = exampleSvg (path c)
                                   
                                    
                                   dynText (fmap fst ctl)
                                   simpleSvg
                                   dyn (fmap graph ctl)
                                   return ()))

simpleSvg :: MonadWidget t m => m ()
simpleSvg = el "div" (svgAttr "svg" (fromList [("width","100")
                                              ,("height","100")]) graph)

tryExpand :: Text -> (Text,Maybe CTL)
tryExpand mctl' = if mctl' == ""
                     then ("", Nothing)
                     else case readMaybe (unpack mctl') :: Maybe CTL' of
                            Just ctl' -> ((pack . show . expand) ctl', Just (expand ctl'))
                            Nothing -> ("(not yet a valid CTL formula...)", Nothing)

exampleKr = let states = [(1,map Var "p")
                         ,(2,[])
                         ,(3,map Var "q")
                         ,(4,map Var "q")
                         ,(5,map Var "pq")]
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
  kstate ns 1 "p"   (450,100)
  kstate ns 2 ""    (100,100)
  kstate ns 3 "p q" (120,300)
  kstate ns 4 "q"   (550,250)
  kstate ns 5 "p q" (350,430)

show' :: Int -> Text
show' = pack . show

kstate :: (MonadWidget t m) => Path -> Int -> Text -> (Int,Int) -> m ()
kstate ns i ps (x,y) = circle (fromList as)
  where as = [("cx",show' x),("cy",show' y),("r","40")
             ,("stroke",fst act),("stroke-width",snd act)
             ,("fill","white")] :: [(Text,Text)]
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
