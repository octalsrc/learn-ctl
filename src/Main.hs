{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)

import Data.Graph.Kripke
import Reflex.Dom

main :: IO ()
main = mainWidget (do el "div" $ text "Welcome to Computer-Aided Verification!"
                      el "div" (do t <- textInput def
                                   dynText $ _textInput_value t)
                      el "div" (svgAttr "svg" (Map.fromList [("width","100")
                                                            ,("height","100")]) graph))

graph :: (MonadWidget t m) => m ()
graph = circle (Map.fromList [("cx","50")
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
