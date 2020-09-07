{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import                               Prelude hiding (head)
import           "base"              Data.Functor
import qualified "containers"        Data.Map as Map
import           "lens"              Control.Lens
import           "data-default"      Data.Default (def)
import           "aeson"             Data.Aeson (toJSON, Value(..))
import           "text"              Data.Text (Text, pack)
import           "reflex-dom"        Reflex.Dom
import           "reflex-utils"      Reflex.Utils
import           "reflex-codemirror" Reflex.CodeMirror
import           "jsaddle"           Language.Javascript.JSaddle -- (JSVal) --  GHCJS.Types (JSVal)

--
main :: IO ()
main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head
            whenLoaded [headD] blank body
            return ()

--
head :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.css"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/theme/zenburn.css"
                     ]
    whenLoaded s1Ds blank $ do
        sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/mode/haskell/haskell.min.js"
                 ]
        return ()

--
body :: MonadWidget t m => m ()
body = do
    marks1ButtonE <- button "push marks 1"
    let marks1E = marks1ButtonE $>
               [ Mark (LineChar 0 2) (LineChar 0 3) (Just $ "css" =: (jsval @JSString "color: blue"))
               , Mark (LineChar 1 2) (LineChar 1 3) (Just $ Map.singleton "css" $ jsval @JSString "color: red")
               ]
    marks2ButtonE <- button "push marks 2"
    let marks2E = marks2ButtonE $>
               [ Mark (LineChar 1 0) (LineChar 1 1) (Just $ "css" =: (jsval @JSString "color: green"))
               , Mark (LineChar 2 1) (LineChar 2 2) (Just $ Map.singleton "css" $ jsval @JSString "color: purple")
               ]
    clickE <- button "goto line 3"
    let lineCharE = (Just $ LineChar 3 1) <$ clickE
    click2E <- button "change text"
    let textE = ("from event" <$ click2E)
    textE <- codemirror config textE lineCharE (leftmost $ [marks1E, marks2E])
    textD <- holdDyn "" textE
    display textD
    where
        config :: Configuration
        config
            = def
            & configuration_value ?~ pack "hello\nworld"
            & configuration_theme ?~ pack "zenburn"
            & configuration_mode  ?~ (String $ pack "haskell")

