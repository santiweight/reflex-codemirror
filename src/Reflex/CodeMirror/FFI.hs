{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.CodeMirror.FFI where

import "lens"      Control.Lens hiding (element, (#))
import "text"      Data.Text (Text)
import "aeson"     Data.Aeson (toJSON)
import "jsaddle"   Language.Javascript.JSaddle
import             GHCJS.DOM.Element (IsElement, toElement, unElement)
import             Reflex.CodeMirror.Types
import Control.Monad (void)

newtype CodeMirrorRef = CodeMirrorRef
                      { unCodeMirrorRef :: JSVal
                      }

--
fromTextArea :: (IsElement element)
             => element
             -- ^ Element to use
             -> Configuration
             -- ^ CodeMirror configuration
             -> JSM CodeMirrorRef
fromTextArea element_
             config = do
    let js_element = unElement . toElement $ element_
    let js_config  = toJSON $ config
    ref <- nextAnimationFrame $ \_ -> (jsg "CodeMirror") # "fromTextArea" $ (js_element, js_config)
    return $ CodeMirrorRef ref


registerOnChange :: CodeMirrorRef
                 -- ^ ref
                 -> (Text-> JSM ())
                 -- ^ Callback with Text
                 -> JSM ()
registerOnChange codeMirrorRef callback = do
    codemirror <- valToObject . unCodeMirrorRef $ codeMirrorRef
    _ <- codemirror ^. js2 "on" "change" (fun $ \_ _ _ -> do
        x <- codemirror ^. js0 "getValue"
        t <- valToText x
        -- _ <- valToText =<< codemirror ^. js0 "getValue"
        callback t)
    return ()

setValueAndRefresh :: CodeMirrorRef
                   -- ^ ref
                   -> Text
                    -- ^ value
                     -> JSM ()
setValueAndRefresh ref text = do
        codemirror <- valToObject . unCodeMirrorRef $ ref
        setValue codemirror
        refreshMirror codemirror
    where
        setValue :: Object -> JSM ()
        setValue codemirror = void $ codemirror ^. js1 "setValue" text
        refreshMirror :: Object -> JSM ()
        refreshMirror codemirror = do
            let refreshFun = fun (\_ _ _ -> void $ codemirror ^. js0 "refresh")
            _ <- jsg2 "setTimeout" refreshFun (1 :: Double)
            return ()

scrollIntoView :: CodeMirrorRef
               -- ^ ref
               -> LineChar
               -- ^ Line & Char
               -> Int
               -- ^ delay ms
               -> JSM ()
scrollIntoView ref lc delay = do
    codemirror <- valToObject . unCodeMirrorRef $ ref
    let js_lc  = toJSON $ lc
    _ <- codemirror ^. js2 "scrollIntoView" js_lc delay
    return ()
