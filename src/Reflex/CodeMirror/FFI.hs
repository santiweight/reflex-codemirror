{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Reflex.CodeMirror.FFI where

import "base"       Control.Monad (void, forM_)
import "lens"       Control.Lens hiding (element, (#))
import "containers" Data.Map
import "text"       Data.Text (Text)
import "aeson"      Data.Aeson (toJSON)
import "jsaddle"    Language.Javascript.JSaddle
import              GHCJS.DOM.Element (IsElement, toElement, unElement)
import              Reflex.CodeMirror.Types

newtype CodeMirrorRef = CodeMirrorRef
                      { unCodeMirrorRef :: JSVal
                      }

newtype CMMarks = CMMarks { unCMMarks :: Object }

newtype CMLoc = CMLoc { unCMLoc :: Object }

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
        refreshMirror ref
    where
        setValue :: Object -> JSM ()
        setValue codemirror = void $ codemirror ^. js1 "setValue" text

setMarks :: CodeMirrorRef -> CMMarks -> [Mark] -> JSM Object
setMarks ref cmMarks marks = do
    -- Remove old marks
    let cmMarksArray = unCMMarks cmMarks
    _ <- cmMarksArray ^. js1 "forEach" (eval "(function(marker) { marker.clear(); })")
    -- Track the new marks, so that they can be referenced for clearing
    newMarksArray <- array @[Int] []
    forM_ marks $ \(Mark start end markOptionsMay) -> do
        -- Activate each mark
        startLoc <- makeLocFromPos start
        endLoc <- makeLocFromPos end
        options_ <- getJSOptionsObj markOptionsMay
        mark <- unCodeMirrorRef ref
                ^. js3 "markText" (unCMLoc startLoc) (unCMLoc endLoc) options_
        newMarksArray ^. js1 "push" mark
    refreshMirror ref
    pure newMarksArray
  where
      getJSOptionsObj :: Maybe (Map Text JSVal) -> JSM Object
      getJSOptionsObj markOptionsMay = do
        options_ <- obj
        case markOptionsMay of
            Nothing -> pure ()
            Just markOptions -> forM_ (assocs markOptions) $ \(optKey, optVal) -> (options_ <# optKey) optVal
        pure options_
      makeLocFromPos :: LineChar -> JSM CMLoc
      makeLocFromPos (LineChar line ch) = makeLoc line ch
      makeLoc :: Int -> Int -> JSM CMLoc
      makeLoc line ch = do
          locObj <- obj
          (locObj <# "line") line
          (locObj <# "ch") ch
          pure (CMLoc locObj)

refreshMirror :: CodeMirrorRef -> JSM ()
refreshMirror (unCodeMirrorRef -> ref) = do
    codemirror <- valToObject ref
    let refreshFun = fun (\_ _ _ -> void $ codemirror ^. js0 "refresh")
    _ <- jsg2 "setTimeout" refreshFun (100 :: Double)
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
