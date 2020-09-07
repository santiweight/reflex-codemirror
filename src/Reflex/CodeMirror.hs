{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
module Reflex.CodeMirror ( module Reflex.CodeMirror.Types
                         , module Reflex.CodeMirror.FFI
                         , codemirror
                         ) where
import "base"             Control.Monad.IO.Class (MonadIO, liftIO)
import "base"             Data.IORef (IORef, newIORef, writeIORef, readIORef)
import "lens"             Control.Lens ((^.))
import "text"             Data.Text (Text)
import "jsaddle"          Language.Javascript.JSaddle
import "reflex-dom"       Reflex.Dom hiding (setValue)
import                    GHCJS.DOM.Element hiding (scrollIntoView) -- (IsElement)
import                    Reflex.CodeMirror.FFI
import                    Reflex.CodeMirror.Types hiding (configuration)

codemirror :: forall t m.
           (DomBuilder t m,
            MonadIO m,
            PostBuild t m,
            TriggerEvent t m,
            MonadJSM m,
            PerformEvent t m,
            MonadIO (Performable m),
            IsElement (RawElement (DomBuilderSpace m))
           )
           => Configuration
           -> Event t Text
           -> Event t (Maybe LineChar)
           -> Event t [Mark]
           -> m (Event t Text)
codemirror configuration textE scrollToE marksE = do
    -- HTML element
    (element_, _) <- el' "textarea" $ text $ maybe "" id (_configuration_value configuration)

    -- local state
    (ref :: IORef (Maybe CodeMirrorRef)) <- liftIO $ newIORef Nothing

    jsCtx <- askJSM
    marksArray <- liftIO $ flip runJSM jsCtx $ CMMarks <$> array @[Int] []
    marksRef :: IORef CMMarks <- liftIO . newIORef $ marksArray

    -- input event
    (postBuildTaggedE :: Event t ()) <- getPostBuild
    let inputE = leftmost
               [ (Nothing, Nothing)        <$  postBuildTaggedE
               , (\x -> (Nothing,x))       <$> scrollToE
               , (\x -> (Just x, Nothing)) <$> textE
               ]

    -- output event + trigger
    (outE :: Event t Text, triggerOut) <- newTriggerEvent

    -- handle input event
    jsCtx' <- askJSM
    performEvent_ $ ffor inputE $ \(mText, mScrollTo) -> flip runJSM jsCtx' $ handle (_element_raw element_)
                                                             ref
                                                             triggerOut
                                                             configuration
                                                             mText
                                                             mScrollTo

    -- handle marks event
    jsCtx'' <- askJSM
    performEvent_ $ ffor marksE (\marks -> flip runJSM jsCtx'' $ handleMarks ref marksRef marks)

    return outE

    where
        handle :: (IsElement el)
               => el
               -- ^ Element
               -> IORef (Maybe CodeMirrorRef)
               -- ^ Local state
               -> (Text -> IO ())
               -- ^ Trigger for output event
               -> Configuration
               -- ^ Chart data
               -> (Maybe Text)
               -- ^ Text
               -> (Maybe LineChar)
               -- ^ Scroll to
               -> JSM ()
        handle element_ ref trigger configuration_ mText mScrollTo = do
            currentRef_ <- liftIO $ readIORef ref
            case currentRef_ of
                Nothing   -> onFirstTime element_ ref trigger configuration_ mText mScrollTo
                Just ref_ -> onNextTime  ref_                 configuration_ mText mScrollTo

        handleMarks :: IORef (Maybe CodeMirrorRef)
                    -- ^ Local state
                    -> IORef CMMarks
                    -- ^ Active Marks state
                    -> [Mark]
                    -- ^ Marks to activate
                    -> JSM ()
        handleMarks ref marksRef marks = do
            currentRef_ <- liftIO $ readIORef ref
            marksArr <- liftIO $ readIORef marksRef
            case currentRef_ of
                Nothing   -> pure ()
                Just ref_ -> do
                    newMarks <- setMarks ref_ marksArr marks
                    liftIO $ writeIORef marksRef $ CMMarks newMarks

        onFirstTime :: (IsElement el)
                    => el
                    -- ^ Element
                    -> IORef (Maybe CodeMirrorRef)
                    -- ^ Local state
                    -> (Text -> IO ())
                    -- ^ Trigger for output event
                    -> Configuration
                    -- ^ Chart data
                    -> (Maybe Text)
                    -- ^ Text
                    -> (Maybe LineChar)
                    -- ^ Scroll To
                    -> JSM ()
        onFirstTime element_ ref trigger configuration_ mText mScrollTo = do
            ref_ <- fromTextArea element_ configuration_
            case configuration_ ^. configuration_value of
                Nothing -> pure ()
                Just startTxt -> setValueAndRefresh ref_ startTxt
            liftIO $ writeIORef ref (Just ref_)
            registerOnChange ref_
                             (onChangeCallback trigger)
            case mText of
                Nothing -> return ()
                Just text_ -> setValueAndRefresh ref_ text_
            case mScrollTo of
                Nothing       -> return ()
                Just scrollTo_ -> scrollIntoView ref_ scrollTo_ 200

            return ()


        onNextTime :: CodeMirrorRef
                   -- ^ Current value of local state
                   -> Configuration
                   -- ^ Chart data
                   -> (Maybe Text)
                   -- ^ Text
                   -> (Maybe LineChar)
                   -- ^ Scroll To
                   -> JSM ()
        onNextTime ref_ _ mText mScrollTo = do
            case mText of
                Nothing -> return ()
                Just text_ -> setValueAndRefresh ref_ text_
            case mScrollTo of
                Nothing        -> return ()
                Just scrollTo_ -> scrollIntoView ref_ scrollTo_ 200


        onChangeCallback :: (Text -> IO ())
                         -> Text
                         -> JSM ()
        onChangeCallback trigger t = do
            liftIO $ trigger t
            return ()
