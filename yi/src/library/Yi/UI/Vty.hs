{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright (C) 2007-8 JP Bernardy
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Originally derived from: riot/UI.hs Copyright (c) Tuomo Valkonen 2004.


-- | This module defines a user interface implemented using vty.

module Yi.UI.Vty (start) where

import Yi.Prelude hiding ((<|>))
import Prelude ()

import Data.Accessor.Basic ( get )
import Data.List ( map
                 , nub
                 , repeat
                 , sort 
                 , take
                 )
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid

import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import Control.Monad.Trans ( liftIO )

import System.Exit
import System.Posix.Signals (raiseSignal, sigTSTP)
import System.Posix.Terminal
import System.Posix.IO (stdInput)

import Yi.ActivityGroup
import Yi.Config
import Yi.Editor
import Yi.Event
import Yi.Layout
import Yi.Style
import Yi.Tab
import Yi.Window

import Yi.Keymap (makeAction, YiM)

import qualified Yi.UI.Common as Common
import Yi.UI.Utils
import Yi.UI.Vty.Data
import Yi.UI.Vty.Layout
import Yi.UI.Vty.Render

import Graphics.Vty ( Image 
                    , (<->)
                    , (<|>)
                    , vert_cat
                    )
import qualified Graphics.Vty  as Vty

mkUI :: UI -> Common.UI
mkUI ui = Common.dummyUI 
  {
   Common.main           = main ui,
   Common.end            = end ui,
   Common.suspend        = raiseSignal sigTSTP,
   Common.refresh        = refresh ui,
   Common.layout         = layout ui,
   Common.userForceRefresh = userForceRefresh ui
  }

-- | Initialise the ui
-- Provided: the editor configuration; the channel to write events to; the channel to output Actions
-- to; and the editor state.
start :: UIBoot
start cfg outEventChannel outActionChannel editor = do
  liftIO $ do 
          oattr <- getTerminalAttributes stdInput
          v <- Vty.mkVtyEscDelay $ configVtyEscDelay $ configUI $ cfg
          nattr <- getTerminalAttributes stdInput
          setTerminalAttributes stdInput (withoutMode nattr ExtendedFunctions) Immediately
          -- remove the above call to setTerminalAttributes when vty does it.
          Vty.DisplayRegion x0 y0 <- Vty.display_bounds $ Vty.terminal v
          sz <- newIORef (fromEnum x0, fromEnum y0)
          -- fork input-reading thread. important to block *thread* on getKey
          -- otherwise all threads will block waiting for input
          tid <- myThreadId
          endUI <- newEmptyMVar
          tuiRefresh <- newEmptyMVar
          editorRef <- newIORef editor
          rectsRef <- newIORef Map.empty
          rendersRef <- newIORef Map.empty
          assocsRef <- newIORef Map.empty
          let result = UI v sz tid endUI tuiRefresh editorRef cfg oattr rectsRef rendersRef assocsRef
              -- | Action to read characters into a channel
              -- Loops until the endUI MVar is signaled.
              getcLoop = maybe (getKey >>= outEventChannel >> getcLoop) 
                               (const (return ())) 
                         =<< tryTakeMVar endUI

              -- | Get the next key event. This waits for VTY to provide the next VTY event; Handles
              -- any EvResize events; Then converts from a VTY key event to a Yi key event.
              getKey = do 
                event <- Vty.next_event v
                case event of 
                  (Vty.EvResize x y) -> do
                      logPutStrLn $ "UI: EvResize: " ++ show (x,y)
                      writeIORef sz (x,y)
                      outActionChannel [makeAction (layoutAction result :: YiM ())] 
                      -- since any action will force a refresh, return () is probably 
                      -- sufficient instead of "layoutAction result"
                      getKey
                  _ -> return (fromVtyEvent event)
          discard $ forkIO getcLoop
          return (mkUI result)

main :: UI -> IO ()
main ui = do
  let
      -- | When the editor state isn't being modified, refresh, then wait for
      -- it to be modified again.
      refreshLoop :: IO ()
      refreshLoop = forever $ do
                      logPutStrLn "waiting for refresh"
                      takeMVar (uiRefresh ui)
                      handle (\(except :: IOException) -> do
                                 logPutStrLn "refresh crashed with IO Error"
                                 logError $ show $ except)
                             (readRef (uiEditor ui) >>= refresh ui >> return ())
  logPutStrLn "refreshLoop started"
  refreshLoop

-- | Clean up and go home
end :: UI -> Bool -> IO ()
end i reallyQuit = do  
  Vty.shutdown (vty i)
  setTerminalAttributes stdInput (oAttrs i) Immediately
  discard $ tryPutMVar (uiEnd i) ()
  when reallyQuit $ throwTo (uiThread i) ExitSuccess
  return ()

fromVtyEvent :: Vty.Event -> Yi.Event.Event
fromVtyEvent (Vty.EvKey Vty.KBackTab mods) = Event Yi.Event.KTab (sort $ nub $ Yi.Event.MShift : map fromVtyMod mods)
fromVtyEvent (Vty.EvKey k mods) = Event (fromVtyKey k) (sort $ map fromVtyMod mods)
fromVtyEvent _ = error "fromVtyEvent: unsupported event encountered."


fromVtyKey :: Vty.Key -> Yi.Event.Key
fromVtyKey (Vty.KEsc     ) = Yi.Event.KEsc      
fromVtyKey (Vty.KFun x   ) = Yi.Event.KFun x    
fromVtyKey (Vty.KPrtScr  ) = Yi.Event.KPrtScr   
fromVtyKey (Vty.KPause   ) = Yi.Event.KPause    
fromVtyKey (Vty.KASCII '\t') = Yi.Event.KTab
fromVtyKey (Vty.KASCII c ) = Yi.Event.KASCII c  
fromVtyKey (Vty.KBS      ) = Yi.Event.KBS       
fromVtyKey (Vty.KIns     ) = Yi.Event.KIns      
fromVtyKey (Vty.KHome    ) = Yi.Event.KHome     
fromVtyKey (Vty.KPageUp  ) = Yi.Event.KPageUp   
fromVtyKey (Vty.KDel     ) = Yi.Event.KDel      
fromVtyKey (Vty.KEnd     ) = Yi.Event.KEnd      
fromVtyKey (Vty.KPageDown) = Yi.Event.KPageDown 
fromVtyKey (Vty.KNP5     ) = Yi.Event.KNP5      
fromVtyKey (Vty.KUp      ) = Yi.Event.KUp       
fromVtyKey (Vty.KMenu    ) = Yi.Event.KMenu     
fromVtyKey (Vty.KLeft    ) = Yi.Event.KLeft     
fromVtyKey (Vty.KDown    ) = Yi.Event.KDown     
fromVtyKey (Vty.KRight   ) = Yi.Event.KRight    
fromVtyKey (Vty.KEnter   ) = Yi.Event.KEnter    
fromVtyKey (Vty.KBackTab ) = error "This should be handled in fromVtyEvent"
fromVtyKey (Vty.KBegin   ) = error "Yi.UI.Vty.fromVtyKey: can't handle KBegin"

fromVtyMod :: Vty.Modifier -> Yi.Event.Modifier
fromVtyMod Vty.MShift = Yi.Event.MShift
fromVtyMod Vty.MCtrl  = Yi.Event.MCtrl
fromVtyMod Vty.MMeta  = Yi.Event.MMeta
fromVtyMod Vty.MAlt   = Yi.Event.MMeta

-- | Redraw the entire terminal from the UI.
refresh :: UI -> Editor -> IO ()
refresh ui e = do
    logPutStrLn "refreshing screen."
    screenSize <- readIORef (scrsize ui)
    viewRenders <- readIORef (viewRendersRef ui)
    viewRects <- readIORef (viewRectsRef ui)
    mwAssocs <- readIORef (mwAssocMapRef ui)
    let editorImage = renderEditor mwAssocs viewRenders screenSize ui e 
        cursor = determineCursor viewRenders viewRects e
    Vty.update (vty $ ui) 
               ( Vty.pic_for_image editorImage ) { Vty.pic_cursor = cursor }
    return ()

renderEditor :: MWAssocMap -> ViewRenderMap -> (Int, Int) -> UI -> Editor -> Image
renderEditor mwAssocMap viewRenders screenSize ui e = 
    let tabBarImages = renderTabBar ui e (fst screenSize)
        (cmd, cmdStyle) = statusLineInfo e
        niceCmd = arrangeItems cmd (fst screenSize) (maxStatusHeight e)
        formatCmdLine text = withAttributes statusBarStyle 
                                            (take (fst screenSize) $ text ++ repeat ' ')
        statusBarStyle = ((appEndo <$> cmdStyle) <*> baseAttributes) 
                            $ configStyle $ configUI $ config $ ui
    in vert_cat tabBarImages
       <->
       compositeViewImage mwAssocMap viewRenders e
       <->
       vert_cat (fmap formatCmdLine niceCmd)

determineCursor :: ViewRenderMap -> ViewRectMap -> Editor -> Vty.Cursor
determineCursor viewRenders viewRects e =
    let view = get (currentWindowA) e
        viewRender = case Map.lookup (wkey view) viewRenders of
                        Nothing -> error "determineCursor: missing view render."
                        Just x -> x
        viewRect = case Map.lookup (wkey view) viewRects of
                        Nothing -> error "determineCursor: missing view rect."
                        Just x -> x
    in case viewCursor viewRender of
        Nothing -> Vty.NoCursor
        Just (x,y) -> 
            if (x >= 0) && (y >= 0) && (x < width view) && (y < height view)
                then let globalX = toEnum $ (round $ rectX viewRect) + x
                         globalY = toEnum $ (round $ rectY viewRect) + y 
                     in Vty.Cursor globalX globalY
                else Vty.NoCursor

compositeViewImage :: MWAssocMap -> ViewRenderMap -> Editor -> Image
compositeViewImage mwAssocMap viewRenders e =
    let tab = get currentTabA e
        lookupImage = viewImage . (\m -> case m of 
                                          Nothing -> error "compositeViewImage: missing render."
                                          Just x -> x
                                  ) . flip Map.lookup viewRenders
        orientationToOp Horizontal = (<|>)
        orientationToOp Vertical   = (<->)
        compositeTabLayout (SingleWindow agID) = 
            let ag = case lookupActivityGroup agID tab of
                        Nothing -> error "compositeViewImage: Missing activity group."
                        Just x -> x
                compositeViewLayout (SingleWindow viewID)
                    = case Map.lookup viewID mwAssocMap of
                        Just mwID -> lookupImage viewID <-> lookupImage mwID
                        Nothing -> lookupImage viewID
                compositeViewLayout (Stack o sl) =
                    let op = orientationToOp o
                    in foldl1 op $ map (compositeViewLayout . fst) sl
                compositeViewLayout (Pair o _ _ sl0 sl1) =
                    let op = orientationToOp o
                    in (compositeViewLayout sl0) `op` (compositeViewLayout sl1)
            in compositeViewLayout (groupLayout ag)
        compositeTabLayout (Stack o sl) =
            let op = orientationToOp o
            in foldl op Vty.empty_image $ map ( compositeTabLayout . fst ) sl
        compositeTabLayout (Pair o _ _ sl0 sl1) =
            let op = orientationToOp o
            in (compositeTabLayout sl0) `op` (compositeTabLayout sl1)
    -- descend the current tab's layout.
    in compositeTabLayout (tabLayout tab) 

