{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yi.UI.Vty.Data where

import Yi.Buffer.Basic
import Yi.Config
import Yi.Editor
import Yi.Layout
import Yi.Region

import Control.Concurrent

import Data.Accessor.Basic
import Data.IORef
import qualified Data.List.PointedList as PL
import Data.Map ( Map )

import Graphics.Vty 

import System.Posix.Terminal

data UI = UI { vty         :: Vty             -- ^ Vty
             , scrsize     :: IORef (Int,Int) -- ^ screen size (width, height)
             , uiThread    :: ThreadId
             , uiEnd       :: MVar ()
             , uiRefresh   :: MVar ()
             , uiEditor    :: IORef Editor    -- ^ Copy of the editor state, 
                                              -- local to the UI, used to 
                                              -- show stuff when the window 
                                              -- is resized.
             , config          :: Config
             , oAttrs          :: TerminalAttributes
             , viewRendersRef  :: IORef ViewRenderMap
             , viewRectsRef    :: IORef ViewRectMap
             , mwAssocMapRef   :: IORef MWAssocMap
             }

-- display rectangles. Adjusted for terminal graphics.
type ViewRectMap   = Map WindowRef Rectangle
type MWAssocMap    = Map WindowRef WindowRef
type ViewRenderMap = Map WindowRef ViewRender

data ViewRender = ViewRender 
    { viewImage       :: Image            -- ^ the picture currently displayed.
    , viewCursor      :: Maybe (Int, Int) -- ^ cursor in the image line/column coordinates.
    , displayedLines  :: Int
    , displayedRegion :: Region
    }

-- | Determine whether it is necessary to render the tab bar
hasTabBar :: Editor -> UI -> Bool
hasTabBar e ui = (not . configAutoHideTabBar . configUI . config $ ui) 
                 || (PL.length $ e ^. tabsA) > 1

