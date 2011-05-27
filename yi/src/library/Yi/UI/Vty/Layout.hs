{-# LANGUAGE ScopedTypeVariables #-}
{- XXX: How a view is rendered is deeply intertwined with a line-wrapping alogirthm. For consistent
 - line wrapping between the UIs this needs to be factored out.
 -
 - XXX: The layout calculation assumes that miniwindows are drawn beneath their associated windows.
 - This is the correct behavior (IMO - coreyoconnor ) if multiple miniwindows should be visible
 - simultaneously. 
 -}
module Yi.UI.Vty.Layout where

import Yi.Prelude hiding ( (<|>) )
import Prelude ()

import Yi.ActivityGroup
import Yi.Buffer
import Yi.Config
import Yi.Editor
import Yi.Layout
import Yi.Style
import Yi.Tab
import Yi.UI.Utils
import Yi.UI.Vty.Data
import Yi.UI.Vty.Render
import Yi.Window

import Control.Monad.Reader ( ask )
import Control.Monad.State.Strict ( evalStateT
                                  , runState
                                  , State
                                  , StateT(..) 
                                  )
import qualified Control.Monad.State as State
import Control.Monad.Trans ( MonadIO, lift)

import Data.Accessor.Basic
import Data.List ( length )
import Data.IORef
import Data.Map ( Map )
import qualified Data.Map as Map

import Graphics.Vty ( (<|>) )
import qualified Graphics.Vty as Vty

-- This re-computes: the display rectangle of all the views; the display image ; The position of the
-- cursor in the view.
--
-- Uses the following procedure:
--  determine the visible area for views.
--  determine the rectangles to render the activities 
--  for each activity's rectange:
--      determine the rectanges to render the views
--          for each activity view:
--              store the view rectangle
--
--  Then, propogate some of the render info back into the editor's view state.
--
--  Then, produce the ViewRender for the view given the above.
--
-- This, in effect, renders the entire UI for each layout. However, since the actual renders are
-- lazy multiple updates can occur before a refresh with no additional cost.
layout :: UI -> Editor -> IO Editor
layout ui e = do
    (screenCols,screenRows) <- readIORef (scrsize ui)
    -- subtract off the tab bar and cmd height to get the final dimensions.
    let tabBarHeight :: Int = if hasTabBar e ui then 1 else 0
        tabBarOffset :: Double = fromIntegral tabBarHeight
        (cmd, _) = statusLineInfo e
        niceCmd = arrangeItems cmd screenCols (maxStatusHeight e)
        cmdHeight = length niceCmd
    let usableRows :: Int = screenRows - tabBarHeight - cmdHeight
        usableColumns :: Int = screenCols
    -- use layoutToRectangles to generate the rectanges for the activity groups.
    let usableRect = Rectangle 0.0 tabBarOffset
                               ( fromIntegral usableColumns ) ( fromIntegral usableRows )
    -- update each tab layout using the usableRect. 
    -- Build the new window rects map at the same time.
    let ( (), (viewRects', mwAssocs') ) = runState ( runReaderT (buildLayout usableRect) 
                                                                (config ui, e)
                                                   ) 
                                                   (Map.empty, Map.empty)
    writeIORef (viewRectsRef ui) viewRects'
    writeIORef (mwAssocMapRef ui) mwAssocs'
    -- build the ViewRender for each view 
    let ( () , viewRenders') = runState ( runReaderT (buildRenders viewRects')
                                                     (config ui, e)
                                        )
                                        Map.empty
    writeIORef (viewRendersRef ui) viewRenders'
    return $ modifyWindows (propogateViewInfo viewRects' viewRenders') e

buildLayout :: Rectangle -> ReaderT (Config, Editor) 
                                    (State (ViewRectMap, MWAssocMap)) 
                                    ()
buildLayout usableRect = do
    (_, e) <- ask
    mapM_ (buildTabLayout usableRect) $ get tabsA e

-- applied to each tab to layout the activity views and update all the view records with their
-- layout rectangles.
buildTabLayout :: Rectangle -> Tab -> ReaderT (Config, Editor) 
                                              (State (ViewRectMap, MWAssocMap)) 
                                              () 
buildTabLayout usableRect tab = do
    let layoutRects = layoutToRectangles usableRect ( tabLayout tab )
        -- adjust for a terminals integer width and height.
        adjustedRects = integerClampRects layoutRects 
        groupRectMap :: Map ActivityGroupRef Rectangle = Map.fromList adjustedRects
        lookupRect ag = case Map.lookup (activityGroupRef ag) groupRectMap of
                            Nothing -> error "buildTabLayout: no layout rectangle for view."
                            Just x -> x
    mapM_ (\ag -> buildActivityLayout (lookupRect ag) ag) 
          ( tabActivities tab ) 

buildActivityLayout :: Rectangle -> ActivityGroup -> ReaderT (Config, Editor) 
                                                             (State (ViewRectMap, MWAssocMap))
                                                             ()
buildActivityLayout usableRect ag = do
    let layoutRects = layoutToRectangles usableRect ( groupLayout ag )
        -- adjust for a terminals integer width and height.
        adjustedRects = integerClampRects layoutRects
        -- build into the initial map. This should only contain the window refs of the
        -- non-miniwindows in this activity.
        initialRectMap = Map.fromList adjustedRects
    -- build the layout for each subview in the activity.
    flip evalStateT Nothing $ do
        mapM_ (buildViewLayout initialRectMap) (groupActivities ag)

buildViewLayout :: Map WindowRef Rectangle 
                -> Window 
                -> StateT (Maybe Window)
                          (ReaderT (Config, Editor) 
                                   (State (ViewRectMap, MWAssocMap) )
                          )
                          ()
buildViewLayout initialRectMap v = 
    case Map.lookup (wkey v) initialRectMap of
        Nothing 
            | isMini v -> do
                mPreviousView <- State.get
                State.put Nothing
                case mPreviousView of 
                    Nothing -> 
                        error "miniwindow must follow a non-miniwindow"
                    Just previousView -> 
                        lift $ buildMiniViewLayout v previousView
            | otherwise -> error "non-miniwindow not in view layout"
        Just viewRect -> do
            lift $ State.modify (\(rm, am) -> ( Map.insert (wkey v) viewRect rm, am ))
            State.put $ Just v

buildMiniViewLayout :: Window -> Window -> ReaderT (Config, Editor)
                                                   (State (ViewRectMap, MWAssocMap))
                                                   ()
buildMiniViewLayout v previousView = do
    -- build the rectangle off the rectangle of the previous view.
    let _cols :: Int = width previousView
        rows :: Int = 1
    (rectMap, associatedMap) <- State.get
    let previousRect = case Map.lookup (wkey previousView) rectMap of
                        Nothing -> error "buildMiniViewLayout: missing display rect"
                        Just x -> x
        viewRect = Rectangle 
                   { rectX = rectX previousRect
                   , rectY = rectY previousRect + rectHeight previousRect
                                                - fromIntegral rows
                   , rectWidth = rectWidth previousRect
                   , rectHeight = fromIntegral rows
                   }
        -- update the the previous view's display rectangle to account for the attached mini
        -- window's display rectangle.
        previousRect' = previousRect
                        { rectHeight = rectHeight previousRect - rectHeight viewRect
                        }
    -- Add these rectangles to the rectangle map being built. 
    let rectMap' = Map.insert (wkey v) 
                              viewRect 
                              (Map.insert (wkey previousView)
                                          previousRect'
                                          rectMap
                              )
    let associatedMap' = Map.insert (wkey previousView) (wkey v) associatedMap
    State.put (rectMap', associatedMap')
    return ()

buildRenders :: ViewRectMap -> ReaderT (Config, Editor) 
                                       (State ViewRenderMap) 
                                       ()
buildRenders viewRects = do
    ( theConfig, e) <- ask
    forM_ (windowsWithFocus e) $ \(view, isViewFocused) -> do
        let viewRect = case Map.lookup (wkey view) viewRects of
                        Nothing -> error "buildRenders: Missing display rect."
                        Just x -> x
            viewRender' = case ( round $ rectX viewRect :: Int ) of
                0 -> 
                    let cols = round $ rectWidth viewRect
                        rows = round $ rectHeight viewRect
                    in buildViewRender (configUI theConfig)
                                       e
                                       view
                                       (cols, rows)
                                       isViewFocused
                _ -> 
                    let cols = round $ rectWidth viewRect - 1
                        rows = round $ rectHeight viewRect
                        viewRender0 = buildViewRender (configUI theConfig)
                                                      e
                                                      view
                                                      (cols, rows)
                                                      isViewFocused
                        viewRender1 = viewRender0 { viewImage = border <|> viewImage viewRender0
                                                  , viewCursor = case viewCursor viewRender0 of
                                                                        Nothing -> Nothing
                                                                        Just (x,y) -> Just (x+1,y)
                                                  }
                        border = Vty.vert_cat $ replicate rows 
                                              $ withAttributes (modelineAttributes uiStyle) "|"
                        uiStyle = configStyle $ configUI $ theConfig
                    in viewRender1
        State.modify $ Map.insert (wkey view) viewRender'

propogateViewInfo :: ViewRectMap -> ViewRenderMap -> Window -> Window
propogateViewInfo viewRects viewRenders v =
    let viewRect' = case Map.lookup (wkey v) viewRects of
                        Nothing -> error "propogateViewInfo: Missing display rect."
                        Just x -> x
        cols = round $ rectWidth viewRect'
        rows = round $ rectHeight viewRect'
        viewRender' = case Map.lookup (wkey v) viewRenders of
                        Nothing -> error "propogateViewInfo: Missing view render."
                        Just x -> x
    in v { width       = cols
         , height      = rows
         , winRegion   = displayedRegion viewRender'
         , actualLines = displayedLines viewRender'
         }
                
-- Do Vty layout inside the Yi event loop
layoutAction :: (MonadEditor m, MonadIO m) => UI -> m ()
layoutAction ui = do
    withEditor . State.put =<< io . layout ui =<< withEditor State.get
    withEditor $ mapM_ (flip withWindowE snapInsB) =<< gets windows 

