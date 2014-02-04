-- | This UI acts like the Vty UI from the perspective of Yi:
--  * The threading behavior is the same.
--  * The state of the editor is evaluated to the same degree.
module Yi.UI.Test (start) where
import Prelude hiding (mapM)

import Yi.Buffer ( runBuffer
                 , Point(..)
                 , Size(..)
                 , sizeB
                 , getMarks
                 , getMarkPointB
                 , MarkSet(..)
                 , pointB
                 , getModeLine
                 , miniIdentString
                 )
import Yi.Config
import Yi.Debug (logPutStrLn)
import Yi.Event (Event)
import Yi.Editor
import qualified Yi.UI.Common as Common
import Yi.Region
import Yi.Style (emptyAttributes, Attributes)
import Yi.UI.Utils
import Yi.Window (Window(..))

import Control.Applicative
import Control.Concurrent
import Control.Lens hiding (lazy)
import Control.Monad (void)
import Control.Monad.Base

import Data.Foldable (toList, forM_)
import Data.Traversable (mapM)
import Data.Hashable (hash)
import Data.IORef
import Data.List (partition)
import qualified Data.List.PointedList.Circular as PL

data UI = UI { uiThread        :: ThreadId
             , uiEndInputLoop  :: MVar ()
             , uiEndRenderLoop :: MVar ()
             , uiEditor        :: IORef Editor
             , uiDirty         :: MVar ()
             , uiEventsPushed  :: MVar ()
             , uiEventsProcessed :: MVar ()
             }

newtype Rendered = Rendered { renderSummary :: String }

mkUI :: Config -> UI -> Common.UI
mkUI config ui = Common.dummyUI
    { Common.main             = main ui
    , Common.end              = end ui
    , Common.suspend          = return ()
    , Common.refresh          = requestRefresh ui
    , Common.layout           = layout config ui
    , Common.userForceRefresh = userForceRefresh ui
    }

start :: UIBoot
start cfg eventOut actionOut editor = liftBase $ do
  ui <- pure UI <*> myThreadId
                <*> newEmptyMVar
                <*> newEmptyMVar
                <*> newIORef editor
                <*> newEmptyMVar
                <*> newEmptyMVar
                <*> newEmptyMVar
  let Just eventsFile = replayEvents $ configUI cfg
  events <- readAllEvents eventsFile
  logPutStrLn $ "Replaying " ++ show (length events) ++ " events."
  void $ forkIO $ inputLoop ui eventOut actionOut events
  void $ forkIO $ renderLoop cfg ui
  return $ mkUI cfg ui

readAllEvents :: String -> IO [Event]
readAllEvents eventsFile = do
  let parseEvents str = case reads str of
        (event,str') : _ -> event : parseEvents str'
        []               -> []
  pure parseEvents <*> readFile eventsFile

inputLoop ui eventOut actionOut [] = putMVar (uiEventsPushed ui) ()
inputLoop ui eventOut actionOut (event:events) = do
  threadDelay 10000
  logPutStrLn $ show event
  void $ eventOut event
  inputLoop ui eventOut actionOut events

-- (?) Implicit dependency on processing an event implies the first uiDirty put.
renderLoop config ui = do
  takeMVar $ uiDirty ui
  e <- readIORef (uiEditor ui)
  let renders = fmap (renderWindow (configUI config) e 80) (PL.withFocus $ windows e)
  forM_ renders $ \(Rendered summary) -> putStrLn summary
  pushed <- tryTakeMVar (uiEventsPushed ui)
  if Nothing == pushed
  then renderLoop config ui
  else putMVar (uiEventsProcessed ui) ()

main ui = do
  takeMVar $ uiEventsProcessed ui
  return ()

end shouldTerm ui = return ()

requestRefresh ui editor = do
  writeIORef (uiEditor ui) editor
  void $ tryPutMVar (uiDirty ui) ()

userForceRefresh ui = do
  return ()

layout :: Config -> UI -> Editor -> IO Editor
layout config ui editor = do
  let (rows,cols) = (50,80)
  let ws = windows editor
      tabBarHeight = if Common.hasTabBar config editor then 1 else 0
      (cmd, _) = statusLineInfo editor
      niceCmd = arrangeItems cmd cols (maxStatusHeight editor)
      cmdHeight = length niceCmd
      ws' = applyHeights (computeHeights (rows - tabBarHeight - cmdHeight + 1) ws) ws
      discardOldRegion w = w { winRegion = emptyRegion }
                           -- Discard this field, otherwise we keep retaining reference to
                           -- old Window objects (leak)

  let apply :: Window -> IO Window
      apply win = do
        let uiconfig = configUI config
        newWinRegion <- return $! getRegionImpl win uiconfig editor cols (height win)
        return $! win { winRegion = newWinRegion, actualLines = 80 }

  ws'' <- mapM (apply . discardOldRegion) ws'
  return $ windowsA .~ ws'' $ editor

-- | Calculate window heights, given all the windows and current height.
-- (No specific code for modelines)
computeHeights :: Int -> PL.PointedList Window -> [Int]
computeHeights totalHeight ws = y+r-1 : repeat y
  where (mwls, wls) = partition isMini (toList ws)
        (y,r) = getY (totalHeight - length mwls) (length wls)

getRegionImpl :: Window -> UIConfig -> Editor -> Int -> Int -> Region
getRegionImpl win cfg e w h = region
  where (_,region) = drawWindow cfg e (error "focus must not be used") win w h

renderWindow :: UIConfig -> Editor -> Int -> (Window, Bool) -> Rendered
renderWindow cfg e width (win,hasFocus) =
    let (rendered,_) = drawWindow cfg e hasFocus win width (height win)
    in rendered

drawWindow :: UIConfig -> Editor -> Bool -> Window -> Int -> Int -> (Rendered, Region)
drawWindow cfg e focused win w h = (Rendered summary, mkRegion fromMarkPoint toMarkPoint')
    where
        b = findBufferWith (bufkey win) e
        sty = configStyle cfg

        notMini = not (isMini win)
        -- off reserves space for the mode line. The mini window does not have a mode line.
        off = if notMini then 1 else 0
        h' = h - off
        point_summary = show $ fst $ runBuffer win b pointB
        eofPoint = fst $ runBuffer win b sizeB
        region = mkSizeRegion fromMarkPoint (Size (w*h'))
        -- Work around a problem with the mini window never displaying it's contents due to a
        -- fromMark that is always equal to the end of the buffer contents.
        (Just (MarkSet fromM _ _), _) = runBuffer win b (getMarks win)
        fromMarkPoint = if notMini
                            then fst $ runBuffer win b (getMarkPointB fromM)
                            else Point 0
        (text, _)    = runBuffer win b (indexedAnnotatedStreamB fromMarkPoint)
        (attributes, _) = runBuffer win b $ attributesPictureAndSelB sty (currentRegex e) region
        paintedChars = paintChars attributes text
        viewedPaintedChars = take (80*50) paintedChars
        bufferHash = show $ hash $ show viewedPaintedChars
        toMarkPoint' = fromMarkPoint + (Point $ length viewedPaintedChars)
        prompt = if isMini win then miniIdentString b else ""
        (modeLine0, _) = runBuffer win b $ getModeLine (commonNamePrefix e)
        modeLine = if notMini then Just modeLine0 else Nothing
        modeLineHash = show $ hash $ show modeLine
        summary = show bufferHash ++ " " ++ show modeLineHash

paintChars :: [(Point,Attributes)] -> [(Point,Char)] -> [(Char, (Attributes,Point))]
paintChars changes cs = [(c,(s,p)) | ((p,c),s) <- zip cs attrs]
    where attrs = lazy (stys changes cs)

lazy :: [a] -> [a]
lazy l = head l : lazy (tail l)

stys :: [(Point,Attributes)] -> [(Point,Char)] -> [Attributes]
stys [] cs = [ emptyAttributes | _ <- cs ]
stys ((endPos,sty'):xs) cs = [ emptyAttributes | _ <- previous ] ++ stys xs later
    where (previous, later) = break ((endPos <=) . fst) cs

getY :: Int -> Int -> (Int,Int)
getY screenHeight 0               = (screenHeight, 0)
getY screenHeight numberOfWindows = screenHeight `quotRem` numberOfWindows

