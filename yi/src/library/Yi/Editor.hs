{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleContexts, StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Copyright (c) 2004-5, Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2007-8, JP Bernardy

-- | The top level editor state, and operations on it.

module Yi.Editor where

import Control.Monad.RWS hiding (get, put, mapM, forM_)
import Data.Accessor.Basic (fromSetGet)
import qualified Data.Accessor.Basic as Accessor
import Data.Accessor.Template
import Data.Binary
import Data.DeriveTH
import Data.Either (rights)
import Data.List (nub, delete, (\\), (!!), intercalate, take, drop, cycle)
import Data.Maybe
import Data.Typeable
import Prelude (map, filter, length, reverse)
import System.FilePath (splitPath)
import Yi.Buffer
import Yi.Config
import Yi.Dynamic
import Yi.Event (Event)
import Yi.Interact as I
import Yi.KillRing
import Yi.ActivityGroup
import Yi.Layout
import Yi.Prelude
import Yi.Style (StyleName, defaultStyle)
import Yi.Tab
import Yi.Window
import qualified Data.Rope as R
import qualified Data.DelayList as DelayList
import qualified Data.List.PointedList as PL (atEnd, atStart)
import qualified Data.List.PointedList.Circular as PL
import qualified Data.Map as M
import {-# source #-} Yi.Keymap (extractTopKeymap)

import qualified Control.Monad.State as State

type Status = ([String],StyleName)
type Statuses = DelayList.DelayList Status

-- | The Editor state
data Editor = Editor {
        bufferStack   :: ![BufferRef]               -- ^ Stack of all the buffers. 
                                                    -- Invariant: never empty
                                                    -- Invariant: first buffer is the current one.
       ,buffers       :: !(M.Map BufferRef FBuffer)
       ,refSupply     :: !Int  -- ^ Supply for buffer, activity layouts, window, activity, and tab ids.

       ,tabs_          :: !(PL.PointedList Tab) -- ^ current tab contains the currently visible activities.

       ,dynamic       :: !DynamicValues              -- ^ dynamic components

       ,statusLines   :: !Statuses
       ,maxStatusHeight :: !Int
       ,killring      :: !Killring
       ,currentRegex         :: !(Maybe SearchExp) -- ^ currently highlighted regex (also most recent regex for use in vim bindings)
       ,searchDirection :: !Direction
       ,pendingEvents :: ![Event]                   -- ^ Processed events that didn't yield any action yet.
       ,onCloseActions :: !(M.Map BufferRef (EditorM ())) -- ^ Actions to be run when the buffer is closed; should be scrapped.
    }
    deriving Typeable

instance Binary Editor where
    put (Editor bss bs supply ts dv _sl msh kr _re _dir _ev _cwa ) = put bss >> put bs >> put supply >> put ts >> put dv >> put msh >> put kr
    get = do
        bss <- get
        bs <- get
        supply <- get
        ts <- get
        dv <- get
        msh <- get
        kr <- get
        return $ emptyEditor {bufferStack = bss,
                              buffers = bs,
                              refSupply = supply,
                              tabs_ = ts,
                              dynamic = dv,
                              maxStatusHeight = msh,
                              killring = kr
                             }

newtype EditorM a = EditorM {fromEditorM :: RWS Config () Editor a}
    deriving (Monad, MonadState Editor, MonadReader Config, Functor)

deriving instance Typeable1 EditorM

instance Applicative EditorM where
  pure = return
  (<*>) = ap

class (Monad m, MonadState Editor m) => MonadEditor m
    where askCfg :: m Config
          withEditor :: EditorM a -> m a
          withEditor f = do
              cfg <- askCfg
              getsAndModify (runEditor cfg f) 
    
liftEditor :: MonadEditor m => EditorM a -> m a
liftEditor = withEditor

instance MonadEditor EditorM where
    askCfg = ask
    withEditor = id

-- | The initial state
emptyEditor :: Editor
emptyEditor = flip State.evalState (0 :: Int) $ do
    let inc = State.get >>= (\(v :: Int) -> State.put (v+1) >> return v)
    buf <- newB <$> (BufferRef <$> inc) <*> pure (Left "console") <*> pure (R.fromString "")
    consoleWKey <- inc
    let win = (dummyWindow (bkey buf)) { wkey = WindowRef consoleWKey, isMini = False }
    activityGroup <- makeActivityGroup1 <$> (ActivityGroupRef <$> inc) <*> pure win
    tab <- makeTab1 <$> inc <*> pure activityGroup
    nextRefValue <- State.get
    return $ Editor {
        buffers      = M.singleton (bkey buf) buf
       ,tabs_        = PL.singleton tab
       ,bufferStack  = [bkey buf]
       ,refSupply    = nextRefValue
       ,currentRegex = Nothing
       ,searchDirection = Forward
       ,dynamic      = initial
       ,statusLines  = DelayList.insert (maxBound, ([""], defaultStyle)) []
       ,killring     = krEmpty
       ,pendingEvents = []
       ,maxStatusHeight = 1
       ,onCloseActions = M.empty
       }

-- ---------------------------------------------------------------------

runEditor :: Config -> EditorM a -> Editor -> (Editor, a)
runEditor cfg f e = let (a, e',()) = runRWS (fromEditorM f) cfg e in (e',a)

$(nameDeriveAccessors ''Editor (\n -> Just (n ++ "A")))

windows :: Editor -> [ Window ]
windows e = concatMap tabWindows $ Accessor.get tabsA e

windowsWithFocus :: Editor -> [ (Window, Bool) ]
windowsWithFocus e =
    let currentWin = Accessor.get currentWindowA e
    in map (\w -> (w, w == currentWin) ) $ windows e

modifyWindowsE :: ( Window -> Window ) -> EditorM ()
modifyWindowsE f = modA tabsA ( fmap ( mapTabWindows f ) )
    
modifyWindows :: (Window -> Window) -> Editor -> Editor
modifyWindows f = Accessor.modify tabsA ( fmap ( mapTabWindows f ) )

tabsA :: Accessor Editor (PL.PointedList Tab)
tabsA = rnfA . tabs_A . fixCurrentBufferA_

currentTabA :: Accessor Editor Tab
currentTabA = PL.focusA . tabsA

dynA :: YiVariable a => Accessor Editor a
dynA = dynamicValueA . dynamicA

-- ---------------------------------------------------------------------
-- Buffer operations

newRef :: EditorM Int
newRef = do
  modA refSupplyA (+ 1)
  getA refSupplyA

newBufRef :: EditorM BufferRef
newBufRef = BufferRef <$> newRef

-- | Create and fill a new buffer, using contents of string.
-- | Does not focus the window, or make it the current window.
-- | Call newWindowE or switchToBufferE to take care of that.
stringToNewBuffer :: BufferId -- ^ The buffer indentifier
                  -> Rope -- ^ The contents with which to populate the buffer
                  -> EditorM BufferRef
stringToNewBuffer nm cs = do
    u <- newBufRef
    defRegStyle <- configRegionStyle <$> askCfg
    insertBuffer $ setVal regionStyleA defRegStyle $ newB u nm cs
    m <- asks configFundamentalMode
    withGivenBuffer0 u $ setAnyMode m
    return u

insertBuffer :: FBuffer -> EditorM ()
insertBuffer b = modify $
                 \e -> -- insert buffers at the end, so that "background" buffers
                        -- do not interfere.
                        e {bufferStack = nub (bufferStack e ++ [bkey b]),
                           buffers = M.insert (bkey b) b (buffers e)}
                       

-- Prevent possible space leaks in the editor structure
forceFold1 :: (Foldable t) => t a -> t a
forceFold1 x = foldr seq x x

-- | Delete a buffer (and release resources associated with it).
deleteBuffer :: BufferRef -> EditorM ()
deleteBuffer k = do
  -- If the buffer has an associated close action execute that now. Unless the buffer is the last
  -- buffer in the editor. In which case it cannot be closed and, I think, the close action should
  -- not be applied.
  -- 
  -- The close actions seem dangerous, but I know of no other simple way to resolve issues related
  -- to what buffer receives actions after the minibuffer closes.
  pure length <*> gets bufferStack 
    >>= \l -> case l of
        1 -> return ()
        _ -> pure (M.lookup k) <*> gets onCloseActions 
                >>= \m_action -> case m_action of
                    Nothing -> return ()
                    Just action -> action
  -- Now try deleting the buffer. Checking, once again, that it is not the last buffer.
  bs <- gets bufferStack
  ws <- gets windows
  case bs of
      (b0:nextB:_) -> do
          let pickOther w = if bufkey w == k then w {bufkey = other} else w
              visibleBuffers = fmap bufkey ws
              other = head $ (bs \\ visibleBuffers) ++ (delete k bs)
          when (b0 == k) $ do
              -- we delete the currently selected buffer: the next buffer will become active in
              -- the main window, therefore it must be assigned a new window.
              switchToBufferE nextB
          -- all windows open on that buffer must switch to another buffer.
          -- remove the key of the deleted buffer from all bufAccessList data members.
          modifyWindowsE (\w -> let w' = pickOther w
                                in w' { bufAccessList = forceFold1 . filter (k/=) $ bufAccessList w' }
                         )
          modify $ \e -> e {bufferStack = forceFold1 $ filter (k /=) $ bufferStack e,
                            buffers = M.delete k (buffers e)
                           }
      _ -> return () -- Don't delete the last buffer.

-- | Return the buffers we have, /in no particular order/
bufferSet :: Editor -> [FBuffer]
bufferSet = M.elems . buffers

-- | Return a prefix that can be removed from all buffer paths while keeping them
-- unique.
commonNamePrefix :: Editor -> [String]
commonNamePrefix = commonPrefix . fmap (dropLast . splitPath) . rights . fmap (^. identA) . bufferSet
    where dropLast [] = []
          dropLast x = init x
          -- drop the last component, so that it is never hidden.

getBufferStack :: EditorM [FBuffer]
getBufferStack = do
  bufMap <- gets buffers
  gets (fmap (bufMap M.!) . bufferStack)

findBuffer :: BufferRef -> EditorM (Maybe FBuffer)
findBuffer k = gets (M.lookup k . buffers)

-- | Find buffer with this key
findBufferWith :: BufferRef -> Editor -> FBuffer
findBufferWith k e = 
    case M.lookup k (buffers e) of
        Just b  -> b
        Nothing -> error "Editor.findBufferWith: no buffer has this key"


-- | Find buffer with this name
findBufferWithName :: String -> Editor -> [BufferRef]
findBufferWithName n e = map bkey $ filter (\b -> shortIdentString (commonNamePrefix e) b == n) (M.elems $ buffers e)

-- | Find buffer with given name. Fail if not found.
getBufferWithName :: String -> EditorM BufferRef
getBufferWithName bufName = do
  bs <- gets $ findBufferWithName bufName
  case bs of
    [] -> fail ("Buffer not found: " ++ bufName)
    (b:_) -> return b

-- | Make all buffers visible by creating a new view in the current tab onto a buffer editing activity.
-- FIXME: rename to displayAllBuffersE; make sure buffers are not open twice.
openAllBuffersE :: EditorM ()
openAllBuffersE = do 
    bs <- gets bufferSet
    forM_ bs $ addBufferEditActivityE . bkey

------------------------------------------------------------------------

-- | Rotate the buffer stack by the given amount.
shiftBuffer :: Int -> EditorM ()
shiftBuffer shift = do 
    modA bufferStackA rotate
    fixCurrentWindow
  where rotate l = take len $ drop (shift `mod` len) $ cycle l
            where len = length l

------------------------------------------------------------------------
-- | Perform action with any given buffer, using the last window that was used for that buffer.
withGivenBuffer0 :: BufferRef -> BufferM a -> EditorM a
withGivenBuffer0 k f = do
    b <- gets (findBufferWith k)

    withGivenBufferAndWindow0 (b ^. lastActiveWindowA) k f

-- | Perform action with any given buffer
withGivenBufferAndWindow0 :: Window -> BufferRef -> BufferM a -> EditorM a
withGivenBufferAndWindow0 w k f = do
  accum <- asks configKillringAccumulate
  (us, v) <- getsAndModify $ (\e ->
                            let b = findBufferWith k e
                                (v, us, b') = runBufferFull w b f
                                
                            in (e {buffers = mapAdjust' (const b') k (buffers e),
                                   killring = (if accum && all updateIsDelete us
                                               then foldl (.) id 
                                                    (reverse [krPut dir (R.toString s) | Delete _ dir s <- us])
                                               else id) 
                                              (killring e)
                                  }, (us, v)))
  updHandler <- return . bufferUpdateHandler =<< ask
  unless (null us || null updHandler) $ do
    forM_ updHandler (\h -> withGivenBufferAndWindow0 w k (h us))
  return v

-- | Perform action with current window's buffer
withBuffer0 :: BufferM a -> EditorM a
withBuffer0 f = do
  w <- getA currentWindowA
  withGivenBufferAndWindow0 w (bufkey w) f

currentWindowA :: Accessor Editor Window
currentWindowA = PL.focusA . groupActivitiesA . currentActivityA . currentTabA

-- | Return the current buffer
currentBuffer :: Editor -> BufferRef
currentBuffer = head . bufferStack

-----------------------
-- Handling of status

-- | Display a transient message
printMsg :: String -> EditorM ()
printMsg s = printStatus ([s], defaultStyle)

printMsgs :: [String] -> EditorM ()
printMsgs s = printStatus (s, defaultStyle)

printStatus :: Status -> EditorM ()
printStatus = setTmpStatus 1

-- | Set the "background" status line 
setStatus :: Status -> EditorM ()
setStatus = setTmpStatus maxBound

-- | Clear the status line
clrStatus :: EditorM ()
clrStatus = setStatus ([""], defaultStyle)

statusLine :: Editor -> [String]
statusLine = fst . statusLineInfo

statusLineInfo :: Editor -> Status
statusLineInfo = snd . head . statusLines


setTmpStatus :: Int -> Status -> EditorM ()
setTmpStatus delay s = do
  modA statusLinesA $ DelayList.insert (delay, s)
  -- also show in the messages buffer, so we don't loose any message
  bs <- gets (filter (\b -> b ^. identA == Left "messages") . M.elems . buffers)

  b <- case bs of
         (b':_) -> return $ bkey b'
         [] -> stringToNewBuffer (Left "messages") (R.fromString "")
  withGivenBuffer0 b $ do botB; insertN (show s ++ "\n")


-- ---------------------------------------------------------------------
-- kill-register (vim-style) interface to killring.

-- | Put string into yank register
setRegE :: String -> EditorM ()
setRegE s = modA killringA $ krSet s

-- | Return the contents of the yank register
getRegE :: EditorM String
getRegE = getsA killringA krGet



-- ---------------------------------------------------------------------
-- | Dynamically-extensible state components.
--
-- These hooks are used by keymaps to store values that result from
-- Actions (i.e. that restult from IO), as opposed to the pure values
-- they generate themselves, and can be stored internally.
--
-- The `dynamic' field is a type-indexed map.
--

-- | Retrieve a value from the extensible state
getDynamic :: YiVariable a => EditorM a
getDynamic = getA (dynamicValueA . dynamicA)

-- | Insert a value into the extensible state, keyed by its type
setDynamic :: YiVariable a => a -> EditorM ()
setDynamic x = putA (dynamicValueA . dynamicA) x

-- | Attach the next buffer in the buffer stack to the current window.
nextBufW :: EditorM ()
nextBufW = shiftBuffer 1

-- | Attach the previous buffer in the stack list to the current window.
prevBufW :: EditorM ()
prevBufW = shiftBuffer (negate 1)

-- | Like fnewE, create a new buffer filled with the String @s@,
-- Switch the current window to this buffer. Doesn't associate any file
-- with the buffer (unlike fnewE) and so is good for popup internal
-- buffers (like scratch)
newBufferE :: BufferId   -- ^ buffer name
              -> Rope -- ^ buffer contents
              -> EditorM BufferRef
newBufferE f s = do
    b <- stringToNewBuffer f s
    switchToBufferE b
    return b

-- | Creates an in-memory buffer with a unique name. 
-- 
-- A hint for the buffer naming scheme can be specified in the dynamic variable TempBufferNameHint
-- The new buffer always has a buffer ID that did not exist before newTempBufferE.
-- TODO: this probably a lot more complicated than it should be: why not count from zero every time?
newTempBufferE :: EditorM BufferRef
newTempBufferE = do
    hint :: TempBufferNameHint <- getDynamic
    e <- gets id
    -- increment the index of the hint until no buffer is found with that name
    let find_next in_name = 
            case findBufferWithName (show in_name) e of
                (_b : _) -> find_next $ inc in_name
                []      -> in_name
        inc in_name = TempBufferNameHint (tmp_name_base in_name) (tmp_name_index in_name  + 1)
        next_tmp_name = find_next hint
        
    b <- newBufferE (Left $ show next_tmp_name)
                    (R.fromString "")
    setDynamic $ inc next_tmp_name
    return b

-- | Specifies the hint for the next temp buffer's name.
data TempBufferNameHint = TempBufferNameHint
    { tmp_name_base :: String
    , tmp_name_index :: Int
    } deriving Typeable

instance Show TempBufferNameHint where
    show (TempBufferNameHint s i) = s ++ "-" ++ show i

alternateBufferE :: Int -> EditorM ()
alternateBufferE n = do
    Window { bufAccessList = lst } <- getA currentWindowA
    if null lst || (length lst - 1) < n
      then fail "no alternate buffer"
      else switchToBufferE $ lst!!n

-- | Create a new zero size window on a given buffer
newZeroSizeWindow ::Bool -> BufferRef -> WindowRef -> Window
newZeroSizeWindow mini bk ref = Window mini bk [] 0 0 emptyRegion ref 0

-- | Create a new window onto the given buffer.
newWindowE :: Bool -> BufferRef -> EditorM Window
newWindowE mini bk = newZeroSizeWindow mini bk . WindowRef <$> newRef

-- | Attach the specified buffer to the current window
switchToBufferE :: BufferRef -> EditorM ()
switchToBufferE bk = do
    modA currentWindowA (\w -> 
           w { bufkey = bk, 
               bufAccessList = forceFold1 $ ((bufkey w):) . filter (bk/=) $ bufAccessList w })

-- | Switch to the buffer specified as parameter. If the buffer name is empty, switch to the next buffer.
switchToBufferWithNameE :: String -> EditorM ()
switchToBufferWithNameE "" = alternateBufferE 0
switchToBufferWithNameE bufName = switchToBufferE =<< getBufferWithName bufName

-- | Close a buffer.
-- Note: close the current buffer if the empty string is given
closeBufferE :: String -> EditorM ()
closeBufferE nm = deleteBuffer =<< getBufferWithNameOrCurrent nm

getBufferWithNameOrCurrent :: String -> EditorM BufferRef
getBufferWithNameOrCurrent nm = if null nm then gets currentBuffer else getBufferWithName nm


------------------------------------------------------------------------

-- | Close current buffer and window, unless it's the last one.
closeBufferAndWindowE :: EditorM ()
closeBufferAndWindowE = do
  -- Fetch the current buffer *before* closing the window. 
  -- Required for the onCloseBufferE actions to work as expected by the minibuffer.
  -- The tryCloseE, since it uses tabsA, will have the current buffer "fixed" to the buffer of the
  -- window that is brought into focus. If the current buffer is accessed after the tryCloseE then
  -- the current buffer may not be the same as the buffer before tryCloseE. This would be bad.
  b <- gets currentBuffer
  tryCloseE
  deleteBuffer b

-- | Rotate focus to the next window
nextWinE :: EditorM ()
nextWinE = do
    -- If the current focus in the current activity group is at the end 
    isAtEnd <- PL.atEnd <$> getA (groupActivitiesA . currentActivityA . currentTabA)
    if isAtEnd
    -- then move to the next activity group and set the focus to the first of the activities
        then do
            modA (tabActivitiesA . currentTabA) PL.next
            modA (groupActivitiesA . currentActivityA . currentTabA) (\as -> fromJust $! PL.move 0 as)
    -- else move to the next focus in the activity group
        else modA (groupActivitiesA . currentActivityA . currentTabA) PL.next

-- | Rotate focus to the previous window
prevWinE :: EditorM ()
prevWinE = do
    -- If the current focus in the current activity group is at the start
    isAtStart <- PL.atStart <$> getA (groupActivitiesA . currentActivityA . currentTabA)
    if isAtStart
    -- then move to the previous activity group and set the focus to the last of the activities
        then do
            modA (tabActivitiesA . currentTabA) PL.previous
            modA (groupActivitiesA . currentActivityA . currentTabA) (\as -> fromJust $! PL.move (PL.length as - 1) as)
    -- else move to the next focus in the activity group
        else modA (groupActivitiesA . currentActivityA . currentTabA) PL.previous

-- | Swaps the focused activity group with the first activity group. Useful for layouts such as
-- 'HPairOneStack', for which the first activity group has the most screen space.
swapWinWithFirstE :: EditorM ()
swapWinWithFirstE = modA (tabActivitiesA . currentTabA) (swapFocus (fromJust . PL.move 0))

-- | Moves the focused activity group to the first window, and moves all other groups down the
-- stack.
pushWinToFirstE :: EditorM ()
pushWinToFirstE = modA (tabActivitiesA . currentTabA) pushToFirst
  where
      pushToFirst ags = case PL.delete ags of
          Nothing -> ags
          Just ags' -> PL.insertLeft (ags ^. PL.focusA) (fromJust $ PL.move 0 ags')

-- | Swap focused window with the next one
moveWinNextE :: EditorM ()
moveWinNextE = modA (tabActivitiesA . currentTabA) (swapFocus PL.next)

-- | Swap focused window with the previous one
moveWinPrevE :: EditorM ()
moveWinPrevE = modA (tabActivitiesA . currentTabA) (swapFocus PL.previous)

-- | A "fake" accessor that fixes the current buffer after a change of the current
-- window. 
-- Enforces invariant that top of buffer stack is the buffer of the current window.
fixCurrentBufferA_ :: Accessor Editor Editor
fixCurrentBufferA_ = fromSetGet (\new _old -> let 
    focusedWin = Accessor.get (PL.focusA . groupActivitiesA . currentActivityA . currentTabA) new
    b = findBufferWith (bufkey focusedWin) new
    newBufferStack = nub (bkey b : bufferStack new)
    -- make sure we do not hold to old versions by seqing the length.
    in length newBufferStack `seq` new { bufferStack = newBufferStack  } ) id
    

-- | Counterpart of fixCurrentBufferA_: fix the current window to point to the
-- right buffer.
fixCurrentWindow :: EditorM ()
fixCurrentWindow = do
    b <- gets currentBuffer
    modA (PL.focusA . groupActivitiesA . currentActivityA . currentTabA) (\w -> w {bufkey = b})

withWindowE :: Window -> BufferM a -> EditorM a
withWindowE w = withGivenBufferAndWindow0 w (bufkey w)

findWindowWith :: WindowRef -> Editor -> Window
findWindowWith k e =
    head $ concatMap (\win -> if (wkey win == k) then [win] else []) $ windows e

-- | Return the windows that are currently open on the buffer whose key is given
windowsOnBufferE :: BufferRef -> EditorM [Window]
windowsOnBufferE k = do
  ts <- getA tabsA
  return $ concatMap (concatMap (\win -> if (bufkey win == k) then [win] else []) . tabWindows) ts

-- | bring the editor focus the window with the given key.
--
-- Fails if no window with the given key is found.
focusWindowE :: WindowRef -> EditorM ()
focusWindowE k = do
    -- Find the tab index, the activity group, and window index
    -- by iterating through all the values until a window with the given ID is found.
    ts <- getA tabsA
    let searchWins (False, i) win = if wkey win == k 
                                    then (True, i)
                                    else (False, i + 1)
        searchWins r@(True, _) _win = r

        searchActivities (False, activityIndex, _) ag = 
            case foldl searchWins (False, 0) (ag ^. groupActivitiesA) of
                (True, winIndex) -> (True, activityIndex, winIndex)
                (False, _)       -> (False, activityIndex + 1, 0)
        searchActivities r@(True, _, _) _ag = r

        searchTabs (False, tabIndex, _, _) tab =
            case foldl searchActivities (False, 0, 0) (tab ^. tabActivitiesA) of
                (True, activityIndex, winIndex) -> (True, tabIndex, activityIndex, winIndex)
                (False, _, _)                   -> (False, tabIndex + 1, 0, 0)
        searchTabs r@(True, _, _, _) _tab = r

    case foldl searchTabs  (False, 0, 0, 0) ts of
        (False, _, _, _) -> fail $ "No window with key " ++ show wkey ++ "found. (focusWindowE)"
        (True, tabIndex, activityIndex, winIndex) -> do
            putA tabsA (fromJust $ PL.move tabIndex ts) 
            modA (tabActivitiesA . currentTabA) (\as -> fromJust $! PL.move activityIndex as)
            modA (groupActivitiesA . currentActivityA . currentTabA) (\as -> fromJust $! PL.move winIndex as)

-- | Split the current activity according to the provided orientation. Creates a new view onto the
-- activity grouped with the current activity. The new view will be placed according to AddSide and
-- will become the new focus.
--
-- "split the view into a <orientation> group of views"
splitE :: Orientation -> AddSide -> EditorM ()
splitE o addSide = do
  b <- gets currentBuffer
  w <- newWindowE False b
  dividerRef <- newRef
  modA (currentActivityA . currentTabA) (addView w o addSide dividerRef)

-- | Cycle to the next layout manager, or the first one if the current one is nonstandard.
layoutManagersNextE :: EditorM ()
layoutManagersNextE = withLMStack PL.next

-- | Cycle to the previous layout manager, or the first one if the current one is nonstandard.
layoutManagersPreviousE :: EditorM ()
layoutManagersPreviousE = withLMStack PL.previous

-- | Helper function for 'layoutManagersNext' and 'layoutManagersPrevious'
withLMStack :: (PL.PointedList AnyLayoutManager -> PL.PointedList AnyLayoutManager) -> EditorM ()
withLMStack f = askCfg >>= \cfg -> modA (tabLayoutManagerA . currentTabA) (go (layoutManagers cfg))
  where
     go [] lm = lm
     go lms lm =
       case findPL (layoutManagerSameType lm) lms of
         Nothing -> head lms
         Just lmsPL -> f lmsPL ^. PL.focusA

-- | Next variant of the current layout manager, as given by 'nextVariant'
layoutManagerNextVariantE :: EditorM ()
layoutManagerNextVariantE = modA (tabLayoutManagerA . currentTabA) nextVariant

-- | Previous variant of the current layout manager, as given by 'previousVariant'
layoutManagerPreviousVariantE :: EditorM ()
layoutManagerPreviousVariantE = modA (tabLayoutManagerA . currentTabA) previousVariant

-- | Enlarge the current window
enlargeWinE :: EditorM ()
enlargeWinE = error "enlargeWinE: not implemented"

-- | Shrink the current window
shrinkWinE :: EditorM ()
shrinkWinE = error "shrinkWinE: not implemented"

-- | Sets the given divider position on the current tab
setDividerPosE :: DividerRef -> DividerPosition -> EditorM ()
setDividerPosE ref pos = putA (tabDividerPositionA ref . currentTabA) pos

-- | Creates a new tab containing a window that views the current buffer.
newTabE :: EditorM ()
newTabE = do
    bk <- gets currentBuffer
    win <- newWindowE False bk
    tabRef <- newRef
    agRef <- newRef
    let theTab = makeTab1 tabRef theAG
        theAG = makeActivityGroup1 (ActivityGroupRef agRef) win
    modA tabsA (PL.insertRight theTab)

-- | Moves to the next tab in the round robin set of tabs
nextTabE :: EditorM ()
nextTabE = modA tabsA PL.next

-- | Moves to the previous tab in the round robin set of tabs
previousTabE :: EditorM ()
previousTabE = modA tabsA PL.previous

-- | Moves the focused tab to the given index, or to the end if the index is not specified.
moveTab :: Maybe Int -> EditorM ()
moveTab Nothing  = do count <- getsA tabsA PL.length
                      modA tabsA $ fromJust . PL.move (pred count)
moveTab (Just n) = do newTabs <- getsA tabsA (PL.move n)
                      when (isNothing newTabs) failure
                      putA tabsA $ fromJust newTabs
  where failure = fail $ "moveTab " ++ show n ++ ": no such tab"

-- | Deletes the current tab. If there is only one tab open then error out.
--   When the last tab is focused, move focus to the left, otherwise
--   move focus to the right.
deleteTabE :: EditorM ()
deleteTabE = modA tabsA $ maybe failure id . deleteTab
  where failure = error "deleteTab: cannot delete sole tab"
        deleteTab tabs = case PL.atEnd tabs of
                           True ->  PL.deleteLeft tabs
                           False -> PL.deleteRight tabs

-- | Adds a buffer editing activity to the current tab.
addBufferEditActivityE :: BufferRef -> EditorM ()
addBufferEditActivityE bref = do
    win <- newWindowE False bref
    aref <- newRef
    let a = makeActivityGroup1 (ActivityGroupRef aref) win 
    modA currentTabA (addActivity AddRight a)
    
-- | Close the current view as long as there is another view to switch focus to.
-- If the current view is the only view in the editor then do nothing.
tryCloseE :: EditorM ()
tryCloseE = do
    ag <- getA $ currentActivityA . currentTabA
    case deleteFocusedView ag of
        -- if deleting the focused view would result in an empty activity then delete the activity.
        Nothing -> do
            ags <- getA $ tabActivitiesA . currentTabA
            case PL.deleteLeft ags of
                -- if deleting the activity would result in an empty tab then delete the tab.
                Nothing -> do
                    tabs <- getA $ tabsA
                    case PL.deleteLeft tabs of
                        -- if deleting the tab would result in no tabs then do nothing.
                        Nothing -> return ()
                        -- otherwise commit the deletion of the tab.
                        Just tabs' -> putA tabsA tabs'
                -- otherwise commit the deletion of the activity
                Just ags' -> putA (tabActivitiesA . currentTabA) ags'
        -- otherwise commit the deletion of the activity view
        Just ag' -> putA ( currentActivityA . currentTabA ) ag'

-- | Make the current view the only view in the current activity.
closeOtherViewsE :: EditorM ()
closeOtherViewsE = modA (currentActivityA . currentTabA) deleteOtherViews

-- | Make the current activity the only activity in the current tab.
closeOtherActivitiesE :: EditorM ()
closeOtherActivitiesE = modA (tabActivitiesA . currentTabA) PL.deleteOthers

-- | close all other views and activities.
closeOtherE :: EditorM ()
closeOtherE = do
    closeOtherViewsE
    closeOtherActivitiesE

-- | Switch focus to some other window. If none is available, create one.
shiftOtherWindow :: MonadEditor m => m ()
shiftOtherWindow = liftEditor $ do
  len <- getsA (groupActivitiesA . currentActivityA . currentTabA) PL.length
  if (len == 1) 
    then splitE Vertical AddRight
    else nextWinE

-- | Execute the argument in the context of an other window. Create
-- one if necessary. The current window is re-focused after the
-- argument has completed.
withOtherWindow :: MonadEditor m => m a -> m a
withOtherWindow f = do
  shiftOtherWindow
  x <- f
  liftEditor prevWinE
  return x

-- | Attach the specified buffer to some other window than the current one
switchToBufferOtherWindowE :: BufferRef -> EditorM ()
switchToBufferOtherWindowE b = shiftOtherWindow >> switchToBufferE b

acceptedInputs :: EditorM [String]
acceptedInputs = do
    cfg <- askCfg
    keymap <- withBuffer0 $ gets (withMode0 modeKeymap)
    let l = I.accepted 3 $ I.mkAutomaton $ extractTopKeymap $ keymap $ defaultKm cfg
    return $ fmap (intercalate " ") l

-- | Defines an action to be executed when the current buffer is closed. 
--
-- Used by the minibuffer to assure the focus is restored to the buffer that spawned the minibuffer.
--
-- todo: These actions are not restored on reload.
--
-- todo: These actions should probably be very careful at what they do.
-- TODO: All in all, this is a very ugly way to achieve the purpose. The nice way to proceed
-- is to somehow attach the miniwindow to the window that has spawned it.
onCloseBufferE :: BufferRef -> EditorM () -> EditorM ()
onCloseBufferE b a = do
    modA onCloseActionsA $ M.insertWith' (\_ old_a -> old_a >> a) b a
    
-- put the template haskell at the end, to avoid 'variable not found' compile errors
$(derive makeBinary ''TempBufferNameHint)

-- For GHC 7.0 with template-haskell 2.5 (at least on my computer - coconnor) the Binary instance
-- needs to be defined before the YiVariable instance. 
--
-- GHC 7.1 does not appear to have this issue.
instance Initializable TempBufferNameHint where
    initial = TempBufferNameHint "tmp" 0

instance YiVariable TempBufferNameHint

