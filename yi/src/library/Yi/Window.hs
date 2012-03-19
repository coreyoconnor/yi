{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving #-}

--
-- Copyright (c) 2008 JP Bernardy
--
--

module Yi.Window where

import qualified Prelude
import Yi.Prelude
import Data.Binary
import Yi.Buffer.Basic (BufferRef, WindowRef)
import Yi.Region (Region,emptyRegion)

------------------------------------------------------------------------
-- | A window is a view of an activity. 
data Window = Window {
                      isMini    :: !Bool   -- ^ regular or mini window?
                     ,bufkey    :: !BufferRef -- ^ the buffer this window opens to
                     ,bufAccessList :: ![BufferRef] -- ^ list of last accessed buffers (former bufKeys). Last accessed one is first element
                     ,height    :: Int    -- ^ height of the window (integer number of Ems available vertically for display)
                     ,width     :: Int    -- ^ width of the window (integer number of Ems available horizontally for display)
                     ,winRegion    :: Region -- ^ view area.
                                              -- note that the top point is also available as a buffer mark.
                     ,wkey      :: !WindowRef -- ^ identifier for the window (for UI sync)
                     -- This is required for accurate scrolling.
                     -- Scrolling depends on the actual number of buffer
                     -- lines displayed. Line wrapping changes that number
                     -- relative to the height so we can't use height for that
                     -- purpose.
                     ,actualLines :: Int-- ^ The actual number of buffer lines displayed. Taking into account line wrapping
                     }
        deriving (Typeable)

instance NFData Window where
    rnf (Window {isMini, bufkey, bufAccessList, height, width, winRegion, wkey, actualLines}) 
        =       rnf isMini 
          `seq` rnf bufkey 
          `seq` rnf bufAccessList 
          `seq` rnf height 
          `seq` rnf width 
          `seq` rnf winRegion 
          `seq` rnf wkey
          `seq` rnf actualLines

instance Binary Window where
    put (Window mini bk bl _h _w _rgn key lns) = put mini >> put bk >> put bl >> put key >> put lns
    get = Window <$> get <*> get <*> get
                   <*> return 0 <*> return 0 <*> return emptyRegion
                   <*> get <*> get

instance Show Window where
    show w = "Window to " ++ show (bufkey w) 
             -- ++ "{" ++ show (tospnt w) ++ "->" ++ show (bospnt w) ++ "}" 
             ++ "(" ++ show (height w) ++ ")"

instance Eq Window where
    (==) w1 w2 = wkey w1 == wkey w2

{-
-- | Is a given point within tospnt / bospnt?
pointInWindow :: Point -> Window -> Bool
pointInWindow point win = tospnt win <= point && point <= bospnt win
-}

-- | Return a "fake" window onto a buffer.
dummyWindow :: BufferRef -> Window
dummyWindow b = Window False b [] 0 0 emptyRegion initial 0

