{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | An activity group is a group of related activities arranged according to a layout. Unlike with
-- the layout of activity groups in a tab, in the case of an activity window there is no layout
-- manager paired with the activity group. 
module Yi.ActivityGroup
where

import Prelude()
import Yi.Prelude

import Yi.Buffer.Basic
import Yi.Layout
import Yi.Window

import Control.Monad ( mzero )

import Data.Accessor.Basic
import qualified Data.Binary as Binary
import Data.List (map)
import Data.Monoid ( First(..) )
import qualified Data.List.PointedList as PL

-- | An activity group is a layout of 'Window's with a unique ID.
-- TODO: Abstract Window into ActivityView. 
data ActivityGroup = ActivityGroup
    { activityGroupRef :: !ActivityGroupRef
    -- The miniwindows will be in the groupActivities list but not in the groupLayout. A miniwindow
    -- is always placed along the bottom of its "target" window. The target window is always the
    -- window to the left of the miniwindow.
    , groupActivities :: !(PL.PointedList Window)
    , groupLayout :: !(Layout WindowRef)
    } deriving (Typeable) 

instance NFData ActivityGroup where
    rnf (ActivityGroup {activityGroupRef, groupActivities, groupLayout})
        = rnf activityGroupRef `seq` rnf groupActivities `seq` rnf groupLayout 

groupActivitiesA :: Accessor ActivityGroup (PL.PointedList Window)
groupActivitiesA = fromSetGet
    (\groupActivities' ag -> ag { groupActivities = groupActivities' } )
    (\ag -> groupActivities ag) 

currentViewA :: Accessor ActivityGroup Window
currentViewA = PL.focusA . groupActivitiesA

lookupChildView :: WindowRef -> ActivityGroup -> Maybe Window
lookupChildView r ag = getFirst $ foldMap (\win -> if r == wkey win then First $ Just win 
                                                                    else First Nothing 
                                          ) 
                                          (groupActivities ag)

newtype ActivityGroupRef = ActivityGroupRef Int
    deriving (Eq, Ord, Show, Typeable, Binary, NFData)

instance Initializable ActivityGroupRef where
    initial = ActivityGroupRef (-1)
    
instance Eq ActivityGroup where
    ag0 == ag1 = activityGroupRef ag0 == activityGroupRef ag1

instance Binary ActivityGroup where
    put (ActivityGroup {activityGroupRef, groupActivities, groupLayout} )  
        = Binary.put activityGroupRef >> Binary.put groupActivities  >> Binary.put groupLayout
    get = ActivityGroup <$> Binary.get <*> Binary.get <*> Binary.get

instance Show ActivityGroup where
    show ag = "ActivityGroup " ++ show (activityGroupRef ag)

makeActivityGroup1 :: ActivityGroupRef -> Window -> ActivityGroup
makeActivityGroup1 ref win 
    = ActivityGroup ref (PL.singleton win) (SingleWindow (wkey win))

mapGroupWindows :: ( Window -> Window ) -> ActivityGroup -> ActivityGroup
mapGroupWindows f g = g { groupActivities = fmap f (groupActivities g) }

modifyLayoutNodeEq :: WindowRef 
                   -> (Layout WindowRef -> Layout WindowRef)
                   -> Layout WindowRef
                   -> Layout WindowRef

modifyLayoutNodeEq w_0 f (SingleWindow w_1)
    | w_0 == w_1 = f (SingleWindow w_1)
    | otherwise  = SingleWindow w_1

modifyLayoutNodeEq w_0 f (Stack o subLayout) 
    = Stack o (map (\(l,s) -> (modifyLayoutNodeEq w_0 f l, s)) subLayout)

modifyLayoutNodeEq w_0 f (Pair { orientation, divPos, divRef, pairFst, pairSnd } )
    = Pair orientation divPos divRef (modifyLayoutNodeEq w_0 f pairFst) (modifyLayoutNodeEq w_0 f pairSnd)

-- add the given view positioned relative to the focus.
-- Cannot be used with miniwindows
addView :: Window -> Orientation -> AddSide -> Int -> ActivityGroup -> ActivityGroup
addView w o side newID ag 
    | isMini w == True 
        = error "addView cannot be used with miniwindows"
    | otherwise 
        = ag { groupLayout = modifyLayoutNodeEq (wkey $ get PL.focusA $ groupActivities ag)
                                                (splitAdd o side (wkey w) newID)
                                                (groupLayout ag)
             , groupActivities = ( case side of
                                    AddLeft -> PL.insertLeft 
                                    AddRight -> PL.insertRight
                                 ) w (groupActivities ag)
             }

-- delete the focused view from the activity.
-- if the view participates in layout (it's not a miniwindow) then the layout is adjusted
-- accordingly.
deleteFocusedView :: ActivityGroup -> Maybe ActivityGroup
deleteFocusedView ag = do
    groupActivities' <- PL.deleteLeft (groupActivities ag)
    groupLayout' <- deleteViewFromLayout (groupLayout ag)
    return $ ag { groupActivities = groupActivities', groupLayout = groupLayout' }
    where
        targetWin = wkey $ get PL.focusA $ (groupActivities ag)
        deleteViewFromLayout (SingleWindow w) 
            | w == targetWin = mzero
            | otherwise      = return $ (SingleWindow w)
        -- assumes that stack is never empty 
        deleteViewFromLayout ( Stack _o [] ) 
            = mzero
        deleteViewFromLayout ( Stack _o [(sublayout, _)] ) 
            = deleteViewFromLayout sublayout 
        deleteViewFromLayout ( Stack o0 ((sl, slPos) : stack0) )
            = case deleteViewFromLayout sl of
                -- assumes no view ref is in the layout multiple times
                Nothing -> return $ Stack o0 stack0
                Just sl' -> case deleteViewFromLayout (Stack o0 stack0) of
                    Nothing -> return $ sl'
                    Just w@(SingleWindow {}) -> return $ Stack o0 ( (sl', slPos) : (w, 1) : [] )
                    Just w@(Stack o1 stack1) 
                        | o1 == o0  -> return $ Stack o0 ( (sl', slPos) : stack1)
                        | otherwise -> return $ Stack o0 ( (sl', slPos) : (w, 1) : [] )
                    Just w -> return $ Stack o0 ( (sl', slPos) : (w,1) : [] )
        deleteViewFromLayout ( Pair o divPos divRef sublayoutFst sublayoutSnd )
            = case deleteViewFromLayout sublayoutFst of
                Nothing -> return $ sublayoutSnd
                Just sublayoutFst' -> case deleteViewFromLayout sublayoutSnd of
                    Nothing -> return sublayoutFst'
                    Just sublayoutSnd' -> return $ Pair o divPos divRef sublayoutFst' sublayoutSnd'

-- cannot be used on miniwindows
deleteOtherViews :: ActivityGroup -> ActivityGroup
deleteOtherViews ag 
    | isMini ( PL.focus (groupActivities ag) ) == True 
        = error "deleteOtherViews cannot be used with miniwindows"
    | otherwise = 
        let groupActivities' = PL.deleteOthers $ groupActivities ag
            groupLayout' = SingleWindow (wkey $ PL.focus groupActivities')
        in ag { groupActivities = groupActivities', groupLayout = groupLayout' }

