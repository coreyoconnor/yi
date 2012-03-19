module Yi.UI.Vty.Render where

import Yi.Prelude hiding ( (<|>) )
import Prelude ()

import Yi.Buffer
import Yi.Config
import Yi.Editor
import Yi.Style
import Yi.UI.TabBar
import Yi.UI.Vty.Data
import Yi.UI.Utils
import Yi.Window

import Control.Arrow

import Data.Char (ord,chr)
import Data.List ( length
                 , break
                 , map
                 , partition
                 , take
                 , repeat
                 , splitAt
                 , zip 
                 )
import qualified Data.List.PointedList as PL
import Data.Maybe ( listToMaybe, maybeToList )
import Data.Monoid

import Graphics.Vty ( Image
                    , (<|>)
                    , vert_cat
                    , horiz_cat  
                    , char_fill
                    )
import qualified Graphics.Vty  as Vty

-- | Construct images for the tabbar if at least one tab exists.
renderTabBar :: UI -> Editor -> Int -> [Image]
renderTabBar ui e xss =
  if hasTabBar e ui
    then [tabImages <|> extraImage]
    else []
  where tabImages       = foldr1 (<|>) $ fmap tabToVtyImage $ tabBarDescr e
        extraImage      = withAttributes (tabBarAttributes uiStyle) (replicate (xss - fromEnum totalTabWidth) ' ')

        totalTabWidth   = Vty.image_width tabImages
        uiStyle         = configStyle $ configUI $ config $ ui
        tabTitle text   = " " ++ text ++ " "
        baseAttr b sty  = if b then attributesToAttr (appEndo (tabInFocusStyle uiStyle) sty) Vty.def_attr
                               else attributesToAttr (appEndo (tabNotFocusedStyle uiStyle) sty) Vty.def_attr `Vty.with_style` Vty.underline
        tabAttr b       = baseAttr b $ tabBarAttributes uiStyle
        tabToVtyImage _tab@(TabDescr text inFocus) = Vty.string (tabAttr inFocus) (tabTitle text)

-- | Produce the rendering data for a view. Does not actually cause it to appear on screen.
-- 
-- TODO: horizontal scrolling.
buildViewRender :: UIConfig -> Editor -> Window -> (Int, Int) -> Bool -> ViewRender
buildViewRender cfg e win (renderWidth, renderHeight) hasFocus  =
    ViewRender 
    { viewImage = pict
    , viewCursor = if hasFocus then cur else Nothing
    , displayedLines = dispLnCount
    , displayedRegion = mkRegion fromMarkPoint toMarkPoint'
    }
    where
        b = findBufferWith (bufkey win) e
        sty = configStyle cfg
        
        notMini = not (isMini win)
        -- off reserves space for the mode line. The mini window does not have a mode line.
        off = if notMini then 1 else 0
        renderHeight' = renderHeight - off
        ground = baseAttributes sty
        wsty = attributesToAttr ground Vty.def_attr
        eofsty = appEndo (eofStyle sty) ground
        (point, _) = runBuffer win b pointB
        (eofPoint, _) = runBuffer win b sizeB
        region = mkSizeRegion fromMarkPoint (Size (renderWidth*renderHeight'))
        -- Work around a problem with the mini window never displaying it's contents due to a
        -- fromMark that is always equal to the end of the buffer contents.
        (Just (MarkSet fromM _ _), _) = runBuffer win b (getMarks win)
        fromMarkPoint = if notMini
                            then fst $ runBuffer win b (getMarkPointB fromM)
                            else Point 0
        (text, _)    = runBuffer win b (indexedAnnotatedStreamB fromMarkPoint) -- read chars from the buffer, lazily
        
        (attributes, _) = runBuffer win b $ attributesPictureAndSelB sty (currentRegex e) region 
        -- TODO: I suspect that this costs quite a lot of CPU in the "dry run" which determines the window size;
        -- In that case, since attributes are also useless there, it might help to replace the call by a dummy value.
        -- This is also approximately valid of the call to "indexedAnnotatedStreamB".
        colors = map (second (($ Vty.def_attr) . attributesToAttr)) attributes
        bufData = -- trace (unlines (map show text) ++ unlines (map show $ concat strokes)) $ 
                  paintChars Vty.def_attr colors text
        tabWidth = tabSize . fst $ runBuffer win b indentSettingsB
        prompt = if isMini win then miniIdentString b else ""

        (rendered,toMarkPoint',cur,dispLnCount) = drawText renderHeight' renderWidth
                                fromMarkPoint
                                point 
                                tabWidth
                                ([(c,(wsty, (-1))) | c <- prompt] ++ bufData ++ [(' ',(wsty, eofPoint))])
                             -- we always add one character which can be used to position the cursor at the end of file
        (modeLine0, _) = runBuffer win b $ getModeLine (commonNamePrefix e)
        modeLine = if notMini then Just modeLine0 else Nothing
        modeLines = map (withAttributes modeStyle . take renderWidth . (++ repeat ' ')) $ maybeToList $ modeLine
        modeStyle = (if hasFocus then appEndo (modelineFocusStyle sty) else id) (modelineAttributes sty)
        filler = take renderWidth (configWindowFill cfg : repeat ' ')
    
        pict = vert_cat (take renderHeight' (rendered ++ repeat (withAttributes eofsty filler)) ++ modeLines)
  
-- | Renders text in a rectangle.
-- This also returns 
-- * the index of the last character fitting in the rectangle
-- * the position of the Point in (x,y) coordinates, if in the window,
-- * the number of display lines for this drawing.
--
-- We calculate the number of lines displayed for this window so that line
-- wrapping doesn't break scrolling.
drawText :: Int    -- ^ The height of the part of the window we are in
         -> Int    -- ^ The width of the part of the window we are in
         -> Point  -- ^ The position of the first character to draw
         -> Point  -- ^ The position of the cursor
         -> Int    -- ^ The number of spaces to represent a tab character with.
         -> [(Char,(Vty.Attr,Point))]  -- ^ The data to draw.
         -> ([Image], Point, Maybe (Int,Int), Int)
drawText h w topPoint point tabWidth bufData
    | h == 0 || w == 0 = ([], topPoint, Nothing, 0)
    | otherwise        = (renderedLines, bottomPoint, pntpos, h - (length wrapped - h))
  where 

  -- the number of lines that taking wrapping into account,
  -- we use this to calculate the number of lines displayed.
  wrapped = concatMap (wrapLine w) $ map (concatMap expandGraphic) $ take h $ lines' $ bufData
  lns0 = take h wrapped

  bottomPoint = case lns0 of 
                 [] -> topPoint 
                 _ -> snd $ snd $ last $ last $ lns0

  pntpos = listToMaybe [ (x,y) | (y,l) <- zip [0..] lns0, (x,(_char,(_attr,p))) <- zip [0..] l, p == point]

  -- fill lines with blanks, so the selection looks ok.
  renderedLines = map fillColorLine lns0
  colorChar (c, (a, _aPoint)) = Vty.char a c

  fillColorLine :: [(Char, (Vty.Attr, Point))] -> Image
  fillColorLine [] = char_fill Vty.def_attr ' ' w 1
  fillColorLine l = horiz_cat (map colorChar l) 
                    <|>
                    char_fill a ' ' (w - length l) 1
                    where (_,(a,_x)) = last l

  -- | Cut a string in lines separated by a '\n' char. Note
  -- that we add a blank character where the \n was, so the
  -- cursor can be positioned there.

  lines' :: [(Char,a)] -> [[(Char,a)]]
  lines' [] =  []
  lines' s  = case s' of
                []          -> [l]
                ((_,x):s'') -> (l++[(' ',x)]) : lines' s''
              where
              (l, s') = break ((== '\n') . fst) s

  wrapLine :: Int -> [x] -> [[x]]
  wrapLine _ [] = []
  wrapLine n l = let (x,rest) = splitAt n l in x : wrapLine n rest
                                      
  expandGraphic ('\t', p) = replicate tabWidth (' ', p)
  expandGraphic (c,p) 
    | ord c < 32 = [('^',p),(chr (ord c + 64),p)]
    | otherwise = [(c,p)]

withAttributes :: Attributes -> String -> Image
withAttributes sty str = Vty.string (attributesToAttr sty Vty.def_attr) str

------------------------------------------------------------------------

userForceRefresh :: UI -> IO ()
userForceRefresh = Vty.refresh . vty

-- | Calculate window heights, given all the windows and current height.
-- (No specific code for modelines)
computeHeights :: Int -> PL.PointedList Window -> [Int]
computeHeights totalHeight ws = ((y+r-1) : repeat y)
  where (mwls, wls) = partition isMini (toList ws)
        (y,r) = getY (totalHeight - length mwls) (length wls)

getY :: Int -> Int -> (Int,Int)
getY screenHeight 0               = (screenHeight, 0)
getY screenHeight numberOfWindows = screenHeight `quotRem` numberOfWindows

------------------------------
-- Low-level stuff

------------------------------------------------------------------------

-- | Convert a Yi Attr into a Vty attribute change.
colorToAttr :: (Vty.Color -> Vty.Attr -> Vty.Attr) -> Vty.Color -> Color -> (Vty.Attr -> Vty.Attr)
colorToAttr set unknown c =
  case c of 
    RGB 0 0 0         -> set Vty.black
    RGB 128 128 128   -> set Vty.bright_black
    RGB 139 0 0       -> set Vty.red
    RGB 255 0 0       -> set Vty.bright_red
    RGB 0 100 0       -> set Vty.green
    RGB 0 128 0       -> set Vty.bright_green
    RGB 165 42 42     -> set Vty.yellow
    RGB 255 255 0     -> set Vty.bright_yellow
    RGB 0 0 139       -> set Vty.blue
    RGB 0 0 255       -> set Vty.bright_blue
    RGB 128 0 128     -> set Vty.magenta
    RGB 255 0 255     -> set Vty.bright_magenta
    RGB 0 139 139     -> set Vty.cyan
    RGB 0 255 255     -> set Vty.bright_cyan
    RGB 165 165 165   -> set Vty.white
    RGB 255 255 255   -> set Vty.bright_white
    Default           -> id
    _                 -> set unknown -- NB

attributesToAttr :: Attributes -> (Vty.Attr -> Vty.Attr)
attributesToAttr (Attributes fg bg reverse bd _itlc underline') =
    (if reverse then (flip Vty.with_style Vty.reverse_video)  else id) .
    (if bd then (flip Vty.with_style Vty.bold) else id) .
    (if underline' then (flip Vty.with_style Vty.underline) else id) .
    colorToAttr (flip Vty.with_fore_color) Vty.black fg . 
    colorToAttr (flip Vty.with_back_color) Vty.white bg

---------------------------------


-- | Apply the attributes in @sty@ and @changes@ to @cs@.  If the
-- attributes are not used, @sty@ and @changes@ are not evaluated.
paintChars :: a -> [(Point,a)] -> [(Point,Char)] -> [(Char, (a,Point))]
paintChars sty changes cs = [(c,(s,p)) | ((p,c),s) <- zip cs attrs]
    where attrs = lazy (stys sty changes cs)

lazy :: [a] -> [a]
lazy l = head l : lazy (tail l)

stys :: a -> [(Point,a)] -> [(Point,Char)] -> [a]
stys sty [] cs = [ sty | _ <- cs ]
stys sty ((endPos,sty'):xs) cs = [ sty | _ <- previous ] ++ stys sty' xs later
    where (previous, later) = break ((endPos <=) . fst) cs

