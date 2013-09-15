import Prelude ()
import Yi
import Yi.Keymap.Vim
import qualified Yi.Keymap.Vim2 as V2
import qualified Yi.Keymap.Vim2.Common as V2
import qualified Yi.Keymap.Vim2.Utils as V2

import qualified Yi.Mode.Haskell as Haskell

main :: IO ()
main = yi $ defaultVimConfig {
    modeTable = (myModes ++ modeTable defaultVimConfig),
    defaultKm = myKeymapSet,
    configCheckExternalChangesObsessively = False
}

defaultSearchKeymap :: Keymap
defaultSearchKeymap = do
    Event (KASCII c) [] <- anyEvent
    write (isearchAddE [c])

myKeymapSet :: KeymapSet
myKeymapSet = V2.mkKeymapSet $ V2.defVimConfig `override` \super this ->
    let eval = V2.pureEval this
    in super {
          -- Here we can add custom bindings.
          -- See Yi.Keymap.Vim2.Common for datatypes and 
          -- Yi.Keymap.Vim2.Utils for useful functions like mkStringBindingE

          -- In case of conflict, that is if there exist multiple bindings
          -- whose prereq function returns WholeMatch,
          -- the first such binding is used.
          -- So it's important to have custom bindings first.
          V2.vimBindings = myBindings eval ++ V2.vimBindings super
        }

myBindings :: (String -> EditorM ()) -> [V2.VimBinding]
myBindings eval =
    let nmap x y = V2.mkStringBindingE V2.Normal V2.Drop (x, y, id)
    in [
         -- Tab traversal
         nmap "<C-h>" previousTabE
       , nmap "<C-l>" nextTabE
       , nmap "<C-l>" nextTabE

         -- Press space to clear incremental search highlight
       , nmap " " (eval ":nohlsearch<CR>")

         -- for times when you don't press shift hard enough
       , nmap ";" (eval ":")

       , nmap "<F3>" (withBuffer0 deleteTrailingSpaceB)
       , nmap "<F4>" (withBuffer0 moveToSol)
       , nmap "<F1>" (withBuffer0 readCurrentWordB >>= printMsg)
       ]

myModes = [
         AnyMode Haskell.fastMode {
             -- Disable beautification
             modePrettify = const $ return ()
           , modeGetAnnotations = (const . const) []
         }
    ]