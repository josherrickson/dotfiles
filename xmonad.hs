import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers -- needed for isFullscreen
import XMonad.Layout.NoBorders -- needed for smartBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

-- Modify dmenu to include -i (case insensitive) option
dmenuLaunch :: MonadIO m => m ()
dmenuLaunch = spawn $ "exe=`ls /usr/bin | dmenu -i `" ++ "&& eval \"exec $exe\""

-- redefine keys (specifically dmenu)
myKeys :: [(String, X())]
myKeys = [ ("M-p" , dmenuLaunch ) ] -- dmenu app launcher

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.rc"
  xmonad $ defaultConfig{
    manageHook = myManageHooks <+> manageDocks <+> manageHook defaultConfig,
    -- smartBorders hides borders around single windows
    layoutHook = avoidStruts  $  smartBorders  $  layoutHook defaultConfig,
    logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc,
                     -- 'shorten' controls # characters in process name
                     ppTitle = xmobarColor "green" "" . shorten 75
                   },
    modMask = mod4Mask, -- Change to command key
    borderWidth = 2,    -- make borders more visible
    terminal = "urxvt"  -- set terminal launched from cmd-shift-enter
  } `additionalKeysP` myKeys -- use the keys!

myManageHooks = composeAll [
  isFullscreen --> doFullFloat -- Allows full-screen flash video
  , className =? "Empathy" --> doFloat
  , className =? "Pidgin" --> doFloat
  , className =? "Gimp" --> doFloat
  ]
