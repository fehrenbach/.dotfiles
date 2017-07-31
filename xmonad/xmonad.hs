import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Spiral
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WindowProperties (getProp32s)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio
import Control.Monad

win = mod4Mask

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

myKeys =
 [((win, xK_b), spawn "chromium"), -- browser on win+b
  ((win, xK_r), spawn "konsole"),
  ((win, xK_e), spawn "emacs"),
    -- Restart xmonad
   --((win, xK_q), spawn "xmonad --recompile; xmonad --restart"),
   -- close focused window
  ((win, xK_c), kill)
 ] ++
 -- switch workspace and move to workspace with win-[1..9] win-shift-[1..9] 
 -- DVORAK AWARE!
 -- Meaning: & [ { } ( = * ) + ] ! are 0 1 2 3 4 5 6 7 8 9 10
 [((m .|. win, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_ampersand,
                                      xK_bracketleft, 
                                      xK_braceleft, 
                                      xK_braceright, 
                                      xK_parenleft, 
                                      xK_equal, 
                                      xK_asterisk, 
                                      xK_parenright, 
                                      xK_plus, 
                                      xK_bracketright,
                                      xK_exclam] --[xK_1 .. xK_10]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] ++
-- Switch focus between screens
-- DVORAK
-- win+, win+., win+p instead of win+w, win+e, win+r
 [((m .|. win, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period, xK_p] [0..2]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myLayoutHook = (smartBorders $ desktopLayoutModifiers $ (reflectHoriz tall)) ||| (smartBorders $ desktopLayoutModifiers $ Mirror tall) ||| noBorders Full
  where tall = (Tall 1 (3/100) (1/2))

myManageHook = composeAll
               [className =? "Vlc" --> doFloat,
                className =? "Klipper" --> doFloat,
                className =? "plasmashell" --> doFloat
--                className =? "dota_linux" --> doFullFloat,
--                isFullscreen --> doF W.focusDown <+> doFullFloat
--                isFullscreen <&&> liftedNot (className =? "dota_linux") --> doFullFloat
                ]

myStartupHook = do
  setWMName "LG3D" -- fix Java GUI

-- Plasma stuff from http://code.google.com/p/xmonad/issues/detail?id=430
kdeOverride = ask >>= \w -> liftX $ do
  override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
  wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
  return $ maybe False (elem $ fromIntegral override) wt

main = xmonad $ desktopConfig
 { modMask = win,               -- use windows button as mod
   terminal = "konsole",
   normalBorderColor = "#000000",
   focusedBorderColor = "#FF0000", -- "#2B74C7",
   focusFollowsMouse = True,
   manageHook = myManageHook <+> (kdeOverride --> doFloat) <+> manageHook desktopConfig,
   layoutHook = myLayoutHook,
   logHook = logHook desktopConfig,
   startupHook = startupHook desktopConfig >> myStartupHook
 } `additionalKeys` myKeys
