
import XMonad hiding ( (|||) ) -- hide ||| from XMonad core for LayoutCombinators (Jumptolayout)
import Data.Monoid
import Data.Ratio -- %
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.IO
-- actions
import XMonad.Actions.CycleWS -- nextScreen
import XMonad.Actions.WithAll -- killAll
import XMonad.Actions.RotSlaves
import XMonad.Actions.GroupNavigation -- cycling windows across workspaces.
-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers -- doCenterFloat
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
-- utils
import XMonad.Util.EZConfig
import XMonad.Util.Run  --spawnPipe, hPutStrLn
import XMonad.Util.Scratchpad --scratchpadSpawnAction, managehook, padfilteronWorkspace
import XMonad.Util.NamedScratchpad -- namedScratchpad
import XMonad.Util.Loggers -- ppExtras
import XMonad.Util.Font -- AlignCenter,Right...
-- layouts
import XMonad.Layout.NoBorders -- noborder, smartBorder
import XMonad.Layout.ResizableTile -- resizableTall
import XMonad.Layout.Roledex
{-import XMonad.Layout.Gaps-}
import XMonad.Layout.Magnifier
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
{-import XMonad.Layout.Reflect-}
{-import XMonad.Layout.TwoPane-}
{-import XMonad.Layout.IM-}
{-import XMonad.Layout.DragPane-}
import XMonad.Layout.Spacing -- spacing
import XMonad.Layout.Accordion
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed -- add description for jumptolayout.
-- prompt
import XMonad.Prompt -- defaultXPConfig, font, height, XPConfig


---- General settings

myTerminal    = "xterm"
myBorderWidth = 1
myModMask     = mod4Mask

myFocusFollowsMouse  = False
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#a41034" 
myStatusBar = "xmobar /etc/X11/xinit/xmonad/xmobarrc"
myWorkspaces  = ["LOG","WEB","DEV","DOC","MSG","ETC"] -- ++ map show [7..9]

---- Keybindings

myKeys = \c -> mkKeymap c $
 [ ("M-S-c", kill)
 , ("M-S-x", killAll)
-- layouts
 ,  ("M-<Space>",    sendMessage NextLayout)
 ,  ("M-S-<Space>",  setLayout   $ XMonad.layoutHook c)
 ,  ("M-x f",        sendMessage $ JumpToLayout "full")
 ,  ("M-x t",        sendMessage $ JumpToLayout "tiled")
 ,  ("M-x c",        sendMessage $ JumpToLayout "circle")
 ,  ("M-x m",        sendMessage $ JumpToLayout "threecolmid")
 ,  ("M-x g",        sendMessage $ JumpToLayout "Grid")
 ,  ("M-x r",        sendMessage $ JumpToLayout "Roledex")
 ,  ("M-x a",        sendMessage $ JumpToLayout "Accordion")
 ,  ("M-b",          sendMessage ToggleStruts)
 ,  ("M-n",          refresh)
 ,  ("M-S-t",        withFocused $ windows . W.sink) -- float window to tiling

-- focus
 ,  ("M-<Tab>",        windows      W.focusDown)
 ,  ("M-S-<Tab>",      rotSlavesUp)
 ,  ("M-j",            windows      W.focusDown)
 ,  ("M-k",            windows      W.focusUp)
 ,  ("M-m",            windows      W.focusMaster)
 ,  ("M-u",            nextMatch History (return True))
 ,  ("M-<Backspace>",  focusUrgent)

-- Swapping
 ,  ("M-S-m",  windows W.swapMaster)
 ,  ("M-S-j",  windows W.swapDown)
 ,  ("M-S-k",  windows W.swapUp)

-- resizing
 ,  ("M-<Up>",     sendMessage MirrorExpand)
 ,  ("M-<Down>",   sendMessage MirrorShrink)
 ,  ("M-<Left>",   sendMessage Shrink)
 ,  ("M-<Right>",  sendMessage Expand)

-- X200T
 ,  ("<XF86AudioPlay>",         spawn "mpc toggle")
 ,  ("<XF86AudioStop>",         spawn "mpc stop")
 ,  ("<XF86AudioNext>",         spawn "mpc next")
 ,  ("<XF86AudioPrev>",         spawn "mpc prev")
 ,  ("<XF86AudioMute>",         spawn "amixer set Master toggle")
 ,  ("<XF86AudioLowerVolume>",  spawn "amixer set Master 5%-")
 ,  ("<XF86AudioRaiseVolume>",  spawn "amixer set Master 5%+")
 ,  ("<XF86HomePage>",          spawn "xmodmap ~/.Xmodmap")
 ,  ("<XF86Sleep>",        spawn "systemctl suspend")
 ,  ("<XF86ScreenSaver>",  spawn "/etc/X11/xinit/xmonad/xlock.sh")
 ,  ("<XF86LaunchA>",      moveTo Next (WSIs notNSP))
 ,  ("<XF86LaunchB>",      spawn "/etc/X11/xinit/xmonad/rotate_screen.sh")

-- applications
 , ("M-l", spawn "dmenu_run -i -l 5 -p 'ryan'")
 , ("M-S-l", spawn "sudo dmenu_run -i -l 5 -p 'root'")
 , ("M-x e", spawn "emacsclient --alternate-editor=\"\" -c") -- 'M-e' used.
 , ("M-t", spawn $ XMonad.terminal c )
 , ("M-p", spawn "touch ~/.pomodoro_session")
 , ("M-S-p", spawn "rm ~/.pomodoro_session")
 , ("M-o", spawn "transset-df --dec .4 -at")
 , ("M-c", namedScratchpadAction myScratchPads "camera")
 , ("M-'", namedScratchpadAction myScratchPads "terminal")
 , ("M-d", namedScratchpadAction myScratchPads "stardict")
--  , ("M-v", namedScratchpadAction myScratchPads "mplayer" >> windows W.focusDown 
--                                                          >> spawn "transset-df -n 'MPlayer' .6")
 , ("M-v", spawn "xvkbd -compact -no-repeat -minimizable")
 , ("M-S-v", spawn "killall xvkbd")

-- quit & restart
 , ("M-q", spawn "/etc/X11/xinit/xmonad/restart_xmonad.sh")
 , ("M-S-q", io (exitWith ExitSuccess))
 ]

  ++
    ---- Switch Workspace
    -- mod-N,       Switch to workspace N; 
    -- mod-shift-N, Move client to workspace N
    -- mod-ctrl-N,  Switch to workspace N on other screen
    [ (m ++ "M-" ++ [k], f i)
        | (i, k) <- zip (XMonad.workspaces c) "1234567890-=[]\\"
        , (f, m) <- [ (windows . W.view, "")
                    , (windows . W.shift, "S-")
                    , (\ws -> nextScreen >> (windows . W.view $ ws), "C-")
                    ]
    ]

    ++
    ---- Switch Screen
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [ (m ++ "M-" ++ [k], screenWorkspace sc >>= flip whenJust f)
         | (k, sc) <- zip "we" [0..]
         , (f, m) <- [(windows . W.view, ""), (windows . W.shift, "S-")]]

  -- hide NSP
  where notNSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)

---- Mouse bindings

-- button4 and button5 are used as scroll wheel
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]



---- Window rules

myManageHook = (composeAll . concat $
  [ [resource  =? r --> doIgnore         |  r    <- myIgnore   ]
  , [className =? c --> doFloat          |  c    <- myFloat    ]
  , [className =? c --> doCenterFloat    |  c    <- myCFloat   ]
  , [className =? c --> doShift "WEB"    |  c    <- myWEBClass ]
  , [className =? c --> doShift "DOC"    |  c    <- myDOCClass ]
  , [className =? c --> doShift "ETC"    |  c    <- myETCClass ]
  ]) <+> manageDocks <+> manageScratchPads 
  where
    myIgnore      = ["desktop","desktop_window","webcamera"        ]
    myFloat       = ["Zenity","Zim","XVkbd"                        ]
    myCFloat      = ["Xmessage","Save As...","XFontSel", "Main.py" ]
    myWEBClass    = ["Chromium","Firefox"                          ]
    myDOCClass    = ["Evince","Xpdf","Zathura","Xournal"           ]
    myETCClass    = ["Wine"                                        ]



---- ScratchPads

    manageScratchPads :: ManageHook
    manageScratchPads = namedScratchpadManageHook myScratchPads
myScratchPads = [  NS   "terminal"   spawnTerm  findTerm  manageTerm
                ,  NS   "camera"     spawnCam   findCam   manageCam
                ,  NS   "stardict"   spawnSD    findSD    manageSD
                ,  NS   "mplayer"    spawnMV    findMV    manageMV
                ]
    where
 spawnTerm  = myTerminal     ++ " -name scratchpad"
 findTerm   = resource       =? "scratchpad"
 manageTerm = customFloating $  W.RationalRect l t w h
    where
        h = 0.4
        w = 0.5
        t = 0     
        l = 1 - w
 spawnSD  = "stardict"
 findSD   = className          =? "Stardict"
 manageSD = customFloating $  W.RationalRect l t w h
    where
        h = 2/3
        w = 2/3
        t = 1/6
        l = 1/6
 spawnMV  = "sh /home/ryan/local/scripts/playmv.sh"
 findMV   = className      =? "MPlayer"
 manageMV = customFloating $  W.RationalRect l t w h
    where
        h = 0.4
        w = 0.39
        t = 0.63
        l = 1-w
 spawnCam  = "sudo mplayer tv:// -tv driver=v4l2:width=640:height=480:device=/dev/video0 -fps 15 -vf screenshot -geometry 128x80+576+720 -name 'webcamera'"
 findCam   = title          =? "camera"
 manageCam = customFloating $  W.RationalRect l t w h
    where
        h = 0.3  
        w = 0.3 
        t = (1 - h)/2
        l = (1 - w)/2



---- Status bars and logging

myLogHook xmobarhook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmobarhook }
myPP = defaultPP { ppHidden          = xmobarColor "#880000" "" . noScratchPad
               , ppHiddenNoWindows = xmobarColor "#222222" "" . showNamedWorkspaces
               , ppCurrent         = xmobarColor "#ff0000" ""  -- . pad . wrap "" ""
               , ppLayout          = xmobarColor "#880000" "#000000" . shorten 20
               , ppUrgent          = xmobarColor "#00ff00" "" 
               , ppSep             = " "
               , ppWsSep           = " "
               , ppOrder           = \(ws:l:t:xs) -> [ws,l]
               }
  where
    showNamedWorkspaces wsId = if any (`elem` wsId) ['A'..'Z']
                                       then wsId
                                       else ""
    noScratchPad ws = if ws =="NSP"
                      then ""
                      else ws
---- Layout

myLayoutHook = avoidStruts $ (  renamed [Replace "full"] full 
                            ||| renamed [Replace "tiled"] tiled 
                            ||| renamed [Replace "circle"] circle 
                            ||| renamed [Replace "threecolmid"] threecolmid 
                            ||| Grid
                            ||| Roledex 
                            ||| Accordion)
   where
        full            = noBorders Full
        tiled           = spacing 4 $ smartBorders (ResizableTall 1 (2/100) (1/2) [])
        magnify'        = magnifiercz' 2.2
        circle          = magnify' Circle
        threecolmid     = ThreeColMid 1 (2/100) (1/3)

-- Other hooks

myEventHook = mempty

myStartupHook = do
  windows $ W.view "DEV"
  setWMName "LG3D"

---- Main

main = do
  workspaceBarPipe <- spawnPipe myStatusBar

  xmonad $ withUrgencyHook NoUrgencyHook
         $ defaultConfig {
           terminal           = myTerminal,
           focusFollowsMouse  = myFocusFollowsMouse,
           borderWidth        = myBorderWidth,
           modMask            = myModMask,
           workspaces         = myWorkspaces,
           normalBorderColor  = myNormalBorderColor,
           focusedBorderColor = myFocusedBorderColor,
           keys               = myKeys,
           mouseBindings      = myMouseBindings,
           layoutHook         = myLayoutHook,
           manageHook         = myManageHook,
           handleEventHook    = myEventHook,
           logHook            = myLogHook workspaceBarPipe <+> historyHook,
           startupHook        = myStartupHook
        }
