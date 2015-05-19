import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Minimize
import XMonad.Layout.ShowWName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.ManageDocks


-- for nextWS etc.
import XMonad.Actions.CycleWS

-- For runOrRaise
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowBringer
import XMonad.Actions.FloatKeys
import System.IO

main = do
--	xmonad =<< xmobar gnomeConfig {
	xmproc <- spawnPipe "xmobar"
	xmonad $ gnomeConfig {
     -- add manage hooks while still ignoring panels and using default manageHooks
--	manageHook = myManageHook <+> manageHook gnomeConfig
	manageHook = myManageHook <+> manageDocks

     -- add a fullscreen tabbed layout that does not avoid covering
     -- up desktop panels before the desktop layouts
--	, layoutHook = desktopLayoutModifiers $ myLayout
	, layoutHook =  myLayout
	, logHook = do
			myLogHook xmproc
			logHook gnomeConfig
--	, handleEventHook = fadeWindowsEventHook
	, handleEventHook = myEventHook
	, modMask = mod4Mask     -- Rebind Mod to the Windows key
	, workspaces = myWorkspaces
	, focusedBorderColor = myFocusedBorderColor
	, normalBorderColor = myNormalBorderColor
	, startupHook = setWMName "LG3D"
	} `additionalKeysP` myKeys

myWorkspaces = map show [1..9]

myEventHook = composeAll 
               [
                 handleEventHook gnomeConfig
                 , fullscreenEventHook
                 , fadeWindowsEventHook] 

myManageHook = composeAll
		[ className =? "Gimp" --> doFloat
		, className =? "Firefox" --> doShift "1"
		, className =? "Pidgin" --> doShift "6"
		, className =? "Skype" --> doShift "6"
                -- Allows focusing other monitors without killing the fullscreen
                --  isFullscreen --> (doF W.focusDown <+> doFullFloat)
		, isFullscreen --> doFullFloat
		, isDialog --> doCenterFloat
		]

myLogHook o = dynamicLogWithPP sjanssenPP
				{ ppOutput = hPutStrLn o
				, ppTitle = xmobarColor "#79B675" "" . shorten 60
				} 

myKeys = [
		("M-<R>", nextWS)
		, ("M-<L>",  prevWS)
		, ("M-S-<R>", shiftToNext >> nextWS)
		, ("M-S-<L>",  shiftToPrev >> prevWS)
		, ("M-f", moveTo Next EmptyWS)
		, ("C-M-<L>", withFocused (keysResizeWindow (-10,0) (0,0)))
		, ("C-M-<U>", withFocused (keysResizeWindow (0,-10) (0,0)))
		, ("C-M-<R>", withFocused (keysResizeWindow (10,0) (0,0)))
		, ("C-M-<D>", withFocused (keysResizeWindow (0,10) (0,0)))
		, ("M-w", raiseMaybe (moveTo Next EmptyWS >> spawn "iceweasel") (className =? "Iceweasel"))
		, ("M-S-t", runOrRaise "gnome-terminal" (className =? "Gnome-terminal"))
--		, ("M-e", spawn "/home/green/bin/external-display.sh")
		, ("M-s", spawn "gnome-screensaver-command --lock")
		, ("M-a", sendMessage MirrorShrink)
		, ("M-z", sendMessage MirrorExpand)
		, ("M-m", sendMessage $ Toggle MIRROR)
		, ("M-x", sendMessage $ Toggle FULL)
		, ("M-c", kill)
		, ("M-S-g", gotoMenu)
		, ("M-S-b", bringMenu)
		, ("M-r", spawn "synapse")
		-- My dmenu style
	--	, ("M-d", spawn "exe=`dmenu_path | dmenu -sb \"#64742f\" -nf \"#eee\" -nb \"#151412\" -fn \"-xos4-terminus-mono-bold-r-normal-13-*-*-*-*-*-*-*\"` && eval \"exec $exe\"")
		-- For minimizing windows
		, ("M-i", withFocused minimizeWindow)
		, ("M-S-i", sendMessage RestoreNextMinimizedWin)
--		for mosaic alt
--		, ("M-a", withFocused (sendMessage . expandWindowAlt))
--		, ("M-z", withFocused (sendMessage . shrinkWindowAlt))
--		, ("M-s", withFocused (sendMessage . tallWindowAlt))
--		, ("M-d", withFocused (sendMessage . wideWindowAlt))
--		, ("M-C-<Space>", sendMessage resetAlt)
        ]

myLayout = 
        -- remove borders from windows that are fullscreen (does not yet work
        -- for Firefox) or the only window on the workspace
        (smartBorders .
        fullscreenFull .
        avoidStruts .
        mkToggle (single MIRROR) .
        mkToggle (single FULL) $
        -- let fullscreen windows cover the Gnome panels (needed for Firefox)
        --layoutHook gnomeConfig 
        (tiled ||| Mirror tiled ||| Grid ))
	  where
	       tiled = ResizableTall nmaster delta ratio []
	       nmaster = 1
	       ratio = 1/2
	       delta = 3/100

{-
myLayout = showWName (smartBorders . mkToggle (single MIRROR) . mkToggle (single FULL)
				 $ (tiled ||| Grid ))
				  where
			 	       tiled = ResizableTall nmaster delta ratio []
			 	       nmaster = 1
			 	       ratio = 1/2
			 	       delta = 3/100
--                                 $ smartBorders (layoutHook defaultConfig))
--                                 $ simpleTabBar $ smartBorders (layoutHook defaultConfig))
                          --       $ smartBorders (simpleDeco shrinkText defaultTheme (layoutHook defaultConfig)))
                              --   $ smartBorders (hinted Tall ||| tiled || Mirror tiled ||| Full))
				--  where hinted = HintedTile nmaster delta ratio TopLeft
			-- 	       tiled = Tall nmaster delta ratio
			-- 	       nmaster =1
			-- 	       ratio = 1/2
			-- 	       delta = 3/100
-}
-- myFocusedBorderColor = "#7F9F7F"
myFocusedBorderColor = "#77aa00"
borderWidth = 0
myNormalBorderColor = "#444444"
