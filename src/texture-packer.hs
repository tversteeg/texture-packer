module Main (main) where

import Paths_texture_packer
import Graphics.UI.Gtk

main = do
	initGUI

	builder <- builderNew
	dataFile <- getDataFileName "application.ui"
	builderAddFromFile builder dataFile

	window <- builderGetObject builder castToWindow "window"
	on window objectDestroy mainQuit

	widgetShowAll window
	mainGUI
