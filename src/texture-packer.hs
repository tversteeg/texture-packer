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

  quitMenuItem <- builderGetObject builder castToMenuItem "quitMenuItem1"
  on quitMenuItem menuItemActivated (do
    widgetDestroy window
    mainQuit)

  addFileButton <- builderGetObject builder castToButton "addFileButton1"
  on addFileButton buttonActivated (openSelectImageDialog window)

  widgetShowAll window
  mainGUI

openSelectImageDialog parentWindow = do
  dialog <- fileChooserDialogNew
    (Just $ "Select an image to add")
    (Just parentWindow)
    FileChooserActionOpen
    [("gtk-cancel", ResponseCancel),
     ("gtk-open", ResponseAccept)]

  previewLabel <- labelNew $ Just "Preview appears here"
  previewLabel `labelSetLineWrap` True
  dialog `FileChooserSetPreviewWidget` previewLabel

  widgetShow dialog

  response <- dialogRun dialog
  case response of
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         putStrLn $ "You selected the folder " ++ show fileName

  widgetHide dialog
