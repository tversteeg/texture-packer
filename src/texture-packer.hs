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

openSelectImageDialog :: Window -> IO ()
openSelectImageDialog parentWindow = do
  dialog <- fileChooserDialogNew
    (Just $ "Select an image to add")
    (Just parentWindow)
    FileChooserActionOpen
    [("gtk-cancel", ResponseCancel),
     ("gtk-open", ResponseAccept)]

  fileChooserSetLocalOnly dialog True

  commonImageFilter <- fileFilterNew
  fileFilterSetName commonImageFilter "Common image files (*.png, *.bmp, *.jpg)"
  fileFilterAddPattern commonImageFilter "*.png"
  fileFilterAddPattern commonImageFilter "*.bmp"
  fileFilterAddPattern commonImageFilter "*.jpg"
  fileFilterAddPattern commonImageFilter "*.jpeg"
  fileChooserAddFilter dialog commonImageFilter

  rawImageFilter <- fileFilterNew
  fileFilterSetName rawImageFilter "Raw image files (*.raw)"
  fileFilterAddPattern rawImageFilter "*.raw"
  fileChooserAddFilter dialog rawImageFilter

  customImageFilter <- fileFilterNew
  fileFilterSetName customImageFilter "Custom image files (*.dfield)"
  fileFilterAddPattern customImageFilter "*.dfield"
  fileChooserAddFilter dialog customImageFilter

  previewLabel <- labelNew $ Just "Preview appears here"
  previewLabel `labelSetLineWrap` True
  dialog `fileChooserSetPreviewWidget` previewLabel
  on dialog updatePreview $ do
    previewFile <- fileChooserGetPreviewFilename dialog
    previewLabel `labelSetText` case previewFile of
      Nothing -> "Preview appears here"
      (Just filename) -> "Preview file:\n" ++
        show filename

  widgetShow dialog

  response <- dialogRun dialog
  widgetHide dialog

  uri <- fileChooserGetFilename dialog

  return ()
