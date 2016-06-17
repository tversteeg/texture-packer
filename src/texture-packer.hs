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
  on addFileButton buttonActivated $ do 
    response <- openSelectImageDialog window
    putStrLn $ case response of
      Just fileName -> fileName
      _             -> "Closed"

  widgetShowAll window
  mainGUI

openSelectImageDialog :: Window -> IO (Maybe String)
openSelectImageDialog parentWindow = do
  dialog <- fileChooserDialogNew
    (Just $ "Select an image to add")
    (Just parentWindow)
    FileChooserActionOpen
    [("gtk-cancel", ResponseCancel),
     ("gtk-open", ResponseAccept)]

  fileChooserSetLocalOnly dialog True

  commonImageFilter <- fileFilterNew
  fileFilterSetName commonImageFilter "Common image files"
  fileFilterAddMimeType commonImageFilter "image/*"
  fileChooserAddFilter dialog commonImageFilter

  rawImageFilter <- fileFilterNew
  fileFilterSetName rawImageFilter "Raw image files (*.raw)"
  fileFilterAddPattern rawImageFilter "*.raw"
  fileChooserAddFilter dialog rawImageFilter

  customImageFilter <- fileFilterNew
  fileFilterSetName customImageFilter "Custom image files (*.dfield)"
  fileFilterAddPattern customImageFilter "*.dfield"
  fileChooserAddFilter dialog customImageFilter

  allFilesFilter <- fileFilterNew
  fileFilterSetName allFilesFilter "All files"
  fileFilterAddPattern allFilesFilter "*"
  fileChooserAddFilter dialog allFilesFilter

  image <- imageNew
  fileChooserSetPreviewWidget dialog image

  on dialog updatePreview $ do
    file <- fileChooserGetPreviewFilename dialog
    case file of
         Nothing -> putStrLn "No file selected"
         Just fpath -> imageSetFromFile image fpath

  widgetShow dialog

  response <- dialogRun dialog
  result <- case response of
    ResponseAccept -> fileChooserGetFilename dialog
    _              -> return Nothing

  widgetDestroy dialog

  return result
