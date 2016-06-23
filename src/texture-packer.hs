module Main (main) where

import Paths_texture_packer
import Graphics.UI.Gtk

data Image = Image {width :: Int,
                    height :: Int,
                    pixel :: String,
                    file :: String,
                    path :: String}

main = do
  initGUI

  builder <- builderNew
  dataFile <- getDataFileName "application.ui"
  builderAddFromFile builder dataFile

  window <- builderGetObject builder castToWindow "window"
  on window objectDestroy mainQuit

  quitMenuItem <- builderGetObject builder castToMenuItem "quitMenuItem1"
  on quitMenuItem menuItemActivated $ do
    widgetDestroy window
    mainQuit

  inputFileView <- builderGetObject builder castToTreeView "inputFiles1"
  inputFileStore <- listStoreNew []
  treeViewSetModel inputFileView inputFileStore
  setupInputList inputFileView inputFileStore

  previewImage <- builderGetObject builder castToImage "preview1"

  on inputFileView cursorChanged $ do
    tree <- treeViewGetSelection inputFileView
    selection <- treeSelectionGetSelectedRows tree
    let sel = head (head selection)
    row <- listStoreGetValue inputFileStore sel
    imageSetFromFile previewImage $ path row

  addFileButton <- builderGetObject builder castToButton "addFileButton1"
  on addFileButton buttonActivated $
    openSelectImageDialog window >>= addImageToList inputFileStore

  removeFileButton <- builderGetObject builder castToButton "removeFileButton1"
  on removeFileButton buttonActivated $ do
    treeSelection <- treeViewGetSelection inputFileView
    indices <- treeSelectionGetSelectedRows $ treeSelection
    sequence_ $ map (listStoreRemove inputFileStore) $ map head indices
    treeSelectionUnselectAll treeSelection

  widgetShowAll window
  mainGUI

setupInputList view store = do
  treeViewSetHeadersVisible view True

  renderer1 <- cellRendererTextNew
  col1 <- treeViewColumnNew
  treeViewColumnPackStart col1 renderer1 True
  cellLayoutSetAttributes col1 renderer1 store
    $ \row -> [ cellText := file row ]
  treeViewColumnSetTitle col1 "File"
  treeViewAppendColumn view col1

  renderer2 <- cellRendererTextNew
  col2 <- treeViewColumnNew
  treeViewColumnPackStart col2 renderer2 True
  cellLayoutSetAttributes col2 renderer2 store
    $ \row -> [ cellText := show (width row) ]
  treeViewColumnSetTitle col2 "Width"
  treeViewAppendColumn view col2

  renderer3 <- cellRendererTextNew
  col3 <- treeViewColumnNew
  treeViewColumnPackStart col3 renderer3 True
  cellLayoutSetAttributes col3 renderer3 store
    $ \row -> [ cellText := show (height row) ]
  treeViewColumnSetTitle col3 "Height"
  treeViewAppendColumn view col3

  renderer4 <- cellRendererTextNew
  col4 <- treeViewColumnNew
  treeViewColumnPackStart col4 renderer4 True
  cellLayoutSetAttributes col4 renderer4 store
    $ \row -> [ cellText := path row ]
  treeViewColumnSetTitle col4 "Path"
  treeViewAppendColumn view col4

  renderer5 <- cellRendererTextNew
  col5 <- treeViewColumnNew
  treeViewColumnPackStart col5 renderer5 True
  cellLayoutSetAttributes col5 renderer5 store
    $ \row -> [ cellText := pixel row ]
  treeViewColumnSetTitle col5 "Format"
  treeViewAppendColumn view col5

addImageToList list fileName = do
  case fileName of
    Just file -> do
      let getImage = return Image {
          width = 10,
          height = 10,
          pixel = "RGBA",
          file = "Image",
          path = file
        }
      getImage >>= listStorePrepend list
    _ -> return ()

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
