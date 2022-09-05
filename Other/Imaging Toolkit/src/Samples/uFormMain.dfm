object FormMain: TFormMain
  Left = 434
  Top = 268
  Width = 611
  Height = 533
  Caption = 'Imaging Toolkit for Delphi'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000099990000000000000000000000000000999
    9000000000000000000000000000099990000000000000000000000000000999
    9000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000099990099990000000000000000000000999
    9009999000000000000000000000099990099990000000000000000000000999
    9009999000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000099990099990099990000000000000000999
    9009999009999000000000000000099990099990099990000000000000000999
    9009999009999000000000000000000000000000000000000000000000000000
    0000000000000000000000000000099990099990099990099990000000000999
    9009999009999009999000000000099990099990099990099990000000000999
    9009999009999009999000000000000000000000000000000000000000000000
    0000000000000000000000000000099990099990099990099990099990000999
    9009999009999009999009999000099990099990099990099990099990000999
    900999900999900999900999900000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFF87FFFFFF87FFFFFF87FFFFFF87FFFFFFFFFFFFFFFFFF
    FFFF861FFFFF861FFFFF861FFFFF861FFFFFFFFFFFFFFFFFFFFF86187FFF8618
    7FFF86187FFF86187FFFFFFFFFFFFFFFFFFF861861FF861861FF861861FF8618
    61FFFFFFFFFFFFFFFFFF86186187861861878618618786186187FFFFFFFF}
  Menu = MainMenu
  OldCreateOrder = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDragDrop = FormDragDrop
  OnDragOver = FormDragOver
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 595
    Height = 29
    ButtonHeight = 26
    ButtonWidth = 30
    EdgeBorders = [ebTop, ebBottom]
    Images = MenuImageList
    TabOrder = 0
    object tbNew: TToolButton
      Left = 0
      Top = 2
      Hint = 'New image'
      Caption = 'tbNew'
      ImageIndex = 1
      ParentShowHint = False
      ShowHint = True
      OnClick = NewItemClick
    end
    object tbOpen: TToolButton
      Left = 30
      Top = 2
      Hint = 'Open image'
      Caption = 'tbOpen'
      ImageIndex = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = OpenItemClick
    end
    object tbSave: TToolButton
      Left = 60
      Top = 2
      Hint = 'Save image'
      Caption = 'tbSave'
      ImageIndex = 3
      ParentShowHint = False
      ShowHint = True
      OnClick = SaveItemClick
    end
    object tbPrint: TToolButton
      Left = 90
      Top = 2
      Hint = 'Print image'
      Caption = 'tbPrint'
      ImageIndex = 6
      ParentShowHint = False
      ShowHint = True
      OnClick = PrintItemClick
    end
    object tbPrintPreview: TToolButton
      Left = 120
      Top = 2
      Hint = 'Show print preview'
      Caption = 'tbPrintPreview'
      ImageIndex = 9
      ParentShowHint = False
      ShowHint = True
      OnClick = PrintPreviewItemClick
    end
    object ToolButton7: TToolButton
      Left = 150
      Top = 2
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 6
      Style = tbsSeparator
    end
    object tbSelect: TToolButton
      Left = 158
      Top = 2
      Hint = 'Select TWAIN source'
      Caption = 'tbSelect'
      ImageIndex = 4
      ParentShowHint = False
      ShowHint = True
      OnClick = SourceItemClick
    end
    object tbScan: TToolButton
      Left = 188
      Top = 2
      Hint = 'Acquire image'
      Caption = 'tbScan'
      ImageIndex = 5
      ParentShowHint = False
      ShowHint = True
      OnClick = AcquireItemClick
    end
    object ToolButton10: TToolButton
      Left = 218
      Top = 2
      Width = 8
      Caption = 'ToolButton10'
      ImageIndex = 8
      Style = tbsSeparator
    end
    object lZoom: TLabel
      Left = 226
      Top = 2
      Width = 42
      Height = 26
      AutoSize = False
      Caption = '  Zoom:  '
      Layout = tlCenter
    end
    object rsZoom: TmcmRealSpin
      Left = 268
      Top = 2
      Width = 64
      Height = 26
      Enabled = False
      TabOrder = 0
      OnChange = rsZoomChange
      Value = 1.000000000000000000
      MaxValue = 2048.000000000000000000
      Decimals = 4
      Increment = 1.000000000000000000
    end
    object tbFitInWindow: TToolButton
      Left = 332
      Top = 2
      Hint = 'Zoom to fit in window'
      Caption = 'tbFitInWindow'
      Enabled = False
      ImageIndex = 7
      ParentShowHint = False
      ShowHint = True
      OnClick = tbFitInWindowClick
    end
    object ToolButton11: TToolButton
      Left = 362
      Top = 2
      Width = 8
      Caption = 'ToolButton11'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object tbText: TToolButton
      Left = 370
      Top = 2
      Hint = 'Add a text'
      Caption = 'tbText'
      ImageIndex = 20
      ParentShowHint = False
      ShowHint = True
      OnClick = tbTextClick
    end
  end
  object mcmTWAIN: TmcmTWAIN
    Left = 40
    Top = 40
    Width = 28
    Height = 28
    Country = USA
    Filename = '.\image.bmp'
    LogFilename = '.\APPTWN.LOG'
    Manufacturer = 'MCM DESIGN'
    ProductFamily = 'Image Capture'
    ProductName = 'TWAIN Application'
    ReturnHandle = False
    XferMech = TWFX_FILES
    OnImageReady = mcmTWAINImageReady
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 455
    Width = 595
    Height = 19
    Panels = <
      item
        Text = 'Pixel'
        Width = 150
      end
      item
        Text = 'Color'
        Width = 200
      end
      item
        Text = 'Time'
        Width = 150
      end>
    SimpleText = 'Colors: '
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 40
    object FileMenu: TMenuItem
      Caption = '&File'
      OnClick = FileMenuClick
      object NewItem: TMenuItem
        Caption = '&New...'
        ShortCut = 16462
        OnClick = NewItemClick
      end
      object OpenItem: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = OpenItemClick
      end
      object SaveItem: TMenuItem
        Caption = '&Save...'
        ShortCut = 16467
        OnClick = SaveItemClick
      end
      object AppendItem: TMenuItem
        Caption = 'Append...'
        OnClick = AppendItemClick
      end
      object BrowseItem: TMenuItem
        Caption = '&Browse'
        ShortCut = 16450
        OnClick = BrowseItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object AcquireItem: TMenuItem
        Caption = '&Acquire'
        ShortCut = 16449
        OnClick = AcquireItemClick
      end
      object SourceItem: TMenuItem
        Caption = 'So&urce...'
        OnClick = SourceItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object PrintItem: TMenuItem
        Caption = '&Print...'
        ShortCut = 16464
        OnClick = PrintItemClick
      end
      object PrintPreviewItem: TMenuItem
        Caption = 'Print preview...'
        ShortCut = 16471
        OnClick = PrintPreviewItemClick
      end
      object PrinterSetupItem: TMenuItem
        Caption = 'Printer set-up...'
        OnClick = PrinterSetupItemClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object PreferencesItem: TMenuItem
        Caption = 'Preferences'
        object AssociatefileItem: TMenuItem
          Caption = 'Associate file format'
          OnClick = AssociatefileItemClick
        end
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = 'E&xit'
        OnClick = ExitItemClick
      end
    end
    object EditMenu: TMenuItem
      Caption = '&Edit'
      OnClick = EditMenuClick
      object CopyItem: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = CopyItemClick
      end
      object CopySectionItem: TMenuItem
        Caption = 'Copy section'
        OnClick = CopySectionItemClick
      end
      object PasteItem: TMenuItem
        Caption = '&Paste'
        object PasteNewImageItem: TMenuItem
          Caption = 'As &new image'
          ShortCut = 16470
          OnClick = PasteItemClick
        end
        object PasteAsSectionItem: TMenuItem
          Caption = 'As &selection'
          ShortCut = 24662
          OnClick = PasteSectionItemClick
        end
      end
      object DuplicateItem: TMenuItem
        Caption = '&Duplicate'
        OnClick = DuplicateItemClick
      end
      object EmptyClipboardItem: TMenuItem
        Caption = '&Empty Clipboard'
        OnClick = EmptyClipboardItemClick
      end
    end
    object ImageMenu: TMenuItem
      Caption = '&Image'
      Enabled = False
      OnClick = ImageMenuClick
      object InformationItem: TMenuItem
        Caption = '&Information...'
        OnClick = InformationItemClick
      end
      object CanvasSizeItem: TMenuItem
        Caption = 'Canvas Size...'
        OnClick = CanvasSizeItemClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object TransparentViewItem: TMenuItem
        Caption = 'Transparent View'
        OnClick = TransparentViewItemClick
      end
    end
    object ColorMenu: TMenuItem
      Caption = '&Color'
      Enabled = False
      OnClick = ColorMenuClick
      object UsedColorsItem: TMenuItem
        Caption = 'Used Colors...'
        OnClick = UsedColorsItemClick
      end
      object PaletteMenu: TMenuItem
        Caption = 'Palette'
        object EditPaletteItem: TMenuItem
          Caption = 'Edit Palette'
          OnClick = EditPaletteItemClick
        end
        object InvertPaletteItem: TMenuItem
          Caption = 'Invert Palette'
          OnClick = InvertPaletteItemClick
        end
      end
      object InvertItem: TMenuItem
        Caption = 'Invert'
        OnClick = InvertItemClick
      end
      object ThresholdItem: TMenuItem
        Caption = 'Threshold...'
        OnClick = ThresholdItemClick
      end
      object TuneMenu: TMenuItem
        Caption = 'Tune'
        object BrightnessContrastItem: TMenuItem
          Caption = 'Brightness/Contrast...'
          OnClick = BrightnessContrastItemClick
        end
        object GammaCorrectionItem: TMenuItem
          Caption = 'Gamma correction...'
          OnClick = GammaCorrectionItemClick
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object ConvertToMenu: TMenuItem
        Caption = 'Convert to'
        object GreyScaleItem: TMenuItem
          Caption = 'Greyscale (Average Intensity)'
          OnClick = GreyScaleItemClick
        end
        object GreyToBayerItem: TMenuItem
          Caption = 'Bayer (grey) to color'
          object OddOddItem: TMenuItem
            Tag = 1
            Caption = 'Odd, Odd'
            OnClick = GreyToBayerItemClick
          end
          object OddEvenItem: TMenuItem
            Tag = 2
            Caption = 'Odd, Even'
            OnClick = GreyToBayerItemClick
          end
          object EvenOddItem: TMenuItem
            Tag = 3
            Caption = 'Even, Odd'
            OnClick = GreyToBayerItemClick
          end
          object EvenEvenItem: TMenuItem
            Tag = 4
            Caption = 'Even, Even'
            OnClick = GreyToBayerItemClick
          end
        end
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object DecColorDepthMenu: TMenuItem
        Caption = 'Decrease Color Depth'
        OnClick = DecColorDepthMenuClick
        object InclWindowsPalItem: TMenuItem
          Caption = 'Include Windows Palette'
          OnClick = InclWindowsPalItemClick
        end
        object Dec2Colors: TMenuItem
          Caption = '2 Colors (1 bit)'
          OnClick = Dec2ColorsClick
        end
        object Dec16Colors: TMenuItem
          Caption = '16 Colors (4 bit)'
          OnClick = Dec16ColorsClick
        end
        object Dec256Colors: TMenuItem
          Caption = '256 Colors (8 bit)'
          OnClick = Dec256ColorsClick
        end
        object Dec32KColors: TMenuItem
          Caption = '32K Colors (15 bit)'
          OnClick = Dec32KColorsClick
        end
        object Dec24MColors: TMenuItem
          Caption = '24Mill Colors (24 bit)'
          OnClick = Dec24MColorsClick
        end
      end
      object IncColorDepthMenu: TMenuItem
        Caption = 'Increase Color Depth'
        OnClick = IncColorDepthMenuClick
        object Inc16Colors: TMenuItem
          Caption = '16 Colors (4 bit)'
          OnClick = Inc16ColorsClick
        end
        object Inc256Colors: TMenuItem
          Caption = '256 Colors (8 bit)'
          OnClick = Inc256ColorsClick
        end
        object Inc32KColors: TMenuItem
          Caption = '32K Colors (15 bit)'
          OnClick = Inc32KColorsClick
        end
        object Inc16MillColors: TMenuItem
          Caption = '16 Mill Colors (24 bit)'
          OnClick = Inc16MillColorsClick
        end
        object Inc16MillColors32: TMenuItem
          Caption = '16 Mill Colors (32 bit)'
          OnClick = Inc16MillColors32Click
        end
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object SplitColorMenu: TMenuItem
        Caption = 'Split color channel'
        object CIEMenu: TMenuItem
          Caption = 'CIE'
          object CIEXD65Item: TMenuItem
            Caption = 'X (D65)'
            OnClick = CIEXD65ItemClick
          end
          object CIEYD65Item: TMenuItem
            Caption = 'Luminance Y (D65)'
            OnClick = CIEYD65ItemClick
          end
          object CIEZD65Item: TMenuItem
            Caption = 'Z (D65)'
            OnClick = CIEZD65ItemClick
          end
        end
        object CMYKMenu: TMenuItem
          Caption = 'CMYK'
          object CyanItem: TMenuItem
            Caption = 'Cyan'
            OnClick = CyanItemClick
          end
          object MagentaItem: TMenuItem
            Caption = 'Magenta'
            OnClick = MagentaItemClick
          end
          object YellowItem: TMenuItem
            Caption = 'Yellow'
            OnClick = YellowItemClick
          end
          object BlackItem: TMenuItem
            Caption = 'Black'
            OnClick = BlackItemClick
          end
        end
        object HSVMenu: TMenuItem
          Caption = 'HSV'
          object HueItem: TMenuItem
            Caption = 'Hue'
            OnClick = HueItemClick
          end
          object SaturationItem: TMenuItem
            Caption = 'Saturation'
            OnClick = SaturationItemClick
          end
          object ValueItem: TMenuItem
            Caption = 'Value'
            OnClick = ValueItemClick
          end
        end
        object RGBMenu: TMenuItem
          Caption = 'RGB'
          OnClick = RGBMenuClick
          object RedItem: TMenuItem
            Caption = 'Red'
            OnClick = RedChannelItemClick
          end
          object GreenItem: TMenuItem
            Caption = 'Green'
            OnClick = GreenChannelItemClick
          end
          object BlueItem: TMenuItem
            Caption = 'Blue'
            OnClick = BlueChannelItemClick
          end
          object AlphaItem: TMenuItem
            Caption = 'Alpha'
            OnClick = AlphaItemClick
          end
        end
        object YCbCrMenu: TMenuItem
          Caption = 'YCbCr'
          object YChannelItem: TMenuItem
            Caption = 'Luminance Y'
            OnClick = YChannelItemClick
          end
          object CbChannelItem: TMenuItem
            Caption = 'Chrominance Cb'
            OnClick = CbChannelItemClick
          end
          object CrChannelItem: TMenuItem
            Caption = 'Chrominance Cr'
            OnClick = CrChannelItemClick
          end
        end
        object YIQMenu: TMenuItem
          Caption = 'YIQ, NTSC'
          object NTSCYItem: TMenuItem
            Caption = 'Luminance Y'
            OnClick = YChannelItemClick
          end
          object IChannelItem: TMenuItem
            Caption = 'In-Phase I'
            OnClick = IChannelItemClick
          end
          object QChannelItem: TMenuItem
            Caption = 'Quadtrature Q'
            OnClick = QChannelItemClick
          end
        end
        object YUVMenu: TMenuItem
          Caption = 'YUV, PAL'
          object PALYItem: TMenuItem
            Caption = 'Luminance Y'
            OnClick = YChannelItemClick
          end
          object UChannelItem: TMenuItem
            Caption = 'U'
            OnClick = UChannelItemClick
          end
          object VChannelItem: TMenuItem
            Caption = 'V'
            OnClick = VChannelItemClick
          end
        end
      end
      object CombineColorMenu: TMenuItem
        Caption = 'Combine color channel'
        object CombineCIEItem: TMenuItem
          Caption = 'CIE...'
          OnClick = CombineCIEItemClick
        end
        object CombineCMYKItem: TMenuItem
          Caption = 'CMYK...'
          OnClick = CombineCMYKItemClick
        end
        object CombineHSVItem: TMenuItem
          Caption = 'HSV...'
          OnClick = CombineHSVItemClick
        end
        object CombineRGBItem: TMenuItem
          Caption = 'RGB...'
          OnClick = CombineRGBItemClick
        end
        object CombineYCbCrItem: TMenuItem
          Caption = 'YCbCr...'
          OnClick = CombineYCbCrItemClick
        end
        object CombineYIQCItem: TMenuItem
          Caption = 'YIQ, NTSC...'
          OnClick = CombineYIQCItemClick
        end
        object CombineYUVItem: TMenuItem
          Caption = 'YUV, PAL...'
          OnClick = CombineYUVItemClick
        end
      end
    end
    object HistogramMenu: TMenuItem
      Caption = '&Histogram'
      Enabled = False
      OnClick = HistogramMenuClick
      object HistogramItem: TMenuItem
        Caption = '&Histogram Adjustment...'
        OnClick = HistogramItemClick
      end
      object EqualizeItem: TMenuItem
        Caption = '&Equalize'
        OnClick = EqualizeItemClick
      end
      object EqualizeLumItem: TMenuItem
        Caption = 'Equalize &Luminance'
        OnClick = EqualizeLumItemClick
      end
      object StretchColorItem: TMenuItem
        Caption = '&Stretch'
        OnClick = StretchColorItemClick
      end
    end
    object FilterMenu: TMenuItem
      Caption = 'Fi&lter'
      Enabled = False
      OnClick = FilterMenuClick
      object FilterBrowser: TMenuItem
        Caption = 'Filter browser...'
        OnClick = FilterBrowserClick
      end
      object SmoothingMenu: TMenuItem
        Caption = 'S&moothing'
        object AverageItem: TMenuItem
          Caption = 'Average'
          OnClick = FilterItemClick
        end
        object AverageHeavyItem: TMenuItem
          Caption = 'Average heavy'
          OnClick = FilterItemClick
        end
        object BlurItem: TMenuItem
          Caption = 'Blur'
          OnClick = FilterItemClick
        end
        object BlurHeavyItem: TMenuItem
          Caption = 'Blur heavy'
          OnClick = FilterItemClick
        end
        object GaussBlurItem: TMenuItem
          Caption = 'Gauss blur'
          OnClick = FilterItemClick
        end
        object SmoothItem: TMenuItem
          Caption = 'Smooth'
          OnClick = FilterItemClick
        end
        object SmoothCircluarItem: TMenuItem
          Caption = 'Smooth circluar'
          OnClick = FilterItemClick
        end
        object SmoothConeItem: TMenuItem
          Caption = 'Smooth cone'
          OnClick = FilterItemClick
        end
        object SmoothPyramidalItem: TMenuItem
          Caption = 'Smooth pyramidal'
          OnClick = FilterItemClick
        end
      end
      object SharpeningMenu: TMenuItem
        Caption = 'S&harpening'
        object EdgeItem: TMenuItem
          Caption = 'Edge'
          OnClick = FilterItemClick
        end
        object EdgePrewittItem: TMenuItem
          Caption = 'Edge Prewitt'
          OnClick = FilterItemClick
        end
        object HighpassItem: TMenuItem
          Caption = 'Highpass'
          OnClick = FilterItemClick
        end
        object LaplacianItem: TMenuItem
          Caption = 'Laplacian'
          OnClick = FilterItemClick
        end
        object PrewittNWItem: TMenuItem
          Caption = 'Prewitt north-west'
          OnClick = FilterItemClick
        end
        object PrewittSEItem: TMenuItem
          Caption = 'Prewitt south-east'
          OnClick = FilterItemClick
        end
        object SharpenItem: TMenuItem
          Caption = 'Sharpen'
          OnClick = FilterItemClick
        end
        object SharpenheavyItem: TMenuItem
          Caption = 'Sharpen heavy'
          OnClick = FilterItemClick
        end
        object SobelNWItem: TMenuItem
          Caption = 'Sobel north-west'
          OnClick = FilterItemClick
        end
        object SobelSEItem: TMenuItem
          Caption = 'Sobel south-east'
          OnClick = FilterItemClick
        end
        object UnsharpMaskItem: TMenuItem
          Caption = 'Unsharp mask'
          OnClick = FilterItemClick
        end
      end
      object SpecialMenu: TMenuItem
        Caption = '&Special'
        object DegrainItem: TMenuItem
          Caption = 'Degrain'
          OnClick = FilterItemClick
        end
        object EmbossItem: TMenuItem
          Caption = 'Emboss'
          OnClick = FilterItemClick
        end
        object MaximumItem: TMenuItem
          Caption = 'Maximum'
          OnClick = FilterItemClick
        end
        object MaxMinItem: TMenuItem
          Caption = 'Max Min'
          OnClick = FilterItemClick
        end
        object MedianItem: TMenuItem
          Caption = 'Median'
          OnClick = FilterItemClick
        end
        object MinimumItem: TMenuItem
          Caption = 'Minimum'
          OnClick = FilterItemClick
        end
        object MosaicItem: TMenuItem
          Caption = 'Mosaic'
          OnClick = FilterItemClick
        end
        object LineMaskHorzItem: TMenuItem
          Caption = 'Line mask horizontal'
          OnClick = FilterItemClick
        end
        object LineMaskVertItem: TMenuItem
          Caption = 'Line mask vertical'
          OnClick = FilterItemClick
        end
      end
      object EdgeMenu: TMenuItem
        Caption = 'Edge'
        object MarrHildreth: TMenuItem
          Caption = 'Marr-Hildreth'
          OnClick = MarrHildrethClick
        end
        object Canny: TMenuItem
          Caption = 'Canny'
          OnClick = CannyClick
        end
        object ShenCastan: TMenuItem
          Caption = 'Shen-Castan'
          OnClick = ShenCastanClick
        end
      end
      object DocumentMenu: TMenuItem
        Caption = 'Document'
        object DespecklelightItem: TMenuItem
          Caption = 'Despeckle light'
          OnClick = DespecklelightItemClick
        end
        object DespeckleItem: TMenuItem
          Caption = 'Despeckle'
          OnClick = DespeckleItemClick
        end
        object DespeckleAltItem: TMenuItem
          Caption = 'Despeckle Alt'
          OnClick = DespeckleAltItemClick
        end
      end
      object UserDefFilterItem: TMenuItem
        Caption = '&User defined...'
        OnClick = UserDefFilterItemClick
      end
    end
    object TransformMenu: TMenuItem
      Caption = '&Transform'
      Enabled = False
      OnClick = TransformMenuClick
      object AffineItem: TMenuItem
        Caption = '&Affine'
        OnClick = AffineItemClick
      end
      object StretchItem: TMenuItem
        Caption = '&Stretch...'
        OnClick = StretchItemClick
      end
      object FlipItem: TMenuItem
        Caption = '&Flip'
        OnClick = FlipItemClick
      end
      object MirrorItem: TMenuItem
        Caption = '&Mirror'
        OnClick = MirrorItemClick
      end
      object RotateItem: TMenuItem
        Caption = '&Rotate...'
        OnClick = RotateItemClick
      end
      object RotateFixItem: TMenuItem
        Tag = 90
        Caption = 'Rotate 90'
        OnClick = RotateFixItemClick
      end
      object Rotate180Item: TMenuItem
        Tag = 180
        Caption = 'Rotate 180'
        OnClick = RotateFixItemClick
      end
      object Rotate270Item: TMenuItem
        Tag = 270
        Caption = 'Rotate 270'
        OnClick = RotateFixItemClick
      end
      object DeskewItem: TMenuItem
        Caption = 'Deskew'
        OnClick = DeskewItemClick
      end
      object DeskewAndAutoCrop: TMenuItem
        Caption = 'Deskew && Auto-Crop'
        OnClick = DeskewAndAutoCropClick
      end
    end
    object MathMenu: TMenuItem
      Caption = '&Math'
      Enabled = False
      OnClick = MathMenuClick
    end
    object MorphingMenu: TMenuItem
      Caption = 'Morphing'
      Enabled = False
      object MorphWizardItem: TMenuItem
        Caption = 'Wizard...'
        OnClick = MorphWizardItemClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object CloseMorphItem: TMenuItem
        Caption = 'Close'
        OnClick = CloseMorphItemClick
      end
      object DilateMorphItem: TMenuItem
        Caption = 'Dilate'
        OnClick = DilateMorphItemClick
      end
      object ErodeMorphItem: TMenuItem
        Caption = 'Erode'
        OnClick = ErodeMorphItemClick
      end
      object OpenMorphItem: TMenuItem
        Caption = 'Open'
        OnClick = OpenMorphItemClick
      end
      object OutlineMorhpItem: TMenuItem
        Caption = 'Outline'
        OnClick = OutlineMorhpItemClick
      end
      object EDMItem: TMenuItem
        Caption = 'Euclidean Distance Map'
        OnClick = EDMItemClick
      end
    end
    object WindowsMenu: TMenuItem
      Caption = '&Window'
      OnClick = WindowsMenuClick
      object WindowCascadeItem: TMenuItem
        Caption = '&Cascade'
        OnClick = WindowCascadeItemClick
      end
      object WindowTileItem: TMenuItem
        Caption = '&Tile'
        OnClick = WindowTileItemClick
      end
      object CloseAllItem: TMenuItem
        Caption = 'Close &All'
        OnClick = CloseAllItemClick
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Help'
      object AboutItem: TMenuItem
        Caption = '&About'
        OnClick = AboutItemClick
      end
      object HelpItem: TMenuItem
        Caption = 'Help'
        OnClick = HelpItemClick
      end
    end
  end
  object mcmOpenDialog: TmcmOpenDialog
    DefaultExt = 'tif'
    Filter = 'Text document (*.txt)|*.txt|All files (*.*)|*.*|'
    HelpContext = 649
    Options = [ofShowHelp, ofAllowMultiSelect, ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofNoReadOnlyReturn]
    Title = 'Load Image'
    ViewStyle = vsPreview
    Left = 72
    Top = 40
  end
  object mcmSaveDialog: TmcmSaveDialog
    HelpContext = 674
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofPathMustExist, ofNoReadOnlyReturn]
    Title = 'Save Image'
    OptionHelpContext = 686
    Left = 104
    Top = 40
  end
  object PrintDialog: TPrintDialog
    Options = [poPageNums, poSelection, poWarning]
    Left = 200
    Top = 40
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 232
    Top = 40
  end
  object MenuImageList: TImageList
    Height = 20
    Width = 20
    Left = 168
    Top = 40
    Bitmap = {
      494C010118001900040014001400FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000500000008C000000010020000000000000AF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CECE
      CE008B878500CECECE00CECECE00CECECE00CECECE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EBE6E60066494200AC34
      1D00AC341D005B130800737373007373730073737300CECECE00000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000008C
      BD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008C
      BD00008CBD00008CBD00008CBD00000000000000000000000000000000000000
      00000000000000000000FBFBFB00F3F3F300E5E5E500D7D7D700D0D0D000D4D4
      D400DEDEDE00E8E8E800F1F1F100F6F6F600FAFAFA00FCFCFC00FEFEFE00FEFE
      FE00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DEDEDE0066494200F05B2700FE75
      3F00E51E1300B40000005B130800737373007373730073737300CECECE000000
      00000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008CBD0021A5
      CE005AC6EF0084E7FF0063CEF70063CEF70063CEF70063CEF70063CEF70063CE
      F70063CEF70063CEF70039ADDE001094C6000000000000000000000000000000
      0000000000003939390039393900F0F0F000DFDFDF00CBCBCB00BEBEBE00BDBD
      BD00C6C6C600D3D3D300DFDFDF00E9E9E900F1F1F100F7F7F700FBFBFB00FDFD
      FD00FEFEFE000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CECECE00AC341D00F05B2700FE75
      3F00B4000000E51E1300B40000005B130800737373007373730073737300CECE
      CE00000000000000000080808000808080008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000000000000000000000000000008CBD004ABD
      E70031ADD60094EFFF006BD6FF006BD6FF006BD6FF006BD6FF006BD6FF006BD6
      FF006BD6FF006BD6FF0042B5DE00CEF7FF00008CBD0000000000000000000000
      0000000000003939390086ABFF003C3C3C003F3F3F00C7C7C700B3B3B300ABAB
      AB00AFAFAF00B9B9B900C7C7C700D4D4D400E0E0E000EAEAEA00F2F2F200F8F8
      F800FCFCFC00FEFEFE0000000000000000000000000000000000D0AB94007373
      73008B878500D0AB9400D0AB9400D0AB940073737300AC341D00FE753F00F05B
      2700E51E1300E51E1300E51E1300B40000006649420073737300CECECE00EBE6
      E6000000000000000000FFFFFF00FFFFFF0000FF0000FEFFFE00000000000000
      0000000000000D0DFF00FEFEFF00000000000000000000000000000000000000
      000000FF00000000000000000000000000000000000000000000008CBD0073D6
      FF00008CBD00ADFFFF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDE
      FF007BDEFF007BDEFF0042B5DE00CEF7FF00008CBD0000000000000000000000
      0000000000003C3C3C0086ABFF0086ABFF008EB1FF003C3C3C003C3C3C00A3A3
      A3009D9D9D00A2A2A200ACACAC00B9B9B900C7C7C700D6D6D600E4E4E400EFEF
      EF00F7F7F700FCFCFC00FEFEFE00000000008B878500664942005B1308005B13
      08005B1308006649420073737300737373005B130800F05B2700FE753F00E51E
      1300E51E1300B4000000E51E13005B1308008B87850000000000000000000000
      0000000000000000000000000000FFFFFF0012FF120000000000000000000000
      0000000000000000FF00FEFEFF0000000000FEFEFE000000000000000000FEFE
      FE0012FF12000000000000000000000000000000000000000000008CBD007BDE
      FF001094C60094EFFF0094EFFF0084E7FF0084E7FF0084E7FF0084E7FF0084E7
      FF0084E7FF0084E7FF0042B5DE00CEF7FF001094C60000000000000000000000
      0000000000003C3C3C0089AFFF0096B5FF00B2CAFF009FBBFF00A2BCFF006D6D
      6D005F5F5F009292920097979700A0A0A000ADADAD00BDBDBD00CECECE00E0E0
      E000EEEEEE00F7F7F700FCFCFC00FEFEFE005B130800F05B2700F05B27005B13
      0800B4000000B400000066494200664942005B130800F05B2700FE753F00E51E
      1300E51E1300B4000000E51E1300737373000000000000000000000000000000
      00000000000000000000FFFFFF000000000012FF120000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      000013FF13000000000000000000000000000000000000000000008CBD0084E7
      FF0042B5DE005AC6EF00ADFFFF008CE7FF008CE7FF008CE7FF008CE7FF008CE7
      FF008CE7FF008CE7FF004ABDE700CEF7FF00CEF7FF006394D600000000000000
      0000000000000000000041414100B4CCFF0000000000CCD8FF007272720072DC
      DC0066DCDC00535353008A8A8A008F8F8F0097979700A5A5A500B7B7B700CBCB
      CB00DEDEDE00EDEDED00F7F7F700FCFCFC008B878500AC341D00F05B2700AC34
      1D005B1308005B1308005B130800AC341D00F05B2700F05B2700F05B2700B400
      0000E51E1300E51E1300B4000000D0AB94000000000000000000000000000000
      00000000000000000000FFFFFF002B2B2B0011FE110000000000000000000000
      0000000000000000FF000000000000000000000000000000000000000000FEFE
      FE0012FF12000000000000000000000000000000000000000000008CBD008CE7
      FF007BDEFF0021A5CE00E7FFFF00CEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7
      FF00CEF7FF00CEF7FF0094EFFF00E7FFFF00CEF7FF0042ADCE00000000000000
      00000000000000000000434343009BB8FF00C6D8FF009696960073FFFF006262
      62006DDCDC0066DCDC0056565600868686008A8A8A0093939300A2A2A200B5B5
      B500CACACA00DEDEDE00EDEDED00F7F7F700F7F7F70066494200F05B2700F05B
      27005B130800F05B2700F05B2700F05B2700F05B2700F05B2700F05B2700B400
      0000E51E1300E51E13005B130800000000000000000000000000000000000000
      00000000000000000000FFFFFF000000000005FF0500FEFFFE00000000000000
      0000000000000000FF00FEFEFE00000000000000000000000000000000000000
      000000FF00000000000000000000000000000000000000000000008CBD0094EF
      FF009CDEE7005AADCE004AA5BD004294B5004294B5004294B5004294B5004294
      B50021739400297B9C004294B5004AA5BD00008CBD0042ADCE00000000000000
      000000000000000000003939390095B5FF0072727200BBFFFF006DFFFF0068FF
      FF005F5F5F0069DCDC0066DCDC004E4E4E00848484008888880092929200A1A1
      A100B5B5B500CACACA00DEDEDE00EDEDED000000000066494200F05B2700F05B
      2700F05B2700F05B2700AC341D005B130800F05B2700FE753F00E51E1300E51E
      1300B4000000E51E13005B130800000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF0000FF0000FEFFFE00000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008CBD009CF7
      FF009CBDC600BDFFBD0018A51800EFDEF700D6D6D600D6D6D600D6D6D600D6D6
      DE006B6B6B006B6B6B008C8C8C00000000000000000000000000000000000000
      000000000000000000000000000056565600B8FFFF00BAFFFF006262620056FF
      FF005AFFFF005A5A5A0066DCDC0063DCDC004E4E4E0083838300888888009292
      9200A1A1A100B5B5B500CACACA00DEDEDE0000000000DEDEDE00AC341D00F05B
      2700F05B27005B130800AC341D0066494200F05B2700FE753F00B4000000E51E
      1300B4000000B40000008B878500000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008CBD00E7FF
      FF00A5FFFF00B5ADB500EFE7F700D6CE7B00FFDEC600EFDEBD00EFDEBD00D6DE
      9C00949C7B008C8C8C0084848400212121000000000000000000000000000000
      00000000000000000000000000005A5A5A00B8FFFF00B5FFFF00B7FFFF006262
      62004EFFFF0056FFFF005353530066DCDC005DDCDC004E4E4E00838383008888
      880092929200A1A1A100B5B5B500CACACA0000000000F7F7F70073737300F05B
      2700F05B2700AC341D00B4000000AC341D00F05B2700FE753F00B4000000E51E
      1300B4000000B40000008B878500000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00000000000000000000000000FEFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000008C
      BD00E7FFFF00A5FFFF007B737300FFFFFF00F7CEA500E7C69400BDAD1800BDA5
      0000DECE420000000000949494008C8C8C00847B7B0000000000000000000000
      00000000000000000000000000000000000056565600B6FFFF00B5FFFF00B4FF
      FF005A5A5A004EFFFF004EFFFF005151510066DCDC0066DCDC00434343008383
      83008888880092929200A1A1A100B5B5B5000000000000000000CECECE00AC34
      1D00F05B2700F05B27005B130800AC341D00FE753F00E51E1300E51E1300E51E
      1300E51E13005B130800CECECE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007023
      2300FBFBFB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000008CBD00008CBD00008CBD006B6B6300F7CEAD00B5BD5200C6BD6300C6B5
      6B00CEB529008484310031315200947B7B004A5252007B7B7B00000000000000
      0000000000000000000000000000000000000000000056565600B4FFFF00B4FF
      FF00B4FFFF00565656004EFFFF004BFFFF00515151004CDADA0059DADA004646
      4600838383008888880092929200A1A1A1000000000000000000000000006649
      4200F05B2700F05B27005B130800AC341D00FE753F00E51E1300E51E1300E51E
      1300E51E13006649420000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000848C9400C6C65A00C69C5A00C69C
      5A00C6A53100BD9C0000846B00005A525A00294A4A0063636300000000000000
      00000000000000000000000000000000000000000000FEFEFE0048484800B4FF
      FF00B4FFFF00B4FFFF00565656004BFFFF003CFFFF004646460053DADA0050DA
      DA0046464600838383008888880092929200000000000000000000000000CECE
      CE00AC341D00F05B2700F05B2700F05B2700FE753F00E51E1300E51E1300B400
      0000E51E13006649420000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000094949400ADADBD00848C94008484
      8C00848C94008C8CA500949CAD00000000006B63630000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFE004343
      4300B2FFFF00AFFFFF00B0FFFF00484848003FFFFF003CFFFF004343430053DA
      DA004CDADA003C3C3C008383830088888800000000000000000000000000DEDE
      DE0066494200F05B2700FE753F00F2916100F2916100E51E1300E51E1300B400
      00005B130800DEDEDE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084848400B5B5B500BDBDBD00BDBDBD00BDBD
      BD00A59C9C00848484008C8C8C00949494000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FE0043434300ADFFFF00AFFFFF00B0FFFF00434343003FFFFF0039FFFF003C3C
      3C0050DADA004CDADA003C3C3C00838383000000000000000000000000000000
      000073737300F2916100F2916100F2916100F2916100F2916100E51E1300E51E
      13005B130800DEDEDE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400BDBDBD00C6C6C600BDBDBD00BDBDBD00BDBD
      BD00BDBDBD007B7B7B004A4A4A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEFEFE003C3C3C00AFFFFF00AFFFFF00B2FFFF003C3C3C003FFFFF0039FF
      FF003C3C3C0050DADA004CDADA003C3C3C000000000000000000000000000000
      00000000000073737300F2916100F2916100F2916100F2916100F2916100E51E
      130066494200EBE6E60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000094949400C6D6BD00CEEFCE00CED6CE00C6C6C600C6C6C600C6C6
      C600B5B5B5007373730000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEFEFE003C3C3C00ADFFFF00ABFFFF00B1FFFF003C3C3C003FFF
      FF0039FFFF003C3C3C0049DADA004CDADA000000000000000000000000000000
      000000000000F7F7F70073737300F2916100F2916100F2916100F29161008B87
      85008B8785000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840084848400848484008484840084848400848484008484
      840084848400DE7BDE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEFEFE003C3C3C00ADFFFF00ADFFFF00AFFFFF003C3C
      3C003FFFFF0039FFFF003939390049DADA000000000000000000000000000000
      00000000000000000000F7F7F70073737300F2916100AC341D0066494200D0AB
      9400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FEFEFE003C3C3C00ABFFFF00ADFFFF00AFFF
      FF003C3C3C003CFFFF0039FFFF003939390000000000EFEFEF009C9C9C001052
      630000315A0010294A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EFEFEF009C9C9C001052630000315A0010294A00000000000000
      000000000000000000000000000000000000000000000000000010294A000031
      5A00105263009C9C9C00EFEFEF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EFEFEF009C9C9C001052630000315A0010294A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000315A6300005AAD0000B5
      F700008CFF00005AE70010294200000000000000000000000000000000000000
      00000000000000000000EFEFEF00DEDEDE00E7E7E70000000000000000000000
      000000000000315A6300005AAD0000B5F700008CFF00005AE700102942000000
      0000000000000000000000000000000000000000000010294200005AE700008C
      FF0000B5F700005AAD00315A6300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000315A6300005AAD0000B5F700008CFF00005AE700102942000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7F7F70000000000000000000000000000000000009CBD00007BE70000BD
      F700008CFF000063FF0008214A00000000000000000000000000000000000000
      000000000000EFEFEF007B847B005A5A5A0042424200ADADAD00000000000000
      000000000000009CBD00007BE70000BDF700008CFF000063FF0008214A000000
      0000000000000000000000000000000000000000000008214A000063FF00008C
      FF0000BDF700007BE700009CBD00000000000000000000000000000000000000
      00009CADBD0010295A0010396B0029314A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000009CBD00007BE70000BDF700008CFF000063FF0008214A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000009CBD00007BE70000BD
      F700008CFF000063FF0008214A00000000000000000000000000000000000000
      000000000000737B7300DEDEDE00EFEFEF00ADADAD0031313100C6C6C6000000
      000000000000009CBD00007BE70000BDF700008CFF000063FF0008214A000000
      0000000000000000000000000000000000000000000008214A000063FF00008C
      FF0000BDF700007BE700009CBD0000000000000000000000000000000000ADBD
      C600004A7300006BDE00005AF70000317B0029314A0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000009CBD00007BE70000BDF700008CFF000063FF0008214A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000009CBD00007BE70000BD
      F700008CFF000063FF0008214A00000000000000000000000000000000000000
      00000000000063636300CECECE00DEDEDE00ADADAD0029292900BDBDBD000000
      000000000000009CBD00007BE70000BDF700008CFF000063FF0008214A000000
      0000000000000000000000000000000000000000000008214A000063FF00008C
      FF0000BDF700007BE700009CBD00000000000000000000000000E7E7E7007B8C
      9C00009CD600009CF700007BFF000063FF0000317B0029395200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000009CBD00007BE70000BDF700008CFF000063FF0008214A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000009CBD00007BE70000BD
      F700007BF700005AF70008214A00000000000000000000000000000000000000
      0000DEDEDE0029636B0052525200847B7B006363630039393900D6D6D6000000
      000000000000009CBD00007BE70000BDF700007BF700005AF70008214A000000
      0000000000000000000000000000000000000000000008214A00005AF700007B
      F70000BDF700007BE700009CBD00000000000000000000000000E7E7E7003973
      9400007BD60000B5F700009CFF00007BFF00005AF70000317B00636B8400D6DE
      DE00EFEFEF000000000000000000000000000000000000000000000000000000
      000000000000009CBD00007BE70000BDF700007BF700005AEF0008214A000000
      0000000000000000000000000000000000000000000000000000000000007394
      AD000073AD004A6B9C00000000000000000000000000009CBD00008CEF0000BD
      F70000B5F7000039D60018295200000000000000000000000000000000000000
      0000636B6B0039BDE70018397B001018310010183100184A9C0052636B00F7F7
      F70000000000009CBD00008CEF0000BDF70000B5F7000039D600182952000000
      000000000000000000000000000000000000E7E7E700182952000039D60000B5
      F70000BDF700008CEF00009CBD00000000000000000000000000BDBDBD003984
      9C00008CD600008CEF0000B5F700009CF7000073F7000042DE0008318C00526B
      A50094949400E7E7E70000000000000000000000000000000000000000000000
      000000000000009CBD00008CEF0000BDF70000B5F7000039D600182952000000
      000000000000000000000000000000000000000000000000000000000000008C
      D60000A5FF000042A500000000000000000000000000009CBD0000CEFF0000CE
      FF0000CEFF00005AF70010318C00E7E7E7000000000000000000000000000000
      0000004A5A005AE7FF007BCEFF005AADFF00009CFF00009CFF000084DE00ADAD
      AD0000000000009CBD0000CEFF000000000000000000005AF70010318C00BDBD
      BD0000000000000000000000000000000000BDBDBD0010318C00005AF7000000
      00000000000000CEFF00009CBD00000000000000000000000000DEDEDE00949C
      A500219CB500008CD600008CEF0000B5F70000ADF700009CF700005AF7000052
      DE003163B5008C8C8C00E7E7E700000000000000000000000000000000000000
      000000000000009CBD0000CEFF0000CEFF0000CEFF00005AF70010318C00BDBD
      BD0000000000000000000000000000000000000000000000000000000000008C
      D600009CFF000042A50000000000000000001052630000C6F70000CEFF0000CE
      FF0000CEFF000063FF000052DE00CECECE000000000000000000000000000000
      0000004A5A0000CEFF0010D6FF0042DEFF009CEFFF00009CFF00005ADE00ADAD
      AD001052630000C6F70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000C6F70010526300000000000000000000000000DEDE
      DE00949CA500219CB500008CD600008CEF0000C6F70000CEFF0000ADFF000063
      FF00005AE7003163B5008C8C8C00E7E7E7000000000000000000000000000000
      00001052630000C6F70000CEFF0000CEFF0000CEFF000063FF000052DE007B7B
      7B000000000000000000000000000000000000000000000000000000000000A5
      DE0000C6F7000839AD00EFEFEF0000000000000000000000000000CEFF000000
      0000000000000063FF0000000000000000000000000000000000000000000000
      00000000000000CEFF00000000000000000000CEFF000000000000000000ADAD
      AD00004A5A0000CEFF0000CEFF0000000000000000000063FF000052DE007B7B
      7B00000000000000000000000000000000007B7B7B000052DE000063FF000000
      00000000000000CEFF0000CEFF00004A5A000000000000000000000000000000
      0000DEDEDE00949CA500219CB500009CD60000CEFF0000CEFF0000CEFF0000AD
      FF000063FF000063E7003173B5009C9C9C00F7F7F70000000000000000000000
      0000004A5A0000CEFF0000CEFF0000CEFF0000CEFF000063FF000052DE007B7B
      7B0000000000000000000000000000000000000000000000000084ADB50000CE
      FF0000CEFF00005AEF00BDBDBD0000000000004A5A0000CEFF0000CEFF0000CE
      FF0000CEFF000063FF000052DE00CECECE000000000000000000000000000000
      0000004A5A0000CEFF0000CEFF0000CEFF0000CEFF000063FF000052DE00ADAD
      AD00004A5A0000CEFF0000CEFF0000CEFF0000CEFF000063FF000052DE007B7B
      7B00000000000000000000000000000000007B7B7B000052DE000063FF0000CE
      FF0000CEFF0000CEFF0000CEFF00004A5A000000000000000000000000000000
      000000000000DEDEDE00949CA500219CBD0000C6F70000CEFF0000CEFF0000CE
      FF0052C6FF00009CFF000084DE00849CA500F7F7F70000000000000000000000
      0000004A5A0000CEFF0000CEFF0000CEFF0000CEFF000063FF000052DE007B7B
      7B000000000000000000000000000000000000000000000000007BA5AD0000CE
      FF0031DEFF00006BEF00BDBDBD0000000000004A5A0000CEFF0010D6FF0042DE
      FF009CEFFF00009CFF00005ADE00CECECE000000000000000000000000000000
      00001052630000C6F70000CEFF0000CEFF0000CEFF000063FF000052DE00ADAD
      AD00004A5A0000CEFF0010D6FF0042DEFF009CEFFF00009CFF00005ADE007B7B
      7B00000000000000000000000000000000007B7B7B00005ADE00009CFF009CEF
      FF0042DEFF0010D6FF0000CEFF00004A5A000000000000000000000000000000
      00000000000000000000D6DEDE002163730000ADD60000CEFF0000CEFF0029D6
      FF0052C6FF00008CEF001052AD007B8494000000000000000000000000000000
      0000004A5A0000CEFF0010D6FF0042DEFF009CEFFF00009CFF00005ADE007B7B
      7B000000000000000000000000000000000000000000000000009CADB5004AAD
      D60021639C001873BD00DEDEDE0000000000004A5A005AE7FF007BCEFF005AAD
      FF00009CFF00009CFF000084DE00CECECE000000000000000000000000000000
      000000000000009CBD0000CEFF0000CEFF0000CEFF00005AF70010318C00D6D6
      D600004A5A005AE7FF007BCEFF005AADFF00009CFF00009CFF000084DE007B7B
      7B00000000000000000000000000000000007B7B7B000084DE00009CFF00009C
      FF005AADFF007BCEFF005AE7FF00004A5A000000000000000000000000000000
      0000000000000000000000000000ADBDC6000052630000ADD60000CEFF0042CE
      FF005A9CE7001031520031425200393939000000000000000000000000000000
      0000004A5A005AE7FF007BCEFF005AADFF00009CFF00009CFF000084DE007B7B
      7B00000000000000000000000000000000000000000000000000F7F7F7006B7B
      7B009C9C9C007B7B7B000000000000000000636B6B0039BDE70018397B001018
      310010183100184A9C0052636B00F7F7F7000000000000000000000000000000
      000000000000009CBD00008CEF0000BDF70000B5F7000039D60018295200EFEF
      EF00636B6B0039BDE70018397B001018310010183100184A9C0052636B00EFEF
      EF0000000000000000000000000000000000EFEFEF0052636B00184A9C001018
      31001018310018397B0039BDE700636B6B000000000000000000000000000000
      000000000000000000000000000000000000ADBDC6000052630018B5DE0063CE
      F700295284004A525A0084848400848484003131310000000000000000000000
      0000636B6B0039BDE70018397B001018310010183100184A9C0052636B00EFEF
      EF0000000000000000000000000000000000000000000000000000000000B5B5
      B500948C8C00ADADAD000000000000000000DEDEDE0029636B0052525200847B
      7B006363630039393900D6D6D600000000000000000000000000000000000000
      000000000000009CBD00007BE70000BDF700007BF700005AF70008214A000000
      0000DEDEDE0029636B0052525200847B7B006363630039393900D6D6D6000000
      00000000000000000000000000000000000000000000D6D6D600393939006363
      6300847B7B005252520029636B00DEDEDE000000000000000000000000000000
      00000000000000000000000000000000000000000000ADBDC60008526B0042AD
      CE00316B84007B737300DEDEDE00C6C6C6007B7B7B0000000000F7F7F7000000
      0000DEDEDE0029636B0052525200847B7B006363630039393900D6D6D6000000
      000000000000000000000000000000000000000000000000000000000000F7F7
      F700EFEFEF000000000000000000000000000000000063636300CECECE00DEDE
      DE00ADADAD0029292900BDBDBD00000000000000000000000000000000000000
      000000000000009CBD00007BE70000BDF700008CFF000063FF0008214A000000
      00000000000063636300D6CECE00DEDEDE00ADADAD0029292900BDBDBD000000
      00000000000000000000000000000000000000000000BDBDBD0029292900ADAD
      AD00DEDEDE00D6CECE0063636300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6CECE009CA5
      AD006384840094949400DEDEDE00BDBDBD0000000000CECECE00000000000000
      00000000000063636300D6CECE00DEDEDE00ADADAD0029292900BDBDBD000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000737B7300DEDEDE00EFEF
      EF00ADADAD0031313100C6C6C600000000000000000000000000000000000000
      000000000000009CBD00007BE70000BDF700008CFF000063FF0008214A000000
      000000000000737B7300DEDEDE00EFEFEF00ADADAD0031313100C6C6C6000000
      00000000000000000000000000000000000000000000C6C6C60031313100ADAD
      AD00EFEFEF00DEDEDE00737B7300000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F7F7
      F700F7F7F70094949400ADADAD000000000000000000EFEFEF00000000000000
      000000000000737B7300DEDEDE00EFEFEF00ADADAD0031313100C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EFEFEF007B847B005A5A
      5A0042424200ADADAD0000000000000000000000000000000000000000000000
      000000000000009CBD00007BE70000BDF700008CFF000063FF0008214A000000
      000000000000EFEFEF007B847B005A5A5A0042424200ADADAD00000000000000
      0000000000000000000000000000000000000000000000000000ADADAD004242
      42005A5A5A007B847B00EFEFEF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E7E7E700F7F7F70000000000000000000000
      000000000000EFEFEF007B847B005A5A5A0042424200ADADAD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFEFEF00DEDE
      DE00E7E7E7000000000000000000000000000000000000000000000000000000
      000000000000315A6300005AAD0000B5F700008CFF00005AE700102942000000
      00000000000000000000EFEFEF00DEDEDE00E7E7E70000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E7E7
      E700DEDEDE00EFEFEF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000EFEFEF00DEDEDE00E7E7E70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DEDEDE00BDBDBD006B94A50063849C006B7B9400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009F9F9F009F9F9F009F9F9F009F9F9F00DBDBDB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CECE
      CE00493F3F0087000000B47272008F000000970000005B2727008F7B7B00DEDE
      DE00EEEEEE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000007F6F
      6F008F000000BF000000D28B8B00BF000000BF000000BF000000A70000005700
      00007F3F3F007F7F7F00CFCFCF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF008080800080808000808080008080
      800080808000FFFFFF000000000000000000000000000000000000000000531F
      1F00BB000000C2000000C8868600BF000000BF0C0C00C1121200C2000000C200
      0000B7000000A100000083131300875B5B00D7C7C70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0080808000808080008080
      8000808080008080800080808000FFFFFF000000000000000000000000000000
      0000000000000000000000000000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF000000000000000000000000000000000000000000531F
      1F00BB000000C2000000C8868600BF000000C1121200F57E7E00E09F9F00BD72
      7200BF000000BB000000BB000000C20000004B20200000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000808080000000000080808000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000FFFFFF00C0C0C00000000000FFFFFF00C0C0C000C0C0C000808080008080
      800080808000FFFFFF0000000000000000000000000000000000000000004B1F
      1F00C2000000BB000000DD909000BF000000C2000000C2000000CD5F5F000000
      0000EBEBEB00E56E6E00C2232300BB0000005320200000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000008080800000000000808080000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0080808000808080008080
      8000808080008080800080808000FFFFFF000000000000000000000000000000
      0000000000000000000000000000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
      FF00C0C0C0008000000080000000800000008000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000004B1F
      1F00C2000000BB000000DD909000BF000000C2000000C2000000CD5F5F000000
      000000000000F47F7F00E0535300BB0000005320200000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008080
      8000808080008080800080808000808080008080800080808000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000FFFFFF00C0C0C000FF0000008000000080000000C0C0C000808080008080
      800080808000FFFFFF0000000000000000000000000000000000000000004F1F
      1F00BF000000BF000000D28B8B00BF000000BF000000BF000000D75F5F000000
      000000000000C61F1F00BF000000BF0000004F20200000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008080
      8000808080008080800080808000808080008080800080808000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF0080808000808080008080
      8000808080008080800080808000FFFFFF000000000000000000000000000000
      0000000000000000000000000000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
      FF00C0C0C0008000000080000000FF0000008000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF000000000000000000000000000000000000000000531F
      1F00BB000000C2000000C8868600BF000000BF0C0C00C13A3A00DCD2D2000000
      000000000000C11F1F00BB000000C20000004B20200000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008080
      8000808080008080800080808000808080008080800080808000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00C0C0C000C0C0C000C0C0C0000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000800000008000000080000000FFFFFF0080000000FFFFFF00FFFFFF00FFFF
      FF0080808000808080000000000000000000000000000000000000000000531F
      1F00BB000000C2000000C8868600BF000000C1121200C4565600EF8C8C00F5F5
      F50000000000C11F1F00BB000000C20000004B20200000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0008080
      8000C0C0C00080808000C0C0C00080808000C0C0C00080808000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00C0C0C000FFFFFF00C0C0C0008000
      000080000000800000000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00808080000000000000000000000000000000000000000000000000004B1F
      1F00C2000000BB000000DD909000BF000000C2000000C2000000BB000000B50F
      0F00D28B8B00CC131300C2000000BB0000005320200000000000000000000000
      000000000000000000000000000000000000000000000000000080808000C0C0
      C00080808000C0C0C00080808000C0C0C00080808000C0C0C000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00C0C0C000C0C0C000C0C0C0000000000000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000FFFFFF00C0C0C000800000008000
      000080000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004B1F
      1F00C2000000BB000000DD909000BF000000C2000000C2000000DAA6A600EDBA
      BA00B34B4B00C2000000C2000000BB0000005320200000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00C0C0C000FFFFFF00C0C0C0008000
      0000C0C0C000FFFFFF00C0C0C000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004F1F
      1F00BF000000BF000000D28B8B00BF000000BF000000BF000000000000000000
      0000DF7F7F00BF000000BF000000BF0000004F20200000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
      C000FFFFFF00C0C0C000FFFFFF00C0C0C0000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000531F
      1F00BB000000C2000000C8868600BF000000BB000000BB000000D1A5A500E5B9
      B900D24B4B00BB000000BB000000C20000004B20200000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFF
      FF00C0C0C000FFFFFF00C0C0C000FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000531F
      1F00BB000000C2000000C8868600BF000000BB000000BB000000C2000000C200
      0000BF000000BB000000BB000000C20000004B20200000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00C0C0
      C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C0008000000080000000800000008000
      00008000000080000000FFFFFF00C0C0C0000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000331F
      1F004A2C2C005656560056565600535353004C383800810000007F000000BB00
      0000BF000000C2000000C2000000BB0000005320200000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0C000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF0000FF000000FF000000FF00
      0000FF0000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003D3D
      3D00BDBDBD00DDDDDD00DDDDDD00DFDFDF00D1D1D100ABABAB0073676700501D
      1D004B1C1C00A6040400AC000000AC0000005720200000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFF0000FF000000FF000000FF00
      0000FF0000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008F8F
      8F006B6B6B0073737300BFBFBF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDF
      DF00DFDFDF008383830073737300572727004020200000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EBEBEB00DFDFDF00DDDDDD005B5B5B005C5C5C007F7F7F00AAAAAA00CFCF
      CF00CECECE00A2A2A200C5C5C500E9E9E900E3E3E30000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C3C3C3009F9F9F00C0C0
      C0003B3B3B004545450000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000946E5C0090685A008660540086605600845E
      5400825C54007E5A54007C5852007A565200785452007854520074505000704C
      4E00724C4C003222200000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007670680064565C00C2968A00FAD4CC00FED4CA00FED4CA00FED2
      C600FED2C600FED0C400FED0C200FECEC000FECCBE00FCCABC00FAC8B800FCCE
      BC00E8B0A000482E2C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF007F7F
      7F007F7F7F007F7F7F007F7F7F007F7F7F007F7F7F007F7F7F007F7F7F007F7F
      7F007F7F7F007F7F7F007F7F7F007F7F7F007F7F7F00BFBFBF00000000000000
      000000000000587EA0006474AC00A68C9E00EADED000FEF0D600FCE8CE00FEE8
      CA00FEE4C600FEE2C200FEE0BC00FEDEB600FEDAB200FED8AC00FED4A600FED8
      A600E8B49A0042282A0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000000000000000FF000000FF000000
      0000000000000000FF000000FF0000000000000000000000FF000000FF000000
      0000000000000000FF000000FF000000000000000000000000007F9F9F002FAF
      AF002FAFAF002FAFAF002FAFAF002FAFAF002FAFAF002FAFAF002FAFAF002FAF
      AF002FAFAF002FAFAF002FAFAF002FAFAF002FAFAF00003F3F00000000000000
      00000000000054BEFE004098EE005E6AA800B89CA800FAE6D200FEECD600FCE4
      CE00FEE2C800FEE0C400FEDEC000FEDCBC00FEDAB600FED8B200FCD4AC00FEDA
      AC00E6B69C0042282A0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000000000000000000BFBFBF005FDF
      DF005FDFDF005FDFDF005FDFDF005FDFDF002FAFAF005F5F5F003F3F3F002F6F
      6F005FDFDF005FDFDF005FDFDF005FDFDF005FDFDF00003F3F00000000000000
      000000000000000000005CC6FE003E96EC005E68A600B89CA800F8E2D000FEEA
      D400FEEAD200FEE8CC00FEE4C800FEE0C200FEDCBA00FEDAB600FCD6B000FEDC
      B000E6B89E0042282A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008000000000000000000000008000000000
      0000000000000080000000000000000000000080000000000000000000000000
      0000008000000000000000000000000000000000000000000000BFBFBF005FDF
      DF005FDFDF005FDFDF002FAFAF005F5F5F005F5F5F005F5F5F003F3F3F003F3F
      3F003F3F3F002F6F6F005FDFDF005FDFDF005FDFDF00003F3F00000000000000
      00000000000000000000000000006EC0F0003E92E8005662A400CCBABE00FAEC
      D800EACEBE00ECCAB600F0D0BA00F8D8BE00FEE2C400FEDCBA00FCD8B400FEDE
      B600E8B8A200442A2A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000000000000000000000
      0000000000000000000000000000008000000000000000000000000000000080
      0000000000000000000000000000000000000000000000000000BFBFBF005FDF
      DF005FDFDF005FDFDF004F4F4F005F5F5F008F8F8F00BFBFBF007F7F7F007F7F
      7F003F3F3F003F3F3F002F6F6F005FDFDF005FDFDF00003F3F00000000000000
      0000000000000000000000000000D4B2980080CAF8006098D000A2989800D8B4
      A200DEC0A000E6D2B000E6D2B600DEC2AE00ECC8B000FEE2C200FCDAB800FEE0
      BA00E8BCA400462C2C0000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000800000000000000000
      0000008000000000000000800000008000000000000000000000008000000000
      000000000000000000000000FF00000000000000000000000000BFBFBF005FDF
      DF005FDFDF004F8F8F009F9F9F00BFBFBF00CFCFCF00DFDFDF009F9F9F007F7F
      7F007F7F7F007F7F7F001F1F1F005FDFDF005FDFDF00003F3F00000000000000
      0000000000000000000000000000DEB09200F0EEE800E2DCD800C8A08E00FAE6
      B800FEFECE00FEFEDE00FEFEE800FCFCF600D8BEB600EECCB200FEE0C200FEE0
      BE00EABEA600482E2C0000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000008000000000
      0000000000000080000000000000000000000080000000800000000000000000
      000000000000000000000000FF00000000000000000000000000BFBFBF005FDF
      DF005FDFDF006FAFAF00DFDFDF00DFDFDF00CFCFCF005F5FDF00BFBFBF00BFBF
      BF009F9F9F007F7F7F003F3F3F005FDFDF005FDFDF00003F3F00000000000000
      0000000000000000000000000000DCB49600F6F0EC00EED0C800F6DAB000FEEE
      BC00FCF6C800FEFCE000FCFCF400FFFFFF00F6F4DA00D2B09C00FEE4C600FEE4
      C200ECC0AA004A302E0000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF005FDF
      DF005FDFDF007FBFBF006FAFAF00BFBFBF005FDF5F00BFBFBF00BFBFBF00DFDF
      DF005F5F5F009F9F9F004F8F8F005FDFDF005FDFDF00003F3F00000000000000
      0000000000000000000000000000E2B89A00F0ECEC00E6CABC00FEF0BC00FEDC
      AA00FEF8C800FEFEDE00FEFCEC00FEFEEC00FCFED600D8C0A400F6D8C200FEE8
      CA00ECC2AE004E322E0000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BFBFBF005FDF
      DF005FDFDF005FDFDF005FDFDF007FBFBF007FBFBF008F8F8F00000000000000
      0000BFBFBF005F9F9F005FDFDF005FDFDF005FDFDF00003F3F00000000000000
      0000000000000000000000000000E4BC9C00F2EEF000E6CABE00FEF4BC00FEDC
      AA00FEEEBC00FEFED200FEFEDA00FEFEDA00FCFACA00DABEA200F6DAC600FEEA
      D000EEC6B0005036300000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      000000000000000000000000FF00000000000000000000000000BFBFBF005FDF
      DF005FDFDF005FDFDF005FDFDF005FDFDF005FDFDF007FBFBF007FBFBF000000
      0000DFDFDF008F8F8F002FAFAF005FDFDF005FDFDF00003F3F00000000000000
      0000000000000000000000000000E8BC9C00F6F6F600EAD4CC00F6EEC800FEF2
      DA00FCDAAE00FEECB800FEEEBC00FEF4C200FAEAB600D2AC9800FCE8D400FEEA
      D200F0C8B2005238300000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF00000000000000000000FFFF000000
      000000000000000000000000FF00000000000000000000000000BFBFBF005FDF
      DF005FDFDF005FDFDF005FDFDF005FDFDF005FDFDF005FDFDF005FDFDF006FAF
      AF004FCFCF005FDFDF005FDFDF005FDFDF005FDFDF00003F3F00000000000000
      0000000000000000000000000000EAC09E00FAFAF800F4EAEC00E4CCBC00FFFF
      FF00FEF6DC00FEE6B200FEE4AE00FEEEB800DEB69600E6CABE00FEEED600FEEA
      D200F2CAB600563C320000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF00000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000AFAFAF00DFDF
      DF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00DFDFDF00BFBFBF00BFBF
      BF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00000000000000
      0000000000000000000000000000EEC4A200F8F6F400FFFFFF00EADADA00DCC2
      B600F0E2C200F2E2B400F6E2B600E2BEA000E0C4BA00FEF2E400FEE8D200FEEC
      D600F2CAB600583C320000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000DFDFDF009FDF
      DF005FDFDF005FDFDF005FDFDF005FDFDF005FDFDF00AFAFAF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000F2C8A400FAF8F600FEFEFE00FFFFFF00F2EC
      EA00DCC6C200E2CAC200E8CEC800F2E2DC00FEF8EC00FEF2E400FADCD200FCC0
      C200F4AEA4005A42380000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000000000000000000000000000DFDF
      DF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000F6CCA600FCFAFA00FEFEFE00FEFEFE00FFFF
      FF00FFFFFF00FFFFFF00FEFEFC00FEF8F200FEFCF200E2CEC800B6827200C084
      7000B67E78006248420000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF0000000000000000000000FF000000FF000000
      0000000000000000FF000000FF0000000000000000000000FF000000FF000000
      0000000000000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000F8CEA600FCFCFA00FFFFFF00FEFEFE00FEFE
      FE00FEFCFC00FEFEFE00FEFAFA00FCF6F200FEFEFA00DCC6C200DEB28400E6B0
      7A009A6E58000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCD2A800FCFCFA00FEFEFE00FEFEFE00FEFE
      FE00FCFCFC00FCFCFC00FCFCFA00FAF6F200FEFEFA00DAC6C200C2927800B68E
      7600FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCCEA400FAE8D800FAE8DA00F6E6D800F4E4
      D800F0E0D400ECDCD400EADAD400E4D6CE00ECDED400CAB0AC00B47E6A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000545454009C9C9C00787878002C28
      2800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008B8B8B00595959003A3939004B4C4C00737272007A6D
      6D00716E6E00525252008E8E8E00000000000000000000000000000000000000
      0000967F78009076770099737F00736670000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000008080800A09CA000B8D8C400BCE4C800BCB0B8009888
      8800746464002C28280000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000059595900A3A3A300D6D6D6009B9C9C00746F6F008E6E6E00A78F
      8F00D2D2D200CACACA0082828200595959008181810000000000000000000000
      0000627F8E004E6AA100907FA400B7868F00886D7A0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005858
      580058585800444440007C7C7C00E8E8E800DCE0E000CCD0D000BCBCBC00A8A4
      A400BCA0A000B890900080606000302828000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002D292C002C2C2C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CDCDCD007171
      71008D8D8D00E1E1E100FEFEFE00D6D6D600AEAFAF00B5B1B100635B5B005355
      550070707000A3A3A300D6D6D600D7D7D700A8A8A8005E5E5E00000000000000
      00004DAFFD003C97F3004969B3008F80A000B1838C005C5E6600FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A4A4A400D8D8
      D800D8D8D800D4D4D800E8E8E800E0E0E000DCD4CC00E8D4C400ECE8E400D0C8
      BC00B0A8A400B49CA400D4A0A400D49494009464640030242400000000000000
      0000000000000000000000000000000000000000000000000000423F40007A83
      7D00B6BFB900B6B2B3005B545400242121000000000000000000000000000000
      000000000000000000000000000000000000000000000000000088888800D2D2
      D200FEFEFE00F7F7F700EEEEEE00C7C7C700A3A3A300AEAFAF00999B9B008685
      8500777777007373730085858500AAABAA00C7C8C70076767600000000000000
      000063B8F30055BEFF003A97F300466FBC009081A100BC86900071636E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CCCCCC00F0EC
      EC00ECE8E400F4E8D800E8E0D000F0F0F000F0DCBC00FCD8B400FCF8F400FCDC
      B800F0BC8C00C8A88400A4948800B0909800E09CA400F4989800885C5C000000
      000000000000000000000000000000000000000000001A1C1B00B6AFB400C7EB
      D200B2E3C100C7BFC200A6979600A78F8F006F5C5C00241F1F00000000000000
      0000000000000000000000000000000000000000000000000000DADADA00FEFE
      FE00F1F1F100D2D2D200ACACAC0089898900808080008D8D8D00A3A2A200B9B9
      B900B9B9B900B0B0B0009C9C9C006A6769009B94990054545400000000000000
      0000000000006DAEE70057BBFE003B99F500456BB8008E83A200B7858E006052
      5A00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B4B4B400D0D0
      D000D0C8BC00DCBC9800E8D8BC00FFFFFF00F8E4C400FCDCB800FCF8F400FCD8
      B400F8BC8400F4AC6400E8984C00C488440098785400A8848000A47474000000
      00000000000048484800757575005F5F5F005655550092929200E7E7E700DCDC
      DF00D1D0D400C8C8C800AEAAAC00B29D9F00C8A09F00BC8E8E00715555001D19
      1900000000000000000000000000000000000000000000000000CDCDCD00D9D9
      D900A6A6A6009A9A9A00BDBDBD00C7C7C700A0A0A0008F8F8F00828282008282
      820090909000A6A6A600C1BDC000A6BCAD007FB08F005D595B00000000000000
      0000000000000000000065B8F30056BBFE00379BF900476CB7008F83A300BA88
      9600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001C1008005C54500084787400A49C9800C8B09800E8C4A800FCF0E800FCE0
      BC00FCC88C00F0B07000D0905000A4703400805C38008C6C6C00A07070000404
      0000AAAAAA00C8C8C800DEDEDE00DEDFE100E4E6E900E6E7E900DFDFDF00DFD7
      CD00EAD6C200F4F0EC00E2D6C900C0B4A900ABA0A200C09CA000DF9FA000CC8B
      8900775252001B171700000000000000000000000000B0B0B00091919100A8A8
      A800C6C6C600DFDFDF00D8D8D800E1E1E100E3E3E300D4D4D400C5C5C500B5B5
      B5009E9E9E00888888008E8A8D00788A7E005985670059555700000000000000
      000000000000000000000000000068B6F0005DB8FB003B9AF500436FB6008772
      8C00C16E9200846F770094777700877070008D72760093747F007E5D6A006E5D
      6A00FFFFFF0000000000000000000000000004648C00186488001C688C001C68
      8C00186C940010648800185C7800206480003888A80064A0B40090ACB800ACA0
      9400A0886C0070645400645C580090747C00BC888C00C0808000986464000000
      0000C1C1C100E0E0E000E4E4E500EFEAE600EEE2D500E9DFD100F4F2F000F2DB
      BD00FFD8B100FFFCFA00FFE6CA00F8C48F00D8AF8400B39C8600A4918E00C296
      9C00F29FA100E28C8C005B3C3C000000000000000000AEAEAE00BFBFBF00EBEB
      EB00DEDEDE00CECECE00DADADA00D7D7D700C4C4C400CCCCCC00D8D8D800D4D4
      D400D5D5D500D5D5D500BFBFBF00ADABAC009A9398005B5B5B00000000000000
      00000000000000000000000000000000000054C6F60048B9FF007694E6008C85
      8E0086808900BD898400E5C2A200F4E0B600F9EDC200FAE7C800DBBAAA00A879
      79006E636C00FFFFFF00000000000000000030A0D40060CCFC0070D8FC006CD4
      FC006CD4FC0070DCFC0070E0FC0068D8FC0058C0EC004494B40034607400343C
      4000605458008C747800AC808400986C6C00684C480000000000000000000000
      00009B9B9B00BCBCBC00D2D3D400D5C3B300DEBB9400EEDCC100FFFDF900F9E2
      C300FFDBB500FEFAF800FDE3C900FBC58E00F5B37200EBA05500D38E4700B080
      4C00977B6E00C68E8E00895A59000000000000000000B5B5B500D5D5D500E2E2
      E200D5D5D500DADADA00D8D8D800D5D5D500F7F7F700F1F1F100DDDDDD00D1D1
      D100CDCDCD00C6C6C600CCCCCC00DCDCDC00C9CBCA006B6B6B00000000000000
      0000000000000000000000000000000000000000000000000000B7CCE200AF9C
      9C00D49F8900FEE5B800FFFCCA00FFFFD200FFFFDA00FEFFDC00FFFFE900F7EE
      DD00B6838B00705D6C00000000000000000040A0CC0068C4EC0074D8FC0070DC
      FC0064D0F8005CB4D0005090A8005474840060686C007C787400A8A09C00B4B4
      B400A08C8C00A4949400B4B0B000606468002024240000000000000000000000
      00000000000000000000000000007C7B7B00A6A5A400A7A39F00BABAB900D4C5
      B100EACCAD00FFFCF900FFEACE00FFC88F00FCBA7800EFA45A00CE874000A76D
      2D007C5F4100A07B7B00865D5C00000000000000000000000000A8A8A800BEBE
      BE00BCBCBC00BBBABA00A4A2A200BFBEBD00D0CFCF00D7D7D700EDEDED00F2F2
      F200EBEBEB00E3E3E300D9D9D900C1C1C1009494940000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D4A6
      9200FFE8B800FEF4C300FDF5C100FEFED200FEFDE000FEFDE900FCFBF800FFFF
      FF00F7EEDE00996D73005C656A000000000034A4C80070C4E8007CE0FC006CB4
      CC00849CA400A0A4A400BCB8B800D8D0CC00F4ECEC00F0F0F000DCDCDC00B0B0
      B00098989800BCC0C000D4D4D4009C9C9C00888888009C9C9C00585858000000
      0000000000000000000000000000000000000000000000000000000000006D6D
      6E00A09A9500ACADAC00C9B9A800DCB38A00A58767007A6958006E5F5A009774
      7B00C98B8F00D88989009864640000000000000000000000000000000000B8B8
      B800B8B8B800E1E5E600C0C6C8009BA0A100A2A5A600ABACAC00ABABAB00B2B2
      B200BEBEBE00DFDEDE00C9C8C8009C9C9C00C7C7C70000000000000000000000
      0000000000000000000000000000000000000000000000000000CD899C00F2CE
      AA00FEF3C100FDE4B100FFF8C600FEFED700FEFEE400FFFFF300FEFEFC00FCFB
      F700FFFFE700CFB09E007C596700000000003CB8D80074C4E40084E8FC0080B8
      C000E0D4D000F0ECEC00E0B0B000CC909000B4BCBC00A0A8A800A8A4A400B8B8
      B800D0D0D000D4D4D400888888004040440040404400A09CA000D4D4D4000C0C
      0C00000000000000000000000000000000000000000000000000000000000000
      0000000000000A0A0B0037383A004B4A4C006D5D60009C7B7E00BF8F9100B785
      83008F6867000000000000000000000000000000000000000000000000000000
      0000B5B5B600F7DBD600F2C7BF00E5C9C300E0D0CC00D8D2D200D0CED000D6D6
      D700B8B9B900898E8C00B0B5B500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D4A39900FDEC
      BD00FEE6B300FFE2AF00FEF9C500FFFFD600FFFDE200FEFEEE00FFFFF200FDFC
      E800FEFEDD00EFDDBB0083626C000000000050C8EC0070C4E8009CF0FC0080E8
      F8007CD0D80078C0C80080BCC4007CD0D80074D4DC00A8B0B400D4D0D000D0D0
      D000C8C8C800D8D8D800707478005C4834008054300060646800BCBCBC001414
      1400000000000000000000000000000000000000000000000000090707002D2C
      2C00494848006666660085848400A38F8F00B5999800A58A8900756261000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D4C5C600E2B39600FECFA900FECDAB00FFD1AF00FECCAC00FCD3B500E8C9
      B8009C9899000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEBBA300FFF6
      C700FEDAA600FEDCA900FFF2BE00FFFECF00FFFEDB00FFFDE300FEFEE400FEFD
      E000FFFFD700FEF3C20086696A00000000006CD4F40074C8F400A4F0FC008CF8
      FC0090FCFC0090FCFC0090FCFC0090FCFC008CF0F400B8C0C000D4D0D000C8C8
      C800CCCCCC00D8D8D8009C9C9C004C4840005848340078787C00BCBCBC000C0C
      0C000000000000000000000000002A2A2A004C4B4B00797E7E00A2A9A900C0C2
      C200DEDEDE00F0F0F000DADADA008D8989004543430000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D4C0BE00EECDB000FDE3C100FBDDBB00FDDDBB00FBDBB900FEE6C000C09A
      8500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D7AF9E00FEF4
      C400FEE3B300FFE1B400FEEAB700FFF9C700FFFECE00FFFFD500FEFFD700FEFE
      D200FEFECF00F3D9B3008A676C00000000008CE0F00074D0FC009CE0F800B0F8
      FC00A8F4FC00ACF4FC00A8F4FC00A8F4FC00A0E8EC00B4BCBC00D0D0D000D4D4
      D400B8B8B800B4B4B400D0D0D000A4A4A40080848800C8C8C800B0B0B0000C0C
      0C00000000000000000086868600D4D4D400F2F4F400EDD7D700EBC4C400F0E6
      E600CCCDCD00A6A6A60081818100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CFB3AD00F8DBC400FEE7CC00FDE4CA00FEE4CA00FCE3C900FEECD100AE90
      8700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6AD9600F8E9
      BD00FEF9DA00FEE6C100FFDDAF00FEE9B500FEF4C000FEF7C300FFF7C400FDF2
      BF00FFFFCC00D4AE970090586D000000000098E4F00080E4FC0070D8FC0084DC
      FC0084DCFC0084DCFC0090E4FC0090E8FC0084D8EC00B8C0C000ECE4E000C8C4
      C00098989800A4A4A400A4A4A400ACACAC00B4B4B4007C7C7C00848484001414
      1400000000000000000000000000B5B6B600AFAEAE00AC6868009D4C4C008163
      6300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CBAEA400FEF1DD00FCE9D600FDEAD700FEEBD800FEECD900FBE6D3009C89
      8600000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E2BF
      A800FFFFF400FDFDFB00FEECD000FEDCAA00FFE3B200FEDDAA00FDE1AF00FEF4
      C200FDE3B400AC7A78000000000000000000A8E4F000A4F4FC0088F0FC0088F0
      FC0088F0FC00A4F0FC0084D0E4008CD8EC008CD8EC008CC0D0007CA4B4007894
      9C00ACACAC00C8C8C800D8D8D8009C9C9C00808080006C6C6C006C6C6C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CEBB
      BB00E4CEC400FEF9EC00FDF1E500FEF3E600FCF0E300FEFBEE00E1CEC4009C92
      9100000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C68B
      9100E9D7CB00FFFFFF00FFFDE800FFEFC700FEDEAD00FFE6B300FFF7C400FEE8
      B400CD9C84009B6B7100000000000000000068BCD80094DCEC00A4ECF400A0EC
      F400A8ECF4007CC4D80000000000000000000000000000000000000000000000
      00000000000000000000000000007C7C7C006C6C6C0084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C8B0
      AE00FEF9F500FDF8F300FEF9F400FDF8F300FCF8F400FCFBF600BCACA900F5F5
      F500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C2879500DBBAAE00F1E4C400FDF3C600FEF1C200FDEDC000EDCBA800C993
      8B00AE787E00000000000000000000000000000000001898C80084C4E00084C4
      E00068BCD8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DBC9
      C800EEEAEB00EAE2E200ECE4E400EFE7E700EDE6E700E4D6D600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C2759400C8968600DBB49100E7BD9800ECB89E00C88494000000
      00000000000000000000000000000000000000000000FFFFFF00DCE0EC00CED4
      E600D0D4E800D0D4E800D0D4E800D0D4E800D0D4E800D0D4E800D0D4E800D0D4
      E800D0D4E800D0D4E800D0D4E800D0D4E800D0D4E800D8DCEC00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF008494D4000420A400001C
      9C000020A4000420A4000420A4000020A4000222A6000222A6000020A8000020
      A8000020AC00001CA8000018AC000018AC000014A80000189C007888C400FFFF
      FF00000000000000000000000000A3877F009E837D009C817B009C817B009B80
      7B009A7F7B009A7E7B009A7F7C009A7F7C009A7F7C009A7F7C00997F7C009E81
      7C008F7772000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007C90E0000C30C4000C34CC002044
      D0002448D4002C4CD4002C4CD400284CD4002448D4001C44D4001844D8001844
      D8001844D8001444D8000C3CD8000838D8000434D8000024C40000189C00DCE0
      EC00000000000000000000000000F0CEC000FFDAC400FFD6C000FFD6C000FFD5
      BF00FFD4BE00FFD4BD00FFD3BC00FFD2BB00FFD2BA00FFD1B900FED0B900FFD6
      B500BE8E81000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AD737300AD524A00B55A5A00AD524A00AD9494000000
      0000CECECE00D6D6D600CECECE00CEC6C600C6C6C6009C4242009C424200A54A
      4A00B5636300BDA5A50000000000000000005470E4000C34D8002044DC00284C
      E0003054E0003658E0003054E000284CE0003C60E0004868E4004468E400305C
      E8001044E4001044E4001048E4000840E4000038E0000434D8000014A800D0D4
      E800000000000000000000000000DAD4CF00FCE4CE00F9DEC700F9DEC500F8DC
      C200F8DABF00F8D9BC00F8D7B900F8D5B700F8D4B400F8D3B200F6D1AD00FDD7
      BC00B68C8400000000000000000000000000000000000000000058ADCB003689
      A6003C7589005D76800080868A00A8A8A800CFCFCF00E4E4E400F4F3F300FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BD736B00C6636300CE636300B55A5A00A5848400BD94
      9400D6BDBD00EFEFEF00F7F7F700E7E7E700DEDEDE009C3131009C313100B54A
      4A00BD636300AD84840000000000000000005C78E800143CDC00284CE0003454
      E0003C5CE2003658E2005472E600B4C4F400ECF0FC00FFFFFF00FFFFFF00ECF0
      FC00ACC0F4003C68EC000440E8000C48E800043CE4000438D8000018AC00D0D4
      E800000000000000000000000000E0D6CE00FDE5CF00FADFC900FADFC700FADD
      C400F9DBC200F9DABF00F9D8BE00F9D7BC00F9D6B900F9D5B700F7D3B300FED7
      BC00B78C830000000000000000000000000000000000000000008BD4EE00A0EB
      FF0050CBF90046BEE90036A2C9003188A70030768E004E7381007D808300A1A1
      A100000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BD736B00CE636300CE636300B55A5A00B58C8C00BD5A
      5A00C6737300D6C6C600F7F7F700F7F7F700E7E7E7009C3131009C313100B54A
      4A00BD636300AD84840000000000000000005C78E8001840DC003054E0004060
      E4003C5CE2007088EA00F4F6FC00E8ECFC008C9EEE005074E8004C74E800809C
      F000E0E8F800F4F8FC004C78EC000440E8000C44E4000838D800001CAC00D0D4
      E800000000000000000000000000E2D9D200FDE7D300FAE2CD00FBE1CB00FADF
      C900FADEC600F9DCC300F9DBC100FAD9BF00FAD9BC00F9D7BA00F7D5B500FFD9
      BE00B88C830000000000000000000000000000000000000000006DC4E700AEE7
      F80077DDFE007FE0FE007DE3FF0072E1FF0067D9FF0055C6EA0037ACD2002C92
      B200397D94004B73820076818600000000000000000000000000000000000000
      00000000000000000000BD736B00CE636300CE636300B55A5A00C69C9C00AD4A
      4A00AD635A00B5A5A500D6D6D600EFEFEF00F7F7F7009C3131009C313100B54A
      4A00BD636300AD8484000000000000000000607CE8002044DC003C5CE2004060
      E400607CE800F4F8FC00C6CEF6004464E400345CE4004468E4003C64E4002054
      E8002054E800B4C8F400F4F4FC003464E8000840E4001040D800001CAC00D0D4
      E800000000000000000000000000E4DAD400FDE9D700FBE4D100FBE2CE00FBE1
      CC00FBDFC900FADEC600FADCC400FADBC300FADABF00FAD8BD00F8D7BA00FFDA
      C000B88B82000000000000000000000000000000000095D1E70050BBE800B1E4
      F30087E8FD007BE2FC007DE3FC0080E4FD0082E5FD0083E8FE0086ECFF0083EC
      FF006FE1FF005AD5F6002582A00096969600F9F9F90000000000000000000000
      00000000000000000000BD736B00CE636300CE636300B55A5A00CEADA5009C39
      3900AD635A00A59C9400B5B5B500D6D6D600F7F7F7009C3131009C313100B54A
      4A00BD636300AD8484000000000000000000627CE800284CE0004262E4004060
      E400C0CCF400E8ECFC004C6CE4004C6CE400345CE400C4D0F400C0CCF4002050
      E4002858E8001C50E400DCE4FC00ACC0F4000840E4001844D8000020AC00D0D4
      E800000000000000000000000000E6DDD600FEEADA00FAE5D300FBE4D200FAE3
      CF00FAE1CC00FBE0CA00FBDFC800FADEC500FADCC200FADAC100F8D9BD00FFDB
      C200B88C8300000000000000000000000000000000009FD6EB004CBCEB0090D3
      EB00A8F3FE0080EBFD0086ECFE0087EDFE0086ECFD0086ECFD0085EBFC0084EA
      FC008CEEFC008BECFE0060CDEA00526F7700DDDDDD0000000000000000000000
      00000000000000000000BD736B00CE636300CE636300B55A5A00C69C9C00D6BD
      B500D6BDB500CEB5B500BDA5A500C6ADAD00D6BDB500A53939009C393900B552
      5200BD636300AD84840000000000000000006480E8002C50E0004464E4005C76
      E600F0F0FC0094A8F0004464E4005470E6003058E000C6CEF600C0CCF4001C4C
      E000305CE8001048E4007494F000ECF0FC002854E4001844D8000024AC00D0D4
      E800000000000000000000000000E8DFD700FEEDDE00FBE8D700FCE7D600FAE5
      D300FBE4D000FAE2CE00FBE1CB00FAE0C800F9DDC600FADCC400F8DCC100FFDC
      C400B88C8300000000000000000000000000000000009FD6EA0064C7F40060BF
      E600C1F9FC0087F4FE0092F5FE0090F5FE0090F5FF008FF5FE0090F5FF008DF3
      FD0090F6FC0080E6FC0095F3FD00438B9B00A8A8A80000000000000000000000
      00000000000000000000BD736B00CE636300CE636300CE636300C6736B00BD73
      6B00BD736B00C66B6B00C6636300BD636300C6736B00C65A5A00BD5A5A00C663
      6300BD636300AD84840000000000000000006880E8003658E2004C68E4006A82
      EA00FEFEFE006A82EA004C68E400506CE4003054E000C6CEF600C0CCF4001C48
      E0002C58E4001848E4003864E800FFFFFF003C64E4001844D8000424AC00D0D4
      E800000000000000000000000000EAE0DA00FDEEE100FCEADA00FCE9D900FBE6
      D600FBE6D300FBE4D100FBE3CF00FBE1CC00FAE0C900FBDEC700F9DEC500FFDD
      C600B78B82000000000000000000000000000000000084CEE7006BCBF70055BD
      EE00BFEFF50099FDFF008EFBFD0093FDFE0095FCFE0097FDFE0097FDFE0095FC
      FE009AFEFE0081EBFC00B2F8FE007ACEDB00656F750000000000000000000000
      00000000000000000000BD736B00B5525200BD848400C68C8C00C68C8C00CE8C
      8C00CE8C8C00CE8C8C00CE8C8C00CE8C8C00CE8C8C00CE8C8C00BD847B00C66B
      6B00BD636300AD84840000000000000000006A82EA003C5CE200506CE4006E86
      EA00FEFEFE006C84E8004C68E400506CE4003054E000C6CEF600C0CCF4001C48
      E0002C58E4001C48E0003860E400FFFFFF004064E4001C44D4000828A800D0D4
      E800000000000000000000000000ECE4DD00FEF0E500FBEBDE00FBEBDB00FCE9
      DA00FBE7D700FBE6D500FBE5D200FBE3CF00FBE2CD00FAE1CA00F8DFC800FFDE
      C800B78C8100000000000000000000000000000000009DD8EE007DD3FC0058C5
      F9008DD2EA00DCFFFE00C0FFFE00BAFFFF00B2FFFF00A5FFFF00A7FFFE00A7FE
      FF00ABFEFE008FEEFD00B3F0FD00C9FDFF003F7B8E0000000000000000000000
      00000000000000000000BD736B00AD4A4A00EFEFEF00EFEFEF00EFEFEF00F7F7
      F700EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00E7D6D600BD73
      6B00BD636300AD84840000000000000000006E86EA004060E4005C76E6006880
      E800F0F4FC0098A8F0004462E400506CE4003054E000C6CEF600C0CCF4001844
      E0003058E0001844E0007088EA00F0F4FC00385CE0002448D4000A2AA800D0D4
      E800000000000000000000000000EFE6DF00FEF2E800FCEDE100FCECDF00FBEC
      DC00FCEADA00FBE9D800FBE7D600FBE5D300FBE3D000FBE2CE00F9E1CC00FFDF
      C900B78B8200000000000000000000000000000000009AD5EB0085DAFB0071D8
      FD005BCAF00072CDEB0089D3EC009BDBEE00C0EAF500D8FDFD00C1FFFF00BDFE
      FF00C4FEFE00A0EFFE00B7EDFB00F0FDFD0071B8CB00767A7F00000000000000
      00000000000000000000BD736B00AD4A4A00F7F7F700EFEFEF00EFEFEF00EFEF
      EF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00E7D6D600BD73
      6B00BD636300AD84840000000000000000007088E8004864E4006880E8005C78
      E600C8D0F400E4E8F8004E6AE4004E6AE4003454E000C6CEF600C0CCF4002048
      E0002C50E0002048E000E0E4F800B4C0F4002048E0003050D4000C2CA800D0D4
      E800000000000000000000000000F0E8E100FEF4EC00FCF0E500FCEEE300FCED
      E100FDECDE00FBEADB00FBE9D900FBE7D600FBE6D400FCE4D100FAE3CF00FFE0
      CB00B78A8100000000000000000000000000000000008ACEE7008CE5FD0081E7
      FE0083E8FF0079E5FE0079E4FD006FDDF8005ED1F00094D8EC00E6F7FB00DCF9
      FC00E7FCFD00C1F2FE00D7F6FE00FEFEFE00D6F7FB0052778300000000000000
      00000000000000000000BD736B00AD4A4A00EFEFEF00E7E7E700CECECE00CECE
      CE00CECECE00CECECE00CECECE00CECECE00CECECE00DEDEDE00E7D6D600BD73
      6B00BD636300AD84840000000000000000007088EA004C68E4007086EA006882
      E8007C90EC00F4F8FC00C8D0F4004C68E4003C5CE0004864E4004060E400284C
      E000284CE000ACB8F000F4F6FC004868E400284CE0003050D4000C2CA800D0D4
      E800000000000000000000000000F3EBE400FEF7EF00FCF2E800FDF1E600FCEE
      E400FCEDE100FCECDF00FCEBDC00FBE9DA00FBE8D700FBE5D500F9E4D200FFE0
      CC00B78A810000000000000000000000000000000000A8DAEC009EF4FD008DF5
      FD008FF4FC0091F6FE0092F6FE008EF6FD0093F9FE0076E7F6006CD4EA0086D9
      EC0097DEEE00A3DFF000B1DEED00C2E4EF00DBF2F90063A9BF00000000000000
      00000000000000000000BD736B00AD4A4A00EFEFEF00EFEFEF00E7E7E700E7E7
      E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700EFEFEF00E7D6D600BD73
      6B00BD636300AD84840000000000000000007088EA00506CE400788EEA00788E
      EA00647EE800889CEC00F4F8FC00E4E8F80092A4EE005C76E6005470E4007C94
      EC00E0E4F800F4F8FC00607CE8002448DC003254E0003050D4000C2CA800D0D4
      E800000000000000000000000000F4ECE500FDF8F200FDF3EB00FDF2EA00FCF1
      E700FCEFE400FCEEE200FCECDF00FCECDD00FBE9DB00FCE9D800FAEDDB00FEE8
      D200B78A810000000000000000000000000000000000ADDCEE00A6F9FC0095FD
      FD0098FDFD0098FEFE0095FDFD00A8FFFF00A6FFFF00A1FEFE0096FEFE0090FE
      FE0092FAFC0058DCF000447C9200000000000000000000000000000000000000
      00000000000000000000BD736B00AD4A4A00EFEFEF00E7E7E700D6D6D600D6D6
      D600D6D6D600D6D6D600DEDEDE00DEDEDE00DEDEDE00E7E7E700E7D6D600BD73
      6B00BD636300AD8484000000000000000000768CEA005A76E600889CEC00889C
      EC007890EA00627CE8007C90EC00C6CEF600F0F4FC00FFFFFF00FFFFFF00F4F6
      FC00C4CCF4005C78E8003054E0003C5CE2003256E0003050D4000828A800D0D4
      E800000000000000000000000000F6EEE700FEF9F600FCF5EF00FDF4ED00FCF3
      EA00FDF1E800FCF0E500FCEEE300FCECE000FCEEDF00FBE2D700F5C9C600FEC2
      BC00BA877E000000000000000000000000000000000099D2E800BCF5FB00A2FE
      FE009FFEFE0094FEFE00B9FEFE00A0D6E6008CDBEB009AE5F300AAF0F800B3F8
      FC00BAFDFE0081EAF700678D9900000000000000000000000000000000000000
      00000000000000000000BD736B00AD4A4A00EFEFEF00DEDEDE00CECECE00CECE
      CE00CECECE00CECECE00CECECE00CECECE00CECECE00DEDEDE00E7D6D600BD73
      6B00BD636300AD8484000000000000000000748AEA006680E8009AAAF00098A8
      F0008498EC00788EEA006884E8005E7AE6006E86EA00768CEA00768CEA006880
      E8004E6AE4004C6AE4004E6CE4004464E400365AE0002E4ED6000222A600D0D4
      E800000000000000000000000000F9F1EA00FEFBF900FDF8F200FDF6F000FDF5
      EE00FDF3EB00FCF2E900FCF0E700FCEEE300FAEEE500EDBB8F00ED963A00EE8E
      3F00A07C680000000000000000000000000000000000000000008DD0E700B3EA
      F400A4ECF500BCF7FB009ED6E50099B5C300000000000000000000000000A3D5
      E7009ED6E90087D0E60000000000000000000000000000000000000000000000
      00000000000000000000BD736B00AD4A4A00EFEFEF00EFEFEF00EFEFEF00EFEF
      EF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00E7D6D600BD73
      6B00BD636300AD84840000000000000000008C9EEE00627CE80092A4EE0098AA
      F0008498EC00768CEA007088EA006C84E800647CE8005C78E8005C78E6005A76
      E6005C76E6005472E6004C6CE4004464E4003658E0002042D2000A2AA800DCE0
      EC00000000000000000000000000FBF3EC00FEFEFD00FEFAF600FEF9F400FFF8
      F200FEF6EF00FFF5EC00FEF3EA00FEF2E700FCEEE600F4CD9F00FFBB4E009F78
      4700FFFFFF000000000000000000000000000000000000000000000000000000
      0000BFE3F000BEE1EE00B4DAE700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B56B6300A5424200DEDEDE00DEDEDE00DEDED600DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00D6BDBD00B563
      6300B5636300BDA5A5000000000000000000FEFEFE009CACF000607CE800607C
      E8005470E6004C6AE4004A66E4004464E4004464E4004462E4004060E4003C5C
      E2003658E2003256E0003254E0002A4EE0002044DC001A3ECC008092D400FFFF
      FF00000000000000000000000000FDF6EE00FDFEFF00FBF9F800FBF8F600F9F6
      F400F7F3F100F6F2ED00F4F0EB00F3EDE800F0EBE600F3D5AE00AC9175000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF008C9EEE007086
      EA007088EA006E86EA006E86EA006E86EA007088E8007086EA006A82EA006A82
      EA006880E8006680E8006880E800647EE8005C78E6007C90E000FFFFFF000000
      0000000000000000000000000000FDECDD00FAEFE600F5EAE000F0E7DE00EDE3
      DB00E9DFD800E5DBD400E2D8D100DCD4CE00DBD4CE00D6BBA200757271000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000500000008C0000000100010000000000900600000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FFE07FFFFFFFFFFFFFFF0000FF803EFFFFE001F4000F0000FF001CFFFFC000F0
      00070000FF000C0001C0007800030000C0000C3937C000780001000000007C78
      47C00078000000000000FC79E7C0003C800000000000FC79E7C0003C00000000
      0001FC39F7C0003C000000008001FC3BFBC001FE000000008001FCB7FDC000FE
      000000008001FEA7FFE0007F00000000C001FFC7FFF0003F80000000E003FFFF
      FFFF003F80000000E003FFFFFFFF007FC0000000E003FFFFFFFE00FFE0000000
      F003FFFFFFFC01FFF0000000F803FFFFFFF803FFF8000000F807FFFFFFF803FF
      FC000000FC0FFFFFFFFFFFFFFE00000083FFF83BC1FFFFF83FFF000081BC781F
      81FFFFF81FF700008118381B81F0FFF81FFF00008118181B81E07FF81FFF0000
      81B8181F81C03FF81FFF000081B0181B81C007F81FE3000081B0081B01C003F8
      1FE3000080B0080F01C001F80FE3000000B0000000E000F00FE1000000A0000F
      00F000700FC1000000B0000B00E800300FC1000000B0000B00C400100FC10000
      00B8000F00C600100FC3000000B8000B00EF00300FE3000001B8101B80EF8010
      1FE7000081B8181F81F7C0381FFF00008118181B81FBE0B81FFF00008318183B
      C1FC70783FFF0000C7B81C7FE3FF8FFC7FFF0000FFF83FFBFFFFFFFFFFFF0000
      FFFFFFFFFFF83FFFFFFF0000FE007EFC01E007FF801F0000FE007EFC01E001FF
      801F0000FE007E0001E0007F801F0000FE007E0001E0007F801F0000FE007E00
      01E0107F801F0000C0007E0001E0187F801F0000C0007E0001E0187F801F0000
      C0007E0001E0187F801F0000C0007E0001E0087F801F0000C0007E0003E0007F
      801F0000C000FE0007E0007F801F0000C001FE007FE0307F801F0000C00FFE00
      7FE0007F801F0000C00FFE007FE0007F801F0000C00FFE007FE0007F801F0000
      C01FFE00FFE0007F801F0000C03FFE03FFE0007F801F0000FFFFFF87FFF0007F
      801F0000FFFFFFFFFFFF83FFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFE00
      03FFF3FFFFFF0000FFFFF80003FFE1FFFFFF0000C000380003FFC3F999990000
      C000380003FFC3FBFFFD0000C0003C0003FB87FEDB770000C0003E0003F887FF
      7EEF0000C0003E0003F80FFBB4DD0000C0003E0003F80FFBDB3D0000C0003E00
      03F803FFE7FF0000C0003E0003F801FFFFFF0000C0303E0003F803FBFF3D0000
      C0103E0003F807FBFEDD0000C0003E0003F80FFFFEDF0000C0003E0003F81FFF
      FF3F0000C03FFE0003F83FFBFFFD0000E07FFE0003F87FF999990000FFFFFE00
      07F8FFFFFFFF0000FFFFFE0007F9FFFFFFFF0000FFFFFE001FFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000FF0FFFFFFFFC01F0FFFF0000FC03FFFFFFF80070
      7FFF0000E000FFF3FFC000301FFF0000C0003FC0FFC000301FFF0000C0001F80
      3FC000380FFF0000C00018000FC0003C0FFF0000F00000000380003E00070000
      000010000180003F00030000000070000180003FC003000000007E0001C0007F
      E001000000001FE001E0007FC001000000000FF807F001FFC001000000000FC0
      1FF007FFC001000000000E007FF00FFFC001000000000C01FFF00FFFC0010000
      00000E0FFFF00FFFE003000000001FFFFFE00FFFE003000003FE3FFFFFE00FFF
      F007000087FFFFFFFFE03FFFF81F000080001FFFFFFFFFFFFFFF000000000E00
      07FFFFFFFFFF000000000E0007FFFFFC1003000000000E0007C00FFC00030000
      00000E0007C00FFC0003000000000E0007C001FC0003000000000E000780007C
      0003000000000E000780007C0003000000000E000780007C0003000000000E00
      0780007C0003000000000E000780007C0003000000000E000780003C00030000
      00000E000780003C0003000000000E000780003C0003000000000E00078001FC
      0003000000000E00078001FC0003000000000E0007C0E3FC0003000000000E00
      07F1FFFC0003000000000E001FFFFFFFFFFF000080001E001FFFFFFFFFFF0000
      00000000000000000000000000000000000000000000}
  end
  object mcmSTI: TmcmSTI
    Left = 40
    Top = 72
  end
  object mcmAppendDialog: TmcmSaveDialog
    DefaultExt = 'tif'
    HelpContext = 674
    Options = [ofHideReadOnly, ofShowHelp, ofFileMustExist, ofNoReadOnlyReturn]
    Title = 'Append image'
    OptionHelpContext = 0
    Left = 104
    Top = 72
  end
  object mcmDropTarget: TmcmDropTarget
    Active = True
    OnDragDrop = FormDragDrop
    OnDragOver = FormDragOver
    Left = 136
    Top = 40
  end
end
