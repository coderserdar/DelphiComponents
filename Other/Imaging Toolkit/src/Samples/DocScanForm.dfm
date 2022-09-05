object FormDocScan: TFormDocScan
  Left = 234
  Top = 104
  Width = 482
  Height = 750
  Caption = 'DocScan'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
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
  KeyPreview = True
  OldCreateOrder = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object ImageCtrl: TmcmImageCtrl
    Left = 0
    Top = 139
    Width = 466
    Height = 553
    Align = alClient
    BorderStyle = BS_SINGLE
    Center = True
    Color = 15000804
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = True
  end
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 466
    Height = 139
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object PanelButton: TPanel
      Left = 0
      Top = 0
      Width = 73
      Height = 139
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object sbScan: TSpeedButton
        Left = 8
        Top = 8
        Width = 57
        Height = 57
        Caption = '&Scan'
        Flat = True
        Glyph.Data = {
          C6050000424DC605000000000000360400002800000014000000140000000100
          0800000000009001000000000000000000000001000000010000000000000000
          80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
          A6000020400000206000002080000020A0000020C0000020E000004000000040
          20000040400000406000004080000040A0000040C0000040E000006000000060
          20000060400000606000006080000060A0000060C0000060E000008000000080
          20000080400000806000008080000080A0000080C0000080E00000A0000000A0
          200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
          200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
          200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
          20004000400040006000400080004000A0004000C0004000E000402000004020
          20004020400040206000402080004020A0004020C0004020E000404000004040
          20004040400040406000404080004040A0004040C0004040E000406000004060
          20004060400040606000406080004060A0004060C0004060E000408000004080
          20004080400040806000408080004080A0004080C0004080E00040A0000040A0
          200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
          200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
          200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
          20008000400080006000800080008000A0008000C0008000E000802000008020
          20008020400080206000802080008020A0008020C0008020E000804000008040
          20008040400080406000804080008040A0008040C0008040E000806000008060
          20008060400080606000806080008060A0008060C0008060E000808000008080
          20008080400080806000808080008080A0008080C0008080E00080A0000080A0
          200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
          200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
          200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
          2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
          2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
          2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
          2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
          2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
          2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
          2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00070707070707
          0707070707070707070707070707070707070707070707070707070707070707
          070707000000000000000000000000A4070707070707F7FF0808070707070707
          070700A4A40707070707F7FFFA02070707070707070700A4A40707070707F7FF
          FFFFFFFFFFFFFFFFFFFF00A4A4A40707070707A40707F3090909F309F3F3A4A4
          A4A400070707070709A4FF090909F3E8E8F3E800A4A4A4A40707070707A4A409
          09F3F3F3F3EBF200A4A4A49BA40707070707A4A409F3F3EBEBE8F2F200A49B5B
          0007070707070707A4F3EBEBEBEBE8E8E800A45B0007070707070707A4F7A4A4
          A4A4A4A4A4A400000707070707070707F70707F7F7F7F7F7F7A4000707070707
          0707A4F70707070707F7F7A4A4A407070707070707A4F707070707070707F7F7
          000707070707070707F70807080707070707F70007070707070707A4F7080808
          070707070707A40707070707070707A4A4A4A4A4A4A4A4A4A400070707070707
          0707070707070707070707070707070707070707070707070707070707070707
          07070707070707070707}
        Layout = blGlyphTop
        OnClick = sbScanClick
      end
      object sbSelect: TSpeedButton
        Left = 8
        Top = 72
        Width = 57
        Height = 25
        Caption = '&Select'
        Flat = True
        Layout = blGlyphTop
        OnClick = sbSelectClick
      end
      object sbClose: TSpeedButton
        Left = 8
        Top = 104
        Width = 57
        Height = 25
        Caption = '&Close'
        Flat = True
        OnClick = sbCloseClick
      end
    end
    object PageControl: TPageControl
      Left = 73
      Top = 0
      Width = 393
      Height = 139
      ActivePage = tsMain
      Align = alClient
      HotTrack = True
      TabHeight = 14
      TabOrder = 1
      object tsMain: TTabSheet
        Caption = 'Main'
        object gbMain: TGroupBox
          Left = 0
          Top = 0
          Width = 385
          Height = 49
          Align = alTop
          Color = clBtnFace
          Ctl3D = True
          ParentColor = False
          ParentCtl3D = False
          TabOrder = 0
          object lPagesToScan: TLabel
            Left = 8
            Top = 24
            Width = 72
            Height = 14
            Caption = '&Pages to scan:'
            FocusControl = sePagesToScan
          end
          object lDesc1: TLabel
            Left = 144
            Top = 24
            Width = 189
            Height = 14
            Caption = 'Enter -1 to scan all pages in the feeder.'
          end
          object sePagesToScan: TSpinEdit
            Left = 88
            Top = 16
            Width = 49
            Height = 23
            MaxValue = 10000
            MinValue = -1
            TabOrder = 0
            Value = 1
            OnChange = sePagesToScanChange
          end
        end
        object gbSaveImage: TGroupBox
          Left = 0
          Top = 50
          Width = 385
          Height = 65
          Align = alBottom
          TabOrder = 1
          object lFileName: TLabel
            Left = 8
            Top = 32
            Width = 48
            Height = 14
            Caption = 'File &name:'
            FocusControl = eFileName
          end
          object sbFileName: TSpeedButton
            Left = 252
            Top = 24
            Width = 22
            Height = 22
            Caption = '...'
            Flat = True
            OnClick = sbFileNameClick
          end
          object eFileName: TEdit
            Left = 88
            Top = 24
            Width = 161
            Height = 22
            TabOrder = 0
            Text = 'eFileName'
            OnChange = eFileNameChange
          end
          object cbMultiTIFF: TCheckBox
            Left = 280
            Top = 26
            Width = 105
            Height = 17
            Caption = '&Multi page TIF file'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
          object cbSaveImage: TCheckBox
            Left = 8
            Top = 0
            Width = 89
            Height = 17
            Caption = 'Save Images'
            TabOrder = 2
            OnClick = cbSaveImageClick
          end
        end
      end
      object tsFormat: TTabSheet
        Caption = 'Format'
        object gbFormat: TGroupBox
          Left = 0
          Top = 0
          Width = 393
          Height = 115
          Align = alClient
          TabOrder = 0
          object lResolution: TLabel
            Left = 8
            Top = 56
            Width = 53
            Height = 14
            Caption = 'R&esolution:'
            FocusControl = seResolution
          end
          object lPaperSize: TLabel
            Left = 160
            Top = 24
            Width = 31
            Height = 14
            Caption = '&Paper:'
            FocusControl = cbPaperSize
          end
          object lLeft: TLabel
            Left = 160
            Top = 56
            Width = 22
            Height = 14
            Caption = '&Left:'
            FocusControl = rsLeft
          end
          object lRight: TLabel
            Left = 160
            Top = 88
            Width = 27
            Height = 14
            Caption = '&Right:'
            FocusControl = rsRight
          end
          object lTop: TLabel
            Left = 280
            Top = 56
            Width = 20
            Height = 14
            Caption = '&Top:'
            FocusControl = rsTop
          end
          object lBottom: TLabel
            Left = 280
            Top = 88
            Width = 36
            Height = 14
            Caption = '&Bottom:'
            FocusControl = rsBottom
          end
          object lUnit: TLabel
            Left = 8
            Top = 24
            Width = 21
            Height = 14
            Caption = '&Unit:'
            FocusControl = cbUnits
          end
          object cbPaperSize: TComboBox
            Left = 200
            Top = 16
            Width = 185
            Height = 22
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 1
            OnChange = cbPaperSizeChange
          end
          object cbResolution: TComboBox
            Left = 72
            Top = 48
            Width = 73
            Height = 22
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 2
            Visible = False
            OnChange = cbResolutionChange
          end
          object seResolution: TSpinEdit
            Left = 72
            Top = 48
            Width = 57
            Height = 23
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnChange = seResolutionChange
          end
          object rsLeft: TmcmRealSpin
            Left = 200
            Top = 48
            Width = 65
            Height = 23
            TabOrder = 3
            OnChange = rsLeftChange
            Value = 1.000000000000000000
            MaxValue = 10000.000000000000000000
            Decimals = 4
            Increment = 0.100000000000000000
          end
          object rsRight: TmcmRealSpin
            Left = 200
            Top = 80
            Width = 65
            Height = 23
            TabOrder = 4
            OnChange = rsRightChange
            Value = 1.000000000000000000
            MaxValue = 10000.000000000000000000
            Decimals = 4
            Increment = 0.100000000000000000
          end
          object rsTop: TmcmRealSpin
            Left = 320
            Top = 48
            Width = 65
            Height = 23
            TabOrder = 5
            OnChange = rsTopChange
            Value = 1.000000000000000000
            MaxValue = 10000.000000000000000000
            Decimals = 4
            Increment = 0.100000000000000000
          end
          object rsBottom: TmcmRealSpin
            Left = 320
            Top = 80
            Width = 65
            Height = 23
            TabOrder = 6
            OnChange = rsBottomChange
            Value = 1.000000000000000000
            MaxValue = 10000.000000000000000000
            Decimals = 4
            Increment = 0.100000000000000000
          end
          object cbUnits: TComboBox
            Left = 72
            Top = 16
            Width = 73
            Height = 22
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 7
            OnChange = cbUnitsChange
          end
        end
      end
      object tsColor: TTabSheet
        Caption = 'Color'
        object gbColor: TGroupBox
          Left = 0
          Top = 0
          Width = 393
          Height = 115
          Align = alClient
          TabOrder = 0
          object lPixelType: TLabel
            Left = 8
            Top = 24
            Width = 49
            Height = 14
            Caption = '&Pixel type:'
          end
          object lBitReduce: TLabel
            Left = 8
            Top = 56
            Width = 66
            Height = 14
            Caption = 'Bit &Reduction:'
          end
          object lThreshold: TLabel
            Left = 8
            Top = 88
            Width = 51
            Height = 14
            Caption = '&Threshold:'
            Enabled = False
          end
          object lBrightness: TLabel
            Left = 192
            Top = 56
            Width = 55
            Height = 14
            Caption = '&Brightness:'
          end
          object lContrast: TLabel
            Left = 192
            Top = 88
            Width = 44
            Height = 14
            Caption = 'C&ontrast:'
          end
          object cbColor: TComboBox
            Left = 80
            Top = 16
            Width = 97
            Height = 22
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 0
            OnChange = cbColorChange
          end
          object cbAutoBright: TCheckBox
            Left = 192
            Top = 16
            Width = 97
            Height = 17
            Caption = '&Auto Brightness'
            TabOrder = 1
          end
          object cbBitReduce: TComboBox
            Left = 80
            Top = 48
            Width = 97
            Height = 22
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 2
            OnChange = cbBitReduceChange
          end
          object isThreshold: TmcmIntSpin
            Left = 112
            Top = 80
            Width = 65
            Height = 23
            Enabled = False
            TabOrder = 3
            Value = 128
            MaxValue = 255
            MinValue = 0
          end
          object isBrightness: TmcmIntSpin
            Left = 256
            Top = 48
            Width = 65
            Height = 23
            TabOrder = 4
            Value = 0
            MaxValue = 0
            MinValue = 0
          end
          object isContrast: TmcmIntSpin
            Left = 256
            Top = 80
            Width = 65
            Height = 23
            TabOrder = 5
            Value = 0
            MaxValue = 0
            MinValue = 0
          end
        end
      end
      object tsADF: TTabSheet
        Caption = 'ADF'
        object gbADF: TGroupBox
          Left = 0
          Top = 0
          Width = 393
          Height = 115
          Align = alClient
          TabOrder = 0
          object cbEnableADF: TCheckBox
            Left = 16
            Top = 16
            Width = 97
            Height = 17
            Caption = '&Enable ADF'
            Checked = True
            Enabled = False
            State = cbChecked
            TabOrder = 0
            OnClick = cbEnableADFClick
          end
          object cbEnableDuplex: TCheckBox
            Left = 40
            Top = 40
            Width = 97
            Height = 17
            Caption = 'Enable &Duplex'
            Checked = True
            Enabled = False
            State = cbChecked
            TabOrder = 1
          end
        end
      end
      object tsTransfer: TTabSheet
        Caption = 'Transfer'
        object gbTransfer: TGroupBox
          Left = 0
          Top = 0
          Width = 385
          Height = 115
          Align = alClient
          TabOrder = 0
          object lMechanism: TLabel
            Left = 8
            Top = 24
            Width = 57
            Height = 14
            Caption = '&Mechanism:'
            FocusControl = cbXferMechanism
          end
          object lFileFormat: TLabel
            Left = 8
            Top = 56
            Width = 55
            Height = 14
            Caption = '&File Format:'
            FocusControl = cbFileFormat
          end
          object lCompression: TLabel
            Left = 8
            Top = 88
            Width = 66
            Height = 14
            Caption = 'C&ompression:'
            FocusControl = cbCompression
          end
          object cbXferMechanism: TComboBox
            Left = 80
            Top = 16
            Width = 97
            Height = 22
            Style = csDropDownList
            ItemHeight = 14
            TabOrder = 0
            OnChange = cbXferMechanismChange
            Items.Strings = (
              'Native'
              'File'
              'Memory')
          end
          object cbFileFormat: TComboBox
            Left = 80
            Top = 48
            Width = 97
            Height = 22
            Style = csDropDownList
            Enabled = False
            ItemHeight = 14
            TabOrder = 1
            Items.Strings = (
              'TWFF_TIFF'
              'TWFF_PICT'
              'TWFF_BMP'
              'TWFF_XBM'
              'TWFF_JFIF'
              'TWFF_FPX'
              'TWFF_TIFFMULTI'
              'TWFF_PNG'
              'TWFF_SPIFF'
              'TWFF_EXIF')
          end
          object cbCompression: TComboBox
            Left = 80
            Top = 80
            Width = 97
            Height = 22
            Style = csDropDownList
            Enabled = False
            ItemHeight = 14
            TabOrder = 2
            Items.Strings = (
              'TWCP_NONE'
              'TWCP_PACKBITS'
              'TWCP_GROUP31D'
              'TWCP_GROUP31DEOL'
              'TWCP_GROUP32D'
              'TWCP_GROUP4'
              'TWCP_JPEG'
              'TWCP_LZW'
              'TWCP_JBIG'
              'TWCP_PNG'
              'TWCP_RLE4'
              'TWCP_RLE8'
              'TWCP_BITFIELDS')
          end
        end
      end
    end
  end
  object mcmTWAIN: TmcmTWAIN
    Left = 8
    Top = 144
    Width = 28
    Height = 28
    Country = UNITEDKINGDOM
    DisableAfterAcquire = False
    FileFormat = TWFF_TIFF
    FileFormats = [TWFF_TIFF]
    LogFilename = '.\docscan.LOG'
    LogToFile = True
    Manufacturer = 'MCM DESIGN'
    MessageLevel = ML_INFO
    ProductFamily = 'Image Capture'
    ProductName = 'Docscan'
    ReturnHandle = False
    XferMech = TWFX_MEMORY
    OnCloseSource = mcmTWAINCloseSource
    OnDeviceEvent = mcmTWAINDeviceEvent
    OnDeviceNotReady = mcmTWAINDeviceNotReady
    OnDisableMenus = mcmTWAINDisableMenus
    OnEnableMenus = mcmTWAINEnableMenus
    OnImageReady = mcmTWAINImageReady
    OnMemXferBuffer = mcmTWAINMemXferBuffer
    OnMemXferSize = mcmTWAINMemXferSize
    OnNegotiation = mcmTWAINNegotiation
    OnXferNext = mcmTWAINXferNext
    OnXferReady = mcmTWAINXferReady
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 692
    Width = 466
    Height = 19
    Panels = <
      item
        Text = 'Status'
        Width = 200
      end
      item
        Width = 50
      end>
  end
  object SaveDialog: TSaveDialog
    FileName = '.\docscan.tif'
    Filter = 'TIFF|*.tif'
    Options = [ofOverwritePrompt, ofHideReadOnly]
    Left = 40
    Top = 144
  end
end
