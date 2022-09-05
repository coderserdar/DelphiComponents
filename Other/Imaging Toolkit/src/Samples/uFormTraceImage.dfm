object Form1: TForm1
  Left = 298
  Top = 140
  Width = 772
  Height = 566
  Caption = 'Trace Image'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 523
    Top = 0
    Width = 4
    Height = 488
    Align = alRight
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 488
    Width = 756
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 527
    Top = 0
    Width = 229
    Height = 488
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel1Resize
    object lMethod: TLabel
      Left = 8
      Top = 8
      Width = 39
      Height = 13
      Caption = 'Method:'
    end
    object lTolerance: TLabel
      Left = 8
      Top = 58
      Width = 60
      Height = 13
      Caption = 'Tolerance:   '
    end
    object lMinLength: TLabel
      Left = 8
      Top = 104
      Width = 59
      Height = 13
      Caption = 'Min. Length:'
    end
    object Label1: TLabel
      Left = 120
      Top = 8
      Width = 30
      Height = 13
      Caption = 'Scale:'
    end
    object cbMethod: TComboBox
      Left = 6
      Top = 27
      Width = 80
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'None'
        'Color RGB'
        'Luminance'
        'Hue')
    end
    object isTolerance: TmcmIntSpin
      Left = 8
      Top = 76
      Width = 81
      Height = 22
      TabOrder = 1
      Value = 50
      MaxValue = 0
      MinValue = 0
    end
    object cbTraceAll: TCheckBox
      Left = 8
      Top = 200
      Width = 73
      Height = 13
      Caption = 'Trace All'
      TabOrder = 2
      OnClick = cbTraceAllClick
    end
    object isMinLength: TmcmIntSpin
      Left = 8
      Top = 120
      Width = 81
      Height = 22
      TabOrder = 3
      Value = 2
      MaxValue = 100000
      MinValue = 0
    end
    object sgData: TStringGrid
      Left = 0
      Top = 232
      Width = 229
      Height = 256
      Align = alBottom
      BorderStyle = bsNone
      ColCount = 2
      DefaultRowHeight = 20
      RowCount = 30
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
      ParentFont = False
      TabOrder = 4
      OnSelectCell = sgDataSelectCell
      RowHeights = (
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20)
    end
    object cbTraceOnEdge: TCheckBox
      Left = 8
      Top = 152
      Width = 97
      Height = 17
      Caption = 'Trace on Edge'
      TabOrder = 5
    end
    object cbClosedOnly: TCheckBox
      Left = 8
      Top = 176
      Width = 97
      Height = 17
      Caption = 'Closed only'
      TabOrder = 6
    end
    object rsScale: TmcmRealSpin
      Left = 120
      Top = 27
      Width = 73
      Height = 22
      TabOrder = 7
      OnChange = rsScaleChange
      Value = 1.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 523
    Height = 488
    ActivePage = tsInput
    Align = alClient
    TabOrder = 2
    object tsInput: TTabSheet
      Caption = 'Input'
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 515
        Height = 460
        Align = alClient
        TabOrder = 0
        object mcmImageCtrl: TmcmImageCtrl
          Left = 0
          Top = 0
          Width = 511
          Height = 456
          Align = alClient
          AutoSize = True
          ParentColor = False
          Scale = 1.000000000000000000
          ScaleToFit = False
          OnChange = mcmImageCtrlChange
          OnMouseMove = mcmImageCtrlMouseMove
          OnMouseUp = mcmImageCtrlMouseUp
        end
      end
    end
    object tsThreshold: TTabSheet
      Caption = 'Threshold'
      object mcmImageCtrlThreshold: TmcmImageCtrl
        Left = 0
        Top = 0
        Width = 515
        Height = 460
        Align = alClient
        ParentColor = False
        Scale = 1.000000000000000000
        ScaleToFit = False
      end
    end
    object tsOutline: TTabSheet
      Caption = 'Outline'
      object mcmImageCtrlOutline: TmcmImageCtrl
        Left = 0
        Top = 0
        Width = 515
        Height = 460
        Align = alClient
        ParentColor = False
        Scale = 1.000000000000000000
        ScaleToFit = False
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 32
    object FileMenu: TMenuItem
      Caption = '&File'
      object OpenItem: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = OpenItemClick
      end
      object SaveItem: TMenuItem
        Caption = 'Save As...'
        OnClick = SaveItemClick
      end
      object N1: TMenuItem
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
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = CopyItemClick
      end
      object PasteItem: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
        OnClick = PasteItemClick
      end
    end
    object ClearItem: TMenuItem
      Caption = 'Clear'
      OnClick = ClearItemClick
    end
  end
  object OpenDialog: TmcmOpenDialog
    HelpContext = 0
    Options = [ofHideReadOnly, ofExtensionDifferent, ofFileMustExist, ofNoNetworkButton]
    Title = 'Open Image'
    ViewStyle = vsPreview
    Left = 72
    Top = 32
  end
  object SaveDialog: TmcmSaveDialog
    HelpContext = 0
    Title = 'Save'
    OptionHelpContext = 0
    Left = 40
    Top = 32
  end
end
