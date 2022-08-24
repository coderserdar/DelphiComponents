object frmMain: TfrmMain
  Left = 314
  Top = 145
  Width = 920
  Height = 599
  Caption = 'TagIndex Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object splLeft: TSplitter
    Left = 269
    Top = 141
    Width = 3
    Height = 431
    Cursor = crHSplit
    AutoSnap = False
    MinSize = 4
  end
  object splRight: TSplitter
    Left = 620
    Top = 141
    Width = 3
    Height = 431
    Cursor = crHSplit
    Align = alRight
    AutoSnap = False
    MinSize = 4
  end
  object pLeft: TPanel
    Left = 0
    Top = 141
    Width = 269
    Height = 431
    Align = alLeft
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clInfoBk
    FullRepaint = False
    TabOrder = 0
    OnResize = pLeftResize
    object tiABC: TTagIndex
      Left = 0
      Top = 0
      Width = 265
      Height = 427
      Align = alClient
      Alignment = taRightJustify
      ColSpacing = 1
      FixedColFullFrame = False
      FixedColWidth = 120
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      HoverColor = clNone
      HoverCursor = crHandPoint
      HoverStyle = [fsUnderline]
      HoverEnlarge = False
      ItemFrame.BackColor = clNone
      ItemFrame.FrameColor = clNone
      ItemFrame.FrameMargin = 0
      ItemFrame.FrameSize = 0
      ItemFrame.FrameStyle = psSolid
      ItemFrame.RoundedSize = 0
      HoverFrame.BackColor = clNone
      HoverFrame.FrameColor = clNone
      HoverFrame.FrameMargin = 0
      HoverFrame.FrameSize = 0
      HoverFrame.FrameStyle = psSolid
      HoverFrame.RoundedSize = 0
      SelectedFrame.BackColor = clNone
      SelectedFrame.FrameColor = clNone
      SelectedFrame.FrameMargin = 0
      SelectedFrame.FrameSize = 0
      SelectedFrame.FrameStyle = psSolid
      SelectedFrame.RoundedSize = 0
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -12
      LabelFont.Name = 'Segoe UI'
      LabelFont.Style = []
      LabelFrame.BackColor = clNone
      LabelFrame.FrameColor = clNone
      LabelFrame.FrameMargin = 0
      LabelFrame.FrameSize = 0
      LabelFrame.FrameStyle = psSolid
      LabelFrame.RoundedSize = 0
      LabelsPosition = tlpTop
      SelectedColor = clNone
      SelLabelColor = clNone
      SelLabelFrame.BackColor = 12181245
      SelLabelFrame.FrameColor = clMaroon
      SelLabelFrame.FrameMargin = 1
      SelLabelFrame.FrameSize = 1
      SelLabelFrame.FrameStyle = psSolid
      SelLabelFrame.RoundedSize = 4
      SelLabelStyle = [fsBold]
      PageIndex = 0
      PageNumberStyle = pnsTabs
      ParentFont = False
      RowSpacing = 1
      Transparent = False
      OnTagClick = tiABCTagClick
    end
  end
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 912
    Height = 141
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 4
      Top = 6
      Width = 507
      Height = 26
      Caption = 
        'This simple application demonstrates some of the features of TTa' +
        'gIndex component. '#13#10'A scrollable index of keywords can be design' +
        'ed by TTagCloud component with Direction set to tcdVertical.'
    end
    object Bevel1: TBevel
      Left = 124
      Top = 43
      Width = 9
      Height = 68
      Shape = bsLeftLine
    end
    object Bevel2: TBevel
      Left = 440
      Top = 43
      Width = 9
      Height = 68
      Shape = bsLeftLine
    end
    object Label2: TLabel
      Left = 4
      Top = 122
      Width = 240
      Height = 13
      Caption = 'Index of keywords (using TTagIndex component) '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      Transparent = True
    end
    object Label3: TLabel
      Left = 669
      Top = 122
      Width = 239
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Index of keywords (using TTagCloud component) '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      Transparent = True
    end
    object sbOpenFile: TButton
      Left = 4
      Top = 45
      Width = 102
      Height = 28
      Hint = 'Open another testing document'
      Caption = 'Open document'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = sbOpenFileClick
    end
    object cbCustomLabels: TCheckBox
      Left = 144
      Top = 49
      Width = 153
      Height = 17
      Caption = 'Custom labels'
      TabOrder = 2
      OnClick = cbCustomLabelsClick
    end
    object cbShowPageNumbs: TCheckBox
      Left = 144
      Top = 70
      Width = 153
      Height = 17
      Caption = 'Show page numbers'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbShowPageNumbsClick
    end
    object cbPageBtns: TCheckBox
      Left = 144
      Top = 90
      Width = 161
      Height = 17
      Caption = 'Page numbers as buttons'
      TabOrder = 4
      OnClick = cbPageBtnsClick
    end
    object sbParse: TButton
      Left = 4
      Top = 79
      Width = 102
      Height = 28
      Hint = 
        'You can edit current document and re-parse the keywords by click' +
        'ing this button'
      Caption = 'Parse keywords'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = ParseDocument
    end
    object rgLabelsPos: TRadioGroup
      Left = 316
      Top = 43
      Width = 105
      Height = 68
      Caption = 'Labels position'
      ItemIndex = 0
      Items.Strings = (
        'Top'
        'Left'
        'Right')
      TabOrder = 5
      OnClick = rgLabelsPosClick
    end
  end
  object reDoc: TRichEdit
    Left = 272
    Top = 141
    Width = 348
    Height = 431
    Align = alClient
    Color = clWhite
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    HideSelection = False
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    WantReturns = False
    WordWrap = False
  end
  object ScrollBox: TScrollBox
    Left = 623
    Top = 141
    Width = 289
    Height = 431
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alRight
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 3
    OnResize = ScrollBoxResize
    object tagCloud: TTagCloud
      Left = 0
      Top = 0
      Width = 229
      Height = 427
      Align = alLeft
      AutoShrinkRows = True
      ColSpacing = 1
      Direction = tcdVertical
      FixedColFullFrame = False
      FixedColWidth = 120
      HoverColor = clNone
      HoverCursor = crHandPoint
      HoverStyle = [fsUnderline]
      HoverEnlarge = False
      ItemFrame.BackColor = clNone
      ItemFrame.FrameColor = clNone
      ItemFrame.FrameMargin = 0
      ItemFrame.FrameSize = 0
      ItemFrame.FrameStyle = psSolid
      ItemFrame.RoundedSize = 0
      HoverFrame.BackColor = clNone
      HoverFrame.FrameColor = clNone
      HoverFrame.FrameMargin = 0
      HoverFrame.FrameSize = 0
      HoverFrame.FrameStyle = psSolid
      HoverFrame.RoundedSize = 0
      SelectedFrame.BackColor = clNone
      SelectedFrame.FrameColor = clNone
      SelectedFrame.FrameMargin = 0
      SelectedFrame.FrameSize = 0
      SelectedFrame.FrameStyle = psSolid
      SelectedFrame.RoundedSize = 0
      LogScale = False
      MaxFontSize = 9
      SelectedColor = clNone
      PageIndex = 0
      RowSpacing = 1
      ShrinkDiacritic = False
      Sorted = False
      OnAdvancedCustomDrawItem = tagCloudAdvancedCustomDrawItem
      OnTagClick = tiABCTagClick
      OnTagPositioning = tagCloudTagPositioning
    end
  end
  object odFile: TOpenDialog
    Filter = 'Test documents (*.rtf, *.txt)|*.rtf;*.txt'
    Left = 432
    Top = 268
  end
end
