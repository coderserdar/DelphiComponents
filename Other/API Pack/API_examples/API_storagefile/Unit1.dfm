object Form1: TForm1
  Left = 194
  Top = 111
  Width = 544
  Height = 419
  Caption = 'StorageFile Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    536
    385)
  PixelsPerInch = 96
  TextHeight = 13
  object API_label1: TAPI_label
    Left = 8
    Top = 168
    Width = 108
    Height = 13
    Caption = 'Storage File Contents:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    LinkedTo = API_listbox2
    ColorMouse = clBlue
    Shadow = False
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clGray
    FontShadow.Height = -11
    FontShadow.Name = 'Tahoma'
    FontShadow.Style = []
  end
  object API_label2: TAPI_label
    Left = 8
    Top = 8
    Width = 67
    Height = 13
    Caption = 'Files to Store:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    LinkedTo = API_listbox1
    ColorMouse = clBlue
    Shadow = False
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clGray
    FontShadow.Height = -11
    FontShadow.Name = 'Tahoma'
    FontShadow.Style = []
  end
  object API_grbutton1: TAPI_grbutton
    Left = 447
    Top = 24
    Width = 80
    Height = 22
    BorderWidth = 1
    Caption = 'Browse'
    Color = clBtnFace
    Enabled = True
    TabOrder = 0
    OnClick = API_grbutton1Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    ColorBorder = clBlack
    ColorOver = clSilver
    ColorDown = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ShowCaption = True
    WordWrap = False
    VerticalAlignment = taVerticalCenter
  end
  object API_grbutton2: TAPI_grbutton
    Left = 447
    Top = 187
    Width = 80
    Height = 22
    BorderWidth = 1
    Caption = 'Open File'
    Color = clBtnFace
    Enabled = True
    TabOrder = 1
    OnClick = API_grbutton2Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    ColorBorder = clBlack
    ColorOver = clSilver
    ColorDown = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ShowCaption = True
    WordWrap = False
    VerticalAlignment = taVerticalCenter
  end
  object API_grbutton3: TAPI_grbutton
    Left = 447
    Top = 52
    Width = 80
    Height = 22
    BorderWidth = 1
    Caption = 'Archive As..'
    Color = clBtnFace
    Enabled = True
    TabOrder = 2
    OnClick = API_grbutton3Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    ColorBorder = clBlack
    ColorOver = clSilver
    ColorDown = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ShowCaption = True
    WordWrap = False
    VerticalAlignment = taVerticalCenter
  end
  object API_listbox1: TAPI_listbox
    Left = 8
    Top = 24
    Width = 433
    Height = 138
    Style = lbOwnerDrawFixed
    Color = clWhite
    Columns = 0
    ItemHeight = 16
    TabOrder = 3
    LineColorSelected = clYellow
    LineColoring = True
    LineColorOdd = 13421772
    LineColorEven = clWhite
    MouseIsOver = False
    MouseOverColor = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    FontSelected.Charset = DEFAULT_CHARSET
    FontSelected.Color = clWindowText
    FontSelected.Height = -11
    FontSelected.Name = 'Tahoma'
    FontSelected.Style = []
    FontDisabled.Charset = DEFAULT_CHARSET
    FontDisabled.Color = clWindowText
    FontDisabled.Height = -11
    FontDisabled.Name = 'MS Sans Serif'
    FontDisabled.Style = []
    DisableString = '<!--'
    ProgressExists = False
    ProgressColor = clLime
    ColumnSeparator = '||'
    ColumnDefaultWidth = 92
  end
  object API_listbox2: TAPI_listbox
    Left = 8
    Top = 187
    Width = 433
    Height = 142
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akBottom]
    Color = clWhite
    Columns = 0
    ItemHeight = 16
    MultiSelect = True
    TabOrder = 4
    LineColorSelected = clYellow
    LineColoring = True
    LineColorOdd = 13421772
    LineColorEven = clWhite
    MouseIsOver = False
    MouseOverColor = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    FontSelected.Charset = DEFAULT_CHARSET
    FontSelected.Color = clWindowText
    FontSelected.Height = -11
    FontSelected.Name = 'Tahoma'
    FontSelected.Style = []
    FontDisabled.Charset = DEFAULT_CHARSET
    FontDisabled.Color = clWindowText
    FontDisabled.Height = -11
    FontDisabled.Name = 'MS Sans Serif'
    FontDisabled.Style = []
    DisableString = '<!--'
    ProgressExists = False
    ProgressColor = clLime
    ColumnSeparator = '||'
    ColumnDefaultWidth = 92
  end
  object API_grbutton4: TAPI_grbutton
    Left = 447
    Top = 212
    Width = 80
    Height = 22
    BorderWidth = 1
    Caption = 'Extract To..'
    Color = clBtnFace
    Enabled = True
    TabOrder = 5
    OnClick = API_grbutton4Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    ColorBorder = clBlack
    ColorOver = clSilver
    ColorDown = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ShowCaption = True
    WordWrap = False
    VerticalAlignment = taVerticalCenter
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 336
    Width = 433
    Height = 17
    Anchors = [akLeft, akBottom]
    TabOrder = 6
  end
  object ProgressBar2: TProgressBar
    Left = 8
    Top = 360
    Width = 433
    Height = 17
    Anchors = [akLeft, akBottom]
    TabOrder = 7
  end
  object API_StorageFile1: TAPI_StorageFile
    OnAddFiles = API_StorageFile1AddFiles
    OnFileProgress = API_StorageFile1FileProgress
    Left = 488
    Top = 128
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 488
    Top = 96
  end
  object SaveDialog1: TSaveDialog
    Left = 456
    Top = 128
  end
  object API_files1: TAPI_files
    ShowProgress = True
    Confirmation = False
    Left = 456
    Top = 96
  end
end
