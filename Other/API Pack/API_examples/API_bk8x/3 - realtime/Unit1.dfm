object Form1: TForm1
  Left = 242
  Top = 413
  Width = 552
  Height = 373
  Caption = 'BK8x Example (Third)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    544
    339)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object Label2: TLabel
    Left = 16
    Top = 35
    Width = 48
    Height = 13
    Caption = 'Baudrate:'
  end
  object Label3: TLabel
    Left = 16
    Top = 54
    Width = 60
    Height = 13
    Caption = 'Writelength:'
  end
  object Label4: TLabel
    Left = 159
    Top = 16
    Width = 65
    Height = 13
    Caption = 'Error History:'
  end
  object Label5: TLabel
    Left = 16
    Top = 73
    Width = 43
    Height = 13
    Caption = 'Address:'
  end
  object Bevel1: TBevel
    Left = 16
    Top = 96
    Width = 497
    Height = 65
    Anchors = [akLeft, akTop, akRight]
  end
  object Label6: TLabel
    Left = 28
    Top = 128
    Width = 31
    Height = 13
    Caption = 'Label6'
  end
  object Label7: TLabel
    Left = 28
    Top = 109
    Width = 31
    Height = 13
    Caption = 'Label7'
  end
  object API_ledgrid1: TAPI_ledgrid
    Left = 16
    Top = 181
    Width = 497
    Height = 68
    XCount = 1
    YCount = 8
    Transparent = False
    TransparentColor = clBlack
    Spacing = 2
    BoolGradient = False
    BoolTrue = clGreen
    BoolTrue2 = clGray
    BoolFalse = clRed
    BoolFalse2 = clGray
  end
  object API_ledgrid2: TAPI_ledgrid
    Left = 16
    Top = 255
    Width = 497
    Height = 66
    XCount = 1
    YCount = 8
    Transparent = False
    TransparentColor = clBlack
    Spacing = 2
    BoolGradient = False
    BoolTrue = clGreen
    BoolTrue2 = clGray
    BoolFalse = clRed
    BoolFalse2 = clGray
  end
  object API_edit1: TAPI_edit
    Left = 88
    Top = 13
    Width = 65
    Height = 21
    Color = clWhite
    TabOrder = 0
    Text = '0'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
    ColorIn = 13303754
    ColorOut = clWhite
    Multiline = False
    ColorMouse = 12058623
    ValueRange = True
    ValueMax = 255.000000000000000000
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    FontEditing.Charset = DEFAULT_CHARSET
    FontEditing.Color = clWindowText
    FontEditing.Height = -11
    FontEditing.Name = 'Tahoma'
    FontEditing.Style = []
  end
  object API_edit2: TAPI_edit
    Left = 88
    Top = 32
    Width = 65
    Height = 21
    Color = clWhite
    TabOrder = 1
    Text = '38400'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
    ColorIn = 13303754
    ColorOut = clWhite
    Multiline = False
    ColorMouse = 12058623
    ValueRange = False
    ValueMax = 255.000000000000000000
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    FontEditing.Charset = DEFAULT_CHARSET
    FontEditing.Color = clWindowText
    FontEditing.Height = -11
    FontEditing.Name = 'Tahoma'
    FontEditing.Style = []
  end
  object API_edit3: TAPI_edit
    Left = 88
    Top = 51
    Width = 65
    Height = 21
    Color = clWhite
    TabOrder = 2
    Text = '0'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
    ColorIn = 13303754
    ColorOut = clWhite
    Multiline = False
    ColorMouse = 12058623
    ValueRange = True
    ValueMax = 255.000000000000000000
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    FontEditing.Charset = DEFAULT_CHARSET
    FontEditing.Color = clWindowText
    FontEditing.Height = -11
    FontEditing.Name = 'Tahoma'
    FontEditing.Style = []
  end
  object API_listbox1: TAPI_listbox
    Left = 159
    Top = 32
    Width = 354
    Height = 54
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
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
  object API_edit4: TAPI_edit
    Left = 88
    Top = 70
    Width = 65
    Height = 21
    Color = clWhite
    TabOrder = 4
    Text = '11'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
    ColorIn = 13303754
    ColorOut = clWhite
    Multiline = False
    ColorMouse = 12058623
    ValueRange = True
    ValueMax = 99.000000000000000000
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    FontEditing.Charset = DEFAULT_CHARSET
    FontEditing.Color = clWindowText
    FontEditing.Height = -11
    FontEditing.Name = 'Tahoma'
    FontEditing.Style = []
  end
  object API_grbutton1: TAPI_grbutton
    Left = 16
    Top = 153
    Width = 497
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BorderWidth = 1
    Caption = 'Start IO Thread'
    Color = clBtnFace
    Enabled = True
    TabOrder = 5
    OnClick = API_grbutton1Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    ColorBorder = clBlack
    GradientEnd = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
    VerticalAlignment = taVerticalCenter
  end
  object API_bk8x1: TAPI_bk8x
    Port = 0
    Baudrate = 38400
    Open = False
    Address = 11
    MessageIdent = 1
    WriteLength = 0
    ThreadEvent = API_bk8x1ThreadEvent
    ThreadPriority = tpNormal
    ThreadError = API_bk8x1ThreadError
    ReadTimeout = 500
    ExpectedReadLength = 0
    Left = 256
    Top = 16
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 288
    Top = 16
  end
end
