object Form1: TForm1
  Left = 192
  Top = 114
  Width = 377
  Height = 354
  Caption = 'bk8x00 terminal register manipulating tool'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    369
    320)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 60
    Width = 115
    Height = 13
    Caption = 'Command Byte Address:'
  end
  object Label3: TLabel
    Left = 12
    Top = 108
    Width = 85
    Height = 13
    Caption = 'Register to Modify'
  end
  object Label4: TLabel
    Left = 12
    Top = 136
    Width = 67
    Height = 13
    Caption = 'Value to Write'
  end
  object Label2: TLabel
    Left = 12
    Top = 84
    Width = 99
    Height = 13
    Caption = 'Status Bute Address:'
  end
  object Bevel1: TBevel
    Left = 12
    Top = 48
    Width = 345
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Bevel2: TBevel
    Left = 12
    Top = 164
    Width = 345
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Bevel3: TBevel
    Left = 12
    Top = 208
    Width = 345
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object Edit2: TEdit
    Left = 236
    Top = 60
    Width = 121
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 0
    Text = '0'
  end
  object Edit3: TEdit
    Left = 236
    Top = 108
    Width = 121
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 1
    Text = '0'
  end
  object Edit4: TEdit
    Left = 236
    Top = 132
    Width = 121
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 2
    Text = '0'
  end
  object API_grbutton1: TAPI_grbutton
    Left = 12
    Top = 16
    Width = 345
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BorderWidth = 1
    Caption = 'Bk8x00 Settings'
    Color = clBtnFace
    Enabled = True
    TabOrder = 3
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
    FontMouseOver.Name = 'MS Sans Serif'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
    VerticalAlignment = taVerticalCenter
  end
  object API_grbutton2: TAPI_grbutton
    Left = 12
    Top = 176
    Width = 345
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BorderWidth = 1
    Caption = 'Write and Verify Registry Value'
    Color = clBtnFace
    Enabled = True
    TabOrder = 4
    OnClick = API_grbutton2Click
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
    FontMouseOver.Name = 'MS Sans Serif'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
    VerticalAlignment = taVerticalCenter
  end
  object API_listbox1: TAPI_listbox
    Left = 12
    Top = 220
    Width = 345
    Height = 89
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
    Columns = 0
    ItemHeight = 16
    TabOrder = 5
    LineColorSelected = clYellow
    LineColoring = True
    LineColorOdd = clBtnFace
    LineColorEven = clWhite
    MouseIsOver = False
    MouseOverColor = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'MS Sans Serif'
    FontMouseOver.Style = []
    FontSelected.Charset = DEFAULT_CHARSET
    FontSelected.Color = clWindowText
    FontSelected.Height = -11
    FontSelected.Name = 'MS Sans Serif'
    FontSelected.Style = []
    ProgressExists = False
    ProgressColor = clLime
    ColumnSeparator = '||'
    ColumnDefaultWidth = 92
  end
  object Edit1: TEdit
    Left = 236
    Top = 84
    Width = 121
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 6
    Text = '0'
  end
  object API_bk8x001: TAPI_bk8x00
    LastError = 0
    CloseOnError = True
    BusAddress = 11
    MessageIdent = 1
    OutputLength = 0
    InputLength = 0
    InputWaitCycles = 1000
    Open = False
    OpenRetries = 5
    ClearBuffersOnFailedRead = True
    ClearBuffersOnFailedWrite = True
    Port = 0
    Baudrate = 38400
    Parity = pEven
    Databits = dbEight
    Stopbits = sbOne
    BufferR = 512
    BufferW = 512
    TimeoutR = 1000
    TimeoutW = 1000
    ThreadDelay = 2
    ThreadPriority = tpIdle
    ThreadActive = False
    TheaadAutoActivate = False
    ThreadSynchronized = True
    Left = 188
    Top = 56
  end
end
