object Form1: TForm1
  Left = 194
  Top = 115
  Width = 516
  Height = 330
  Caption = 'EKOWell Data Logger'
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 45
    Height = 13
    Caption = 'Com port:'
  end
  object Label2: TLabel
    Left = 8
    Top = 128
    Width = 90
    Height = 13
    Caption = 'Message log book:'
  end
  object Label4: TLabel
    Left = 8
    Top = 32
    Width = 46
    Height = 13
    Caption = 'Baudrate:'
  end
  object Label5: TLabel
    Left = 8
    Top = 56
    Width = 47
    Height = 13
    Caption = 'Last error:'
  end
  object Label6: TLabel
    Left = 64
    Top = 56
    Width = 5
    Height = 13
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object API_chart1: TAPI_chart
    Left = 232
    Top = 0
    Width = 276
    Height = 148
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
    ParentColor = False
    Sorted = False
    Title = 'Data Values'
    TitleColor = clBlack
    ColorBackground1 = clSilver
    ColorBackground2 = clWhite
    ChartType = ctBar
    XAxisFormat = '0.0'
    YAxisFormat = '0.0'
    XAxisShowItem = False
    YAxisShowItem = False
    XAxisShowExt = True
    YAxisShowExt = True
    MaxValues = 0
    XAxisSpacing = 5
    YAxisSpacing = 5
    ColorDefault = clRed
    ColorGrid = clBlack
  end
  object ComboBox1: TComboBox
    Left = 64
    Top = 8
    Width = 161
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = 'COM1'
    OnChange = ComboBoxChange
    Items.Strings = (
      'COM1'
      'COM2'
      'COM3'
      'COM4'
      'COM5'
      'COM6'
      'COM7'
      'COM8')
  end
  object Memo1: TMemo
    Left = 0
    Top = 148
    Width = 508
    Height = 148
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object ComboBox2: TComboBox
    Left = 64
    Top = 32
    Width = 161
    Height = 21
    ItemHeight = 13
    ItemIndex = 6
    TabOrder = 2
    Text = '9600'
    OnChange = ComboBoxChange
    Items.Strings = (
      '110'
      '300'
      '600'
      '1200'
      '2400'
      '4800'
      '9600'
      '14400'
      '19200'
      '38400'
      '56000'
      '115200'
      '128000'
      '256000')
  end
  object API_grbutton1: TAPI_grbutton
    Left = 8
    Top = 80
    Width = 217
    Height = 41
    BorderWidth = 1
    Caption = '---'
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
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 304
    Top = 24
  end
  object API_logfile1: TAPI_logfile
    MaxLines = 0
    TimeStamp = False
    MemoryList = False
    Left = 304
    Top = 56
  end
end
