object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 
    'API_logfile Example, Maximum lines limited set to 6 in propertie' +
    's'
  ClientHeight = 178
  ClientWidth = 727
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    727
    178)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 17
    Top = 71
    Width = 90
    Height = 13
    Caption = 'Msg to put into log:'
  end
  object Label2: TLabel
    Left = 16
    Top = 8
    Width = 63
    Height = 13
    Caption = 'Log filename:'
  end
  object Label3: TLabel
    Left = 20
    Top = 148
    Width = 9
    Height = 13
    Caption = '---'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 328
    Top = 157
    Width = 32
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Label4'
  end
  object API_edit1: TAPI_edit
    Left = 17
    Top = 87
    Width = 289
    Height = 26
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnKeyPress = API_edit1KeyPress
    EditMode = mTextEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taLeftJustify
    ColorIn = clLime
    ColorOut = clWhite
    Multiline = False
    ColorMouse = clYellow
    ValueRange = False
    FontMouseOver.Charset = ANSI_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -15
    FontMouseOver.Name = 'Verdana'
    FontMouseOver.Style = []
    FontEditing.Charset = ANSI_CHARSET
    FontEditing.Color = clWindowText
    FontEditing.Height = -15
    FontEditing.Name = 'Verdana'
    FontEditing.Style = []
  end
  object API_grbutton1: TAPI_grbutton
    Left = 17
    Top = 119
    Width = 289
    Height = 22
    BorderWidth = 1
    Caption = 'log message'
    Color = clBtnFace
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    VerticalAlignment = taVerticalCenter
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
    FontMouseOver.Height = -16
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    ColorBorder = 8556441
    ColorOver = clSilver
    ColorDown = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ShowCaption = True
    WordWrap = False
  end
  object API_edit2: TAPI_edit
    Left = 16
    Top = 21
    Width = 290
    Height = 26
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    EditMode = mTextEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taLeftJustify
    ColorIn = clLime
    ColorOut = clWhite
    Multiline = False
    ColorMouse = clYellow
    ValueRange = False
    FontMouseOver.Charset = ANSI_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -15
    FontMouseOver.Name = 'Verdana'
    FontMouseOver.Style = []
    FontEditing.Charset = ANSI_CHARSET
    FontEditing.Color = clWindowText
    FontEditing.Height = -15
    FontEditing.Name = 'Verdana'
    FontEditing.Style = []
  end
  object ListBox1: TListBox
    Left = 328
    Top = 8
    Width = 385
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 3
  end
  object CheckBox1: TCheckBox
    Left = 17
    Top = 48
    Width = 289
    Height = 17
    Caption = 'Create new log file on max lines reached'
    TabOrder = 4
  end
  object API_logfile1: TAPI_logfile
    Filename = 'c:\testi.txt'
    MaxLines = 6
    TimeStamp = True
    OnLog = API_logfile1Log
    OnLogFull = API_logfile1LogFull
    OnOpenFailed = API_logfile1OpenFailed
    Left = 168
    Top = 88
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 204
    Top = 88
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 20
    OnTimer = Timer2Timer
    Left = 240
    Top = 88
  end
end
