object Form1: TForm1
  Left = 561
  Top = 106
  BorderStyle = bsDialog
  Caption = 'Example 4'
  ClientHeight = 189
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 369
    Height = 145
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 53
    Height = 13
    Caption = 'Port status:'
  end
  object Label2: TLabel
    Left = 104
    Top = 16
    Width = 265
    Height = 13
    AutoSize = False
    Caption = 'Label2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 32
    Width = 68
    Height = 13
    Caption = 'Thread status:'
  end
  object Label4: TLabel
    Left = 104
    Top = 32
    Width = 265
    Height = 13
    AutoSize = False
    Caption = 'Label4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 16
    Top = 48
    Width = 64
    Height = 13
    Caption = 'IO cycle time:'
  end
  object Label6: TLabel
    Left = 104
    Top = 48
    Width = 39
    Height = 13
    Caption = 'Label6'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 16
    Top = 64
    Width = 72
    Height = 13
    Caption = 'IO cycle count:'
  end
  object Label8: TLabel
    Left = 104
    Top = 64
    Width = 39
    Height = 13
    Caption = 'Label8'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label9: TLabel
    Left = 16
    Top = 80
    Width = 69
    Height = 13
    Caption = 'bk8x00 status:'
  end
  object Label10: TLabel
    Left = 104
    Top = 80
    Width = 46
    Height = 13
    Caption = 'Label10'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label11: TLabel
    Left = 16
    Top = 112
    Width = 61
    Height = 13
    Caption = 'Input values:'
  end
  object Label12: TLabel
    Left = 16
    Top = 128
    Width = 69
    Height = 13
    Caption = 'Output values:'
  end
  object Label13: TLabel
    Left = 104
    Top = 112
    Width = 38
    Height = 13
    Caption = 'Label13'
  end
  object Label14: TLabel
    Left = 104
    Top = 128
    Width = 38
    Height = 13
    Caption = 'Label14'
  end
  object Label15: TLabel
    Left = 104
    Top = 96
    Width = 46
    Height = 13
    Caption = 'Label15'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label16: TLabel
    Left = 152
    Top = 112
    Width = 38
    Height = 13
    Caption = 'Label13'
  end
  object Label17: TLabel
    Left = 200
    Top = 112
    Width = 38
    Height = 13
    Caption = 'Label13'
  end
  object Label18: TLabel
    Left = 248
    Top = 112
    Width = 38
    Height = 13
    Caption = 'Label13'
  end
  object Label19: TLabel
    Left = 152
    Top = 128
    Width = 38
    Height = 13
    Caption = 'Label14'
  end
  object Label20: TLabel
    Left = 200
    Top = 128
    Width = 38
    Height = 13
    Caption = 'Label14'
  end
  object Label21: TLabel
    Left = 248
    Top = 128
    Width = 38
    Height = 13
    Caption = 'Label14'
  end
  object Label22: TLabel
    Left = 296
    Top = 128
    Width = 38
    Height = 13
    Caption = 'Label14'
  end
  object Label23: TLabel
    Left = 296
    Top = 112
    Width = 38
    Height = 13
    Caption = 'Label13'
  end
  object API_gradient1: TAPI_gradient
    Left = 384
    Top = 0
    Width = 121
    Height = 189
    Align = alRight
    Caption = 'API_gradient1'
    TabOrder = 0
    StartColor = clBlack
    EndColor = clGray
    NumberofColors = 255
    GradientStyle = gsRightToLeft
    AutoHide = False
    DesignSize = (
      121
      189)
    object API_grbutton3: TAPI_grbutton
      Left = 8
      Top = 8
      Width = 105
      Height = 22
      BorderWidth = 1
      Caption = 'open/close port'
      Color = 8454143
      Enabled = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      VerticalAlignment = taVerticalCenter
      OnClick = API_grbutton3Click
      LedExists = False
      LedState = False
      LedChangeOnClick = False
      LedColorOn = clGreen
      LedStyle = lsellipse
      LedPosition = lpright
      LedColorOff = clRed
      ColorBorder = 8556441
      GradientEnd = clWhite
      FontMouseOver.Charset = DEFAULT_CHARSET
      FontMouseOver.Color = clWindowText
      FontMouseOver.Height = -11
      FontMouseOver.Name = 'Arial'
      FontMouseOver.Style = []
      ColorOver = clSilver
      ColorDown = clGray
      ShowCaption = True
      WordWrap = False
    end
    object API_grbutton1: TAPI_grbutton
      Left = 8
      Top = 32
      Width = 105
      Height = 22
      BorderWidth = 1
      Caption = 'start/stop thread'
      Color = 8454143
      Enabled = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
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
      ColorBorder = 8556441
      GradientEnd = clWhite
      FontMouseOver.Charset = DEFAULT_CHARSET
      FontMouseOver.Color = clWindowText
      FontMouseOver.Height = -11
      FontMouseOver.Name = 'Arial'
      FontMouseOver.Style = []
      ColorOver = clSilver
      ColorDown = clGray
      ShowCaption = True
      WordWrap = False
    end
    object API_grbutton2: TAPI_grbutton
      Left = 8
      Top = 56
      Width = 105
      Height = 22
      BorderWidth = 1
      Caption = 'settings'
      Color = 8454143
      Enabled = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      VerticalAlignment = taVerticalCenter
      OnClick = API_grbutton2Click
      LedExists = False
      LedState = False
      LedChangeOnClick = False
      LedColorOn = clGreen
      LedStyle = lsellipse
      LedPosition = lpright
      LedColorOff = clRed
      ColorBorder = 8556441
      GradientEnd = clWhite
      FontMouseOver.Charset = DEFAULT_CHARSET
      FontMouseOver.Color = clWindowText
      FontMouseOver.Height = -11
      FontMouseOver.Name = 'Arial'
      FontMouseOver.Style = []
      ColorOver = clSilver
      ColorDown = clGray
      ShowCaption = True
      WordWrap = False
    end
    object API_grbutton4: TAPI_grbutton
      Left = 8
      Top = 158
      Width = 105
      Height = 22
      Anchors = [akLeft, akBottom]
      BorderWidth = 1
      Caption = 'close'
      Color = 8454143
      Enabled = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      VerticalAlignment = taVerticalCenter
      OnClick = API_grbutton4Click
      LedExists = False
      LedState = False
      LedChangeOnClick = False
      LedColorOn = clGreen
      LedStyle = lsellipse
      LedPosition = lpright
      LedColorOff = clRed
      ColorBorder = 8556441
      GradientEnd = clWhite
      FontMouseOver.Charset = DEFAULT_CHARSET
      FontMouseOver.Color = clWindowText
      FontMouseOver.Height = -11
      FontMouseOver.Name = 'Arial'
      FontMouseOver.Style = []
      ColorOver = clSilver
      ColorDown = clGray
      ShowCaption = True
      WordWrap = False
    end
  end
  object API_grbutton5: TAPI_grbutton
    Left = 8
    Top = 160
    Width = 100
    Height = 22
    BorderWidth = 1
    Caption = 'save bus settings'
    Color = clBtnFace
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton5Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    ColorBorder = 8556441
    GradientEnd = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton6: TAPI_grbutton
    Left = 112
    Top = 160
    Width = 100
    Height = 22
    BorderWidth = 1
    Caption = 'open bus settings'
    Color = clBtnFace
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton6Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    ColorBorder = 8556441
    GradientEnd = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
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
    OpenRetries = 2
    ClearBuffersOnFailedRead = True
    ClearBuffersOnFailedWrite = True
    Port = 0
    Baudrate = 38400
    ThreadDelay = 2
    ThreadPriority = tpIdle
    ThreadActive = False
    TheaadAutoActivate = False
    ThreadSynchronized = True
    Left = 296
    Top = 80
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 328
    Top = 80
  end
end
