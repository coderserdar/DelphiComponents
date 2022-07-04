object Form1: TForm1
  Left = 217
  Top = 167
  Width = 431
  Height = 375
  Caption = 'API_windows component Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    423
    341)
  PixelsPerInch = 96
  TextHeight = 13
  object API_label1: TAPI_label
    Left = 8
    Top = 8
    Width = 237
    Height = 13
    Caption = 'Application Caption to Monitor Memory Usage Of:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
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
    Left = 9
    Top = 47
    Width = 75
    Height = 13
    Caption = 'Memory Usage:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = False
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clGray
    FontShadow.Height = -11
    FontShadow.Name = 'Tahoma'
    FontShadow.Style = []
  end
  object API_label3: TAPI_label
    Left = 97
    Top = 47
    Width = 25
    Height = 13
    Caption = '-----'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = False
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clGray
    FontShadow.Height = -11
    FontShadow.Name = 'Tahoma'
    FontShadow.Style = [fsBold]
  end
  object API_label4: TAPI_label
    Left = 8
    Top = 59
    Width = 50
    Height = 13
    Caption = 'File Name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = False
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clGray
    FontShadow.Height = -11
    FontShadow.Name = 'Tahoma'
    FontShadow.Style = []
  end
  object API_label5: TAPI_label
    Left = 96
    Top = 59
    Width = 25
    Height = 13
    Caption = '-----'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = False
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clGray
    FontShadow.Height = -11
    FontShadow.Name = 'Tahoma'
    FontShadow.Style = [fsBold]
  end
  object API_linechart1: TAPI_linechart
    Left = 9
    Top = 84
    Width = 406
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clSilver
    ParentColor = False
    HistoryCount = 100
    Lines = 1
    BackgroundImage.Data = {07544269746D617000000000}
    Grid = True
    GridColor = clTeal
    GridCountH = 10
    GridCountV = 10
    ZoomAuto = True
    ShowMinMax = False
    Gradient = True
    GradientStart = 8404992
    GradientEnd = clGray
    GradientStyle = gsEllipse
  end
  object API_edit1: TAPI_edit
    Left = 8
    Top = 20
    Width = 301
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clWhite
    TabOrder = 0
    Text = 'Home Ftp Server'
    EditMode = mTextEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taLeftJustify
    ColorIn = 13303754
    ColorOut = clWhite
    Multiline = False
    ColorMouse = 12058623
    ValueRange = False
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
  object Button1: TButton
    Left = 315
    Top = 18
    Width = 100
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Force Foreground'
    TabOrder = 1
    OnClick = Button1Click
  end
  object API_windows1: TAPI_windows
    Left = 296
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 264
    Top = 8
  end
end
