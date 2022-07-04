object Form1: TForm1
  Left = 192
  Top = 109
  Width = 455
  Height = 279
  Caption = 'Labjack demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 320
    Top = 176
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 320
    Top = 200
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 320
    Top = 224
    Width = 32
    Height = 13
    Caption = 'Label3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object API_linechart1: TAPI_linechart
    Left = 8
    Top = 8
    Width = 305
    Height = 233
    Color = clSilver
    ParentColor = False
    HistoryCount = 100
    Lines = 1
    BackgroundImage.Data = {07544269746D617000000000}
    Grid = False
    GridColor = clTeal
    GridCountH = 0
    GridCountV = 0
    ZoomAuto = True
    ShowMinMax = True
    Gradient = False
    GradientStart = clWhite
    GradientEnd = clSilver
    GradientStyle = gsRightToLeft
  end
  object API_edit1: TAPI_edit
    Left = 320
    Top = 8
    Width = 80
    Height = 21
    Color = clWhite
    TabOrder = 0
    Text = '0,00'
    EditMode = mFloatEdit
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
    FontMouseOver.Name = 'MS Sans Serif'
    FontMouseOver.Style = []
    FontEditing.Charset = DEFAULT_CHARSET
    FontEditing.Color = clWindowText
    FontEditing.Height = -11
    FontEditing.Name = 'MS Sans Serif'
    FontEditing.Style = []
  end
  object API_edit2: TAPI_edit
    Left = 320
    Top = 32
    Width = 80
    Height = 21
    Color = clWhite
    TabOrder = 1
    Text = '0,00'
    EditMode = mFloatEdit
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
    FontMouseOver.Name = 'MS Sans Serif'
    FontMouseOver.Style = []
    FontEditing.Charset = DEFAULT_CHARSET
    FontEditing.Color = clWindowText
    FontEditing.Height = -11
    FontEditing.Name = 'MS Sans Serif'
    FontEditing.Style = []
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 408
    Top = 64
  end
end
