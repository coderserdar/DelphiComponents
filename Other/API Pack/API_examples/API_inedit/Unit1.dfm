object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Form1'
  ClientHeight = 49
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object API_grbutton1: TAPI_grbutton
    Left = 8
    Top = 8
    Width = 289
    Height = 17
    Alignment = taLeftJustify
    BorderWidth = 1
    Caption = 'click this button to edit this'
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
    TabOrder = 0
    VerticalAlignment = taVerticalCenter
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
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
  end
  object API_inedit1: TAPI_inedit
    Left = 16
    Top = 8
    Width = 16
    Height = 21
    AutoSize = False
    TabOrder = 1
    Text = 'API_inedit1'
    OnlyNumbers = False
    Wordwrap = True
    Alignment = taLeftJustify
    ExitonEnter = True
    ExitonEsc = False
    ExitonArrows = False
    ExitonTab = True
    ExitonPg = True
    OnHide = API_inedit1Hide
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 32
    Width = 97
    Height = 17
    Caption = 'only numbers'
    TabOrder = 2
  end
end