object Form1: TForm1
  Left = 213
  Top = 141
  Width = 298
  Height = 251
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    290
    217)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 45
    Height = 13
    Caption = 'Computer'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 70
    Height = 13
    Caption = 'Service Name:'
  end
  object Label3: TLabel
    Left = 8
    Top = 104
    Width = 118
    Height = 13
    Caption = 'List of All Local Services:'
  end
  object API_edit1: TAPI_edit
    Left = 88
    Top = 16
    Width = 193
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clWhite
    TabOrder = 0
    EditMode = mTextEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taLeftJustify
    ColorIn = clLime
    ColorOut = clWhite
    Multiline = False
    ColorMouse = clYellow
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
    Left = 88
    Top = 40
    Width = 193
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clWhite
    TabOrder = 1
    Text = 'Alerter'
    EditMode = mTextEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taLeftJustify
    ColorIn = clLime
    ColorOut = clWhite
    Multiline = False
    ColorMouse = clYellow
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
  object API_grbutton1: TAPI_grbutton
    Left = 8
    Top = 72
    Width = 80
    Height = 22
    BorderWidth = 1
    Caption = 'Query State'
    Color = clBtnFace
    Enabled = True
    TabOrder = 2
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
    Left = 112
    Top = 72
    Width = 80
    Height = 22
    Anchors = [akTop, akRight]
    BorderWidth = 1
    Caption = 'Start Service'
    Color = 8454016
    Enabled = True
    TabOrder = 3
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
  object API_grbutton3: TAPI_grbutton
    Left = 200
    Top = 72
    Width = 80
    Height = 22
    Anchors = [akTop, akRight]
    BorderWidth = 1
    Caption = 'Stop Service'
    Color = 8421631
    Enabled = True
    TabOrder = 4
    OnClick = API_grbutton3Click
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
  object ListBox1: TListBox
    Left = 8
    Top = 120
    Width = 273
    Height = 62
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
  object API_edit3: TAPI_edit
    Left = 8
    Top = 189
    Width = 193
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    Color = clWhite
    TabOrder = 6
    EditMode = mTextEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taLeftJustify
    ColorIn = clLime
    ColorOut = clWhite
    Multiline = False
    ColorMouse = clYellow
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
  object API_grbutton4: TAPI_grbutton
    Left = 208
    Top = 189
    Width = 72
    Height = 22
    Anchors = [akRight, akBottom]
    BorderWidth = 1
    Caption = 'Locate'
    Color = clBtnFace
    Enabled = True
    TabOrder = 7
    OnClick = API_grbutton4Click
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
  object API_services1: TAPI_services
    Service = 'Event Log'
    Running = False
    Left = 224
    Top = 24
  end
end
