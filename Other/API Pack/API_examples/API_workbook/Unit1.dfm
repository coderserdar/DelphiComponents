object Form1: TForm1
  Left = 536
  Top = 129
  Caption = 'Form1'
  ClientHeight = 432
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    600
    432)
  PixelsPerInch = 96
  TextHeight = 13
  object API_label1: TAPI_label
    Left = 8
    Top = 120
    Width = 324
    Height = 13
    Caption = 
      'Table Snapshot (also when created, you can edit workbook directl' +
      'y):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = True
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clTeal
    FontShadow.Height = -11
    FontShadow.Name = 'MS Sans Serif'
    FontShadow.Style = []
  end
  object API_label2: TAPI_label
    Left = 8
    Top = 8
    Width = 33
    Height = 13
    Caption = 'Pages:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = True
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clTeal
    FontShadow.Height = -11
    FontShadow.Name = 'MS Sans Serif'
    FontShadow.Style = []
  end
  object API_label3: TAPI_label
    Left = 8
    Top = 32
    Width = 43
    Height = 13
    Caption = 'Columns:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = True
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clTeal
    FontShadow.Height = -11
    FontShadow.Name = 'MS Sans Serif'
    FontShadow.Style = []
  end
  object API_label4: TAPI_label
    Left = 8
    Top = 56
    Width = 30
    Height = 13
    Caption = 'Rows:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = True
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clTeal
    FontShadow.Height = -11
    FontShadow.Name = 'MS Sans Serif'
    FontShadow.Style = []
  end
  object Bevel1: TBevel
    Left = 8
    Top = 96
    Width = 569
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
    ExplicitWidth = 633
  end
  object API_label6: TAPI_label
    Left = 144
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Column:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = True
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clTeal
    FontShadow.Height = -11
    FontShadow.Name = 'MS Sans Serif'
    FontShadow.Style = []
  end
  object API_label7: TAPI_label
    Left = 280
    Top = 8
    Width = 25
    Height = 13
    Caption = 'Row:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = True
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clTeal
    FontShadow.Height = -11
    FontShadow.Name = 'MS Sans Serif'
    FontShadow.Style = []
  end
  object API_label8: TAPI_label
    Left = 144
    Top = 32
    Width = 30
    Height = 13
    Caption = 'Value:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = True
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clTeal
    FontShadow.Height = -11
    FontShadow.Name = 'MS Sans Serif'
    FontShadow.Style = []
  end
  object API_label9: TAPI_label
    Left = 144
    Top = 80
    Width = 38
    Height = 13
    Caption = 'Column:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ColorMouse = clBlue
    Shadow = True
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clTeal
    FontShadow.Height = -11
    FontShadow.Name = 'MS Sans Serif'
    FontShadow.Style = []
  end
  object API_edit1: TAPI_edit
    Left = 72
    Top = 8
    Width = 56
    Height = 21
    Color = clWhite
    TabOrder = 0
    Text = '1'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
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
    Elliptic = False
    EllipticRadius = 5
  end
  object API_edit2: TAPI_edit
    Left = 72
    Top = 32
    Width = 56
    Height = 21
    Color = clWhite
    TabOrder = 1
    Text = '7'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
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
    Elliptic = False
    EllipticRadius = 5
  end
  object API_edit3: TAPI_edit
    Left = 72
    Top = 56
    Width = 56
    Height = 21
    Color = clWhite
    TabOrder = 2
    Text = '31'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
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
    Elliptic = False
    EllipticRadius = 5
  end
  object API_grbutton1: TAPI_grbutton
    Left = 8
    Top = 83
    Width = 121
    Height = 22
    BorderWidth = 1
    Caption = 'Re-generate Table'
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
    OnClick = API_grbutton1Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_edit5: TAPI_edit
    Left = 208
    Top = 8
    Width = 56
    Height = 21
    Color = clWhite
    TabOrder = 4
    Text = '0'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
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
    Elliptic = False
    EllipticRadius = 5
  end
  object API_edit6: TAPI_edit
    Left = 328
    Top = 8
    Width = 56
    Height = 21
    Color = clWhite
    TabOrder = 5
    Text = '0'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
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
    Elliptic = False
    EllipticRadius = 5
  end
  object API_edit7: TAPI_edit
    Left = 208
    Top = 32
    Width = 177
    Height = 21
    Color = clWhite
    TabOrder = 6
    Text = '0,00'
    EditMode = mFloatEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
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
    Elliptic = False
    EllipticRadius = 5
  end
  object API_grbutton2: TAPI_grbutton
    Left = 208
    Top = 52
    Width = 49
    Height = 22
    BorderWidth = 1
    Caption = 'Read'
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
    TabOrder = 7
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton2Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton3: TAPI_grbutton
    Left = 263
    Top = 52
    Width = 49
    Height = 22
    BorderWidth = 1
    Caption = 'Write'
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
    TabOrder = 8
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton3Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_edit8: TAPI_edit
    Left = 208
    Top = 80
    Width = 56
    Height = 21
    Color = clWhite
    TabOrder = 9
    Text = '0'
    EditMode = mNumEdit
    Decimals = 2
    Wordwrap = False
    Alignment = taRightJustify
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
    Elliptic = False
    EllipticRadius = 5
  end
  object API_grbutton4: TAPI_grbutton
    Left = 287
    Top = 80
    Width = 98
    Height = 22
    BorderWidth = 1
    Caption = 'Draw Diagram'
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
    TabOrder = 10
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton4Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton5: TAPI_grbutton
    Left = 3
    Top = 404
    Width = 80
    Height = 22
    Anchors = [akBottom]
    BorderWidth = 1
    Caption = 'first'
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
    TabOrder = 11
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton5Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton6: TAPI_grbutton
    Left = 133
    Top = 404
    Width = 80
    Height = 22
    Anchors = [akBottom]
    BorderWidth = 1
    Caption = 'prev'
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
    TabOrder = 12
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton6Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton7: TAPI_grbutton
    Left = 380
    Top = 404
    Width = 80
    Height = 22
    Anchors = [akBottom]
    BorderWidth = 1
    Caption = 'next'
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
    TabOrder = 13
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton7Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton8: TAPI_grbutton
    Left = 501
    Top = 404
    Width = 80
    Height = 22
    Anchors = [akBottom]
    BorderWidth = 1
    Caption = 'last'
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
    TabOrder = 14
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton8Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton9: TAPI_grbutton
    Left = 256
    Top = 404
    Width = 80
    Height = 22
    Anchors = [akBottom]
    BorderWidth = 1
    Caption = '0'
    Color = clWhite
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 15
    VerticalAlignment = taVerticalCenter
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton10: TAPI_grbutton
    Left = 480
    Top = 56
    Width = 95
    Height = 22
    Anchors = [akTop, akRight]
    BorderWidth = 1
    Caption = 'Save Workbook'
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
    TabOrder = 16
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton10Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton11: TAPI_grbutton
    Left = 480
    Top = 36
    Width = 95
    Height = 22
    Anchors = [akTop, akRight]
    BorderWidth = 1
    Caption = 'Open Workbook'
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
    TabOrder = 17
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton11Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object Chart1: TChart
    Left = 400
    Top = 8
    Width = 71
    Height = 97
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Legend.Visible = False
    MarginBottom = 3
    MarginTop = 3
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    View3D = False
    TabOrder = 18
    Anchors = [akLeft, akTop, akRight]
    object Series1: TLineSeries
      Marks.Callout.Brush.Color = clBlack
      Marks.Visible = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object API_grbutton12: TAPI_grbutton
    Left = 480
    Top = 8
    Width = 95
    Height = 22
    Anchors = [akTop, akRight]
    BorderWidth = 1
    Caption = 'Save Diagram'
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
    TabOrder = 19
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton12Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object GroupBox1: TGroupBox
    Left = 430
    Top = 128
    Width = 145
    Height = 273
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Information:'
    TabOrder = 20
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object API_label5: TAPI_label
      Left = 8
      Top = 24
      Width = 52
      Height = 13
      Caption = 'Document:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
      ColorMouse = clBlue
      Shadow = True
      ShadowOffset = -2
      FontShadow.Charset = DEFAULT_CHARSET
      FontShadow.Color = clTeal
      FontShadow.Height = -11
      FontShadow.Name = 'MS Sans Serif'
      FontShadow.Style = []
    end
    object API_label10: TAPI_label
      Left = 8
      Top = 56
      Width = 34
      Height = 13
      Caption = 'Author:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
      ColorMouse = clBlue
      Shadow = True
      ShadowOffset = -2
      FontShadow.Charset = DEFAULT_CHARSET
      FontShadow.Color = clTeal
      FontShadow.Height = -11
      FontShadow.Name = 'MS Sans Serif'
      FontShadow.Style = []
    end
    object Label2: TLabel
      Left = 8
      Top = 72
      Width = 32
      Height = 13
      Caption = 'Label2'
    end
    object API_label11: TAPI_label
      Left = 8
      Top = 88
      Width = 40
      Height = 13
      Caption = 'Created:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
      ColorMouse = clBlue
      Shadow = True
      ShadowOffset = -2
      FontShadow.Charset = DEFAULT_CHARSET
      FontShadow.Color = clTeal
      FontShadow.Height = -11
      FontShadow.Name = 'MS Sans Serif'
      FontShadow.Style = []
    end
    object Label3: TLabel
      Left = 8
      Top = 104
      Width = 32
      Height = 13
      Caption = 'Label3'
    end
    object API_label12: TAPI_label
      Left = 8
      Top = 120
      Width = 30
      Height = 13
      Caption = 'Editor:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
      ColorMouse = clBlue
      Shadow = True
      ShadowOffset = -2
      FontShadow.Charset = DEFAULT_CHARSET
      FontShadow.Color = clTeal
      FontShadow.Height = -11
      FontShadow.Name = 'MS Sans Serif'
      FontShadow.Style = []
    end
    object Label4: TLabel
      Left = 8
      Top = 136
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object API_label13: TAPI_label
      Left = 8
      Top = 152
      Width = 43
      Height = 13
      Caption = 'Modified:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
      ColorMouse = clBlue
      Shadow = True
      ShadowOffset = -2
      FontShadow.Charset = DEFAULT_CHARSET
      FontShadow.Color = clTeal
      FontShadow.Height = -11
      FontShadow.Name = 'MS Sans Serif'
      FontShadow.Style = []
    end
    object Label5: TLabel
      Left = 8
      Top = 168
      Width = 32
      Height = 13
      Caption = 'Label5'
    end
  end
  object API_grbutton13: TAPI_grbutton
    Left = 480
    Top = 84
    Width = 95
    Height = 22
    Anchors = [akTop, akRight]
    BorderWidth = 1
    Caption = 'Export as HTML'
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
    TabOrder = 21
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton13Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = clGray
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clBlack
    Elliptic = True
    CornerRadius = 5
    ShowCaption = True
    WordWrap = False
  end
  object API_stringgrid1: TAPI_stringgrid
    Left = 8
    Top = 139
    Width = 418
    Height = 255
    Anchors = [akLeft, akTop, akRight, akBottom]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    PopupMenu = PopupMenu1
    TabOrder = 22
    OnGetEditText = API_stringgrid1GetEditText
    OnSetEditText = API_stringgrid1SetEditText
    OnTopLeftChanged = API_stringgrid1TopLeftChanged
    MultilineCells = True
    WordWrap = True
    FontFixed = clWindowText
    FontFocused = clWindowText
    FontSelected = clWindowText
    ColorFocused = clYellow
    ColorSelected = clWindow
    VerAlign = taAlignTop
    HorAlign = taLeftJustify
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object OpenDialog1: TOpenDialog
    Left = 288
    Top = 88
  end
  object SaveDialog1: TSaveDialog
    Left = 288
    Top = 120
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 664
    Top = 432
  end
  object PopupMenu1: TPopupMenu
    Left = 200
    Top = 240
    object ExportCurrentRowasStringList1: TMenuItem
      Caption = 'Export Current Row as StringList'
      OnClick = ExportCurrentRowasStringList1Click
    end
    object ExportCurrentColasStringList1: TMenuItem
      Caption = 'Export Current Col as StringList'
      OnClick = ExportCurrentColasStringList1Click
    end
    object Cancel1: TMenuItem
      Caption = 'Cancel'
    end
  end
  object API_workbook1: TAPI_workbook
    Author = 'nobody'
    Created = 39713.603133310180000000
    Editor = 'unknown'
    Modified = 40119.515747407400000000
    Pages = 1
    Page = 0
    Columns = 2
    Rows = 10
    Priority = TWB_double
    Left = 152
    Top = 96
  end
end