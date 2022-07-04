object Form1: TForm1
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 253
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object API_label2: TAPI_label
    Left = 16
    Top = 104
    Width = 43
    Height = 13
    Caption = 'recipient:'
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
    Left = 16
    Top = 128
    Width = 49
    Height = 13
    Caption = 'command:'
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
    Left = 16
    Top = 176
    Width = 58
    Height = 13
    Caption = 'text to send:'
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
    Left = 16
    Top = 16
    Width = 26
    Height = 13
    Caption = 'name'
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
  object API_msgcomm1: TAPI_msgcomm
    Left = 344
    Top = 96
    Width = 33
    Height = 33
    Visible = False
    Mailbox = 'MyMailbox'
    OnMessage = API_msgcomm1Message
  end
  object API_label6: TAPI_label
    Left = 16
    Top = 48
    Width = 209
    Height = 41
    AutoSize = False
    Caption = '[no messages received]'
    Color = clGray
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
    WordWrap = True
    ColorMouse = clBlue
    Shadow = True
    ShadowOffset = -2
    FontShadow.Charset = DEFAULT_CHARSET
    FontShadow.Color = clTeal
    FontShadow.Height = -16
    FontShadow.Name = 'MS Sans Serif'
    FontShadow.Style = []
  end
  object API_label1: TAPI_label
    Left = 16
    Top = 152
    Width = 29
    Height = 13
    Caption = 'value:'
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
  object API_edit2: TAPI_edit
    Left = 96
    Top = 104
    Width = 153
    Height = 21
    Color = clWhite
    TabOrder = 0
    Text = 'API_edit2'
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
  object API_edit3: TAPI_edit
    Left = 96
    Top = 128
    Width = 80
    Height = 21
    Color = clWhite
    TabOrder = 1
    Text = '0'
    EditMode = mNumEdit
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
  object API_edit4: TAPI_edit
    Left = 96
    Top = 176
    Width = 321
    Height = 21
    Color = clWhite
    TabOrder = 2
    Text = 'API_edit4'
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
    Left = 96
    Top = 224
    Width = 321
    Height = 22
    BorderWidth = 1
    Caption = 'Send message'
    Color = clBtnFace
    Enabled = True
    TabOrder = 3
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
    FontMouseOver.Name = 'MS Sans Serif'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
  end
  object API_edit5: TAPI_edit
    Left = 96
    Top = 16
    Width = 321
    Height = 21
    Color = clWhite
    TabOrder = 4
    Text = 'API_edit5'
    OnChange = API_edit5Change
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
  object CheckBox1: TCheckBox
    Left = 96
    Top = 200
    Width = 145
    Height = 17
    Caption = 'Broadcast message'
    TabOrder = 5
  end
  object API_edit1: TAPI_edit
    Left = 96
    Top = 152
    Width = 80
    Height = 21
    Color = clWhite
    TabOrder = 6
    Text = '0'
    EditMode = mNumEdit
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
  object Panel1: TPanel
    Left = 232
    Top = 48
    Width = 185
    Height = 41
    TabOrder = 7
    object Label3: TLabel
      Left = 8
      Top = 4
      Width = 25
      Height = 13
      Caption = 'Sent:'
    end
    object Label4: TLabel
      Left = 8
      Top = 20
      Width = 49
      Height = 13
      Caption = 'Received:'
    end
    object Label1: TLabel
      Left = 145
      Top = 4
      Width = 32
      Height = 13
      Alignment = taRightJustify
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 145
      Top = 20
      Width = 32
      Height = 13
      Alignment = taRightJustify
      Caption = 'Label2'
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 384
    Top = 96
  end
end
