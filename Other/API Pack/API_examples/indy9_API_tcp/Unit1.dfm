object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 347
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  DesignSize = (
    493
    347)
  PixelsPerInch = 96
  TextHeight = 13
  object API_richedit1: TAPI_richedit
    Left = 0
    Top = 0
    Width = 493
    Height = 256
    Align = alClient
    TabOrder = 1
    WordWrap = False
    SearchType = [stMatchCase]
  end
  object Panel1: TPanel
    Left = 364
    Top = 8
    Width = 121
    Height = 81
    Anchors = [akTop, akRight]
    TabOrder = 0
    DesignSize = (
      121
      81)
    object API_label1: TAPI_label
      Left = 12
      Top = 2
      Width = 69
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Listening port:'
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
      ExplicitLeft = 164
    end
    object API_edit1: TAPI_edit
      Left = 9
      Top = 21
      Width = 104
      Height = 21
      Anchors = [akTop, akRight]
      Color = clWhite
      TabOrder = 0
      Text = '0'
      EditMode = mNumEdit
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
    object API_grbutton1: TAPI_grbutton
      Left = 8
      Top = 48
      Width = 105
      Height = 22
      Alignment = taLeftJustify
      Anchors = [akTop, akRight]
      BorderWidth = 1
      Caption = 'Activate server'
      Color = clBtnFace
      Enabled = True
      TabOrder = 1
      VerticalAlignment = taVerticalCenter
      OnClick = API_grbutton1Click
      LedExists = True
      LedState = False
      LedChangeOnClick = False
      LedColorOn = clGreen
      LedStyle = lsbox
      LedPosition = lpright
      LedColorOff = clRed
      ColorBorder = clBlack
      GradientEnd = clWhite
      FontMouseOver.Charset = DEFAULT_CHARSET
      FontMouseOver.Color = clWindowText
      FontMouseOver.Height = -11
      FontMouseOver.Name = 'Tahoma'
      FontMouseOver.Style = []
      ColorOver = clSilver
      ColorDown = clGray
      ShowCaption = True
      WordWrap = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 256
    Width = 493
    Height = 91
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      493
      91)
    object API_label2: TAPI_label
      Left = 8
      Top = 6
      Width = 26
      Height = 13
      Caption = 'Host:'
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
      Left = 8
      Top = 33
      Width = 20
      Height = 13
      Caption = 'File:'
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
    object API_label4: TAPI_label
      Left = 8
      Top = 60
      Width = 46
      Height = 13
      Caption = 'Message:'
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
      Left = 335
      Top = 6
      Width = 24
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Port:'
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
    object API_edit2: TAPI_edit
      Left = 66
      Top = 6
      Width = 263
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clWhite
      TabOrder = 0
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
    object API_edit3: TAPI_edit
      Left = 66
      Top = 33
      Width = 304
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clWhite
      TabOrder = 2
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
    object API_edit4: TAPI_edit
      Left = 66
      Top = 60
      Width = 367
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clWhite
      TabOrder = 3
      OnKeyPress = API_edit4KeyPress
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
    object API_grbutton2: TAPI_grbutton
      Left = 376
      Top = 33
      Width = 57
      Height = 22
      Anchors = [akTop, akRight]
      BorderWidth = 1
      Caption = 'Browse'
      Color = clBtnFace
      Enabled = True
      TabOrder = 4
      VerticalAlignment = taVerticalCenter
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
      FontMouseOver.Name = 'Tahoma'
      FontMouseOver.Style = []
      ColorOver = clSilver
      ColorDown = clGray
      ShowCaption = True
      WordWrap = False
    end
    object API_grbutton3: TAPI_grbutton
      Left = 439
      Top = 33
      Width = 46
      Height = 22
      Anchors = [akTop, akRight]
      BorderWidth = 1
      Caption = 'Send'
      Color = clBtnFace
      Enabled = True
      TabOrder = 5
      VerticalAlignment = taVerticalCenter
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
      FontMouseOver.Name = 'Tahoma'
      FontMouseOver.Style = []
      ColorOver = clSilver
      ColorDown = clGray
      ShowCaption = True
      WordWrap = False
    end
    object API_grbutton4: TAPI_grbutton
      Left = 439
      Top = 61
      Width = 46
      Height = 22
      Anchors = [akTop, akRight]
      BorderWidth = 1
      Caption = 'Send'
      Color = clBtnFace
      Enabled = True
      TabOrder = 6
      VerticalAlignment = taVerticalCenter
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
      FontMouseOver.Name = 'Tahoma'
      FontMouseOver.Style = []
      ColorOver = clSilver
      ColorDown = clGray
      ShowCaption = True
      WordWrap = False
    end
    object API_edit5: TAPI_edit
      Left = 376
      Top = 6
      Width = 114
      Height = 21
      Anchors = [akTop, akRight]
      Color = clWhite
      TabOrder = 1
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
  end
  object API_tcp1: TAPI_tcp
    ServerPort = 0
    Header = 'tapi_tcp_header'
    Active = False
    ReadTimeout = 0
    OnServerStart = API_tcp1ServerStart
    OnServerError = API_tcp1ServerError
    OnServerGetFile = API_tcp1ServerGetFile
    OnServerGetFileComplete = API_tcp1ServerGetFileComplete
    OnServerPutFile = API_tcp1ServerPutFile
    OnServerPutFileComplete = API_tcp1ServerPutFileComplete
    OnServerGetText = API_tcp1ServerGetText
    OnServerClose = API_tcp1ServerClose
    Left = 328
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Left = 328
    Top = 40
  end
  object SaveDialog1: TSaveDialog
    Left = 328
    Top = 72
  end
  object API_tools1: TAPI_tools
    Left = 328
    Top = 104
  end
end
