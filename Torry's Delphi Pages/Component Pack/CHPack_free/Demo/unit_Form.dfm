object frmCHForm: TfrmCHForm
  Left = 434
  Top = 266
  Width = 460
  Height = 456
  Caption = 'CH Form'
  Color = 16776176
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 8
    Top = 10
    Width = 433
    Height = 49
    Caption = 'On Top'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 0
    object cbOnTopForm: TCHCheckBox
      Left = 8
      Top = 20
      Width = 107
      Height = 20
      OnClick = cbOnTopFormClick
      AllowGrayed = False
      Box.Style = bxNormal
      Box.Color = bcRed
      Box.Fill = cfHook
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = False
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF008080
        8000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        800000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00C0C0C000FFFFFF00FF00FF00FF00FF008080
        8000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000C0C0C000FFFFFF00FF00FF00FF00FF008080
        8000808080008080800080808000808080008080800080808000808080008080
        800080808000808080008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      State = cbUnchecked
      Caption = 'Allways On Top'
      TabOrder = 0
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 70
    Width = 433
    Height = 81
    Caption = 'On Top Mode'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 419
      Height = 25
      AutoSize = False
      Caption = 
        'Open another program which has also the option "OnTop" (for exam' +
        'ple Winamp) and switches to "ForceOnTop". This window will be al' +
        'ways stay "OnTop".'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object rbNormalOnTop: TCHRadioButton
      Left = 8
      Top = 18
      Width = 101
      Height = 20
      OnClick = rbNormalOnTopClick
      Box.Style = bxNormal
      Box.Color = bcGreen
      Box.Fill = rfDot
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = True
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        76020000424D760200000000000036000000280000000C0000000C0000000100
        2000000000004002000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00C0C0
        C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C00080808000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF000080000000800000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00008000000080
        00000080000000800000FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0008080
        8000FFFFFF00FFFFFF0000800000008000000080000000800000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF000080
        000000800000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0
        C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C0008080800080808000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00C0C0C000C0C0C00080808000808080008080800080808000C0C0C000C0C0
        C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0C000C0C0
        C000C0C0C000C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      GroupIndex = 0
      Caption = 'NormalOnTop'
      TabOrder = 0
    end
    object rbForceOnTop: TCHRadioButton
      Left = 134
      Top = 18
      Width = 101
      Height = 20
      OnClick = rbForceOnTopClick
      Box.Style = bxNormal
      Box.Color = bcGreen
      Box.Fill = rfDot
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = False
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        76020000424D760200000000000036000000280000000C0000000C0000000100
        2000000000004002000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00C0C0
        C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C00080808000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0
        C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C0008080800080808000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00C0C0C000C0C0C00080808000808080008080800080808000C0C0C000C0C0
        C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0C000C0C0
        C000C0C0C000C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      GroupIndex = 0
      Caption = 'ForceOnTop'
      TabOrder = 1
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 160
    Width = 433
    Height = 49
    Caption = 'Title'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 2
    object rbShowTitle: TCHRadioButton
      Left = 8
      Top = 18
      Width = 97
      Height = 20
      OnClick = rbShowTitleClick
      Box.Style = bxNormal
      Box.Color = bcBlue
      Box.Fill = rfDot
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = True
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        76020000424D760200000000000036000000280000000C0000000C0000000100
        2000000000004002000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00C0C0
        C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C00080808000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FF000000FF00
        0000FF000000FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0008080
        8000FFFFFF00FFFFFF00FF000000FF000000FF000000FF000000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FF00
        0000FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0
        C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C0008080800080808000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00C0C0C000C0C0C00080808000808080008080800080808000C0C0C000C0C0
        C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0C000C0C0
        C000C0C0C000C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      GroupIndex = 0
      Caption = 'Show Titlebar'
      TabOrder = 0
    end
    object rbHideTitle: TCHRadioButton
      Left = 134
      Top = 18
      Width = 91
      Height = 20
      OnClick = rbHideTitleClick
      Box.Style = bxNormal
      Box.Color = bcBlue
      Box.Fill = rfDot
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = False
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        76020000424D760200000000000036000000280000000C0000000C0000000100
        2000000000004002000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00C0C0
        C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C00080808000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0
        C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C0008080800080808000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00C0C0C000C0C0C00080808000808080008080800080808000C0C0C000C0C0
        C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0C000C0C0
        C000C0C0C000C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      GroupIndex = 0
      Caption = 'Hide Titlebar'
      TabOrder = 1
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 218
    Width = 433
    Height = 63
    Caption = 'Move'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 3
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 419
      Height = 17
      AutoSize = False
      Caption = 
        'With "OnForm" the Form can move with the titelbar and the Formba' +
        'ckground (here: Skyblue)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object rbMoveOnlyTitle: TCHRadioButton
      Left = 8
      Top = 18
      Width = 77
      Height = 20
      OnClick = rbMoveOnlyTitleClick
      Box.Style = bxNormal
      Box.Color = bcLime
      Box.Fill = rfDot
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = True
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        76020000424D760200000000000036000000280000000C0000000C0000000100
        2000000000004002000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00C0C0
        C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C00080808000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF0000FF000000FF0000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF0000FF000000FF
        000000FF000000FF0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0008080
        8000FFFFFF00FFFFFF0000FF000000FF000000FF000000FF0000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF0000FF
        000000FF0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0
        C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C0008080800080808000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00C0C0C000C0C0C00080808000808080008080800080808000C0C0C000C0C0
        C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0C000C0C0
        C000C0C0C000C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      GroupIndex = 0
      Caption = 'Only on Title'
      TabOrder = 0
    end
    object rbMoveEverywhere: TCHRadioButton
      Left = 134
      Top = 18
      Width = 77
      Height = 20
      OnClick = rbMoveEverywhereClick
      Box.Style = bxNormal
      Box.Color = bcLime
      Box.Fill = rfDot
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = False
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        76020000424D760200000000000036000000280000000C0000000C0000000100
        2000000000004002000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00C0C0
        C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C00080808000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0
        C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C0008080800080808000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00C0C0C000C0C0C00080808000808080008080800080808000C0C0C000C0C0
        C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0C000C0C0
        C000C0C0C000C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      GroupIndex = 0
      Caption = 'On Form'
      TabOrder = 1
    end
    object rbMoveNone: TCHRadioButton
      Left = 248
      Top = 18
      Width = 77
      Height = 20
      OnClick = rbMoveNoneClick
      Box.Style = bxNormal
      Box.Color = bcLime
      Box.Fill = rfDot
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = False
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        76020000424D760200000000000036000000280000000C0000000C0000000100
        2000000000004002000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00C0C0
        C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C00080808000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0
        C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C0008080800080808000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00C0C0C000C0C0C00080808000808080008080800080808000C0C0C000C0C0
        C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0C000C0C0
        C000C0C0C000C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      GroupIndex = 0
      Caption = 'None'
      TabOrder = 2
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 292
    Width = 433
    Height = 49
    Caption = 'Fullscreen'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 4
    object CHRadioButton1: TCHRadioButton
      Left = 8
      Top = 18
      Width = 97
      Height = 20
      OnClick = CHRadioButton1Click
      Box.Style = bxNormal
      Box.Color = bcRed
      Box.Fill = rfDot
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = False
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        76020000424D760200000000000036000000280000000C0000000C0000000100
        2000000000004002000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00C0C0
        C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C00080808000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0
        C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C0008080800080808000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00C0C0C000C0C0C00080808000808080008080800080808000C0C0C000C0C0
        C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0C000C0C0
        C000C0C0C000C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      GroupIndex = 0
      Caption = 'On'
      TabOrder = 0
    end
    object CHRadioButton2: TCHRadioButton
      Left = 134
      Top = 18
      Width = 91
      Height = 20
      OnClick = CHRadioButton2Click
      Box.Style = bxNormal
      Box.Color = bcRed
      Box.Fill = rfDot
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Checked = True
      Color = clBtnFace
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmControl
      Glyph.Glyph.Data = {
        76020000424D760200000000000036000000280000000C0000000C0000000100
        2000000000004002000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00C0C0
        C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C00080808000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0C0008080
        8000FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF000000FF000000
        FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C0008080
        8000FFFFFF00FFFFFF000000FF000000FF000000FF000000FF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00C0C0C00080808000FFFFFF00FFFFFF00FFFFFF000000
        FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00C0C0
        C00080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FF00FF00FF00FF00C0C0C0008080800080808000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008080800080808000FFFFFF00FF00FF00FF00FF00FF00
        FF00C0C0C000C0C0C00080808000808080008080800080808000C0C0C000C0C0
        C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0C000C0C0
        C000C0C0C000C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00}
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      GroupIndex = 0
      Caption = 'Off'
      TabOrder = 1
    end
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 356
    Width = 433
    Height = 49
    Caption = 'Look'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 5
    object CHButton1: TCHButton
      Left = 16
      Top = 16
      Width = 75
      Height = 25
      Bitmap.Mode = bmNormal
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 2
      Border.SingleWidth = 1
      Border.Style = bsNormal
      Caption = 'Triangle'
      CaptionLayout.Alignment = tgCenter
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Color = clBtnFace
      Down = False
      Fill.Style = fsNormal
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmCaption
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      Gradient.Active0 = True
      Gradient.Active1 = True
      Gradient.Active2 = True
      Gradient.Active3 = False
      Gradient.Active4 = False
      Gradient.Active5 = False
      Gradient.Active6 = False
      Gradient.Active7 = False
      Gradient.Active8 = False
      Gradient.Active9 = False
      Gradient.Color0 = clGreen
      Gradient.Color1 = clRed
      Gradient.Color2 = clBlue
      Gradient.Color3 = clYellow
      Gradient.Color4 = clOlive
      Gradient.Color5 = clWhite
      Gradient.Color6 = clBlack
      Gradient.Color7 = clGray
      Gradient.Color8 = clLime
      Gradient.Color9 = clPurple
      Gradient.Style = gsHorizontal_L
      Gradient.Rotation = 100
      Style.Mode = bmButton
      Style.GroupIndex = 0
      Style.AllowAllUp = False
      TabOrder = 0
      OnClick = CHButton1Click
    end
    object CHButton2: TCHButton
      Left = 104
      Top = 16
      Width = 75
      Height = 25
      Bitmap.Mode = bmNormal
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 2
      Border.SingleWidth = 1
      Border.Style = bsNormal
      Caption = 'RoundRect'
      CaptionLayout.Alignment = tgCenter
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Color = clBtnFace
      Down = False
      Fill.Style = fsNormal
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmCaption
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      Gradient.Active0 = True
      Gradient.Active1 = True
      Gradient.Active2 = True
      Gradient.Active3 = False
      Gradient.Active4 = False
      Gradient.Active5 = False
      Gradient.Active6 = False
      Gradient.Active7 = False
      Gradient.Active8 = False
      Gradient.Active9 = False
      Gradient.Color0 = clGreen
      Gradient.Color1 = clRed
      Gradient.Color2 = clBlue
      Gradient.Color3 = clYellow
      Gradient.Color4 = clOlive
      Gradient.Color5 = clWhite
      Gradient.Color6 = clBlack
      Gradient.Color7 = clGray
      Gradient.Color8 = clLime
      Gradient.Color9 = clPurple
      Gradient.Style = gsHorizontal_L
      Gradient.Rotation = 100
      Style.Mode = bmButton
      Style.GroupIndex = 0
      Style.AllowAllUp = False
      TabOrder = 1
      OnClick = CHButton2Click
    end
    object CHButton3: TCHButton
      Left = 192
      Top = 16
      Width = 75
      Height = 25
      Bitmap.Mode = bmNormal
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 2
      Border.SingleWidth = 1
      Border.Style = bsNormal
      Caption = 'Ring'
      CaptionLayout.Alignment = tgCenter
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Color = clBtnFace
      Down = False
      Fill.Style = fsNormal
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmCaption
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      Gradient.Active0 = True
      Gradient.Active1 = True
      Gradient.Active2 = True
      Gradient.Active3 = False
      Gradient.Active4 = False
      Gradient.Active5 = False
      Gradient.Active6 = False
      Gradient.Active7 = False
      Gradient.Active8 = False
      Gradient.Active9 = False
      Gradient.Color0 = clGreen
      Gradient.Color1 = clRed
      Gradient.Color2 = clBlue
      Gradient.Color3 = clYellow
      Gradient.Color4 = clOlive
      Gradient.Color5 = clWhite
      Gradient.Color6 = clBlack
      Gradient.Color7 = clGray
      Gradient.Color8 = clLime
      Gradient.Color9 = clPurple
      Gradient.Style = gsHorizontal_L
      Gradient.Rotation = 100
      Style.Mode = bmButton
      Style.GroupIndex = 0
      Style.AllowAllUp = False
      TabOrder = 2
      OnClick = CHButton3Click
    end
    object CHButton4: TCHButton
      Left = 280
      Top = 16
      Width = 75
      Height = 25
      Bitmap.Mode = bmNormal
      Border.AutoColor = False
      Border.AutoPercent = 25
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 2
      Border.SingleWidth = 1
      Border.Style = bsNormal
      Caption = 'Elliptic'
      CaptionLayout.Alignment = tgCenter
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
      CaptionLayout.DisableColor = clBtnShadow
      CaptionLayout.HighlightDirection = drNone
      CaptionLayout.HighlightDepth = 0
      CaptionLayout.HighlightColor = clBtnHighlight
      CaptionLayout.PosX = 0
      CaptionLayout.PosY = 0
      CaptionLayout.ShadowDirection = drNone
      CaptionLayout.ShadowDepth = 0
      CaptionLayout.ShadowColor = clBtnShadow
      CaptionLayout.TextStyle = tsNone
      Color = clBtnFace
      Down = False
      Fill.Style = fsNormal
      Fill.Transparent = False
      Focus.Active = True
      Focus.Color = clBlack
      Focus.Mode = cmAuto
      Focus.Show = False
      Focus.Step = 2
      Focus.Style = csDot
      Focus.PosTop = 0
      Focus.PosBottom = 0
      Focus.PosLeft = 0
      Focus.PosRight = 0
      Focus.Width = 1
      Glyph.Alignment = gaLeft
      Glyph.AlignMode = gmCaption
      Glyph.PosX = 0
      Glyph.PosY = 0
      Glyph.Space = 5
      Glyph.TransparentColor = clWhite
      Glyph.TransparentMode = tmAuto
      Gradient.Active0 = True
      Gradient.Active1 = True
      Gradient.Active2 = True
      Gradient.Active3 = False
      Gradient.Active4 = False
      Gradient.Active5 = False
      Gradient.Active6 = False
      Gradient.Active7 = False
      Gradient.Active8 = False
      Gradient.Active9 = False
      Gradient.Color0 = clGreen
      Gradient.Color1 = clRed
      Gradient.Color2 = clBlue
      Gradient.Color3 = clYellow
      Gradient.Color4 = clOlive
      Gradient.Color5 = clWhite
      Gradient.Color6 = clBlack
      Gradient.Color7 = clGray
      Gradient.Color8 = clLime
      Gradient.Color9 = clPurple
      Gradient.Style = gsHorizontal_L
      Gradient.Rotation = 100
      Style.Mode = bmButton
      Style.GroupIndex = 0
      Style.AllowAllUp = False
      TabOrder = 3
      OnClick = CHButton4Click
    end
  end
  object MainMenu1: TMainMenu
    Left = 410
    Top = 4
    object close1: TMenuItem
      Caption = 'Close'
      OnClick = close1Click
    end
    object Info1: TMenuItem
      Caption = 'Info'
      OnClick = Info1Click
    end
  end
  object CHForm1: TCHForm
    AllwaysOnTop = False
    Fullscreen = False
    FormLook.Look = flNormal
    FormLook.RoundRectX = 20
    FormLook.RoundRectY = 20
    FormLook.InnerRing = 100
    MoveMode = foOnlyCaption
    OnTopMode = otNormalOnTop
    Title.Blink = False
    Title.Interval = 50
    Title.ShowTitlebar = True
    Left = 378
    Top = 6
  end
end
