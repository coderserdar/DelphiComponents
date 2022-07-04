object frmCHApplication: TfrmCHApplication
  Left = 330
  Top = 321
  Width = 460
  Height = 360
  Caption = 'CH Application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox3: TGroupBox
    Left = 8
    Top = 12
    Width = 433
    Height = 49
    Caption = 'Just One Application'
    TabOrder = 0
    object cbJustOne: TCHCheckBox
      Left = 8
      Top = 21
      Width = 129
      Height = 20
      OnClick = cbJustOneClick
      AllowGrayed = False
      Box.Style = bxNormal
      Box.Color = bcLime
      Box.Fill = cfHook
      Border.Color = clBlack
      Border.HighlightColor = clWhite
      Border.ShadowColor = clGray
      Border.Width = 0
      Border.SingleWidth = 1
      Border.Style = bsNormal
      CaptionLayout.Alignment = tgAuto
      CaptionLayout.Angle = 0
      CaptionLayout.Antialiasing = True
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
      Caption = 'Just One Application'
      TabOrder = 0
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
  object CHApplication1: TCHApplication
    Hint.Color = clInfoBk
    Hint.HidePause = 2500
    Hint.Pause = 500
    Hint.Shortcuts = True
    Hint.ShortPause = 0
    Hint.ShowHints = True
    Instance.JustOneApp = False
    Instance.ShowMessage = False
    Restart.Mode = rsTimer
    Restart.Timer = False
    Restart.TimerInterval = 5000
    Restart.ShowMessage = False
    Restart.OnlyClose = False
    ShowApplication = True
    StartUp.Active = False
    StartUp.KeyMode = ruRUN
    StartUp.RegEvent = reAdd
    StartUp.RegKey = rkLocalMachine
    Flash.Blink = False
    Flash.Interval = 500
    Left = 378
    Top = 4
  end
end
