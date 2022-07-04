object frmCHRichedit: TfrmCHRichedit
  Left = 406
  Top = 233
  Width = 538
  Height = 500
  Caption = 'CH Richedit'
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
  object CHRichedit1: TCHRichedit
    Left = 14
    Top = 44
    Width = 217
    Height = 205
    TabOrder = 0
    Margin.MBottom = 0
    Margin.MLeft = 15
    Margin.MRight = 15
    Margin.MTop = 0
    Margin.Color = clWhite
    Margin.LineColor = clBlack
    Margin.LineWidth = 1
    LineSpacing = 2
    SelectColor = clBlack
  end
  object CHRichedit2: TCHRichedit
    Left = 262
    Top = 44
    Width = 217
    Height = 205
    TabOrder = 1
    Margin.MBottom = 15
    Margin.MLeft = 15
    Margin.MRight = 15
    Margin.MTop = 14
    Margin.Color = clInfoBk
    Margin.LineColor = clBlack
    Margin.LineWidth = 0
    LineSpacing = 2
    SelectColor = clBlack
  end
  object CHRichedit3: TCHRichedit
    Left = 22
    Top = 288
    Width = 459
    Height = 105
    TabOrder = 2
    Margin.MBottom = 0
    Margin.MLeft = 15
    Margin.MRight = 15
    Margin.MTop = 25
    Margin.Color = clBlue
    Margin.LineColor = clBlack
    Margin.LineWidth = 3
    LineSpacing = 2
    SelectColor = clBlack
  end
  object MainMenu1: TMainMenu
    Left = 118
    object close1: TMenuItem
      Caption = 'Close'
      OnClick = close1Click
    end
    object Info1: TMenuItem
      Caption = 'Info'
      OnClick = Info1Click
    end
  end
end
