object ViewContent: TViewContent
  Left = 349
  Top = 138
  Width = 563
  Height = 397
  Caption = 'View Content of '
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 328
    Width = 555
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 240
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkClose
    end
  end
  object lbFiles: TListBox
    Left = 0
    Top = 0
    Width = 555
    Height = 328
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
end
