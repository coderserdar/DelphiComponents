object frmMessages: TfrmMessages
  Left = 439
  Top = 263
  Width = 389
  Height = 276
  Caption = 'Server Messages'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RichEdit: TRichEdit
    Left = 0
    Top = 0
    Width = 381
    Height = 211
    Align = alClient
    PlainText = True
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 211
    Width = 381
    Height = 38
    Align = alBottom
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 297
      Top = 7
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkClose
    end
  end
end
