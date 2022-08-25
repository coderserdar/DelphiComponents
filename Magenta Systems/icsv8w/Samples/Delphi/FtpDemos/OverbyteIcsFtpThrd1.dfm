object ThrdFtpForm: TThrdFtpForm
  Left = 118
  Top = 122
  Caption = 'Threaded FTP'
  ClientHeight = 360
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 41
    Width = 583
    Height = 319
    Align = alClient
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 583
    Height = 41
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    object StartButton: TButton
      Left = 48
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = StartButtonClick
    end
    object SaveButton: TButton
      Left = 160
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 1
      OnClick = SaveButtonClick
    end
  end
end
