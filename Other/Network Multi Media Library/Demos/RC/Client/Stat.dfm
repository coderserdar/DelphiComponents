object frmStat: TfrmStat
  Left = 460
  Top = 291
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Statistics'
  ClientHeight = 110
  ClientWidth = 265
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 249
    Height = 65
    TabOrder = 0
    object lbDeltaBytesReceived: TLabel
      Left = 10
      Top = 13
      Width = 101
      Height = 13
      Caption = 'FrameBytesRecieved'
    end
    object lbDeltasReceived: TLabel
      Left = 10
      Top = 29
      Width = 80
      Height = 13
      Caption = 'FramesRecieved'
    end
    object lbTotalBytesReceived: TLabel
      Left = 10
      Top = 44
      Width = 96
      Height = 13
      Caption = 'TotalBytesRecieved'
    end
  end
  object BitBtn1: TBitBtn
    Left = 180
    Top = 77
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkClose
  end
end
