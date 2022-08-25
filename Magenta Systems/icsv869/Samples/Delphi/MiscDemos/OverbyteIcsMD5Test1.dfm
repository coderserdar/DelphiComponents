object Form1: TForm1
  Left = 53
  Top = 89
  Caption = 'OverByte ICS - MD5 checksum test - http://www.overbyte.be'
  ClientHeight = 358
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 75
    Width = 680
    Height = 283
    Align = alBottom
    TabOrder = 0
  end
  object RunButton: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Run test'
    TabOrder = 1
    OnClick = RunButtonClick
  end
  object Button1: TButton
    Left = 104
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 190
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
  end
  object TestFileName: TEdit
    Left = 5
    Top = 48
    Width = 431
    Height = 21
    TabOrder = 4
  end
  object dpFtpFileMd5: TButton
    Left = 442
    Top = 8
    Width = 75
    Height = 25
    Caption = 'FtpFileMd5'
    TabOrder = 5
    OnClick = dpFtpFileMd5Click
  end
  object doFileMd5: TButton
    Left = 361
    Top = 8
    Width = 75
    Height = 25
    Caption = 'FileMd5'
    TabOrder = 6
    OnClick = doFileMd5Click
  end
  object doSelectFile: TButton
    Left = 280
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select File'
    TabOrder = 7
    OnClick = doSelectFileClick
  end
  object doFileCrcB: TButton
    Left = 523
    Top = 8
    Width = 75
    Height = 25
    Caption = 'FileCrcB'
    TabOrder = 8
    OnClick = doFileCrcBClick
  end
  object doFtpFileCrcB: TButton
    Left = 604
    Top = 8
    Width = 75
    Height = 25
    Caption = 'FtpFileCrcB'
    TabOrder = 9
    OnClick = doFtpFileCrcBClick
  end
  object OpenDialog: TOpenDialog
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent, ofForceShowHidden]
    Left = 140
    Top = 105
  end
end
