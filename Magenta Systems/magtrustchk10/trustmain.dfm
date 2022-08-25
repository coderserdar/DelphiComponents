object Form4: TForm4
  Left = 165
  Top = 80
  Caption = 'Code Signing Trust and Certificate Check - 26th November 2018'
  ClientHeight = 477
  ClientWidth = 682
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object doClose: TButton
    Left = 380
    Top = 430
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = doCloseClick
  end
  object Log: TMemo
    Left = 10
    Top = 5
    Width = 664
    Height = 371
    Lines.Strings = (
      'Log')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object doVerifyTrust: TButton
    Left = 200
    Top = 430
    Width = 75
    Height = 25
    Caption = 'Verify Trust'
    TabOrder = 3
    OnClick = doVerifyTrustClick
  end
  object doChkCert: TButton
    Left = 290
    Top = 430
    Width = 75
    Height = 25
    Caption = 'Check Cert'
    TabOrder = 2
    OnClick = doChkCertClick
  end
  object FileNames: TComboBox
    Left = 72
    Top = 392
    Width = 489
    Height = 21
    ItemHeight = 13
    TabOrder = 4
  end
  object doSelect: TButton
    Left = 576
    Top = 390
    Width = 75
    Height = 25
    Caption = 'Select File'
    TabOrder = 5
    OnClick = doSelectClick
  end
  object OpenDialog: TOpenDialog
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 64
    Top = 424
  end
end
