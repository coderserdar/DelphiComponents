object PSForm: TPSForm
  Left = 276
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Namespaces'
  ClientHeight = 320
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 89
    Width = 382
    Height = 13
    Margins.Left = 0
    Margins.Top = 16
    Margins.Right = 0
    Margins.Bottom = 4
    Align = alTop
    Caption = 'List of namespaces:'
    ExplicitWidth = 95
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 382
    Height = 65
    Align = alTop
    Caption = 
      'Here you can define a list of known namespaces. This is an impor' +
      'tant option for importing resourcestring identifiers correctly f' +
      'rom source files when using Delphi 2009 and newer.'#13#10#13#10'You must r' +
      'estart PLS Editor to let the changes to take effect.'
    WordWrap = True
  end
  object mList: TMemo
    Left = 8
    Top = 106
    Width = 382
    Height = 173
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 8
    Top = 279
    Width = 382
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object sbOK: TButton
      Left = 227
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
    end
    object sbCancel: TButton
      Left = 307
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
    end
  end
end
