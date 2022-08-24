object PSForm: TPSForm
  Left = 276
  Top = 108
  BorderStyle = bsDialog
  Caption = 'VirtualTree Options'
  ClientHeight = 320
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 106
    Width = 224
    Height = 13
    Caption = 'List of classes inherited from TBaseVirtualTree:'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 382
    Height = 89
    AutoSize = False
    Caption = 
      'TVirtualStringTree component uses a specific aproach to store (a' +
      'nd load) some kinds of properties into DFM files. If you want to' +
      ' use components inherited from TVirtualStringTree in PLS Editor,' +
      ' enter their class names into the list below.'#13#10#13#10'You must restar' +
      't PLS Editor to let the changes to take effect.'
    WordWrap = True
  end
  object mList: TMemo
    Left = 8
    Top = 124
    Width = 381
    Height = 153
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object sbOK: TButton
    Left = 236
    Top = 288
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
  end
  object sbCancel: TButton
    Left = 316
    Top = 288
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
  end
end
