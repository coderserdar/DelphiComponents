object Form1: TForm1
  Left = 311
  Top = 210
  Width = 291
  Height = 340
  Caption = 'Host Lookup Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 25
    Height = 13
    Caption = 'Host:'
  end
  object Label2: TLabel
    Left = 16
    Top = 44
    Width = 39
    Height = 13
    Caption = 'Method:'
  end
  object eHost: TEdit
    Left = 80
    Top = 12
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object cLookupMethod: TComboBox
    Left = 80
    Top = 40
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 1
    Text = 'Block'
    Items.Strings = (
      'Block'
      'Async'
      'Thread')
  end
  object bLookup: TButton
    Left = 16
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Lookup'
    TabOrder = 2
    OnClick = bLookupClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 120
    Width = 249
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object bCancel: TButton
    Left = 96
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = bCancelClick
  end
  object fndSocketHostLookup: TfndSocketHostLookup
    Active = False
    OnComplete = fndSocketHostLookupComplete
    Left = 200
    Top = 56
  end
end
