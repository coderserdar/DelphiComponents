object LinkDlg: TLinkDlg
  Left = 192
  Top = 107
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Link Options'
  ClientHeight = 240
  ClientWidth = 358
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
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 33
    Height = 13
    Alignment = taRightJustify
    Caption = 'Table1'
  end
  object Label2: TLabel
    Left = 20
    Top = 27
    Width = 28
    Height = 13
    Alignment = taRightJustify
    Caption = 'Field1'
  end
  object Label3: TLabel
    Left = 16
    Top = 50
    Width = 33
    Height = 13
    Alignment = taRightJustify
    Caption = 'Table2'
  end
  object Label4: TLabel
    Left = 20
    Top = 69
    Width = 28
    Height = 13
    Alignment = taRightJustify
    Caption = 'Field2'
  end
  object StaticText1: TStaticText
    Left = 56
    Top = 8
    Width = 297
    Height = 17
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkSoft
    Caption = 'StaticText1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object StaticText2: TStaticText
    Left = 56
    Top = 27
    Width = 297
    Height = 17
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkSoft
    Caption = 'StaticText2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object StaticText3: TStaticText
    Left = 56
    Top = 48
    Width = 297
    Height = 17
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkSoft
    Caption = 'StaticText3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
  end
  object StaticText4: TStaticText
    Left = 56
    Top = 67
    Width = 297
    Height = 17
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkSoft
    Caption = 'StaticText4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 280
    Top = 176
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 280
    Top = 208
    Width = 73
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 96
    Width = 129
    Height = 137
    Caption = 'Join Operator'
    ItemIndex = 0
    Items.Strings = (
      '='
      '>'
      '<'
      '>='
      '<='
      '<>')
    TabOrder = 6
  end
  object RadioGroup2: TRadioGroup
    Left = 144
    Top = 96
    Width = 129
    Height = 137
    Caption = 'Join Type'
    TabOrder = 7
  end
end
