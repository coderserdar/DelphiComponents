object frmPhoneSettings: TfrmPhoneSettings
  Left = 539
  Top = 370
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Phone settings'
  ClientHeight = 147
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 13
    Top = 5
    Width = 57
    Height = 13
    Caption = 'Phone IP:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 198
    Top = 5
    Width = 68
    Height = 13
    Caption = 'Phone Port:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 13
    Top = 53
    Width = 65
    Height = 13
    Caption = 'User name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edIP: TEdit
    Left = 10
    Top = 22
    Width = 177
    Height = 21
    ReadOnly = True
    TabOrder = 0
  end
  object edPort: TEdit
    Left = 193
    Top = 22
    Width = 74
    Height = 21
    TabOrder = 1
  end
  object edUser: TEdit
    Left = 10
    Top = 68
    Width = 175
    Height = 21
    TabOrder = 2
  end
  object BitBtn1: TBitBtn
    Left = 56
    Top = 112
    Width = 75
    Height = 25
    TabOrder = 3
    OnClick = BitBtn1Click
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 152
    Top = 112
    Width = 75
    Height = 25
    TabOrder = 4
    OnClick = BitBtn2Click
    Kind = bkCancel
  end
end
