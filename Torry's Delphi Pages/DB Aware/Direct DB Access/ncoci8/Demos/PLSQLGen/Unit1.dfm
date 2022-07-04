object Form1: TForm1
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DBMS_SQL via PL/SQL Wrapper Objects Gen'
  ClientHeight = 219
  ClientWidth = 340
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
    Left = 8
    Top = 8
    Width = 82
    Height = 13
    Caption = 'Customer number'
  end
  object Label2: TLabel
    Left = 8
    Top = 36
    Width = 64
    Height = 13
    Caption = 'Customer info'
  end
  object Edit1: TEdit
    Left = 102
    Top = 6
    Width = 141
    Height = 21
    TabOrder = 0
    Text = '106'
  end
  object Button1: TButton
    Left = 252
    Top = 5
    Width = 75
    Height = 25
    Caption = 'Fetch info'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 56
    Width = 321
    Height = 153
    TabOrder = 2
  end
  object OCIDatabase1: TOCIDatabase
    UserName = 'DEMO'
    Password = 'demo'
    Connected = True
    Left = 88
    Top = 40
  end
end
