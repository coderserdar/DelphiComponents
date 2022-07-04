object Form1: TForm1
  Left = 192
  Top = 107
  Width = 277
  Height = 103
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 144
    Top = 8
    Width = 110
    Height = 20
    Caption = 'Threads count :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 145
    Top = 32
    Width = 6
    Height = 20
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 83
    Height = 25
    Caption = 'Run threads'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 8
    Top = 40
    Width = 83
    Height = 25
    Caption = 'Run foreground'
    TabOrder = 1
    OnClick = Button3Click
  end
  object OCIDatabase1: TOCIDatabase
    NonBlockingMode = True
    InitModes = [dmThreaded]
    UserName = 'demo'
    Password = 'demo'
    Connected = True
    DefaultDataFormat.OBoolType = otSmallInt
    DefaultDataFormat.OBoolSize = 4
    Left = 104
    Top = 8
  end
end
