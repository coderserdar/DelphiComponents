object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 289
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 88
    Height = 13
    Caption = 'Ethernet Address:'
  end
  object Label2: TLabel
    Left = 8
    Top = 66
    Width = 67
    Height = 13
    Caption = 'Host address:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 35
    Width = 257
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 8
    Top = 120
    Width = 129
    Height = 25
    Caption = 'Send Wakeup On Lan'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 8
    Top = 85
    Width = 257
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object IdUDPClient1: TIdUDPClient
    Port = 0
    Left = 144
    Top = 96
  end
  object API_tools1: TAPI_tools
    Left = 176
    Top = 96
  end
end
