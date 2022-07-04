object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SMS Send'
  ClientHeight = 231
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 109
    Width = 60
    Height = 13
    Caption = 'SMS Center:'
  end
  object Label2: TLabel
    Left = 32
    Top = 136
    Width = 48
    Height = 13
    Caption = 'Recipient:'
  end
  object Label3: TLabel
    Left = 32
    Top = 163
    Width = 69
    Height = 13
    Caption = 'SMS Message:'
  end
  object Label4: TLabel
    Left = 32
    Top = 29
    Width = 53
    Height = 13
    Caption = 'Serial Port:'
  end
  object Label5: TLabel
    Left = 32
    Top = 56
    Width = 48
    Height = 13
    Caption = 'Baudrate:'
  end
  object Bevel1: TBevel
    Left = 16
    Top = 8
    Width = 233
    Height = 81
  end
  object Bevel2: TBevel
    Left = 16
    Top = 95
    Width = 233
    Height = 97
  end
  object Edit1: TEdit
    Left = 112
    Top = 106
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 112
    Top = 133
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 112
    Top = 160
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Test message'
  end
  object Edit4: TEdit
    Left = 112
    Top = 26
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '1'
  end
  object Edit5: TEdit
    Left = 112
    Top = 53
    Width = 121
    Height = 21
    TabOrder = 4
    Text = '115200'
  end
  object Button1: TButton
    Left = 112
    Top = 198
    Width = 121
    Height = 25
    Caption = 'Send'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 264
    Top = 8
    Width = 353
    Height = 184
    TabOrder = 6
  end
  object API_modem1: TAPI_modem
    open = False
    port = 0
    baudrate = 115200
    Left = 192
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 312
    Top = 16
  end
end
