object Form1: TForm1
  Left = 415
  Top = 322
  Caption = 'Demo: DbxOOdbc Oracle Connect'
  ClientHeight = 306
  ClientWidth = 579
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
  object TTNS: TLabel
    Left = 40
    Top = 8
    Width = 50
    Height = 13
    Caption = 'TNS NAME'
  end
  object LUSER: TLabel
    Left = 40
    Top = 39
    Width = 26
    Height = 13
    Caption = 'USER'
  end
  object LPWD: TLabel
    Left = 40
    Top = 68
    Width = 51
    Height = 13
    Caption = 'PASWORD'
  end
  object LExample_oracle: TLabel
    Left = 8
    Top = 200
    Width = 209
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'ORACLE ODBC DRIVER'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Visible = False
  end
  object LExample_microsoft: TLabel
    Left = 324
    Top = 200
    Width = 265
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'MICROSOFT ODBC DRIVER FOR ORACLE'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Visible = False
  end
  object LDNS: TLabel
    Left = 40
    Top = 89
    Width = 20
    Height = 13
    Caption = 'DNS'
  end
  object sh1: TShape
    Left = 8
    Top = 4
    Width = 16
    Height = 16
    Brush.Color = clGray
  end
  object ETNS: TEdit
    Left = 110
    Top = 8
    Width = 120
    Height = 21
    TabOrder = 0
    Text = 'TNS_ORA'
  end
  object EUSER: TEdit
    Left = 110
    Top = 36
    Width = 120
    Height = 21
    TabOrder = 1
    Text = 'scott'
  end
  object EPWD: TEdit
    Left = 110
    Top = 60
    Width = 120
    Height = 21
    TabOrder = 2
    Text = 'tiger'
  end
  object BConnect: TButton
    Left = 48
    Top = 124
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 6
    OnClick = BConnectClick
  end
  object BDisconnect: TButton
    Left = 136
    Top = 124
    Width = 85
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 7
    OnClick = BDisconnectClick
  end
  object CMSDriver: TCheckBox
    Left = 252
    Top = 8
    Width = 109
    Height = 17
    Caption = 'Microsoft Driver'
    TabOrder = 4
  end
  object EDNS: TEdit
    Left = 110
    Top = 86
    Width = 120
    Height = 21
    TabOrder = 3
  end
  object CDirectOdbc: TCheckBox
    Left = 252
    Top = 32
    Width = 109
    Height = 17
    Caption = 'Direct ODBC'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object SQLConnection: TSQLConnection
    AfterConnect = SQLConnectionAfterConnect
    AfterDisconnect = SQLConnectionAfterDisconnect
    Left = 376
    Top = 20
  end
end
