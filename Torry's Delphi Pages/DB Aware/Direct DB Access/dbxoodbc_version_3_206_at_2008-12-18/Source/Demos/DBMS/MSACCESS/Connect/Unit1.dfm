object Form1: TForm1
  Left = 415
  Top = 322
  Width = 639
  Height = 317
  Caption = 'Demo: DbxOOdbc MSAccess Connect'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LUSER: TLabel
    Left = 20
    Top = 92
    Width = 26
    Height = 13
    Caption = 'USER'
  end
  object LPWD: TLabel
    Left = 20
    Top = 120
    Width = 51
    Height = 13
    Caption = 'PASWORD'
  end
  object LDNS: TLabel
    Left = 20
    Top = 57
    Width = 20
    Height = 13
    Caption = 'DNS'
  end
  object LAdd: TLabel
    Left = 20
    Top = 153
    Width = 89
    Height = 13
    Caption = 'Additional options:'
  end
  object LDB: TLabel
    Left = 19
    Top = 23
    Width = 52
    Height = 13
    Caption = 'DATABASE'
  end
  object lbl1: TLabel
    Left = 4
    Top = 45
    Width = 10
    Height = 13
    Caption = 'or'
  end
  object sh1: TShape
    Left = 4
    Top = 4
    Width = 17
    Height = 17
    Brush.Color = clGray
  end
  object EUSER: TEdit
    Left = 100
    Top = 88
    Width = 120
    Height = 21
    TabOrder = 1
  end
  object EPWD: TEdit
    Left = 100
    Top = 112
    Width = 120
    Height = 21
    TabOrder = 2
  end
  object BConnect: TButton
    Left = 28
    Top = 181
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 5
    OnClick = BConnectClick
  end
  object BDisconnect: TButton
    Left = 116
    Top = 181
    Width = 85
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 6
    OnClick = BDisconnectClick
  end
  object EDNS: TEdit
    Left = 100
    Top = 58
    Width = 120
    Height = 21
    TabOrder = 3
  end
  object CDirectOdbc: TCheckBox
    Left = 296
    Top = 21
    Width = 109
    Height = 17
    Caption = 'Direct ODBC'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object EAdditional: TEdit
    Left = 113
    Top = 147
    Width = 472
    Height = 21
    TabOrder = 7
  end
  object EMDBFILENAME: TEdit
    Left = 100
    Top = 24
    Width = 161
    Height = 21
    TabOrder = 0
    Text = 'dbdemos.mdb'
  end
  object btn_mdb_load: TButton
    Left = 264
    Top = 24
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 8
    OnClick = btn_mdb_loadClick
  end
  object CUnicodeDriver: TCheckBox
    Left = 296
    Top = 45
    Width = 109
    Height = 17
    Caption = 'Unicode Driver'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 9
  end
  object SQLConnection: TSQLConnection
    LoginPrompt = False
    AfterConnect = SQLConnectionAfterConnect
    AfterDisconnect = SQLConnectionAfterDisconnect
    Left = 336
    Top = 72
  end
  object OD: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 440
    Top = 24
  end
end
