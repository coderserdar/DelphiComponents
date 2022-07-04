object Form1: TForm1
  Left = 271
  Top = 216
  Caption = 'dbxoodbc demo: mssql: RefCursor'
  ClientHeight = 248
  ClientWidth = 388
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
  object btnTest: TButton
    Left = 296
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object SQLConnection1: TSQLConnection
    DriverName = 'DbxSQLServer'
    GetDriverFunc = 'getSQLDriverODBC'
    LibraryName = 'dbxoodbc.dll'
    LoginPrompt = False
    Params.Strings = (
      'DbxSQLServer TransIsolation=ReadCommited'
      'Database=DATABASE=dbdemos;Trusted_Connection=Yes'
      'User_Name=user'
      'Password=password'
      'RowsetSize=20'
      'BlobSize=-1'
      'Trim Char=True'
      
        'Custom String=coICloneCon=1;coNetPacketSize=8192;coLockMode=17;c' +
        'oCatPrefix=DATABASE')
    VendorLib = 'sqlsrv32.dll'
    Left = 24
    Top = 16
  end
  object SQLStoredProc1: TSQLStoredProc
    MaxBlobSize = -1
    Params = <>
    SQLConnection = SQLConnection1
    Left = 56
    Top = 16
  end
end
