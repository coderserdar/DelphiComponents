object Form1: TForm1
  Left = 313
  Top = 111
  Width = 232
  Height = 395
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 16
    Top = 8
    Width = 193
    Height = 313
    Lines.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 72
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Reverse'
    TabOrder = 1
    OnClick = Button1Click
  end
  object OCIDatabase1: TOCIDatabase
    UserName = 'demo'
    Password = 'demo'
    Connected = True
    Left = 16
    Top = 8
  end
  object OCIStoredProc1: TOCIStoredProc
    Prepared = True
    Params = <
      item
        OName = ':ATABLE'
        ODataType = otString
        OParamType = odInOut
        ODataSize = 50
        IsPLSQLTable = True
      end>
    OPackageName = 'TEST_TAB'
    OProcedureName = 'TANSFORMARRAY'
    Left = 56
    Top = 8
  end
end
