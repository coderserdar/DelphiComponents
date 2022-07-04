object Service1: TService1
  OldCreateOrder = True
  DisplayName = 'NCOCI'
  StartType = stManual
  OnExecute = Service1Execute
  Left = 301
  Top = 148
  Height = 150
  Width = 215
  object OCIDatabase1: TOCIDatabase
    InitModes = [dmThreaded]
    UserName = 'demo'
    Password = 'demo'
    SilentMode = True
    Left = 24
    Top = 8
  end
  object OCIQuery1: TOCIQuery
    SQL.Strings = (
      'Create table abab (f1 varchar2(100))')
    Left = 88
    Top = 8
  end
  object OCIQuery2: TOCIQuery
    SQL.Strings = (
      'qweqwe drop table abab')
    Left = 88
    Top = 56
  end
end
