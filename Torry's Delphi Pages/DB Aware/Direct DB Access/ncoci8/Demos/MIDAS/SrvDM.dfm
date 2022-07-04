object NCOCI8MidasTest: TNCOCI8MidasTest
  OldCreateOrder = False
  Left = 328
  Top = 149
  Height = 480
  Width = 696
  object OCIDatabase1: TOCIDatabase
    UserName = 'demo'
    Password = 'demo'
    SilentMode = True
    Connected = True
    Left = 40
    Top = 16
  end
  object OCIQuery1: TOCIQuery
    SQL.Strings = (
      'select e.*, rowid from employee e')
    Left = 40
    Top = 64
  end
  object DataSetProvider1: TDataSetProvider
    DataSet = OCIQuery1
    Constraints = True
    ResolveToDataSet = True
    Left = 40
    Top = 112
  end
end
