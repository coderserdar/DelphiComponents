object dmBase: TdmBase
  OldCreateOrder = False
  Height = 269
  Width = 390
  object Database1: TDatabase
    AliasName = 'DBDEMOS'
    Connected = True
    DatabaseName = 'dbEmployy'
    LoginPrompt = False
    SessionName = 'Default'
    Left = 104
    Top = 24
  end
  object Session1: TSession
    Left = 152
    Top = 24
  end
  object tbCustomer: TTable
    Active = True
    AutoRefresh = True
    DatabaseName = 'dbEmployy'
    TableName = 'customer.db'
    Left = 32
    Top = 136
  end
  object tbCountry: TTable
    Active = True
    AutoRefresh = True
    DatabaseName = 'dbEmployy'
    TableName = 'country.db'
    Left = 104
    Top = 136
  end
  object tbVendors: TTable
    Active = True
    AutoRefresh = True
    DatabaseName = 'dbEmployy'
    TableName = 'vendors.db'
    Left = 200
    Top = 128
  end
end
