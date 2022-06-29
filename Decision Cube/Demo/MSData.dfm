object dmMSSQL: TdmMSSQL
  OldCreateOrder = False
  Left = 534
  Top = 389
  Height = 150
  Width = 215
  object Connection: TADOConnection
    ConnectionString = 
      'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security In' +
      'fo=False;Initial Catalog=Northwind'
    CursorLocation = clUseServer
    LoginPrompt = False
    Provider = 'SQLOLEDB.1'
    Left = 24
    Top = 8
  end
  object sales: TADODataSet
    Connection = Connection
    CursorLocation = clUseServer
    CursorType = ctOpenForwardOnly
    LockType = ltReadOnly
    CommandText = 
      'select OrderDate,ProductName, Sum(Quantity) as Qty'#13#10'  from [Orde' +
      'r Details] as i'#13#10'    join  Orders as o on o.OrderID=i.OrderID'#13#10' ' +
      '   join  Products as p on p.ProductID=i.ProductID'#13#10'  group by Or' +
      'derDate,ProductName'
    Parameters = <>
    Left = 80
    Top = 8
    object salesOrderDate: TDateTimeField
      FieldName = 'OrderDate'
    end
    object salesProductName: TWideStringField
      FieldName = 'ProductName'
      Size = 40
    end
    object salesQty: TIntegerField
      FieldName = 'Qty'
    end
  end
end
