object FormObject: TFormObject
  Left = 0
  Top = 0
  Caption = 'Json Object'
  ClientHeight = 400
  ClientWidth = 686
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object SubRespList: TListView
    Left = 0
    Top = 0
    Width = 686
    Height = 400
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Type'
        Width = 70
      end
      item
        Caption = 'Value'
        Width = 400
      end
      item
        Width = 100
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = SubRespListDblClick
  end
end
