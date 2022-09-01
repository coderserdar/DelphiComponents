object frmCustomers: TfrmCustomers
  Left = 0
  Top = 0
  ClientHeight = 408
  ClientWidth = 631
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 367
    Width = 631
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      631
      41)
    object btnOk: TButton
      Left = 460
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 548
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Button1: TButton
      Left = 16
      Top = 6
      Width = 105
      Height = 25
      Action = DBFormAction1
      TabOrder = 2
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 631
    Height = 367
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DataSource1: TDataSource
    DataSet = dmBase.tbCustomer
    Left = 8
    Top = 8
  end
  object ActionList1: TActionList
    Left = 40
    Top = 8
    object DBFormAction1: TDBFormAction
      Category = 'DataSet'
      DestFormName = 'TfrmCountry'
      Locating = <
        item
          GetDataSet = 'dmBase.tbCustomer'
          SetDataSet = 'dmBase.tbCountry'
          GetFieldName = 'Country'
          SetFieldName = 'Name'
          SetDataSetPost = False
        end>
      Restoring = <
        item
          GetDataSet = 'dmBase.tbCountry'
          SetDataSet = 'dmBase.tbCustomer'
          GetFieldName = 'Name'
          SetFieldName = 'Country'
          SetDataSetPost = True
        end>
      OpenDataSets = <>
      Caption = 'Change country'
    end
  end
end
