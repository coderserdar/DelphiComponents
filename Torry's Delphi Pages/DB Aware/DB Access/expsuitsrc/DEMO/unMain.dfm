object frmMain: TfrmMain
  Left = 246
  Top = 108
  Width = 565
  Height = 412
  Caption = 'Firesoft Export Suite Demo (Export Grid)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbHelp: TLabel
    Left = 92
    Top = 10
    Width = 221
    Height = 13
    Caption = 'Click Export and select the file format to export!'
  end
  object dbgContacts: TDBGrid
    Left = 4
    Top = 32
    Width = 547
    Height = 349
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dsContacts
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'FirstName'
        Title.Caption = 'First Name'
        Width = 59
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LastName'
        Title.Caption = 'Last Name'
        Width = 60
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Company'
        Width = 151
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Prefix'
        Width = 40
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Title'
        Visible = False
      end
      item
        Expanded = False
        FieldName = 'Address'
        Width = 137
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'City'
        Width = 77
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'State'
        Visible = False
      end
      item
        Expanded = False
        FieldName = 'ZipCode'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Source'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Customer'
        Width = 51
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PurchaseDate'
        Title.Caption = 'Purchase Date'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'HomePhone'
        Title.Caption = 'Home Phone'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FaxPhone'
        Title.Caption = 'Fax Phone'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PaymentType'
        Title.Caption = 'Payment Type'
        Width = 74
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Spouse'
        Width = 83
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Occupation'
        Width = 113
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PaymentAmount'
        Title.Caption = 'Payment Amount'
        Width = 85
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Link'
        Width = 186
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Copies'
        Width = 44
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Time'
        Width = 44
        Visible = True
      end>
  end
  object btnExport: TButton
    Left = 4
    Top = 4
    Width = 75
    Height = 25
    Caption = '&Export !'
    TabOrder = 1
    OnClick = btnExportClick
  end
  object chkCopyColumns: TCheckBox
    Left = 412
    Top = 8
    Width = 133
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Assign DBGrid Columns'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object tblContacts: TTable
    Active = True
    TableName = 'Contacts.DB'
    Left = 36
    Top = 64
  end
  object dsContacts: TDataSource
    DataSet = tblContacts
    Left = 64
    Top = 64
  end
  object DataToDbf1: TDataToDbf
    DataSet = tblContacts
    Fields = <>
    Left = 36
    Top = 92
  end
  object DataToAscii1: TDataToAscii
    DataSet = tblContacts
    Fields = <>
    Left = 64
    Top = 92
  end
  object DataToXLS1: TDataToXLS
    DataSet = tblContacts
    Columns = <>
    Title.Font.Charset = DEFAULT_CHARSET
    Title.Font.Color = clWindowText
    Title.Font.Height = -13
    Title.Font.Name = 'Arial'
    Title.Font.Style = [fsBold]
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -13
    Header.Font.Name = 'Arial'
    Header.Font.Style = [fsBold]
    Left = 92
    Top = 92
  end
  object DataToWK11: TDataToWK1
    DataSet = tblContacts
    Columns = <>
    Left = 120
    Top = 92
  end
  object SaveDialog: TSaveDialog
    Filter = 
      'DBF File|*.dbf|CSV File|*.csv|XLS File|*.xls|WK1 File|*.wk1|HTML' +
      ' File|*.html'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Export'
    Left = 92
    Top = 64
  end
  object DataToHTML1: TDataToHTML
    DataSet = tblContacts
    Fields = <>
    Left = 148
    Top = 92
  end
end
