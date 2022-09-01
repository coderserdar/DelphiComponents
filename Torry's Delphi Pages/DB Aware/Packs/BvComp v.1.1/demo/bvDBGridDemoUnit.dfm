object bvDBGridDemoForm: TbvDBGridDemoForm
  Left = 28
  Top = 62
  Width = 639
  Height = 446
  ActiveControl = bvDBGrid1
  Caption = 'bvDBGrid'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 631
    Height = 4
    Align = alTop
    Shape = bsTopLine
  end
  object Panel1: TPanel
    Left = 0
    Top = 121
    Width = 631
    Height = 298
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    Caption = 'Panel1'
    TabOrder = 0
    object bvDBGrid1: TbvDBGrid
      Left = 5
      Top = 5
      Width = 621
      Height = 288
      Align = alClient
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnGetCellParams = bvDBGrid1GetCellParams
      FixedCols = 1
      SelectedIndex = 0
      StrippedRows = 3
      StrippedColor = 16054260
      CellHeights = 180
      TitleMinHeight = 100
      PrinterAutoPopup = True
      SaverCannotAddColumns = False
      Columns = <
        item
          Alignment = taCenter
          Expanded = False
          FieldName = 'CustNo'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Last_Name'
          Title.Caption = 'Last Name'
          Width = 92
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'First_Name'
          Title.Caption = 'This is very long string in caption'
          Width = 101
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Address1'
          Title.Font.Charset = DEFAULT_CHARSET
          Title.Font.Color = clWindowText
          Title.Font.Height = -16
          Title.Font.Name = 'MS Sans Serif'
          Title.Font.Style = []
          Width = 83
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'VIP_Status'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Title.Caption = 'VIP Status'
          Width = 45
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'City'
          Width = 73
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'State/Prov'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Post_Code'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsItalic]
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Country'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Phone'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Address2'
          Width = 129
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Fax'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'EMail'
          Visible = True
        end>
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 4
    Width = 631
    Height = 117
    Align = alTop
    BevelOuter = bvNone
    BevelWidth = 5
    BorderWidth = 5
    TabOrder = 1
    object Label1: TLabel
      Left = 5
      Top = 5
      Width = 388
      Height = 107
      Align = alLeft
      Caption = 
        '1.Click on right button in mouse and see automatic generated men' +
        'uitems'#13#10' in popup-menus or automatic generated popup-menus for b' +
        'vDBGrid'#13#10'   a) Grid Saver - Runtime saver for grid options'#13#10'   b' +
        ') Table Saver - Dataset Saver in db,dbf,txt,doc and excel format' +
        's'#13#10'   c) Bookmarks for table'#13#10'   d) Find in table'#13#10'2. Change a w' +
        'idth of columns and look, as height of titles is automatically c' +
        'hanged'#13#10'3. See hints for very long string in cells'
    end
  end
  object bvFormSaver1: TbvFormSaver
    Enabled = True
    SavePosOnly = False
    Left = 344
    Top = 92
  end
  object Table1: TTable
    TableName = 'custoly.db'
    Left = 176
    Top = 52
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 160
    Top = 124
  end
end
