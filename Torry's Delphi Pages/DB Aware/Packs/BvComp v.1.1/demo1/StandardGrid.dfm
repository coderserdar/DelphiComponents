object StandardGridDemoForm: TStandardGridDemoForm
  Left = 59
  Top = 85
  Width = 639
  Height = 446
  ActiveControl = DBGrid1
  Caption = 'Work with standard DBGrid'
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
    Top = 97
    Width = 631
    Height = 322
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    Caption = 'Panel1'
    TabOrder = 0
    object DBGrid1: TDBGrid
      Left = 5
      Top = 5
      Width = 621
      Height = 312
      Hint = 'Click right button of mouse '#13#10'for advanced functions'
      Align = alClient
      DataSource = DataSource1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
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
    Height = 93
    Align = alTop
    BevelOuter = bvNone
    BevelWidth = 5
    BorderWidth = 5
    TabOrder = 1
    object Label1: TLabel
      Left = 5
      Top = 5
      Width = 333
      Height = 83
      Align = alLeft
      Caption = 
        'Click on right button in mouse and see automatic generated menui' +
        'tems'#13#10' in popup-menus or automatic generated popup-menus for DBG' +
        'rid'#13#10'   a) Grid Saver - Runtime saver for grid options'#13#10'   b) Ta' +
        'ble Saver - Dataset Saver in db,dbf,txt,doc and excel formats'#13#10' ' +
        '  c) Bookmarks for table'#13#10'   d) Find in table'
    end
  end
  object bvFormSaver1: TbvFormSaver
    Enabled = True
    SavePosOnly = False
    Left = 304
    Top = 108
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 160
    Top = 124
  end
  object DBGridSaver1: TDBGridSaver
    ThisGrid = DBGrid1
    Enabled = True
    SaveWidthOnly = False
    Left = 64
    Top = 225
  end
  object DBTableSaver1: TDBTableSaver
    ThisGrid = DBGrid1
    Enabled = True
    Left = 136
    Top = 241
  end
  object bv_Find1: bv_Find
    ThisGrid = DBGrid1
    Left = 216
    Top = 249
  end
  object bvBookMark1: TbvBookMark
    ThisGrid = DBGrid1
    Left = 312
    Top = 201
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    FileName = 'custoly.cds'
    Params = <>
    Left = 208
    Top = 185
  end
end
