object MainForm: TMainForm
  Left = 520
  Top = 214
  Caption = 'KDBGrid demo'
  ClientHeight = 523
  ClientWidth = 646
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  DesignSize = (
    646
    523)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 113
    Height = 13
    Caption = 'ADO Connection string:'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 30
    Height = 13
    Caption = 'Table:'
  end
  object Label3: TLabel
    Left = 184
    Top = 48
    Width = 90
    Height = 13
    Caption = 'First column value:'
  end
  object BUModify: TButton
    Left = 512
    Top = 104
    Width = 123
    Height = 25
    Action = ACModify
    TabOrder = 0
  end
  object EDConnectionString: TEdit
    Left = 8
    Top = 24
    Width = 630
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 
      'Provider=SQLOLEDB.1;Password=npm;Persist Security Info=True;User' +
      ' ID=npm_login;Initial Catalog=data;Data Source=TK-PC\SQLSERVER'
    ExplicitWidth = 627
  end
  object EDTable: TEdit
    Left = 8
    Top = 64
    Width = 156
    Height = 21
    TabOrder = 2
    Text = '[Table]'
  end
  object EDFirstCol: TDBEdit
    Left = 184
    Top = 64
    Width = 121
    Height = 21
    DataSource = DSMain
    TabOrder = 3
  end
  object BUOpen: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Action = ACOpen
    TabOrder = 4
  end
  object BUClose: TButton
    Left = 89
    Top = 104
    Width = 75
    Height = 25
    Action = ACClose
    TabOrder = 5
  end
  object DBNav: TDBNavigator
    Left = 184
    Top = 104
    Width = 240
    Height = 25
    DataSource = DSMain
    TabOrder = 6
  end
  object BUPrint: TButton
    Left = 536
    Top = 73
    Width = 99
    Height = 25
    Action = ACPrint
    TabOrder = 7
  end
  object BUAutoSize: TButton
    Left = 328
    Top = 62
    Width = 89
    Height = 25
    Caption = 'Autosize row'
    TabOrder = 8
    OnClick = BUAutoSizeClick
  end
  object DBGrid: TKDBGrid
    Left = 0
    Top = 144
    Width = 646
    Height = 379
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    DBOptions = [dboAutoMoveRecord, dboAutoSizeBooleanCells, dboColNamesToHeader, dboImageHint, dboIndexFixedCol, dboIndicateActiveRecord]
    ColCount = 7
    DataSource = DSMain
    Options = [goAlwaysShowEditor, goClippedCells, goColMoving, goColSizing, goColSorting, goDrawFocusSelected, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goIndicateHiddenCells, goMouseCanHideCells, goMouseOverCells, goRowSizing, goRowSorting, goTabs, goThemes, goThemedCells, goVertLine]
    RowCount = 2
    TabOrder = 9
    OnCustomSortRows = DBGridCustomSortRows
    OnDrawCell = DBGridDrawCell
    OnEditorCreate = DBGridEditorCreate
    OnMouseClickCell = DBGridMouseClickCell
    ExplicitWidth = 643
    ExplicitHeight = 372
    ColWidths = (
      64
      64
      64
      64
      64
      56
      110)
    RowHeights = (
      21
      21)
  end
  object ALMain: TActionList
    Left = 480
    Top = 96
    object ACOpen: TAction
      Caption = 'Open table'
      OnExecute = ACOpenExecute
      OnUpdate = ACOpenUpdate
    end
    object ACClose: TAction
      Caption = 'Close table'
      OnExecute = ACCloseExecute
      OnUpdate = ACCloseUpdate
    end
    object ACModify: TAction
      Caption = 'Modify random cell'
      OnExecute = ACModifyExecute
      OnUpdate = ACCloseUpdate
    end
    object ACPrint: TAction
      Caption = 'Print...'
      OnExecute = ACPrintExecute
      OnUpdate = ACPrintUpdate
    end
  end
  object DSMain: TDataSource
    Left = 448
    Top = 96
  end
  object PSDMain: TKPrintSetupDialog
    Control = DBGrid
    SelAvail = False
    Left = 448
    Top = 56
  end
end
