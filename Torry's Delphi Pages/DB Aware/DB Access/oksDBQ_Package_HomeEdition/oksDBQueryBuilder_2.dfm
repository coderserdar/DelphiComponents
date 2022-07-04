object Form_QueryBuilder: TForm_QueryBuilder
  Left = 243
  Top = 138
  Width = 779
  Height = 662
  Caption = 'oksDBQueryBuilder Home Edition'
  Color = 15987699
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF007723
    300000330000FFFF323333333300222277338BBBB830FFFFF23B88BBB833FFFF
    F333B877BBB322227333B7777B832222F3F3B7777B30FFFFF23337777300FFFF
    F2BBB777700033332BBBB77770000022B22BB377000002B2B2B2B33300000288
    82288B330000028FF28288B300000022722BBB300000000033333300000007CF
    0000000300000001000000000000000000000000000000010000000300000007
    000000070000C00F0000800F0000800F0000800F0000C01F0000F03F0000}
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel_Header: TPanel
    Left = 0
    Top = 0
    Width = 771
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    DesignSize = (
      771
      73)
    object Image1: TImage
      Left = 735
      Top = 8
      Width = 24
      Height = 24
      Anchors = [akTop]
      AutoSize = True
      Picture.Data = {
        07544269746D617076060000424D760600000000000036040000280000001800
        000018000000010008000000000040020000880B0000880B0000000100000000
        000002580400025804000259040002590400035A0500035A0500045D07000664
        0B000A70120011892000169828001AA630001CAB34001EAE370020B23B0023B5
        3F0025B9430027BC470029BE4A002BC14E002DC351002FC5550032C8590034CB
        5D0035CD5F0037CE620039CF65003CCF68003FCE6B0043CB6F0048C6720050BD
        78005CB082006C9F8F008683A200B84DC900E41DEA00F807F900FE01FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FD01FD00FC02
        FC00FA03FA00F805F800F508F500F00CF000E912E900E118E100D722D700CA2E
        CA00B93EB900A352A300906590007B7B7B007C7C7C007D7D7D007E7E7E007F7F
        7F00808080008181810082828200838383008484840085858500868686008787
        870088888800898989008A8A8A008B8B8B008C8C8C008D8D8D008E8E8E008A91
        940084979D007F9BA4007B9FAA006FAABB0066B1C60060B8D0005BBED80056C7
        E20052CBE8004ED0ED004AD4F20046D8F70043D9F90041DAFB0040DBFD0040DB
        FD0042DEFE0046E1FE0047E3FE004BE3FE0052E3FE0058E4FE005EE5FE0060E7
        FE0062E9FE0065ECFE0067EEFE0068F0FE006AEFFE006DEEFD0073EEFD0078EE
        FD007AEEFD007DF0FD007FF4FD0081F6FD0083F8FD0087F7FD008BF8FD0090F8
        FD0098F9FD009FF9FD00A9FAFE00B0F8FD00BCF6FC00C4F4FA00C4F3FA00BBF0
        F700B7EEF600B3ECF400A7E7F1009CE3EE0092DFEC0086DBE9007BD8E70071D5
        E5006BD4E40065D2E40060D1E3005ED1E3005DD1E3005BD1E3005AD1E30057D1
        E30054D1E40051D1E5004DD0E70040D0EC0035D0F2002DD1F60026D1F90022D1
        FB001FCFFB001CCDFC001ACDFC0018CCFD0016CAFD0013C9FC0011C8FC0011C7
        FC0012C4F80016C0F2001CB9E90022B6E30026B3DE002BAFD8002EAED5002DAC
        D4002BA8D1002AA4CD0029A1CA00269CC5001E95C200148EBE000A89BD000785
        BB000681B700047CB300047AB0000477AC000475A9000374A7000373A7000372
        A500046FA100046D9E00056C9C00056A9B000569980005679700056796000567
        96004A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A4A7A7B0000FAFA
        FA4A4A4A4A4A4A4AFAFAFAFA4A4A4A4A4A4ABFBFBFBFBFBFFA00FEFEFEFAFAFA
        FAECEBF5F9E9EC4A4A4ABDBDBDBDBFBFBF7BE3E397FAEBECEDE2D4ECE6E5ED4A
        4A4A0808080808088F7BDBFAFAE9C9E6E3DCD59AE5C4C5F94A4ABDBDBDBDBDBD
        BF00DBFAE6E6C5C79AE1DFD498C9E2EBED4A0808080808088FFADBFAECE3C9C7
        98E6E9E3DFDFDFE2EC4ABDBDBDBDBDBDBFFADBFAECE498C580888888E6DFDFDF
        F14A0808080808088FFADBFAECEB9A7BBD22882380D4C5C4F34ABDBDBDBDBDBD
        BFFADBFAEDECE37BBD8F8FC022C9C4C7ED4A080808080808BFFAB9BCF9EDE380
        BD228823809A9AFD4A4ABDBDBDBDBDBDBF00F0F3EDF8F380BD8F8FC022F1F14A
        4A4A080808080808BF00E09FA5C9E780BD22228F224A4A4A4A4ABDBDBDBDBDBD
        BD00DB9FA6B1988088228888804A4A4A4A4AFAFAFAFAFA0000DFDB9FA6B1987B
        BF8F888F804A4A4A4A4A4A4AF5CDABA2D7DFDB9FA6B19880BDBDC488804A4A4A
        4A4A4A4A00000000D7000000A6B198E8807B807B4A4A4A4A4A4A4A00F5CD0000
        D700DB9F00B198E8EFF1FE4A4A4A4A4A4A4A4A00F5CDAB00D700DB9F00B198E8
        EFF1FE4A4A4A4A4A4A4A4A00F5C9B800B7000000B4B4AF9AE8F0FE4A4A4A4A4A
        4A4A4A00F5C1BD00BC00B7B400B4B4B4A5E3FE4A4A4A4A4A4A4A4A00F593BD00
        BD00B9B500B4B4B4ACECFE4A4A4A4A4A4A4A4A4A0000009494000000CDCDE6E6
        FAFE4A4A4A4A4A4A4A4A4A4A4A4A4AF5F5F5F5F5F5F5F5F54A4A4A4A4A4A4A4A
        4A4A}
      Transparent = True
    end
    object Label_header1: TLabel
      Left = 8
      Top = 8
      Width = 122
      Height = 16
      Caption = 'oksDBQueryBuilder'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label_header2: TLabel
      Left = 32
      Top = 32
      Width = 227
      Height = 13
      Caption = 'Nur verwendbar in Verbindung mit oksDBEngine'
    end
    object Label_header3: TLabel
      Left = 32
      Top = 48
      Width = 197
      Height = 13
      Caption = 'Use only in conjunction with oksDBEngine'
    end
    object oksDistinct: TCheckBox
      Left = 336
      Top = 16
      Width = 73
      Height = 17
      TabStop = False
      Caption = 'doDistinct'
      TabOrder = 0
      Visible = False
    end
    object oksInto: TCheckBox
      Left = 336
      Top = 32
      Width = 81
      Height = 17
      TabStop = False
      Caption = 'doIntoTable'
      TabOrder = 1
      Visible = False
    end
  end
  object ScrollBox_Main: TScrollBox
    Left = 0
    Top = 102
    Width = 771
    Height = 506
    HorzScrollBar.Smooth = True
    HorzScrollBar.Style = ssFlat
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Style = ssFlat
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 1
    DesignSize = (
      771
      506)
    object Splitter1: TSplitter
      Left = 168
      Top = 0
      Width = 6
      Height = 506
      ResizeStyle = rsUpdate
    end
    object ListTables: TListBox
      Left = 0
      Top = 0
      Width = 168
      Height = 506
      Align = alLeft
      Color = 15987699
      Constraints.MaxWidth = 300
      Constraints.MinWidth = 100
      DragMode = dmAutomatic
      ItemHeight = 13
      TabOrder = 0
    end
    object Panel_Main: TPanel
      Left = 174
      Top = 0
      Width = 597
      Height = 506
      Align = alClient
      BevelOuter = bvNone
      Color = 15987699
      Constraints.MinWidth = 200
      TabOrder = 1
      object Splitter2: TSplitter
        Left = 0
        Top = 257
        Width = 597
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ResizeStyle = rsUpdate
      end
      object Splitter3: TSplitter
        Left = 0
        Top = 381
        Width = 597
        Height = 6
        Cursor = crVSplit
        Align = alTop
        ResizeStyle = rsUpdate
      end
      object ScrollBox_Canvas: TScrollBox
        Left = 0
        Top = 0
        Width = 597
        Height = 257
        HorzScrollBar.Smooth = True
        HorzScrollBar.Style = ssFlat
        HorzScrollBar.Tracking = True
        VertScrollBar.Smooth = True
        VertScrollBar.Style = ssFlat
        VertScrollBar.Tracking = True
        Align = alTop
        Constraints.MinHeight = 100
        Color = 15066597
        ParentColor = False
        TabOrder = 0
      end
      object Conditions: TStringGrid
        Left = 0
        Top = 263
        Width = 597
        Height = 118
        Align = alTop
        ColCount = 14
        Constraints.MinHeight = 100
        DefaultRowHeight = 16
        FixedColor = 15066597
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowMoving, goThumbTracking]
        PopupMenu = Popup_StringGrid
        TabOrder = 1
        OnClick = ConditionsClick
        OnContextPopup = ConditionsContextPopup
        OnDrawCell = ConditionsDrawCell
        OnMouseWheelDown = ConditionsMouseWheelDown
        OnMouseWheelUp = ConditionsMouseWheelUp
        OnRowMoved = ConditionsRowMoved
        OnSelectCell = ConditionsSelectCell
        RowHeights = (
          16
          16)
      end
      object RichEdit_SQL: TRichEdit
        Left = 0
        Top = 387
        Width = 597
        Height = 100
        Align = alClient
        Constraints.MinHeight = 100
        ScrollBars = ssBoth
        TabOrder = 2
        OnChange = RichEdit_SQLChange
      end
      object StatusBar1: TStatusBar
        Left = 0
        Top = 487
        Width = 597
        Height = 19
        Color = 14342099
        Panels = <
          item
            Width = 250
          end
          item
            Width = 150
          end
          item
            Width = 50
          end>
        SizeGrip = False
      end
    end
    object rich_tmp: TRichEdit
      Left = 184
      Top = 225
      Width = 29
      Height = 16
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      PlainText = True
      TabOrder = 2
      Visible = False
      WordWrap = False
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 73
    Width = 771
    Height = 29
    Caption = 'ToolBar1'
    EdgeBorders = [ebLeft, ebTop, ebRight]
    Flat = True
    Images = ImageList_mnu
    TabOrder = 2
    object tBtn_FileClose: TToolButton
      Left = 0
      Top = 0
      Hint = 'oksQueryBuilder schliessen (Close oksQueryBuilder)'
      Caption = 'Schliessen (Close)'
      ImageIndex = 0
      MenuItem = mnu_FileClose
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tBtn_DatabaseOpen: TToolButton
      Left = 31
      Top = 0
      Hint = 'Datenbank '#246'ffnen (Open database)'
      Caption = #214'ffnen (Open)'
      ImageIndex = 2
      MenuItem = mnu_DatabaseOpen
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton5: TToolButton
      Left = 54
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tBtn_SaveSQL: TToolButton
      Left = 62
      Top = 0
      Hint = 'SQL speichern... (Save SQL...)'
      Caption = 'Speichern (Save)'
      ImageIndex = 1
      MenuItem = mnu_SaveSQL
      ParentShowHint = False
      ShowHint = True
    end
  end
  object Datenbank: TOKSDBEDatabase
    DatabaseName = 'DB4QueryBuilder'
    Exclusive = False
    ClientServer = True
    SessionName = 'Default'
    Left = 328
    Top = 128
  end
  object MainMenu1: TMainMenu
    Images = ImageList_mnu
    Left = 208
    Top = 128
    object mnu_File: TMenuItem
      Caption = '&Datei (File)'
      object mnu_FileClose: TMenuItem
        Caption = 'Schliessen (Close)'
        Hint = 'oksQueryBuilder schliessen (Close oksQueryBuilder)'
        ImageIndex = 0
        OnClick = mnu_FileCloseClick
      end
    end
    object mnu_Database: TMenuItem
      Caption = 'Daten&bank (Database)'
      object mnu_DatabaseOpen: TMenuItem
        Caption = #214'ffnen (Open)'
        Hint = 'Datenbank '#246'ffnen (Open database)'
        ImageIndex = 2
        OnClick = mnu_DatabaseOpenClick
      end
    end
    object mnu_SQL: TMenuItem
      Caption = 'SQL'
      object mnu_SaveSQL: TMenuItem
        Caption = 'Speichern (Save)'
        Hint = 'SQL speichern... (Save SQL...)'
        ImageIndex = 1
        OnClick = mnu_SaveSQLClick
      end
    end
    object TMenuItem
      Enabled = False
    end
    object TMenuItem
      Enabled = False
    end
    object mnu_Help: TMenuItem
      Caption = 'Hilfe (Help)'
      object mnu_ShowInfo: TMenuItem
        Caption = 'Info'
        OnClick = mnu_ShowInfoClick
      end
      object mnu_ShowHelp: TMenuItem
        Caption = 'Hilfe (Help)'
        OnClick = mnu_ShowHelpClick
      end
      object N4: TMenuItem
        Caption = '-'
        Visible = False
      end
      object mnu_ShowOrder: TMenuItem
        Caption = 'Bestellen (Order)'
        Enabled = False
        Visible = False
        OnClick = mnu_ShowOrderClick
      end
    end
  end
  object openDatabaseDlg: TOpenDialog
    DefaultExt = 'odb'
    Filter = 'oksDBEngine (*.odb)|*.odb'
    Options = [ofReadOnly, ofHideReadOnly, ofEnableSizing]
    Left = 216
    Top = 216
  end
  object aktTabelle: TOKSDBETable
    DatabaseName = 'DB4QueryBuilder'
    UseTempMemoryProcessing = False
    ReadOnly = False
    Exclusive = False
    Left = 370
    Top = 129
  end
  object TableFields: TOKSDBETable
    DatabaseName = 'Tmp_Mem'
    UseTempMemoryProcessing = True
    ReadOnly = False
    StoreDefs = True
    IndexDefs = <
      item
        Name = 'sortorder'
        Fields = 'sortorder'
      end>
    IndexName = 'sortorder'
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftAutoInc
      end
      item
        Name = 'sortorder'
        DataType = ftAutoInc
      end
      item
        Name = 'Name'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'ObjectID'
        DataType = ftInteger
      end
      item
        Name = 'Type'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Size'
        DataType = ftInteger
      end
      item
        Name = 'Required'
        DataType = ftBoolean
      end
      item
        Name = 'Default'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'MinValue'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'MaxValue'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'BLOBCompressionAlgorithm'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'BLOBCompressionMode'
        DataType = ftInteger
      end
      item
        Name = 'BLOBBlockSize'
        DataType = ftInteger
      end>
    TableName = 'fields'
    Exclusive = False
    Left = 402
    Top = 129
    object TableFieldsid: TAutoIncField
      FieldName = 'id'
    end
    object TableFieldssortorder: TAutoIncField
      FieldName = 'sortorder'
      Visible = False
    end
    object TableFieldsName: TStringField
      DisplayWidth = 40
      FieldName = 'Name'
      Size = 255
    end
    object TableFieldsType: TStringField
      DisplayLabel = 'Typ (Type)'
      FieldName = 'Type'
      Size = 15
    end
    object TableFieldsSize: TIntegerField
      DisplayLabel = 'Gr'#246'sse (Size)'
      FieldName = 'Size'
    end
    object TableFieldsRequired: TBooleanField
      DisplayLabel = 'Erforderlich (Required)'
      FieldName = 'Required'
    end
    object TableFieldsDefault: TStringField
      DisplayLabel = 'Vorgabe (Default)'
      FieldName = 'Default'
      Size = 255
    end
    object TableFieldsMinValue: TStringField
      DisplayLabel = 'min. Wert (min. Value)'
      DisplayWidth = 4
      FieldName = 'MinValue'
      Size = 255
    end
    object TableFieldsMaxValue: TStringField
      DisplayLabel = 'max. Wert (max. Value)'
      DisplayWidth = 4
      FieldName = 'MaxValue'
      Size = 255
    end
    object TableFieldsBLOBCompressionAlgorithm: TStringField
      DisplayWidth = 10
      FieldName = 'BLOBCompressionAlgorithm'
      Visible = False
      Size = 255
    end
    object TableFieldsBLOBCompressionMode: TIntegerField
      DisplayWidth = 4
      FieldName = 'BLOBCompressionMode'
      Visible = False
    end
    object TableFieldsBLOBBlockSize: TIntegerField
      FieldName = 'BLOBBlockSize'
      Visible = False
    end
    object TableFieldsObjectID: TIntegerField
      FieldName = 'ObjectID'
      Visible = False
    end
  end
  object TableIdxCols: TOKSDBETable
    DatabaseName = 'Tmp_Mem'
    UseTempMemoryProcessing = True
    ReadOnly = False
    StoreDefs = True
    IndexDefs = <
      item
        Name = 'indexid'
        Fields = 'indexid;sortorder'
      end>
    IndexName = 'indexid'
    FieldDefs = <
      item
        Name = 'indexid'
        DataType = ftInteger
      end
      item
        Name = 'sortorder'
        DataType = ftAutoInc
      end
      item
        Name = 'ColumnID'
        DataType = ftInteger
      end
      item
        Name = 'CaseInsensitive'
        DataType = ftBoolean
      end
      item
        Name = 'Asc'
        DataType = ftBoolean
      end
      item
        Name = 'MaxIndexedSize'
        DataType = ftInteger
      end>
    TableName = 'IndexColumns'
    Exclusive = False
    MasterDatasetFields = 'id'
    Left = 440
    Top = 128
    object TableIdxColsindexid: TIntegerField
      FieldName = 'indexid'
      Visible = False
    end
    object TableIdxColssortorder: TAutoIncField
      FieldName = 'sortorder'
      Visible = False
    end
    object TableIdxColsColumnID: TIntegerField
      FieldName = 'ColumnID'
      Required = True
    end
    object TableIdxColsCaseInsensitive: TBooleanField
      DisplayLabel = 'Gross-/Kleinschreibung (CaseInsensitive)'
      FieldName = 'CaseInsensitive'
    end
    object TableIdxColsAsc: TBooleanField
      DisplayLabel = 'Aufsteigend (Ascending)'
      FieldName = 'Asc'
    end
    object TableIdxColsMaxIndexedSize: TIntegerField
      FieldName = 'MaxIndexedSize'
      Visible = False
    end
    object TableIdxColsColumnName: TStringField
      DisplayLabel = 'Spaltenname (Columnname)'
      DisplayWidth = 15
      FieldKind = fkLookup
      FieldName = 'ColumnName'
      LookupDataSet = TableFields
      LookupKeyFields = 'id'
      LookupResultField = 'Name'
      KeyFields = 'ColumnID'
      Required = True
      Size = 255
      Lookup = True
    end
  end
  object TableIdxs: TOKSDBETable
    DatabaseName = 'Tmp_Mem'
    UseTempMemoryProcessing = True
    ReadOnly = False
    StoreDefs = True
    IndexDefs = <
      item
        Name = 'sortorder'
        Fields = 'sortorder'
      end>
    IndexName = 'sortorder'
    FieldDefs = <
      item
        Name = 'id'
        DataType = ftAutoInc
      end
      item
        Name = 'sortorder'
        DataType = ftAutoInc
      end
      item
        Name = 'Type'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Name'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'Columns'
        DataType = ftString
        Size = 255
      end>
    TableName = 'Indexes'
    Exclusive = False
    Left = 474
    Top = 129
    object TableIdxsid: TAutoIncField
      FieldName = 'id'
      Visible = False
    end
    object TableIdxssortorder: TAutoIncField
      FieldName = 'sortorder'
      Visible = False
    end
    object TableIdxsType: TStringField
      DisplayLabel = 'Typ (Type)'
      DisplayWidth = 6
      FieldName = 'Type'
    end
    object TableIdxsName: TStringField
      DisplayWidth = 10
      FieldName = 'Name'
      Size = 255
    end
    object TableIdxsColumns: TStringField
      DisplayWidth = 40
      FieldName = 'Columns'
      Size = 255
    end
  end
  object Data_AktTable: TDataSource
    DataSet = aktTabelle
    Left = 376
    Top = 160
  end
  object Data_TableFields: TDataSource
    DataSet = TableFields
    Left = 408
    Top = 160
  end
  object Data_TableIdxCols: TDataSource
    DataSet = TableIdxCols
    Left = 440
    Top = 160
  end
  object Data_TableIdxs: TDataSource
    DataSet = TableIdxs
    Left = 472
    Top = 160
  end
  object MainQuery: TOKSDBEQuery
    DatabaseName = 'db'
    UseTempMemoryProcessing = False
    ReadOnly = False
    Left = 514
    Top = 129
  end
  object Data_MainQuery: TDataSource
    DataSet = MainQuery
    Left = 512
    Top = 160
  end
  object Popup_Joins: TPopupMenu
    Left = 212
    Top = 164
    object mnu_Inner: TMenuItem
      Tag = 1
      Caption = 'INNER JOIN'
      GroupIndex = 1
      OnClick = mnu_CrossClick
    end
    object N2: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnu_Outer: TMenuItem
      Tag = 4
      Caption = 'FULL OUTER JOIN'
      GroupIndex = 1
      OnClick = mnu_CrossClick
    end
    object mnu_LeftOuter: TMenuItem
      Tag = 2
      Caption = 'LEFT OUTER JOIN'
      GroupIndex = 1
      OnClick = mnu_CrossClick
    end
    object mnu_RightOuter: TMenuItem
      Tag = 3
      Caption = 'RIGHT OUTER JOIN'
      GroupIndex = 1
      OnClick = mnu_CrossClick
    end
    object N3: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnu_Cross: TMenuItem
      Caption = 'CROSS JOIN'
      GroupIndex = 1
      OnClick = mnu_CrossClick
    end
    object N1: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mnu_ClearJoin: TMenuItem
      Tag = 5
      Caption = 'Join entfernen (Clear join)'
      GroupIndex = 1
      OnClick = mnu_CrossClick
    end
  end
  object Popup_StringGrid: TPopupMenu
    Left = 280
    Top = 163
    object Grid_DeleteField: TMenuItem
      Caption = 'Feld aus Liste l'#246'schen (Delete field from list)'
    end
    object Grid_DeleteAllFields: TMenuItem
      Caption = 'alle Felder aus Liste l'#246'schen (Delete all fields from list)'
    end
  end
  object Popup_Table: TPopupMenu
    Left = 249
    Top = 163
    object mnu_RemoveSelTable: TMenuItem
      Caption = 'Ausgew'#228'hlte Tabelle entfernen (Remove selected table)'
      OnClick = mnu_RemoveSelTableClick
    end
    object mnu_RemoveAllTables: TMenuItem
      Caption = 'Alle Tabellen entfernen (Remove all tables)'
      OnClick = mnu_RemoveAllTablesClick
    end
    object MenuItem1: TMenuItem
      Caption = '-'
    end
    object mnu_addCalcField: TMenuItem
      Caption = 'Kalk. Feld hinzuf'#252'gen (Add calc field)'
      OnClick = mnu_addCalcFieldClick
    end
    object mnu_EditCalcField: TMenuItem
      Caption = 'Kalk. Feld bearbeiten (Edit calc field)'
      OnClick = mnu_EditCalcFieldClick
    end
    object mnu_DeleteCalcField: TMenuItem
      Caption = 'Kalk. Feld l'#246'schen (Delete calc field)'
      OnClick = mnu_DeleteCalcFieldClick
    end
  end
  object saveSQLdlg: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'Textdatei (*.sql)|*.sql|Textdatei (*.txt)|*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'SQL speichern (Save SQL)'
    Left = 216
    Top = 248
  end
  object ImageList_mnu: TImageList
    Left = 254
    Top = 222
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F8F8F8FFEEEEEEFFE0E0
      E0FFDEDEDEFFDDDDDDFFDDDDDDFFDDDDDDFFDDDDDDFFDDDDDDFFD8D8D8FFD8D8
      D8FFDBDBDBFFDBDBDBFFE4E4E4FFFBFBFBFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCFCFCFFF7F7F7FFF7F7
      F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7
      F7FFF7F7F7FFF7F7F7FFFCFCFCFF0000000000000000E8E8E8FFD96A84FFC6AA
      B2FFCB96A3FFD79CABFFE1A1B0FFE0D0D5FFDDCCD0FFD9C6CBFFD55D79FFD55D
      79FFAF3F5AFFD6617CFFEDB8C5FFE8E8E8FF00000000DEDEDEFFBB5B72FFA089
      91FFA47985FFAE7E8BFFB6828FFFB5A8ADFFB3A5A9FFB0A0A5FFAC4B63FFAC4B
      63FF8D334AFFBD556EFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F0F0F0FFDBDBDBFFDBDBDBFFDBDB
      DBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDB
      DBFFDBDBDBFFDBDBDBFFDBDBDBFFF0F0F0FFF0F0F0FFDC7890FFD6607CFFC9AF
      B6FFD0627CFFCD7388FFD5A3B0FFEEE4E8FFDFCED3FFDBC9CEFFD35371FFD353
      71FFAA3350FFD35371FFD7647FFFE1E1E1FFE9E9E9FF08A7D7FF00A3D2FF00A3
      D2FF009DCAFF009DCAFF0097C8FF0097C8FF0097C8FF008FBEFF008FBEFF008F
      BEFF008CBAFF01A7D7FFBB5F7AFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DADADAFF38B938FF12B912FF00B9
      00FF00B900FF00B900FF00B900FF00B900FF00B900FF00B900FF00B900FF00B9
      00FF00B900FF12B912FF38B938FFDBDBDBFFD05978FFD35476FFD76480FFCFB7
      C0FFD1647FFFCD7087FFC8909EFFD6C2C9FFEEE4E8FFDFCED3FFD35371FFD353
      71FFAA3350FFD35371FFD55B78FFDDDDDDFF08A6D6FF08B0E3FF2FCEFDFF7BE1
      FFFF5FDBFFFF5FDBFFFF5FDBFFFF5FDBFFFF5FDBFFFF5FDBFFFF5FDBFFFF5FDB
      FFFF03C7FFFF00A6D6FF6E536DFFD6D6D6FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBDBDBFF12B912FF00B500FF00AE
      00FFD4FFE3FF00AE00FF00AE00FF00AE00FF00AE00FF00AE00FF00AE00FF4BD2
      51FFD4FFE3FFD4FFE3FF12B912FFDBDBDBFFC53F64FFCC486AFFD35476FFD2BC
      C3FFD26680FFCF758BFFC68C9BFFC4A8B0FFD6C2C9FFEEE4E8FFD35371FFD353
      71FFAA3350FFD35371FFD55B78FFDDDDDDFF07A5D5FF37CFFCFF08B0E3FF7FE2
      FEFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1
      FFFF1BCCFFFF00C4FCFF2984ABFFC0C5C6FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBDBDBFF00B900FF00B900FF7CE2
      85FFD4FFE3FF00B500FF00B500FF00B500FF00B500FF4BD251FF00B500FF4BD2
      51FFD4FFE3FFD4FFE3FF00B900FFDBDBDBFFB63052FFC2345BFFCC486AFFD7B8
      C1FFD26781FFD2788EFFCC95A3FFC1A3ACFFC4A8B0FFD6C2C9FFD35371FFC33F
      5EFFAA3350FFD35371FFD55B78FFDDDDDDFF07A4D5FF53D8FDFF08A5D5FF76E0
      FEFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1FFFF7BE1
      FFFF2BD0FFFF0FC9FFFF199FCCFF9FB2B7FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBDBDBFF4BD251FFD4FFE3FFD4FF
      E3FF66DB6EFF2AC72DFF4BD251FF00B900FF00B900FFD4FFE3FF66DB6EFF4BD2
      51FFD4FFE3FFD4FFE3FF00B900FFDBDBDBFFBF3558FFB63052FFC2345BFFE792
      A6FFD7B8C1FFD2ABB5FFD099A8FFC9AFB6FFC1A3ACFFCA9EAAFFB83455FFB834
      55FFBB3859FFD35371FFD55B78FFDDDDDDFF07A4D5FF7DE2FFFF07A4D5FF72DF
      FEFFABECFFFF8BE5FFFF8BE5FFFF8BE5FFFF8BE5FFFF8BE5FFFF8BE5FFFF8BE5
      FFFF33D1FFFF2BD0FFFF11A2D0FF8CADB6FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBDBDBFF00B900FF7CE285FF00B9
      00FFD4FFE3FF2AC72DFFD4FFE3FF66DB6EFFD4FFE3FFD4FFE3FFD4FFE3FF8DE8
      97FFD4FFE3FFD4FFE3FF00B900FFDBDBDBFFC1385AFFBF3558FFB63052FFC438
      5EFFCC486AFFD55C79FFDA7089FFD76480FFD45674FFD35371FFD35371FFD353
      71FFD35371FFD35371FFD55B78FFDDDDDDFF07A9DBFF8BE5FFFF07AEE2FF58D6
      FCFFC5F3FFFFADEFFFFFABECFFFFABECFFFF97E8FFFF97E8FFFF97E8FFFF97E8
      FFFF25CEFFFF4BD7FFFF01A7D7FF61A3B6FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBDBDBFF00B900FF4BD251FFD4FF
      E3FFD4FFE3FFD4FFE3FFD4FFE3FF00B900FF00B900FFD4FFE3FF66DB6EFF4BD2
      51FFD4FFE3FFD4FFE3FF00B900FFDBDBDBFFC33B5DFFC1385AFFDBA8B7FFE8DC
      E2FFE8DDE2FFE8DDE2FFE8DDE2FFE8DDE2FFE8DDE2FFE8DDE2FFE8DDE2FFE8DD
      E2FFE1B2BFFFD35371FFD55B78FFDDDDDDFF07A9DBFF97E8FFFF4ED6FDFF09B5
      E7FF97E8FFFFC7F2FFFFC7F2FFFFC5F2FFFFC5F2FFFFAFEDFFFFAFEDFFFFAFED
      FFFF2BD0FFFF97E8FFFF29CFFFFF1596BBFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBDBDBFF00B900FF09B909FF09B9
      09FF66DB6EFFD4FFE3FF66DB6EFF00B900FF00B900FF4BD251FF00B900FF4BD2
      51FFD4FFE3FFD4FFE3FF00B900FFDBDBDBFFCB4767FFC33B5DFFE8DDE2FFF0E8
      ECFFF0E9ECFFF3EDEFFFF3EDEFFFF3EDEFFFF3EDEFFFF3EDEFFFF3EDEFFFF3ED
      EFFFE8DEE3FFD35371FFD55B78FFDDDDDDFF08AADBFFA7EDFFFF8BE5FEFF09AB
      DBFF41D5FFFF71DFFFFFB7EFFFFFB7EFFFFFB7EFFFFFB7EFFFFFB7EFFFFFB7EF
      FFFF2FD1FFFF7FE2FFFF33D1FFFF01A8D8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBDBDBFF12B912FF44B944FF44B9
      44FFD4FFE3FFD4FFE3FFD4FFE3FF22B922FF22B922FF22B922FF22B922FF4BD2
      51FFD4FFE3FFD4FFE3FF12B912FFDBDBDBFFD35371FFCB4767FFEFE9ECFFE193
      A5FFD96F88FFD96E88FFD86E87FFD86E87FFD86E87FFD86E87FFD86E87FFDE8F
      A3FFE8DEE3FFD35371FFD55B78FFDDDDDDFF08AADCFFC7F4FFFFBFF1FEFF85E4
      FFFF00BCF2FF00B7ECFF00B7ECFF00B6EAFF00B6EAFF00B1E4FF00B1E4FF00B1
      E4FF00A6D6FF00A6D6FF00A6D6FF9CCDDCFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DBDBDBFF44B944FF12B912FF00B9
      00FF66DB6EFFD4FFE3FF66DB6EFF00B900FF00B900FF00B900FF00B900FF00B9
      00FF00B900FF0EB10EFF38B938FFDBDBDBFFD35371FFD35371FFEFE9ECFFF3EE
      F0FFF1EBEFFFF1EAEEFFEFE8EBFFEFE8EBFFEFE8EBFFEFE8EBFFEFE8EBFFEFE8
      EBFFE8DEE3FFD35371FFD55B78FFDDDDDDFF08AADCFF81E3FFFFE3F9FFFFE3F9
      FFFFD3F6FFFFD3F6FFFFD3F6FFFFD3F6FFFFD3F6FFFFD3F6FFFFD3F6FFFFC5F3
      FFFF00A9DAFF9B3C54FFC1526DFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F0F0F0FFDBDBDBFFDBDBDBFFDBDB
      DBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDBDBFFDBDB
      DBFFDBDBDBFFDBDBDBFFDBDBDBFFF0F0F0FFD35371FFD35371FFEFE9ECFFE092
      A5FFD96F88FFD96E88FFD96E88FFD96E88FFD96E88FFD96E88FFD96E88FFDF90
      A4FFE8DEE3FFD45674FFD55B78FFDEDEDEFFBC4A64FF00AEE0FF89E5FFFFEFFD
      FFFFE3F9FFFFE3F9FFFF81E3FFFF00AEE0FF00AEE0FF00AEE0FF00A9DAFF00A9
      DAFFB4ADB2FFC24E6AFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCFCFCFFF7F7F7FFF7F7
      F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7
      F7FFF7F7F7FFF7F7F7FFFCFCFCFF00000000D35371FFD35371FFEFE9ECFFF3EE
      F0FFF2EDEFFFF2EDEFFFF2EDEFFFF2EDEFFFF2EDEFFFF2EDEFFFF0EAEDFFF0E9
      ECFFE8DEE3FFD76480FFD65E7BFFE9E9E9FF00000000BD4A65FF00AEE0FF00AE
      E0FF00AEE0FF00A9DAFF00A9DAFFD0CCCFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D55D79FFD35371FFEFE9ECFFE092
      A5FFD96F88FFD96F88FFD96F88FFD96F88FFD96F88FFD96F88FFD96F88FFE193
      A5FFE8DEE3FFDA7089FFDA708AFFF7F7F7FF00000000D0526FFFE5DFE2FFD78C
      9EFFD06A82FFD06A82FFD06A82FFD66D86FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EDBAC6FFD55C78FFF0EAEDFFF0EA
      EDFFF0EAEDFFF0EAEDFFF0EAEDFFF0EAEDFFF0EAEDFFF0EAEDFFF0EAEDFFF0EA
      EDFFF0EAEDFFD76580FFF2CFD8FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF8000FFFF00008001800080030000
      0000000000010000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000001000000000000000300008001000080FF0000
      FFFF000080FF0000FFFF0001FFFF000000000000000000000000000000000000
      000000000000}
  end
end
