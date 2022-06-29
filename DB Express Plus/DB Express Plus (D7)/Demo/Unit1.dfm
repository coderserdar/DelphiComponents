object F_DbExpPlus: TF_DbExpPlus
  Left = 87
  Top = 129
  Width = 815
  Height = 540
  Caption = 'DbExpress Plus Demo'
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 807
    Height = 513
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'SQLScript'
      object m_SQLScript: TMemo
        Left = 116
        Top = 66
        Width = 581
        Height = 252
        ScrollBars = ssVertical
        TabOrder = 0
        OnEnter = m_SQLScriptEnter
      end
      object b_SQLExec: TButton
        Left = 622
        Top = 29
        Width = 75
        Height = 25
        Caption = 'Execute'
        TabOrder = 1
        OnClick = b_SQLExecClick
      end
      object b_SQLScript1: TButton
        Left = 116
        Top = 29
        Width = 75
        Height = 25
        Caption = 'Script 1'
        TabOrder = 2
        OnClick = b_SQLScript1Click
      end
      object b_SQLScript2: TButton
        Left = 210
        Top = 29
        Width = 75
        Height = 25
        Caption = 'Script 2'
        TabOrder = 3
        OnClick = b_SQLScript2Click
      end
      object b_SQLScript3: TButton
        Left = 305
        Top = 29
        Width = 75
        Height = 25
        Caption = 'Reset'
        TabOrder = 4
        OnClick = b_SQLScript3Click
      end
      object DBGrid1: TDBGrid
        Left = 116
        Top = 336
        Width = 581
        Height = 90
        DataSource = DataSource1
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
        TabOrder = 5
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'KEY_FIELD'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'KEY_DESC'
            Visible = True
          end>
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'SQLMetaData'
      ImageIndex = 1
      object l_MDLevel1: TLabel
        Left = 35
        Top = 29
        Width = 80
        Height = 13
        Caption = 'Database Object'
      end
      object l_MDLevel2: TLabel
        Left = 35
        Top = 174
        Width = 61
        Height = 13
        Caption = 'Table Object'
      end
      object l_MDLevel3: TLabel
        Left = 35
        Top = 319
        Width = 56
        Height = 13
        Caption = 'Field Object'
      end
      object b_MDLevel1: TButton
        Left = 208
        Top = 23
        Width = 58
        Height = 25
        Caption = 'Execute'
        TabOrder = 0
        OnClick = b_MDLevel1Click
      end
      object lb_MDLevel1: TListBox
        Left = 289
        Top = 5
        Width = 487
        Height = 148
        ItemHeight = 13
        TabOrder = 1
      end
      object cb_MDLevel1: TComboBox
        Left = 35
        Top = 53
        Width = 231
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = cb_MDLevel1Change
        Items.Strings = (
          'GetTablesNames'
          'GetSysTabelNames'
          'GetViewNames')
      end
      object b_MDLevel2: TButton
        Left = 208
        Top = 168
        Width = 58
        Height = 25
        Caption = 'Execute'
        TabOrder = 3
        OnClick = b_MDLevel2Click
      end
      object cb_MDLevel2: TComboBox
        Left = 35
        Top = 199
        Width = 231
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        Items.Strings = (
          'GetFieldNames'
          'GetPrimaryKeyFieldNames'
          'GetPrimaryKeyFields'
          'GetInsertStatement'
          'GetUpdateStatement'
          'GetSelectStatement'
          'GetIndexNames')
      end
      object lb_MDLevel2: TListBox
        Left = 289
        Top = 159
        Width = 487
        Height = 148
        ItemHeight = 13
        TabOrder = 5
      end
      object cb_MDLevel3: TComboBox
        Left = 35
        Top = 344
        Width = 231
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        Items.Strings = (
          'GetFieldMetaData'
          'GetIndexFieldNames')
      end
      object b_MDLevel3: TButton
        Left = 208
        Top = 313
        Width = 58
        Height = 25
        Caption = 'Execute'
        TabOrder = 7
        OnClick = b_MDLevel3Click
      end
      object lb_MDLevel3: TListBox
        Left = 289
        Top = 314
        Width = 487
        Height = 148
        ItemHeight = 13
        TabOrder = 8
      end
    end
    object SQLDataPump: TTabSheet
      Caption = 'SQLDataPump'
      ImageIndex = 2
      object b_DPCreate: TButton
        Left = 180
        Top = 35
        Width = 131
        Height = 25
        Caption = 'Create Destination Table'
        TabOrder = 0
        OnClick = b_DPCreateClick
      end
      object b_DPDrop: TButton
        Left = 488
        Top = 35
        Width = 131
        Height = 25
        Caption = 'Drop Destination Table'
        TabOrder = 1
        OnClick = b_DPDropClick
      end
      object b_DPDataPump: TButton
        Left = 334
        Top = 35
        Width = 131
        Height = 25
        Caption = 'Data Pump'
        TabOrder = 2
        OnClick = b_DPDataPumpClick
      end
      object DBGrid2: TDBGrid
        Left = 74
        Top = 96
        Width = 657
        Height = 301
        DataSource = DataSource2
        TabOrder = 3
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'EMP_NO'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'FIRST_NAME'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'LAST_NAME'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'PHONE_EXT'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'HIRE_DATE'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'DEPT_NO'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'JOB_CODE'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'JOB_GRADE'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'JOB_COUNTRY'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'SALARY'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'FULL_NAME'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'LITERAL_EXAMPLE'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'EVENT_EXAMPLE'
            Visible = True
          end>
      end
    end
  end
  object SQLMetaData1: TSQLMetaData
    ConnectionName = 'IBLocal'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'BlobSize=-1'
      'CommitRetain=False'
      'Database=c:\program files\firebird\examples\v5\employee.gdb'
      'DriverName=Interbase'
      'LocaleCode=0000'
      'Password=masterkey'
      'RoleName=RoleName'
      'ServerCharSet=ASCII'
      'SQLDialect=1'
      'Interbase TransIsolation=ReadCommited'
      'User_Name=sysdba'
      'WaitOnLocks=True')
    TableScope = [tsTable]
    VendorLib = 'GDS32.DLL'
    Left = 8
    Top = 36
  end
  object SQLScript1: TSQLScript
    CommitEach = False
    Debug = False
    SQLProc = False
    SQLConnection = SQLConnection1
    Left = 8
    Top = 64
  end
  object SQLConnection1: TSQLConnection
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'BlobSize=-1'
      'CommitRetain=False'
      'Database=c:\program files\firebird\examples\v5\employee.gdb'
      'LocaleCode=0000'
      'Password=masterkey'
      'RoleName=RoleName'
      'ServerCharSet=ASCII'
      'SQLDialect=1'
      'Interbase TransIsolation=ReadCommited'
      'User_Name=sysdba'
      'WaitOnLocks=True')
    VendorLib = 'GDS32.DLL'
    Left = 8
    Top = 92
  end
  object DataSource1: TDataSource
    DataSet = SQLClientDataSet1
    Left = 7
    Top = 149
  end
  object SQLClientDataSet1: TSQLClientDataSet
    CommandText = 'SELECT * FROM MYTABLE'
    Aggregates = <>
    Options = [poAllowCommandText]
    ObjectView = True
    Params = <>
    DBConnection = SQLConnection1
    Left = 8
    Top = 120
    object SQLClientDataSet1KEY_FIELD: TStringField
      FieldName = 'KEY_FIELD'
      Size = 12
    end
    object SQLClientDataSet1KEY_DESC: TStringField
      FieldName = 'KEY_DESC'
      Size = 47
    end
  end
  object SQLDataPump1: TSQLDataPump
    AbortOnException = False
    AsciiDataDef = <>
    AsciiDateSeparator = '/'
    AsciiDelimiter = '"'
    AsciiFileName = 'AsciiFile.txt'
    AsciiRecordDelimiter = rdDOS
    AsciiRecordFormat = ffFixedLength
    AsciiSeparator = ','
    ClearDestination = True
    CommitCount = 1
    ConfirmClear = True
    DestinationDateTimeFormat = 'mm/dd/yyyy-hh:mm:ss'
    DestinationFields = <
      item
        FieldName = 'EMP_NO'
        SourceColumn = 'EMP_NO'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'FIRST_NAME'
        SourceColumn = 'FIRST_NAME'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'LAST_NAME'
        SourceColumn = 'LAST_NAME'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'PHONE_EXT'
        SourceColumn = 'PHONE_EXT'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'HIRE_DATE'
        SourceColumn = 'HIRE_DATE'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'DEPT_NO'
        SourceColumn = 'DEPT_NO'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'JOB_CODE'
        SourceColumn = 'JOB_CODE'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'JOB_GRADE'
        SourceColumn = 'JOB_GRADE'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'JOB_COUNTRY'
        SourceColumn = 'JOB_COUNTRY'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'SALARY'
        SourceColumn = 'SALARY'
        SourceValueType = fvtColumn
      end
      item
        FieldName = 'LITERAL_EXAMPLE'
        SourceLiteral = 'Example'
        SourceValueType = fvtLiteral
      end
      item
        FieldName = 'EVENT_EXAMPLE'
        SourceValueType = fvtLiteral
        BeforeFieldPump = SQLDataPump1DestinationFields11BeforeFieldPump
      end>
    DestinationTable = 'EMPLOYEE_DEST'
    DataMoveMode = dmAppendUpdate
    ExceptionFileAction = efaNone
    ExceptionFileName = 'ExceptionLog.txt'
    ShowRunningStatistics = dmsReadWrite
    ShowSummaryStatistic = dmsReadWrite
    SQLDataPumpMode = sdpTableToTable
    SQLMetaDataDestination = md_Destination
    SQLMetaDataSource = md_Source
    SQLSource.Strings = (
      'SELECT'
      '  EMP_NO,'
      '  FIRST_NAME,'
      '  LAST_NAME,'
      '  PHONE_EXT,'
      '  HIRE_DATE,'
      '  DEPT_NO,'
      '  JOB_CODE,'
      '  JOB_GRADE,'
      '  JOB_COUNTRY,'
      '  SALARY'
      'FROM'
      '  EMPLOYEE')
    StatisticsCaption = 'Data Pump Statistics'
    StatisticsInterval = 5
    UseTransaction = True
    Left = 6
    Top = 272
  end
  object md_Destination: TSQLMetaData
    ConnectionName = 'IBLocal'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'BlobSize=-1'
      'CommitRetain=False'
      'Database=c:\program files\firebird\examples\v5\employee.gdb'
      'DriverName=Interbase'
      'LocaleCode=0000'
      'Password=masterkey'
      'RoleName=RoleName'
      'ServerCharSet=ASCII'
      'SQLDialect=1'
      'Interbase TransIsolation=ReadCommited'
      'User_Name=sysdba'
      'WaitOnLocks=True')
    TableScope = [tsTable]
    VendorLib = 'GDS32.DLL'
    Left = 6
    Top = 216
  end
  object md_Source: TSQLMetaData
    ConnectionName = 'IBLocal'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'BlobSize=-1'
      'CommitRetain=False'
      'Database=c:\program files\firebird\examples\v5\employee.gdb'
      'DriverName=Interbase'
      'LocaleCode=0000'
      'Password=masterkey'
      'RoleName=RoleName'
      'ServerCharSet=ASCII'
      'SQLDialect=1'
      'Interbase TransIsolation=ReadCommited'
      'User_Name=sysdba'
      'WaitOnLocks=True')
    TableScope = [tsTable]
    VendorLib = 'GDS32.DLL'
    Left = 6
    Top = 244
  end
  object DataSource2: TDataSource
    DataSet = SQLClientDataSet2
    Left = 8
    Top = 336
  end
  object SQLClientDataSet2: TSQLClientDataSet
    CommandText = 'SELECT * FROM EMPLOYEE_DEST'
    Aggregates = <>
    Options = [poAllowCommandText]
    ObjectView = True
    Params = <>
    DBConnection = SQLConnection1
    Left = 8
    Top = 306
    object SQLClientDataSet2EMP_NO: TSmallintField
      FieldName = 'EMP_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object SQLClientDataSet2FIRST_NAME: TStringField
      FieldName = 'FIRST_NAME'
      Size = 17
    end
    object SQLClientDataSet2LAST_NAME: TStringField
      FieldName = 'LAST_NAME'
      Size = 22
    end
    object SQLClientDataSet2PHONE_EXT: TStringField
      FieldName = 'PHONE_EXT'
      Size = 6
    end
    object SQLClientDataSet2HIRE_DATE: TSQLTimeStampField
      FieldName = 'HIRE_DATE'
    end
    object SQLClientDataSet2DEPT_NO: TStringField
      FieldName = 'DEPT_NO'
      FixedChar = True
      Size = 3
    end
    object SQLClientDataSet2JOB_CODE: TStringField
      FieldName = 'JOB_CODE'
      Size = 7
    end
    object SQLClientDataSet2JOB_GRADE: TSmallintField
      FieldName = 'JOB_GRADE'
    end
    object SQLClientDataSet2JOB_COUNTRY: TStringField
      FieldName = 'JOB_COUNTRY'
      Size = 17
    end
    object SQLClientDataSet2SALARY: TFMTBCDField
      FieldName = 'SALARY'
      Precision = 15
      Size = 2
    end
    object SQLClientDataSet2FULL_NAME: TStringField
      FieldName = 'FULL_NAME'
      Size = 39
    end
    object SQLClientDataSet2LITERAL_EXAMPLE: TStringField
      FieldName = 'LITERAL_EXAMPLE'
      Size = 12
    end
    object SQLClientDataSet2EVENT_EXAMPLE: TStringField
      FieldName = 'EVENT_EXAMPLE'
      Size = 47
    end
  end
end
