object Form1: TForm1
  Left = 183
  Top = 80
  BorderStyle = bsDialog
  Caption = 'Test Array 1.073 25-10-2005'
  ClientHeight = 534
  ClientWidth = 533
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 338
    Width = 53
    Height = 13
    Caption = 'Array UInt8'
  end
  object Label2: TLabel
    Left = 5
    Top = 374
    Width = 59
    Height = 13
    Caption = 'Array UInt16'
  end
  object Label3: TLabel
    Left = 5
    Top = 411
    Width = 51
    Height = 13
    Caption = 'Array Int32'
  end
  object Label4: TLabel
    Left = 5
    Top = 447
    Width = 61
    Height = 13
    Caption = 'Array Double'
  end
  object Label7: TLabel
    Left = 4
    Top = 486
    Width = 71
    Height = 13
    Caption = 'Display manual'
  end
  object Label9: TLabel
    Left = 206
    Top = 294
    Width = 53
    Height = 13
    Caption = 'Blob mode:'
  end
  object Label10: TLabel
    Left = 206
    Top = 319
    Width = 75
    Height = 13
    Caption = 'Blob chunk size'
  end
  object DBEdit1: TDBEdit
    Left = 5
    Top = 351
    Width = 400
    Height = 21
    DataField = 'fArrayUInt8'
    DataSource = DataSource1
    TabOrder = 0
  end
  object DBEdit2: TDBEdit
    Left = 5
    Top = 389
    Width = 400
    Height = 21
    DataField = 'fArrayUInt16'
    DataSource = DataSource1
    TabOrder = 1
  end
  object DBEdit3: TDBEdit
    Left = 5
    Top = 424
    Width = 400
    Height = 21
    DataField = 'fArrayInt32'
    DataSource = DataSource1
    TabOrder = 2
  end
  object DBEdit4: TDBEdit
    Left = 5
    Top = 460
    Width = 400
    Height = 21
    DataField = 'fArrayDouble'
    DataSource = DataSource1
    TabOrder = 3
  end
  object Edit3: TEdit
    Left = 5
    Top = 501
    Width = 400
    Height = 21
    TabOrder = 4
    Text = '1234567890123.99'
  end
  object CheckBox1: TCheckBox
    Left = 361
    Top = 260
    Width = 51
    Height = 17
    Caption = 'Array'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object GroupBox1: TGroupBox
    Left = 416
    Top = 283
    Width = 114
    Height = 148
    Caption = ' Index value '
    TabOrder = 6
    object Label5: TLabel
      Left = 4
      Top = 18
      Width = 22
      Height = 13
      Caption = 'Field'
    end
    object Label6: TLabel
      Left = 4
      Top = 56
      Width = 26
      Height = 13
      Caption = 'Index'
    end
    object ComboBox1: TComboBox
      Left = 4
      Top = 33
      Width = 104
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'fArrayUInt8'
        'fArrayUInt16'
        'fArrayInt32'
        'fArrayDouble')
    end
    object ComboBox2: TComboBox
      Left = 4
      Top = 71
      Width = 104
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        '0'
        '1'
        '2'
        '3'
        '4')
    end
    object Button3: TButton
      Left = 4
      Top = 119
      Width = 104
      Height = 21
      Caption = 'Get Value'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 4
      Top = 96
      Width = 104
      Height = 21
      Caption = 'Set Value'
      TabOrder = 3
      OnClick = Button4Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 416
    Top = 432
    Width = 114
    Height = 91
    Caption = ' CSV string '
    TabOrder = 7
    object Label8: TLabel
      Left = 4
      Top = 18
      Width = 84
      Height = 13
      Caption = 'Exp: 1,2,3,123.55'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Button2: TButton
      Left = 4
      Top = 63
      Width = 104
      Height = 21
      Caption = 'Get array string'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button1: TButton
      Left = 4
      Top = 41
      Width = 104
      Height = 21
      Caption = 'Set array string'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 533
    Height = 247
    ActivePage = TabSheet1
    Align = alTop
    Style = tsFlatButtons
    TabOrder = 8
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'Grid'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 525
        Height = 216
        Align = alClient
        BevelOuter = bvLowered
        TabOrder = 0
        object DBGrid1: TDBGrid
          Left = 1
          Top = 1
          Width = 523
          Height = 214
          Align = alClient
          BorderStyle = bsNone
          DataSource = DataSource1
          Options = [dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Blob'
      ImageIndex = 1
      object btnLoadGeneric: TButton
        Left = 426
        Top = 1
        Width = 99
        Height = 25
        Caption = 'Load from file...'
        TabOrder = 0
        OnClick = btnLoadGenericClick
      end
      object btnSaveGeneric: TButton
        Left = 426
        Top = 32
        Width = 99
        Height = 25
        Caption = 'Save to file...'
        TabOrder = 1
        OnClick = btnSaveGenericClick
      end
      object btnClearGeneric: TButton
        Left = 426
        Top = 64
        Width = 99
        Height = 25
        Caption = 'Clear'
        TabOrder = 2
        OnClick = btnClearGenericClick
      end
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 418
        Height = 146
        Align = alLeft
        BorderStyle = bsNone
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 3
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Memo'
      ImageIndex = 2
      object DBMemo1: TDBMemo
        Left = 0
        Top = 0
        Width = 418
        Height = 146
        Align = alLeft
        BorderStyle = bsNone
        DataField = 'fmemo'
        DataSource = DataSource1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Button5: TButton
        Left = 424
        Top = 0
        Width = 99
        Height = 25
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button5Click
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Graphics'
      ImageIndex = 3
      object DBImage1: TDBImage
        Left = 0
        Top = 0
        Width = 525
        Height = 138
        Align = alClient
        BorderStyle = bsNone
        Center = False
        DataField = 'fgraphics'
        DataSource = DataSource1
        Stretch = True
        TabOrder = 0
      end
    end
  end
  object ComboBox3: TComboBox
    Left = 296
    Top = 287
    Width = 110
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 9
    OnChange = ComboBox3Change
    Items.Strings = (
      'bmDirect'
      'bmAuto'
      'bmInMemory'
      'bmCache')
  end
  object CheckBox2: TCheckBox
    Left = 206
    Top = 260
    Width = 148
    Height = 17
    Caption = 'Blob Auto start transaction'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 3
    Top = 313
    Width = 198
    Height = 17
    Caption = 'Status [Start transaction]'
    Color = clBtnHighlight
    ParentColor = False
    TabOrder = 11
  end
  object Button6: TButton
    Left = 3
    Top = 281
    Width = 65
    Height = 25
    Caption = 'Start'
    TabOrder = 12
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 70
    Top = 281
    Width = 65
    Height = 25
    Caption = 'Commit'
    TabOrder = 13
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 137
    Top = 281
    Width = 65
    Height = 25
    Caption = 'Roolback'
    TabOrder = 14
    OnClick = Button8Click
  end
  object DBNavigator1: TDBNavigator
    Left = 3
    Top = 253
    Width = 200
    Height = 25
    DataSource = DataSource1
    Flat = True
    TabOrder = 15
  end
  object SpinEdit1: TSpinEdit
    Left = 296
    Top = 312
    Width = 110
    Height = 22
    Increment = 524288
    MaxValue = 2147483646
    MinValue = 0
    TabOrder = 16
    Value = 524288
    OnChange = SpinEdit1Change
  end
  object CheckBox4: TCheckBox
    Left = 415
    Top = 260
    Width = 120
    Height = 17
    Caption = 'Blob check modified'
    TabOrder = 17
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 206
    Top = 277
    Width = 77
    Height = 17
    Caption = 'Flip order'
    TabOrder = 18
  end
  object FSClient1: TFSClient
    ClientName = 'CA'
    ServerEngine = FSServer1
    UserName = 'asd'
    Left = 208
    Top = 17
  end
  object FSSession1: TFSSession
    ClientName = 'CA'
    SessionName = 'A'
    Left = 268
    Top = 15
  end
  object FSTable1: TFSTable
    BlobAutoStartTransaction = True
    BlobModifiedError = False
    BlobMode = bmDirect
    CheckTimeout = 0
    DeleteTimeout = 0
    DataBaseName = 'Arr'
    RecLockedBeforeEdit = False
    RecLockedType = tluDatabase
    FieldDefs = <
      item
        Name = 'fAutoInc32'
        DataType = ftAutoInc
      end
      item
        Name = 'fBoolean'
        DataType = ftBoolean
      end
      item
        Name = 'fSingleChar'
        DataType = ftString
        Size = 1
      end
      item
        Name = 'fSingleWideChar'
        DataType = ftString
        Size = 1
      end
      item
        Name = 'fUInt8'
        DataType = ftSmallint
      end
      item
        Name = 'fUInt16'
        DataType = ftWord
      end
      item
        Name = 'fUInt32'
        DataType = ftInteger
      end
      item
        Name = 'fInt8'
        DataType = ftSmallint
      end
      item
        Name = 'fInt16'
        DataType = ftSmallint
      end
      item
        Name = 'fInt32'
        DataType = ftInteger
      end
      item
        Name = 'fInt64'
        DataType = ftLargeint
      end
      item
        Name = 'fSingle'
        DataType = ftFloat
      end
      item
        Name = 'fDouble'
        DataType = ftFloat
      end
      item
        Name = 'fExtended'
        DataType = ftFloat
      end
      item
        Name = 'fCurrency'
        DataType = ftCurrency
      end
      item
        Name = 'fDate'
        DataType = ftDate
      end
      item
        Name = 'fTime'
        DataType = ftTime
      end
      item
        Name = 'fDateTime'
        DataType = ftDateTime
      end
      item
        Name = 'fBlob'
        DataType = ftBlob
      end
      item
        Name = 'fMemo'
        DataType = ftMemo
      end
      item
        Name = 'fGraphics'
        DataType = ftGraphic
      end
      item
        Name = 'fRecVersion'
        DataType = ftLargeint
      end
      item
        Name = 'fArrayUInt8'
        DataType = ftBytes
        Size = 5
      end
      item
        Name = 'fArrayUInt16'
        DataType = ftBytes
        Size = 5
      end
      item
        Name = 'fArrayInt32'
        DataType = ftBytes
        Size = 5
      end
      item
        Name = 'fArrayDouble'
        DataType = ftBytes
        Size = 5
      end
      item
        Name = 'fShortString'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'fAnsiShortString'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'fNullString'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'fNullAnsiString'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'fWideString'
        DataType = ftString
        Size = 20
      end>
    FilterEval = fseServer
    FilterTimeout = 3000
    FlipOrder = False
    BlobChunkSize = 512
    SessionName = 'A'
    SupportRecNo = False
    TableName = 'TestArray'
    Left = 330
    Top = 15
  end
  object DataSource1: TDataSource
    DataSet = FSTable1
    OnStateChange = DataSource1StateChange
    OnDataChange = DataSource1DataChange
    Left = 387
    Top = 19
  end
  object FSServer1: TFSServer
    CollectGarbageEnabled = False
    CollectCloseInactiveTables = False
    CollectClearCache = False
    SecurityEnabled = False
    ClearCachePerCount = 0
    CloseInactiveTablesAfterCommitOrRoolback = False
    Left = 42
    Top = 37
  end
  object FSDatabase1: TFSDatabase
    DataBaseName = 'Arr'
    SessionName = 'A'
    RecLocking = tlOptimisticNoWait
    Left = 155
    Top = 22
  end
  object SaveDialog1: TSaveDialog
    Left = 191
    Top = 81
  end
end
