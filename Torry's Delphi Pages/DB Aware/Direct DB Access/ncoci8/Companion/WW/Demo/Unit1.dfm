object Form1: TForm1
  Left = 310
  Top = 142
  Width = 643
  Height = 352
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 13
  object Label1: TLabel
    Left = 256
    Top = 14
    Width = 57
    Height = 13
    Caption = 'LockMode :'
  end
  object Label2: TLabel
    Left = 408
    Top = 14
    Width = 54
    Height = 13
    Caption = 'LockPoint :'
  end
  object Label3: TLabel
    Left = 408
    Top = 37
    Width = 68
    Height = 13
    Caption = 'UpdateMode :'
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 8
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 0
  end
  object ComboBox1: TComboBox
    Left = 317
    Top = 10
    Width = 87
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnClick = ComboBox1Click
    Items.Strings = (
      'lmPessimistic'
      'lmOptimistic'
      'lmNone')
  end
  object ComboBox2: TComboBox
    Left = 477
    Top = 10
    Width = 87
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnClick = ComboBox2Click
    Items.Strings = (
      'lpImmediate'
      'lpDeferred')
  end
  object CheckBox1: TCheckBox
    Left = 255
    Top = 36
    Width = 146
    Height = 17
    Alignment = taLeftJustify
    Caption = 'UpdateChangedFields :'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object ComboBox3: TComboBox
    Left = 477
    Top = 34
    Width = 87
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnClick = ComboBox3Click
    Items.Strings = (
      'upWhereAll'
      'upWhereChanged'
      'upWhereKeyOnly')
  end
  object CheckBox2: TCheckBox
    Left = 255
    Top = 59
    Width = 105
    Height = 17
    Alignment = taLeftJustify
    Caption = 'CachedUpdates :'
    TabOrder = 5
    OnClick = CheckBox2Click
  end
  object Button1: TButton
    Left = 384
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 464
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Start TX'
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 88
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Commit TX'
    TabOrder = 9
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 168
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Rollback TX'
    TabOrder = 10
    OnClick = Button5Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 301
    Width = 635
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object wwDBGrid1: TwwDBGrid
    Left = 0
    Top = 112
    Width = 635
    Height = 189
    IniAttributes.Delimiter = ';;'
    TitleColor = clBtnFace
    FixedCols = 0
    ShowHorzScrollBar = True
    Align = alBottom
    DataSource = DataSource1
    TabOrder = 12
    TitleAlignment = taLeftJustify
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    TitleLines = 1
    TitleButtons = True
  end
  object OCIDatabase1: TOCIDatabase
    UserName = 'scott'
    Password = 'tiger'
    Connected = True
    LoginPrompt = False
    AutoCommit = False
    SQLMonitor = NCSQLMonitorClient1
    Left = 16
    Top = 8
  end
  object OCIQuery1: TOCIQuery
    Active = True
    Prepared = True
    Constraints = <
      item
        CustomConstraint = 'EMPNO <> MGR'
        ErrorMessage = 'Employee can'#39't be self manager'
        FromDictionary = False
      end
      item
        CustomConstraint = 'SAL > COMM'
        ErrorMessage = 'SAL must be greater than COMM'
        FromDictionary = False
      end>
    UpdateObject = OCIUpdateSQL1
    SQL.Strings = (
      'select t.*, t.rowid from emp t')
    Left = 48
    Top = 8
    object OCIQuery1ENAME: TStringField
      CustomConstraint = 'upper(ENAME) = ENAME'
      ConstraintErrorMessage = 'Please enter ENAME in UPPER CASE !'
      FieldName = 'ENAME'
      Origin = 'ENAME'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Size = 10
    end
    object OCIQuery1EMPNO: TFloatField
      FieldName = 'EMPNO'
      Origin = 'EMPNO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object OCIQuery1JOB: TStringField
      FieldName = 'JOB'
      Origin = 'JOB'
      Size = 9
    end
    object OCIQuery1MGR: TFloatField
      FieldName = 'MGR'
      Origin = 'MGR'
    end
    object OCIQuery1HIREDATE: TDateTimeField
      DefaultExpression = 'FIRST DAY OF THIS MONTH'
      FieldName = 'HIREDATE'
      Origin = 'HIREDATE'
    end
    object OCIQuery1SAL: TFloatField
      FieldName = 'SAL'
      Origin = 'SAL'
    end
    object OCIQuery1COMM: TFloatField
      FieldName = 'COMM'
      Origin = 'COMM'
    end
    object OCIQuery1Total: TFloatField
      DefaultExpression = 'NVL(SAL,0) + NVL(COMM,0)'
      FieldKind = fkCalculated
      FieldName = 'Total'
      Calculated = True
    end
    object OCIQuery1DEPTNO: TFloatField
      FieldName = 'DEPTNO'
      Origin = 'DEPTNO'
    end
    object OCIQuery1ROWID: TStringField
      FieldName = 'ROWID'
      Origin = 'ROWID'
      Size = 18
    end
  end
  object DataSource1: TDataSource
    DataSet = OCIQuery1
    OnDataChange = DataSource1DataChange
    Left = 80
    Top = 8
  end
  object OCIUpdateSQL1: TOCIUpdateSQL
    Left = 48
    Top = 40
  end
  object NCSQLMonitorClient1: TNCSQLMonitorClient
    Active = True
    ClientObjName = 'Default'
    Left = 16
    Top = 40
  end
end
