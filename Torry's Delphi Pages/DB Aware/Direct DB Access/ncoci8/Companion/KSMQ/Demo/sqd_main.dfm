object MainForm: TMainForm
  Left = 285
  Top = 106
  Width = 602
  Height = 467
  Caption = 'Simple Query Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 248
    Width = 594
    Height = 6
    Cursor = crVSplit
    Align = alBottom
  end
  object Toolbar: TPanel
    Left = 0
    Top = 0
    Width = 594
    Height = 33
    Align = alTop
    Caption = ' '
    TabOrder = 0
    object Bevel1: TBevel
      Left = 1
      Top = 1
      Width = 104
      Height = 31
      Align = alLeft
      Shape = bsRightLine
    end
    object Bevel2: TBevel
      Left = 105
      Top = 1
      Width = 188
      Height = 31
      Align = alLeft
      Shape = bsRightLine
    end
    object sbActivateFilter: TSpeedButton
      Left = 232
      Top = 4
      Width = 25
      Height = 25
      Hint = 'Activate query panel'
      AllowAllUp = True
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333FF3FF3333333333CC30003333333333773777333333333C33
        3000333FF33337F33777339933333C3333333377F33337F3333F339933333C33
        33003377333337F33377333333333C333300333F333337F33377339333333C33
        3333337FF3333733333F33993333C33333003377FF33733333773339933C3333
        330033377FF73F33337733339933C33333333FF377F373F3333F993399333C33
        330077F377F337F33377993399333C33330077FF773337F33377399993333C33
        33333777733337F333FF333333333C33300033333333373FF7773333333333CC
        3000333333333377377733333333333333333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbActivateFilterClick
    end
    object sbBuildQuery: TSpeedButton
      Left = 68
      Top = 4
      Width = 25
      Height = 25
      Hint = 'Build Query'
      Flat = True
      Glyph.Data = {
        F6000000424DF60000000000000076000000280000000C000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777087777777
        00007770087777770000777700877777000077770B0877770000770000B08777
        0000770BBFBB087700007770FB00007700007770BFB08777000000000BFB0877
        00000FBFBFBFB087000070FBFB000077000070BFBFB087770000770BFBFB0877
        0000770FFFBFF08700007770FBFFBF0800007770000000000000}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbBuildQueryClick
    end
    object sbOpenFilter: TSpeedButton
      Left = 8
      Top = 4
      Width = 25
      Height = 25
      Hint = 'Open Query'
      Flat = True
      Glyph.Data = {
        06020000424D0602000000000000760000002800000028000000140000000100
        0400000000009001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        333FFFFFFFFFFFFFF3333380000000000000333333888888888888883F333300
        7B7B7B7B7B7B033333883F33333333338F33330F07B7B7B7B7B70333338F8F33
        3333333383F3330B0B7B7B7B7B7B7033338F83F33333333338F3330FB0B7B7B7
        B7B7B033338F38F333333333383F330BF07B7B7B7B7B7B03338F383FFFFF3333
        338F330FBF000007B7B7B703338F33888883FFFFFF83330BFBFBFBF000000033
        338F3333333888888833330FBFBFBFBFBFB03333338F333333333338F333330B
        FBFBFBFBFBF03333338F33333FFFFFF83333330FBFBF0000000333333387FFFF
        8888888333333330000033333333333333388888333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        33333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbOpenFilterClick
    end
    object sbSaveFilter: TSpeedButton
      Left = 38
      Top = 4
      Width = 25
      Height = 25
      Hint = 'Save Query'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333FFFFFFFFFFFFF33000077777770033377777777777773F000007888888
        00037F3337F3FF37F37F00000780088800037F3337F77F37F37F000007800888
        00037F3337F77FF7F37F00000788888800037F3337777777337F000000000000
        00037F3FFFFFFFFFFF7F00000000000000037F77777777777F7F000FFFFFFFFF
        00037F7F333333337F7F000FFFFFFFFF00037F7F333333337F7F000FFFFFFFFF
        00037F7F333333337F7F000FFFFFFFFF00037F7F333333337F7F000FFFFFFFFF
        00037F7F333333337F7F000FFFFFFFFF07037F7F33333333777F000FFFFFFFFF
        0003737FFFFFFFFF7F7330099999999900333777777777777733}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbSaveFilterClick
    end
    object sbSQL: TSpeedButton
      Left = 262
      Top = 4
      Width = 25
      Height = 25
      Hint = 'Show result'
      AllowAllUp = True
      GroupIndex = 2
      Down = True
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8888888888888888888888888888888888888888888888888888888888888888
        8888888888888888888888888888448888888888888877888888844888444888
        4444877888777888777748448448448844887877877877887788884484484488
        4488887787787788778884448448448844888777877877887788444884484488
        4488777887787788778844888448448844887788877877887788448484484488
        4488778787787788778884488844488844888778887778887788888888888888
        8888888888888888888888888888888888888888888888888888888888888888
        8888888888888888888888888888888888888888888888888888}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbSQLClick
    end
  end
  object pcSQL: TPageControl
    Left = 0
    Top = 254
    Width = 594
    Height = 167
    ActivePage = tshResult
    Align = alBottom
    TabOrder = 1
    object tshSQL: TTabSheet
      Caption = 'SQL'
      object memSQL: TMemo
        Left = 0
        Top = 0
        Width = 570
        Height = 139
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tshResult: TTabSheet
      Caption = 'Query result'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 586
        Height = 139
        Align = alClient
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 33
    Width = 594
    Height = 24
    Align = alTop
    TabOrder = 2
    object ddlRFields: TDropDownLabel
      Left = 8
      Top = 6
      Width = 4
      Height = 13
      Cursor = crHandPoint
      Caption = '*'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      Transparent = True
      DropDownType = ddtCustom
      OnCustomDropDown = ddlRFieldsCustomDropDown
    end
  end
  object GroupBox1: TGroupBox
    Left = 391
    Top = 57
    Width = 203
    Height = 191
    Align = alRight
    Caption = ' Description '
    TabOrder = 3
    object mDesc: TMemo
      Left = 2
      Top = 15
      Width = 199
      Height = 174
      Align = alClient
      Color = 16776176
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object KQueryPanel1: TKQueryPanel
    Left = 0
    Top = 57
    Width = 391
    Height = 191
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    TabStop = True
    DbStruct = KDBStructure1
    MainTableName = 'CUSTOMER'
    Titles.Strings = (
      'Choose records where @ of the following apply'
      '@ of the following apply'
      'all'
      'any'
      'none'
      'not all'
      'Add &Bracket'
      'Add &Condition'
      '&Delete Current Row'
      ' &Available fields'
      'Result fields'
      'Create result field'
      'Delete result field'
      'Sorting type'
      'Not Sorted'
      'Ascending'
      'Descending'
      'Sorted'
      'Rename'
      'Delete'
      'Root level'
      'All')
    Options = [poFlatButtons, poRoundButtons, poNonEmptyBrackets, poNewJoinsStyle, poAsciiRFields, poPopupField, poPopupValue, poSelectDistinct, poAllowKeyboard]
    ViewOptions.ActiveRowColor = cl3DLight
    ViewOptions.ControlBorderColor = clBlack
    OnActive = KQueryPanel1Active
    OnCustomEdit = KQueryPanel1CustomEdit
    OnChangeValue = KQueryPanel1ChangeValue
    RFields = (
      '[RFields]'
      'Count=0'
      '')
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 66
    object File1: TMenuItem
      Caption = 'File'
      object Savestructure1: TMenuItem
        Caption = 'Edit structure'
        OnClick = sbEditStructureClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Filter1: TMenuItem
      Caption = 'Query'
      object miActiveFilter: TMenuItem
        Caption = 'Active'
        OnClick = sbActivateFilterClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object N1: TMenuItem
        Caption = 'Open Query'
        OnClick = sbOpenFilterClick
      end
      object Savefilter1: TMenuItem
        Caption = 'Save Query'
        OnClick = sbSaveFilterClick
      end
      object Editresultfields1: TMenuItem
        Caption = 'Edit result fields'
        OnClick = sbEditRFieldsClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object BuildQuery1: TMenuItem
        Caption = 'Build Query'
        OnClick = sbBuildQueryClick
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'qry'
    Filter = 'Query files|*.qry'
    Left = 8
    Top = 98
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'qry'
    Filter = 'Query files (*.qry)|*.qry'
    Left = 40
    Top = 98
  end
  object DataSource1: TDataSource
    DataSet = OCIQuery1
    Left = 148
    Top = 293
  end
  object KDBStructure1: TKDBStructure
    DatabaseName = 'Default'
    SQLFormats.Date = 'dd.mm.yyyy'
    SQLFormats.Time = 'hh:nn:ss'
    SQLFormats.DateTime = 'dd.mm.yyyy hh:nn:ss'
    DatabaseInfo = OciKSMQInfo1
    Left = 8
    Top = 66
    Struct = (
      '[Main]'
      'Version=2.01'
      'DatabaseName=Default'
      'OperatCount=13'
      'TableCount=6'
      'FieldCount=12'
      'FieldGroups=3'
      'DefaultMainTable=CUSTOMER'
      ' '
      '[Operator:1]'
      'Name=EqualTo'
      'DisplayName=is equal to'
      'SQLName=@f = '#39'@1'#39
      'ValFormat='
      'ExprType=SameAsField'
      
        'AppliedTypes=ftString,ftSmallint,ftInteger,ftWord,ftBoolean,ftFl' +
        'oat,ftCurrency,ftBCD,ftDate,ftTime,ftDateTime,ftAutoInc,'
      'ValueIsList=0'
      ' '
      '[Operator:2]'
      'Name=NotEqual'
      'DisplayName=is not equal to'
      'SQLName=@f <> '#39'@1'#39
      'ValFormat='
      'ExprType=SameAsField'
      
        'AppliedTypes=ftString,ftSmallint,ftInteger,ftWord,ftBoolean,ftFl' +
        'oat,ftCurrency,ftBCD,ftDate,ftTime,ftDateTime,ftAutoInc,'
      'ValueIsList=0'
      ' '
      '[Operator:3]'
      'Name=LessThan'
      'DisplayName=is less than'
      'SQLName=@f < '#39'@1'#39
      'ValFormat='
      'ExprType=SameAsField'
      
        'AppliedTypes=ftString,ftSmallint,ftInteger,ftWord,ftBoolean,ftFl' +
        'oat,ftCurrency,ftBCD,ftDate,ftTime,ftDateTime,ftAutoInc,'
      'ValueIsList=0'
      ' '
      '[Operator:4]'
      'Name=LessOrEqual'
      'DisplayName=is less than or equal to'
      'SQLName=@f <= '#39'@1'#39
      'ValFormat='
      'ExprType=SameAsField'
      
        'AppliedTypes=ftString,ftSmallint,ftInteger,ftWord,ftBoolean,ftFl' +
        'oat,ftCurrency,ftBCD,ftDate,ftTime,ftDateTime,ftAutoInc,'
      'ValueIsList=0'
      ' '
      '[Operator:5]'
      'Name=GreaterThan'
      'DisplayName=greater than'
      'SQLName=@f >'#39'@1'#39
      'ValFormat='
      'ExprType=SameAsField'
      
        'AppliedTypes=ftString,ftSmallint,ftInteger,ftWord,ftBoolean,ftFl' +
        'oat,ftCurrency,ftBCD,ftDate,ftTime,ftDateTime,ftAutoInc,'
      'ValueIsList=0'
      ' '
      '[Operator:6]'
      'Name=GreaterOrEqual'
      'DisplayName=greater than or equal to'
      'SQLName=@f >= '#39'@1'#39
      'ValFormat='
      'ExprType=SameAsField'
      
        'AppliedTypes=ftString,ftSmallint,ftInteger,ftWord,ftBoolean,ftFl' +
        'oat,ftCurrency,ftBCD,ftDate,ftTime,ftDateTime,ftAutoInc,'
      'ValueIsList=0'
      ' '
      '[Operator:7]'
      'Name=IsNull'
      'DisplayName=is null'
      'SQLName=@f is null'
      'ValFormat='
      'ExprType=SameAsField'
      
        'AppliedTypes=ftString,ftSmallint,ftInteger,ftWord,ftBoolean,ftFl' +
        'oat,ftCurrency,ftBCD,ftDate,ftTime,ftDateTime,ftAutoInc,ftMemo,f' +
        'tFmtMemo,'
      'ValueIsList=0'
      ' '
      '[Operator:8]'
      'Name=InList'
      'DisplayName=is in list'
      'SQLName=@f in (@1)'
      'ValFormat='
      'ExprType=SameAsField'
      
        'AppliedTypes=ftString,ftSmallint,ftInteger,ftWord,ftBoolean,ftFl' +
        'oat,ftCurrency,ftBCD,ftDate,ftTime,ftDateTime,ftAutoInc,'
      'ValueIsList=0'
      ' '
      '[Operator:9]'
      'Name=StartingWith'
      'DisplayName=is starting with'
      'SQLName=@f like '#39'@1%'#39
      'ValFormat='
      'ExprType=SameAsField'
      'AppliedTypes=ftString,ftMemo,ftFmtMemo,'
      'ValueIsList=0'
      ' '
      '[Operator:10]'
      'Name=NotStartingWith'
      'DisplayName=is not starting with'
      'SQLName=not(@f like '#39'@1%'#39')'
      'ValFormat='
      'ExprType=SameAsField'
      'AppliedTypes=ftString,ftMemo,ftFmtMemo,'
      'ValueIsList=0'
      ' '
      '[Operator:11]'
      'Name=Contains'
      'DisplayName=contains'
      'SQLName=@f like '#39'%@1%'#39
      'ValFormat='
      'ExprType=SameAsField'
      'AppliedTypes=ftString,ftMemo,ftFmtMemo,'
      'ValueIsList=0'
      ' '
      '[Operator:12]'
      'Name=NotContains'
      'DisplayName=not contains'
      'SQLName=not(@f like '#39'%@1%'#39')'
      'ValFormat='
      'ExprType=SameAsField'
      'AppliedTypes=ftString,ftMemo,ftFmtMemo,'
      'ValueIsList=0'
      ' '
      '[Operator:13]'
      'Name=Between'
      'DisplayName=is between'
      'SQLName=@f between '#39'@1'#39' and '#39'@2'#39
      'ValFormat=and'
      'ExprType=SameAsField'
      
        'AppliedTypes=ftString,ftSmallint,ftInteger,ftWord,ftBoolean,ftFl' +
        'oat,ftCurrency,ftBCD,ftDate,ftTime,ftDateTime,ftAutoInc,'
      'ValueIsList=0'
      ' '
      '[Table:1]'
      'Name=CUSTOMER'
      'DisplayName=CUSTOMER'
      'Alias=C'
      'Links=O'
      ' '
      '[Link:C-O]'
      'FldName1=CustNo'
      'FldName2=CustNo'
      'QuoteFields=0'
      'LinkType=Left'
      ' '
      '[Table:2]'
      'Name=EMPLOYEE'
      'DisplayName=EMPLOYEE'
      'Alias=E'
      'Links=O'
      ' '
      '[Link:E-O]'
      'FldName1=EmpNo'
      'FldName2=EmpNo'
      'QuoteFields=0'
      'LinkType=Inner'
      ' '
      '[Table:3]'
      'Name=ITEMS'
      'DisplayName=ITEMS'
      'Alias=I'
      'Links=O,P'
      ' '
      '[Link:I-O]'
      'FldName1=OrderNo'
      'FldName2=OrderNo'
      'QuoteFields=0'
      'LinkType=Inner'
      ' '
      '[Link:I-P]'
      'FldName1=PartNo'
      'FldName2=PartNo'
      'QuoteFields=0'
      'LinkType=Inner'
      ' '
      '[Table:4]'
      'Name=ORDERS'
      'DisplayName=ORDERS'
      'Alias=O'
      'Links'
      ' '
      '[Table:5]'
      'Name=VENDORS'
      'DisplayName=VENDORS'
      'Alias=V'
      'Links'
      ' '
      '[Table:6]'
      'Name=PARTS'
      'DisplayName=PARTS'
      'Alias=P'
      'Links=V'
      ' '
      '[Link:P-V]'
      'FldName1=VendorNo'
      'FldName2=VendorNo'
      'QuoteFields=0'
      'LinkType=Inner'
      ' '
      '[Field:1]'
      'ID=1'
      'Kind=kfkData'
      'FieldExpr=Company'
      'DisplayName=Company'
      'TableAlias=C'
      'EditType=0'
      'FieldType=1'
      'FieldSize=30'
      'Quoted=0'
      
        'Operators=EqualTo,NotEqual,StartingWith,NotStartingWith,Contains' +
        ',NotContains'
      'DefaultValue='
      ' '
      '[Field:2]'
      'ID=2'
      'Kind=kfkData'
      'FieldExpr=LastName'
      'DisplayName=EmplLastName'
      'TableAlias=E'
      'EditType=0'
      'FieldType=1'
      'FieldSize=20'
      'Quoted=0'
      
        'Operators=EqualTo,NotEqual,StartingWith,NotStartingWith,Contains' +
        ',NotContains'
      'DefaultValue='
      ' '
      '[Field:3]'
      'ID=3'
      'Kind=kfkData'
      'FieldExpr=FirstName'
      'DisplayName=EmplFirstName'
      'TableAlias=E'
      'EditType=0'
      'FieldType=1'
      'FieldSize=15'
      'Quoted=0'
      
        'Operators=EqualTo,NotEqual,StartingWith,NotStartingWith,Contains' +
        ',NotContains'
      'DefaultValue='
      ' '
      '[Field:4]'
      'ID=4'
      'Kind=kfkData'
      'FieldExpr=Addr1'
      'DisplayName=Addr1'
      'TableAlias=C'
      'EditType=0'
      'FieldType=1'
      'FieldSize=30'
      'Quoted=0'
      
        'Operators=EqualTo,NotEqual,LessThan,LessOrEqual,GreaterThan,Grea' +
        'terOrEqual,IsNull,InList,StartingWith,NotStartingWith,Contains,N' +
        'otContains,Between'
      'DefaultValue='
      ' '
      '[Field:5]'
      'ID=5'
      'Kind=kfkData'
      'FieldExpr=ShipDate'
      'DisplayName=ShipDate'
      'TableAlias=O'
      'EditType=2'
      'FieldType=11'
      'FieldSize=0'
      'Quoted=0'
      
        'Operators=EqualTo,NotEqual,GreaterOrEqual,LessOrEqual,GreaterTha' +
        'n,LessThan,Between'
      ' '
      '[Field:6]'
      'ID=6'
      'Kind=kfkData'
      'FieldExpr=SaleDate'
      'DisplayName=SaleDate'
      'TableAlias=O'
      'EditType=2'
      'FieldType=11'
      'FieldSize=0'
      'Quoted=0'
      
        'Operators=EqualTo,NotEqual,LessThan,LessOrEqual,GreaterThan,Grea' +
        'terOrEqual,Between'
      ' '
      '[Field:7]'
      'ID=7'
      'Kind=kfkData'
      'FieldExpr=Terms'
      'DisplayName=Terms'
      'TableAlias=O'
      'EditType=0'
      'FieldType=1'
      'FieldSize=6'
      'Quoted=0'
      'Operators='
      'DefaultValue='
      ' '
      '[Field:8]'
      'ID=8'
      'Kind=kfkData'
      'FieldExpr=PaymentMethod'
      'DisplayName=PaymentMethod'
      'TableAlias=O'
      'EditType=1'
      'FieldType=1'
      'FieldSize=7'
      'Quoted=0'
      'Operators=EqualTo,NotEqual'
      'Items=Visa,"American Express",COD,Cash,Check,Credit,MC'
      'Values=Visa,AmEx,COD,Cash,Check,Credit,MC'
      ' '
      '[Field:9]'
      'ID=9'
      'Kind=kfkData'
      'FieldExpr=Cost'
      'DisplayName=Cost'
      'TableAlias=P'
      'EditType=0'
      'FieldType=7'
      'FieldSize=0'
      'Quoted=0'
      'Operators='
      'DefaultValue='
      ' '
      '[Field:10]'
      'ID=10'
      'Kind=kfkData'
      'FieldExpr=ListPrice'
      'DisplayName=ListPrice'
      'TableAlias=P'
      'EditType=0'
      'FieldType=7'
      'FieldSize=0'
      'Quoted=0'
      'Operators='
      'DefaultValue='
      ' '
      '[Field:11]'
      'ID=11'
      'Kind=kfkData'
      'FieldExpr=Description'
      'DisplayName=PartDescription'
      'TableAlias=P'
      'EditType=0'
      'FieldType=1'
      'FieldSize=30'
      'Quoted=0'
      'Operators='
      'DefaultValue='
      ' '
      '[Field:12]'
      'ID=12'
      'Kind=kfkData'
      'FieldExpr=VendorName'
      'DisplayName=VendorName'
      'TableAlias=V'
      'EditType=3'
      'FieldType=1'
      'FieldSize=30'
      'Quoted=0'
      
        'Operators=EqualTo,NotEqual,StartingWith,NotStartingWith,Contains' +
        ',NotContains'
      ''
      '[Field:12.SQL]'
      'Select distinct VendorName, VendorName'
      'From Vendors'
      'Order by 1'
      ' '
      '[FieldGroup1]'
      'GroupName=Company'
      'Fields=E.LastName,E.FirstName,C.Addr1,C.Company'
      ' '
      '[FieldGroup2]'
      'GroupName=Parts'
      'Fields=P.Description,P.ListPrice,P.Cost'
      ' '
      '[FieldGroup3]'
      'GroupName=Order'
      'Fields=O.ShipDate,O.SaleDate,O.Terms,O.PaymentMethod')
  end
  object OciKSMQInfo1: TOciKSMQInfo
    DatabaseName = 'Default'
    Left = 74
    Top = 67
  end
  object OCIQuery1: TOCIQuery
    Left = 116
    Top = 294
  end
  object OCIDatabase1: TOCIDatabase
    Left = 106
    Top = 67
  end
end
