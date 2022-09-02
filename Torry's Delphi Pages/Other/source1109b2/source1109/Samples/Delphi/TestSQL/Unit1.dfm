object frmTestSQL: TfrmTestSQL
  Left = 116
  Top = 119
  Width = 657
  Height = 483
  Caption = 'Test SQL'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 129
    Width = 649
    Height = 3
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Color = clBtnHighlight
    ParentColor = False
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 428
    Width = 649
    Height = 21
    Panels = <
      item
        Text = 'Waiting for input...'
        Width = 200
      end
      item
        Text = 'Record count = 0'
        Width = 165
      end
      item
        Text = 'Execution Time = 0 ms'
        Width = 165
      end
      item
        Style = psOwnerDraw
        Text = 'LiveLight'
        Width = 30
      end>
    SimplePanel = False
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 649
    Height = 129
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = pnl1Resize
    object Panel3: TPanel
      Left = 305
      Top = 0
      Width = 344
      Height = 129
      Align = alClient
      BevelOuter = bvLowered
      TabOrder = 0
      object pgc1: TPageControl
        Left = 1
        Top = 1
        Width = 342
        Height = 127
        ActivePage = TabSheet1
        Align = alClient
        Style = tsButtons
        TabOrder = 0
        object TabSheet1: TTabSheet
          Caption = 'Alias or Path'
          ImageIndex = 4
          object ListBox1: TListBox
            Left = 0
            Top = 29
            Width = 334
            Height = 67
            Align = alClient
            BorderStyle = bsNone
            ItemHeight = 13
            Sorted = True
            TabOrder = 0
            OnDblClick = ListBox1DblClick
          end
          object Panel5: TPanel
            Left = 0
            Top = 0
            Width = 334
            Height = 29
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            OnResize = Panel5Resize
            object lbl4: TLabel
              Left = 0
              Top = 0
              Width = 39
              Height = 29
              Align = alLeft
              AutoSize = False
              Caption = 'Alias or Path'
              WordWrap = True
            end
            object cbb1: TComboBox
              Left = 39
              Top = 4
              Width = 295
              Height = 21
              ItemHeight = 13
              TabOrder = 0
              Text = 'Alias or path'
              OnClick = cbb1Click
              OnKeyUp = cbb1KeyUp
            end
          end
        end
        object ts1: TTabSheet
          Caption = 'SQL'
          ImageIndex = 3
          object memSQL: TMemo
            Left = 0
            Top = 23
            Width = 334
            Height = 73
            Align = alClient
            BorderStyle = bsNone
            ScrollBars = ssBoth
            TabOrder = 0
          end
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 334
            Height = 23
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            OnResize = Panel2Resize
            object ComboBox1: TComboBox
              Left = -1
              Top = 0
              Width = 338
              Height = 21
              DropDownCount = 20
              ItemHeight = 13
              TabOrder = 0
              Items.Strings = (
                'TOP'
                'SELECT top 10 * FROM Author'
                'SELECT top 10 percent * FROM Author'
                'SELECT top 10 startposition 5 * FROM Author'
                'SELECT top 100 startposition 1 percent * FROM Author'
                'SELECT top 10 startposition 3 * FROM Author'
                'SELECT top 100 startposition 3 DIVBY 2 * FROM Author'
                'SELECT top 100 startposition 3 DIVBY 2 PERCENT * FROM Author'
                'SELECT top 100 startposition 3 DIVBY 2 ATFIRST * FROM Author'
                
                  'SELECT top 100 startposition 3 DIVBY 2 ATFIRST DISTINCT * FROM A' +
                  'uthor'
                ''
                'DOWN'
                'SELECT down 10 * FROM Author'
                'SELECT down 10 percent * FROM Author'
                'SELECT down 10 startposition 5 * FROM Author'
                'SELECT down 100 startposition 1 percent * FROM Author'
                'SELECT down 10 startposition 3 * FROM Author'
                'SELECT down 100 startposition 3 DIVBY 2 * FROM Author'
                'SELECT down 100 startposition 3 DIVBY 2 PERCENT * FROM Author'
                'SELECT down 100 startposition 3 DIVBY 2 ATFIRST * FROM Author'
                
                  'SELECT down 100 startposition 3 DIVBY 2 ATFIRST DISTINCT * FROM ' +
                  'Author'
                ''
                'TOPDOWN - rows top+down'
                'SELECT topdown 10 * FROM Author'
                'SELECT topdown 10 percent * FROM Author'
                'SELECT topdown 10 startposition 5 * FROM Author'
                'SELECT topdown 100 startposition 1 percent * FROM Author'
                'SELECT topdown 10 startposition 3 * FROM Author'
                'SELECT topdown 100 startposition 3 DIVBY 2 * FROM Author'
                'SELECT topdown 100 startposition 3 DIVBY 2 PERCENT * FROM Author'
                'SELECT topdown 100 startposition 3 DIVBY 2 ATFIRST * FROM Author'
                
                  'SELECT topdown 100 startposition 3 DIVBY 2 ATFIRST DISTINCT * FR' +
                  'OM Author'
                ''
                'TOPCENTERDOWN - rows top+center+down'
                'SELECT topcenterdown 10 * FROM Author'
                ''
                'CENTER - rows center'
                'SELECT center 10 * FROM Author'
                ''
                'ORDERUNION'
                'Order by for UNION'
                ''
                'UNION [ALL]'
                'SELECT top 10 * FROM (SELECT * from A union SELECT * from B)'
                'Union [ALL]'
                
                  'SELECT top 10 * FROM (SELECT * from C union SELECT * from (SELEC' +
                  'T * from D union SELECT * from E))'
                'ORDERUNION BY AA (1, 2)'
                ''
                'COMMITBY'
                'Commit executes if value > 0'
                'DELETE Commitby 1000 from  A'
                'INSERT Commitby 1000 INTO A ......'
                'UPDATE Commitby 1000A set ....'
                ''
                'ONLINE'
                
                  'Return cursor query online. Online it functions for this syntax ' +
                  'only. It'#39's very fast.'
                'SELECT ONLINE * from A'
                'SELECT ONLINE top, topdown, center,topcenterdown * from A'
                ''
                'USEINDEX'
                'Before open table set indexname.'
                'SELECT  * from A USEINDEX  YOURINDEX'
                'SELECT ONLINE  * from A USEINDEX  YOURINDEX'
                'SELECT ONLINE  top 100 * from A USEINDEX  YOURINDEX'
                ''
                'Ex.'
                
                  '----------------------------------------------------------------' +
                  '-------------'
                'SELECT  * from A  USEINDEX  YOURINDEX ..... - speed order by'
                '='
                'SELECT  * from A .....'
                'Order by ..'
                
                  '----------------------------------------------------------------' +
                  '-------------'
                ''
                ''
                'MOD function'
                'MOD(<simple expr>, <simple expr>)'
                'Select Mod(A, 2) from A1'
                'Select * from A1 where Mod(A, 2) = 0'
                ''
                'DIV function'
                'DIV(<simple expr>, <simple expr>)'
                'Select Div(A, 2) from A1'
                'Select * from A1 where Div(A, 2) = 2'
                ''
                'ODD function'
                'ODD(<simple expr>)'
                'Select ODD(A) from A1'
                'Select * from A1 where ODD(A)')
            end
          end
        end
        object ts2: TTabSheet
          Caption = 'Passwords'
          ImageIndex = 1
          object Label3: TLabel
            Left = 0
            Top = 0
            Width = 334
            Height = 21
            Align = alTop
            Alignment = taRightJustify
            AutoSize = False
            Caption = '   You must reopen database [Disconnect, Connect]'
            Color = clYellow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object mempsw: TMemo
            Left = 0
            Top = 21
            Width = 334
            Height = 75
            Align = alClient
            BorderStyle = bsNone
            Lines.Strings = (
              'Passwords here 1'
              'Passwords here 2'
              '...')
            TabOrder = 0
          end
        end
        object ts3: TTabSheet
          Caption = 'Protocol'
          ImageIndex = 1
          object ServerProtocol: TRadioGroup
            Left = 0
            Top = 21
            Width = 334
            Height = 75
            Align = alClient
            Caption = 'Server Protocol'
            Columns = 3
            ItemIndex = 0
            Items.Strings = (
              'Local'
              'TCP/IP'
              'IPX')
            TabOrder = 0
          end
          object Panel4: TPanel
            Left = 0
            Top = 0
            Width = 334
            Height = 21
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object Label2: TLabel
              Left = 0
              Top = 0
              Width = 334
              Height = 21
              Align = alClient
              Alignment = taRightJustify
              AutoSize = False
              Caption = '   You must reopen database [Disconnect, Connect]'
              Color = clYellow
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clRed
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentColor = False
              ParentFont = False
            end
            object btn7: TButton
              Left = 0
              Top = 0
              Width = 76
              Height = 21
              Caption = 'Port >>'
              TabOrder = 0
              OnClick = btn7Click
            end
          end
        end
        object ts4: TTabSheet
          Caption = 'Locked record'
          ImageIndex = 2
          object Label1: TLabel
            Left = 0
            Top = 0
            Width = 334
            Height = 21
            Align = alTop
            Alignment = taRightJustify
            AutoSize = False
            Caption = '   You must reopen database [Disconnect, Connect]'
            Color = clYellow
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object rg1: TRadioGroup
            Left = 0
            Top = 21
            Width = 334
            Height = 75
            Align = alClient
            Caption = 'Database record LockType for delete, update, insert'
            Columns = 2
            ItemIndex = 0
            Items.Strings = (
              'OptimisticNoWait [fast update]'
              'OptimisticWait [fast update]'
              'PessimisticNoWait'
              'PessimisticWait')
            TabOrder = 0
            OnClick = rg1Click
          end
        end
      end
    end
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 305
      Height = 129
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 1
      object SpeedButton1: TSpeedButton
        Left = 2
        Top = 31
        Width = 70
        Height = 24
        Caption = 'Run SQL'
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B0000FFFFFF00FFFFFF00FFFF
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B0000007B0000007B0000FFFF
          FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B0000007B0000007B0000007B
          0000007B0000FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B0000007B0000007B0000007B
          0000007B0000007B0000007B0000FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B0000007B0000007B0000007B
          0000007B0000007B0000007B0000007B0000007B0000FFFFFF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B0000007B0000007B0000007B
          0000007B0000007B0000007B0000007B0000007B0000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B0000007B0000007B0000007B
          0000007B0000007B0000007B00007B7B7B00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B0000007B0000007B0000007B
          0000007B00007B7B7B00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B0000007B0000007B00007B7B
          7B00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B00007B0000007B00007B7B7B00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF007B7B7B007B7B7B00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        Layout = blGlyphRight
        Spacing = 0
        OnClick = SpeedButton1Click
      end
      object lbl3: TLabel
        Left = 78
        Top = 67
        Width = 32
        Height = 13
        Caption = 'Adress'
      end
      object lbl2: TLabel
        Left = 78
        Top = 37
        Width = 37
        Height = 13
        Caption = 'Passwd'
      end
      object lbl1: TLabel
        Left = 78
        Top = 9
        Width = 26
        Height = 13
        Caption = 'Login'
      end
      object lbl5: TLabel
        Left = 231
        Top = 7
        Width = 40
        Height = 13
        Caption = 'TimeOut'
      end
      object btn1: TButton
        Left = 2
        Top = 3
        Width = 70
        Height = 24
        Caption = 'Connect'
        TabOrder = 0
        OnClick = btn1Click
      end
      object SyntaxOnly: TCheckBox
        Left = 1
        Top = 67
        Width = 72
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Syntax only '
        TabOrder = 1
        OnClick = SyntaxOnlyClick
      end
      object btn4: TButton
        Left = 2
        Top = 100
        Width = 76
        Height = 24
        Caption = 'Start trans.'
        TabOrder = 2
        OnClick = btn4Click
      end
      object btn5: TButton
        Left = 79
        Top = 100
        Width = 76
        Height = 24
        Caption = 'Commit trans.'
        TabOrder = 3
        OnClick = btn5Click
      end
      object btn6: TButton
        Left = 156
        Top = 100
        Width = 76
        Height = 24
        Caption = 'Rollback trans.'
        TabOrder = 4
        OnClick = btn6Click
      end
      object btnParamValues: TButton
        Left = 233
        Top = 100
        Width = 65
        Height = 24
        Caption = 'Params SQL'
        TabOrder = 5
        OnClick = btnParamValuesClick
      end
      object edt3: TEdit
        Left = 116
        Top = 62
        Width = 182
        Height = 21
        TabOrder = 6
        Text = 'name@localhost'
      end
      object edt2: TEdit
        Left = 116
        Top = 31
        Width = 100
        Height = 21
        TabOrder = 7
        Text = 'masteradmin'
      end
      object edt1: TEdit
        Left = 116
        Top = 4
        Width = 100
        Height = 21
        TabOrder = 8
        Text = 'systemadmin'
      end
      object se1: TSpinEdit
        Left = 231
        Top = 30
        Width = 67
        Height = 22
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Increment = 100
        MaxValue = 10000000
        MinValue = -1
        ParentFont = False
        TabOrder = 9
        Value = 10000
        OnChange = se1Change
      end
      object CheckBox1: TCheckBox
        Left = 1
        Top = 82
        Width = 72
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Req. Live'
        TabOrder = 10
        OnClick = CheckBox1Click
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 132
    Width = 649
    Height = 296
    ActivePage = TabSheet2
    Align = alClient
    Style = tsButtons
    TabOrder = 2
    object TabSheet2: TTabSheet
      Caption = 'Result'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 641
        Height = 265
        Align = alClient
        BevelOuter = bvLowered
        Caption = 'Panel1'
        TabOrder = 0
        object grdResults: TDBGrid
          Left = 1
          Top = 21
          Width = 639
          Height = 243
          Align = alClient
          BorderStyle = bsNone
          DataSource = DataSource1
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
        end
        object DBNavigator1: TDBNavigator
          Left = 1
          Top = 1
          Width = 639
          Height = 20
          DataSource = DataSource1
          Align = alTop
          Flat = True
          TabOrder = 1
        end
      end
    end
  end
  object DataSource1: TDataSource
    DataSet = fsquery1
    Left = 26
    Top = 186
  end
  object FSSession1: TFSSession
    ClientName = 'tt'
    SessionName = 'tt'
    Left = 258
    Top = 348
  end
  object FSDatabase1: TFSDatabase
    DataBaseName = 'testdb'
    SessionName = 'tt'
    RecLocking = tlOptimisticNoWait
    Left = 323
    Top = 348
  end
  object FSClient1: TFSClient
    ClientName = 'tt'
    OnConnectionLost = FSClient1ConnectionLost
    ServerEngine = FSRemoteServer1
    Left = 18
    Top = 348
  end
  object FSRemoteServer1: TFSRemoteServer
    Transport = FSParamConnect1
    Left = 89
    Top = 348
  end
  object FSParamConnect1: TFSParamConnect
    ServerName = 'Local'
    Protocol = ptSingleUser
    Left = 180
    Top = 348
  end
  object fsquery1: TFSQuery
    BlobAutoStartTransaction = True
    BlobChunkSize = 524288
    BlobModifiedError = False
    BlobMode = bmDirect
    DataBaseName = 'testdb'
    FilterEval = fseLocal
    FilterTimeout = 3000
    FlipOrder = False
    SessionName = 'tt'
    SQL.Strings = (
      ''
      'Select * from "TestArray_"'
      'where fautoinc32=:dd')
    SupportRecNo = False
    Left = 92
    Top = 187
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'dd'
        ParamType = ptUnknown
      end>
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 17
    Top = 396
  end
end
