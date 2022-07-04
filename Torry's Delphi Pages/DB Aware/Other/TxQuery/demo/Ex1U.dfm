object frmTest: TfrmTest
  Left = 89
  Top = 137
  Width = 783
  Height = 527
  Caption = 'TxQuery v1.86 Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 775
    Height = 462
    ActivePage = TabSheet1
    Align = alClient
    HotTrack = True
    MultiLine = True
    TabIndex = 0
    TabOrder = 0
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'SQL statements'
      object StatusBar1: TStatusBar
        Left = 0
        Top = 412
        Width = 767
        Height = 19
        Panels = <>
        SimplePanel = True
      end
      object PageControlSQLExamples: TPageControl
        Left = 0
        Top = 0
        Width = 767
        Height = 412
        ActivePage = TabSheetSQLString
        Align = alClient
        TabIndex = 0
        TabOrder = 1
        OnChange = PageControlSQLExamplesChange
        object TabSheetSQLString: TTabSheet
          Caption = 'SQL String'
          object Panel8: TPanel
            Left = 0
            Top = 38
            Width = 209
            Height = 343
            Align = alLeft
            TabOrder = 0
            object TreeView1: TTreeView
              Left = 1
              Top = 1
              Width = 207
              Height = 341
              Align = alClient
              Indent = 19
              TabOrder = 0
              OnChange = TreeView1Change
              OnClick = TreeView1Click
            end
          end
          object PanelSideButtons: TPanel
            Left = 0
            Top = 0
            Width = 759
            Height = 38
            Align = alTop
            TabOrder = 1
            object Button2: TButton
              Left = 238
              Top = 4
              Width = 104
              Height = 30
              Caption = 'Color &settings...'
              TabOrder = 0
              OnClick = Button2Click
            end
            object Button5: TButton
              Left = 353
              Top = 4
              Width = 105
              Height = 30
              Caption = '&Meanings...'
              TabOrder = 1
              OnClick = Button5Click
            end
            object BtnQBuilder: TButton
              Left = 122
              Top = 4
              Width = 104
              Height = 30
              Caption = 'Query Builder...'
              TabOrder = 2
              OnClick = BtnQBuilderClick
            end
            object ButtonRunSQL: TBitBtn
              Left = 6
              Top = 4
              Width = 105
              Height = 30
              Hint = 'Run SQL with Open method'
              Caption = 'Run SQL'
              Enabled = False
              ParentShowHint = False
              ShowHint = True
              TabOrder = 3
              OnClick = ButtonRunSQLClick
              Glyph.Data = {
                2E060000424D2E06000000000000360400002800000015000000150000000100
                080000000000F801000000000000000000000001000000010000000000000000
                80000080000000808000800000008000800080800000C0C0C000688DA200BBCC
                D500000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                0000000000000000000000000000000000000000000000000000000000000000
                000000000000000000000000000000000000DDE6EA00A4A0A000808080000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FDFDFDFDFDFD
                FDFDFDFDFDFDFDFDFDFDFDFDFDFDFD01B602FDFDFDFDFDFDFDFDFDFDFDFDFDFD
                FDFDFDFDFDFDFD000400FDFDFDFDFDFDFDFD00F8FDFDFDFDFDFDFDFDFDFDFD00
                8401FDFDFDFDFDFDFDFD0000F8FDFDFDFDFDFDFDFDFDFD013301FDFDFDFDFDFD
                FDFDFD0000F8FDFDFDFDFDFDFDFDFD007200FDFDFDFDFDFDFDFDFD00FB00F8FD
                FDFDFDFDFDFDFD00DB00FDFDFDFDFDFDFD00000000FB00F8FDFDFDFDFDFDFD02
                B602FDFDFDFDFDFDFD00FBFBFFFBFB00F8FDFDFDFDFDFD00DB00FDFDFDFDFDFD
                FDFD00FFFB00000000FDFDFDFDFDFD01B701FDFDFDFDFDFDFDFD00FBFFFB00F8
                FDFDFDFDFDFDFD02B602FDFDFDFDFD0000000000FBFFFB00F8FDFDFDFDFDFD01
                6901FDFDFDFDFD00FFFBFFFBFFFBFFFB00F8FDFDFDFDFD000200FDFDFDFDFDFD
                00FFFBFFFB00000000FDFDFDFDFDFD4EB781FDFDFDFDFDFD00FBFFFBFFFB00F8
                FDFDFDFDFDFDFD015201FDFDFDFDFDFDFD00FBFFFBFFFB00F8FDFDFDFDFDFD01
                5201FDFDFDFDFDFDFD00FFFFFFFBFFFF00F8FDFDFDFDFD01C202FDFDFDFDFDFD
                FDFD00FFFBFFFFFBFF00F8FDFDFDFD005201FDFDFDFDFDFDFDFD000000000000
                000000FDFDFDFD00AA00FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD02
                F102FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD000600FDFDFDFDFDFD
                FDFDFDFDFDFDFDFDFDFDFDFDFDFDFD74B681}
            end
            object ChkParse: TCheckBox
              Left = 528
              Top = 9
              Width = 169
              Height = 16
              Caption = '&Parse SQL'
              TabOrder = 4
              OnClick = ChkParseClick
            end
          end
          object Panel11: TPanel
            Left = 209
            Top = 38
            Width = 550
            Height = 343
            Align = alClient
            TabOrder = 2
            object Splitter1: TSplitter
              Left = 1
              Top = 157
              Width = 548
              Height = 4
              Cursor = crVSplit
              Align = alBottom
              Visible = False
            end
            object RichEdit1: TRichEdit
              Left = 1
              Top = 1
              Width = 548
              Height = 156
              Align = alClient
              Font.Charset = ANSI_CHARSET
              Font.Color = clNavy
              Font.Height = -15
              Font.Name = 'Verdana'
              Font.Style = [fsBold]
              Lines.Strings = (
                ' '
                '  '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' '
                ' ')
              ParentFont = False
              ScrollBars = ssVertical
              TabOrder = 0
            end
            object Panel12: TPanel
              Left = 1
              Top = 161
              Width = 548
              Height = 181
              Align = alBottom
              TabOrder = 1
              Visible = False
              object MemoParse: TMemo
                Left = 169
                Top = 1
                Width = 378
                Height = 179
                Align = alClient
                Font.Charset = ANSI_CHARSET
                Font.Color = clMaroon
                Font.Height = -18
                Font.Name = 'Courier New'
                Font.Style = []
                ParentFont = False
                ScrollBars = ssBoth
                TabOrder = 0
              end
              object TreeView2: TTreeView
                Left = 1
                Top = 1
                Width = 168
                Height = 179
                Align = alLeft
                Indent = 19
                TabOrder = 1
                OnClick = TreeView2Click
              end
            end
          end
        end
        object TabSheetResultDataSet: TTabSheet
          Caption = 'Result Set'
          object Panel9: TPanel
            Left = 0
            Top = 0
            Width = 934
            Height = 490
            Align = alClient
            Caption = 'Panel9'
            TabOrder = 0
            object DBGrid1: TDBGrid
              Left = 1
              Top = 1
              Width = 932
              Height = 461
              Align = alClient
              DataSource = DataSource1
              Font.Charset = ANSI_CHARSET
              Font.Color = clBlack
              Font.Height = -15
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
              ParentFont = False
              TabOrder = 0
              TitleFont.Charset = DEFAULT_CHARSET
              TitleFont.Color = clWindowText
              TitleFont.Height = -11
              TitleFont.Name = 'MS Sans Serif'
              TitleFont.Style = []
            end
            object Panel10: TPanel
              Left = 1
              Top = 462
              Width = 932
              Height = 27
              Align = alBottom
              TabOrder = 1
              object SpeedButton1: TSpeedButton
                Left = 256
                Top = 1
                Width = 119
                Height = 26
                Caption = 'To &HTML...'
                Flat = True
                OnClick = SpeedButton1Click
              end
              object DBNavigator1: TDBNavigator
                Left = 1
                Top = 1
                Width = 251
                Height = 25
                DataSource = DataSource1
                VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbPost, nbCancel, nbRefresh]
                Align = alLeft
                TabOrder = 0
              end
              object Panel1: TPanel
                Left = 443
                Top = 1
                Width = 487
                Height = 25
                Align = alRight
                BevelOuter = bvNone
                TabOrder = 1
                object Bar1: TProgressBar
                  Left = 0
                  Top = 0
                  Width = 487
                  Height = 25
                  Align = alClient
                  Min = 0
                  Max = 100
                  TabOrder = 0
                  Visible = False
                end
                object Button1: TButton
                  Left = 28
                  Top = 0
                  Width = 75
                  Height = 25
                  Caption = 'Delete'
                  TabOrder = 1
                  OnClick = Button1Click
                end
              end
            end
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Filtering and Finding'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 767
        Height = 78
        Align = alTop
        TabOrder = 0
        object Label1: TLabel
          Left = 118
          Top = 20
          Width = 104
          Height = 16
          Caption = 'Filter expression :'
        end
        object Label3: TLabel
          Left = 118
          Top = 48
          Width = 101
          Height = 16
          Caption = 'Find &expression :'
        end
        object Button3: TButton
          Left = 4
          Top = 4
          Width = 94
          Height = 30
          Caption = 'Filter'
          TabOrder = 0
          OnClick = Button3Click
        end
        object ComboBox1: TComboBox
          Left = 222
          Top = 10
          Width = 473
          Height = 24
          ItemHeight = 16
          TabOrder = 1
          Items.Strings = (
            '(CustNo >= 1500) And (CustNo <= 3000)'
            'LIKE(Company,'#39'%Under%'#39','#39#39')'
            'NOT LIKE(Company,'#39'%Under%'#39','#39#39')'
            'LIKE(Company,'#39'A_%Under%'#39','#39#39')'
            
              'IN(City, '#39'Kapaa Kauai'#39', '#39'Freeport'#39', '#39'Bogota'#39', '#39'Sarasota'#39', '#39'Negri' +
              'l'#39', '#39'Largo'#39')'
            
              'NOT IN(City, '#39'Kapaa Kauai'#39', '#39'Freeport'#39', '#39'Bogota'#39', '#39'Sarasota'#39', '#39'N' +
              'egril'#39', '#39'Largo'#39')'
            'IN(CustNo, 1356, 1624, 1510, 2984, 2975)'
            'LastInvoiceDate < Now'
            'LastInvoiceDate > StrToDate('#39'12/31/94'#39')')
        end
        object Edit1: TEdit
          Left = 222
          Top = 38
          Width = 473
          Height = 21
          TabOrder = 2
          Text = 'Addr1 = '#39'32 Main St.'#39
        end
        object Button6: TButton
          Left = 4
          Top = 38
          Width = 94
          Height = 31
          Caption = '&Find'
          TabOrder = 3
          OnClick = Button6Click
        end
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 78
        Width = 767
        Height = 353
        Align = alClient
        DataSource = DataSource1
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Verdana'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Extended identifiers and blob fields'
      object DBGrid3: TDBGrid
        Left = 0
        Top = 296
        Width = 767
        Height = 135
        Align = alBottom
        DataSource = DataSource2
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Verdana'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object Panel3: TPanel
        Left = 0
        Top = 78
        Width = 767
        Height = 36
        Align = alTop
        TabOrder = 1
        object DBNavigator2: TDBNavigator
          Left = 10
          Top = 10
          Width = 276
          Height = 22
          DataSource = DataSource2
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
          TabOrder = 0
        end
        object Button4: TButton
          Left = 295
          Top = 4
          Width = 174
          Height = 30
          Caption = 'Create result set'
          TabOrder = 1
          OnClick = Button4Click
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 114
        Width = 331
        Height = 182
        Hint = 'Scroll grid below to see other fish'
        Align = alLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        object DBLabel1: TDBText
          Left = 1
          Top = 277
          Width = 329
          Height = 29
          Align = alBottom
          DataField = 'Common_Name'
          DataSource = DataSource2
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -23
          Font.Name = 'MS Serif'
          Font.Style = [fsBold, fsItalic]
          ParentFont = False
        end
        object DBImage1: TDBImage
          Left = 1
          Top = 1
          Width = 329
          Height = 276
          Hint = 'Scroll grid below to see other fish'
          Align = alClient
          DataField = 'Graphic'
          DataSource = DataSource2
          TabOrder = 0
        end
      end
      object Panel6: TPanel
        Left = 331
        Top = 114
        Width = 549
        Height = 182
        Align = alLeft
        BevelOuter = bvLowered
        Caption = 'Panel6'
        TabOrder = 3
        object DBMemo1: TDBMemo
          Left = 1
          Top = 28
          Width = 547
          Height = 278
          Align = alClient
          BorderStyle = bsNone
          Color = clSilver
          Ctl3D = False
          DataField = 'Notes'
          DataSource = DataSource2
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -15
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object Panel5: TPanel
          Left = 1
          Top = 1
          Width = 547
          Height = 27
          Align = alTop
          TabOrder = 1
          object Label2: TLabel
            Left = 9
            Top = 5
            Width = 66
            Height = 16
            Caption = 'About the'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object DBLabel2: TDBText
            Left = 82
            Top = 5
            Width = 69
            Height = 16
            AutoSize = True
            DataField = 'Common_Name'
            DataSource = DataSource2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -15
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
      end
      object RichEdit2: TRichEdit
        Left = 0
        Top = 0
        Width = 767
        Height = 78
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -15
        Font.Name = 'Verdana'
        Font.Style = []
        Lines.Strings = (
          
            'SELECT [Species No], Category, Common_Name, [Species Name], [Len' +
            'gth (cm)], '
          'Length_In, '
          'Notes, Graphic'
          'FROM Biolife ;')
        ParentFont = False
        ReadOnly = True
        TabOrder = 4
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Params'
      object Label4: TLabel
        Left = 5
        Top = 42
        Width = 120
        Height = 16
        Caption = 'LOWRANGE value :'
      end
      object Label5: TLabel
        Left = 276
        Top = 42
        Width = 123
        Height = 16
        Caption = 'HIGHRANGE value :'
      end
      object RichEdit3: TRichEdit
        Left = 0
        Top = 0
        Width = 767
        Height = 30
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Verdana'
        Font.Style = []
        Lines.Strings = (
          
            'SELECT * FROM CUSTOMER WHERE CustNo BETWEEN :LOWRANGE AND  :HIGH' +
            'RANGE;')
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object DBGrid4: TDBGrid
        Left = 0
        Top = -27
        Width = 767
        Height = 458
        Align = alBottom
        DataSource = DataSource3
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -15
        Font.Name = 'Verdana'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object Edit2: TEdit
        Left = 138
        Top = 32
        Width = 104
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        Text = '1000'
      end
      object Edit3: TEdit
        Left = 409
        Top = 32
        Width = 104
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = '3000'
      end
      object Button7: TButton
        Left = 539
        Top = 32
        Width = 134
        Height = 26
        Caption = 'Create result set'
        TabOrder = 4
        OnClick = Button7Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'DataSource property'
      object Label6: TLabel
        Left = 0
        Top = 309
        Width = 767
        Height = 16
        Align = alTop
        Caption = 
          'In this query, the lowest DBGrid is connected to a TxQuery that ' +
          'is linked to Customers Table with DataSource property.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 767
        Height = 121
        Align = alTop
        TabOrder = 0
        object Button8: TButton
          Left = 692
          Top = 5
          Width = 90
          Height = 31
          Caption = 'Open query'
          TabOrder = 0
          OnClick = Button8Click
        end
        object RichEdit4: TRichEdit
          Left = 1
          Top = 1
          Width = 687
          Height = 119
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -15
          Font.Name = 'Verdana'
          Font.Style = []
          Lines.Strings = (
            'SELECT * FROM Orders WHERE CustNo = :CustNo;')
          ParentFont = False
          TabOrder = 1
        end
      end
      object DBGrid5: TDBGrid
        Left = 0
        Top = 121
        Width = 767
        Height = 188
        Align = alTop
        DataSource = DataSource4
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBGrid6: TDBGrid
        Left = 0
        Top = 347
        Width = 767
        Height = 84
        Align = alClient
        DataSource = DataSource5
        Font.Charset = ANSI_CHARSET
        Font.Color = clNavy
        Font.Height = -15
        Font.Name = 'Verdana'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBNavigator3: TDBNavigator
        Left = 0
        Top = 325
        Width = 767
        Height = 22
        DataSource = DataSource4
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
        Align = alTop
        TabOrder = 3
      end
    end
  end
  object XQuery1: TxQuery
    ParamCheck = False
    About = 'TxQuery Version 1.86.1 (Aug 2003)'
    AutoDisableControls = False
    DateFormat = 'm/d/yyyy'
    ParamsAsFields = <>
    OnUDFCheck = XQuery1UDFCheck
    OnUDFSolve = XQuery1UDFSolve
    OnProgress = XQuery1Progress
    OnSyntaxError = XQuery1SyntaxError
    OnCancelQuery = XQuery1CancelQuery
    OnResolveDataset = XQuery1ResolveDataset
    DataSets = <
      item
        Alias = 'Customer'
        DataSet = Table1
      end
      item
        Alias = 'Orders'
        DataSet = Table2
      end
      item
        Alias = 'Items'
        DataSet = Table3
      end
      item
        Alias = 'Parts'
        DataSet = Table4
      end>
    OnIndexNeededFor = XQuery1IndexNeededFor
    OnSetRange = XQuery1SetRange
    OnCancelRange = XQuery1CancelRange
    OnCreateTable = XQuery1CreateTable
    OnCreateIndex = XQuery1CreateIndex
    OnDropTable = XQuery1DropTable
    OnDropIndex = XQuery1DropIndex
    OnSetFilter = XQuery1SetFilter
    OnCancelFilter = XQuery1CancelFilter
    Left = 339
    Top = 150
  end
  object DataSource1: TDataSource
    DataSet = XQuery1
    Left = 339
    Top = 218
  end
  object XQuery2: TxQuery
    SQL.Strings = (
      'SELECT * FROM Biolife;')
    About = 'TxQuery Version 1.86.1 (Aug 2003)'
    AutoDisableControls = False
    DateFormat = 'm/d/yyyy'
    ParamsAsFields = <>
    DataSets = <
      item
        Alias = 'Biolife'
        DataSet = Table5
      end>
    Left = 418
    Top = 150
  end
  object DataSource2: TDataSource
    DataSet = XQuery2
    Left = 418
    Top = 218
  end
  object Table5: TTable
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    TableName = 'biolife.db'
    Left = 576
    Top = 94
    object Table5SpeciesNo: TFloatField
      FieldName = 'Species No'
    end
    object Table5Category: TStringField
      FieldName = 'Category'
      Size = 15
    end
    object Table5Common_Name: TStringField
      FieldName = 'Common_Name'
      Size = 30
    end
    object Table5SpeciesName: TStringField
      FieldName = 'Species Name'
      Size = 40
    end
    object Table5Lengthcm: TFloatField
      FieldName = 'Length (cm)'
    end
    object Table5Length_In: TFloatField
      FieldName = 'Length_In'
    end
    object Table5Notes: TMemoField
      FieldName = 'Notes'
      BlobType = ftMemo
      Size = 50
    end
    object Table5Graphic: TGraphicField
      FieldName = 'Graphic'
      BlobType = ftGraphic
    end
  end
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    IndexName = 'ByCompany'
    TableName = 'customer.db'
    Left = 260
    Top = 94
    object Table1CustNo: TFloatField
      FieldName = 'CustNo'
    end
    object Table1Company: TStringField
      FieldName = 'Company'
      Size = 30
    end
    object Table1Addr1: TStringField
      FieldName = 'Addr1'
      Size = 30
    end
    object Table1Addr2: TStringField
      FieldName = 'Addr2'
      Size = 30
    end
    object Table1City: TStringField
      FieldName = 'City'
      Size = 15
    end
    object Table1State: TStringField
      FieldName = 'State'
    end
    object Table1Zip: TStringField
      FieldName = 'Zip'
      Size = 10
    end
    object Table1Country: TStringField
      FieldName = 'Country'
    end
    object Table1Phone: TStringField
      FieldName = 'Phone'
      Size = 15
    end
    object Table1FAX: TStringField
      FieldName = 'FAX'
      Size = 15
    end
    object Table1TaxRate: TFloatField
      FieldName = 'TaxRate'
    end
    object Table1Contact: TStringField
      FieldName = 'Contact'
    end
    object Table1LastInvoiceDate: TDateTimeField
      FieldName = 'LastInvoiceDate'
    end
    object Table1_Campo: TIntegerField
      FieldName = '_Campo'
    end
  end
  object Table2: TTable
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    TableName = 'orders.db'
    Left = 307
    Top = 110
    object Table2OrderNo: TFloatField
      FieldName = 'OrderNo'
      DisplayFormat = #39'#'#39'0000'
    end
    object Table2CustNo: TFloatField
      Alignment = taLeftJustify
      CustomConstraint = 'CustNo IS NOT NULL'
      ConstraintErrorMessage = 'CustNo cannot be blank'
      FieldName = 'CustNo'
      Required = True
      DisplayFormat = 'CN 0000'
      MaxValue = 9999
      MinValue = 1000
    end
    object Table2SaleDate: TDateTimeField
      FieldName = 'SaleDate'
    end
    object Table2ShipDate: TDateTimeField
      FieldName = 'ShipDate'
    end
    object Table2EmpNo: TIntegerField
      CustomConstraint = 'Value > 0'
      ConstraintErrorMessage = 'EmpNo cannot be 0 or negative'
      FieldName = 'EmpNo'
      Required = True
      DisplayFormat = 'Emp'#39'#'#39' 0000'
      MaxValue = 9999
      MinValue = 1
    end
    object Table2ShipToContact: TStringField
      FieldName = 'ShipToContact'
    end
    object Table2ShipToAddr1: TStringField
      FieldName = 'ShipToAddr1'
      Size = 30
    end
    object Table2ShipToAddr2: TStringField
      FieldName = 'ShipToAddr2'
      Size = 30
    end
    object Table2ShipToCity: TStringField
      FieldName = 'ShipToCity'
      Size = 15
    end
    object Table2ShipToState: TStringField
      FieldName = 'ShipToState'
    end
    object Table2ShipToZip: TStringField
      FieldName = 'ShipToZip'
      Size = 10
    end
    object Table2ShipToCountry: TStringField
      FieldName = 'ShipToCountry'
    end
    object Table2ShipToPhone: TStringField
      FieldName = 'ShipToPhone'
      Size = 15
    end
    object Table2ShipVIA: TStringField
      FieldName = 'ShipVIA'
      Size = 7
    end
    object Table2PO: TStringField
      FieldName = 'PO'
      Size = 15
    end
    object Table2Terms: TStringField
      FieldName = 'Terms'
      Size = 6
    end
    object Table2PaymentMethod: TStringField
      FieldName = 'PaymentMethod'
      Size = 7
    end
    object Table2ItemsTotal: TCurrencyField
      FieldName = 'ItemsTotal'
    end
    object Table2TaxRate: TFloatField
      FieldName = 'TaxRate'
      DisplayFormat = '0.00%'
      MaxValue = 100
    end
    object Table2Freight: TCurrencyField
      FieldName = 'Freight'
    end
    object Table2AmountPaid: TCurrencyField
      FieldName = 'AmountPaid'
    end
  end
  object Table3: TTable
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    TableName = 'items.db'
    Left = 418
    Top = 94
    object Table3OrderNo: TFloatField
      FieldName = 'OrderNo'
      DisplayFormat = #39'#'#39'0000'
    end
    object Table3ItemNo: TFloatField
      FieldName = 'ItemNo'
    end
    object Table3PartNo: TFloatField
      Alignment = taLeftJustify
      FieldName = 'PartNo'
      DisplayFormat = 'PN-00000'
    end
    object Table3Qty: TIntegerField
      FieldName = 'Qty'
    end
    object Table3Discount: TFloatField
      FieldName = 'Discount'
      DisplayFormat = '#%'
      MaxValue = 100
    end
  end
  object Table4: TTable
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    TableName = 'parts.db'
    Left = 497
    Top = 94
    object Table4PartNo: TFloatField
      Alignment = taLeftJustify
      FieldName = 'PartNo'
      DisplayFormat = 'PN-00000'
    end
    object Table4VendorNo: TFloatField
      CustomConstraint = '(VendorNo > 1000) and (VendorNo < 9999)'
      ConstraintErrorMessage = 'Vendor No has to be between 1000 and 9999'
      FieldName = 'VendorNo'
      DisplayFormat = 'VN 0000'
      MaxValue = 9999
      MinValue = 1000
    end
    object Table4Description: TStringField
      FieldName = 'Description'
      Size = 30
    end
    object Table4OnHand: TFloatField
      FieldName = 'OnHand'
    end
    object Table4OnOrder: TFloatField
      FieldName = 'OnOrder'
    end
    object Table4Cost: TCurrencyField
      FieldName = 'Cost'
    end
    object Table4ListPrice: TCurrencyField
      FieldName = 'ListPrice'
    end
  end
  object MainMenu1: TMainMenu
    Left = 655
    Top = 94
    object File1: TMenuItem
      Caption = '&File'
      object Saveresultsetastext1: TMenuItem
        Caption = '&Save result set as text...'
        OnClick = Saveresultsetastext1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
        OnClick = Exit1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Contents1: TMenuItem
        Caption = '&Contents...'
        OnClick = Contents1Click
      end
      object Howtobuy1: TMenuItem
        Caption = 'On line &registration...'
        OnClick = Howtobuy1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 363
    Top = 318
  end
  object SyntaxHighlighter1: TSyntaxHighlighter
    UpdateMode = umCharacter
    Editor = RichEdit1
    XQuery = XQuery1
    OnPosChange = SyntaxHighlighter1PosChange
    Left = 655
    Top = 218
  end
  object xQuery3: TxQuery
    SQL.Strings = (
      
        'SELECT * FROM CUSTOMER WHERE CustNo BETWEEN :LOWRANGE AND  :HIGH' +
        'RANGE;')
    About = 'TxQuery Version 1.86.1 (Aug 2003)'
    AutoDisableControls = False
    DateFormat = 'm/d/yyyy'
    ParamsAsFields = <>
    DataSets = <
      item
        Alias = 'Customer'
        DataSet = Table1
      end>
    Left = 497
    Top = 150
  end
  object DataSource3: TDataSource
    DataSet = xQuery3
    Left = 497
    Top = 218
  end
  object DataSource4: TDataSource
    DataSet = Table1
    Left = 260
    Top = 150
  end
  object DataSource5: TDataSource
    DataSet = xQuery4
    Left = 576
    Top = 218
  end
  object xQuery4: TxQuery
    DataSource = DataSource4
    SQL.Strings = (
      'SELECT * FROM Orders WHERE CustNo = :CustNo')
    About = 'TxQuery Version 1.86.1 (Aug 2003)'
    AutoDisableControls = False
    DateFormat = 'm/d/yyyy'
    ParamsAsFields = <>
    BeforeInsert = xQuery4BeforeInsert
    DataSets = <
      item
        Alias = 'Orders'
        DataSet = Table2
      end>
    Left = 576
    Top = 150
    ParamData = <
      item
        DataType = ftString
        Name = 'CustNo'
        ParamType = ptUnknown
      end>
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.TXT)|*.txt'
    Title = 'Save result set to text file'
    Left = 655
    Top = 150
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = 'htm'
    Filter = 'HTM files (*.htm)|*.htm'
    Title = 'Export to HTML'
    Left = 344
    Top = 376
  end
end
