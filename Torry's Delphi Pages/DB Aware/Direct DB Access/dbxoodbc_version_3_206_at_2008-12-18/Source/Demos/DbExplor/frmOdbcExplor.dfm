object FormOdbcExplor: TFormOdbcExplor
  Left = 215
  Top = 138
  Width = 784
  Height = 640
  Caption = 'DbExpress Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spCon: TSplitter
    Left = 0
    Top = 300
    Width = 776
    Height = 5
    Cursor = crVSplit
    Align = alTop
    Color = clBtnShadow
    ParentColor = False
  end
  object pClient: TPanel
    Left = 0
    Top = 305
    Width = 776
    Height = 263
    Align = alClient
    TabOrder = 1
    object spTree: TSplitter
      Left = 237
      Top = 1
      Width = 5
      Height = 261
      Color = clBtnShadow
      ParentColor = False
    end
    object pData: TPanel
      Left = 242
      Top = 1
      Width = 533
      Height = 261
      Align = alClient
      BevelOuter = bvLowered
      BorderWidth = 1
      TabOrder = 0
      object pcData: TPageControl
        Left = 2
        Top = 2
        Width = 529
        Height = 257
        ActivePage = tsData
        Align = alClient
        TabOrder = 0
        object tsData: TTabSheet
          Caption = 'Data'
          object Panel3: TPanel
            Left = 0
            Top = 0
            Width = 521
            Height = 54
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object lbTableName: TLabel
              Left = 60
              Top = 6
              Width = 65
              Height = 13
              Caption = 'TableName'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clHotLight
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object dbNavData: TDBNavigator
              Left = 3
              Top = 26
              Width = 220
              Height = 24
              DataSource = DataSource1
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              BeforeAction = dbNavDataBeforeAction
            end
            object btnBeginTransaction: TBitBtn
              Left = 225
              Top = 25
              Width = 105
              Height = 25
              Caption = '&BeginTransaction'
              Enabled = False
              TabOrder = 1
              OnClick = btnBeginTransactionClick
            end
            object btnCommit: TBitBtn
              Left = 330
              Top = 25
              Width = 75
              Height = 25
              Caption = 'Commi&t'
              Enabled = False
              TabOrder = 2
              OnClick = btnCommitClick
            end
            object btnRollack: TBitBtn
              Left = 405
              Top = 25
              Width = 75
              Height = 25
              Caption = '&Rollack'
              Enabled = False
              TabOrder = 3
              OnClick = btnRollackClick
            end
            object pbApplyUpdates: TBitBtn
              Left = 329
              Top = 1
              Width = 151
              Height = 23
              Caption = 'Apply &Updates'
              Enabled = False
              TabOrder = 4
              OnClick = pbApplyUpdatesClick
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00000000000000000000000000000000000000
                00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000ADD60000B5DE0000C6EF0000CEF70000D6FF0000D6
                FF0000D6FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00009CC600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF0000D6FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF000884AD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF0000C6EF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0008739C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF0000BDE70000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0008638C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
                FF0000ADD60000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00105A7B0008638C0008739400087BA500088CB5000894
                BD0000A5CE00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF0000000000000000000000000000000000FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF0000000000FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
              Layout = blGlyphRight
            end
            object btTreeAllign: TBitBtn
              Left = 3
              Top = 3
              Width = 25
              Height = 22
              TabOrder = 5
              OnClick = btTreeAllignClick
              Glyph.Data = {
                36030000424D3603000000000000360000002800000010000000100000000100
                1800000000000003000000000000000000000000000000000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FF000000FF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF
                00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FF000000808080000000FF00FFFF00FF000000808080000000FF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF00FF000000FF
                00FFFF00FF000000FF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FF
                000000000000FF00FFFF00FF000000FF00FFFF00FF000000FF00FFFF00FF0000
                00000000FF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF00FF000000FF
                00FFFF00FF000000FF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF00FF
                000000000000FF00FFFF00FF000000FF00FFFF00FF000000FF00FFFF00FF0000
                00000000FF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF00FF000000FF
                00FFFF00FF000000FF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FF000000808080000000FF00FFFF00FF000000808080000000FF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF
                00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FF000000FF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
            end
            object btConAllign: TBitBtn
              Left = 29
              Top = 3
              Width = 25
              Height = 22
              TabOrder = 6
              OnClick = btConAllignClick
              Glyph.Data = {
                36030000424D3603000000000000360000002800000010000000100000000100
                18000000000000030000C40E0000C40E00000000000000000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FF000000000000000000FF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF
                00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FF000000000000FF00FFFF00FFFF00FF000000000000FF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000808080FF00FFFF00FFFF
                00FFFF00FFFF00FF808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                0000000000000000000000000000000000000000000000000000000000000000
                00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FF00000000000000000000000000000000
                0000000000000000000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FF000000808080FF00FFFF00FFFF00FFFF00FFFF00FF808080000000FF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF00FFFF
                00FFFF00FF000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FF000000000000FF00FF000000000000FF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00000000
                0000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
            end
            object pbLogChanges: TBitBtn
              Left = 201
              Top = 1
              Width = 128
              Height = 23
              Caption = 'LogChanges (True)'
              Enabled = False
              TabOrder = 7
              OnClick = pbLogChangesClick
            end
          end
          object DBGrid1: TDBGrid
            Left = 0
            Top = 54
            Width = 521
            Height = 175
            Align = alClient
            DataSource = DataSource1
            TabOrder = 1
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            OnDrawDataCell = DBGrid1DrawDataCell
          end
        end
        object tsUpdateOpt: TTabSheet
          Caption = 'Update Options'
          ImageIndex = 1
          object GroupBox4: TGroupBox
            Left = 0
            Top = 0
            Width = 521
            Height = 73
            Align = alTop
            Caption = ' Update Style '
            TabOrder = 0
            object rbUpdateWhereAll: TRadioButton
              Left = 7
              Top = 17
              Width = 73
              Height = 17
              Caption = 'Where All'
              Checked = True
              TabOrder = 0
              TabStop = True
            end
            object rbUpdateWhereKey: TRadioButton
              Left = 7
              Top = 32
              Width = 89
              Height = 18
              Caption = 'Where Key'
              TabOrder = 1
            end
            object rbUpdateWhereChanged: TRadioButton
              Left = 7
              Top = 49
              Width = 97
              Height = 17
              Caption = 'Where Changed'
              TabOrder = 2
            end
            object pbMarkAllRecordsAsNew: TBitBtn
              Left = 112
              Top = 20
              Width = 165
              Height = 25
              Caption = 'Mark All Records As New'
              Enabled = False
              TabOrder = 3
              Visible = False
              OnClick = pbMarkAllRecordsAsNewClick
            end
            object gbTextBlob: TGroupBox
              Left = 307
              Top = 15
              Width = 170
              Height = 56
              Caption = '         -'
              TabOrder = 4
              Visible = False
              object cbCompressBlob: TCheckBox
                Left = 12
                Top = 16
                Width = 113
                Height = 17
                Caption = 'Compress ( LZH )'
                Checked = True
                State = cbChecked
                TabOrder = 0
              end
              object cbBase64: TCheckBox
                Left = 12
                Top = 32
                Width = 121
                Height = 17
                Caption = 'Encode Base64'
                Checked = True
                State = cbChecked
                TabOrder = 1
              end
            end
            object cbPackBlobsToString: TCheckBox
              Left = 324
              Top = 12
              Width = 145
              Height = 17
              Hint =
                'Use for "Mark All Records As New" or "Mark Current Records As Ne' +
                'w"'
              Caption = 'Blob / Text Blob Options : '
              ParentShowHint = False
              ShowHint = True
              TabOrder = 5
              Visible = False
            end
            object pbMarkCurrentRecordsAsNew: TBitBtn
              Left = 112
              Top = 44
              Width = 165
              Height = 25
              Caption = 'Mark Current Records As New'
              TabOrder = 6
              Visible = False
              OnClick = pbMarkCurrentRecordsAsNewClick
            end
          end
          object pcUpdSQL: TPageControl
            Left = 0
            Top = 113
            Width = 521
            Height = 116
            ActivePage = tsUpdSQL_Insert
            Align = alClient
            Enabled = False
            TabOrder = 1
            object tsUpdSQL_Insert: TTabSheet
              Caption = 'Insert SQL'
              object mSQLInsert: TMemo
                Left = 0
                Top = 0
                Width = 513
                Height = 88
                Align = alClient
                Enabled = False
                Lines.Strings = (
                  'Not Implemented it.')
                ScrollBars = ssBoth
                TabOrder = 0
              end
            end
            object tsUpdSQL_Modify: TTabSheet
              Caption = 'Modify SQL'
              ImageIndex = 1
              object mSQLModify: TMemo
                Left = 0
                Top = 0
                Width = 513
                Height = 88
                Align = alClient
                Enabled = False
                Lines.Strings = (
                  'Not Implemented it.')
                ScrollBars = ssBoth
                TabOrder = 0
              end
            end
            object tsUpdSQL_Delete: TTabSheet
              Caption = 'Delete SQL'
              ImageIndex = 2
              object mSQLDelete: TMemo
                Left = 0
                Top = 0
                Width = 513
                Height = 88
                Align = alClient
                Enabled = False
                Lines.Strings = (
                  'Not Implemented it.')
                ScrollBars = ssBoth
                TabOrder = 0
              end
            end
          end
          object pUO: TPanel
            Left = 0
            Top = 73
            Width = 521
            Height = 40
            Align = alTop
            BevelOuter = bvLowered
            TabOrder = 2
            object cbUpdIgnoreError: TCheckBox
              Left = 4
              Top = 5
              Width = 381
              Height = 17
              Caption = 'Ignore Error: "Record not found or changed by another user"'
              TabOrder = 0
            end
            object cbUpdateCustSQL: TCheckBox
              Left = 5
              Top = 21
              Width = 267
              Height = 17
              Caption = 'Usage Custom Update SQL (Not Implemented it)'
              Enabled = False
              TabOrder = 1
            end
          end
        end
      end
    end
    object pTree: TPanel
      Left = 1
      Top = 1
      Width = 236
      Height = 261
      Align = alLeft
      BevelOuter = bvNone
      Constraints.MinWidth = 2
      TabOrder = 1
      object TreeView1: TTreeView
        Left = 0
        Top = 0
        Width = 236
        Height = 261
        Align = alClient
        BevelOuter = bvRaised
        DragMode = dmAutomatic
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        HideSelection = False
        HotTrack = True
        Images = ImageList
        Indent = 19
        MultiSelectStyle = [msControlSelect, msShiftSelect, msSiblingOnly]
        ParentFont = False
        PopupMenu = PopupMenuTree
        ReadOnly = True
        RightClickSelect = True
        TabOrder = 0
        OnDblClick = TreeView1DblClick
        OnExpanding = TreeView1Expanding
        OnKeyDown = TreeView1KeyDown
        OnMouseDown = TreeView1MouseDown
      end
    end
  end
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 776
    Height = 300
    Align = alTop
    BevelOuter = bvLowered
    Constraints.MinHeight = 5
    ParentColor = True
    TabOrder = 0
    object pConnOptEx: TPanel
      Left = 1
      Top = 1
      Width = 774
      Height = 298
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Notebook1: TNotebook
        Left = 0
        Top = 0
        Width = 774
        Height = 277
        Align = alClient
        Anchors = [akLeft, akBottom]
        TabOrder = 0
        object TPage
          Left = 0
          Top = 0
          Caption = 'Connection'
          object gbLogin: TGroupBox
            Left = 0
            Top = 0
            Width = 774
            Height = 119
            Align = alTop
            Caption = 'ODBC Connection'
            Constraints.MinWidth = 10
            TabOrder = 0
            DesignSize = (
              774
              119)
            object DSN: TLabel
              Left = 8
              Top = 40
              Width = 23
              Height = 13
              Caption = 'D&SN'
              FocusControl = dfDSN
            end
            object UID: TLabel
              Left = 8
              Top = 64
              Width = 19
              Height = 13
              Caption = 'UI&D'
              FocusControl = dfUID
            end
            object PWD: TLabel
              Left = 8
              Top = 88
              Width = 26
              Height = 13
              Caption = '&PWD'
              FocusControl = dfPWD
            end
            object lbStatementsPerConnection: TLabel
              Left = 96
              Top = 17
              Width = 150
              Height = 13
              Caption = 'Statements Per Connection : [?]'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clHotLight
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object lbConnectedTime: TLabel
              Left = 403
              Top = 18
              Width = 126
              Height = 13
              Caption = 'Connected Time: 00:00:00'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clHotLight
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object LSVendors: TLabel
              Left = 176
              Top = 92
              Width = 60
              Height = 13
              Caption = 'Vendor Lib : '
            end
            object dfUID: TEdit
              Left = 37
              Top = 64
              Width = 700
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 2
            end
            object dfPWD: TEdit
              Left = 37
              Top = 88
              Width = 132
              Height = 21
              PasswordChar = '*'
              TabOrder = 3
            end
            object pbConnect: TBitBtn
              Left = 598
              Top = 88
              Width = 90
              Height = 25
              Caption = 'C&onnect'
              TabOrder = 0
              OnClick = pbConnectClick
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF0000636300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000063
                6300F7CEA50000636300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF0000636300F7CEA5000063630000636300006363000063630000636300FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF000063630000636300C6848400C6842100C6842100C68421000063
                6300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000636300F7CEA500F7CEA500F7CEA500C6848400C684
                210000636300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF0000000000F7CEA500F7CEA500F7CEA500C6848400C6842100C684
                21000063630000636300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF0000000000F7CEA500F7CEA500C6848400F7CEA500F7CEA5000063
                6300C6842100C684210000636300FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF0000000000FFFF0000C6848400F7CEA500F7CEA50000636300F7CE
                A500F7CEA500C6848400C684210000636300FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000000000FFFF0000F7CEA50000636300F7CEA500F7CE
                A500C6848400C6842100C684210000636300FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF000000000000000000F7CEA500F7CEA500C684
                8400F7CEA500F7CEA500C684210000636300FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF0000C6848400F7CE
                A500F7CEA500F7CEA500C684840000636300FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FFFF0000F7CE
                A500F7CEA500C68484000063630000636300FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
                0000C68484000063630000636300F7CEA50000636300FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF0000636300F7CEA50000636300FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF0000636300FF00FF00}
            end
            object dfDSN: TComboBox
              Left = 37
              Top = 38
              Width = 701
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              DropDownCount = 33
              ItemHeight = 13
              TabOrder = 1
              Text = 'DSN=dsn_mssql_dbdemos'
              OnCloseUp = dfDSNCloseUp
              OnSelect = dfDSNSelect
              Items.Strings = (
                '?')
            end
            object chkConnected: TCheckBox
              Left = 8
              Top = 16
              Width = 81
              Height = 17
              TabStop = False
              Caption = 'Connected'
              Enabled = False
              TabOrder = 5
            end
            object cbAutoClone: TCheckBox
              Left = 292
              Top = 17
              Width = 97
              Height = 17
              Hint = 'Switch property SQLConnection1.AutoClone'
              Caption = 'Auto Clone'
              Checked = True
              Enabled = False
              ParentShowHint = False
              ShowHint = True
              State = cbChecked
              TabOrder = 6
              OnClick = cbAutoCloneClick
            end
            object cbCursorPreserved: TCheckBox
              Left = 556
              Top = 16
              Width = 109
              Height = 17
              TabStop = False
              Caption = 'Cursor Preserved'
              Enabled = False
              TabOrder = 7
            end
            object VendorLib: TComboBox
              Left = 240
              Top = 88
              Width = 349
              Height = 21
              Style = csDropDownList
              DropDownCount = 25
              ItemHeight = 13
              TabOrder = 4
              OnChange = VendorLibChange
              Items.Strings = (
                'VendorName=MSODBC;VendorLib=odbc32.dll'
                'VendorName=MSODBC2;VendorLib=2:odbc32.dll'
                'VendorName=MSSQL 2000;VendorLib=sqlsrv32.dll'
                'VendorName=MSSQL 2005;VendorLib=sqlncli.dll'
                'VendorName=ORACLE;VendorLib=sqora32.dll'

                  'VendorName=INFORMIX;VendorLib=ICLIT09B.DLL;ICLIT09A.DLL;ICLIT09B' +
                  'W.DLL;ICLIT09AW.DLL'
                'VendorName=MS JET;VendorLib=odbcjt32.dll'
                'VendorName=IBM DB2;VendorLib=db2cli.dll'
                'VendorName=Sybase ASE 11;VendorLib=sysybnt.dll'
                'VendorName=Sybase ASA 8;VendorLib=dbodbc8.dll'
                'VendorName=Sybase ASA 7;VendorLib=dbodbc7.dll'
                'VendorName=MySQL 3;VendorLib=myodbc3.dll'
                'VendorName=MySQL;VendorLib=myodbc.dll'
                'VendorName=Merant DBase;VendorLib=ivdbf15.dll'
                'VendorName=IB/FB Phoenix;VendorLib=odbcjdbc.dll'
                'VendorName=IB6 XTG ODBC;VendorLib=ib6xtg10.dll'
                'VendorName=IB Easysoft;VendorLib=ib60odbc.dll'
                'VendorName=IB Gemini;VendorLib=ibgem20.dll'
                'VendorName=SQLite;VendorLib=sqliteodbc.dll'
                'VendorName=ThinkSQL;VendorLib=thinksql.dll'
                'VendorName=PostgreSQL;VendorLib=psqlodbc.dll'
                'VendorName=Oterro2;VendorLib=2:ot2k_32.dll')
            end
          end
          object pConOpt: TPanel
            Left = 0
            Top = 119
            Width = 774
            Height = 22
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object cbDefConOpt: TCheckBox
              Left = 8
              Top = 3
              Width = 177
              Height = 17
              Caption = 'Default Connection Options'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = cbDefConOptClick
            end
          end
          object ScrollBox1: TScrollBox
            Left = 0
            Top = 141
            Width = 774
            Height = 136
            VertScrollBar.Tracking = True
            Align = alClient
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 2
            object gbCustConOpt: TGroupBox
              Left = 0
              Top = 0
              Width = 755
              Height = 700
              Align = alTop
              Caption = 'Custom Connection Options'
              TabOrder = 0
              object sh_coReadOnly: TShape
                Left = 1
                Top = 432
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object sh_coSupportsMetadata: TShape
                Left = 1
                Top = 318
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object sh_coLockMode: TShape
                Left = 1
                Top = 299
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coSupportsSchemaFilter: TShape
                Left = 1
                Top = 242
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object sh_coMapSmallBcdToNative: TShape
                Left = 1
                Top = 261
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coMapCharAsBDE: TShape
                Left = 1
                Top = 280
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object Shape1: TShape
                Left = 1
                Top = 508
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object Shape8: TShape
                Left = 1
                Top = 527
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 16114387
              end
              object Shape16: TShape
                Left = 1
                Top = 546
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coNullStrParam: TShape
                Left = 1
                Top = 565
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object Shape2: TShape
                Left = 1
                Top = 489
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 16114387
              end
              object sh_coICloneCon: TShape
                Left = 1
                Top = 204
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object sh_drv_bugs: TShape
                Left = 1
                Top = 470
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 12567278
                Pen.Color = clMaroon
              end
              object sh_coNumericSeparator: TShape
                Left = 1
                Top = 223
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coMixedFetch: TShape
                Left = 1
                Top = 185
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coBCD2Exp: TShape
                Left = 1
                Top = 109
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coBlobChunkSize: TShape
                Left = 1
                Top = 128
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object sh_coNetPacketSize: TShape
                Left = 1
                Top = 147
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coTrimChar: TShape
                Left = 1
                Top = 33
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coMapInt64ToBCD: TShape
                Left = 1
                Top = 52
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
                Pen.Style = psClear
              end
              object sh_coIgnoreUnkFldType: TShape
                Left = 1
                Top = 71
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coEnableBCD: TShape
                Left = 1
                Top = 90
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
                Pen.Style = psClear
              end
              object sh_coSupportsCatalog: TShape
                Left = 1
                Top = 337
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coCatalogPrefix: TShape
                Left = 1
                Top = 356
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object sh_coConTimeout: TShape
                Left = 1
                Top = 375
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coEmptyStrParam: TShape
                Left = 1
                Top = 394
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object sh_coEnableUnicode: TShape
                Left = 1
                Top = 413
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_coSafeMode: TShape
                Left = 1
                Top = 451
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = clSilver
              end
              object sh_template: TShape
                Left = 1
                Top = 603
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = clBtnFace
                Pen.Color = clSilver
              end
              object sh_drv_custom: TShape
                Left = 1
                Top = 14
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 12615808
                Pen.Color = clMaroon
              end
              object co_coEmptyStrParam: TLabel
                Left = 4
                Top = 396
                Width = 84
                Height = 13
                Caption = 'coEmptyStrParam'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coMaxBCD: TLabel
                Left = 4
                Top = 549
                Width = 54
                Height = 13
                Caption = 'coMaxBCD'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coParDateByLev3: TLabel
                Left = 4
                Top = 530
                Width = 87
                Height = 13
                Caption = 'coParDateByLev3'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coFldReadOnly: TLabel
                Left = 4
                Top = 511
                Width = 73
                Height = 13
                Caption = 'coFldReadOnly'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coAutoInc: TLabel
                Left = 4
                Top = 491
                Width = 49
                Height = 13
                Caption = 'coAutoInc'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object Label_drv_bugs: TLabel
                Left = 21
                Top = 473
                Width = 124
                Height = 13
                Caption = 'ODBC DRIVER BUGS'
                Color = 12567278
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clMaroon
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentColor = False
                ParentFont = False
              end
              object co_coNumericSeparator: TLabel
                Left = 4
                Top = 225
                Width = 56
                Height = 13
                Caption = 'coNumSepr'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object Label7: TLabel
                Left = 240
                Top = 226
                Width = 415
                Height = 13
                Caption =
                  'Decimal Separator transmitted into odbc driver ("X" == default =' +
                  '= '#39#39'.";  "-" - ignore value).'
                Color = 16114387
                ParentColor = False
              end
              object co_coICloneCon: TLabel
                Left = 4
                Top = 205
                Width = 67
                Height = 13
                Caption = 'coICloneCone'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object sh_FetchRowCount: TShape
                Left = 1
                Top = 166
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 14346967
                Pen.Color = 14346967
              end
              object lbBlockRowsCount: TLabel
                Left = 240
                Top = 169
                Width = 121
                Height = 13
                Caption = 'Fetch Block Rows Count.'
                Color = 14346967
                ParentColor = False
              end
              object co_FetchRowCount: TLabel
                Left = 4
                Top = 168
                Width = 66
                Height = 13
                Caption = '"RowsetSize"'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
                Visible = False
              end
              object co_coNetPacketSize: TLabel
                Left = 4
                Top = 148
                Width = 83
                Height = 13
                Caption = 'coNetPacketSize'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object Label2: TLabel
                Left = 240
                Top = 150
                Width = 142
                Height = 13
                Caption = 'Network Packet Size (>4096).'
                Color = 16114387
                ParentColor = False
              end
              object Label1: TLabel
                Left = 240
                Top = 131
                Width = 233
                Height = 13
                Caption = 'Blob Chunk Size (256-1'#39'024'#39'000). Default: 40960.'
                Color = 14346967
                ParentColor = False
              end
              object co_coBlobChunkSize: TLabel
                Left = 4
                Top = 129
                Width = 84
                Height = 13
                Caption = 'coBlobChunkSize'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coBCD2Exp: TLabel
                Left = 4
                Top = 110
                Width = 58
                Height = 13
                Caption = 'coBCD2Exp'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coEnableBCD: TLabel
                Left = 4
                Top = 91
                Width = 67
                Height = 13
                Caption = 'coEnableBCD'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coIgnoreUnkFldType: TLabel
                Left = 4
                Top = 72
                Width = 100
                Height = 13
                Caption = 'coIgnoreUnkFldType'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coMapInt64ToBCD: TLabel
                Left = 4
                Top = 53
                Width = 92
                Height = 13
                Caption = 'coMapInt64ToBCD'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coTrimChar: TLabel
                Left = 4
                Top = 34
                Width = 54
                Height = 13
                Caption = 'coTrimChar'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coMixedFetch: TLabel
                Left = 4
                Top = 186
                Width = 67
                Height = 13
                Caption = 'coMixedFetch'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object Label3: TLabel
                Left = 21
                Top = 15
                Width = 151
                Height = 13
                Caption = 'DbxOOdbc custom options'
                Color = 12615808
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clYellow
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = [fsBold]
                ParentColor = False
                ParentFont = False
              end
              object co_coNullStrParam: TLabel
                Left = 4
                Top = 569
                Width = 73
                Height = 13
                Caption = 'coNullStrParam'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object lbFetchRowsInfo: TLabel
                Left = 364
                Top = 169
                Width = 263
                Height = 13
                Caption = '( STATIC_CURSOR when >1 and enabled MixedFetch)'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clHotLight
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object lbMixedFetchInfo: TLabel
                Left = 252
                Top = 188
                Width = 243
                Height = 13
                Caption = 'Allow use STATIC cursor (when "RowsetSize" > 1).'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clHotLight
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object lbMixedFetchInfoNB: TLabel
                Left = 500
                Top = 188
                Width = 265
                Height = 13
                Caption = 'This mode support only very qualitative the odbc drivers.'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clMaroon
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coSafeMode: TLabel
                Left = 4
                Top = 245
                Width = 61
                Height = 13
                Caption = 'coSafeMode'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coMapSmallBcdToNative: TLabel
                Left = 4
                Top = 264
                Width = 121
                Height = 13
                Caption = 'coMapSmallBcdToNative'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coMapCharAsBDE: TLabel
                Left = 5
                Top = 283
                Width = 89
                Height = 13
                Caption = 'coMapCharAsBDE'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coEnableUnicode: TLabel
                Left = 4
                Top = 302
                Width = 85
                Height = 13
                Caption = 'coEnableUnicode'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coLockMode: TLabel
                Left = 4
                Top = 321
                Width = 63
                Height = 13
                Caption = 'coLockMode'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coReadOnly: TLabel
                Left = 4
                Top = 340
                Width = 59
                Height = 13
                Caption = 'coReadOnly'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coConTimeout: TLabel
                Left = 4
                Top = 359
                Width = 69
                Height = 13
                Caption = 'coConTimeout'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object Label4: TLabel
                Left = 240
                Top = 359
                Width = 147
                Height = 13
                Caption = 'Connection Timeout (seconds).'
                Color = 14346967
                ParentColor = False
              end
              object co_coSupportsSchemaFilter: TLabel
                Left = 4
                Top = 378
                Width = 56
                Height = 13
                Caption = 'coSchemFlt'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object co_coBlobFragmntns: TLabel
                Left = 4
                Top = 416
                Width = 82
                Height = 13
                Caption = 'coBlobFragmntns'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object sh_coBlobNotTermChar: TShape
                Left = 1
                Top = 584
                Width = 753
                Height = 19
                Align = alTop
                Brush.Color = 16114387
                Pen.Color = 14346967
              end
              object co_coBlobNotTermChar: TLabel
                Left = 4
                Top = 590
                Width = 96
                Height = 13
                Caption = 'coBlobNotTermChar'
                Color = 16114387
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object Label6: TLabel
                Left = 228
                Top = 319
                Width = 277
                Height = 13
                Caption = 'Wait lock timeout (seconds). "-1" - for endless expectation.'
                Color = 14346967
                ParentColor = False
              end
              object co_coQueryTimeout: TLabel
                Left = 4
                Top = 435
                Width = 67
                Height = 13
                Caption = 'coNetTimeout'
                Color = 14346967
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clOlive
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ParentColor = False
                ParentFont = False
              end
              object Label8: TLabel
                Left = 240
                Top = 435
                Width = 133
                Height = 13
                Caption = 'Network Timeout (seconds).'
                Color = 14346967
                ParentColor = False
              end
              object cbParamDateByOdbcLevel: TCheckBox
                Left = 146
                Top = 528
                Width = 353
                Height = 17
                Hint = 'Handling DateTime parameter follow odbc Level 2'
                Caption = 'Bind DateType Params as Odbc Level 3 DateTypes Columns Binding'
                Color = 14346967
                ParentColor = False
                ParentShowHint = False
                ShowHint = True
                TabOrder = 13
                OnClick = cbParamDateByOdbcLevelClick
              end
              object cbMaxBCD: TCheckBox
                Left = 146
                Top = 547
                Width = 255
                Height = 17
                Caption = 'BCD bind with greatest possible of the bcd size'
                Color = 16114387
                ParentColor = False
                TabOrder = 10
                OnClick = cbMaxBCDClick
              end
              object cbEmptyStrParam: TCheckBox
                Left = 146
                Top = 396
                Width = 176
                Height = 17
                Caption = 'Update empty string as NULL'
                Color = 14346967
                ParentColor = False
                TabOrder = 4
                OnClick = cbEmptyStrParamClick
              end
              object cbReadOnlyField: TCheckBox
                Left = 146
                Top = 509
                Width = 177
                Height = 17
                Caption = 'Ignore ReadOnly Fields Attribute'
                Checked = True
                Color = 16114387
                ParentColor = False
                State = cbChecked
                TabOrder = 14
                OnClick = cbReadOnlyFieldClick
              end
              object cbAutoInc: TCheckBox
                Left = 146
                Top = 490
                Width = 202
                Height = 17
                Hint =
                  'Disabling this option allow update AutoInc fields. It need for m' +
                  'igrating table data to other database/table.'
                Caption = 'Ignore Auto Increment Field Attribute'
                Checked = True
                Color = 14346967
                ParentColor = False
                ParentShowHint = False
                ShowHint = True
                State = cbChecked
                TabOrder = 12
                OnClick = cbAutoIncClick
              end
              object edDecimalSeparator: TEdit
                Left = 146
                Top = 223
                Width = 65
                Height = 19
                MaxLength = 1
                TabOrder = 15
                OnChange = edDecimalSeparatorChange
              end
              object cbInternalCloneConnection: TCheckBox
                Tag = 1
                Left = 146
                Top = 205
                Width = 179
                Height = 17
                Caption = 'Use Internal Clone Connection'
                Checked = True
                Color = 14346967
                ParentColor = False
                State = cbChecked
                TabOrder = 11
                OnClick = cbInternalCloneConnectionClick
              end
              object cbMixedFetch: TCheckBox
                Left = 146
                Top = 186
                Width = 107
                Height = 17
                Hint = 'This mode support only very qualitative the odbc drivers'
                Caption = 'Use Mixed Fetch'
                Color = 16114387
                ParentColor = False
                ParentShowHint = False
                ShowHint = True
                TabOrder = 7
                OnClick = cbMixedFetchClick
              end
              object edBlockRowsCount: TEdit
                Left = 146
                Top = 167
                Width = 65
                Height = 19
                TabOrder = 8
                Text = '20'
                OnChange = edBlockRowsCountChange
              end
              object edConPacketSize: TEdit
                Left = 146
                Top = 147
                Width = 65
                Height = 19
                TabOrder = 6
                Text = '8192'
                OnChange = edConPacketSizeChange
              end
              object edBlobChunkSize: TEdit
                Left = 146
                Top = 127
                Width = 65
                Height = 19
                TabOrder = 5
                Text = '40960'
                OnChange = edBlobChunkSizeChange
              end
              object cbBCD2Exp: TCheckBox
                Left = 146
                Top = 110
                Width = 347
                Height = 17
                Caption = 'BCD2Exp Format. Example: "1,2" == "12-E1". (Oracle 9)'
                Color = 16114387
                ParentColor = False
                TabOrder = 3
                OnClick = cbBCD2ExpClick
              end
              object cbEnableBCD: TCheckBox
                Left = 146
                Top = 91
                Width = 85
                Height = 17
                Caption = 'Enable BCD'
                Checked = True
                Color = 14346967
                ParentColor = False
                State = cbChecked
                TabOrder = 9
                OnClick = cbEnableBCDClick
              end
              object cbIgnoreUnknownFieldType: TCheckBox
                Left = 146
                Top = 72
                Width = 165
                Height = 17
                Caption = 'Ignore Unknown Field Type'
                Checked = True
                Color = 16114387
                ParentColor = False
                State = cbChecked
                TabOrder = 2
                OnClick = cbIgnoreUnknownFieldTypeClick
              end
              object cbMapInt64ToBcd: TCheckBox
                Left = 146
                Top = 53
                Width = 155
                Height = 17
                Caption = 'Map Int64(Big Int) to BCD'
                Checked = True
                Color = 14346967
                ParentColor = False
                State = cbChecked
                TabOrder = 1
                OnClick = cbMapInt64ToBcdClick
              end
              object cbTrimChar: TCheckBox
                Left = 146
                Top = 34
                Width = 125
                Height = 17
                Caption = 'Trim Fixed Char'
                Checked = True
                Color = 16114387
                ParentColor = False
                State = cbChecked
                TabOrder = 0
                OnClick = cbTrimCharClick
              end
              object cbNullStrParam: TCheckBox
                Left = 146
                Top = 567
                Width = 199
                Height = 17
                Caption = 'Update NULL string as empty string'
                Color = 14346967
                ParentColor = False
                TabOrder = 16
                OnClick = cbNullStrParamClick
              end
              object cbSafeMode: TCheckBox
                Tag = 1
                Left = 146
                Top = 244
                Width = 179
                Height = 17
                Caption = 'Safe(Silent) Mode'
                Checked = True
                Color = 14346967
                ParentColor = False
                State = cbChecked
                TabOrder = 17
                OnClick = cbSafeModeClick
              end
              object cbMapSmallBcdToNative: TCheckBox
                Left = 146
                Top = 262
                Width = 165
                Height = 17
                Caption = 'Map SmallBcd To Native'
                Color = 16114387
                ParentColor = False
                TabOrder = 18
                OnClick = cbMapSmallBcdToNativeClick
              end
              object cbMapCharAsBDE: TCheckBox
                Tag = 1
                Left = 146
                Top = 281
                Width = 179
                Height = 17
                Caption = 'Map Char As BDE'
                Color = 14346967
                ParentColor = False
                TabOrder = 19
                OnClick = cbMapCharAsBDEClick
              end
              object cbEnableUnicode: TCheckBox
                Left = 146
                Top = 300
                Width = 404
                Height = 17
                Caption =
                  'Enable Unicode (uses SqlExprFix.pas; and SqlExprFix.bFixedSqlExp' +
                  'r = True)'
                Color = 16114387
                ParentColor = False
                TabOrder = 20
                OnClick = cbEnableUnicodeClick
              end
              object cbReadOnly: TCheckBox
                Left = 146
                Top = 338
                Width = 404
                Height = 17
                Caption = 'Read Only Connection'
                Color = 16114387
                ParentColor = False
                TabOrder = 21
                OnClick = cbReadOnlyClick
              end
              object edLockMode: TEdit
                Left = 146
                Top = 318
                Width = 65
                Height = 19
                MaxLength = 1
                ParentShowHint = False
                ShowHint = True
                TabOrder = 22
                Text = '60'
                OnChange = edLockModeChange
              end
              object edConTimeout: TEdit
                Left = 146
                Top = 356
                Width = 65
                Height = 19
                MaxLength = 1
                ParentShowHint = False
                ShowHint = True
                TabOrder = 23
                Text = '-1'
                OnChange = edConTimeoutChange
              end
              object cbSupportsSchemaFilter: TCheckBox
                Left = 146
                Top = 376
                Width = 404
                Height = 17
                Caption = 'Supports Schema Filter (Oracle)'
                Color = 16114387
                ParentColor = False
                TabOrder = 24
                OnClick = cbSupportsSchemaFilterClick
              end
              object cbBlobFragmntns: TCheckBox
                Left = 146
                Top = 414
                Width = 501
                Height = 17
                Caption =
                  'Allows fragmentation of blob in memory when blob fetching (Minim' +
                  'ization of memory reallocation).'
                Checked = True
                Color = 16114387
                ParentColor = False
                State = cbChecked
                TabOrder = 25
                OnClick = cbBlobFragmntnsClick
              end
              object cbBlobNotTermChar: TCheckBox
                Left = 146
                Top = 586
                Width = 481
                Height = 17
                Caption =
                  'Determines the not use of TerminationCharSize, at the fetch of t' +
                  'ext blob'
                Color = 16114387
                ParentColor = False
                TabOrder = 26
                OnClick = cbBlobNotTermCharClick
              end
              object edQueryTimeout: TEdit
                Left = 146
                Top = 432
                Width = 65
                Height = 19
                MaxLength = 1
                ParentShowHint = False
                ShowHint = True
                TabOrder = 27
                Text = '-1'
                OnChange = edQueryTimeoutChange
              end
            end
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'Query'
          object pDataNav: TPanel
            Left = 0
            Top = 250
            Width = 757
            Height = 27
            Align = alBottom
            BevelOuter = bvLowered
            TabOrder = 0
            object dbQryLastAccessed: TDBText
              Left = 472
              Top = 10
              Width = 107
              Height = 13
              DataField = 'id'
              DataSource = sCDSQuery
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clHotLight
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object lbQryLastAccessedCap: TLabel
              Left = 364
              Top = 10
              Width = 104
              Height = 13
              Caption = 'Query Last Accessed:'
            end
            object lbQryCntCap: TLabel
              Left = 582
              Top = 10
              Width = 93
              Height = 13
              Caption = 'Queries Pos/Count:'
            end
            object lbQryCnt: TLabel
              Left = 678
              Top = 10
              Width = 30
              Height = 13
              Caption = '00000'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clHotLight
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object pbOpen: TBitBtn
              Left = 4
              Top = 2
              Width = 63
              Height = 24
              Hint = 'Open && Fetch or Close'
              Caption = 'Open'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              OnClick = pbOpenClick
              Glyph.Data = {
                F6000000424DF600000000000000760000002800000010000000100000000100
                0400000000008000000000000000000000001000000000000000000000000000
                8000008000000080800080000000800080008080000080808000C0C0C0000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
                8888888888888888888888888888888888888888884444888888888884BBBB48
                8888888884BBBB488888888884BBBB488888888884BBBB488888888884BBBB48
                8888888884BBBB488888888884BBBB488888888884BBBB488888888888444488
                8888888888888888888888888888888888888888888888888888}
            end
            object pbExecute: TBitBtn
              Left = 67
              Top = 2
              Width = 68
              Height = 24
              Hint = 'Execute Script Command'
              Caption = 'Execute'
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              OnClick = pbExecuteClick
              Glyph.Data = {
                F6000000424DF600000000000000760000002800000010000000100000000100
                0400000000008000000000000000000000001000000000000000000000000000
                8000008000000080800080000000800080008080000080808000C0C0C0000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
                8888888888888888888888888888888888888888848888888888888884488888
                8888888884B488888888888884BB48888888888884BBB4888888888884BBBB48
                8888888884BBB4888888888884BB48888888888884B488888888888884488888
                8888888884888888888888888888888888888888888888888888}
            end
            object dbNavQuery: TDBNavigator
              Left = 135
              Top = 2
              Width = 225
              Height = 24
              DataSource = sCDSQuery
              VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
              Hints.Strings = (
                'First Query'
                'Prior Query'
                'Next Query'
                'Last Query'
                'Insert New Query'
                'Delete Saved Query'
                'Edit Query'
                'Save Current Query'
                'Cancel edit Query'
                'Refresh data')
              ParentShowHint = False
              ConfirmDelete = False
              ShowHint = True
              TabOrder = 2
              BeforeAction = dbNavQueryBeforeAction
            end
          end
          object pMemoCli: TPanel
            Left = 0
            Top = 0
            Width = 757
            Height = 250
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object pQueryOpt: TPanel
              Left = 646
              Top = 0
              Width = 111
              Height = 250
              Align = alRight
              BevelOuter = bvNone
              TabOrder = 0
              object lbScript: TLabel
                Left = 12
                Top = 80
                Width = 93
                Height = 13
                Caption = 'Script Line Delimiter'
              end
              object Label5: TLabel
                Left = 28
                Top = 144
                Width = 72
                Height = 26
                Caption = 'Comments from'#13#10'Script'
              end
              object cbGetMetadata: TCheckBox
                Left = 8
                Top = 8
                Width = 97
                Height = 17
                Caption = 'Get Metadata'
                Checked = True
                State = cbChecked
                TabOrder = 0
                OnClick = cbGetMetadataClick
              end
              object edPacketRecords: TEdit
                Left = 8
                Top = 56
                Width = 95
                Height = 21
                Enabled = False
                TabOrder = 1
                Text = '20'
                OnChange = edPacketRecordsChange
              end
              object cbPacketRecords: TCheckBox
                Left = 8
                Top = 32
                Width = 97
                Height = 17
                Caption = 'Packet Records'
                TabOrder = 2
                OnClick = cbPacketRecordsClick
              end
              object edScriptLD: TEdit
                Left = 8
                Top = 96
                Width = 97
                Height = 21
                TabOrder = 3
                Text = '^'
              end
              object cbRemoveScriptComments: TCheckBox
                Left = 8
                Top = 124
                Width = 69
                Height = 21
                Hint = 'Remove Comments from Script'
                Caption = 'Remove'
                Checked = True
                ParentShowHint = False
                ShowHint = True
                State = cbChecked
                TabOrder = 4
              end
              object pbDetach: TBitBtn
                Left = 4
                Top = 176
                Width = 105
                Height = 25
                Hint =
                  'Debug: Detach Cursor (Lock SqlHStmt).  It will allow to debug is' +
                  ' dependent transactions and new cursors on the cursors non-fetch' +
                  'ed up to the end.'
                Caption = 'Detach Cursor'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 5
                OnClick = pbDetachClick
              end
            end
            object pSQL: TPanel
              Left = 0
              Top = 0
              Width = 646
              Height = 250
              Align = alClient
              BevelOuter = bvNone
              Caption = 'Database SQL Editor'
              Color = clBtnShadow
              TabOrder = 1
            end
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'Memo'
          object lbMemoData: TLabel
            Left = 0
            Top = 0
            Width = 774
            Height = 26
            Align = alTop
            AutoSize = False
            Caption = ' Memo Data Field:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold, fsStrikeOut]
            ParentFont = False
          end
          object DBMemo1: TDBMemo
            Left = 0
            Top = 26
            Width = 774
            Height = 251
            Align = alClient
            BorderStyle = bsNone
            DataSource = DataSource1
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 0
          end
          object cmMemo: TComboBox
            Left = 115
            Top = 0
            Width = 250
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 1
            OnChange = cmMemoChange
          end
          object btMemoSaveToFile: TBitBtn
            Left = 381
            Top = 1
            Width = 88
            Height = 23
            Caption = 'Save To File'
            TabOrder = 2
            OnClick = btMemoSaveToFileClick
          end
          object btMemoLoadFromFile: TBitBtn
            Left = 469
            Top = 1
            Width = 88
            Height = 23
            Caption = 'Load From  File'
            TabOrder = 3
            OnClick = btMemoLoadFromFileClick
          end
          object btMemoClear: TBitBtn
            Left = 557
            Top = 1
            Width = 57
            Height = 23
            Caption = 'Clear'
            TabOrder = 4
            OnClick = btMemoClearClick
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'Image'
          object pImage: TPanel
            Left = 0
            Top = 0
            Width = 774
            Height = 277
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lbImageData: TLabel
              Left = 0
              Top = 0
              Width = 774
              Height = 26
              Align = alTop
              AutoSize = False
              Caption = ' Image Data Field:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold, fsStrikeOut]
              ParentFont = False
            end
            object cmImage: TComboBox
              Left = 115
              Top = 0
              Width = 250
              Height = 21
              Style = csDropDownList
              ItemHeight = 0
              TabOrder = 0
              OnChange = cmImageChange
            end
            object sbImage: TScrollBox
              Left = 0
              Top = 26
              Width = 774
              Height = 251
              HorzScrollBar.Tracking = True
              VertScrollBar.Tracking = True
              Align = alClient
              BorderStyle = bsNone
              PopupMenu = pmImage
              TabOrder = 1
              object DBImage1: TDBImage
                Left = 0
                Top = 0
                Width = 90
                Height = 90
                AutoDisplay = False
                BorderStyle = bsNone
                DataSource = DataSource1
                PopupMenu = pmImage
                TabOrder = 0
              end
            end
            object btImageSaveToFile: TBitBtn
              Left = 381
              Top = 1
              Width = 88
              Height = 23
              Caption = 'Save To File'
              TabOrder = 2
              OnClick = pmImageSaveClick
            end
            object btImageLoadFromFile: TBitBtn
              Left = 469
              Top = 1
              Width = 88
              Height = 23
              Caption = 'Load From  File'
              TabOrder = 3
              OnClick = pmImageLoadClick
            end
            object btImageClear: TBitBtn
              Left = 557
              Top = 1
              Width = 57
              Height = 23
              Caption = 'Clear'
              TabOrder = 4
              OnClick = miImageClearClick
            end
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'Binary'
          object pBinary: TPanel
            Left = 0
            Top = 0
            Width = 774
            Height = 277
            Align = alClient
            BevelOuter = bvNone
            Caption = 'Hex Editor Not Presented'
            TabOrder = 0
            OnResize = pBinaryResize
            object lbBinaryData: TLabel
              Left = 0
              Top = 0
              Width = 774
              Height = 26
              Align = alTop
              AutoSize = False
              Caption = ' Binary Data Field:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cmBinary: TComboBox
              Left = 115
              Top = 0
              Width = 250
              Height = 21
              Style = csDropDownList
              ItemHeight = 0
              TabOrder = 0
              OnChange = cmBinaryChange
            end
            object cbUnicodeChars: TCheckBox
              Left = 632
              Top = 4
              Width = 105
              Height = 17
              Caption = 'Unicode Chars'
              TabOrder = 4
              Visible = False
              OnClick = cbUnicodeCharsClick
            end
            object btBlobSave: TBitBtn
              Left = 381
              Top = 0
              Width = 88
              Height = 23
              Caption = 'Save To File'
              TabOrder = 1
              Visible = False
              OnClick = btBlobSaveClick
            end
            object btBlobLoad: TBitBtn
              Left = 469
              Top = 0
              Width = 88
              Height = 23
              Caption = 'Load From  File'
              TabOrder = 2
              Visible = False
              OnClick = btBlobLoadClick
            end
            object btBlobClear: TBitBtn
              Left = 557
              Top = 0
              Width = 57
              Height = 23
              Caption = 'Clear'
              TabOrder = 3
              Visible = False
              OnClick = btBlobClearClick
            end
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'SQLMonitor'
          object mem_sql_monitor: TMemo
            Left = 0
            Top = 0
            Width = 640
            Height = 277
            Align = alClient
            Color = clBlack
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clLime
            Font.Height = -16
            Font.Name = 'Times New Roman'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 0
            WantTabs = True
            WordWrap = False
          end
          object p_sql_monitor_btns: TPanel
            Left = 640
            Top = 0
            Width = 134
            Height = 277
            Align = alRight
            TabOrder = 1
            object btn_sql_monitor_clear: TButton
              Left = 9
              Top = 32
              Width = 116
              Height = 25
              Caption = 'Cear Monitor Log'
              TabOrder = 1
              OnClick = btn_sql_monitor_clearClick
            end
            object cb_sql_monitor_active: TCheckBox
              Left = 8
              Top = 8
              Width = 121
              Height = 17
              Caption = 'Active SQL Monitor'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = cb_sql_monitor_activeClick
            end
          end
        end
      end
      object TabSet1: TTabSet
        Left = 0
        Top = 277
        Width = 774
        Height = 21
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Tabs.Strings = (
          '&Connection'
          '&Query'
          '&Memo'
          '&Image'
          'Bi&nary'
          '&SQLMonitor')
        TabIndex = 0
        OnChange = TabSet1Change
      end
    end
  end
  object SB: TStatusBar
    Left = 0
    Top = 568
    Width = 776
    Height = 19
    Panels = <
      item
        Width = 0
      end>
    Visible = False
  end
  object SQLConnection1: TSQLConnection
    ConnectionName = 'OpenOdbc:Prompt'
    DriverName = 'OpenOdbc'
    GetDriverFunc = 'getSQLDriverODBC'
    LibraryName = 'dbxoodbc1.dll'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=OpenOdbc'
      'Database=?'
      'User_Name='
      'Password='
      'BlobSize=-1'
      'RowsetSize=20'
      'Trim Char=True'

        'Custom String=coNetPacketSize=8192;coLockMode=17;coBlobChunkSize' +
        '=40960')
    VendorLib = 'odbc32.dll'
    AfterConnect = SQLConnection1AfterConnect
    AfterDisconnect = SQLConnection1AfterDisconnect
    BeforeConnect = SQLConnection1BeforeConnect
    BeforeDisconnect = SQLConnection1BeforeDisconnect
    Left = 64
    Top = 464
  end
  object DataSetProvider1: TDataSetProvider
    DataSet = SQLDataSet1
    UpdateMode = upWhereKeyOnly
    OnUpdateError = DataSetProvider1UpdateError
    BeforeUpdateRecord = DataSetProvider1BeforeUpdateRecord
    Left = 304
    Top = 468
  end
  object CDS: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DataSetProvider1'
    AfterOpen = CDSAfterOpen
    BeforeClose = CDSBeforeClose
    AfterClose = CDSAfterClose
    AfterEdit = CDSAfterEdit
    BeforePost = CDSBeforePost
    BeforeCancel = CDSBeforeCancel
    AfterCancel = CDSAfterCancel
    AfterScroll = CDSAfterScroll
    AfterRefresh = CDSAfterRefresh
    Left = 560
    Top = 468
  end
  object DataSource1: TDataSource
    DataSet = CDS
    OnDataChange = DataSource1DataChange
    Left = 612
    Top = 468
  end
  object SQLDataSet1: TSQLDataSet
    MaxBlobSize = -1
    ParamCheck = False
    Params = <>
    SQLConnection = SQLConnection1
    Left = 172
    Top = 464
  end
  object pmImage: TPopupMenu
    OnPopup = pmImagePopup
    Left = 692
    Top = 172
    object PopMenuImageCut: TMenuItem
      Caption = 'Cut'
      OnClick = PopMenuImageCutClick
    end
    object PopMenuImageCopy: TMenuItem
      Caption = 'Copy'
      OnClick = PopMenuImageCopyClick
    end
    object PopMenuImagePaste: TMenuItem
      Caption = 'Paste'
      OnClick = PopMenuImagePasteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miImageClear: TMenuItem
      Caption = 'Clear'
      OnClick = miImageClearClick
    end
    object pmImageRecompess: TMenuItem
      Caption = 'Recompress'
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object pmImageLoad: TMenuItem
      Caption = 'Load From File'
      OnClick = pmImageLoadClick
    end
    object pmImageSave: TMenuItem
      Caption = 'Save To File'
      OnClick = pmImageSaveClick
    end
  end
  object PopupMenuTree: TPopupMenu
    OnPopup = PopupMenuTreePopup
    Left = 168
    Top = 296
    object mnuGenSqlSelect: TMenuItem
      Caption = 'Generate Sql: Select * From Table'
      OnClick = mnuGenSqlCreateTableClick
    end
    object mnuGenerateSqlSelectEach: TMenuItem
      Caption = 'Generate Sql: Select (each column) From Table'
      OnClick = mnuGenSqlCreateTableClick
    end
    object mnuGenSqlInsert: TMenuItem
      Caption = 'Generate Sql: Insert Into Table'
      OnClick = mnuGenSqlCreateTableClick
    end
    object mnuGenerateSqlUpdate: TMenuItem
      Caption = 'Generate Sql: Update Table'
      OnClick = mnuGenSqlCreateTableClick
    end
    object mnuGenSqlCreateTable: TMenuItem
      Caption = 'Generate Sql: Create Table'
      OnClick = mnuGenSqlCreateTableClick
    end
    object pmnuGenerateSqlDrop: TMenuItem
      Caption = 'Generate Sql: Drop Table'
      OnClick = mnuGenSqlCreateTableClick
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object pmnuShowData: TMenuItem
      Caption = 'Show Data'
      ShortCut = 32
      OnClick = mnuGenSqlCreateTableClick
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object pmnuRefreshAll: TMenuItem
      Caption = 'Refresh All'
      ShortCut = 116
      OnClick = pmnuRefreshAllClick
    end
  end
  object CDSQuery: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <
      item
        Name = 'id'
        Fields = 'id'
        Options = [ixPrimary]
      end>
    IndexFieldNames = 'id'
    Params = <>
    StoreDefs = True
    BeforePost = CDSQueryBeforePost
    Left = 394
    Top = 466
    object CDSQueryid: TSQLTimeStampField
      FieldName = 'id'
    end
    object CDSQueryqry: TMemoField
      FieldName = 'qry'
      BlobType = ftMemo
    end
  end
  object sCDSQuery: TDataSource
    DataSet = CDSQuery
    OnDataChange = sCDSQueryDataChange
    Left = 486
    Top = 465
  end
  object MainMenu1: TMainMenu
    Left = 445
    Top = 173
    object File1: TMenuItem
      Caption = 'File'
      object Save1: TMenuItem
        Caption = 'Save'
        object SaveData1: TMenuItem
          Caption = 'Save Data'
          ShortCut = 113
          OnClick = SaveData1Click
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object SaveOptions1: TMenuItem
          Caption = 'Save Options'
          ShortCut = 8305
          OnClick = SaveOptions1Click
        end
      end
      object Open1: TMenuItem
        Caption = 'Open'
        object OpenData1: TMenuItem
          Caption = 'Open Data'
          ShortCut = 16463
          OnClick = OpenData1Click
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object OpenSavedQueries1: TMenuItem
          Caption = 'Open Saved Queries'
          Enabled = False
          ShortCut = 114
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      Enabled = False
    end
    object Options1: TMenuItem
      Caption = 'Options'
      Enabled = False
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object Glosary1: TMenuItem
        Caption = 'Glosary'
        Enabled = False
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object pmMemo: TPopupMenu
    Left = 638
    Top = 173
    object pmMemoFont: TMenuItem
      Caption = 'Font'
      object pnMemoFontPlus: TMenuItem
        Caption = 'Increase Font Size'
        ShortCut = 16605
        OnClick = pnMemoFontPlusClick
      end
      object pnMemoFontMinus: TMenuItem
        Caption = 'Decrease Font Size'
        ShortCut = 16603
        OnClick = pnMemoFontMinusClick
      end
    end
    object pmMemoWordWrap: TMenuItem
      Caption = 'Word Wrap'
      OnClick = pmMemoWordWrapClick
    end
  end
  object OPD: TOpenPictureDialog
    InitialDir = '.'
    Title = 'Open Image'
    Left = 542
    Top = 173
  end
  object SPD: TSavePictureDialog
    DefaultExt = '*.*'
    Filter =
      'All Images(*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf)|*.jpg;*.jpeg;*.' +
      'bmp;*.ico;*.emf;*.wmf|JPEG Image File (*.jpg;(.jpeg)|*.jpg;*.jpe' +
      'g|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Metafiles (*.wmf;*.e' +
      'mf)|*.wmf;*.emf|All(*.*)|*.*'
    InitialDir = '.'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 494
    Top = 173
  end
  object ImageList: TImageList
    ShareImages = True
    Left = 172
    Top = 339
    Bitmap = {
      494C010117001800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000006000000001002000000000000060
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
      0000000000000000000000000000000000000084000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      0000000000000000000000000000000000008442000084420000844200008442
      0000844200008442000084420000844200008442000084420000844200008442
      000000000000000000000000FF00000000000084000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      0000008400000084000000840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000084840000848400008484
      0000848400008484000084840000848400008484000084840000848400000084
      000000000000000000000000FF00000000008442000084840000848400008484
      0000848400008484000084840000848400008484000084840000848400008442
      0000000000000000FF000000FF000000FF000084000084840000848400008484
      0000848400008484000084840000848400008484000084840000848400008484
      0000848400008484000084840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF00000084
      000000000000000000000000FF000000000084420000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008442
      000000000000000000000000FF000000000000840000848400008484000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00008484000084840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000000FF0000008400000084
      000000840000008400000084000000840000008400000084000000FF00000084
      000000000000000000000000FF000000000084420000FFFF0000844200008442
      0000844200008442000084420000844200008442000084420000FFFF00008442
      000000000000000000000000FF000000000000840000848400008484000000FF
      0000008400000084000000840000008400000084000000840000008400000084
      000000FF00008484000084840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000000FF0000008400000084
      000000840000008400000084000000840000008400000084000000FF00000084
      000000000000000000000000FF000000000084420000FFFF0000844200008442
      0000844200008442000084420000844200008442000084420000FFFF00008442
      000000000000000000000000FF000000000000840000848400008484000000FF
      0000008400000084000000840000008400000084000000840000008400000084
      000000FF00008484000084840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF00000084
      000000000000000000000000FF000000000084420000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008442
      000000000000000000000000FF000000000000840000848400008484000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00008484000084840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000084840000848400008484
      0000848400008484000084840000848400008484000084840000848400000084
      0000000000000000FF000000FF000000FF008442000084840000848400008484
      0000848400008484000084840000848400008484000084840000848400008442
      000000000000000000000000FF00000000000084000084840000848400008484
      0000848400008484000084840000848400008484000084840000848400008484
      0000848400008484000084840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      000000000000000000000000FF00000000008442000084420000844200008442
      0000844200008442000084420000844200008442000084420000844200008442
      0000000000000000000000000000000000000084000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      0000008400000084000000840000008400000000000000000000000000000000
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
      0000000000000000000000000000000000000000000084840000000000000000
      0000000000008484000000000000000000000000000084840000000000000000
      0000000000008484000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084840000008400000000
      0000000000008484000000840000000000000000000084840000008400000000
      000000000000848400000084000000000000FF0000000000000000000000FF00
      0000FF0000000000000000000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000008484000000000000000000008484
      0000848400000000000000000000008400000084000000840000008400000084
      0000008400000084000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084840000008400000000
      0000000000008484000000840000000000000000000084840000008400000000
      000000000000848400000084000000000000FF00000084000000000000000000
      0000FF0000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484000000840000000000000000
      0000848400000084000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      00000000FF0000000000000000000000FF000000000084840000848400008484
      0000000000008484000000840000000000000000000084840000008400000000
      000000000000848400000084000000000000FF00000084000000000000000000
      0000FF0000008400000000000000000000000000000000A58400008400000084
      00000084000000840000C6C68400000000008484000000840000000000000000
      00008484000000840000000000000000000000000000FF848400844200008442
      00008442000084420000FF848400000000000000000000000000000000000000
      0000C6C6C6000000000000000000000000000000FF0000000000000000000000
      00000000FF00000000000000FF00000000000000000000000000000000000000
      0000000000008484000000840000000000000000000084840000008400000000
      000000000000848400000084000000000000FF000000FF000000FF0000000000
      0000FF00000084000000000000000000000084C6C6000084000084A50000C6DE
      C60000A5A50000840000C6C68400000000008484000084840000848400000000
      000084840000008400000000000000000000FF8484008442000084A50000C6DE
      C600FF84840084420000FF848400000000000000000000000000000000000000
      0000C6C6C6000084840000000000000000000000FF000000FF000000FF000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000008484000000840000000000000000000084840000008400000000
      0000000000008484000000840000000000000000000000000000000000000000
      0000FF00000084000000000000000000000084C6C60000840000C6A500000000
      000000A5A50000840000C6C68400000000000000000000000000000000000000
      000084840000008400000000000000000000FF84840084420000FF8400000000
      0000FF84840084420000FF848400000000000000000000000000000000000000
      0000C6C6C6000000000000000000000000000000FF00000000000000FF000000
      00000000FF00000000000000FF00000000000000000000000000000000000000
      0000000000008484000000840000000000000000000084840000008400000000
      0000000000008484000000840000000000000000000000000000000000000000
      0000FF00000084000000000000000000000084C6C60000840000C6A500000000
      000000A5A50000840000C6C68400000000000000000000000000000000000000
      000084840000008400000000000000000000FF84840084420000FF8400000000
      0000FF84840084420000FF848400000000000000000000000000000000000000
      0000C6C6C6000084840000000000000000000000FF00000000000000FF000000
      00000000FF00000000000000FF00000000000000000000000000000000000000
      0000000000008484000084840000848400000000000084840000008400000000
      0000000000008484000000840000000000000000000000000000000000000000
      0000FF00000084000000000000000000000084C6C60000840000C6A500000000
      000000A5A50000840000C6C68400000000000000000000000000000000000000
      000084840000008400000000000000000000FF84840084420000FF8400000000
      0000FF84840084420000FF848400000000000000000000000000000000000000
      0000C6C6C6000000000000000000000000000000FF000000FF000000FF000000
      00000000FF0000000000000000000000FF000000000000008400000084000000
      8400000000000000000000000000000000000000000084840000008400000000
      0000000000008484000000840000000000000000000000000000000000000000
      0000FF000000FF000000FF0000000000000084C6C60000840000C6A500000000
      000000A5A50000840000C6C68400000000000000000000000000000000000000
      000084840000848400008484000000000000FF84840084420000FF8400000000
      0000FF84840084420000FF848400000000000000000000000000000000000084
      8400C6C6C6000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000084000000
      0000000000000000000000000000000000000000000084840000008400000000
      0000000000008484000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000084000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C6000084840000848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      8400000000000000000000000000000000000000000084840000008400000000
      0000000000008484000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00C6C6
      C600000000000084840000848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000084840000848400008484
      0000000000008484000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF00C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000840000000000000000000000000000000000000000000000
      0000000000008484000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF0000FFFF00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000008484000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484000084840000848400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
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
      00000000000000000000000000000000000000000000FF000000000000000000
      000000000000FF000000000000000000000000000000FF000000000000000000
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000840000000000
      000000000000FF000000840000000000000000000000FF000000840000000000
      000000000000FF00000084000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      00000000000000000000000000000000000000000000FF000000840000000000
      000000000000FF000000840000000000000000000000FF000000840000000000
      000000000000FF00000084000000000000000000000000000000000000000000
      0000000000000000000084000000FF840000FF84000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF000000000000000000000000000000000000FF000000FF000000FF00
      000000000000FF000000840000000000000000000000FF000000840000000000
      000000000000FF00000084000000000000000000000000000000000000000000
      00000000000084000000FF840000FF840000FF000000FF840000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000042636300C684
      0000C6840000C6C60000C6C60000C6C60000FFFF0000FFFF0000FFFF0000FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000840000000000000000000000FF000000840000000000
      000000000000FF00000084000000000000000000000000000000000000000000
      000084000000FF840000FF840000FF00000000000000FF840000FF8400008400
      0000000000000000000000000000000000000000000042420000424200004242
      0000424200004242000042420000424200004242000042420000424200004242
      000042420000424200004242000000000000000000000000000042636300C6A5
      8400C6846300C6840000C6840000C6A5000084840000C6A50000C6C60000FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000840000000000000000000000FF000000840000000000
      000000000000FF00000084000000000000000000000000000000000000008400
      0000FF840000FF840000FF000000FF848400FF84000000000000FF840000FF84
      0000840000000000000000000000000000004242000042420000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      840084848400848484004242000042420000000000000000000042636300F7CE
      A500C6848400C6632100F7CEA500FFFFFF00F7CEA500C6840000C6840000C6C6
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000840000000000000000000000FF000000840000000000
      000000000000FF0000008400000000000000000000000000000000000000FF84
      0000FF840000FF000000FF848400FF848400FF000000FF84000000000000FF84
      0000FF840000840000000000000000000000424200008484840084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00848484008484840042420000000000000000000042636300F7CE
      A500C6A56300C6422100C6632100FFFFFF00C6422100C6840000C6840000C6A5
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF000000FF00000000000000FF000000840000000000
      000000000000FF00000084000000000000000000000000000000FF000000FF84
      0000FF000000FF848400FF848400FF000000FF848400FF000000FF8400000000
      0000FF840000FF0000000000000000000000424200008484840084848400FFFF
      FF00424200004242000042420000424200004242000042420000424200004242
      0000FFFFFF00848484008484840042420000000000000000000042636300F7CE
      A500C6846300C6632100C6632100FFFFFF00C6632100C6632100C6840000C684
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000840000000000
      000000000000FF0000008400000000000000000000000000000000000000FF00
      0000FF848400FF848400FF000000FF848400FF000000FF848400FF000000FF84
      000000000000000000000000000000000000424200008484840084848400FFFF
      FF00424200004242000042420000424200004242000042420000424200004242
      0000FFFFFF00848484008484840042420000000000000000000042636300F7CE
      A500C6A58400C6632100F7CEA500FFFFFF00C6632100C6422100C6632100C684
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000840000000000
      000000000000FF00000084000000000000000000000000000000000000000000
      0000FF000000FF848400FF848400FF000000FF848400FF000000FF8400000000
      000000000000000000000000000000000000424200008484840084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00848484008484840042420000000000000000000042636300F7CE
      A500C6A58400C6848400C6846300C6A58400C6846300C6846300C6636300C684
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000840000000000
      000000000000FF00000084000000000000000000000000000000000000000000
      000000000000FF000000FF848400FF848400FF000000FF840000000000000000
      0000000000000000000000000000000000004242000042420000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      840084848400848484004242000042420000000000000000000042636300F7CE
      A500F7CEA500F7CEA500C6848400FFFFFF00C6848400C6A56300C6848400C684
      6300000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      000000000000FF00000084000000000000000000000000000000000000000000
      00000000000000000000FF000000FF848400FF84000000000000000000000000
      0000000000000000000000000000000000000000000042420000424200004242
      0000424200004242000042420000424200004242000042420000424200004242
      000042420000424200004242000000000000000000000000000042638400C6A5
      8400F7CEA500F7CEA500F7CEA500F7CEA500F7CEA500F7CEA500F7CEA500C6A5
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000084000000000000000000000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004263
      6300426363004263630042636300426363004263630042636300426363004263
      6300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF000000FF0000000000000000000000000000000000
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
      0000848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484000084840000848400008484000084840000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000848484008484000000000000000000000000000084840000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00008400000084000000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00424200004242000000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000084000000840000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000008484840084848400848484008484
      8400848484008484000000000000848400008484000084840000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000008484840000FFFF0000FFFF0000FF
      FF00848484008484000000000000000000000000000084840000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00008400000084000000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00424200004242000000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000084000000840000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000008484840000FFFF00000000000000
      0000848484008484000084840000848400000000000084848400848484008484
      8400848484008484840084848400848484000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000008484840000FFFF0000FFFF000000
      0000848484008484000000000000000000000000000084848400848400008484
      0000848400008484000084840000848484000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00008400000084000000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00424200004242000000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000084000000840000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000008484840000FFFF0000FFFF000000
      0000848484008484000084840000848400008484000084848400848400000000
      0000000000000000000084840000848484000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000008484840000FFFF00000000000000
      0000848484008484840084848400848484008484840084848400848400008484
      0000848400000000000084840000848484000000000000000000008400000084
      0000000000000084000000840000008400000084000000840000008400000084
      0000008400000084000000000000000000000000000000000000424200004242
      0000000000004242000042420000424200004242000042420000424200004242
      0000424200004242000000000000000000000000000000000000000084000000
      8400000000000000840000008400000084000000840000008400000084000000
      8400000084000000840000000000000000008484840000FFFF0000FFFF000000
      000000FFFF0000FFFF0084848400000000000000000084848400848400000000
      0000000000000000000084840000848484000000000000000000C6C6C60000FF
      0000C6C6C60000FF0000C6C6C60000FF0000C6C6C60000FF0000C6C6C60000FF
      0000C6C6C60000FF000000000000000000000000000000000000C6C6C6008484
      8400C6C6C60084848400C6C6C60084848400C6C6C60084848400C6C6C6008484
      8400C6C6C6008484840000000000000000000000000000000000C6C6C6000000
      FF00C6C6C6000000FF00C6C6C6000000FF00C6C6C6000000FF00C6C6C6000000
      FF00C6C6C6000000FF0000000000000000008484840000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0084848400000000000000000084848400848400008484
      000084840000000000008484000084848400000000000000000000FF0000C6C6
      C60000FF0000C6C6C60000FF0000C6C6C60000FF0000C6C6C60000FF0000C6C6
      C60000FF0000C6C6C6000000000000000000000000000000000084848400C6C6
      C60084848400C6C6C60084848400C6C6C60084848400C6C6C60084848400C6C6
      C60084848400C6C6C600000000000000000000000000000000000000FF00C6C6
      C6000000FF00C6C6C6000000FF00C6C6C6000000FF00C6C6C6000000FF00C6C6
      C6000000FF00C6C6C60000000000000000008484840084848400848484008484
      8400848484008484840084848400000000000000000084848400848400000000
      0000000000000000000084840000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848400008484
      0000848400008484000084840000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840084848400848484000000000000000000000000000000
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
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004284FF004284
      FF004284FF004284FF004284FF004284FF004284FF004284FF004284FF004284
      FF004284FF004284FF000000000000000000000000000000000084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084FF
      FF0084FFFF0084FFFF0000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004284FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004284FF000000000000000000000000000000000084FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084FFFF0000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF000000FF00C6C6C600FFFFFF000000FF00FFFF
      FF000000FF00FFFFFF0084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00000000000000000000000000000000004284FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00000000004284FF000000000000000000000000000000000084FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000084FFFF0000000000000000000000000000000000000000008484
      84008484840084848400FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF000000
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00840000008400000000000000FFFFFF000000
      0000FFFFFF00FFFFFF00000000000000000000000000000000004284FF000000
      0000FFFFFF000000840000008400FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00000000004284FF000000000000000000000000000000000084FFFF000000
      0000FFFFFF000084840000848400FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000084FFFF000000000000000000000000008484840084848400C6C6
      C600C6C6C60084848400FFFFFF00FFFFFF000000FF00FFFFFF000000FF00FFFF
      FF000000FF00FFFFFF0084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00000000000000000000000000000000004284FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00000000004284FF000000000000000000000000000000000084FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000084FFFF0000000000000000008484840000FFFF000084840000FF
      FF0000FFFF0084848400FFFFFF000000FF000000FF000000FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00840000008400000000000000FFFFFF000000
      0000FFFFFF00FFFFFF00000000000000000000000000000000004284FF000000
      0000FFFFFF000000840000008400FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00000000004284FF000000000000000000000000000000000084FFFF000000
      0000FFFFFF000084840000848400FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000084FFFF000000000000000000848484000084840000FFFF0000FF
      FF0000FFFF0084848400FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00000000000000000000000000000000004284FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00000000004284FF000000000000000000000000000000000084FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000084FFFF0000000000000000008484840000FFFF000084000000FF
      FF0000FFFF0084848400FFFFFF00FFFFFF000000FF00FFFFFF000000FF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00840000008400000000000000FFFFFF000000
      0000FFFFFF00FFFFFF00000000000000000000000000000000004284FF000000
      000000008400FFFF000000008400FFFF000000008400FFFF000000008400FFFF
      0000000000004284FF000000000000000000000000000000000084FFFF000000
      000000848400FFFF000000848400FFFF000000848400FFFF000000848400FFFF
      00000000000084FFFF000000000000000000848484000084000000FFFF0000FF
      FF00C6C6C60084848400FFFFFF00FFFFFF00C6C6C6000000FF00C6C6C600FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00000000000000000000000000000000004284FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004284FF000000000000000000000000000000000084FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084FFFF0000000000000000008484840000FFFF00008400008484
      84008484840084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000840000008400
      0000000000008400000084000000840000008400000084000000840000008400
      00008400000084000000000000000000000000000000000000004284FF004284
      FF004284FF004284FF004284FF004284FF004284FF004284FF004284FF004284
      FF004284FF004284FF000000000000000000000000000000000084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084FF
      FF0084FFFF0084FFFF00000000000000000084848400848484008484840000FF
      FF00C6C6C6008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000FFFF0000FF00
      0000FFFF0000FF000000FFFF0000FF000000FFFF0000FF000000FFFF0000FF00
      0000FFFF0000FF000000000000000000000000000000000000004284FF004284
      FF004284FF004284FF004284FF004284FF000000000000000000000000000000
      000000000000000000008484840000000000000000000000000084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF000000000000000000000000000000
      00000000000000000000848484000000000084848400C6C6C60000FFFF00C6C6
      C60000FFFF0000FFFF000000000000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FFFF
      0000FF000000FFFF0000FF000000FFFF0000FF000000FFFF0000FF000000FFFF
      0000FF000000FFFF000000000000000000000000000000000000000000004284
      FF004284FF004284FF004284FF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000084FF
      FF0084FFFF0084FFFF0084FFFF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000848484008484840000FF
      FF00C6C6C6000000000000FFFF00848484008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484008484840084848400000000000000000000000000000000000000
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
      0000840021008400210084002100840021008400210084002100840021008400
      2100840021008400210084002100840021000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084002100F7CEA500FFFFFF00F7CEA500FFFFFF00F7CEA500FFFFFF00F7CE
      A500FFFFFF00F7CEA500FFFFFF0084002100000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000001842
      6B001839630010395A0010315A0010315A0010315A0010315A0010315A001031
      5A0010315A000000000000000000000000000000000000000000000000000000
      000084002100FFFFFF008400210084002100F7CEA5008400210084002100FFFF
      FF008400210084002100F7CEA50084002100000000000000000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000000000000000000000000000000000C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C6000000000000000000000000000000000000000000295A
      7B00E7FFFF00E7FFFF00DEFFFF00DEF7FF00D6F7FF00D6F7FF00CEF7FF00C6F7
      FF0010315A00000000000000000000000000000000000000000000000000C6A5
      840084002100F7CEA500FFFFFF00F7CEA500FFFFFF00F7CEA500FFFFFF00F7CE
      A500FFFFFF00F7CEA500FFFFFF0084002100000000000000000000FF00000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000000FF000000000000000000000000000000000000C6C6C6000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF0000000000C6C6C60000000000000000000000000000000000000000003163
      8400EFFFFF00E7FFFF00E7FFFF00DEFFFF00DEFFFF00D6F7FF00D6F7FF00CEF7
      FF0010315A0000000000000000000000000000000000C6A58400C6A58400C6C6
      C60084002100FFFFFF008400210084002100F7CEA5008400210084002100FFFF
      FF008400210084002100F7CEA50084002100000000000000000000FF00000000
      0000FFFFFF000084000000840000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000000FF000000000000000000000000000000000000C6C6C6000000
      0000FFFFFF008484840084848400FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF0000000000C6C6C6000000000000000000000000000000000000000000396B
      8C00EFFFFF00EFFFFF00EFFFFF00E7FFFF00E7FFFF00DEFFFF00D6F7FF00D6F7
      FF0010315A00000000000000000000000000C6A58400FFFF000000848400FFFF
      000084002100F7CEA500FFFFFF00F7CEA500FFFFFF00F7CEA500FFFFFF00F7CE
      A500FFFFFF00F7CEA500FFFFFF0084002100000000000000000000FF00000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000000FF000000000000000000000000000000000000C6C6C6000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF0000000000C6C6C6000000000000000000000000000000000000000000427B
      9400F7FFFF00F7FFFF00EFFFFF00EFFFFF00E7FFFF00E7FFFF00DEFFFF00DEF7
      FF0010395A00000000000000000000000000C6A5840000848400FFFF0000FFFF
      000084002100FFFFFF008400210084002100F7CEA5008400210084002100FFFF
      FF008400210084002100F7CEA50084002100000000000000000000FF00000000
      0000FFFFFF000084000000840000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000000FF000000000000000000000000000000000000C6C6C6000000
      0000FFFFFF008484840084848400FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF0000000000C6C6C60000000000000000000000000000000000000000004A84
      A500FFFFFF00F7FFFF00F7FFFF00EFFFFF00EFFFFF00E7FFFF00E7FFFF00DEFF
      FF0018426300000000000000000000000000C6A58400FFFF000000840000FFFF
      000084002100F7CEA500FFFFFF00F7CEA500FFFFFF00F7CEA500FFFFFF00F7CE
      A500FFFFFF00F7CEA500FFFFFF0084002100000000000000000000FF00000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF000000000000FF000000000000000000000000000000000000C6C6C6000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF0000000000C6C6C60000000000000000000000000000000000000000005294
      AD00FFFFFF00FFFFFF00F7FFFF00F7FFFF00EFFFFF00EFFFFF00EFFFFF00E7FF
      FF00214A6B00000000000000000000000000C6A584000000000000000000C6A5
      8400FFFF000000000000C6C6C600840000008442000084420000844200008442
      000084420000844200008442000084420000000000000000000000FF00000000
      000000840000FFFF000000840000FFFF000000840000FFFF000000840000FFFF
      00000000000000FF000000000000000000000000000000000000C6C6C6000000
      000084848400FFFF000084848400FFFF000084848400FFFF000084848400FFFF
      000000000000C6C6C60000000000000000000000000000000000000000005AA5
      BD00FFFFFF00FFFFFF00FFFFFF00F7FFFF00F7FFFF00F7FFFF00EFFFFF00EFFF
      FF00215273000000000000000000000000000000000000000000FFFF0000FFFF
      0000000000000000000000000000844200008442000084420000844200008442
      000084420000844200008442000084420000000000000000000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000000000000000000000000000000000C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C600000000000000000000000000000000000000000063AD
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF00F7FFFF00EFFF
      FF00295A7B00000000000000000000000000C6A58400C6A58400C6A584000000
      0000FFFF0000FFFF0000FFFF0000842100008421000084210000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600000000000000000000000000000000000000000073C6
      D6006BB5CE0063ADC60063A5BD005A9CB5005294B500528CAD004A8CA5004284
      9C0039738C00000000000000000000000000FFFF000000000000C6A58400FFFF
      000000000000C6A58400FFFF0000FFFF0000FFFF000084210000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C6000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6A58400FFFF0000C6A5
      8400FFFF000000000000C6A58400C6A58400C6A5840000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6A58400FFFF000000000000C6A5
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF00000000000000000000C6A5
      8400FFFF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000600000000100010000000000000300000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFFFFFF0000000F000D00000000
      000D000800000000000D000D00000000000D000D00000000000D000D00000000
      000D000D000000000008000D00000000000D000F00000000FFFFFFFFFFFF0000
      FFFFFFFFFFFF0000FFFFFFFFFFFF00008888FFFFFFFFFFFF888800010001FFFF
      888811FF11FFF776888811811181E375F88811011101E113F888F111F111E355
      F888F111F111E155F888F111F111E3168F88F111F111C1FF9F88BFFF8FFF80FF
      AF88DFFF9FFF80FFF788EFFFAFFF80FFFBF8F5FFF7FFC1FFFDF8F9FFFBFFE3FF
      FFF8F1FFFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8888FFFFFFFFFFFF8888
      FE7FFFFFF00F8888FC3FFFFFE0078888F81FFFFFC007F888F00F8001C007F888
      E0070000C007F888C0030000C007F888C0030000C007FF88E0070000C007DF88
      F00F0000C007EF88F81F0000C007F788FC3F8001C007FAF8FE7FFFFFE00FFCF8
      FFFFFFFFFFFFF8F8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF01F
      800180018001F01F800180018001F01F800180018001001F800180018001001F
      8001800180010000800180018001000080018001800100008001800180010000
      800180018001018080018001800101808001800180010180800180018001FF80
      FFFFFFFFFFFFFF80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF80018001F801FFFF
      80018001F801800180018001F801800180018001E00180018001800180018001
      8001800100018001800180010001800180018001000180018001800100018001
      8001800100018001800180010001800180018001023F8001C0FFC0FF847F8001
      E1FFE1FFE1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00080018001
      FFFFF00080018001E007F00080018001E007E00080018001E007800080018001
      E007000080018001E007000080018001E007000080018001E007640080018001
      E007CE0080018001E007103F80018001E007483F80018001FFFF847FC0FFC0FF
      FFFF2FFFE1FFE1FFFFFF67FFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 590
    Top = 173
  end
  object SD: TSaveDialog
    DefaultExt = '*.bin'
    FileName = 'blob'
    Filter =
      'Binary files (*.bin)|*.bin|Text files (*.txt)|*.txt|Office files' +
      ' (*.doc;*.xls;*ppt)|*.doc;*.xls;*.ppt|Xml files (*.xml)|*.xml;*|' +
      'Archive files (*.zip;*.rar;*.cab;*.7z)|*.*.zip;*.rar;*.cab;*.7z|' +
      'Execute files(*.exe;*.dll)|*.exe;*.dll|All(*.*)|*.*'
    InitialDir = '.'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 401
    Top = 173
  end
  object OD: TOpenDialog
    DefaultExt = '*.bin'
    Filter =
      'Binary files (*.bin)|*.bin|Text files (*.txt)|*.txt|Office files' +
      ' (*.doc;*.xls;*ppt)|*.doc;*.xls;*.ppt|Xml files (*.xml)|*.xml;*|' +
      'Archive files (*.zip;*.rar;*.cab;*.7z)|*.*.zip;*.rar;*.cab;*.7z|' +
      'Execute files(*.exe;*.dll)|*.exe;*.dll|All(*.*)|*.*'
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 365
    Top = 177
  end
  object ac_Connection: TActionList
    Left = 37
    Top = 513
    object ac_Connection_Connect: TAction
      Caption = '&Connect'
      ShortCut = 32835
      OnExecute = ac_Connection_ConnectExecute
    end
    object ac_Connection_Disconnect: TAction
      Caption = '&Disconnect'
      ShortCut = 32836
      OnExecute = ac_Connection_DisconnectExecute
    end
    object ac_Connection_ReConnect: TAction
      Caption = 'ReCo&nnect'
      ShortCut = 32846
      OnExecute = ac_Connection_ReConnectExecute
    end
  end
  object ac_Query: TActionList
    State = asSuspended
    Left = 57
    Top = 517
    object ac_Query_Open: TAction
      Caption = 'ac_Query_Open'
      ShortCut = 114
      OnExecute = ac_Query_OpenExecute
    end
    object ac_Query_Execute: TAction
      Caption = 'ac_Query_Execute'
      ShortCut = 16498
      OnExecute = ac_Query_ExecuteExecute
    end
    object ac_Query_Close: TAction
      Caption = 'ac_Query_Close'
      ShortCut = 32882
      OnExecute = ac_Query_CloseExecute
    end
  end
  object ac_Memo: TActionList
    State = asSuspended
    Left = 81
    Top = 513
  end
  object ac_Image: TActionList
    State = asSuspended
    Left = 105
    Top = 517
  end
  object ac_Binary: TActionList
    State = asSuspended
    Left = 129
    Top = 513
  end
  object ac_Main: TActionList
    Left = 153
    Top = 517
    object ac_Main_Connection: TAction
      Caption = 'ac_Main_Connection'
      Enabled = False
      ShortCut = 32817
      OnExecute = ac_Main_SwitchSheetExecute
    end
    object ac_Main_Query: TAction
      Tag = 1
      Caption = 'ac_Main_Query'
      Enabled = False
      ShortCut = 32818
      OnExecute = ac_Main_SwitchSheetExecute
    end
    object ac_Main_Memo: TAction
      Tag = 2
      Caption = 'ac_Main_Memo'
      Enabled = False
      ShortCut = 32819
      OnExecute = ac_Main_SwitchSheetExecute
    end
    object ac_Main_Image: TAction
      Tag = 3
      Caption = 'ac_Main_Image'
      Enabled = False
      ShortCut = 32820
      OnExecute = ac_Main_SwitchSheetExecute
    end
    object ac_Main_Binary: TAction
      Tag = 4
      Caption = 'ac_Main_Binary'
      Enabled = False
      ShortCut = 32821
      OnExecute = ac_Main_SwitchSheetExecute
    end
    object ac_Main_Query_Data: TAction
      Caption = 'ac_Main_Query_Data'
      Enabled = False
      ShortCut = 16433
      OnExecute = ac_Main_Query_DataExecute
    end
    object ac_Main_Query_UpdateOptions: TAction
      Caption = 'ac_Main_Query_UpdateOptions'
      Enabled = False
      ShortCut = 16434
      OnExecute = ac_Main_Query_UpdateOptionsExecute
    end
    object ac_Main_ApplyUpdates: TAction
      Caption = 'ac_Main_ApplyUpdates'
      Enabled = False
      OnExecute = ac_Main_ApplyUpdatesExecute
    end
    object ac_Main_LogChanges: TAction
      Caption = 'ac_Main_LogChanges'
      Enabled = False
      ShortCut = 32844
      OnExecute = ac_Main_LogChangesExecute
    end
    object ac_Main_BeginTransaction: TAction
      Caption = 'ac_Main_BeginTransaction'
      Enabled = False
      ShortCut = 16450
      OnExecute = ac_Main_BeginTransactionExecute
    end
    object ac_Main_Commit: TAction
      Caption = 'ac_Main_Commit'
      Enabled = False
      ShortCut = 16461
      OnExecute = ac_Main_CommitExecute
    end
    object ac_Main_Rollback: TAction
      Caption = 'ac_Main_Rollback'
      Enabled = False
      ShortCut = 16466
      OnExecute = ac_Main_RollbackExecute
    end
  end
end
