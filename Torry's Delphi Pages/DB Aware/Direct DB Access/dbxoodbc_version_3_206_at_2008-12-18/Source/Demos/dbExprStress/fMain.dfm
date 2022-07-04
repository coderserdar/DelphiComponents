object frmMain: TfrmMain
  Left = 514
  Top = 195
  Width = 462
  Height = 441
  Caption = 'DbExpress Stress (testing dbExpress driver)'
  Color = clBtnFace
  Constraints.MinHeight = 380
  Constraints.MinWidth = 440
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SB: TStatusBar
    Left = 0
    Top = 388
    Width = 454
    Height = 19
    Panels = <>
  end
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 454
    Height = 121
    Align = alTop
    TabOrder = 1
    object Shape1: TShape
      Left = 1
      Top = 115
      Width = 452
      Height = 5
      Align = alBottom
    end
    object StaticText1: TStaticText
      Left = 16
      Top = 16
      Width = 116
      Height = 17
      Caption = 'Concurent Threads:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      TabOrder = 0
    end
    object stThreadCount: TStaticText
      Left = 136
      Top = 16
      Width = 67
      Height = 17
      Caption = '000000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object StaticText3: TStaticText
      Left = 206
      Top = 16
      Width = 84
      Height = 17
      Caption = 'Time Elapsed:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      TabOrder = 2
    end
    object stTimeElapsed: TStaticText
      Left = 298
      Top = 16
      Width = 67
      Height = 17
      Caption = '000000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
    object StaticText7: TStaticText
      Left = 206
      Top = 35
      Width = 85
      Height = 17
      Caption = 'Thread Errors:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      TabOrder = 4
    end
    object stErrors: TStaticText
      Left = 298
      Top = 35
      Width = 67
      Height = 17
      Caption = '000000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
    end
    object StaticText8: TStaticText
      Left = 207
      Top = 54
      Width = 92
      Height = 17
      Caption = 'Updates Count:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      TabOrder = 6
    end
    object stUpdates: TStaticText
      Left = 298
      Top = 54
      Width = 67
      Height = 17
      Caption = '000000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 7
    end
    object btCreateThread: TButton
      Left = 16
      Top = 48
      Width = 145
      Height = 25
      Caption = 'Add New Thread'
      TabOrder = 8
      OnClick = btCreateThreadClick
    end
    object btStop: TButton
      Left = 16
      Top = 78
      Width = 145
      Height = 25
      Caption = 'Stop'
      TabOrder = 9
      OnClick = btStopClick
    end
  end
  object pcMain: TPageControl
    Left = 0
    Top = 121
    Width = 454
    Height = 267
    ActivePage = tsCon
    Align = alClient
    TabOrder = 2
    object tsCon: TTabSheet
      Caption = 'Connection'
      object pcCon: TPageControl
        Left = 0
        Top = 0
        Width = 446
        Height = 239
        ActivePage = ts_sql_select
        Align = alClient
        TabOrder = 0
        object tcConStr: TTabSheet
          Caption = 'Connection string'
          ImageIndex = 2
          object StaticText2: TStaticText
            Left = 2
            Top = 5
            Width = 91
            Height = 17
            Caption = 'Connection String:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
          object edDSN: TEdit
            Left = 1
            Top = 25
            Width = 416
            Height = 21
            TabOrder = 1
            Text = 'DSN=dsn_dbxODBC'
          end
          object StaticText4: TStaticText
            Left = 4
            Top = 51
            Width = 129
            Height = 17
            Caption = 'Connection Custom String:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
          end
          object ed_custStr: TEdit
            Left = 2
            Top = 68
            Width = 415
            Height = 21
            TabOrder = 3
          end
          object gbTranIsol: TGroupBox
            Left = 4
            Top = 120
            Width = 413
            Height = 85
            Caption = '  Supported Transaction Isolations:  '
            TabOrder = 4
            object cbti_DirtyRead: TCheckBox
              Left = 12
              Top = 20
              Width = 189
              Height = 17
              Caption = 'DirtyRead'
              TabOrder = 0
            end
            object cbti_ReadCommited: TCheckBox
              Left = 12
              Top = 40
              Width = 193
              Height = 17
              Caption = 'ReadCommited'
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
            object cbti_RepeatableRead: TCheckBox
              Left = 12
              Top = 60
              Width = 185
              Height = 17
              Caption = 'RepeatableRead   '
              TabOrder = 2
            end
          end
          object cb_allowopencursorintransaction: TCheckBox
            Left = 4
            Top = 96
            Width = 405
            Height = 17
            Caption = 'Allow Open Cursor in Transaction'
            Checked = True
            State = cbChecked
            TabOrder = 5
          end
        end
        object ts_sql_select: TTabSheet
          Caption = 'Query text template:'
          object m_sql_select: TMemo
            Left = 0
            Top = 57
            Width = 438
            Height = 154
            Align = alClient
            Lines.Strings = (
              'SELECT * FROM %s')
            TabOrder = 0
          end
          object pTO: TPanel
            Left = 0
            Top = 0
            Width = 438
            Height = 57
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object cb_ReadMetadata: TCheckBox
              Left = 4
              Top = 4
              Width = 425
              Height = 17
              Caption = 
                'Read Cursor Metadata  ( Becomes possible UpdateMode = upWhereKey' +
                'Only )'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
            object cb_AllowUsagePacketRecords: TCheckBox
              Left = 4
              Top = 28
              Width = 313
              Height = 17
              Caption = 'Usage Packet Records'
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
        end
        object ts_sql_update: TTabSheet
          Caption = 'Query params:'
          ImageIndex = 1
          object ed_table: TEdit
            Left = 77
            Top = 8
            Width = 196
            Height = 21
            TabOrder = 0
            Text = 'CUSTOMER'
          end
          object StaticText5: TStaticText
            Left = 4
            Top = 12
            Width = 59
            Height = 17
            Caption = 'table name:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
          object StaticText6: TStaticText
            Left = 4
            Top = 35
            Width = 67
            Height = 17
            Caption = 'update fields:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
          end
          object u_updflds: TMemo
            Left = 76
            Top = 36
            Width = 197
            Height = 167
            Lines.Strings = (
              'COMPANY')
            TabOrder = 3
          end
          object gbFldOp: TGroupBox
            Left = 280
            Top = 8
            Width = 129
            Height = 85
            Caption = ' Record Operations '
            Enabled = False
            TabOrder = 4
            object cboUpdate: TCheckBox
              Left = 8
              Top = 20
              Width = 97
              Height = 17
              Caption = 'Update'
              Checked = True
              Enabled = False
              State = cbChecked
              TabOrder = 0
            end
            object cboInsert: TCheckBox
              Left = 8
              Top = 40
              Width = 97
              Height = 17
              Caption = 'Insert'
              Enabled = False
              TabOrder = 1
            end
            object cboDelete: TCheckBox
              Left = 8
              Top = 60
              Width = 97
              Height = 17
              Caption = 'Delete'
              Enabled = False
              TabOrder = 2
            end
          end
        end
      end
    end
    object tsLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      object mLog: TMemo
        Left = 0
        Top = 29
        Width = 446
        Height = 210
        TabStop = False
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 446
        Height = 29
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Button1: TButton
          Left = 4
          Top = 0
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
    object tsHelp: TTabSheet
      Caption = 'Help'
      ImageIndex = 2
      object mHelp: TMemo
        Left = 0
        Top = 0
        Width = 446
        Height = 239
        Align = alClient
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Terminal'
        Font.Style = []
        Lines.Strings = (
          '/*******************************************'
          ' **  part of BDE:DBDEMOS:customer.db      **'
          ' *******************************************/'
          ' '
          'create table customer('
          '  custno integer primary key,'
          '  company varchar(30),'
          '  taxrate double'
          ');'
          ''
          'insert into customer (custno, company, '
          'taxrate) values (1, '#39'company'#39', 0 );'
          '...'
          'insert into customer (custno, company, '
          'taxrate) values (100, '#39'company'#39', 0 );')
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 388
    Top = 16
  end
  object SQLConTemplate: TSQLConnection
    ConnectionName = 'IBConnection'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=Interbase'
      'Database=D:\PROGRAMS\DB\InterBase\Databases\MASTSQL.GDB'
      'RoleName=RoleName'
      'User_Name=sysdba'
      'Password=masterkey'
      'ServerCharSet='
      'SQLDialect=1'
      'ErrorResourceFile='
      'LocaleCode=0000'
      'BlobSize=-1'
      'CommitRetain=False'
      'WaitOnLocks=True'
      'Interbase TransIsolation=ReadCommited'
      'Trim Char=False')
    VendorLib = 'gds32.dll'
    Left = 324
    Top = 80
  end
end
