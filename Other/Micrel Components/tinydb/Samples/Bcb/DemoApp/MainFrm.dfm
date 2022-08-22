object MainForm: TMainForm
  Left = 192
  Top = 133
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Using TinyDatabase in C++Builder'
  ClientHeight = 242
  ClientWidth = 489
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
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 473
    Height = 226
    ActivePage = DatabaseTabSheet
    HotTrack = True
    TabOrder = 0
    TabStop = False
    object DatabaseTabSheet: TTabSheet
      Caption = 'Database'
      object Label1: TLabel
        Left = 10
        Top = 16
        Width = 47
        Height = 13
        Caption = 'File Name'
      end
      object StatePanel: TPanel
        Left = 10
        Top = 163
        Width = 445
        Height = 23
        Alignment = taLeftJustify
        BevelInner = bvRaised
        BevelOuter = bvLowered
        Caption = 'Database State: Closed.'
        TabOrder = 7
      end
      object OpenDatabaseButton: TButton
        Left = 11
        Top = 128
        Width = 75
        Height = 25
        Caption = 'Open'
        TabOrder = 5
        OnClick = OpenDatabaseButtonClick
      end
      object CloseDatabaseButton: TButton
        Left = 100
        Top = 128
        Width = 75
        Height = 25
        Caption = 'Close'
        TabOrder = 6
        OnClick = CloseDatabaseButtonClick
      end
      object GroupBox5: TGroupBox
        Left = 10
        Top = 40
        Width = 217
        Height = 77
        TabOrder = 3
        object Label11: TLabel
          Left = 13
          Top = 23
          Width = 43
          Height = 13
          Caption = 'Algorithm'
        end
        object Label14: TLabel
          Left = 13
          Top = 47
          Width = 26
          Height = 13
          Caption = 'Level'
        end
        object CompCheckBox: TCheckBox
          Left = 12
          Top = -2
          Width = 83
          Height = 17
          Caption = 'Compression'
          TabOrder = 2
          OnClick = CompCheckBoxClick
        end
        object CompAlgoComboBox: TComboBox
          Left = 65
          Top = 19
          Width = 139
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object CompLevelComboBox: TComboBox
          Left = 65
          Top = 43
          Width = 139
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'clMaximum'
            'clNormal'
            'clFast'
            'clSuperFast')
        end
      end
      object GroupBox6: TGroupBox
        Left = 239
        Top = 40
        Width = 215
        Height = 77
        TabOrder = 4
        object Label12: TLabel
          Left = 16
          Top = 23
          Width = 43
          Height = 13
          Caption = 'Algorithm'
        end
        object Label13: TLabel
          Left = 16
          Top = 47
          Width = 46
          Height = 13
          Caption = 'Password'
        end
        object EncryptCheckBox: TCheckBox
          Left = 14
          Top = -2
          Width = 73
          Height = 17
          Caption = 'Encryption'
          TabOrder = 2
          OnClick = EncryptCheckBoxClick
        end
        object EncAlgoComboBox: TComboBox
          Left = 72
          Top = 19
          Width = 129
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object EncPwdEdit: TEdit
          Left = 72
          Top = 43
          Width = 129
          Height = 21
          TabOrder = 1
        end
      end
      object FileNameEdit: TEdit
        Left = 72
        Top = 12
        Width = 251
        Height = 21
        TabOrder = 0
      end
      object CreateDatabaseButton: TButton
        Left = 366
        Top = 12
        Width = 87
        Height = 21
        Caption = 'Create'
        TabOrder = 2
        OnClick = CreateDatabaseButtonClick
      end
      object BrowseButton: TButton
        Left = 325
        Top = 12
        Width = 23
        Height = 21
        Caption = '...'
        TabOrder = 1
        OnClick = BrowseButtonClick
      end
    end
    object RecordsTabSheet: TTabSheet
      Caption = 'Records'
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Left = 10
        Top = 8
        Width = 362
        Height = 177
        Caption = 'Record'
        TabOrder = 0
        object Label2: TLabel
          Left = 14
          Top = 24
          Width = 28
          Height = 13
          Caption = 'Name'
        end
        object Label3: TLabel
          Left = 187
          Top = 24
          Width = 19
          Height = 13
          Caption = 'Age'
        end
        object Label4: TLabel
          Left = 14
          Top = 52
          Width = 18
          Height = 13
          Caption = 'Sex'
        end
        object Label5: TLabel
          Left = 187
          Top = 52
          Width = 15
          Height = 13
          Caption = 'Tel'
        end
        object Label6: TLabel
          Left = 14
          Top = 80
          Width = 22
          Height = 13
          Caption = 'Addr'
        end
        object Label7: TLabel
          Left = 14
          Top = 107
          Width = 23
          Height = 13
          Caption = 'Note'
        end
        object NameEdit: TEdit
          Left = 54
          Top = 20
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object AgeEdit: TEdit
          Left = 227
          Top = 20
          Width = 121
          Height = 21
          TabOrder = 1
        end
        object SexComboBox: TComboBox
          Left = 54
          Top = 48
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            'Male'
            'Female')
        end
        object TelEdit: TEdit
          Left = 227
          Top = 48
          Width = 121
          Height = 21
          TabOrder = 3
        end
        object AddrEdit: TEdit
          Left = 54
          Top = 76
          Width = 295
          Height = 21
          TabOrder = 4
        end
        object NoteMemo: TMemo
          Left = 54
          Top = 104
          Width = 295
          Height = 58
          ScrollBars = ssVertical
          TabOrder = 5
        end
      end
      object AppendButton: TButton
        Left = 382
        Top = 14
        Width = 71
        Height = 25
        Caption = 'Append'
        TabOrder = 1
        OnClick = AppendButtonClick
      end
      object PostButton: TButton
        Left = 382
        Top = 42
        Width = 71
        Height = 25
        Caption = 'Post'
        TabOrder = 2
        OnClick = PostButtonClick
      end
      object FirstButton: TButton
        Left = 382
        Top = 86
        Width = 71
        Height = 25
        Caption = 'First'
        TabOrder = 3
        OnClick = FirstButtonClick
      end
      object PrevButton: TButton
        Left = 382
        Top = 111
        Width = 71
        Height = 25
        Caption = 'Prev'
        TabOrder = 4
        OnClick = PrevButtonClick
      end
      object NextButton: TButton
        Left = 382
        Top = 136
        Width = 71
        Height = 25
        Caption = 'Next'
        TabOrder = 5
        OnClick = NextButtonClick
      end
      object LastButton: TButton
        Left = 382
        Top = 161
        Width = 71
        Height = 25
        Caption = 'Last'
        TabOrder = 6
        OnClick = LastButtonClick
      end
    end
    object SearchTabSheet: TTabSheet
      Caption = 'Search'
      ImageIndex = 2
      object GroupBox2: TGroupBox
        Left = 10
        Top = 8
        Width = 444
        Height = 50
        Caption = 'GotoKey'
        TabOrder = 0
        object Label8: TLabel
          Left = 16
          Top = 21
          Width = 28
          Height = 13
          Caption = 'Name'
        end
        object GotoKeyNameEdit: TEdit
          Left = 64
          Top = 17
          Width = 180
          Height = 21
          TabOrder = 0
        end
        object GotoKeyButton: TButton
          Left = 357
          Top = 16
          Width = 75
          Height = 25
          Caption = 'GotoKey'
          TabOrder = 1
          OnClick = GotoKeyButtonClick
        end
      end
      object GroupBox3: TGroupBox
        Left = 10
        Top = 63
        Width = 444
        Height = 50
        Caption = 'Find'
        TabOrder = 1
        object Label9: TLabel
          Left = 16
          Top = 21
          Width = 28
          Height = 13
          Caption = 'Name'
        end
        object FindNameEdit: TEdit
          Left = 64
          Top = 17
          Width = 180
          Height = 21
          TabOrder = 0
        end
        object FindFirstButton: TButton
          Left = 269
          Top = 15
          Width = 75
          Height = 25
          Caption = 'FindFirst'
          TabOrder = 1
          OnClick = FindFirstButtonClick
        end
        object FindNextButton: TButton
          Left = 357
          Top = 15
          Width = 75
          Height = 25
          Caption = 'FindNext'
          TabOrder = 2
          OnClick = FindNextButtonClick
        end
      end
      object SearchDBGrid: TDBGrid
        Left = 10
        Top = 124
        Width = 444
        Height = 61
        DataSource = DataSource1
        TabOrder = 2
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object FilterTabSheet: TTabSheet
      Caption = 'Filter'
      ImageIndex = 3
      object Label10: TLabel
        Left = 10
        Top = 16
        Width = 22
        Height = 13
        Caption = 'Filter'
      end
      object FilterEdit: TEdit
        Left = 48
        Top = 12
        Width = 237
        Height = 21
        TabOrder = 0
      end
      object FilterButton: TButton
        Left = 295
        Top = 11
        Width = 75
        Height = 23
        Caption = 'Filter'
        TabOrder = 1
        OnClick = FilterButtonClick
      end
      object FilterDBGrid: TDBGrid
        Left = 10
        Top = 48
        Width = 444
        Height = 137
        DataSource = DataSource1
        TabOrder = 3
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object CancelFilterButton: TButton
        Left = 379
        Top = 11
        Width = 75
        Height = 23
        Caption = 'Cancel Filter'
        TabOrder = 2
        OnClick = CancelFilterButtonClick
      end
    end
  end
  object TinyDatabase1: TTinyDatabase
    MediumType = mtDisk
    SessionName = 'Default'
    Left = 308
    Top = 8
  end
  object TinyTable1: TTinyTable
    MediumType = mtDisk
    AfterOpen = TinyTable1AfterOpen
    AfterClose = TinyTable1AfterClose
    AfterScroll = TinyTable1AfterScroll
    IndexDefs = <>
    FieldDefs = <>
    Left = 340
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = TinyTable1
    Left = 372
    Top = 8
  end
end
