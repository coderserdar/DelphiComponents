object MainForm: TMainForm
  Left = 155
  Top = 21
  Width = 578
  Height = 514
  Caption = 'TAppIniFile, TPropStorage demo'
  Color = clBtnFace
  Constraints.MinHeight = 218
  Constraints.MinWidth = 326
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 551
    Height = 185
    Anchors = [akLeft, akTop, akRight]
    Caption = ' TAppIniFile: '
    TabOrder = 0
    object vgLabel1: TvgLabel
      Left = 13
      Top = 19
      Width = 392
      Height = 26
      Caption = 
        'TAppIniFile components incapsulates customizable INI-file. '#13#10'TPr' +
        'opStorage components can link to it to load or save published ob' +
        'ject properties.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      WordWrap = True
    end
    object Label1: TLabel
      Left = 12
      Top = 92
      Width = 78
      Height = 13
      Caption = '&Store settings in:'
    end
    object Label2: TLabel
      Left = 12
      Top = 60
      Width = 59
      Height = 13
      Caption = 'Ini file &name:'
      FocusControl = edName
    end
    object Label3: TLabel
      Left = 12
      Top = 124
      Width = 82
      Height = 13
      Caption = '&Registry root key:'
      FocusControl = cbRegRoot
    end
    object edName: TEdit
      Left = 112
      Top = 56
      Width = 423
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object cbRegRoot: TComboBox
      Left = 112
      Top = 120
      Width = 423
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'HKEY_CURRENT_USER'
        'HKEY_LOCAL_MACHINE'
        'HKEY_CLASSES_ROOT'
        'HKEY_CURRENT_CONFIG'
        'HKEY_USERS'
        'HKEY_DYN_DATA')
    end
    object cmLoad: TButton
      Left = 16
      Top = 148
      Width = 75
      Height = 25
      Caption = '&Load all'
      TabOrder = 3
      OnClick = cmLoadClick
    end
    object cmSave: TButton
      Left = 100
      Top = 148
      Width = 75
      Height = 25
      Caption = '&Save all'
      TabOrder = 4
      OnClick = cmSaveClick
    end
    object cbIniFileType: TComboBox
      Left = 112
      Top = 88
      Width = 423
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'Common IniFile'
        'Buffered IniFile'
        'Registry')
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 200
    Width = 301
    Height = 279
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' TPropStorage (form): '
    TabOrder = 1
    object Label4: TLabel
      Left = 16
      Top = 176
      Width = 67
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '&Browse folder:'
      FocusControl = cbFolder
    end
    object cmLoadForm: TButton
      Left = 16
      Top = 208
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Load form'
      TabOrder = 2
      OnClick = cmLoadFormClick
    end
    object cmSaveForm: TButton
      Left = 100
      Top = 208
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Save form'
      TabOrder = 3
      OnClick = cmSaveFormClick
    end
    object cmLoadData: TButton
      Left = 16
      Top = 243
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Load DM'
      TabOrder = 4
      OnClick = cmLoadDataClick
    end
    object cmSaveData: TButton
      Left = 100
      Top = 244
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Save DM'
      TabOrder = 5
      OnClick = cmSaveDataClick
    end
    object etv: TExplorerTreeView
      Left = 12
      Top = 20
      Width = 276
      Height = 141
      ExplorerSource = ExplorerSource1
      Anchors = [akLeft, akTop, akRight, akBottom]
      Indent = 19
      TabOrder = 0
    end
    object cbFolder: TComboBox
      Left = 92
      Top = 172
      Width = 196
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbFolderChange
      Items.Strings = (
        'Desktop'
        'Internet'
        'Programs'
        'Controls'
        'Printers'
        'Personal'
        'Favorites'
        'StartUp'
        'Recent'
        'SendTo'
        'BitBucket'
        'StartMenu'
        'DesktopDir'
        'Drives'
        'Network'
        'NetHood'
        'Fonts'
        'Templates'
        'CommonStartMenu'
        'CommonPrograms'
        'CommonStartUp'
        'CommonDesktopDir'
        'AppData'
        'PrintHood'
        'AltStartUp'
        'CommonAltStartUp'
        'CommonFavorites'
        'Cache'
        'Cookies'
        'History')
    end
  end
  object GroupBox3: TGroupBox
    Left = 316
    Top = 200
    Width = 245
    Height = 279
    Anchors = [akTop, akRight, akBottom]
    Caption = ' TPropStorage (frame): '
    TabOrder = 2
    inline fr: TStoredFrame
      Left = 2
      Top = 15
      Width = 235
      Height = 258
      Anchors = [akLeft, akTop, akRight, akBottom]
      inherited cmLoad: TButton
        Top = 229
      end
      inherited cmSave: TButton
        Top = 229
      end
      inherited me: TMemo
        Width = 225
        Height = 173
      end
      inherited cmFont: TButton
        Top = 193
      end
      inherited psFrame: TPropStorage
        MasterStorage = psForm
      end
    end
  end
  object psForm: TPropStorage
    AppIniFile = dm.afIniFile
    AppSection = 'MainForm'
    StoredProps.Strings = (
      'MainForm.Left'
      'MainForm.Top'
      'MainForm.Width')
    StoredValues = <>
    Left = 214
    Top = 408
  end
  object ExplorerSource1: TExplorerSource
    ExplorerRoot = dm.enShell
    Images = [ilLarge, ilSmall, ilState]
    Left = 32
    Top = 288
  end
end
