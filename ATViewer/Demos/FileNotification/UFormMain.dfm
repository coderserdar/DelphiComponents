object FormMain: TFormMain
  Left = 176
  Top = 175
  BorderStyle = bsDialog
  Caption = 'ATFileNotification Demo (Unicode enabled)'
  ClientHeight = 280
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 13
    Caption = 'Watch directory:'
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 52
    Height = 13
    Caption = 'Watch file:'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 48
    Width = 337
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel2: TBevel
    Left = 8
    Top = 104
    Width = 337
    Height = 9
    Shape = bsBottomLine
  end
  object EditDir: TTntEdit
    Left = 8
    Top = 24
    Width = 225
    Height = 21
    TabOrder = 0
    Text = 'C:\'
  end
  object btnBrowseDir: TButton
    Left = 240
    Top = 24
    Width = 49
    Height = 23
    Caption = '...'
    TabOrder = 1
    OnClick = btnBrowseDirClick
  end
  object EditFile: TTntEdit
    Left = 8
    Top = 80
    Width = 225
    Height = 21
    TabOrder = 3
    Text = 'C:\config.sys'
  end
  object btnBrowseFile: TButton
    Left = 240
    Top = 80
    Width = 49
    Height = 23
    Caption = '...'
    TabOrder = 4
    OnClick = btnBrowseFileClick
  end
  object btnWatchDir: TButton
    Left = 296
    Top = 24
    Width = 49
    Height = 23
    Caption = 'Watch'
    TabOrder = 2
    OnClick = btnWatchDirClick
  end
  object btnWatchFile: TButton
    Left = 296
    Top = 80
    Width = 49
    Height = 23
    Caption = 'Watch'
    TabOrder = 5
    OnClick = btnWatchFileClick
  end
  object btnClose: TButton
    Left = 256
    Top = 248
    Width = 89
    Height = 23
    Caption = 'Close'
    TabOrder = 7
    OnClick = btnCloseClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 120
    Width = 337
    Height = 121
    Caption = ' What to watch: '
    TabOrder = 6
    object chkSubtree: TCheckBox
      Left = 8
      Top = 16
      Width = 281
      Height = 17
      Caption = 'Subdirectories (for directory only)'
      TabOrder = 0
    end
    object chkFilenames: TCheckBox
      Left = 8
      Top = 32
      Width = 281
      Height = 17
      Caption = 'File names'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkDirnames: TCheckBox
      Left = 8
      Top = 48
      Width = 281
      Height = 17
      Caption = 'Directories names (for directory only)'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkAttr: TCheckBox
      Left = 8
      Top = 64
      Width = 281
      Height = 17
      Caption = 'Attributes'
      TabOrder = 3
    end
    object chkSize: TCheckBox
      Left = 8
      Top = 80
      Width = 281
      Height = 17
      Caption = 'Sizes'
      TabOrder = 4
    end
    object chkModif: TCheckBox
      Left = 8
      Top = 96
      Width = 281
      Height = 17
      Caption = 'Modification time'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
  end
  object OpenDialog1: TTntOpenDialog
    InitialDir = 'C:\'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 64
    Top = 248
  end
  object Notif: TATFileNotification
    OnChanged = DirChanged
    Left = 32
    Top = 248
  end
end
