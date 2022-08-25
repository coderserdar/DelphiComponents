object MainForm: TMainForm
  Left = 102
  Top = 93
  Caption = 
    'Testing ChkDsk and Format Disk - Magenta version 1.3 -26th Novem' +
    'ber 2018'
  ClientHeight = 464
  ClientWidth = 610
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Log: TMemo
    Left = 0
    Top = 0
    Width = 610
    Height = 352
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 352
    Width = 610
    Height = 112
    Align = alBottom
    TabOrder = 1
    object Label1: TLabel
      Left = 183
      Top = 55
      Width = 64
      Height = 14
      Caption = 'Volume Label'
    end
    object doChkDsk: TButton
      Left = 115
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Check Disk'
      TabOrder = 0
      OnClick = doChkDskClick
    end
    object doExit: TButton
      Left = 465
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = doExitClick
    end
    object DriveBox: TDriveComboBox
      Left = 10
      Top = 54
      Width = 145
      Height = 20
      TabOrder = 2
      TextCase = tcUpperCase
    end
    object OptCorrectErrors: TCheckBox
      Left = 10
      Top = 25
      Width = 91
      Height = 17
      Caption = 'Correct Errors'
      TabOrder = 3
    end
    object OptVerbose: TCheckBox
      Left = 105
      Top = 25
      Width = 71
      Height = 17
      Caption = 'Verbose'
      TabOrder = 4
    end
    object OptCheckDirty: TCheckBox
      Left = 185
      Top = 25
      Width = 86
      Height = 17
      Caption = 'Check if Dirty'
      TabOrder = 5
    end
    object OptScanDrive: TCheckBox
      Left = 275
      Top = 25
      Width = 81
      Height = 17
      Caption = 'Scan Drive'
      TabOrder = 6
    end
    object OptQuickFmt: TCheckBox
      Left = 360
      Top = 25
      Width = 86
      Height = 16
      Caption = 'Quick Format'
      TabOrder = 7
    end
    object doAbort: TButton
      Left = 375
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Abort'
      TabOrder = 8
      OnClick = doAbortClick
    end
    object doFrmtDsk: TButton
      Left = 205
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Format Disk'
      TabOrder = 9
      OnClick = doFrmtDskClick
    end
    object ProgressBar: TProgressBar
      Left = 10
      Top = 5
      Width = 531
      Height = 13
      Step = 1
      TabOrder = 10
    end
    object FileSystem: TComboBox
      Left = 455
      Top = 23
      Width = 56
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      ItemIndex = 0
      TabOrder = 11
      Text = 'NTFS'
      Items.Strings = (
        'NTFS'
        'FAT'
        'FAT32'
        'EXFAT')
    end
    object VolumeLabel: TEdit
      Left = 260
      Top = 50
      Width = 121
      Height = 22
      TabOrder = 12
    end
    object dioRefresh: TButton
      Left = 10
      Top = 80
      Width = 89
      Height = 25
      Caption = 'Refresh Drives'
      TabOrder = 13
      OnClick = dioRefreshClick
    end
  end
end
