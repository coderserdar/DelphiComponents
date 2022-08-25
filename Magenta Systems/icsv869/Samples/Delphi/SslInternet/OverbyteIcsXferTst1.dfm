object Form1: TForm1
  Left = 196
  Top = 51
  Caption = 
    'ICS Multi File Transfer Demo -  http://www.overbyte.be - V8.69 -' +
    ' 14th April 2022'
  ClientHeight = 773
  ClientWidth = 908
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 460
    Top = 2
    Width = 418
    Height = 42
    Caption = 
      'The demonstration application is designed to show how the three ' +
      'multi file rransfer components are used from code, there are num' +
      'erous properties at are set in code, not through the GUI.  '
    WordWrap = True
  end
  object LabelProgress: TLabel
    Left = 10
    Top = 311
    Width = 890
    Height = 46
    AutoSize = False
    Caption = 'Progress:'
    WordWrap = True
  end
  object LabelSslState: TLabel
    Left = 660
    Top = 203
    Width = 116
    Height = 37
    AutoSize = False
    Caption = 'LabelSslState'
    WordWrap = True
  end
  object Label26: TLabel
    Left = 460
    Top = 203
    Width = 185
    Height = 28
    Caption = 
      'This demo supports full Unicode when built with Delphi 2009 and ' +
      'later.  '
    WordWrap = True
  end
  object lbl1: TLabel
    Left = 713
    Top = 150
    Width = 92
    Height = 14
    Caption = 'SSL Client Security'
  end
  object Label29: TLabel
    Left = 460
    Top = 55
    Width = 65
    Height = 14
    Caption = 'Log Directory'
  end
  object LogText: TMemo
    Left = 0
    Top = 502
    Width = 908
    Height = 271
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 5
    Top = 5
    Width = 440
    Height = 300
    ActivePage = TabSheet1
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'TIcsFileCopy'
      object Label2: TLabel
        Left = 5
        Top = 0
        Width = 82
        Height = 14
        Caption = 'Source Directory'
      end
      object Label3: TLabel
        Left = 5
        Top = 50
        Width = 82
        Height = 14
        Caption = 'Source File Mask'
      end
      object Label4: TLabel
        Left = 5
        Top = 70
        Width = 77
        Height = 14
        Caption = 'Target Directory'
      end
      object Label19: TLabel
        Left = 5
        Top = 120
        Width = 30
        Height = 14
        Caption = 'Logon'
      end
      object Label20: TLabel
        Left = 170
        Top = 118
        Width = 50
        Height = 14
        Caption = 'Password'
      end
      object Label27: TLabel
        Left = 158
        Top = 52
        Width = 77
        Height = 14
        Caption = 'Ignore Directory'
      end
      object Label31: TLabel
        Left = 270
        Top = 143
        Width = 81
        Height = 14
        Caption = 'File Replacement'
      end
      object doCopyCheck: TButton
        Left = 30
        Top = 190
        Width = 101
        Height = 25
        Caption = 'Check Copy Files'
        TabOrder = 7
        OnClick = CopyFiles
      end
      object doCopyList: TButton
        Left = 30
        Top = 160
        Width = 101
        Height = 25
        Caption = 'List Source Files'
        TabOrder = 6
        OnClick = doCopyListClick
      end
      object CopySrcDir: TEdit
        Left = 5
        Top = 20
        Width = 416
        Height = 22
        TabOrder = 0
        Text = 'c:\windows\system32'
      end
      object CopySrcFile: TEdit
        Left = 93
        Top = 50
        Width = 52
        Height = 22
        TabOrder = 1
        Text = '*.txt'
      end
      object doCopyFiles: TButton
        Left = 30
        Top = 220
        Width = 101
        Height = 25
        Caption = 'Copy Files'
        TabOrder = 8
        OnClick = CopyFiles
      end
      object CopyTarDir: TEdit
        Left = 5
        Top = 85
        Width = 416
        Height = 22
        TabOrder = 3
        Text = 'c:\tempfiles'
      end
      object doCopyAbort: TButton
        Left = 150
        Top = 160
        Width = 101
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 9
        OnClick = doAbortClick
      end
      object doDeleteCheck: TButton
        Left = 150
        Top = 190
        Width = 101
        Height = 25
        Caption = 'Check Delete Files'
        TabOrder = 10
        OnClick = DeleteFiles
      end
      object doDeleteFiles: TButton
        Left = 150
        Top = 220
        Width = 101
        Height = 25
        Caption = 'Delete Target Files'
        TabOrder = 11
        OnClick = DeleteFiles
      end
      object NetLogon: TEdit
        Left = 45
        Top = 115
        Width = 111
        Height = 22
        TabOrder = 4
      end
      object NetPassword: TEdit
        Left = 230
        Top = 115
        Width = 101
        Height = 22
        PasswordChar = '*'
        TabOrder = 5
      end
      object CopySubdirs: TCheckBox
        Left = 270
        Top = 190
        Width = 136
        Height = 17
        Caption = 'Include Sub Directories'
        Checked = True
        State = cbChecked
        TabOrder = 13
      end
      object CopyEmptyDirs: TCheckBox
        Left = 270
        Top = 210
        Width = 136
        Height = 17
        Caption = 'Copy Empty Directories'
        TabOrder = 14
      end
      object CopyWow64Disable: TCheckBox
        Left = 270
        Top = 230
        Width = 136
        Height = 17
        Caption = 'Disable Wow64 Redirect'
        TabOrder = 15
      end
      object CopyIgnorePath: TEdit
        Left = 242
        Top = 50
        Width = 174
        Height = 22
        TabOrder = 2
        Text = 'c:\temp\'
      end
      object CopyFileRepl: TComboBox
        Left = 270
        Top = 165
        Width = 113
        Height = 22
        ItemHeight = 14
        TabOrder = 12
        Text = ' Never'
        Items.Strings = (
          'Never'
          'Always'
          'If Different'
          'If Newer')
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TIcsFtpMulti'
      ImageIndex = 1
      object Label9: TLabel
        Left = 5
        Top = 10
        Width = 43
        Height = 14
        Caption = 'FTP Host'
      end
      object Label10: TLabel
        Left = 5
        Top = 40
        Width = 30
        Height = 14
        Caption = 'Logon'
      end
      object Label11: TLabel
        Left = 170
        Top = 38
        Width = 50
        Height = 14
        Caption = 'Password'
      end
      object Label21: TLabel
        Left = 5
        Top = 70
        Width = 74
        Height = 14
        Caption = 'Secure Server:'
      end
      object Label22: TLabel
        Left = 110
        Top = 100
        Width = 40
        Height = 14
        Caption = 'FTP Port'
      end
      object Label23: TLabel
        Left = 220
        Top = 100
        Width = 42
        Height = 14
        Caption = 'SSL Port'
      end
      object Label24: TLabel
        Left = 5
        Top = 130
        Width = 152
        Height = 14
        Caption = 'Keep Alive (secs or 0 for none)'
      end
      object Label25: TLabel
        Left = 138
        Top = 155
        Width = 133
        Height = 14
        Caption = 'Bandwidth Limit (KBits/sec)'
        WordWrap = True
      end
      object FtpUsername: TEdit
        Left = 45
        Top = 35
        Width = 111
        Height = 22
        TabOrder = 1
        Text = 'anonymous'
      end
      object FtpPassword: TEdit
        Left = 230
        Top = 35
        Width = 101
        Height = 22
        PasswordChar = '*'
        TabOrder = 2
        Text = 'test@'
      end
      object FtpServerType: TComboBox
        Left = 90
        Top = 65
        Width = 241
        Height = 20
        Style = csOwnerDrawFixed
        ItemHeight = 14
        TabOrder = 3
      end
      object FtpHost: TComboBox
        Left = 55
        Top = 5
        Width = 276
        Height = 22
        ItemHeight = 14
        TabOrder = 0
        Text = 'www.magsys.co.uk'
        Items.Strings = (
          'www.magsys.co.uk'
          'ics.ftptest.org'
          'filezilla.ftptest.org'
          'msftp7.ftptest.org')
      end
      object FtpPassive: TCheckBox
        Left = 5
        Top = 100
        Width = 97
        Height = 17
        Caption = 'Passive Mode'
        TabOrder = 4
      end
      object FtpPort: TEdit
        Left = 165
        Top = 95
        Width = 46
        Height = 22
        TabOrder = 5
        Text = '21'
      end
      object FtpPortSsl: TEdit
        Left = 270
        Top = 95
        Width = 46
        Height = 22
        TabOrder = 6
        Text = '990'
      end
      object FtpKeepAlive: TEdit
        Left = 165
        Top = 125
        Width = 46
        Height = 22
        TabOrder = 7
        Text = '30'
      end
      object FtpNoFeatCmd: TCheckBox
        Left = 5
        Top = 155
        Width = 126
        Height = 17
        Caption = 'No FEAT Command'
        TabOrder = 8
      end
      object FtpBandWidth: TEdit
        Left = 285
        Top = 150
        Width = 46
        Height = 22
        TabOrder = 15
        Text = '0'
      end
      object FtpNoZlib: TCheckBox
        Left = 5
        Top = 175
        Width = 126
        Height = 17
        Caption = 'No Mode Z Compress'
        TabOrder = 9
      end
      object FtpNoTmpFile: TCheckBox
        Left = 145
        Top = 175
        Width = 134
        Height = 17
        Caption = 'No TMP File for Xfers'
        TabOrder = 12
      end
      object FtpNoUtf8: TCheckBox
        Left = 285
        Top = 174
        Width = 100
        Height = 17
        Caption = 'Turn UTF8 Off'
        TabOrder = 16
      end
      object ftpNoHost: TCheckBox
        Left = 145
        Top = 195
        Width = 117
        Height = 17
        Caption = 'No HOST Command'
        TabOrder = 13
      end
      object ftpIgnoreUtf8: TCheckBox
        Left = 285
        Top = 195
        Width = 100
        Height = 17
        Caption = 'Ignore UTF8 '
        TabOrder = 17
      end
      object FtpNoMd5: TCheckBox
        Left = 5
        Top = 195
        Width = 126
        Height = 17
        Caption = 'No MD5 Check'
        TabOrder = 10
      end
      object FtpNoCrc: TCheckBox
        Left = 3
        Top = 215
        Width = 126
        Height = 17
        Caption = 'No CRC Check'
        TabOrder = 11
      end
      object FtpFixPassiveLanIP: TCheckBox
        Left = 145
        Top = 215
        Width = 146
        Height = 17
        Caption = 'Fix Passive LAN IP Addr'
        TabOrder = 14
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Multi FTP'
      ImageIndex = 4
      object Label8: TLabel
        Left = 5
        Top = 10
        Width = 21
        Height = 14
        Caption = 'Path'
      end
      object Label13: TLabel
        Left = 125
        Top = 35
        Width = 82
        Height = 14
        Caption = 'Source File Mask'
      end
      object Label12: TLabel
        Left = 5
        Top = 110
        Width = 73
        Height = 14
        Caption = 'Local Directory'
      end
      object Label28: TLabel
        Left = 143
        Top = 52
        Width = 52
        Height = 28
        Caption = 'Ignore Directories'
        WordWrap = True
      end
      object Label30: TLabel
        Left = 143
        Top = 85
        Width = 81
        Height = 14
        Caption = 'File Replacement'
      end
      object FtpPath: TEdit
        Left = 60
        Top = 5
        Width = 361
        Height = 22
        TabOrder = 0
        Text = '/'
      end
      object FtpSrcFile: TEdit
        Left = 210
        Top = 30
        Width = 211
        Height = 22
        TabOrder = 5
        Text = 'd*.zip'
      end
      object FtpLocDir: TEdit
        Left = 5
        Top = 125
        Width = 416
        Height = 22
        TabOrder = 9
        Text = 'c:\tempfiles'
      end
      object doFtpDownCheck: TButton
        Left = 120
        Top = 155
        Width = 101
        Height = 25
        Caption = 'Check Download'
        TabOrder = 11
        OnClick = FtpDownload
      end
      object doFtpDownFiles: TButton
        Left = 120
        Top = 185
        Width = 101
        Height = 25
        Caption = 'Download Files'
        TabOrder = 14
        OnClick = FtpDownload
      end
      object doFtpAbort: TButton
        Left = 345
        Top = 155
        Width = 60
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 13
        OnClick = doAbortClick
      end
      object doFtpUpCheck: TButton
        Left = 230
        Top = 155
        Width = 101
        Height = 25
        Caption = 'Check Upload'
        TabOrder = 12
        OnClick = FtpUpload
      end
      object doFtpUpFiles: TButton
        Left = 230
        Top = 185
        Width = 101
        Height = 25
        Caption = 'Upload Files'
        TabOrder = 15
        OnClick = FtpUpload
      end
      object doFtpList: TButton
        Left = 10
        Top = 155
        Width = 101
        Height = 25
        Caption = 'List Host Files'
        TabOrder = 10
        OnClick = doFtpListClick
      end
      object FtpSubdirs: TCheckBox
        Left = 5
        Top = 55
        Width = 136
        Height = 17
        Caption = 'Include Sub Directories'
        TabOrder = 2
      end
      object FtpDelDone: TCheckBox
        Left = 5
        Top = 90
        Width = 136
        Height = 17
        Caption = 'Delete After Transfer'
        TabOrder = 4
      end
      object FtpDelOldTar: TCheckBox
        Left = 5
        Top = 75
        Width = 136
        Height = 13
        Caption = 'Delete Old Target Files'
        TabOrder = 3
      end
      object FtpCopyAllDir: TCheckBox
        Left = 5
        Top = 35
        Width = 116
        Height = 17
        Caption = 'Copy All Directory '
        TabOrder = 1
      end
      object FtpEmptyDirs: TCheckBox
        Left = 150
        Top = 103
        Width = 136
        Height = 17
        Caption = 'Copy Empty Directories'
        TabOrder = 8
      end
      object doFtpUpThread: TButton
        Left = 230
        Top = 215
        Width = 101
        Height = 25
        Caption = 'Upload (Thread)'
        TabOrder = 18
        OnClick = doFtpUpThreadClick
      end
      object doFtpListThread: TButton
        Left = 10
        Top = 215
        Width = 101
        Height = 25
        Caption = 'List Files (Thread)'
        TabOrder = 16
        OnClick = doFtpListThreadClick
      end
      object doFtpDownThread: TButton
        Left = 120
        Top = 215
        Width = 101
        Height = 25
        Caption = 'Download  (Thread)'
        TabOrder = 17
        OnClick = doFtpDownThreadClick
      end
      object FtpIgnorePath: TEdit
        Left = 208
        Top = 55
        Width = 213
        Height = 22
        TabOrder = 6
        Text = '/work/'
      end
      object FtpMultiFileRepl: TComboBox
        Left = 245
        Top = 83
        Width = 113
        Height = 22
        ItemHeight = 14
        TabOrder = 7
        Text = ' Never'
        Items.Strings = (
          ' Never'
          'Always'
          'If Different'
          'If Newer')
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Single FTP'
      ImageIndex = 3
      object Label15: TLabel
        Left = 5
        Top = 10
        Width = 60
        Height = 14
        Caption = 'Remote Path'
      end
      object Label16: TLabel
        Left = 5
        Top = 40
        Width = 98
        Height = 14
        Caption = 'Download File Name'
      end
      object Label17: TLabel
        Left = 5
        Top = 60
        Width = 96
        Height = 14
        Caption = 'Download Directory'
      end
      object Label18: TLabel
        Left = 5
        Top = 110
        Width = 127
        Height = 14
        Caption = 'Upload File Path and Name'
      end
      object Label32: TLabel
        Left = 5
        Top = 165
        Width = 81
        Height = 14
        Caption = 'File Replacement'
      end
      object Ftp1Path: TEdit
        Left = 75
        Top = 5
        Width = 346
        Height = 22
        TabOrder = 0
        Text = '/software'
      end
      object Ftp1SrcName: TEdit
        Left = 115
        Top = 35
        Width = 216
        Height = 22
        TabOrder = 1
        Text = 'dunman.zip'
      end
      object Ftp1LocDir: TEdit
        Left = 3
        Top = 80
        Width = 418
        Height = 22
        TabOrder = 2
        Text = 'c:\tempfiles'
      end
      object doFtpDown1: TButton
        Left = 10
        Top = 200
        Width = 96
        Height = 25
        Caption = 'Single Download'
        TabOrder = 4
        OnClick = doFtpDown1Click
      end
      object doFtpUp1: TButton
        Left = 115
        Top = 200
        Width = 96
        Height = 25
        Caption = 'Single Upload'
        TabOrder = 6
        OnClick = doFtpUp1Click
      end
      object Ftp1UpFile: TEdit
        Left = 3
        Top = 131
        Width = 413
        Height = 22
        TabOrder = 5
        Text = 'c:\tempfiles\dunman.zip'
      end
      object FtpOneDelDone: TCheckBox
        Left = 250
        Top = 164
        Width = 136
        Height = 17
        Caption = 'Delete After Transfer'
        TabOrder = 3
        Visible = False
      end
      object doFtpAbort1: TButton
        Left = 225
        Top = 200
        Width = 76
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 7
        OnClick = doAbortClick
      end
      object FtpOneFileRepl: TComboBox
        Left = 110
        Top = 160
        Width = 113
        Height = 22
        ItemHeight = 14
        TabOrder = 8
        Text = ' Never'
        Items.Strings = (
          ' Never'
          'Always'
          'If Different'
          'If Newer')
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TIcsHttpMulti'
      ImageIndex = 2
      object Label5: TLabel
        Left = 5
        Top = 0
        Width = 93
        Height = 14
        Caption = 'Source HTTP Paths'
      end
      object Label6: TLabel
        Left = 5
        Top = 145
        Width = 82
        Height = 14
        Caption = 'Source File Mask'
      end
      object Label7: TLabel
        Left = 5
        Top = 190
        Width = 77
        Height = 14
        Caption = 'Target Directory'
      end
      object Label14: TLabel
        Left = 284
        Top = 168
        Width = 79
        Height = 28
        Caption = 'Bandwidth Limit (KBits/sec)'
        WordWrap = True
      end
      object Label33: TLabel
        Left = 217
        Top = 145
        Width = 81
        Height = 14
        Caption = 'File Replacement'
      end
      object Label34: TLabel
        Left = 111
        Top = 170
        Width = 63
        Height = 14
        Caption = 'Parse Levels'
        WordWrap = True
      end
      object HttpTarDir: TEdit
        Left = 5
        Top = 210
        Width = 416
        Height = 22
        TabOrder = 6
        Text = 'c:\tempfiles'
      end
      object doHttpDownCheck: TButton
        Left = 135
        Top = 240
        Width = 101
        Height = 25
        Caption = 'Check Down Files'
        TabOrder = 8
        OnClick = HttpDownload
      end
      object doHttpDownFiles: TButton
        Left = 3
        Top = 240
        Width = 101
        Height = 25
        Caption = 'Download Files'
        TabOrder = 7
        OnClick = HttpDownload
      end
      object doHttpAbort: TButton
        Left = 260
        Top = 240
        Width = 101
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 9
        OnClick = doAbortClick
      end
      object HttpSrcFile: TComboBox
        Left = 100
        Top = 140
        Width = 111
        Height = 22
        ItemHeight = 14
        TabOrder = 1
        Text = '*.zip'
        Items.Strings = (
          '*.ide'
          '*.zip'
          '*.htm'
          '*.*')
      end
      object HttpSrcDir: TMemo
        Left = 5
        Top = 20
        Width = 416
        Height = 115
        Lines.Strings = (
          'http://www.magsys.co.uk/dunman/default.asp')
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object HttpBandWidth: TEdit
        Left = 380
        Top = 170
        Width = 46
        Height = 22
        TabOrder = 5
        Text = '0'
      end
      object HttpFileRepl: TComboBox
        Left = 316
        Top = 140
        Width = 113
        Height = 22
        ItemHeight = 14
        TabOrder = 2
        Text = ' Never'
        Items.Strings = (
          ' Never'
          'Always'
          'If Different'
          'If Newer')
      end
      object HttpParsePage: TCheckBox
        Left = 5
        Top = 170
        Width = 82
        Height = 17
        Caption = 'Parse HTML'
        TabOrder = 3
      end
      object HttpParseLevels: TEdit
        Left = 180
        Top = 168
        Width = 46
        Height = 22
        TabOrder = 4
        Text = '0'
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'File Clean-up'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label35: TLabel
        Left = 5
        Top = 114
        Width = 124
        Height = 16
        AutoSize = False
        Caption = 'Clean-up Directory'
        WordWrap = True
      end
      object Label36: TLabel
        Left = 5
        Top = 159
        Width = 73
        Height = 26
        AutoSize = False
        Caption = 'Clean-up Mask with Wildcard'
        WordWrap = True
      end
      object CleanupType: TRadioGroup
        Left = 5
        Top = 5
        Width = 166
        Height = 86
        Caption = 'File Clean-up Method'
        ItemIndex = 0
        Items.Strings = (
          'Delete Files by Age'
          'Delete Files by Dates'
          'Zip Files by Age'
          'Zip Files by Dates')
        TabOrder = 0
        OnClick = CleanupTypeClick
      end
      object BoxCleanupDays: TGroupBox
        Left = 189
        Top = 5
        Width = 211
        Height = 40
        Caption = 'Clean-up By Age'
        TabOrder = 1
        object Label115: TLabel
          Left = 10
          Top = 15
          Width = 78
          Height = 14
          Caption = 'Older than Days'
        end
        object CleanupDays: TEdit
          Left = 123
          Top = 12
          Width = 53
          Height = 22
          Hint = 'ie log-2021*.log'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = '90'
        end
      end
      object BoxCleanupDates: TGroupBox
        Left = 189
        Top = 50
        Width = 211
        Height = 64
        Caption = 'Clean-up Date Range'
        TabOrder = 2
        object Label109: TLabel
          Left = 10
          Top = 15
          Width = 49
          Height = 14
          Caption = 'From Date'
        end
        object Label114: TLabel
          Left = 10
          Top = 35
          Width = 36
          Height = 14
          Caption = 'To Date'
        end
        object CleanupDateFrom: TDateTimePicker
          Left = 100
          Top = 12
          Width = 92
          Height = 22
          Date = 36526.540688657400000000
          Time = 36526.540688657400000000
          ParseInput = True
          TabOrder = 0
        end
        object CleanupDateTo: TDateTimePicker
          Left = 100
          Top = 35
          Width = 92
          Height = 22
          Date = 43466.540688657400000000
          Time = 43466.540688657400000000
          TabOrder = 1
        end
      end
      object CleanupFileMask: TEdit
        Left = 93
        Top = 165
        Width = 96
        Height = 22
        Hint = 'ie log-2021*.log'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Text = 'log*.log'
      end
      object CleanupSubDirs: TCheckBox
        Left = 217
        Top = 165
        Width = 174
        Height = 17
        Caption = 'Sub Directories'
        TabOrder = 5
      end
      object CleanupPath: TEdit
        Left = 5
        Top = 130
        Width = 418
        Height = 22
        TabOrder = 3
        Text = 'c:\tempfiles'
      end
      object doCleanupCheck: TButton
        Left = 137
        Top = 238
        Width = 101
        Height = 25
        Caption = 'Check Path'
        TabOrder = 6
        OnClick = doCleanupCheckClick
      end
      object doCleanupStart: TButton
        Left = 15
        Top = 238
        Width = 101
        Height = 25
        Caption = 'Start Clean-up'
        TabOrder = 7
        OnClick = doCleanupStartClick
      end
    end
  end
  object LogDelim: TMemo
    Left = 0
    Top = 365
    Width = 908
    Height = 137
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 14
  end
  object ShowDiagsHigh: TCheckBox
    Left = 460
    Top = 120
    Width = 231
    Height = 17
    Caption = 'Show High Level Diagnostic Information'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object doExit: TButton
    Left = 474
    Top = 255
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 12
    OnClick = doExitClick
  end
  object ShowDiagsLow: TCheckBox
    Left = 460
    Top = 140
    Width = 231
    Height = 17
    Caption = 'Show Low Level Diagnostic Information'
    TabOrder = 8
  end
  object ShowDiagsSSL: TCheckBox
    Left = 460
    Top = 160
    Width = 231
    Height = 17
    Caption = 'Show SSL Dump Diagnostic Information'
    TabOrder = 9
  end
  object doClear: TButton
    Left = 570
    Top = 255
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 13
    OnClick = doClearClick
  end
  object ShowXProgesss: TCheckBox
    Left = 460
    Top = 100
    Width = 140
    Height = 17
    Caption = 'Show Extended Progress'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object RevokeCheck: TCheckBox
    Left = 605
    Top = 80
    Width = 121
    Height = 17
    Caption = 'SSL Revoke Check'
    TabOrder = 4
  end
  object VerifyCertMode: TRadioGroup
    Left = 749
    Top = 80
    Width = 151
    Height = 68
    Caption = 'Verify Certificate Mode'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'PEM Bundle File'
      'Windows Cert Store')
    TabOrder = 5
  end
  object ReportChain: TCheckBox
    Left = 460
    Top = 80
    Width = 141
    Height = 17
    Caption = 'Report SSL Certificates'
    TabOrder = 3
  end
  object SslSecurity: TComboBox
    Left = 713
    Top = 165
    Width = 182
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    ItemIndex = 0
    TabOrder = 10
    Text = 'Ignore'
    Items.Strings = (
      'Ignore'
      'None'
      'SSLv3 Only'
      'TLSv1 Only'
      'TLSv1.1 Only'
      'TLSv1.2 Only'
      'TLSv1.3 Only'
      'TLSv1 or Better'
      'TLSv1.1 or Better'
      'TLSv1.2 or Better'
      'Backward Ciphers'
      'Intermediate Ciphers'
      'High Ciphers, 2048 keys'
      'High Ciphers, 3072 keys'
      'High Ciphers, 7680 keys')
  end
  object DirLogs: TEdit
    Left = 537
    Top = 50
    Width = 341
    Height = 22
    TabOrder = 1
  end
  object XferSockFamily: TRadioGroup
    Left = 793
    Top = 193
    Width = 102
    Height = 96
    Caption = 'Socket Family'
    ItemIndex = 0
    Items.Strings = (
      'Any'
      'Prefer IPv4'
      'Prefer IPv6'
      'Only IPv4 '
      'Only IPv6')
    TabOrder = 11
  end
  object TimerUpdates: TTimer
    OnTimer = TimerUpdatesTimer
    Left = 450
    Top = 390
  end
end
