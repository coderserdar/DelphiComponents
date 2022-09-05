object Form1: TForm1
  Left = 196
  Top = 51
  Caption = 
    'Magenta Systems File Transfer Components Demo - 17th May 2009 - ' +
    'v3.4'
  ClientHeight = 756
  ClientWidth = 767
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
    Left = 440
    Top = 2
    Width = 303
    Height = 112
    Caption = 
      'The demonstration application is designed to show how the three ' +
      'Magenta File Transfer components are used from code, there are n' +
      'umerous properties at are set in code, not through the GUI.  To ' +
      'see a demonstration with a full GUI, please download DUN Manager' +
      ' from http://www.magsys.co.uk/dunman/, look under Scheduled Task' +
      ' Properties, at Sync Files. FTP Upload, FTP Download and HTTP Do' +
      'wnload. '
    WordWrap = True
  end
  object LabelProgress: TLabel
    Left = 10
    Top = 295
    Width = 47
    Height = 14
    Caption = 'Progress:'
  end
  object LabelVersion: TLabel
    Left = 440
    Top = 120
    Width = 38
    Height = 14
    Caption = 'Version'
  end
  object LabelSslState: TLabel
    Left = 615
    Top = 259
    Width = 66
    Height = 14
    Caption = 'LabelSslState'
  end
  object Label26: TLabel
    Left = 440
    Top = 205
    Width = 311
    Height = 28
    Caption = 
      'This demo supports full Unicode when built with Delphi 2009 and ' +
      'later.  '
    WordWrap = True
  end
  object LogText: TMemo
    Left = 0
    Top = 485
    Width = 767
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
    Width = 421
    Height = 281
    ActivePage = TabSheet2
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'TMagFileCopyW'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Width = 78
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
      object doCopyCheck: TButton
        Left = 30
        Top = 190
        Width = 101
        Height = 25
        Caption = 'Check Copy Files'
        TabOrder = 4
        OnClick = CopyFiles
      end
      object doCopyList: TButton
        Left = 30
        Top = 160
        Width = 101
        Height = 25
        Caption = 'List Source Files'
        TabOrder = 3
        OnClick = doCopyListClick
      end
      object CopySrcDir: TEdit
        Left = 5
        Top = 20
        Width = 396
        Height = 22
        TabOrder = 0
        Text = 'd:\windows\system32'
      end
      object CopySrcFile: TEdit
        Left = 105
        Top = 45
        Width = 121
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
        TabOrder = 5
        OnClick = CopyFiles
      end
      object CopyTarDir: TEdit
        Left = 5
        Top = 85
        Width = 391
        Height = 22
        TabOrder = 2
        Text = 'd:\tempfiles'
      end
      object doCopyAbort: TButton
        Left = 150
        Top = 160
        Width = 101
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 6
        OnClick = doAbortClick
      end
      object doDeleteCheck: TButton
        Left = 150
        Top = 190
        Width = 101
        Height = 25
        Caption = 'Check Delete Files'
        TabOrder = 7
        OnClick = DeleteFiles
      end
      object doDeleteFiles: TButton
        Left = 150
        Top = 220
        Width = 101
        Height = 25
        Caption = 'Delete Target Files'
        TabOrder = 8
        OnClick = DeleteFiles
      end
      object NetLogon: TEdit
        Left = 45
        Top = 115
        Width = 111
        Height = 22
        TabOrder = 9
      end
      object NetPassword: TEdit
        Left = 230
        Top = 115
        Width = 101
        Height = 22
        PasswordChar = '*'
        TabOrder = 10
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TMagFtp'
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
      object Label14: TLabel
        Left = 10
        Top = 220
        Width = 298
        Height = 28
        Caption = 
          'The magsys FTP server is anonymous downloads only, you'#39'll need y' +
          'our own upload host '
        WordWrap = True
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
        Width = 153
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
        TabOrder = 0
        Text = 'anonymous'
      end
      object FtpPassword: TEdit
        Left = 230
        Top = 35
        Width = 101
        Height = 22
        PasswordChar = '*'
        TabOrder = 1
        Text = 'test@'
      end
      object FtpServerType: TComboBox
        Left = 90
        Top = 65
        Width = 241
        Height = 20
        Style = csOwnerDrawFixed
        ItemHeight = 14
        TabOrder = 2
      end
      object FtpHost: TComboBox
        Left = 55
        Top = 5
        Width = 276
        Height = 22
        ItemHeight = 14
        TabOrder = 3
        Text = 'www.magsys.co.uk'
        Items.Strings = (
          'www.magsys.co.uk'
          'secure.magsys.co.uk'
          'ics.ftptest.org'
          'filezilla.ftptest.org'
          'wsftp.ftptest.org'
          'servu.ftptest.org'
          'gene6.ftptest.org'
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
        TabOrder = 9
        Text = '0'
      end
      object FtpNoMd5Crc: TCheckBox
        Left = 5
        Top = 195
        Width = 126
        Height = 17
        Caption = 'No MD5 or CRC Check'
        TabOrder = 10
      end
      object FtpNoZlib: TCheckBox
        Left = 5
        Top = 175
        Width = 126
        Height = 17
        Caption = 'No Mode Z Compress'
        TabOrder = 11
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
        TabOrder = 13
      end
      object ftpNoHost: TCheckBox
        Left = 145
        Top = 195
        Width = 117
        Height = 17
        Caption = 'No HOST Command'
        TabOrder = 14
      end
      object ftpIgnoreUtf8: TCheckBox
        Left = 285
        Top = 195
        Width = 100
        Height = 17
        Caption = 'Ignore UTF8 '
        TabOrder = 15
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Multi FTP'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label8: TLabel
        Left = 5
        Top = 10
        Width = 21
        Height = 14
        Caption = 'Path'
      end
      object Label13: TLabel
        Left = 125
        Top = 40
        Width = 82
        Height = 14
        Caption = 'Source File Mask'
      end
      object Label12: TLabel
        Left = 5
        Top = 105
        Width = 73
        Height = 14
        Caption = 'Local Directory'
      end
      object FtpPath: TEdit
        Left = 60
        Top = 5
        Width = 341
        Height = 22
        TabOrder = 0
        Text = '/'
      end
      object FtpSrcFile: TEdit
        Left = 210
        Top = 35
        Width = 191
        Height = 22
        TabOrder = 1
        Text = 'd*.zip'
      end
      object FtpReplace: TCheckBox
        Left = 5
        Top = 60
        Width = 71
        Height = 17
        Caption = 'Replace'
        TabOrder = 2
      end
      object FtpLocDir: TEdit
        Left = 5
        Top = 125
        Width = 386
        Height = 22
        TabOrder = 3
        Text = 'd:\tempfiles'
      end
      object doFtpDownCheck: TButton
        Left = 30
        Top = 185
        Width = 101
        Height = 25
        Caption = 'Check Download'
        TabOrder = 4
        OnClick = FtpDownload
      end
      object doFtpDownFiles: TButton
        Left = 30
        Top = 215
        Width = 101
        Height = 25
        Caption = 'Download Files'
        TabOrder = 5
        OnClick = FtpDownload
      end
      object doFtpAbort: TButton
        Left = 140
        Top = 155
        Width = 101
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 6
        OnClick = doAbortClick
      end
      object doFtpUpCheck: TButton
        Left = 140
        Top = 185
        Width = 101
        Height = 25
        Caption = 'Check Upload'
        TabOrder = 7
        OnClick = FtpUpload
      end
      object doFtpUpFiles: TButton
        Left = 140
        Top = 215
        Width = 101
        Height = 25
        Caption = 'Upload Files'
        TabOrder = 8
        OnClick = FtpUpload
      end
      object doFtpList: TButton
        Left = 30
        Top = 155
        Width = 101
        Height = 25
        Caption = 'List Host Files'
        TabOrder = 9
        OnClick = doFtpListClick
      end
      object FtpSubdirs: TCheckBox
        Left = 150
        Top = 80
        Width = 136
        Height = 17
        Caption = 'Include Sub Directories'
        TabOrder = 10
      end
      object FtpDelDone: TCheckBox
        Left = 150
        Top = 60
        Width = 136
        Height = 17
        Caption = 'Delete After Transfer'
        TabOrder = 11
      end
      object FtpDelOldTar: TCheckBox
        Left = 5
        Top = 82
        Width = 136
        Height = 13
        Caption = 'Delete Old Target Files'
        TabOrder = 12
      end
      object FtpCopyAllDir: TCheckBox
        Left = 5
        Top = 40
        Width = 116
        Height = 17
        Caption = 'Copy All Directory '
        TabOrder = 13
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Single FTP'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      object Ftp1Path: TEdit
        Left = 75
        Top = 5
        Width = 326
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
        Left = 5
        Top = 80
        Width = 396
        Height = 22
        TabOrder = 2
        Text = 'd:\tempfiles'
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
        TabOrder = 5
        OnClick = doFtpUp1Click
      end
      object Ftp1UpFile: TEdit
        Left = 5
        Top = 130
        Width = 396
        Height = 22
        TabOrder = 3
        Text = 'd:\tempfiles\dunman.zip'
      end
      object FtpOneDelDone: TCheckBox
        Left = 85
        Top = 160
        Width = 136
        Height = 17
        Caption = 'Delete After Transfer'
        TabOrder = 6
        Visible = False
      end
      object FtpOneReplace: TCheckBox
        Left = 10
        Top = 160
        Width = 71
        Height = 17
        Caption = 'Replace'
        TabOrder = 7
      end
      object doFtpAbort1: TButton
        Left = 225
        Top = 200
        Width = 76
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 8
        OnClick = doAbortClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TMagHttp'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label5: TLabel
        Left = 5
        Top = 0
        Width = 87
        Height = 14
        Caption = 'Source HTTP Path'
      end
      object Label6: TLabel
        Left = 5
        Top = 50
        Width = 82
        Height = 14
        Caption = 'Source File Mask'
      end
      object Label7: TLabel
        Left = 5
        Top = 70
        Width = 78
        Height = 14
        Caption = 'Target Directory'
      end
      object HttpTarDir: TEdit
        Left = 5
        Top = 85
        Width = 391
        Height = 22
        TabOrder = 2
        Text = 'd:\tempfiles'
      end
      object doHttpDownCheck: TButton
        Left = 30
        Top = 115
        Width = 101
        Height = 25
        Caption = 'Check Down Files'
        TabOrder = 3
        OnClick = HttpDownload
      end
      object doHttpDownFiles: TButton
        Left = 30
        Top = 145
        Width = 101
        Height = 25
        Caption = 'Download Files'
        TabOrder = 4
        OnClick = HttpDownload
      end
      object doHttpAbort: TButton
        Left = 150
        Top = 115
        Width = 101
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 5
        OnClick = doAbortClick
      end
      object HttpSrcDir: TComboBox
        Left = 5
        Top = 20
        Width = 401
        Height = 22
        ItemHeight = 0
        TabOrder = 0
        Text = 'http://www.magsys.co.uk/dunman/default.asp'
        Items.Strings = (
          'http://www.magsys.co.uk/dunman/default.asp'
          'http://www.magsys.co.uk/telecom/residx.htm'
          'http://www.sophos.com/downloads/ide/index.html'
          'http://web4.magenta/dunman/default.asp'
          'http://web4.magenta/telecom/residx.htm'
          'https://secure.magsys.co.uk/secure/download.asp')
      end
      object HttpSrcFile: TComboBox
        Left = 100
        Top = 50
        Width = 111
        Height = 22
        ItemHeight = 0
        TabOrder = 1
        Text = '*.zip'
        Items.Strings = (
          '*.ide'
          '*.zip'
          '*.htm'
          '*.*')
      end
    end
  end
  object LogDelim: TMemo
    Left = 0
    Top = 330
    Width = 767
    Height = 155
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object ShowDiagsHigh: TCheckBox
    Left = 440
    Top = 140
    Width = 231
    Height = 17
    Caption = 'Show High Level Diagnostic Information'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object doExit: TButton
    Left = 444
    Top = 255
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 4
    OnClick = doExitClick
  end
  object doTest: TButton
    Left = 684
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 5
    Visible = False
    OnClick = doTestClick
  end
  object ShowDiagsLow: TCheckBox
    Left = 440
    Top = 163
    Width = 231
    Height = 17
    Caption = 'Show Low Level Diagnostic Information'
    TabOrder = 6
  end
  object ShowDiagsSSL: TCheckBox
    Left = 440
    Top = 186
    Width = 231
    Height = 17
    Caption = 'Show SSL Dump Diagnostic Information'
    TabOrder = 7
  end
  object doClear: TButton
    Left = 525
    Top = 255
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 8
    OnClick = doClearClick
  end
end
