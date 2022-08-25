object Form1: TForm1
  Left = 196
  Top = 51
  Caption = 
    'Magenta Systems File Transfer Components Demo Wide -27th Novembe' +
    'r 2018 - v4.7'
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
    Width = 426
    Height = 42
    Caption = 
      'The demonstration application is designed to show how the three ' +
      'Magenta File Transfer components are used from code, there are n' +
      'umerous properties at are set in code, not through the GUI.  '
    WordWrap = True
  end
  object LabelProgress: TLabel
    Left = 10
    Top = 295
    Width = 890
    Height = 45
    AutoSize = False
    Caption = 'Progress:'
    WordWrap = True
  end
  object LabelVersion: TLabel
    Left = 460
    Top = 48
    Width = 37
    Height = 14
    Caption = 'Version'
  end
  object LabelSslState: TLabel
    Left = 640
    Top = 254
    Width = 66
    Height = 14
    Caption = 'LabelSslState'
  end
  object Label26: TLabel
    Left = 460
    Top = 209
    Width = 384
    Height = 28
    Caption = 
      'This Wide demo supports the full Unicode components for Delphi 7' +
      ' and later, but the GUI is only Unicode when built with Delphi 2' +
      '009 and later.  '
    WordWrap = True
  end
  object lbl1: TLabel
    Left = 713
    Top = 155
    Width = 92
    Height = 14
    Caption = 'SSL Client Security'
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
    Left = 8
    Top = 8
    Width = 440
    Height = 281
    ActivePage = TabSheet1
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'TMagFileCopyW'
      object Label2: TLabel
        Left = 5
        Top = 0
        Width = 82
        Height = 14
        Caption = 'Source Directory'
      end
      object Label3: TLabel
        Left = 5
        Top = 52
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
        Left = 31
        Top = 159
        Width = 101
        Height = 25
        Caption = 'List Source Files'
        TabOrder = 6
        OnClick = doCopyListClick
      end
      object CopySrcDir: TEdit
        Left = 5
        Top = 20
        Width = 411
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
        Left = 3
        Top = 83
        Width = 411
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
        Top = 165
        Width = 136
        Height = 17
        Caption = 'Include Sub Directories'
        Checked = True
        State = cbChecked
        TabOrder = 12
      end
      object CopyEmptyDirs: TCheckBox
        Left = 270
        Top = 185
        Width = 136
        Height = 17
        Caption = 'Copy Empty Directories'
        TabOrder = 13
      end
      object CopyIgnorePath: TEdit
        Left = 242
        Top = 50
        Width = 174
        Height = 22
        TabOrder = 2
        Text = 'c:\temp\'
      end
      object CopyWow64Disable: TCheckBox
        Left = 270
        Top = 205
        Width = 136
        Height = 17
        Caption = 'Disable Wow64 Redirect'
        TabOrder = 14
      end
      object CopyReplace: TCheckBox
        Left = 270
        Top = 225
        Width = 71
        Height = 17
        Caption = 'Replace'
        TabOrder = 15
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TMagFtpW'
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
        Left = 212
        Top = 40
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
        Width = 136
        Height = 22
        TabOrder = 1
        Text = 'anonymous'
      end
      object FtpPassword: TEdit
        Left = 285
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
        Width = 301
        Height = 22
        ItemHeight = 14
        TabOrder = 0
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
        TabOrder = 14
        Text = '0'
      end
      object FtpNoMd5: TCheckBox
        Left = 5
        Top = 195
        Width = 126
        Height = 17
        Caption = 'No MD5 Check'
        TabOrder = 10
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
        TabOrder = 15
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
        TabOrder = 16
      end
      object FtpNoCrc: TCheckBox
        Left = 5
        Top = 215
        Width = 126
        Height = 17
        Caption = 'No CRC Check'
        TabOrder = 11
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
        Left = 125
        Top = 60
        Width = 77
        Height = 14
        Caption = 'Ignore Directory'
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
        TabOrder = 2
        Text = 'd*.zip'
      end
      object FtpReplace: TCheckBox
        Left = 5
        Top = 55
        Width = 71
        Height = 17
        Caption = 'Replace'
        TabOrder = 3
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
        Width = 114
        Height = 25
        Caption = 'Check Download'
        TabOrder = 11
        OnClick = FtpDownload
      end
      object doFtpDownFiles: TButton
        Left = 121
        Top = 185
        Width = 113
        Height = 25
        Caption = 'Download Files'
        TabOrder = 14
        OnClick = FtpDownload
      end
      object doFtpAbort: TButton
        Left = 359
        Top = 153
        Width = 66
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 13
        OnClick = doAbortClick
      end
      object doFtpUpCheck: TButton
        Left = 245
        Top = 154
        Width = 101
        Height = 25
        Caption = 'Check Upload'
        TabOrder = 12
        OnClick = FtpUpload
      end
      object doFtpUpFiles: TButton
        Left = 245
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
        Left = 140
        Top = 83
        Width = 136
        Height = 17
        Caption = 'Include Sub Directories'
        TabOrder = 7
      end
      object FtpDelDone: TCheckBox
        Left = 5
        Top = 90
        Width = 136
        Height = 17
        Caption = 'Delete After Transfer'
        TabOrder = 6
      end
      object FtpDelOldTar: TCheckBox
        Left = 5
        Top = 75
        Width = 136
        Height = 13
        Caption = 'Delete Old Target Files'
        TabOrder = 5
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
        Left = 140
        Top = 103
        Width = 136
        Height = 17
        Caption = 'Copy Empty Directories'
        TabOrder = 8
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
        Left = 122
        Top = 214
        Width = 112
        Height = 25
        Caption = 'Download  (Thread)'
        TabOrder = 17
        OnClick = doFtpDownThreadClick
      end
      object doFtpUpThread: TButton
        Left = 245
        Top = 216
        Width = 101
        Height = 25
        Caption = 'Upload (Thread)'
        TabOrder = 18
        OnClick = doFtpUpThreadClick
      end
      object FtpIgnorePath: TEdit
        Left = 208
        Top = 55
        Width = 213
        Height = 22
        TabOrder = 4
        Text = '/work/'
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
      object Ftp1Path: TEdit
        Left = 75
        Top = 5
        Width = 341
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
        Width = 411
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
        TabOrder = 6
        OnClick = doFtpDown1Click
      end
      object doFtpUp1: TButton
        Left = 115
        Top = 200
        Width = 96
        Height = 25
        Caption = 'Single Upload'
        TabOrder = 7
        OnClick = doFtpUp1Click
      end
      object Ftp1UpFile: TEdit
        Left = 3
        Top = 130
        Width = 413
        Height = 22
        TabOrder = 3
        Text = 'c:\tempfiles\dunman.zip'
      end
      object FtpOneDelDone: TCheckBox
        Left = 85
        Top = 160
        Width = 136
        Height = 17
        Caption = 'Delete After Transfer'
        TabOrder = 5
        Visible = False
      end
      object FtpOneReplace: TCheckBox
        Left = 10
        Top = 160
        Width = 71
        Height = 17
        Caption = 'Replace'
        TabOrder = 4
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
      Caption = 'TMagHttpW'
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
        Left = 3
        Top = 175
        Width = 77
        Height = 14
        Caption = 'Target Directory'
      end
      object Label14: TLabel
        Left = 220
        Top = 175
        Width = 133
        Height = 14
        Caption = 'Bandwidth Limit (KBits/sec)'
        WordWrap = True
      end
      object HttpTarDir: TEdit
        Left = 3
        Top = 195
        Width = 418
        Height = 22
        TabOrder = 3
        Text = 'c:\tempfiles'
      end
      object doHttpDownCheck: TButton
        Left = 161
        Top = 222
        Width = 101
        Height = 25
        Caption = 'Check Down Files'
        TabOrder = 6
        OnClick = HttpDownload
      end
      object doHttpDownFiles: TButton
        Left = 29
        Top = 222
        Width = 101
        Height = 25
        Caption = 'Download Files'
        TabOrder = 5
        OnClick = HttpDownload
      end
      object doHttpAbort: TButton
        Left = 284
        Top = 222
        Width = 101
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 7
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
      object HttpBandWidth: TEdit
        Left = 364
        Top = 170
        Width = 46
        Height = 22
        TabOrder = 4
        Text = '0'
      end
      object HttpReplace: TCheckBox
        Left = 217
        Top = 145
        Width = 71
        Height = 17
        Caption = 'Replace'
        TabOrder = 2
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
    end
  end
  object LogDelim: TMemo
    Left = 0
    Top = 347
    Width = 908
    Height = 155
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
    Top = 140
    Width = 231
    Height = 17
    Caption = 'Show High Level Diagnostic Information'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object doExit: TButton
    Left = 465
    Top = 253
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 11
    OnClick = doExitClick
  end
  object doTest: TButton
    Left = 820
    Top = 254
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 13
    Visible = False
    OnClick = doTestClick
  end
  object ShowDiagsLow: TCheckBox
    Left = 460
    Top = 160
    Width = 231
    Height = 17
    Caption = 'Show Low Level Diagnostic Information'
    TabOrder = 8
  end
  object ShowDiagsSSL: TCheckBox
    Left = 460
    Top = 180
    Width = 231
    Height = 17
    Caption = 'Show SSL Dump Diagnostic Information'
    TabOrder = 9
  end
  object doClear: TButton
    Left = 554
    Top = 253
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 12
    OnClick = doClearClick
  end
  object ShowDiagsUtf8: TCheckBox
    Left = 460
    Top = 120
    Width = 160
    Height = 17
    Caption = 'Show FTP UTF-8 Commands'
    TabOrder = 6
  end
  object ShowXProgesss: TCheckBox
    Left = 460
    Top = 100
    Width = 140
    Height = 17
    Caption = 'Show Extended Progress'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object ReportChain: TCheckBox
    Left = 460
    Top = 80
    Width = 141
    Height = 17
    Caption = 'Report SSL Certificates'
    TabOrder = 2
  end
  object RevokeCheck: TCheckBox
    Left = 605
    Top = 80
    Width = 121
    Height = 17
    Caption = 'SSL Revoke Check'
    TabOrder = 3
  end
  object VerifyCertMode: TRadioGroup
    Left = 745
    Top = 80
    Width = 151
    Height = 68
    Caption = 'Verify Certificate Mode'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'PEM Bundle File'
      'Windows Cert Store')
    TabOrder = 4
  end
  object SslSecurity: TComboBox
    Left = 713
    Top = 175
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
      'TLSv1.2 Only'
      'TLSv1.3 Only'
      'TLSv1 or better'
      'TLSv1.1 or better'
      'TLSv1.2 or better'
      'Backward Ciphers'
      'Intermedate Ciphers'
      'High Ciphers, 2048 keys'
      'High Ciphers, 3072 keys'
      'High Ciphers, 7680 keys')
  end
end
