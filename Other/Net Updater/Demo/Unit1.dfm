object Form1: TForm1
  Left = 236
  Top = 172
  Width = 518
  Height = 348
  Caption = 'Net Updater Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 352
    Top = 16
    Width = 50
    Height = 13
    Caption = 'Run Mode'
  end
  object Label2: TLabel
    Left = 16
    Top = 8
    Width = 144
    Height = 20
    Caption = 'Demo Version 1.0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object CheckBox1: TCheckBox
    Left = 352
    Top = 64
    Width = 97
    Height = 17
    Caption = 'Active'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 352
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Use Cache'
    TabOrder = 1
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 352
    Top = 96
    Width = 97
    Height = 17
    Caption = 'Create Backup'
    TabOrder = 2
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 352
    Top = 112
    Width = 121
    Height = 17
    Caption = 'Hide File Location'
    TabOrder = 3
    OnClick = CheckBox4Click
  end
  object ComboBox1: TComboBox
    Left = 352
    Top = 32
    Width = 97
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'Normal'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Normal'
      'Silent'
      'Hidden')
  end
  object CheckBox5: TCheckBox
    Left = 352
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Ask to upgrade'
    TabOrder = 5
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 352
    Top = 144
    Width = 153
    Height = 17
    Caption = 'Show no update message'
    TabOrder = 6
    OnClick = CheckBox6Click
  end
  object CheckBox7: TCheckBox
    Left = 352
    Top = 160
    Width = 97
    Height = 17
    Caption = 'Stay on top'
    TabOrder = 7
    OnClick = CheckBox7Click
  end
  object CheckBox8: TCheckBox
    Left = 352
    Top = 176
    Width = 97
    Height = 17
    Caption = 'Warn on cancel'
    TabOrder = 8
    OnClick = CheckBox8Click
  end
  object CheckBox9: TCheckBox
    Left = 352
    Top = 192
    Width = 97
    Height = 17
    Caption = 'Warn on restart'
    TabOrder = 9
    OnClick = CheckBox9Click
  end
  object Button1: TButton
    Left = 352
    Top = 248
    Width = 145
    Height = 25
    Caption = 'Check for update'
    TabOrder = 10
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 40
    Width = 321
    Height = 257
    BevelInner = bvSpace
    BevelOuter = bvNone
    Ctl3D = False
    Lines.Strings = (
      'Memo1')
    ParentCtl3D = False
    ScrollBars = ssVertical
    TabOrder = 11
  end
  object kmNetUpdate1: TkmNetUpdate
    AutoCheckDelay = 2000
    CacheOptions = [coAlwaysReload]
    CheckDay = Sunday
    CheckTime = '2:00:00 AM'
    CreateBackup = True
    HideFileLocation = False
    InternetOptions = [ioKeepConnection]
    MsgStrs.Strings = (
      'Net Update'
      'Checking for updates...'
      'Downloading files...'
      'Update completed!'
      'Current file:'
      'Destination: %s'
      'Source: %s'
      'File: %s'
      'Retreiving file information... please wait.'
      'Elapsed: %d.%.2d min, remaining: %d.%.2d min  (%.2n of %.2n Kb)'
      'Downloading file %d of %d'
      'Installing file %d of %d'
      
        'The update downloaded successfully. Press OK to restart applicat' +
        'ion.'
      
        'The application, %s, is open. Close the application and press OK' +
        ' to continue.'
      
        'Application is still open. Do you want Net Update to close the a' +
        'pplication.'
      '&Yes'
      '&No'
      '&OK'
      '&Cancel'
      '&Next >'
      '&Later'
      '&Finish'
      'Warning'
      'Error'
      'Information'
      'A newer version of this software was released.'
      'Do you want to update your version with the latest one?'
      'A newer version of this software was not found.'
      'Update cancelled. File not found: %s'
      'Update cancelled. File lost: %s'
      'Are you sure you want to exit?'
      'Host is unreachable. Please check your Internet connection.'
      'The update is not complete. Are you sure you want to cancel?'
      'Error: 404 - File not found or inaccessible: '
      
        'Connection with remote server lost. Restore connection and try a' +
        'gain.')
    ProxyBypass = '127.0.0.1,'
    ProxyPort = 8080
    RunMode = rmNormal
    Schedule = schNone
    ShowMessages = [mAskUpgrade, mPromptCancel, mNoFile]
    ThreadPriority = tpNormal
    FTPPassive = True
    Login = False
    Port = 80
    URLFile = 'demo1_01.inf'
    URLPath = 'www.kidmoses.com/components/demo/netupdater/'
    URLProtocol = pHTTP
    UseRegistry = False
    VersionControl = byVersion
    VersionDate = '2006-10-03 19:54:46'
    VersionNumber = '1.00'
    Left = 208
    Top = 8
  end
end
