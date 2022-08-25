object Form1: TForm1
  Left = 77
  Top = 98
  Caption = 'TMagRas - test large number of connections'
  ClientHeight = 572
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial Narrow'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 247
    Top = 89
    Width = 218
    Height = 14
    Caption = 'Phonebook Entries Mask (number replaces #)'
  end
  object Label2: TLabel
    Left = 243
    Top = 168
    Width = 71
    Height = 14
    Caption = 'Number Range'
  end
  object Label3: TLabel
    Left = 383
    Top = 168
    Width = 9
    Height = 14
    Caption = 'to'
  end
  object LabelTot: TLabel
    Left = 247
    Top = 47
    Width = 64
    Height = 14
    Caption = 'Total Entries: '
  end
  object Label4: TLabel
    Left = 252
    Top = 457
    Width = 158
    Height = 36
    Caption = 
      'This test program will create large '#13#10'numbers of RAS phone book ' +
      'entries, '#13#10'and allow them to be deleted again'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelProgress: TLabel
    Left = 243
    Top = 313
    Width = 44
    Height = 14
    Caption = 'Progress'
  end
  object Entries: TListBox
    Left = 5
    Top = 5
    Width = 225
    Height = 565
    ItemHeight = 14
    MultiSelect = True
    TabOrder = 0
  end
  object doCreate: TButton
    Left = 350
    Top = 9
    Width = 70
    Height = 24
    Caption = 'Create Entries'
    TabOrder = 1
    OnClick = doCreateClick
  end
  object doDelete: TButton
    Left = 247
    Top = 387
    Width = 132
    Height = 24
    Caption = 'Delete Selected Entries'
    TabOrder = 2
    OnClick = doDeleteClick
  end
  object doShowEntries: TButton
    Left = 247
    Top = 9
    Width = 76
    Height = 24
    Caption = 'Show Entries'
    TabOrder = 3
    OnClick = doShowEntriesClick
  end
  object NameMask: TEdit
    Left = 243
    Top = 117
    Width = 220
    Height = 22
    TabOrder = 4
    Text = 'Test Entry #'
  end
  object NumLow: TSpinEdit
    Left = 322
    Top = 163
    Width = 52
    Height = 23
    MaxValue = 999998
    MinValue = 1
    TabOrder = 5
    Value = 1001
  end
  object NumHigh: TSpinEdit
    Left = 401
    Top = 163
    Width = 57
    Height = 23
    MaxValue = 99999
    MinValue = 1
    TabOrder = 6
    Value = 1020
  end
  object doExit: TButton
    Left = 261
    Top = 537
    Width = 70
    Height = 23
    Caption = 'Exit'
    TabOrder = 7
    OnClick = doExitClick
  end
  object Device: TComboBox
    Left = 243
    Top = 201
    Width = 220
    Height = 22
    Style = csDropDownList
    TabOrder = 8
  end
  object entPhoneBook: TRadioGroup
    Left = 243
    Top = 233
    Width = 108
    Height = 48
    Caption = 'Phonebook Location'
    ItemIndex = 1
    Items.Strings = (
      'Current User'
      'All Users')
    TabOrder = 9
  end
  object MagRasCon: TMagRasCon
    PhoneBookPath = 'C:\ProgramData\Microsoft\Network\Connections\Pbk\rasphone.pbk'
    PBLocation = 1
    bDefaultCreds = False
    SubEntry = 1
    CountryID = 0
    CountryCode = 0
    EntryOptions = 0
    DialMode = 0
    Left = 445
    Top = 310
  end
  object MagRasEdt: TMagRasEdt
    bUseCountryAndAreaCodes = False
    bSpecificIPAddress = False
    bSpecificNameServers = False
    bHeaderCompression = False
    bRemoteDefaultGateway = False
    bDisableLCPExtensions = False
    bTerminalBeforeDial = False
    bTerminalAfterDial = False
    bModemLights = False
    bSoftwareCompression = False
    bRequireEncryptedPassword = False
    bRequireMSEncryptedPassword = False
    bRequireDataEncryption = False
    bNetworkLogon = False
    bUseLogonCredentials = False
    bPromoteAlternates = False
    bSecureLocalFiles = False
    bRequireEAP = False
    bRequirePAP = False
    bRequireSPAP = False
    bCustom = False
    bPreviewPhoneNumber = False
    bSharedPhoneNumbers = False
    bPreviewUserPw = False
    bPreviewDomain = False
    bShowDialingProgress = False
    bRequireCHAP = False
    bRequireMsCHAP = False
    bRequireMsCHAP2 = False
    bRequireW95MSCHAP = False
    bCustomScript = False
    CountryCode = 0
    CountryID = 0
    IPAddress = '0.0.0.0'
    DNSAddress = '0.0.0.0'
    DNSAddressAlt = '0.0.0.0'
    WINSAddress = '0.0.0.0'
    WINSAddressAlt = '0.0.0.0'
    FrameSize = 0
    FramingProtocol = framePPP
    bNetBEUI = False
    bNetIPX = False
    bNetTCPIP = False
    ISDNChannels = 0
    DialMode = dialAll
    DialExtraPercent = 0
    DialExtraSampleSeconds = 0
    HangUpExtraPercent = 0
    HangUpExtraSampleSeconds = 0
    IdleDisconnectSeconds = -1
    PType = typePhone
    EncryptionType = encryptOptional
    CustomAuthKey = 0
    VpnStrategy = vpnDefault
    bSecureFileAndPrint = False
    bSecureClientForMSNet = False
    bDontNegotiateMultilink = False
    bDontUseRasCredentials = False
    bUsePreSharedKey = False
    bInternet = False
    bDisableNbtOverIP = False
    bUseGlobalDeviceSettings = False
    bReconnectIfDropped = False
    bSharePhoneNumbers = False
    TcpWindowSize = 0
    RedialCount = 0
    RedialPause = 0
    PhoneBookPath = 'C:\ProgramData\Microsoft\Network\Connections\Pbk\rasphone.pbk'
    PBLocation = 1
    bDefaultCreds = False
    PasswordFlag = False
    Left = 440
    Top = 270
  end
end
