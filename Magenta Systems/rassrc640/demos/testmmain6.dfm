object Form1: TForm1
  Left = 77
  Top = 98
  Caption = 'TMagRas - test large number of connections'
  ClientHeight = 616
  ClientWidth = 509
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 265
    Top = 95
    Width = 216
    Height = 13
    Caption = 'Phonebook Entries Mask (number replaces #)'
  end
  object Label2: TLabel
    Left = 260
    Top = 180
    Width = 72
    Height = 13
    Caption = 'Number Range'
  end
  object Label3: TLabel
    Left = 410
    Top = 180
    Width = 9
    Height = 13
    Caption = 'to'
  end
  object LabelTot: TLabel
    Left = 265
    Top = 50
    Width = 65
    Height = 13
    Caption = 'Total Entries: '
  end
  object Label4: TLabel
    Left = 270
    Top = 490
    Width = 177
    Height = 39
    Caption = 
      'This test program will create large '#13#10'numbers of RAS phone book ' +
      'entries, '#13#10'and allow them to be deleted again'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LabelProgress: TLabel
    Left = 260
    Top = 335
    Width = 41
    Height = 13
    Caption = 'Progress'
  end
  object Entries: TListBox
    Left = 5
    Top = 5
    Width = 241
    Height = 606
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
  end
  object doCreate: TButton
    Left = 375
    Top = 10
    Width = 75
    Height = 25
    Caption = 'Create Entries'
    TabOrder = 1
    OnClick = doCreateClick
  end
  object doDelete: TButton
    Left = 265
    Top = 415
    Width = 141
    Height = 25
    Caption = 'Delete Selected Entries'
    TabOrder = 2
    OnClick = doDeleteClick
  end
  object doShowEntries: TButton
    Left = 265
    Top = 10
    Width = 81
    Height = 25
    Caption = 'Show Entries'
    TabOrder = 3
    OnClick = doShowEntriesClick
  end
  object NameMask: TEdit
    Left = 260
    Top = 125
    Width = 236
    Height = 21
    TabOrder = 4
    Text = 'Test Entry #'
  end
  object NumLow: TSpinEdit
    Left = 345
    Top = 175
    Width = 56
    Height = 22
    MaxValue = 999998
    MinValue = 1
    TabOrder = 5
    Value = 1001
  end
  object NumHigh: TSpinEdit
    Left = 430
    Top = 175
    Width = 61
    Height = 22
    MaxValue = 99999
    MinValue = 1
    TabOrder = 6
    Value = 1020
  end
  object doExit: TButton
    Left = 280
    Top = 575
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 7
    OnClick = doExitClick
  end
  object Device: TComboBox
    Left = 260
    Top = 215
    Width = 236
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 8
  end
  object entPhoneBook: TRadioGroup
    Left = 260
    Top = 250
    Width = 116
    Height = 51
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
    CallbackId = 0
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
