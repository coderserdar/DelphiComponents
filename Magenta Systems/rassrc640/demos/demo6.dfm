object MainForm: TMainForm
  Left = 55
  Top = 66
  BorderStyle = bsSingle
  Caption = 
    'RAS Delphi Component Demo 6 Unicode - Not a fully functioning ap' +
    'plication'
  ClientHeight = 592
  ClientWidth = 919
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Label4: TLabel
    Left = 9
    Top = 334
    Width = 141
    Height = 56
    Caption = 
      'This demo monitors and controls connections started by other app' +
      'lications as well as those started here'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 10
    Top = 2
    Width = 133
    Height = 28
    Caption = 'Defined Connection Entries aka Phone Books'
    WordWrap = True
  end
  object Label60: TLabel
    Left = 100
    Top = 210
    Width = 45
    Height = 14
    Caption = 'ISDN Link'
  end
  object Status: TStatusBar
    Left = 0
    Top = 573
    Width = 919
    Height = 19
    Panels = <
      item
        Width = 300
      end
      item
        Width = 200
      end>
  end
  object doExit: TButton
    Left = 10
    Top = 400
    Width = 77
    Height = 25
    Caption = 'Exit'
    TabOrder = 0
    OnClick = doExitClick
  end
  object ConnList: TListBox
    Left = 8
    Top = 36
    Width = 199
    Height = 164
    ItemHeight = 14
    TabOrder = 1
    OnClick = ConnListClick
  end
  object doConnect: TButton
    Left = 10
    Top = 230
    Width = 77
    Height = 25
    Caption = 'Connect'
    TabOrder = 2
    OnClick = doConnectClick
  end
  object doDisConn: TButton
    Left = 10
    Top = 265
    Width = 77
    Height = 25
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 3
    OnClick = doDisConnClick
  end
  object Debug: TCheckBox
    Left = 95
    Top = 305
    Width = 56
    Height = 17
    Caption = 'Debug'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object doStat: TButton
    Left = 10
    Top = 300
    Width = 75
    Height = 25
    Caption = 'Dump Stats'
    TabOrder = 4
    OnClick = doStatClick
  end
  object OptSpeaker: TCheckBox
    Left = 10
    Top = 210
    Width = 81
    Height = 17
    Caption = 'Speaker On'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object DialLink: TComboBox
    Left = 100
    Top = 230
    Width = 46
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    ItemIndex = 0
    TabOrder = 8
    Text = 'All'
    Items.Strings = (
      'All'
      '1'
      '2')
  end
  object MainPages: TPageControl
    Left = 213
    Top = 2
    Width = 698
    Height = 576
    ActivePage = TabSheet9
    TabOrder = 9
    object TabSheet1: TTabSheet
      Caption = 'Online'
      object ConInfoList: TListView
        Left = 0
        Top = 0
        Width = 686
        Height = 96
        AllocBy = 5
        Columns = <
          item
            Caption = 'Name'
            Width = 100
          end
          item
            Caption = 'Status'
            Width = 200
          end
          item
            Caption = 'Link'
            Width = 40
          end
          item
            Caption = 'Tel Nr (not Win9x)'
            Width = 150
          end
          item
          end>
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object ConDevList: TListView
        Left = 3
        Top = 88
        Width = 684
        Height = 96
        AllocBy = 5
        Columns = <
          item
            Caption = 'Name'
            Width = 100
          end
          item
            Caption = 'Device'
            Width = 200
          end
          item
            Caption = 'Port'
          end
          item
            Caption = 'Type'
          end
          item
            Caption = 'Link'
            Width = 40
          end
          item
            Caption = 'Handle'
            Width = 80
          end
          item
            Caption = 'Par Handle'
            Width = 80
          end
          item
            Caption = 'Phonebook'
            Width = 150
          end
          item
            Caption = 'GUID'
            Width = 70
          end
          item
            Caption = 'Flags'
          end
          item
            Caption = 'LUID'
          end>
        GridLines = True
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
      end
      object ConSpeedList: TListView
        Left = 0
        Top = 190
        Width = 687
        Height = 96
        AllocBy = 5
        Columns = <
          item
            Caption = 'Name'
            Width = 100
          end
          item
            Caption = 'Speed'
            Width = 70
          end
          item
            Caption = 'Data Send'
            Width = 70
          end
          item
            Caption = 'Data Rcvd'
            Width = 70
          end
          item
            Caption = 'Auth'
          end
          item
            Caption = 'IP Address'
            Width = 200
          end>
        GridLines = True
        ReadOnly = True
        TabOrder = 2
        ViewStyle = vsReport
      end
      object ConnLog: TMemo
        Left = 0
        Top = 285
        Width = 686
        Height = 266
        ScrollBars = ssVertical
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Phonebooks'
      object LabelRasDevices: TLabel
        Left = 10
        Top = 215
        Width = 186
        Height = 14
        Caption = 'RAS Capable Modems and ISDN Cards'
      end
      object LabelDefPhonebook: TLabel
        Left = 10
        Top = 145
        Width = 96
        Height = 14
        Caption = 'LabelDefPhonebook'
        WordWrap = True
      end
      object LabelFileConns: TLabel
        Left = 10
        Top = 122
        Width = 93
        Height = 14
        Caption = 'Default Phonebook:'
      end
      object DeviceList: TListBox
        Left = 10
        Top = 235
        Width = 461
        Height = 307
        ItemHeight = 14
        TabOrder = 0
      end
      object GroupBox1: TGroupBox
        Left = 10
        Top = 10
        Width = 216
        Height = 104
        Caption = 'Dialog Boxes'
        TabOrder = 1
        object doCreateConn: TButton
          Left = 10
          Top = 25
          Width = 86
          Height = 25
          Caption = 'Create Entry'
          TabOrder = 0
          OnClick = doCreateConnClick
        end
        object doEditConn: TButton
          Left = 10
          Top = 60
          Width = 86
          Height = 25
          Caption = 'Edit Entry'
          TabOrder = 1
          OnClick = doEditConnClick
        end
        object doDUNDialog: TButton
          Left = 110
          Top = 25
          Width = 86
          Height = 25
          Caption = 'DUN Dialog'
          TabOrder = 2
          OnClick = doDUNDialogClick
        end
        object doDialDialog: TButton
          Left = 110
          Top = 60
          Width = 86
          Height = 25
          Caption = 'Dial Dialog'
          TabOrder = 3
          OnClick = doDialDialogClick
        end
      end
      object GroupBox2: TGroupBox
        Left = 235
        Top = 10
        Width = 221
        Height = 104
        Caption = 'Functions'
        TabOrder = 2
        object doRenameConn: TButton
          Left = 10
          Top = 60
          Width = 86
          Height = 25
          Caption = 'Rename Entry'
          TabOrder = 0
          OnClick = doRenameConnClick
        end
        object doCopyConn: TButton
          Left = 10
          Top = 25
          Width = 86
          Height = 25
          Caption = 'Copy Entry'
          TabOrder = 1
          OnClick = doCopyConnClick
        end
        object doDeleteConn: TButton
          Left = 106
          Top = 25
          Width = 105
          Height = 25
          Caption = 'Delete Entry'
          TabOrder = 2
          OnClick = doDeleteConnClick
        end
        object doShortcut: TButton
          Left = 106
          Top = 60
          Width = 105
          Height = 25
          Caption = 'Desktop Shortcut'
          TabOrder = 3
          OnClick = doShortcutClick
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Some Props'
      object Label41: TLabel
        Left = 55
        Top = 340
        Width = 277
        Height = 28
        Caption = 
          'This pages shows selected properties for the connection'#13#10'selecte' +
          'd for dialling in the list.  '
      end
      object doLogonUpdate: TButton
        Left = 14
        Top = 375
        Width = 100
        Height = 25
        Caption = 'Update Details'
        TabOrder = 3
        OnClick = doLogonUpdateClick
      end
      object doDumpEntSome: TButton
        Left = 170
        Top = 375
        Width = 91
        Height = 25
        Caption = 'Dump Rasentry'
        TabOrder = 4
        OnClick = doDumpEntSomeClick
      end
      object SBoxLogon: TGroupBox
        Left = 5
        Top = 5
        Width = 451
        Height = 86
        Caption = 'Logon Details'
        TabOrder = 0
        object Label2: TLabel
          Left = 10
          Top = 20
          Width = 60
          Height = 14
          Caption = 'Logon Name'
        end
        object Label3: TLabel
          Left = 10
          Top = 50
          Width = 50
          Height = 14
          Caption = 'Password'
        end
        object Label49: TLabel
          Left = 300
          Top = 10
          Width = 146
          Height = 71
          AutoSize = False
          Caption = 
            'Warning - on Windows 2000 the number of asterisks in the passwor' +
            'd field do not reflect the actual length of the password'
          WordWrap = True
        end
        object ConnUser: TEdit
          Left = 80
          Top = 15
          Width = 216
          Height = 22
          TabOrder = 0
          Text = 'ConnUser'
        end
        object ConnPw: TEdit
          Left = 80
          Top = 45
          Width = 216
          Height = 22
          PasswordChar = '*'
          TabOrder = 1
          Text = 'ConnPw'
        end
      end
      object SBoxDial: TGroupBox
        Left = 5
        Top = 95
        Width = 451
        Height = 156
        Caption = 'Dialling'
        TabOrder = 1
        object LabelCan: TLabel
          Left = 10
          Top = 22
          Width = 50
          Height = 28
          Caption = 'Canonical Number'
          WordWrap = True
        end
        object ConnPhone: TLabel
          Left = 10
          Top = 75
          Width = 73
          Height = 14
          Caption = 'Phone Number:'
        end
        object LabelCountry: TLabel
          Left = 10
          Top = 55
          Width = 44
          Height = 14
          Caption = 'Country: '
        end
        object Label10: TLabel
          Left = 265
          Top = 18
          Width = 90
          Height = 14
          Caption = 'Alternate Numbers'
        end
        object ConnDialNum: TLabel
          Left = 10
          Top = 95
          Width = 60
          Height = 14
          Caption = 'Dial Number:'
        end
        object Label7: TLabel
          Left = 10
          Top = 115
          Width = 75
          Height = 14
          Caption = 'Phonebook File:'
        end
        object LabelPhonebookPath: TLabel
          Left = 90
          Top = 115
          Width = 351
          Height = 31
          AutoSize = False
          Caption = 'LabelPhonebookPath'
          WordWrap = True
        end
        object ConnCanonical: TEdit
          Left = 80
          Top = 20
          Width = 171
          Height = 22
          TabOrder = 0
          Text = 'ConnCanonical'
          OnChange = ConnCanonicalChange
        end
        object AltNumList: TMemo
          Left = 265
          Top = 35
          Width = 161
          Height = 76
          Lines.Strings = (
            'AltNumList')
          ReadOnly = True
          TabOrder = 1
        end
      end
      object SBoxDevice: TGroupBox
        Left = 5
        Top = 255
        Width = 451
        Height = 81
        Caption = 'Device'
        TabOrder = 2
        object DeviceType: TLabel
          Left = 10
          Top = 38
          Width = 56
          Height = 14
          Caption = 'DeviceType'
        end
        object DeviceName: TLabel
          Left = 10
          Top = 18
          Width = 69
          Height = 14
          Caption = 'Device Name: '
        end
        object DevicePort: TLabel
          Left = 150
          Top = 38
          Width = 52
          Height = 14
          Caption = 'DevicePort'
        end
        object LabelSubEnt: TLabel
          Left = 10
          Top = 58
          Width = 83
          Height = 14
          Caption = 'Multiple Channels'
        end
        object LabelModemInfo: TLabel
          Left = 270
          Top = 38
          Width = 55
          Height = 14
          Caption = 'Modem Info'
        end
        object LabelDialMode: TLabel
          Left = 150
          Top = 58
          Width = 70
          Height = 14
          Caption = 'Multi Dial Mode'
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Full Props '
      object doPropNew: TButton
        Left = 140
        Top = 375
        Width = 56
        Height = 25
        Caption = 'New'
        TabOrder = 2
        OnClick = doPropNewClick
      end
      object doPropLoad: TButton
        Left = 10
        Top = 375
        Width = 56
        Height = 25
        Caption = 'Load'
        TabOrder = 0
        OnClick = doPropLoadClick
      end
      object doPropSave: TButton
        Left = 205
        Top = 375
        Width = 56
        Height = 25
        Caption = 'Save'
        TabOrder = 3
        OnClick = doPropSaveClick
      end
      object FullPropsPages: TPageControl
        Left = 3
        Top = 0
        Width = 483
        Height = 361
        ActivePage = TabDial
        TabOrder = 5
        object TabDial: TTabSheet
          Caption = 'Dialling'
          ExplicitWidth = 443
          object Label5: TLabel
            Left = 10
            Top = 10
            Width = 55
            Height = 14
            Caption = 'Entry Name'
          end
          object LocationBox: TGroupBox
            Tag = 1
            Left = 12
            Top = 35
            Width = 419
            Height = 291
            Caption = 'Location and Phone Number'
            TabOrder = 1
            object Label6: TLabel
              Left = 10
              Top = 45
              Width = 74
              Height = 14
              Caption = 'Country/Region'
            end
            object Label11: TLabel
              Left = 190
              Top = 75
              Width = 52
              Height = 14
              Caption = 'Area Code'
            end
            object Label12: TLabel
              Left = 10
              Top = 135
              Width = 87
              Height = 14
              Caption = 'Canonical Number'
            end
            object Label13: TLabel
              Left = 10
              Top = 105
              Width = 66
              Height = 14
              Caption = 'Local Number'
            end
            object Label14: TLabel
              Left = 10
              Top = 200
              Width = 90
              Height = 28
              Caption = 'Alternate Numbers'#13#10'(NT4/W2K)'
            end
            object LabelNumberDisp: TLabel
              Left = 25
              Top = 160
              Width = 84
              Height = 14
              Caption = 'LabelNumberDisp'
            end
            object LabelNumberDial: TLabel
              Left = 25
              Top = 180
              Width = 80
              Height = 14
              Caption = 'LabelNumberDial'
            end
            object Label44: TLabel
              Left = 10
              Top = 75
              Width = 66
              Height = 14
              Caption = 'Country Code'
            end
            object entUseCountryandAreaCodes: TCheckBox
              Left = 10
              Top = 20
              Width = 161
              Height = 17
              Caption = 'Use Country and Area Codes'
              TabOrder = 0
              OnClick = entUseCountryandAreaCodesClick
            end
            object entCountryName: TComboBox
              Left = 110
              Top = 40
              Width = 211
              Height = 22
              Style = csDropDownList
              ItemHeight = 14
              TabOrder = 2
              OnChange = entCountryNameChange
            end
            object entAreaCode: TEdit
              Tag = 2
              Left = 255
              Top = 70
              Width = 121
              Height = 22
              TabOrder = 5
              Text = 'entAreaCode'
              OnChange = NumberChanged
            end
            object entLocalNumber: TEdit
              Left = 110
              Top = 100
              Width = 271
              Height = 22
              TabOrder = 6
              Text = 'entLocalNumber'
              OnChange = NumberChanged
            end
            object entCanonNumber: TEdit
              Left = 110
              Top = 130
              Width = 271
              Height = 22
              ReadOnly = True
              TabOrder = 7
              Text = 'entCanonNumber'
              OnChange = CanonNumberChange
            end
            object entAlternates: TMemo
              Left = 110
              Top = 195
              Width = 271
              Height = 71
              ScrollBars = ssVertical
              TabOrder = 8
            end
            object entPromoteAlternates: TCheckBox
              Left = 10
              Top = 270
              Width = 181
              Height = 17
              Caption = 'Promote Alternate Numbers'
              TabOrder = 9
            end
            object entCountryCode: TEdit
              Left = 110
              Top = 70
              Width = 66
              Height = 22
              TabOrder = 4
              OnChange = NumberChanged
            end
            object doPropDial: TButton
              Left = 325
              Top = 10
              Width = 80
              Height = 25
              Caption = 'Dialling Props'
              TabOrder = 1
              OnClick = doPropDialClick
            end
            object entCountryId: TEdit
              Left = 335
              Top = 40
              Width = 36
              Height = 22
              Color = clInactiveCaptionText
              Enabled = False
              TabOrder = 3
              Text = 'entCountryId'
            end
          end
          object entEntryName: TEdit
            Left = 75
            Top = 5
            Width = 226
            Height = 22
            ReadOnly = True
            TabOrder = 0
            Text = 'entEntryName'
          end
        end
        object TabLogon: TTabSheet
          Caption = 'Logon && Device'
          ExplicitWidth = 443
          object DeviceBox: TGroupBox
            Left = 12
            Top = 162
            Width = 419
            Height = 119
            Caption = 'Dial Device'
            TabOrder = 1
            object Label15: TLabel
              Left = 10
              Top = 20
              Width = 27
              Height = 14
              Caption = 'Name'
            end
            object LabelPort: TLabel
              Left = 10
              Top = 80
              Width = 19
              Height = 14
              Caption = 'Port'
            end
            object Label17: TLabel
              Left = 10
              Top = 50
              Width = 23
              Height = 14
              Caption = 'Type'
            end
            object Label32: TLabel
              Left = 325
              Top = 50
              Width = 62
              Height = 14
              Caption = 'Idle Seconds'
            end
            object entDeviceType: TEdit
              Left = 50
              Top = 45
              Width = 76
              Height = 22
              ReadOnly = True
              TabOrder = 1
              Text = 'entDeviceType'
            end
            object entDevicePort: TEdit
              Left = 50
              Top = 75
              Width = 61
              Height = 22
              ReadOnly = True
              TabOrder = 2
              Text = 'entDevicePort'
            end
            object entDeviceName: TComboBox
              Left = 50
              Top = 15
              Width = 321
              Height = 22
              Style = csDropDownList
              ItemHeight = 14
              TabOrder = 0
              OnChange = entDeviceNameChange
            end
            object entIdleDisconnectSeconds: TSpinEdit
              Left = 325
              Top = 70
              Width = 76
              Height = 23
              MaxLength = 5
              MaxValue = 0
              MinValue = 0
              TabOrder = 4
              Value = 0
            end
            object entIdleOption: TRadioGroup
              Left = 145
              Top = 40
              Width = 166
              Height = 71
              Caption = 'Idle Disconnect (NT4/W2K/XP)'
              ItemIndex = 0
              Items.Strings = (
                'None'
                'From User Preferences'
                'Specified Period')
              TabOrder = 3
            end
          end
          object LogonBox: TGroupBox
            Left = 12
            Top = 5
            Width = 419
            Height = 156
            Caption = 'Logon Details '
            TabOrder = 0
            object Label16: TLabel
              Left = 10
              Top = 50
              Width = 50
              Height = 14
              Caption = 'Password'
            end
            object Label18: TLabel
              Left = 10
              Top = 20
              Width = 60
              Height = 14
              Caption = 'Logon Name'
            end
            object Label19: TLabel
              Left = 10
              Top = 95
              Width = 35
              Height = 14
              Caption = 'Domain'
            end
            object Label20: TLabel
              Left = 10
              Top = 125
              Width = 80
              Height = 14
              Caption = 'Callback Number'
            end
            object entUsername: TEdit
              Left = 105
              Top = 15
              Width = 231
              Height = 22
              TabOrder = 0
              Text = 'entUsername'
            end
            object entPassword: TEdit
              Left = 105
              Top = 40
              Width = 231
              Height = 22
              PasswordChar = '*'
              TabOrder = 1
              Text = 'entPassword'
            end
            object entDomain: TEdit
              Left = 105
              Top = 90
              Width = 231
              Height = 22
              TabOrder = 2
              Text = 'entDomain'
            end
            object entCallBackNumber: TEdit
              Left = 105
              Top = 120
              Width = 231
              Height = 22
              TabOrder = 3
              Text = 'entCallBackNumber'
            end
            object entDefaultCreds: TCheckBox
              Left = 10
              Top = 70
              Width = 316
              Height = 17
              Caption = 'Save logon for anyone who uses this computer'
              TabOrder = 4
              OnClick = entUseCountryandAreaCodesClick
            end
          end
          object AutoDialBox: TGroupBox
            Left = 12
            Top = 285
            Width = 419
            Height = 46
            Caption = 'Autodial (not settable)'
            TabOrder = 2
            object Label21: TLabel
              Left = 280
              Top = 25
              Width = 41
              Height = 14
              Caption = 'Function'
            end
            object Label22: TLabel
              Left = 10
              Top = 25
              Width = 40
              Height = 14
              Caption = 'Program'
            end
            object entAutoDialDll: TEdit
              Left = 60
              Top = 20
              Width = 211
              Height = 22
              ReadOnly = True
              TabOrder = 0
              Text = 'entAutoDialDll'
            end
            object entAutoDialFunc: TEdit
              Left = 330
              Top = 20
              Width = 81
              Height = 22
              ReadOnly = True
              TabOrder = 1
              Text = 'entAutoDialFunc'
            end
          end
        end
        object TabProtocol: TTabSheet
          Caption = 'Protocols'
          ExplicitWidth = 443
          object entFramingProtocol: TRadioGroup
            Left = 10
            Top = 5
            Width = 166
            Height = 81
            Caption = 'Network Framing Protocol'
            ItemIndex = 0
            Items.Strings = (
              'PPP (internet)'
              'SLIP (UNIX)'
              'Async Netbeui (not W2K)')
            TabOrder = 0
          end
          object ProtocolBox: TGroupBox
            Left = 190
            Top = 5
            Width = 121
            Height = 81
            Caption = 'Protocol'
            TabOrder = 1
            object entNetTCPIP: TCheckBox
              Left = 10
              Top = 15
              Width = 106
              Height = 17
              Caption = 'TCP/IP (internet)'
              TabOrder = 0
            end
            object entNetIPX: TCheckBox
              Left = 10
              Top = 35
              Width = 97
              Height = 17
              Caption = 'IPX (Netware)'
              TabOrder = 1
            end
            object entNetBEUI: TCheckBox
              Left = 10
              Top = 55
              Width = 106
              Height = 21
              Caption = 'NetBEUI (LANs)'
              TabOrder = 2
            end
          end
          object TCPIPBox: TGroupBox
            Left = 10
            Top = 95
            Width = 411
            Height = 136
            Caption = 'TCP/IP Settings'
            TabOrder = 3
            object Label23: TLabel
              Left = 150
              Top = 80
              Width = 26
              Height = 14
              Caption = 'WINS'
            end
            object Label24: TLabel
              Left = 285
              Top = 50
              Width = 13
              Height = 14
              Caption = 'Alt'
            end
            object Label25: TLabel
              Left = 150
              Top = 50
              Width = 21
              Height = 14
              Caption = 'DNS'
            end
            object Label26: TLabel
              Left = 285
              Top = 80
              Width = 13
              Height = 14
              Caption = 'Alt'
            end
            object entSpecificIPAddress: TCheckBox
              Left = 10
              Top = 20
              Width = 151
              Height = 17
              Caption = 'Specific Fixed IP Address'
              TabOrder = 0
            end
            object entSpecificNameServers: TCheckBox
              Left = 10
              Top = 55
              Width = 136
              Height = 17
              Caption = 'Specific Name Servers'
              TabOrder = 2
            end
            object entHeaderCompression: TCheckBox
              Left = 10
              Top = 105
              Width = 146
              Height = 17
              Caption = 'Header Compression'
              TabOrder = 7
            end
            object entRemoteDefaultGateway: TCheckBox
              Left = 185
              Top = 105
              Width = 166
              Height = 17
              Caption = 'Remote Default Gateway'
              TabOrder = 8
            end
            object entIPAddress: TMaskEdit
              Left = 185
              Top = 15
              Width = 91
              Height = 22
              EditMask = '999.999.999.999;1;_'
              MaxLength = 15
              TabOrder = 1
              Text = '   .   .   .   '
            end
            object entDNSAddress: TMaskEdit
              Left = 185
              Top = 45
              Width = 91
              Height = 22
              EditMask = '999.999.999.999;1;_'
              MaxLength = 15
              TabOrder = 3
              Text = '   .   .   .   '
            end
            object entDNSAddressAlt: TMaskEdit
              Left = 310
              Top = 45
              Width = 91
              Height = 22
              EditMask = '999.999.999.999;1;_'
              MaxLength = 15
              TabOrder = 4
              Text = '   .   .   .   '
            end
            object entWINSAddress: TMaskEdit
              Left = 185
              Top = 75
              Width = 91
              Height = 22
              EditMask = '999.999.999.999;1;_'
              MaxLength = 15
              TabOrder = 5
              Text = '   .   .   .   '
            end
            object entWINSAddressAlt: TMaskEdit
              Left = 310
              Top = 75
              Width = 91
              Height = 22
              EditMask = '999.999.999.999;1;_'
              MaxLength = 15
              TabOrder = 6
              Text = '   .   .   .   '
            end
          end
          object entSlipFrameSize: TRadioGroup
            Left = 330
            Top = 10
            Width = 101
            Height = 56
            Caption = 'SLIP Frame Size'
            ItemIndex = 0
            Items.Strings = (
              '1,006'
              '1,500')
            TabOrder = 2
          end
          object SpecialBox: TGroupBox
            Left = 10
            Top = 240
            Width = 411
            Height = 86
            Caption = 'Special Settings'
            TabOrder = 4
            object entNetworkLogon: TCheckBox
              Left = 10
              Top = 20
              Width = 231
              Height = 17
              Caption = 'Network Logon (Win9x, WAN/LAN only)'
              TabOrder = 0
            end
            object entDisableLCPExtensions: TCheckBox
              Left = 10
              Top = 40
              Width = 231
              Height = 17
              Caption = 'Disable LCP Extensions (for older system) '
              TabOrder = 1
            end
            object entSoftwareCompression: TCheckBox
              Left = 10
              Top = 60
              Width = 206
              Height = 17
              Caption = 'Software Compression (CCP)'
              TabOrder = 2
            end
            object entTerminalAfterDial: TCheckBox
              Left = 245
              Top = 39
              Width = 151
              Height = 17
              Caption = 'Terminal After Dialling'
              TabOrder = 4
            end
            object entTerminalBeforeDial: TCheckBox
              Left = 245
              Top = 20
              Width = 156
              Height = 17
              Caption = 'Terminal Before Dialling'
              TabOrder = 3
            end
            object entModemLights: TCheckBox
              Left = 245
              Top = 60
              Width = 97
              Height = 17
              Caption = 'Modem Lights'
              TabOrder = 5
            end
          end
        end
        object TabSecurity: TTabSheet
          Caption = 'Security'
          ExplicitWidth = 443
          object PasswordBox: TGroupBox
            Left = 10
            Top = 5
            Width = 416
            Height = 201
            Caption = 'Authentification and Encyption'
            TabOrder = 0
            object Label33: TLabel
              Left = 140
              Top = 115
              Width = 64
              Height = 28
              Caption = 'Custom Auth Key'
              WordWrap = True
            end
            object entRequireEncryptedPassword: TCheckBox
              Left = 10
              Top = 20
              Width = 181
              Height = 17
              Caption = 'Require Encrypted Password'
              TabOrder = 0
            end
            object entRequireMSEncryptedPassword: TCheckBox
              Left = 10
              Top = 40
              Width = 201
              Height = 17
              Caption = 'Require MS Encrypted Password'
              TabOrder = 1
            end
            object entRequireDataEncryption: TCheckBox
              Left = 10
              Top = 60
              Width = 176
              Height = 17
              Caption = 'Require Data Encryption'
              TabOrder = 2
            end
            object entUseLogonCredentials: TCheckBox
              Left = 10
              Top = 80
              Width = 156
              Height = 17
              Caption = 'Use Logon Credentials'
              TabOrder = 3
            end
            object entRequireEAP: TCheckBox
              Left = 225
              Top = 20
              Width = 141
              Height = 17
              Caption = 'Require EAP (W2K)'
              TabOrder = 6
            end
            object entRequirePAP: TCheckBox
              Left = 225
              Top = 40
              Width = 136
              Height = 17
              Caption = 'Require PAP (W2K)'
              TabOrder = 7
            end
            object entRequireSPAP: TCheckBox
              Left = 225
              Top = 60
              Width = 141
              Height = 17
              Caption = 'Require SPAP (W2K)'
              TabOrder = 8
            end
            object entRequireCHAP: TCheckBox
              Left = 225
              Top = 80
              Width = 156
              Height = 17
              Caption = 'Require CHAP (W2K)'
              TabOrder = 9
            end
            object entRequireMsCHAP: TCheckBox
              Left = 225
              Top = 100
              Width = 161
              Height = 17
              Caption = 'Require MS CHAP (W2K)'
              TabOrder = 10
            end
            object entRequireMsCHAP2: TCheckBox
              Left = 225
              Top = 120
              Width = 181
              Height = 17
              Caption = 'Require MS CHAP2 (W2K)'
              TabOrder = 11
            end
            object entRequireW95MSCHAP: TCheckBox
              Left = 225
              Top = 140
              Width = 181
              Height = 17
              Caption = 'Require W95 MS CHAP (W2K)'
              TabOrder = 12
            end
            object entCustom: TCheckBox
              Left = 225
              Top = 160
              Width = 156
              Height = 17
              Caption = 'Custom Encryption (W2K)'
              TabOrder = 13
            end
            object entEncryptionType: TRadioGroup
              Left = 10
              Top = 105
              Width = 121
              Height = 86
              Caption = 'Encryption Type (W2K)'
              ItemIndex = 3
              Items.Strings = (
                'None'
                '40 bit'
                '128 bit'
                'Optional (typical)')
              TabOrder = 4
            end
            object entCustomAuthKey: TSpinEdit
              Left = 140
              Top = 150
              Width = 66
              Height = 23
              MaxValue = 0
              MinValue = 0
              TabOrder = 5
              Value = 0
            end
          end
          object X25Box: TGroupBox
            Left = 10
            Top = 215
            Width = 416
            Height = 111
            Caption = 'X25 Packet Switching - NT/W2K only'
            TabOrder = 1
            object Label28: TLabel
              Left = 225
              Top = 55
              Width = 41
              Height = 14
              Caption = 'Facilities'
            end
            object Label29: TLabel
              Left = 10
              Top = 55
              Width = 42
              Height = 14
              Caption = 'Address'
            end
            object Label30: TLabel
              Left = 10
              Top = 80
              Width = 48
              Height = 14
              Caption = 'User Data'
            end
            object Label31: TLabel
              Left = 10
              Top = 25
              Width = 44
              Height = 14
              Caption = 'Pad Type'
            end
            object entX25PadType: TEdit
              Left = 70
              Top = 20
              Width = 191
              Height = 22
              TabOrder = 0
              Text = 'entX25PadType'
            end
            object entX25Address: TEdit
              Left = 70
              Top = 50
              Width = 141
              Height = 22
              TabOrder = 1
              Text = 'entX25Address'
            end
            object entX25UserData: TEdit
              Left = 70
              Top = 80
              Width = 331
              Height = 22
              TabOrder = 3
              Text = 'entX25UserData'
            end
            object entX25Facilities: TEdit
              Left = 280
              Top = 50
              Width = 121
              Height = 22
              TabOrder = 2
              Text = 'entX25Facilities'
            end
          end
        end
        object TabScript: TTabSheet
          Caption = 'Script'
          ExplicitWidth = 443
          object Label51: TLabel
            Left = 10
            Top = 300
            Width = 75
            Height = 14
            Caption = 'Phonebook File:'
          end
          object LabelPhonebookPathFull: TLabel
            Left = 90
            Top = 300
            Width = 351
            Height = 31
            AutoSize = False
            Caption = 'LabelPhonebookPath'
            WordWrap = True
          end
          object ScriptBox: TGroupBox
            Left = 10
            Top = 5
            Width = 416
            Height = 91
            Caption = 'Script File'
            TabOrder = 0
            object Label27: TLabel
              Left = 10
              Top = 25
              Width = 46
              Height = 14
              Caption = 'File Name'
            end
            object entScript: TEdit
              Left = 65
              Top = 20
              Width = 336
              Height = 22
              TabOrder = 0
              Text = 'entScript'
            end
            object doScriptOpen: TButton
              Left = 180
              Top = 50
              Width = 75
              Height = 25
              Caption = 'Browse'
              TabOrder = 1
              OnClick = doScriptOpenClick
            end
            object doScriptView: TButton
              Left = 275
              Top = 50
              Width = 75
              Height = 25
              Caption = 'View'
              TabOrder = 2
              OnClick = doScriptViewClick
            end
          end
          object ViewScript: TMemo
            Left = 10
            Top = 105
            Width = 416
            Height = 181
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 1
          end
        end
        object TabMultilink: TTabSheet
          Caption = 'Multilink'
          ExplicitWidth = 443
          object MultilinkBox: TGroupBox
            Left = 10
            Top = 5
            Width = 421
            Height = 211
            Caption = 'Multilink - MPPP'
            TabOrder = 0
            object Label34: TLabel
              Left = 10
              Top = 20
              Width = 98
              Height = 14
              Caption = 'Number of Channels'
            end
            object Label42: TLabel
              Left = 190
              Top = 20
              Width = 72
              Height = 14
              Caption = '(Old Channels)'
            end
            object entSubEntries: TEdit
              Left = 130
              Top = 15
              Width = 36
              Height = 22
              ReadOnly = True
              TabOrder = 0
              Text = 'entSubEntries'
            end
            object entISDNChannels: TEdit
              Left = 275
              Top = 15
              Width = 46
              Height = 22
              TabOrder = 1
              Text = 'entISDNChannels'
            end
            object MultilinkList: TListView
              Left = 5
              Top = 40
              Width = 411
              Height = 166
              Checkboxes = True
              Columns = <
                item
                  Caption = 'Device'
                  Width = 240
                end
                item
                  Caption = 'Port'
                  Width = 60
                end
                item
                  Caption = 'Local Telephone Number'
                  Width = 150
                end
                item
                  Caption = 'Type'
                  Width = 70
                end>
              ReadOnly = True
              TabOrder = 2
              ViewStyle = vsReport
            end
          end
          object BAPBox: TGroupBox
            Left = 10
            Top = 230
            Width = 421
            Height = 96
            Caption = 'Bandwidth Allocation Protocol (BAP) (W2K/XP only)'
            TabOrder = 1
            object Label35: TLabel
              Left = 275
              Top = 60
              Width = 62
              Height = 14
              Caption = 'Sample secs'
            end
            object Label36: TLabel
              Left = 105
              Top = 60
              Width = 85
              Height = 14
              Caption = 'Hang-Up, percent'
            end
            object Label37: TLabel
              Left = 275
              Top = 30
              Width = 63
              Height = 14
              Caption = 'Sample Secs'
            end
            object Label38: TLabel
              Left = 105
              Top = 30
              Width = 88
              Height = 14
              Caption = 'Dial, extra percent'
            end
            object entDialMode: TRadioGroup
              Left = 10
              Top = 20
              Width = 86
              Height = 71
              Caption = 'Dial Mode'
              ItemIndex = 0
              Items.Strings = (
                'Single'
                'All'
                'As Needed')
              TabOrder = 0
            end
            object entHangUpExtraPercent: TSpinEdit
              Left = 205
              Top = 55
              Width = 61
              Height = 23
              MaxValue = 0
              MinValue = 0
              TabOrder = 3
              Value = 0
            end
            object entDialExtraPercent: TSpinEdit
              Left = 205
              Top = 25
              Width = 61
              Height = 23
              MaxValue = 0
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
            object entDialExtraSampleSeconds: TSpinEdit
              Left = 350
              Top = 25
              Width = 61
              Height = 23
              MaxValue = 0
              MinValue = 0
              TabOrder = 2
              Value = 0
            end
            object entHangUpExtraSampleSeconds: TSpinEdit
              Left = 350
              Top = 55
              Width = 61
              Height = 23
              MaxValue = 0
              MinValue = 0
              TabOrder = 4
              Value = 0
            end
          end
        end
        object TabW2000: TTabSheet
          Caption = 'W2K'
          ExplicitWidth = 443
          object BoxExtras: TGroupBox
            Left = 10
            Top = 5
            Width = 416
            Height = 321
            Caption = ' Windows 2000/XP/Net Extras'
            TabOrder = 0
            object Label39: TLabel
              Left = 15
              Top = 290
              Width = 34
              Height = 14
              Caption = 'Guid ID'
            end
            object Label40: TLabel
              Left = 15
              Top = 265
              Width = 78
              Height = 14
              Caption = 'Custom Dial DLL'
            end
            object entSecureLocalFiles: TCheckBox
              Left = 10
              Top = 15
              Width = 136
              Height = 17
              Caption = 'Secure Local Files'
              TabOrder = 0
            end
            object entPreviewPhoneNumber: TCheckBox
              Left = 10
              Top = 35
              Width = 151
              Height = 17
              Caption = 'Preview Phone Number'
              TabOrder = 1
            end
            object entSharedPhoneNumbers: TCheckBox
              Left = 225
              Top = 75
              Width = 141
              Height = 17
              Caption = 'Shared Phone Numbers'
              TabOrder = 2
            end
            object entPreviewUserPw: TCheckBox
              Left = 225
              Top = 15
              Width = 136
              Height = 17
              Caption = 'Preview User Pw'
              TabOrder = 4
            end
            object entPreviewDomain: TCheckBox
              Left = 225
              Top = 35
              Width = 126
              Height = 17
              Caption = 'Preview Domain'
              TabOrder = 5
            end
            object entShowDialingProgress: TCheckBox
              Left = 225
              Top = 55
              Width = 161
              Height = 17
              Caption = 'Show Dialing Progress'
              TabOrder = 6
            end
            object entPType: TRadioGroup
              Left = 10
              Top = 60
              Width = 156
              Height = 131
              Caption = 'Phonebook Entry Type'
              ItemIndex = 0
              Items.Strings = (
                'Unknown'
                'Phone Modem/ISDN'
                'VNP'
                'Direct Serial/Parallel'
                'Connection Manager'
                'Broadband (XP only)')
              TabOrder = 3
            end
            object entVpnStrategy: TRadioGroup
              Left = 225
              Top = 100
              Width = 156
              Height = 126
              Caption = 'VPN Strategy'
              ItemIndex = 0
              Items.Strings = (
                'Default'
                'PptpOnly'
                'PptpFirst'
                'L2tpOnly'
                'L2tpFirst')
              TabOrder = 7
            end
            object entguidId: TEdit
              Left = 100
              Top = 290
              Width = 301
              Height = 22
              ReadOnly = True
              TabOrder = 9
            end
            object entCustomDialDll: TEdit
              Left = 100
              Top = 260
              Width = 301
              Height = 22
              ReadOnly = True
              TabOrder = 8
              Text = 'entCustomDialDll'
            end
            object entPhoneBook: TRadioGroup
              Left = 10
              Top = 195
              Width = 131
              Height = 51
              Caption = 'Phonebook Location'
              ItemIndex = 1
              Items.Strings = (
                'Current User'
                'All Users')
              TabOrder = 10
            end
          end
        end
        object TabSheet8: TTabSheet
          Caption = 'XP'
          ImageIndex = 7
          ExplicitWidth = 443
          object GroupBox6: TGroupBox
            Left = 10
            Top = 5
            Width = 426
            Height = 316
            Caption = ' Windows XP Extras'
            TabOrder = 0
            object Label52: TLabel
              Left = 10
              Top = 160
              Width = 86
              Height = 14
              Caption = 'TCP Window Size'
            end
            object Label54: TLabel
              Left = 10
              Top = 130
              Width = 53
              Height = 14
              Caption = 'DNS Suffix'
            end
            object Label55: TLabel
              Left = 10
              Top = 220
              Width = 85
              Height = 14
              Caption = 'Prerequisite Entry'
            end
            object Label56: TLabel
              Left = 10
              Top = 190
              Width = 113
              Height = 14
              Caption = 'Prerequisite Phonebook'
            end
            object Label57: TLabel
              Left = 10
              Top = 270
              Width = 104
              Height = 28
              Caption = 'Time Between Redial Attempts (secs)'
              WordWrap = True
            end
            object Label58: TLabel
              Left = 10
              Top = 250
              Width = 74
              Height = 14
              Caption = 'Redial Attempts'
            end
            object entSecureFileAndPrint: TCheckBox
              Left = 10
              Top = 15
              Width = 156
              Height = 17
              Caption = 'Don'#39't Allow File and Print'
              TabOrder = 0
            end
            object entDontNegotiateMultilink: TCheckBox
              Left = 10
              Top = 55
              Width = 211
              Height = 17
              Caption = 'Don'#39't Negotiate Multilink for Single Link'
              TabOrder = 1
            end
            object entSecureClientForMSNet: TCheckBox
              Left = 10
              Top = 35
              Width = 206
              Height = 17
              Caption = 'Don'#39't Allow Client for MS Networks'
              TabOrder = 2
            end
            object entDontUseRasCredentials: TCheckBox
              Left = 10
              Top = 75
              Width = 166
              Height = 17
              Caption = 'Don'#39't Use RAS Credentials'
              TabOrder = 3
            end
            object entUsePreSharedKey: TCheckBox
              Left = 10
              Top = 95
              Width = 196
              Height = 17
              Caption = 'Use Pre-Shared Authentication Key'
              TabOrder = 4
            end
            object entUseGlobalDeviceSettings: TCheckBox
              Left = 225
              Top = 55
              Width = 161
              Height = 17
              Caption = 'Use Global Device Settings'
              TabOrder = 5
            end
            object entDisableNbtOverIP: TCheckBox
              Left = 225
              Top = 35
              Width = 176
              Height = 17
              Caption = 'Disable NBT Probing Over IP'
              TabOrder = 6
            end
            object entInternet: TCheckBox
              Left = 225
              Top = 15
              Width = 136
              Height = 17
              Caption = 'Internet'
              TabOrder = 7
            end
            object entReconnectIfDropped: TCheckBox
              Left = 225
              Top = 75
              Width = 136
              Height = 17
              Caption = 'Redial If Line Dropped'
              TabOrder = 8
            end
            object entSharePhoneNumbers: TCheckBox
              Left = 225
              Top = 95
              Width = 191
              Height = 17
              Caption = 'Multilink Devices Use Same Number'
              TabOrder = 9
            end
            object entTcpWindowSize: TSpinEdit
              Left = 130
              Top = 155
              Width = 61
              Height = 23
              MaxValue = 0
              MinValue = 0
              TabOrder = 10
              Value = 0
            end
            object entDnsSuffix: TEdit
              Left = 130
              Top = 125
              Width = 101
              Height = 22
              TabOrder = 11
            end
            object entPrerequisitePbk: TEdit
              Left = 130
              Top = 185
              Width = 266
              Height = 22
              TabOrder = 12
            end
            object entPrerequisiteEntry: TEdit
              Left = 130
              Top = 215
              Width = 266
              Height = 22
              TabOrder = 13
            end
            object entRedialCount: TSpinEdit
              Left = 130
              Top = 245
              Width = 61
              Height = 23
              MaxValue = 0
              MinValue = 0
              TabOrder = 14
              Value = 0
            end
            object entRedialPause: TSpinEdit
              Left = 130
              Top = 275
              Width = 61
              Height = 23
              MaxValue = 0
              MinValue = 0
              TabOrder = 15
              Value = 0
            end
          end
        end
      end
      object doPropDump: TButton
        Left = 365
        Top = 375
        Width = 91
        Height = 25
        Caption = 'Dump Rasentry'
        TabOrder = 4
        OnClick = doPropDumpClick
      end
      object doPropCopy: TButton
        Left = 75
        Top = 375
        Width = 56
        Height = 25
        Caption = 'Copy'
        TabOrder = 1
        OnClick = doPropCopyClick
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Quick New'
      ExplicitLeft = 0
      object Label46: TLabel
        Left = 10
        Top = 40
        Width = 55
        Height = 14
        Caption = 'Entry Name'
      end
      object Label48: TLabel
        Left = 60
        Top = 10
        Width = 310
        Height = 14
        Caption = 'Create a New Connection/Phonebook Using Default PPP Settings'
      end
      object GroupBox3: TGroupBox
        Left = 5
        Top = 70
        Width = 451
        Height = 116
        Caption = 'Logon Details'
        TabOrder = 0
        object Label43: TLabel
          Left = 10
          Top = 20
          Width = 60
          Height = 14
          Caption = 'Logon Name'
        end
        object Label45: TLabel
          Left = 10
          Top = 50
          Width = 50
          Height = 14
          Caption = 'Password'
        end
        object Label9: TLabel
          Left = 40
          Top = 95
          Width = 386
          Height = 14
          Caption = 
            'Note: Program Needs Admin Rights to create All Users entries (se' +
            'e Network tab)'
        end
        object quickUserName: TEdit
          Left = 80
          Top = 15
          Width = 216
          Height = 22
          TabOrder = 0
          Text = 'quickUserName'
        end
        object quickPassword: TEdit
          Left = 80
          Top = 45
          Width = 216
          Height = 22
          PasswordChar = '*'
          TabOrder = 1
          Text = 'quickPassword'
        end
        object quickPhoneBook: TRadioGroup
          Left = 310
          Top = 15
          Width = 131
          Height = 51
          Caption = 'Phonebook Location'
          ItemIndex = 1
          Items.Strings = (
            'Current User'
            'All Users')
          TabOrder = 2
        end
        object quickDefaultCreds: TCheckBox
          Left = 10
          Top = 75
          Width = 316
          Height = 17
          Caption = 'Save logon for anyone who uses this computer'
          TabOrder = 3
          OnClick = entUseCountryandAreaCodesClick
        end
      end
      object quickEntryName: TEdit
        Left = 80
        Top = 35
        Width = 226
        Height = 22
        TabOrder = 1
        Text = 'quickEntryName'
      end
      object GroupBox4: TGroupBox
        Left = 5
        Top = 292
        Width = 419
        Height = 49
        Caption = 'Dial Device'
        TabOrder = 2
        object Label47: TLabel
          Left = 10
          Top = 20
          Width = 27
          Height = 14
          Caption = 'Name'
        end
        object quickDeviceName: TComboBox
          Left = 50
          Top = 15
          Width = 321
          Height = 22
          Style = csDropDownList
          ItemHeight = 14
          TabOrder = 0
        end
      end
      object GroupBox5: TGroupBox
        Tag = 1
        Left = 5
        Top = 195
        Width = 419
        Height = 91
        Caption = 'Location and Phone Number'
        TabOrder = 3
        object Label53: TLabel
          Left = 15
          Top = 20
          Width = 87
          Height = 14
          Caption = 'Canonical Number'
        end
        object qLabelNumberDisp: TLabel
          Left = 25
          Top = 45
          Width = 90
          Height = 14
          Caption = 'qLabelNumberDisp'
        end
        object qLabelNumberDial: TLabel
          Left = 25
          Top = 65
          Width = 86
          Height = 14
          Caption = 'qLabelNumberDial'
        end
        object quickCanonNumber: TEdit
          Left = 110
          Top = 15
          Width = 271
          Height = 22
          TabOrder = 0
          Text = 'quickCanonNumber'
          OnChange = quickCanonNumberChange
        end
      end
      object doQuickClear: TButton
        Left = 35
        Top = 375
        Width = 75
        Height = 25
        Caption = 'Clear'
        TabOrder = 4
        OnClick = doQuickClearClick
      end
      object doQuickCreate: TButton
        Left = 130
        Top = 375
        Width = 75
        Height = 25
        Caption = 'Create'
        TabOrder = 5
        OnClick = doQuickCreateClick
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Entry List'
      ImageIndex = 6
      object LabelEntryRes: TLabel
        Left = 225
        Top = 10
        Width = 3
        Height = 14
      end
      object doRefreshEntries: TButton
        Left = 5
        Top = 5
        Width = 75
        Height = 25
        Caption = 'Refresh List'
        TabOrder = 0
        OnClick = doRefreshEntriesClick
      end
      object EntriesList: TListView
        Left = 0
        Top = 45
        Width = 687
        Height = 497
        Columns = <
          item
            Caption = 'Name'
            Width = 100
          end
          item
            Caption = 'Number/Host'
            Width = 100
          end
          item
            Caption = 'Device 1: Name'
            Width = 120
          end
          item
            Caption = 'Port'
          end
          item
            Caption = 'Type'
            Width = 60
          end
          item
            Caption = 'Device 2: Name'
            Width = 120
          end
          item
            Caption = 'Port'
          end
          item
            Caption = 'Type'
            Width = 60
          end
          item
            Caption = 'Location'
            Width = 60
          end
          item
            Caption = 'Phonebook'
            Width = 600
          end>
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
      end
      object EntryUseAPI: TCheckBox
        Left = 105
        Top = 10
        Width = 97
        Height = 17
        Caption = 'Use RAS APIs'
        TabOrder = 2
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Network'
      ImageIndex = 7
      object GroupBox7: TGroupBox
        Left = 3
        Top = 125
        Width = 518
        Height = 101
        Caption = 'Network Information'
        TabOrder = 0
        object LabelMACAddr: TLabel
          Left = 10
          Top = 25
          Width = 67
          Height = 14
          Caption = 'MAC Address'
        end
        object LabelNetAlive: TLabel
          Left = 240
          Top = 25
          Width = 42
          Height = 14
          Caption = 'Net Alive'
        end
        object LabelQOS: TLabel
          Left = 240
          Top = 45
          Width = 42
          Height = 14
          Caption = 'Net QOS'
        end
      end
      object GroupBox8: TGroupBox
        Left = 5
        Top = 235
        Width = 516
        Height = 166
        Caption = 'Internet Options (for MSIE)'
        TabOrder = 1
        object Label59: TLabel
          Left = 10
          Top = 105
          Width = 94
          Height = 14
          Caption = 'Default Connection '
        end
        object optMSIEDefConn: TComboBox
          Left = 115
          Top = 100
          Width = 306
          Height = 22
          ItemHeight = 14
          TabOrder = 0
          Text = 'optMSIEDefConn'
          OnDropDown = optMSIEDefConnDropDown
        end
        object doMSIEUpdate: TButton
          Left = 175
          Top = 130
          Width = 75
          Height = 25
          Caption = 'Update'
          TabOrder = 1
          OnClick = doMSIEUpdateClick
        end
        object optMSIEAutDial: TRadioGroup
          Left = 10
          Top = 20
          Width = 316
          Height = 71
          Caption = 'Auto Dial Option'
          ItemIndex = 0
          Items.Strings = (
            'Never dial a connection'
            'Dial whenever a network connection is not present'
            'Always dial my default connection')
          TabOrder = 2
        end
      end
      object GroupBox9: TGroupBox
        Left = 3
        Top = 3
        Width = 518
        Height = 116
        Caption = 'Windows Information'
        TabOrder = 2
        object LabelRASVer: TLabel
          Left = 10
          Top = 35
          Width = 71
          Height = 14
          Caption = 'LabelRASVer'
          Font.Charset = ANSI_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelWinVer: TLabel
          Left = 10
          Top = 15
          Width = 69
          Height = 14
          Caption = 'LabelWinVer'
          Font.Charset = ANSI_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelVersion: TLabel
          Left = 10
          Top = 75
          Width = 496
          Height = 31
          AutoSize = False
          Caption = 'Version'#13#10'More version'
          Font.Charset = ANSI_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object LabelAdmin: TLabel
          Left = 10
          Top = 55
          Width = 66
          Height = 14
          Caption = 'LabelAdmin'
          Font.Charset = ANSI_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
    end
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 27
    Top = 120
  end
  object SaveDump: TSaveDialog
    Title = 'Save RASENTRY Structure as Binary File'
    Left = 54
    Top = 54
  end
  object OpenScript: TOpenDialog
    DefaultExt = 'scr'
    Filter = 'Dial-Up Script (*.SCP)|*.SCP'
    Title = 'Select Dial-Up Script Fille'
    Left = 83
    Top = 123
  end
end
