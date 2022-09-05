object ConfigForm: TConfigForm
  Left = 213
  Top = 130
  BorderStyle = bsDialog
  Caption = 'Config'
  ClientHeight = 449
  ClientWidth = 454
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 17
    Top = 9
    Width = 54
    Height = 13
    Caption = 'Provider list'
  end
  object Label2: TLabel
    Left = 17
    Top = 182
    Width = 52
    Height = 13
    Caption = 'Session list'
  end
  object Label3: TLabel
    Left = 104
    Top = 347
    Width = 66
    Height = 13
    Caption = 'Session name'
  end
  object Label4: TLabel
    Left = 104
    Top = 381
    Width = 58
    Height = 13
    Caption = 'Player name'
  end
  object ProviderList: TComboBox
    Left = 95
    Top = 9
    Width = 348
    Height = 20
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ItemHeight = 12
    ParentFont = False
    TabOrder = 0
    OnChange = ProviderListChange
  end
  object ProviderSetting: TNotebook
    Left = 95
    Top = 43
    Width = 348
    Height = 131
    TabOrder = 1
    object TPage
      Left = 0
      Top = 0
      Caption = 'Default'
      object OtherConnectButton: TButton
        Left = 251
        Top = 95
        Width = 82
        Height = 27
        Caption = 'Connect'
        TabOrder = 0
        OnClick = OtherConnectButtonClick
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'ProviderSettingTCPIP'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label5: TLabel
        Left = 9
        Top = 12
        Width = 51
        Height = 13
        Caption = 'Host name'
      end
      object Label8: TLabel
        Left = 9
        Top = 46
        Width = 47
        Height = 13
        Caption = 'Fixed Port'
      end
      object TCPIPHostName: TEdit
        Left = 87
        Top = 9
        Width = 243
        Height = 20
        TabOrder = 0
      end
      object TCPIPConnectButton: TButton
        Left = 251
        Top = 95
        Width = 82
        Height = 27
        Caption = 'Connect'
        TabOrder = 1
        OnClick = TCPIPConnectButtonClick
      end
      object TCPIPPort: TSpinEdit
        Left = 87
        Top = 40
        Width = 87
        Height = 21
        MaxValue = 65535
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'ProviderSettingModem'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label6: TLabel
        Left = 9
        Top = 9
        Width = 69
        Height = 13
        Caption = 'Phone number'
      end
      object Label7: TLabel
        Left = 9
        Top = 52
        Width = 35
        Height = 13
        Caption = 'Modem'
      end
      object ModemPhoneNumber: TEdit
        Left = 95
        Top = 9
        Width = 235
        Height = 20
        TabOrder = 0
      end
      object ModemComboBox: TComboBox
        Left = 95
        Top = 52
        Width = 235
        Height = 20
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
      end
      object ModemConnectButton: TButton
        Left = 251
        Top = 95
        Width = 82
        Height = 27
        Caption = 'Connect'
        TabOrder = 2
        OnClick = ModemConnectButtonClick
      end
    end
  end
  object SessionListBox: TListBox
    Left = 95
    Top = 182
    Width = 348
    Height = 148
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ItemHeight = 12
    ParentFont = False
    TabOrder = 2
    OnClick = SessionListBoxClick
  end
  object JoinButton: TButton
    Left = 277
    Top = 416
    Width = 82
    Height = 27
    Caption = 'Join'
    TabOrder = 3
    OnClick = JoinButtonClick
  end
  object HostButton: TButton
    Left = 364
    Top = 416
    Width = 81
    Height = 27
    Caption = 'Host'
    TabOrder = 4
    OnClick = HostButtonClick
  end
  object SessionNameEdit: TEdit
    Left = 191
    Top = 347
    Width = 252
    Height = 21
    TabOrder = 5
  end
  object PlayerNameEdit: TEdit
    Left = 191
    Top = 381
    Width = 252
    Height = 21
    TabOrder = 6
  end
end
