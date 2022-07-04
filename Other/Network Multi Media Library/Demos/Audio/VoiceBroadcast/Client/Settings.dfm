object frmSettings: TfrmSettings
  Left = 570
  Top = 240
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 227
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcParams: TPageControl
    Left = 0
    Top = 0
    Width = 258
    Height = 186
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'Server info'
      object Label5: TLabel
        Left = 11
        Top = 10
        Width = 54
        Height = 13
        Alignment = taRightJustify
        Caption = 'Server host'
      end
      object Label8: TLabel
        Left = 144
        Top = 10
        Width = 52
        Height = 13
        Alignment = taRightJustify
        Caption = 'Server port'
      end
      object edHost: TEdit
        Left = 10
        Top = 28
        Width = 116
        Height = 21
        TabOrder = 0
        Text = 'localhost'
      end
      object sedPort: TSpinEdit
        Left = 139
        Top = 28
        Width = 78
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'User info'
      ImageIndex = 1
      object Label1: TLabel
        Left = 13
        Top = 11
        Width = 48
        Height = 13
        Alignment = taRightJustify
        Caption = 'Username'
      end
      object Label6: TLabel
        Left = 15
        Top = 65
        Width = 46
        Height = 13
        Alignment = taRightJustify
        Caption = 'Password'
      end
      object edUser: TEdit
        Left = 11
        Top = 29
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Test'
      end
      object edPassword: TEdit
        Left = 11
        Top = 84
        Width = 121
        Height = 21
        Color = cl3DLight
        Enabled = False
        ReadOnly = True
        TabOrder = 1
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Proxy info'
      ImageIndex = 2
      object Label9: TLabel
        Left = 8
        Top = 43
        Width = 49
        Height = 13
        Alignment = taRightJustify
        Caption = 'Proxy host'
      end
      object Label10: TLabel
        Left = 8
        Top = 70
        Width = 47
        Height = 13
        Alignment = taRightJustify
        Caption = 'Proxy port'
      end
      object Label3: TLabel
        Left = 7
        Top = 102
        Width = 78
        Height = 13
        Alignment = taRightJustify
        Caption = 'Proxy user name'
      end
      object Label4: TLabel
        Left = 7
        Top = 129
        Width = 74
        Height = 13
        Alignment = taRightJustify
        Caption = 'Proxy password'
      end
      object Label7: TLabel
        Left = 6
        Top = 7
        Width = 61
        Height = 26
        Caption = 'Socks proxy version'
        WordWrap = True
      end
      object edProxyHost: TEdit
        Left = 90
        Top = 40
        Width = 109
        Height = 21
        TabOrder = 0
      end
      object sedProxyPort: TSpinEdit
        Left = 90
        Top = 68
        Width = 109
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object edProxyUserName: TEdit
        Left = 89
        Top = 99
        Width = 110
        Height = 21
        TabOrder = 2
      end
      object edProxyPassword: TEdit
        Left = 89
        Top = 126
        Width = 110
        Height = 21
        PasswordChar = '*'
        TabOrder = 3
      end
      object cbSocksProxyVersion: TComboBox
        Left = 90
        Top = 9
        Width = 110
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 4
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Audio Settings'
      ImageIndex = 3
      inline SelectAudioInput: TSelectAudioInput
        Left = 2
        Top = 3
        Width = 241
        Height = 148
        TabOrder = 0
        inherited Panel12: TPanel
          Width = 241
          inherited Label2: TLabel
            Left = 153
          end
          inherited Label3: TLabel
            Left = 153
          end
          inherited Label4: TLabel
            Left = 153
          end
          inherited lbWaveDevices: TListBox
            Width = 139
          end
          inherited cbBits: TComboBox
            Left = 152
          end
          inherited cbFrequency: TComboBox
            Left = 152
          end
          inherited cbChannels: TComboBox
            Left = 152
          end
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 186
    Width = 258
    Height = 41
    Align = alBottom
    TabOrder = 1
    object bOK: TBitBtn
      Left = 48
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = bOKClick
      Kind = bkOK
    end
    object bCancel: TBitBtn
      Left = 144
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 1
      OnClick = bCancelClick
      Kind = bkCancel
    end
  end
end
