object frmOptions: TfrmOptions
  Left = 570
  Top = 240
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Options'
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
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 186
    Width = 258
    Height = 41
    Align = alBottom
    TabOrder = 0
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
  object pcParams: TPageControl
    Left = 0
    Top = 0
    Width = 258
    Height = 186
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 1
    object TabSheet2: TTabSheet
      Caption = 'Server'
      object Label2: TLabel
        Left = 12
        Top = 10
        Width = 142
        Height = 13
        Caption = 'Screen Update Period (MSec)'
      end
      object edPeriod: TSpinEdit
        Left = 14
        Top = 26
        Width = 81
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 1000
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Proxy'
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
        ItemHeight = 13
        TabOrder = 4
      end
    end
  end
end
