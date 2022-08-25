object DynCliForm: TDynCliForm
  Left = 113
  Top = 123
  Caption = 'DynCliForm'
  ClientHeight = 237
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 101
    Width = 321
    Height = 136
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'This sample programs shows how to'
      'dynamically create a TWSocket.'
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 101
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 12
      Top = 20
      Width = 48
      Height = 13
      Caption = 'Hostname'
    end
    object Label2: TLabel
      Left = 16
      Top = 44
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object HostnameEdit: TEdit
      Left = 72
      Top = 12
      Width = 173
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object PortEdit: TEdit
      Left = 72
      Top = 40
      Width = 173
      Height = 21
      TabOrder = 1
      Text = 'telnet'
    end
    object ConnectButton: TButton
      Left = 72
      Top = 68
      Width = 75
      Height = 25
      Caption = '&Connect'
      Default = True
      TabOrder = 2
      OnClick = ConnectButtonClick
    end
    object DisconnectButton: TButton
      Left = 152
      Top = 68
      Width = 75
      Height = 25
      Caption = '&Disconnect'
      TabOrder = 3
      OnClick = DisconnectButtonClick
    end
  end
end
