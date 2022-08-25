object DllTestForm: TDllTestForm
  Left = 20
  Top = 141
  Caption = 'DllTest - http://www.rtfm.be/fpiette/indexuk.htm'
  ClientHeight = 231
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 388
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 48
      Height = 13
      Caption = 'Hostname'
    end
    object Label2: TLabel
      Left = 152
      Top = 12
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object CallDllButton: TButton
      Left = 240
      Top = 8
      Width = 75
      Height = 21
      Caption = 'CallDll'
      Default = True
      TabOrder = 0
      OnClick = CallDllButtonClick
    end
    object HostnameEdit: TEdit
      Left = 64
      Top = 8
      Width = 73
      Height = 21
      TabOrder = 1
      Text = 'localhost'
    end
    object PortEdit: TEdit
      Left = 184
      Top = 8
      Width = 45
      Height = 21
      TabOrder = 2
      Text = 'telnet'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 388
    Height = 190
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
