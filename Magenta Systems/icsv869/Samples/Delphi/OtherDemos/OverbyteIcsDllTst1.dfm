object DllTestForm: TDllTestForm
  Left = 138
  Top = 119
  Caption = 'DllTest - http://www.overbyte.be'
  ClientHeight = 213
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 371
    Height = 57
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
    object Label3: TLabel
      Left = 44
      Top = 36
      Width = 13
      Height = 13
      Caption = 'Url'
    end
    object HostnameEdit: TEdit
      Left = 64
      Top = 8
      Width = 73
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object PortEdit: TEdit
      Left = 184
      Top = 8
      Width = 45
      Height = 21
      TabOrder = 1
      Text = 'telnet'
    end
    object CallDll1Button: TButton
      Left = 236
      Top = 8
      Width = 75
      Height = 21
      Caption = 'Call DLL1'
      Default = True
      TabOrder = 2
      OnClick = CallDll1ButtonClick
    end
    object CallDll2Button: TButton
      Left = 236
      Top = 32
      Width = 75
      Height = 21
      Caption = 'Call DLL2'
      TabOrder = 3
      OnClick = CallDll2ButtonClick
    end
    object UrlEdit: TEdit
      Left = 64
      Top = 32
      Width = 165
      Height = 21
      TabOrder = 4
      Text = 'http://www.overbyte.be'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 57
    Width = 371
    Height = 156
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
