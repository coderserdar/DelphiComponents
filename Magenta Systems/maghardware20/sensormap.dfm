object FormMap: TFormMap
  Left = 0
  Top = 0
  VertScrollBar.Visible = False
  Caption = 'Map'
  ClientHeight = 654
  ClientWidth = 815
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 585
    Width = 815
    Height = 69
    Align = alBottom
    TabOrder = 0
    object LabelUpdate: TLabel
      Left = 10
      Top = 18
      Width = 83
      Height = 13
      Caption = 'Last Plot Update:'
    end
    object doClose: TButton
      Left = 705
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = doCloseClick
    end
    object EditAddr: TEdit
      Left = 110
      Top = 45
      Width = 356
      Height = 21
      TabOrder = 1
      Text = 'London W1A 1AA'
    end
    object doAddress: TButton
      Left = 0
      Top = 41
      Width = 91
      Height = 25
      Caption = 'Find Address'
      TabOrder = 2
      OnClick = doAddressClick
    end
    object CheckRealTime: TCheckBox
      Left = 265
      Top = 15
      Width = 131
      Height = 17
      Caption = 'Locate in Real Time'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckRealTimeClick
    end
    object doClear: TButton
      Left = 567
      Top = 11
      Width = 91
      Height = 25
      Caption = 'Clear Markers'
      TabOrder = 4
      OnClick = doClearClick
    end
    object CheckRoute: TCheckBox
      Left = 402
      Top = 15
      Width = 97
      Height = 17
      Caption = 'Plot Route'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
  end
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 0
    Width = 815
    Height = 585
    Align = alClient
    TabOrder = 1
    OnCommandStateChange = WebBrowserCommandStateChange
    ExplicitLeft = 385
    ExplicitTop = 330
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C0000003C540000763C00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E12620D000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
