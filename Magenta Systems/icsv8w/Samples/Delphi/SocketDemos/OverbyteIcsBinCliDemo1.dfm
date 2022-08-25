object BinClientForm: TBinClientForm
  Left = 769
  Top = 326
  Caption = 'Binary Client Demo'
  ClientHeight = 247
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 73
    Width = 452
    Height = 174
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 452
    Height = 73
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label2: TLabel
      Left = 104
      Top = 12
      Width = 31
      Height = 13
      Caption = 'Server'
    end
    object Label3: TLabel
      Left = 6
      Top = 43
      Width = 23
      Height = 13
      Caption = 'Data'
    end
    object SendEdit: TEdit
      Left = 32
      Top = 40
      Width = 185
      Height = 21
      TabOrder = 0
      Text = 'SendEdit'
    end
    object SendButton: TButton
      Left = 230
      Top = 40
      Width = 75
      Height = 21
      Caption = '&Send'
      Default = True
      TabOrder = 1
      OnClick = SendButtonClick
    end
    object DisconnectButton: TButton
      Left = 214
      Top = 8
      Width = 75
      Height = 21
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 2
      OnClick = DisconnectButtonClick
    end
    object PortEdit: TEdit
      Left = 32
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 3
      Text = 'telnet'
    end
    object ServerEdit: TEdit
      Left = 144
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 4
      Text = 'localhost'
    end
    object AddCRLFCheckBox: TCheckBox
      Left = 320
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Add CR/LF'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
  end
end
