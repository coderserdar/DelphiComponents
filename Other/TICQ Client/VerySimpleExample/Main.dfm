object Form1: TForm1
  Left = 257
  Top = 289
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'TICQClient Example'
  ClientHeight = 330
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 289
    Height = 129
    Caption = 'User'#39's UIN && Password'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 47
      Height = 13
      Caption = 'Your UIN:'
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 73
      Height = 13
      Caption = 'Your password:'
    end
    object PasswordEdit: TEdit
      Left = 96
      Top = 40
      Width = 177
      Height = 21
      TabOrder = 1
    end
    object UINEdit: TEdit
      Left = 96
      Top = 16
      Width = 177
      Height = 21
      TabOrder = 0
    end
    object LoginBtn: TButton
      Left = 192
      Top = 96
      Width = 81
      Height = 25
      Caption = 'Login!'
      TabOrder = 2
      OnClick = LoginBtnClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 136
    Width = 289
    Height = 193
    Caption = 'Send a message'
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 32
      Width = 83
      Height = 13
      Caption = 'Your friend'#39's UIN:'
    end
    object DestUINEdit: TEdit
      Left = 96
      Top = 24
      Width = 177
      Height = 21
      TabOrder = 0
    end
    object MessageMemo: TMemo
      Left = 8
      Top = 48
      Width = 265
      Height = 97
      Lines.Strings = (
        'Yeah, it'#39's a test message :)')
      TabOrder = 1
    end
    object SendMsgBtn: TButton
      Left = 192
      Top = 160
      Width = 83
      Height = 25
      Caption = 'Send message!'
      TabOrder = 2
      OnClick = SendMsgBtnClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 296
    Top = 0
    Width = 281
    Height = 329
    Caption = 'Events && Received Messages'
    TabOrder = 2
    object EventMemo: TMemo
      Left = 2
      Top = 15
      Width = 277
      Height = 312
      Align = alClient
      TabOrder = 0
    end
  end
  object ICQClient1: TICQClient
    ProxyPort = 0
    UIN = 0
    ICQServer = 'login.icq.com'
    ICQPort = 0
    ConvertToPlaintext = False
    OnLogin = ICQClient1Login
    OnMessageRecv = ICQClient1MessageRecv
    OnConnectionFailed = ICQClient1ConnectionFailed
    OnError = ICQClient1Error
    ConnectionTimeout = 0
    Left = 8
    Top = 296
  end
end
