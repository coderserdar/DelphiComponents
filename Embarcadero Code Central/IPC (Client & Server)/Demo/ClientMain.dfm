object frmClient: TfrmClient
  Left = 208
  Top = 164
  Width = 476
  Height = 349
  Caption = 'IPC Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = 'User Name:'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object lblStatus: TLabel
    Left = 280
    Top = 80
    Width = 129
    Height = 24
    Caption = 'Disconnected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 256
    Top = 8
    Width = 71
    Height = 13
    Caption = 'Session Name:'
  end
  object btnSend: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 0
    OnClick = btnSendClick
  end
  object edtUserName: TEdit
    Left = 8
    Top = 24
    Width = 217
    Height = 21
    TabOrder = 1
  end
  object edtPassword: TEdit
    Left = 8
    Top = 72
    Width = 217
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object edtSessionName: TEdit
    Left = 256
    Top = 24
    Width = 209
    Height = 21
    TabOrder = 3
  end
  object btnOpen: TButton
    Left = 264
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 4
    OnClick = btnOpenClick
  end
  object btnClose: TButton
    Left = 376
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 5
    OnClick = btnCloseClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 128
    Width = 241
    Height = 185
    Caption = 'Data From Server'
    TabOrder = 6
    object Label3: TLabel
      Left = 16
      Top = 16
      Width = 56
      Height = 13
      Caption = 'User Name:'
    end
    object Label4: TLabel
      Left = 16
      Top = 56
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label5: TLabel
      Left = 16
      Top = 96
      Width = 67
      Height = 13
      Caption = 'Some Number'
    end
    object Label7: TLabel
      Left = 16
      Top = 136
      Width = 55
      Height = 13
      Caption = 'Some Real:'
    end
    object edtServerUserName: TEdit
      Left = 16
      Top = 28
      Width = 201
      Height = 21
      TabOrder = 0
    end
    object edtServerPassword: TEdit
      Left = 16
      Top = 68
      Width = 201
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object edtSomeNumber: TEdit
      Left = 16
      Top = 112
      Width = 121
      Height = 21
      TabOrder = 2
    end
    object edtSomeReal: TEdit
      Left = 16
      Top = 152
      Width = 121
      Height = 21
      TabOrder = 3
    end
  end
  object IPCClient: TIPCClient
    OnData = IPCClientData
    OnConnect = IPCClientConnect
    OnDisconnect = IPCClientDisconnect
    Left = 344
    Top = 48
  end
end
