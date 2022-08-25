object Form1: TForm1
  Left = 53
  Top = 89
  Caption = 
    'OverByte ICS - One Time Password Testing - http://www.overbyte.b' +
    'e'
  ClientHeight = 398
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 10
    Top = 15
    Width = 35
    Height = 14
    Caption = 'Method'
  end
  object LabelSeed: TLabel
    Left = 370
    Top = 50
    Width = 34
    Height = 16
    Caption = 'Seed:'
    Color = clBtnHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LabelSequence: TLabel
    Left = 240
    Top = 50
    Width = 62
    Height = 16
    Caption = 'Sequence:'
    Color = clBtnHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LabelResposnse: TLabel
    Left = 240
    Top = 115
    Width = 25
    Height = 16
    Caption = 'Otp:'
    Color = clBtnHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LabelChecked: TLabel
    Left = 240
    Top = 140
    Width = 55
    Height = 16
    Caption = 'Checked:'
    Color = clBtnHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 178
    Top = 15
    Width = 76
    Height = 14
    Caption = 'User Password'
  end
  object Log: TMemo
    Left = 0
    Top = 190
    Width = 554
    Height = 208
    Align = alBottom
    TabOrder = 0
  end
  object doNewChallenge: TButton
    Left = 16
    Top = 47
    Width = 85
    Height = 25
    Caption = 'New Challenge'
    TabOrder = 1
    OnClick = doNewChallengeClick
  end
  object PassMethod: TComboBox
    Left = 60
    Top = 10
    Width = 106
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    ItemIndex = 1
    TabOrder = 2
    Text = 'Otp MD5'
    Items.Strings = (
      'No Encryption'
      'Otp MD5'
      'Otp MD4'
      'Otp SHA1')
  end
  object doNextChallenge: TButton
    Left = 116
    Top = 47
    Width = 85
    Height = 25
    Caption = 'Next Challenge'
    TabOrder = 3
    OnClick = doNextChallengeClick
  end
  object doPassResp: TButton
    Left = 16
    Top = 82
    Width = 150
    Height = 25
    Caption = 'Create One Time Password'
    TabOrder = 4
    OnClick = doPassRespClick
  end
  object HexPassword: TCheckBox
    Left = 180
    Top = 85
    Width = 51
    Height = 17
    Caption = 'Hex'
    TabOrder = 5
  end
  object ChallengeResponse: TEdit
    Left = 235
    Top = 80
    Width = 301
    Height = 22
    TabOrder = 6
  end
  object UserPassword: TEdit
    Left = 280
    Top = 10
    Width = 256
    Height = 22
    TabOrder = 7
    Text = 'A_Valid_Pass_Phrase'
  end
  object doAutoTest: TButton
    Left = 15
    Top = 120
    Width = 86
    Height = 25
    Caption = 'Next Auto Test'
    TabOrder = 8
    OnClick = doAutoTestClick
  end
  object doExit: TButton
    Left = 15
    Top = 155
    Width = 51
    Height = 25
    Caption = 'Exit'
    TabOrder = 9
    OnClick = doExitClick
  end
  object doCheckOtp: TButton
    Left = 115
    Top = 120
    Width = 81
    Height = 25
    Caption = 'Check OTP'
    TabOrder = 10
    OnClick = doCheckOtpClick
  end
end
