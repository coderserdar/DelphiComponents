object SetPwdForm: TSetPwdForm
  Left = 262
  Top = 220
  BorderStyle = bsDialog
  Caption = 'Input Password'
  ClientHeight = 184
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 9
    Top = 65
    Width = 274
    Height = 77
    TabOrder = 0
    object PromptLabel: TLabel
      Left = 13
      Top = 22
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object Label1: TLabel
      Left = 13
      Top = 48
      Width = 57
      Height = 13
      Caption = 'Type again:'
    end
    object PasswordEdit: TEdit
      Left = 90
      Top = 18
      Width = 172
      Height = 21
      PasswordChar = '*'
      TabOrder = 0
    end
    object Password2Edit: TEdit
      Left = 90
      Top = 44
      Width = 172
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
  end
  object OkButton: TButton
    Left = 117
    Top = 150
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 208
    Top = 150
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
  object Panel1: TPanel
    Left = 9
    Top = 9
    Width = 274
    Height = 51
    BevelOuter = bvLowered
    TabOrder = 3
    object Image1: TImage
      Left = 8
      Top = 4
      Width = 16
      Height = 16
      AutoSize = True
      Picture.Data = {
        07544269746D6170F6000000424DF60000000000000076000000280000001000
        0000100000000100040000000000800000000000000000000000100000000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF0077777770777777777777770007777777777777070777777777B77B000B77
        B777777BBB0B0BBB7777777BBB0F0BBB777777BBB0F8F0BBB77777BB0FB8BF0B
        B777BBBB0BF8FB0BBBB777BB0FBFBF0BB77777BBB0FBF0BBB777777BBB000BBB
        7777777BBBBBBBBB777777B77BBBBB77B7777777777B777777777777777B7777
        7777}
    end
    object TipLabel: TLabel
      Left = 36
      Top = 6
      Width = 233
      Height = 44
      AutoSize = False
      Caption = '#'
      WordWrap = True
    end
  end
end
