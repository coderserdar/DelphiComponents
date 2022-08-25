object Form1: TForm1
  Left = 193
  Top = 124
  Caption = 'Form1'
  ClientHeight = 297
  ClientWidth = 487
  Color = clBtnFace
  Constraints.MinHeight = 324
  Constraints.MinWidth = 495
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    487
    297)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 132
    Width = 75
    Height = 13
    Caption = 'Number of mails'
  end
  object Label2: TLabel
    Left = 192
    Top = 132
    Width = 138
    Height = 13
    Caption = 'Max. concurrent connections'
  end
  object Label3: TLabel
    Left = 63
    Top = 10
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label4: TLabel
    Left = 62
    Top = 34
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label5: TLabel
    Left = 72
    Top = 58
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object Label6: TLabel
    Left = 49
    Top = 82
    Width = 36
    Height = 13
    Caption = 'Subject'
  end
  object Label7: TLabel
    Left = 38
    Top = 106
    Width = 47
    Height = 13
    Caption = 'Msg. Text'
  end
  object Label8: TLabel
    Left = 242
    Top = 10
    Width = 22
    Height = 13
    Caption = 'User'
  end
  object Label9: TLabel
    Left = 218
    Top = 34
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Label10: TLabel
    Left = 402
    Top = 110
    Width = 38
    Height = 13
    Caption = 'Label10'
  end
  object FillQueueButton: TButton
    Left = 402
    Top = 127
    Width = 75
    Height = 21
    Caption = 'Fill Queue'
    TabOrder = 14
    OnClick = FillQueueButtonClick
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 158
    Width = 487
    Height = 139
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 15
  end
  object StartSendbutton: TButton
    Left = 402
    Top = 77
    Width = 75
    Height = 21
    Caption = 'Start Send'
    Enabled = False
    TabOrder = 13
    OnClick = StartSendbuttonClick
  end
  object NumOfMailsEdit: TEdit
    Left = 88
    Top = 128
    Width = 51
    Height = 21
    TabOrder = 5
    Text = 'NumOfMailsEdit'
  end
  object MaxConEdit: TEdit
    Left = 332
    Top = 128
    Width = 57
    Height = 21
    TabOrder = 10
    Text = 'MaxConEdit'
  end
  object HostEdit: TEdit
    Left = 88
    Top = 6
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'HostEdit'
  end
  object FromEdit: TEdit
    Left = 88
    Top = 30
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'FromEdit'
  end
  object ToEdit: TEdit
    Left = 88
    Top = 54
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'ToEdit'
  end
  object SubjectEdit: TEdit
    Left = 88
    Top = 78
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'SubjectEdit'
  end
  object MessageEdit: TEdit
    Left = 88
    Top = 102
    Width = 301
    Height = 21
    TabOrder = 4
    Text = 'MessageEdit'
  end
  object UserEdit: TEdit
    Left = 268
    Top = 6
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'UserEdit'
  end
  object PasswordEdit: TEdit
    Left = 268
    Top = 30
    Width = 121
    Height = 21
    TabOrder = 7
    Text = 'PasswordEdit'
  end
  object CheckBoxAuth: TCheckBox
    Left = 268
    Top = 56
    Width = 81
    Height = 17
    Caption = 'Authenticate'
    TabOrder = 8
  end
  object CheckBoxDisplay: TCheckBox
    Left = 268
    Top = 80
    Width = 115
    Height = 17
    Caption = 'Display Log (slow)'
    Checked = True
    State = cbChecked
    TabOrder = 9
  end
  object ClearMemoButton: TButton
    Left = 402
    Top = 6
    Width = 75
    Height = 21
    Caption = 'Clear Memo'
    TabOrder = 11
    OnClick = ClearMemoButtonClick
  end
  object AbortButton: TButton
    Left = 402
    Top = 52
    Width = 75
    Height = 21
    Caption = 'Abort'
    TabOrder = 12
    OnClick = AbortButtonClick
  end
end
