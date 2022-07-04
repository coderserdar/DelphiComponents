object Form1: TForm1
  Left = 192
  Top = 107
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 195
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 32
    Width = 145
    Height = 17
    AutoSize = False
    TabOrder = 0
    Text = 'Mailbox'
  end
  object Edit2: TEdit
    Left = 8
    Top = 56
    Width = 145
    Height = 17
    AutoSize = False
    TabOrder = 1
    Text = 'Messageheader'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 80
    Width = 145
    Height = 17
    Caption = 'Active'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object Edit3: TEdit
    Left = 216
    Top = 8
    Width = 145
    Height = 17
    AutoSize = False
    TabOrder = 3
    Text = 'Recipient'
  end
  object Button1: TButton
    Left = 368
    Top = 16
    Width = 41
    Height = 33
    Caption = 'Send'
    Enabled = False
    TabOrder = 4
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 0
    Top = 104
    Width = 414
    Height = 91
    Align = alBottom
    ItemHeight = 13
    TabOrder = 5
  end
  object Edit4: TEdit
    Left = 8
    Top = 8
    Width = 145
    Height = 21
    Enabled = False
    TabOrder = 6
    Text = 'Edit4'
  end
  object Edit5: TEdit
    Left = 216
    Top = 32
    Width = 145
    Height = 17
    AutoSize = False
    TabOrder = 7
    Text = 'Mailbox'
  end
  object Edit6: TEdit
    Left = 216
    Top = 56
    Width = 145
    Height = 17
    AutoSize = False
    TabOrder = 8
    Text = 'Message'
  end
  object tAPI_mailslot1: TAPI_mailslot
    Nick = 'unknown'
    Header = 'Message'
    Mailbox = 'MailBox'
    Active = False
    Left = 160
    Top = 32
  end
end
