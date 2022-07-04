object frmUsers: TfrmUsers
  Left = 286
  Top = 189
  Width = 322
  Height = 249
  Caption = 'Users'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 168
    Width = 314
    Height = 54
    Align = alBottom
    TabOrder = 0
    object Label1: TLabel
      Left = 11
      Top = 7
      Width = 139
      Height = 13
      Caption = 'Message to the selected user'
    end
    object btnSend: TButton
      Left = 256
      Top = 20
      Width = 49
      Height = 25
      Caption = 'Send'
      TabOrder = 0
    end
    object edMessage: TEdit
      Left = 8
      Top = 22
      Width = 241
      Height = 21
      TabOrder = 1
    end
  end
  object lbUsers: TListBox
    Left = 0
    Top = 0
    Width = 314
    Height = 168
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
end
