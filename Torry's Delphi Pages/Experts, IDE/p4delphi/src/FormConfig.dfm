object FrmConfig: TFrmConfig
  Left = 376
  Top = 217
  BorderStyle = bsDialog
  Caption = 'Create Config File'
  ClientHeight = 137
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LblPort: TLabel
    Left = 8
    Top = 12
    Width = 20
    Height = 13
    Caption = 'Port'
    FocusControl = EdtPort
  end
  object LblClient: TLabel
    Left = 8
    Top = 44
    Width = 27
    Height = 13
    Caption = 'Client'
    FocusControl = EdtClient
  end
  object LblUser: TLabel
    Left = 8
    Top = 76
    Width = 22
    Height = 13
    Caption = 'User'
    FocusControl = EdtUser
  end
  object BtnOK: TButton
    Left = 131
    Top = 104
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BtnCancel: TButton
    Left = 214
    Top = 104
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object EdtPort: TEdit
    Left = 48
    Top = 8
    Width = 241
    Height = 21
    TabOrder = 2
  end
  object EdtClient: TEdit
    Left = 48
    Top = 40
    Width = 241
    Height = 21
    TabOrder = 3
  end
  object EdtUser: TEdit
    Left = 48
    Top = 72
    Width = 241
    Height = 21
    TabOrder = 4
  end
end
