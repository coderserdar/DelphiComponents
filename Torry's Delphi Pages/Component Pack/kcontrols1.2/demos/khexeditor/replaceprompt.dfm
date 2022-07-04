object ReplacePromptForm: TReplacePromptForm
  Left = 510
  Top = 422
  BorderStyle = bsDialog
  Caption = 'Confirm'
  ClientHeight = 90
  ClientWidth = 365
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
  object LBText: TLabel
    Left = 16
    Top = 16
    Width = 7
    Height = 13
    Caption = 'A'
  end
  object BUYes: TButton
    Left = 16
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Yes'
    ModalResult = 6
    TabOrder = 0
  end
  object BuNo: TButton
    Left = 104
    Top = 48
    Width = 75
    Height = 25
    Caption = 'No'
    ModalResult = 7
    TabOrder = 1
  end
  object BUAll: TButton
    Left = 192
    Top = 48
    Width = 75
    Height = 25
    Caption = 'All'
    ModalResult = 10
    TabOrder = 2
  end
  object BUCancel: TButton
    Left = 280
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
