object FormViewEdit: TFormViewEdit
  Left = 286
  Top = 327
  ActiveControl = edString
  BorderStyle = bsDialog
  Caption = 'Edit'
  ClientHeight = 87
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object labCaption: TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 13
    Caption = 'Caption:'
    FocusControl = edString
  end
  object btnOK: TButton
    Left = 84
    Top = 56
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 172
    Top = 56
    Width = 81
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object edString: TTntEdit
    Left = 8
    Top = 24
    Width = 321
    Height = 21
    TabOrder = 0
  end
end
