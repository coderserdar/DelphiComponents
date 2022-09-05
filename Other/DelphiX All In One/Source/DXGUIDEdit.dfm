object DelphiXGUIDEditForm: TDelphiXGUIDEditForm
  Left = 207
  Top = 115
  BorderStyle = bsDialog
  Caption = 'GUID Editor'
  ClientHeight = 93
  ClientWidth = 372
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Edit: TEdit
    Left = 12
    Top = 16
    Width = 272
    Height = 21
    TabOrder = 0
    OnChange = EditChange
    OnEnter = EditEnter
    OnExit = EditExit
  end
  object OKButton: TButton
    Left = 208
    Top = 62
    Width = 74
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 288
    Top = 62
    Width = 74
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
  object NewButton: TButton
    Left = 288
    Top = 14
    Width = 73
    Height = 25
    Caption = 'New'
    TabOrder = 3
    OnClick = NewButtonClick
  end
end
