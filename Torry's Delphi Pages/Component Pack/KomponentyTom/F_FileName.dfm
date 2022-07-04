object FormFileName: TFormFileName
  Left = 0
  Top = 0
  ActiveControl = EditFileName
  BorderStyle = bsDialog
  ClientHeight = 127
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelKom: TLabel
    Left = 16
    Top = 16
    Width = 3
    Height = 13
  end
  object EditFileName: TEdit
    Left = 16
    Top = 32
    Width = 353
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 208
    Top = 88
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 296
    Top = 88
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Anuluj'
    ModalResult = 2
    TabOrder = 2
  end
end
