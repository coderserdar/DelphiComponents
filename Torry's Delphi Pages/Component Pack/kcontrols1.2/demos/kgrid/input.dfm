object InputForm: TInputForm
  Left = 478
  Top = 395
  BorderStyle = bsDialog
  Caption = 'a'
  ClientHeight = 117
  ClientWidth = 278
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 87
    Height = 13
    Caption = 'Enter column title:'
    Color = clBtnFace
    ParentColor = False
  end
  object Edit1: TEdit
    Left = 16
    Top = 32
    Width = 249
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 56
    Top = 72
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 144
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
