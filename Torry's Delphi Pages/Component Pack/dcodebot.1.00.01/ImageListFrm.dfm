object ImageListForm: TImageListForm
  Left = 249
  Top = 177
  BorderStyle = bsDialog
  Caption = 'Image List Viewer'
  ClientHeight = 373
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object CloseButton: TButton
    Left = 366
    Top = 96
    Width = 75
    Height = 25
    Caption = '&Close'
    ModalResult = 1
    TabOrder = 1
  end
  object ListView: TListView
    Left = 8
    Top = 8
    Width = 433
    Height = 81
    Columns = <>
    IconOptions.Arrangement = iaLeft
    IconOptions.WrapText = False
    TabOrder = 0
  end
end
