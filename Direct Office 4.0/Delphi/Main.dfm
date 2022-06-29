object FormMain: TFormMain
  Left = 774
  Top = 413
  BorderStyle = bsDialog
  Caption = 'Direct Office example'
  ClientHeight = 115
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonCreateDocument: TButton
    Left = 48
    Top = 25
    Width = 200
    Height = 25
    Caption = 'Create Word Document'
    TabOrder = 0
    OnClick = ButtonCreateDocumentClick
  end
  object ButtonCreateWorkbook: TButton
    Left = 49
    Top = 65
    Width = 200
    Height = 25
    Caption = 'Create Excel Workbook'
    TabOrder = 1
    OnClick = ButtonCreateWorkbookClick
  end
end
