object frmExcelErrors: TfrmExcelErrors
  Left = 309
  Top = 355
  BorderStyle = bsDialog
  Caption = 'Errors during import'
  ClientHeight = 275
  ClientWidth = 541
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lblCaption: TLabel
    Left = 8
    Top = 8
    Width = 495
    Height = 39
    Caption = 
      'There were some  rows with illegal values in the Excel sheet. Ei' +
      'ther you tried to changed locked fields (or rows) or the values ' +
      'for the primary dimensions were outside the open crieteria for t' +
      'he forecast talble. All legal values were imported.'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 248
    Top = 240
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object MemoErrors: TMemo
    Left = 8
    Top = 52
    Width = 529
    Height = 173
    Lines.Strings = (
      'MemoErrors')
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
