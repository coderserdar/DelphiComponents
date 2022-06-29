object ELStringsEditorDlg: TELStringsEditorDlg
  Left = 245
  Top = 177
  BorderStyle = bsDialog
  Caption = 'String List Editor'
  ClientHeight = 279
  ClientWidth = 431
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
  object lbLineCount: TLabel
    Left = 12
    Top = 12
    Width = 169
    Height = 17
    AutoSize = False
    Caption = '0 lines'
  end
  object bvlMain: TBevel
    Left = 8
    Top = 8
    Width = 413
    Height = 229
    Shape = bsFrame
  end
  object btnOk: TButton
    Left = 265
    Top = 248
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 345
    Top = 248
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object memMain: TRichEdit
    Left = 16
    Top = 31
    Width = 397
    Height = 197
    HideScrollBars = False
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
    OnChange = memMainChange
  end
end
