object mcmPrintProgress: TmcmPrintProgress
  Left = 370
  Top = 193
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Print Progress'
  ClientHeight = 133
  ClientWidth = 293
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 8
    Top = 8
    Width = 277
    Height = 57
    Shape = bsFrame
  end
  object lStatus: TLabel
    Left = 20
    Top = 20
    Width = 249
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'lStatus'
  end
  object lPageNumber: TLabel
    Left = 20
    Top = 40
    Width = 253
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'lPageNumber'
  end
  object btnCancel: TButton
    Left = 109
    Top = 100
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 0
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 76
    Width = 277
    Height = 13
    Max = 10
    Step = 1
    TabOrder = 1
  end
end
