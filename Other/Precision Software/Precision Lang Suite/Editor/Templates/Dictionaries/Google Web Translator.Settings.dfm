object PSForm: TPSForm
  Left = 276
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Google Web Translator'
  ClientHeight = 160
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    420
    160)
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel
    Left = 16
    Top = 16
    Width = 385
    Height = 81
    AutoSize = False
    Caption = 
      'Please, do not use "Google Web Translator" service if it is not ' +
      'really necessary! It is provided for very special situations onl' +
      'y, when no other service or translator is available and the tran' +
      'slation is crucial for you.'#13#10#13#10'If you decide to use it, translat' +
      'e only a few items at a time.'
    WordWrap = True
  end
  object Bevel2: TBevel
    AlignWithMargins = True
    Left = 0
    Top = 118
    Width = 420
    Height = 2
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 40
    Align = alBottom
    Shape = bsBottomLine
    ExplicitTop = 296
    ExplicitWidth = 600
  end
  object sbCancel: TButton
    Left = 334
    Top = 128
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 0
    ExplicitLeft = 514
    ExplicitTop = 308
  end
end
