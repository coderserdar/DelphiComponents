object FormViewGoto: TFormViewGoto
  Left = 338
  Top = 255
  ActiveControl = edPos
  BorderStyle = bsDialog
  Caption = 'Go to'
  ClientHeight = 204
  ClientWidth = 265
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
  object btnOk: TButton
    Left = 48
    Top = 172
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 136
    Top = 172
    Width = 81
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object boxPosition: TGroupBox
    Left = 8
    Top = 4
    Width = 249
    Height = 161
    Caption = 'Position:'
    TabOrder = 0
    object edPos: TEdit
      Left = 56
      Top = 16
      Width = 81
      Height = 21
      TabOrder = 0
    end
    object chkPercent: TRadioButton
      Left = 56
      Top = 40
      Width = 190
      Height = 17
      Caption = '&Percent (%)'
      TabOrder = 1
    end
    object chkHex: TRadioButton
      Left = 56
      Top = 80
      Width = 190
      Height = 17
      Caption = '&Hex offset'
      TabOrder = 3
    end
    object chkDec: TRadioButton
      Left = 56
      Top = 96
      Width = 190
      Height = 17
      Caption = '&Decimal offset'
      TabOrder = 4
    end
    object chkSelStart: TRadioButton
      Left = 56
      Top = 120
      Width = 190
      Height = 17
      Caption = 'Selection start'
      TabOrder = 5
    end
    object chkSelEnd: TRadioButton
      Left = 56
      Top = 136
      Width = 190
      Height = 17
      Caption = 'Selection end'
      TabOrder = 6
    end
    object chkLine: TRadioButton
      Left = 56
      Top = 56
      Width = 190
      Height = 17
      Caption = 'Line number'
      TabOrder = 2
    end
  end
end
