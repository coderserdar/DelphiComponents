object FormPalette: TFormPalette
  Left = 445
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Image Palette'
  ClientHeight = 344
  ClientWidth = 413
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 8
    Top = 312
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 88
    Top = 312
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object gbPalette: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 297
    Caption = 'Palette'
    TabOrder = 2
    object ColorGrid: TmcmColorGrid
      Left = 17
      Top = 25
      Width = 256
      Height = 256
      AutoUpdate = True
      Brush.Color = clBtnFace
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
      TabStop = True
      OnPaletteIndex = ColorGridPaletteIndex
    end
  end
  object gbEditColor: TGroupBox
    Left = 304
    Top = 8
    Width = 97
    Height = 297
    Caption = 'Edit Color'
    TabOrder = 3
    object sNewColor: TShape
      Left = 16
      Top = 56
      Width = 65
      Height = 33
      Brush.Color = clBlack
      Pen.Color = clGrayText
      Pen.Style = psClear
    end
    object sOldColor: TShape
      Left = 16
      Top = 24
      Width = 65
      Height = 33
      ParentShowHint = False
      Pen.Style = psClear
      ShowHint = False
    end
    object lA: TLabel
      Left = 16
      Top = 144
      Width = 23
      Height = 13
      Caption = '&Red:'
    end
    object lB: TLabel
      Left = 16
      Top = 192
      Width = 32
      Height = 13
      Caption = '&Green:'
    end
    object lC: TLabel
      Left = 16
      Top = 240
      Width = 24
      Height = 13
      Caption = '&Blue:'
    end
    object lPalIndex: TLabel
      Left = 16
      Top = 112
      Width = 32
      Height = 13
      Caption = 'Index: '
    end
    object seA: TmcmIntSpin
      Left = 16
      Top = 160
      Width = 65
      Height = 22
      TabOrder = 0
      OnChange = ChangeColor
      Value = 0
      MaxValue = 255
      MinValue = 0
    end
    object seB: TmcmIntSpin
      Left = 16
      Top = 208
      Width = 65
      Height = 22
      TabOrder = 1
      OnChange = ChangeColor
      Value = 0
      MaxValue = 255
      MinValue = 0
    end
    object seC: TmcmIntSpin
      Left = 16
      Top = 256
      Width = 65
      Height = 22
      TabOrder = 2
      OnChange = ChangeColor
      Value = 0
      MaxValue = 255
      MinValue = 0
    end
  end
end
