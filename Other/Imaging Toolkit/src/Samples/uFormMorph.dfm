object FormMorph: TFormMorph
  Left = 266
  Top = 157
  BorderStyle = bsDialog
  Caption = 'Morphing'
  ClientHeight = 351
  ClientWidth = 362
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbOperation: TGroupBox
    Left = 8
    Top = 168
    Width = 345
    Height = 89
    Caption = 'Operation'
    TabOrder = 0
    object lIterations: TLabel
      Left = 16
      Top = 64
      Width = 46
      Height = 13
      Caption = '&Iterations:'
    end
    object lCoefEven: TLabel
      Left = 200
      Top = 32
      Width = 53
      Height = 13
      Caption = 'Coef Even:'
    end
    object lCoefOdd: TLabel
      Left = 200
      Top = 64
      Width = 48
      Height = 13
      Caption = 'Coef Odd:'
    end
    object cbMethod: TComboBox
      Left = 16
      Top = 24
      Width = 137
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'Close'
        'Dilate'
        'Erode'
        'Open'
        'Outline'
        'Shrinking (Look up table)'
        'Skeleton'
        'Skeleton (Look up table)'
        'Thinning (Look up table)')
      TabOrder = 0
      OnChange = cbMethodChange
    end
    object seIterations: TmcmIntSpin
      Left = 96
      Top = 56
      Width = 54
      Height = 22
      TabOrder = 1
      OnChange = SpinChange
      Value = 1
      MaxValue = 255
      MinValue = 1
    end
    object seCoefEven: TmcmIntSpin
      Left = 272
      Top = 24
      Width = 54
      Height = 22
      TabOrder = 2
      OnChange = SpinChange
      Value = 0
      MaxValue = 8
      MinValue = 0
    end
    object seCoefOdd: TmcmIntSpin
      Left = 272
      Top = 56
      Width = 54
      Height = 22
      TabOrder = 3
      OnChange = SpinChange
      Value = 0
      MaxValue = 8
      MinValue = 0
    end
  end
  object btnOK: TButton
    Left = 8
    Top = 320
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 88
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object gbFeatures: TGroupBox
    Left = 8
    Top = 264
    Width = 345
    Height = 49
    Caption = 'Features'
    TabOrder = 3
    object rbBlack: TRadioButton
      Left = 16
      Top = 24
      Width = 65
      Height = 17
      Caption = '&Black'
      TabOrder = 0
      OnClick = rbBlackClick
    end
    object rbWhite: TRadioButton
      Left = 96
      Top = 24
      Width = 57
      Height = 17
      Caption = '&White'
      TabOrder = 1
      OnClick = rbWhiteClick
    end
    object cbKeepSep: TCheckBox
      Left = 216
      Top = 24
      Width = 97
      Height = 17
      Caption = '&Keep Separated'
      TabOrder = 2
      OnClick = cbKeepSepClick
    end
  end
  object DualView: TmcmImageDualView
    Left = 0
    Top = 0
    Width = 361
    Height = 170
    GlyphSelectView.Data = {
      66010000424D6601000000000000760000002800000014000000140000000100
      040000000000F000000000000000000000001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFF
      FFFFF0FFFFFFFFFF0000FFFFFFFF808FFFFFFFFF0000FFFFFFFF000FFFFFFFFF
      0000FFFFFFFFF0FFFFFFFFFF0000FFFFFFFFF0FFFFFFFFFF0000FFFF80FFF0FF
      F08FFFFF0000FFF000000F000000FFFF0000FFFF80FFF0FFF08FFFFF0000FFFF
      FFFFF0FFFFFFFFFF0000FFFFFFFFF0FFFFFFFFFF0000FFFFFFFF000FFFFFFFFF
      0000FFFFFFFF808FFFFFFFFF0000FFFFFFFFF0FFFFFFFFFF0000FFFFFFFFFFFF
      FFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFF
      FFFFFFFFFFFFFFFF0000}
    GlyphZoomIn.Data = {
      66010000424D6601000000000000760000002800000014000000140000000100
      040000000000F000000000000000000000001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
      8888888800008888888888888888778800008888888888888887700800008888
      8888888888770008000088888888888887700088000088888777778877000888
      000088877000000770008888000088700F8F8F80000888880000880F88F7F8F8
      0088888800008708F8F708F8808888880000808F8F870F8FF7088888000080F8
      77770778870888880000808F80000000F7088888000080F8F8F708F887088888
      0000808F8F870F8F7808888800008808F8F808F8708888880000880F8F8F8F8F
      F08888880000888008F8F8700888888800008888800000088888888800008888
      88888888888888880000}
    GlyphZoomOut.Data = {
      66010000424D6601000000000000760000002800000014000000140000000100
      040000000000F000000000000000000000001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
      8888888800008888888888888888778800008888888888888887700800008888
      8888888888770008000088888888888887700088000088888777778877000888
      000088877000000770008888000088700F8F8F80000888880000880F88F8F8F8
      0088888800008708F8F8F8F8808888880000808F8F8F8F8FF7088888000080F8
      77777778870888880000808F80000000F7088888000080F8F8F8F8F887088888
      0000808F8F8F8F8F7808888800008808F8F8F8F8708888880000880F8F8F8F8F
      F08888880000888008F8F8700888888800008888800000088888888800008888
      88888888888888880000}
    TabOrder = 4
  end
  object mcmImageMorph: TmcmImageMorph
    Left = 168
    Top = 320
  end
end
