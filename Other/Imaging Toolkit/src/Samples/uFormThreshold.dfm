object FormThreshold: TFormThreshold
  Left = 425
  Top = 131
  ActiveControl = btnOK
  BorderStyle = bsDialog
  Caption = 'Threshold'
  ClientHeight = 313
  ClientWidth = 364
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
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
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 8
    Top = 280
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 88
    Top = 280
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object gbThreshold: TGroupBox
    Left = 8
    Top = 168
    Width = 185
    Height = 105
    TabOrder = 0
    object lMethod: TLabel
      Left = 16
      Top = 16
      Width = 39
      Height = 13
      Caption = '&Method:'
      FocusControl = cbMethod
    end
    object lLevel: TLabel
      Left = 16
      Top = 72
      Width = 29
      Height = 13
      Caption = '&Level:'
      FocusControl = seLevel
    end
    object cbMethod: TComboBox
      Left = 16
      Top = 32
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'Intensity Level'
        'ISO Data'
        'Background Symmetry'
        'Trace'
        'Triangular'
        'Edge'
        'Optimized'
        'Statistic')
      TabOrder = 0
      OnChange = cbMethodChange
    end
    object seLevel: TmcmIntSpin
      Left = 96
      Top = 64
      Width = 73
      Height = 22
      TabOrder = 1
      OnChange = ValueChange
      Value = 0
      MaxValue = 255
      MinValue = 0
    end
  end
  object gbSegments: TGroupBox
    Left = 200
    Top = 168
    Width = 153
    Height = 105
    Caption = 'Segment'
    TabOrder = 4
    object lCols: TLabel
      Left = 16
      Top = 40
      Width = 43
      Height = 13
      Caption = 'Col&umns:'
      Enabled = False
      FocusControl = seCols
    end
    object lRows: TLabel
      Left = 16
      Top = 72
      Width = 30
      Height = 13
      Caption = '&Rows:'
      Enabled = False
      FocusControl = seRows
    end
    object seCols: TmcmIntSpin
      Left = 72
      Top = 32
      Width = 62
      Height = 22
      Enabled = False
      TabOrder = 0
      OnChange = ValueChange
      Value = 1
      MaxValue = 64
      MinValue = 1
    end
    object seRows: TmcmIntSpin
      Left = 72
      Top = 64
      Width = 62
      Height = 22
      Enabled = False
      TabOrder = 1
      OnChange = ValueChange
      Value = 1
      MaxValue = 64
      MinValue = 1
    end
  end
  object gbTrace: TGroupBox
    Left = 200
    Top = 168
    Width = 153
    Height = 105
    Caption = '     '
    TabOrder = 5
    Visible = False
    object lLow: TLabel
      Left = 16
      Top = 40
      Width = 23
      Height = 13
      Caption = 'Lo&w:'
      Enabled = False
    end
    object lHigh: TLabel
      Left = 16
      Top = 72
      Width = 25
      Height = 13
      Caption = '&High:'
      Enabled = False
    end
    object isLow: TmcmIntSpin
      Left = 72
      Top = 32
      Width = 62
      Height = 22
      Enabled = False
      TabOrder = 0
      OnChange = ValueChange
      Value = 0
      MaxValue = 255
      MinValue = 0
    end
    object isHigh: TmcmIntSpin
      Left = 72
      Top = 64
      Width = 62
      Height = 22
      Enabled = False
      TabOrder = 1
      OnChange = ValueChange
      Value = 0
      MaxValue = 255
      MinValue = 0
    end
    object cbTraceLevels: TCheckBox
      Left = 16
      Top = 0
      Width = 89
      Height = 17
      Caption = '&Trace levels'
      Enabled = False
      TabOrder = 2
      OnClick = cbTraceLevelsClick
    end
  end
  object mcmImageColor: TmcmImageColor
    Left = 280
    Top = 280
  end
end
