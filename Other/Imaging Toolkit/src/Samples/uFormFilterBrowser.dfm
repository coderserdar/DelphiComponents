object FormFilterBrowser: TFormFilterBrowser
  Left = 280
  Top = 159
  BorderStyle = bsDialog
  Caption = 'Filter Browser'
  ClientHeight = 384
  ClientWidth = 362
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 88
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnOK: TButton
    Left = 8
    Top = 352
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object gbMatrixFilter: TGroupBox
    Left = 8
    Top = 224
    Width = 345
    Height = 121
    Caption = 'Matrix Filter'
    TabOrder = 4
    Visible = False
    object lFilterSize: TLabel
      Left = 16
      Top = 32
      Width = 46
      Height = 13
      Caption = 'Filter &size:'
      FocusControl = sePercentage
    end
    object lHysteresis: TLabel
      Left = 16
      Top = 64
      Width = 51
      Height = 13
      Caption = '&Hysteresis:'
      FocusControl = seHysteresis
    end
    object lHarmonicOrder: TLabel
      Left = 200
      Top = 32
      Width = 29
      Height = 13
      Caption = 'Or&der:'
      FocusControl = rsHarmonicOrder
    end
    object seFilterSize: TmcmIntSpin
      Left = 96
      Top = 24
      Width = 54
      Height = 22
      TabOrder = 0
      OnChange = seFilterSizeChange
      Value = 3
      MaxValue = 15
      MinValue = 3
    end
    object seHysteresis: TmcmIntSpin
      Left = 96
      Top = 56
      Width = 54
      Height = 22
      TabOrder = 1
      OnChange = seHysteresisChange
      Value = 3
      MaxValue = 255
      MinValue = 1
    end
    object rsHarmonicOrder: TmcmRealSpin
      Left = 256
      Top = 24
      Width = 62
      Height = 22
      TabOrder = 2
      OnChange = rsHarmonicOrderChange
      Value = 1.000000000000000000
      MaxValue = 10.000000000000000000
      MinValue = -10.000000000000000000
      Decimals = 2
      Increment = 0.100000000000000000
    end
  end
  object gbMarrHildreth: TGroupBox
    Left = 8
    Top = 224
    Width = 345
    Height = 121
    Caption = 'Marr-Hildreth'
    TabOrder = 5
    object lDeviation: TLabel
      Left = 16
      Top = 32
      Width = 48
      Height = 13
      Caption = '&Deviation:'
      FocusControl = rsGaussSD
    end
    object lDelta: TLabel
      Left = 16
      Top = 64
      Width = 28
      Height = 13
      Caption = 'D&elta:'
      FocusControl = rsDeltaSD
    end
    object rsGaussSD: TmcmRealSpin
      Left = 96
      Top = 24
      Width = 54
      Height = 22
      TabOrder = 0
      OnChange = rsGaussSDChange
      Value = 2.000000000000000000
      MaxValue = 5.000000000000000000
      Decimals = 2
      Increment = 0.100000000000000000
    end
    object rsDeltaSD: TmcmRealSpin
      Left = 96
      Top = 56
      Width = 54
      Height = 22
      TabOrder = 1
      OnChange = rsDeltaSDChange
      Value = 0.800000000000000000
      MaxValue = 4.000000000000000000
      MinValue = 0.100000000000000000
      Decimals = 2
      Increment = 0.100000000000000000
    end
  end
  object gbCanny: TGroupBox
    Left = 8
    Top = 224
    Width = 169
    Height = 121
    Caption = 'Canny'
    TabOrder = 6
    object lCannyDeviation: TLabel
      Left = 16
      Top = 32
      Width = 48
      Height = 13
      Caption = '&Deviation:'
      FocusControl = rsCannyDeviation
    end
    object lPercentage: TLabel
      Left = 16
      Top = 64
      Width = 58
      Height = 13
      Caption = '&Percentage:'
      FocusControl = sePercentage
    end
    object lSCSmoothFct: TLabel
      Left = 16
      Top = 32
      Width = 72
      Height = 13
      Caption = 'Smooth Factor:'
    end
    object rsCannyDeviation: TmcmRealSpin
      Left = 96
      Top = 24
      Width = 54
      Height = 22
      TabOrder = 0
      OnChange = rsGaussSDChange
      Value = 2.000000000000000000
      MaxValue = 5.000000000000000000
      Decimals = 2
      Increment = 0.100000000000000000
    end
    object sePercentage: TmcmIntSpin
      Left = 96
      Top = 56
      Width = 54
      Height = 22
      TabOrder = 2
      OnChange = sePercentageChange
      Value = 1
      MaxValue = 100
      MinValue = 1
    end
    object rsSCSmoothFct: TmcmRealSpin
      Left = 96
      Top = 24
      Width = 54
      Height = 22
      TabOrder = 1
      OnChange = rsSCSmoothFctChange
      Value = 0.700000000000000000
      MaxValue = 0.990000000000000000
      MinValue = 0.010000000000000000
      Decimals = 2
      Increment = 0.010000000000000000
    end
  end
  object gbCannyTrace: TGroupBox
    Left = 184
    Top = 224
    Width = 169
    Height = 121
    Caption = '     '
    TabOrder = 7
    Visible = False
    object lLow: TLabel
      Left = 24
      Top = 64
      Width = 23
      Height = 13
      Caption = 'Lo&w:'
      Enabled = False
      FocusControl = isLow
    end
    object lHigh: TLabel
      Left = 24
      Top = 32
      Width = 25
      Height = 13
      Caption = '&High:'
      Enabled = False
      FocusControl = isHigh
    end
    object isLow: TmcmIntSpin
      Left = 80
      Top = 56
      Width = 62
      Height = 22
      Enabled = False
      TabOrder = 0
      OnChange = isLowChange
      Value = 0
      MaxValue = 255
      MinValue = 0
    end
    object isHigh: TmcmIntSpin
      Left = 80
      Top = 24
      Width = 62
      Height = 22
      Enabled = False
      TabOrder = 1
      OnChange = isHighChange
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
      TabOrder = 2
      OnClick = cbTraceLevelsClick
    end
  end
  object DualView: TmcmImageDualView
    Left = 0
    Top = 0
    Width = 361
    Height = 169
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
    TabOrder = 8
  end
  object gbFilter: TGroupBox
    Left = 8
    Top = 168
    Width = 345
    Height = 49
    TabOrder = 3
    object lFilter: TLabel
      Left = 24
      Top = 20
      Width = 25
      Height = 13
      Caption = '&Filter:'
      FocusControl = cbFilter
    end
    object cbFilter: TComboBox
      Left = 80
      Top = 16
      Width = 169
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = lbFilterClick
      OnDropDown = cbFilterDropDown
    end
  end
  object btnHelp: TButton
    Left = 280
    Top = 352
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = btnHelpClick
  end
end
