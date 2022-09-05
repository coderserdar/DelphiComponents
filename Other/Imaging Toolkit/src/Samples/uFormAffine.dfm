object FormAffine: TFormAffine
  Left = 299
  Top = 232
  BorderStyle = bsDialog
  Caption = 'Affine transformation'
  ClientHeight = 375
  ClientWidth = 361
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
  object gbStretch: TGroupBox
    Left = 8
    Top = 168
    Width = 153
    Height = 81
    Caption = '&Scale/Stretch'
    TabOrder = 2
    object lHorizScale: TLabel
      Left = 16
      Top = 24
      Width = 44
      Height = 13
      Caption = 'Horiz [%]:'
    end
    object LVertScale: TLabel
      Left = 16
      Top = 56
      Width = 39
      Height = 13
      Caption = 'Vert [%]:'
    end
    object sexScale: TmcmIntSpin
      Left = 80
      Top = 16
      Width = 57
      Height = 22
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
      OnChange = AffineChange
      Value = 0
      MaxValue = 0
      MinValue = 0
    end
    object seyScale: TmcmIntSpin
      Left = 80
      Top = 48
      Width = 57
      Height = 22
      TabOrder = 1
      OnChange = AffineChange
      Value = 0
      MaxValue = 0
      MinValue = 0
    end
  end
  object gbRotate: TGroupBox
    Left = 200
    Top = 256
    Width = 153
    Height = 49
    Caption = 'Rotate'
    TabOrder = 5
    object LRotate: TLabel
      Left = 16
      Top = 24
      Width = 38
      Height = 13
      Caption = '&Degree:'
    end
    object seRotate: TmcmIntSpin
      Left = 80
      Top = 16
      Width = 57
      Height = 22
      TabOrder = 0
      OnChange = AffineChange
      Value = 0
      MaxValue = 0
      MinValue = 0
    end
  end
  object gbShear: TGroupBox
    Left = 8
    Top = 256
    Width = 153
    Height = 81
    Caption = 'S&hear'
    TabOrder = 3
    object LHorizShear: TLabel
      Left = 16
      Top = 24
      Width = 58
      Height = 13
      Caption = 'Horiz [Pixel]:'
    end
    object LVertShear: TLabel
      Left = 16
      Top = 56
      Width = 53
      Height = 13
      Caption = 'Vert [Pixel]:'
    end
    object sexShear: TmcmIntSpin
      Left = 80
      Top = 16
      Width = 54
      Height = 22
      TabOrder = 0
      OnChange = AffineChange
      Value = 0
      MaxValue = 0
      MinValue = 0
    end
    object seyShear: TmcmIntSpin
      Left = 80
      Top = 48
      Width = 54
      Height = 22
      TabOrder = 1
      OnChange = AffineChange
      Value = 0
      MaxValue = 0
      MinValue = 0
    end
  end
  object gbTranslate: TGroupBox
    Left = 200
    Top = 168
    Width = 153
    Height = 81
    Caption = '&Translate'
    TabOrder = 4
    object LHorizTrans: TLabel
      Left = 16
      Top = 24
      Width = 58
      Height = 13
      Caption = 'Horiz [Pixel]:'
    end
    object LVertTrans: TLabel
      Left = 16
      Top = 56
      Width = 53
      Height = 13
      Caption = 'Vert [Pixel]:'
    end
    object rsxTrans: TmcmRealSpin
      Left = 80
      Top = 16
      Width = 57
      Height = 22
      TabOrder = 0
      OnChange = AffineDblChange
      Value = 1.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
    object rsyTrans: TmcmRealSpin
      Left = 80
      Top = 48
      Width = 57
      Height = 22
      TabOrder = 1
      OnChange = AffineDblChange
      Value = 1.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
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
    MoveScaleResult = False
    TabOrder = 7
    OnImageMoved = DualViewImageMoved
  end
  object btnOK: TButton
    Left = 8
    Top = 344
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 88
    Top = 344
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbInterpolate: TGroupBox
    Left = 200
    Top = 312
    Width = 153
    Height = 57
    Caption = '&Interpolate'
    TabOrder = 6
    object cbMethod: TComboBox
      Left = 16
      Top = 24
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'Nearest Neighbour'
        'Bilinear'
        'Biquadratic'
        'Bicubic'
        'Hermite')
      TabOrder = 0
      OnChange = cbMethodChange
    end
  end
  object mcmImageTransform: TmcmImageTransform
    Left = 168
    Top = 344
  end
end
