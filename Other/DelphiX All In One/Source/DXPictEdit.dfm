object DelphiXPictureEditForm: TDelphiXPictureEditForm
  Left = 318
  Top = 330
  ActiveControl = CancelButton
  BorderStyle = bsDialog
  Caption = 'Picture editor'
  ClientHeight = 305
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PopupMenu = PopupMenu1
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 272
    Top = 80
    Width = 81
    Height = 81
  end
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 257
    Height = 289
    Shape = bsFrame
  end
  object SizeLabel: TLabel
    Left = 280
    Top = 104
    Width = 20
    Height = 13
    Caption = 'Size'
  end
  object BitCountLabel: TLabel
    Left = 280
    Top = 120
    Width = 40
    Height = 13
    Caption = 'BitCount'
  end
  object BitSizeLabel: TLabel
    Left = 280
    Top = 136
    Width = 32
    Height = 13
    Caption = 'BitSize'
  end
  object ClassNameLabel: TLabel
    Left = 280
    Top = 88
    Width = 53
    Height = 13
    Caption = 'ClassName'
  end
  object LoadButton: TButton
    Left = 16
    Top = 264
    Width = 73
    Height = 25
    Caption = '&Load...'
    TabOrder = 2
    OnClick = LoadButtonClick
  end
  object SaveButton: TButton
    Left = 96
    Top = 264
    Width = 73
    Height = 25
    Caption = '&Save...'
    TabOrder = 3
    OnClick = SaveButtonClick
  end
  object ClearButton: TButton
    Left = 176
    Top = 264
    Width = 73
    Height = 25
    Caption = '&Clear'
    TabOrder = 4
    OnClick = ClearButtonClick
  end
  object OKButton: TButton
    Left = 272
    Top = 8
    Width = 81
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 272
    Top = 40
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Default = True
    TabOrder = 1
    OnClick = CancelButtonClick
  end
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 241
    Height = 241
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWhite
    TabOrder = 5
    object Shape: TShape
      Left = -10
      Top = -10
      Width = 251
      Height = 251
      Brush.Color = clBlack
      Brush.Style = bsDiagCross
    end
    object NoneLabel: TLabel
      Left = 110
      Top = 112
      Width = 32
      Height = 13
      Alignment = taCenter
      Caption = '(None)'
    end
    object ViewBox: TImage
      Left = 6
      Top = 6
      Width = 228
      Height = 228
      Center = True
      PopupMenu = PopupMenu1
      Stretch = True
    end
  end
  object ConvertToDIB: TButton
    Left = 272
    Top = 168
    Width = 81
    Height = 25
    Caption = 'TDIB'
    TabOrder = 6
    OnClick = ConvertToDIBClick
  end
  object OpenDialog: TOpenPictureDialog
    DefaultExt = 'bmp'
    Filter = ' '
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Left = 104
    Top = 168
  end
  object SaveDialog: TSavePictureDialog
    DefaultExt = 'bmp'
    Filter = ' '
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist]
    Left = 136
    Top = 168
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 168
    Top = 168
    object geConvertColor: TMenuItem
      Tag = 1
      Caption = '&Convert color'
      object N15: TMenuItem
        Tag = 1
        Caption = '2 color'
        OnClick = geConvertColorClick
      end
      object N41: TMenuItem
        Tag = 4
        Caption = '16 color'
        OnClick = geConvertColorClick
      end
      object N21: TMenuItem
        Tag = 8
        Caption = '256 color'
        OnClick = geConvertColorClick
      end
      object N22: TMenuItem
        Tag = 24
        Caption = '24 bit color'
        OnClick = geConvertColorClick
      end
    end
    object geGreyscale: TMenuItem
      Tag = 1
      Caption = '&Greyscale'
      object N11: TMenuItem
        Tag = 1
        Caption = '2 color'
        OnClick = geGreyscaleClick
      end
      object N12: TMenuItem
        Tag = 4
        Caption = '16 color'
        OnClick = geGreyscaleClick
      end
      object N13: TMenuItem
        Tag = 8
        Caption = '256 color'
        OnClick = geGreyscaleClick
      end
      object N14: TMenuItem
        Tag = 24
        Caption = '24 bit color'
        OnClick = geGreyscaleClick
      end
    end
    object geNegative: TMenuItem
      Tag = 1
      Caption = '&Negative'
      OnClick = geNegativeClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object geCompress: TMenuItem
      Tag = 1
      Caption = '&Compress'
      OnClick = geCompressClick
    end
    object geDecompress: TMenuItem
      Tag = 1
      Caption = '&Decompress'
      OnClick = geDecompressClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object geCopy: TMenuItem
      Tag = 1
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = geCopyClick
    end
    object gePaste: TMenuItem
      Caption = '&Paste'
      ShortCut = 16470
      OnClick = gePasteClick
    end
  end
end
