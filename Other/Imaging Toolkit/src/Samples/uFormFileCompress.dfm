object FormFileCompress: TFormFileCompress
  Left = 365
  Top = 121
  BorderStyle = bsDialog
  Caption = 'Compress'
  ClientHeight = 223
  ClientWidth = 221
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pcCompress: TPageControl
    Left = 0
    Top = 0
    Width = 221
    Height = 185
    ActivePage = tsTiff
    Align = alTop
    TabOrder = 0
    object tsCri: TTabSheet
      Caption = 'CRI'
      object cbCRI: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object rbCRIUncompress: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Uncompressed'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbSelectCompressClick
        end
      end
    end
    object tsJpeg: TTabSheet
      Caption = 'JPEG'
      object gbJPEG: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object lJPEGQuality: TLabel
          Left = 16
          Top = 88
          Width = 35
          Height = 13
          Caption = 'Quality:'
        end
        object seQuality: TSpinEdit
          Left = 64
          Top = 80
          Width = 57
          Height = 22
          MaxValue = 100
          MinValue = 1
          TabOrder = 0
          Value = 100
          OnChange = seQualityChange
        end
        object rbJPEGStandard: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Standard encoding'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = rbSelectCompressClick
        end
        object rbJPEGProgressive: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = 'Progressive encoding'
          TabOrder = 2
          OnClick = rbSelectCompressClick
        end
      end
    end
    object tsTiff: TTabSheet
      Caption = 'TIFF'
      object gbTIFF: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object rbTIFFFaxCcitt3: TRadioButton
          Left = 16
          Top = 16
          Width = 177
          Height = 17
          Caption = 'FAX, CCITT Group 3, 1-D'
          Enabled = False
          TabOrder = 0
          OnClick = rbSelectCompressClick
        end
        object rbTIFFHuffmanRLE: TRadioButton
          Left = 16
          Top = 76
          Width = 177
          Height = 17
          Caption = 'Huffman run length encoding'
          Enabled = False
          TabOrder = 1
          OnClick = rbSelectCompressClick
        end
        object rbTIFFLZW: TRadioButton
          Left = 16
          Top = 96
          Width = 177
          Height = 17
          Caption = 'LZW compression'
          Enabled = False
          TabOrder = 2
          OnClick = rbSelectCompressClick
        end
        object rbTIFFPackBits: TRadioButton
          Left = 16
          Top = 116
          Width = 177
          Height = 17
          Caption = 'Pack bits'
          Enabled = False
          TabOrder = 3
          OnClick = rbSelectCompressClick
        end
        object rbTIFFUncompress: TRadioButton
          Left = 16
          Top = 136
          Width = 177
          Height = 17
          Caption = 'Uncompressed'
          Checked = True
          Enabled = False
          TabOrder = 4
          TabStop = True
          OnClick = rbSelectCompressClick
        end
        object rbTIFFFaxCcitt3_2D: TRadioButton
          Left = 16
          Top = 36
          Width = 177
          Height = 17
          Caption = 'FAX, CCITT Group 3, 2-D'
          Enabled = False
          TabOrder = 5
          OnClick = rbSelectCompressClick
        end
        object rbTIFFFaxCcitt4: TRadioButton
          Left = 16
          Top = 56
          Width = 177
          Height = 17
          Caption = 'FAX, CCITT Group 4'
          Enabled = False
          TabOrder = 6
          OnClick = rbSelectCompressClick
        end
      end
    end
    object tsTarga: TTabSheet
      Caption = 'Targa'
      object gbTARGA: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object rbTGACompress: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Run length encoding'
          Enabled = False
          TabOrder = 0
          OnClick = rbSelectCompressClick
        end
        object rbTGAUncompress: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = 'Uncompressed'
          Checked = True
          Enabled = False
          TabOrder = 1
          TabStop = True
          OnClick = rbSelectCompressClick
        end
      end
    end
    object tsBmp: TTabSheet
      Caption = 'BMP'
      object gbBMP: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object rbBMPRGB: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'RGB, Uncompressed'
          Checked = True
          Enabled = False
          TabOrder = 0
          TabStop = True
          OnClick = rbSelectCompressClick
        end
        object rbBMP4RLE: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = '4 Bit run length encoding'
          Enabled = False
          TabOrder = 1
          OnClick = rbSelectCompressClick
        end
        object rbBMP8RLE: TRadioButton
          Left = 16
          Top = 72
          Width = 177
          Height = 17
          Caption = '8 Bit run length encoding'
          Enabled = False
          TabOrder = 2
          OnClick = rbSelectCompressClick
        end
      end
    end
    object tsGIF: TTabSheet
      Caption = 'GIF'
      object gbGIF: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 73
        Align = alTop
        Caption = 'Version'
        TabOrder = 0
        object rbGIFVersion87A: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Version 87 A'
          Enabled = False
          TabOrder = 0
          OnClick = rbSelectCompressClick
        end
        object rbGIFVersion89A: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = 'Version 89 A'
          Checked = True
          Enabled = False
          TabOrder = 1
          TabStop = True
          OnClick = rbSelectCompressClick
        end
      end
      object gbGIFInterlacing: TGroupBox
        Left = 0
        Top = 80
        Width = 213
        Height = 77
        Align = alBottom
        Caption = 'Interlacing'
        TabOrder = 1
        object rbGIFInterlaced: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Interlaced'
          Checked = True
          Enabled = False
          TabOrder = 0
          TabStop = True
          OnClick = rbSelectGIFCompressClick
        end
        object rbGIFNonInterlaced: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = 'Non-Interlaced'
          Enabled = False
          TabOrder = 1
          OnClick = rbSelectGIFCompressClick
        end
      end
    end
    object tsICO: TTabSheet
      Caption = 'ICO'
      object gbICO: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
      end
    end
    object tsPBM: TTabSheet
      Caption = 'PBM'
      object gbPBM: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object rbPBMBinary: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Binary'
          Enabled = False
          TabOrder = 0
          OnClick = rbSelectCompressClick
        end
        object rbPBMASCII: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = 'ASCII'
          Enabled = False
          TabOrder = 1
          OnClick = rbSelectCompressClick
        end
      end
    end
    object tsPGM: TTabSheet
      Caption = 'PGM'
      object gbPGM: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object rbPGMBinary: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Binary'
          Enabled = False
          TabOrder = 0
          OnClick = rbSelectCompressClick
        end
        object rbPGMASCII: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = 'ASCII'
          Enabled = False
          TabOrder = 1
          OnClick = rbSelectCompressClick
        end
      end
    end
    object tsPPM: TTabSheet
      Caption = 'PPM'
      object gbPPM: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object rbPPMBinary: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Binary'
          Enabled = False
          TabOrder = 0
          OnClick = rbSelectCompressClick
        end
        object rbPPMASCII: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = 'ASCII'
          Enabled = False
          TabOrder = 1
          OnClick = rbSelectCompressClick
        end
      end
    end
    object tsPNG: TTabSheet
      Caption = 'PNG'
      object gbPNG: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object lPNGQuality: TLabel
          Left = 16
          Top = 88
          Width = 35
          Height = 13
          Caption = 'Quality:'
        end
        object rbPNGProgressive: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = 'LZ, Progressive'
          TabOrder = 0
          OnClick = rbSelectCompressClick
        end
        object rbPNGStandard: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'LZ, Standard'
          Enabled = False
          TabOrder = 1
          OnClick = rbSelectCompressClick
        end
        object cbPNGQuality: TComboBox
          Left = 64
          Top = 80
          Width = 73
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'Low'
            'Medium'
            'High'
            'Maximum')
          TabOrder = 2
          OnChange = cbPNGQualityChange
        end
      end
    end
    object tsSGI: TTabSheet
      Caption = 'SGI'
      object gbSGI: TGroupBox
        Left = 0
        Top = 0
        Width = 213
        Height = 157
        Align = alClient
        Caption = 'Encoding'
        TabOrder = 0
        object rbSGIUncompressed: TRadioButton
          Left = 16
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Uncompressed'
          Enabled = False
          TabOrder = 0
          OnClick = rbSelectCompressClick
        end
        object rbSGIRLE: TRadioButton
          Left = 16
          Top = 48
          Width = 177
          Height = 17
          Caption = 'Run length encoding'
          Enabled = False
          TabOrder = 1
          OnClick = rbSelectCompressClick
        end
      end
    end
  end
  object btnOK: TButton
    Left = 8
    Top = 192
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
    Top = 192
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
