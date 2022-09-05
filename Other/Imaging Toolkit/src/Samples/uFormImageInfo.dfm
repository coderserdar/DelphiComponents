object FormImageInfo: TFormImageInfo
  Left = 344
  Top = 150
  BorderStyle = bsDialog
  Caption = 'Image Infomation'
  ClientHeight = 343
  ClientWidth = 354
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 96
    Top = 312
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 8
    Top = 312
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object pcImageInfo: TPageControl
    Left = 0
    Top = 0
    Width = 353
    Height = 305
    ActivePage = tsInformation
    TabOrder = 2
    object tsInformation: TTabSheet
      Caption = 'Information'
      object gbImageInfo: TGroupBox
        Left = 0
        Top = 0
        Width = 345
        Height = 277
        Align = alClient
        Caption = 'Image'
        TabOrder = 0
        object lDimension: TLabel
          Left = 16
          Top = 24
          Width = 52
          Height = 13
          Caption = 'Dimension:'
        end
        object lResolution: TLabel
          Left = 16
          Top = 72
          Width = 53
          Height = 13
          Caption = 'Resolution:'
        end
        object lPixelColor: TLabel
          Left = 16
          Top = 112
          Width = 89
          Height = 13
          Caption = 'Pixel bits / colours:'
        end
        object lDimensionVal: TLabel
          Left = 128
          Top = 24
          Width = 66
          Height = 13
          Caption = 'lDimensionVal'
        end
        object lResolutionVal: TLabel
          Left = 128
          Top = 72
          Width = 67
          Height = 13
          Caption = 'lResolutionVal'
        end
        object lPixelColorVal: TLabel
          Left = 128
          Top = 112
          Width = 63
          Height = 13
          Caption = 'lPixelColorVal'
        end
        object lDimensionDpi: TLabel
          Left = 128
          Top = 48
          Width = 66
          Height = 13
          Caption = 'lDimensionVal'
        end
        object lUsedColours: TLabel
          Left = 16
          Top = 136
          Width = 65
          Height = 13
          Caption = 'Used colours:'
        end
        object lUsedColoursVal: TLabel
          Left = 128
          Top = 136
          Width = 77
          Height = 13
          Caption = 'lUsedColoursVal'
        end
        object lModified: TLabel
          Left = 16
          Top = 248
          Width = 43
          Height = 13
          Caption = 'Modified:'
        end
        object lModifiedVal: TLabel
          Left = 128
          Top = 248
          Width = 57
          Height = 13
          Caption = 'lModifiedVal'
        end
        object lPixelHeight: TLabel
          Left = 16
          Top = 176
          Width = 57
          Height = 13
          Caption = 'Pixel height:'
        end
        object lPixelWidth: TLabel
          Left = 16
          Top = 200
          Width = 53
          Height = 13
          Caption = 'Pixel width:'
        end
        object lPixelHeightVal: TLabel
          Left = 128
          Top = 176
          Width = 70
          Height = 13
          Caption = 'lPixelHeightVal'
        end
        object lPixelWidthVal: TLabel
          Left = 128
          Top = 200
          Width = 67
          Height = 13
          Caption = 'lPixelWidthVal'
        end
      end
    end
    object tsAdditional: TTabSheet
      Caption = 'Additional'
      object gbExtraImageInfo: TGroupBox
        Left = 0
        Top = 0
        Width = 345
        Height = 277
        Align = alClient
        Caption = 'File information'
        TabOrder = 0
        object lFileFormat: TLabel
          Left = 16
          Top = 44
          Width = 51
          Height = 13
          Caption = 'File format:'
        end
        object lArtist: TLabel
          Left = 16
          Top = 100
          Width = 26
          Height = 13
          Caption = 'Artist:'
        end
        object lCopyright: TLabel
          Left = 16
          Top = 128
          Width = 47
          Height = 13
          Caption = 'Copyright:'
        end
        object lDateTime: TLabel
          Left = 16
          Top = 72
          Width = 69
          Height = 13
          Caption = 'Date and time:'
        end
        object lDescription: TLabel
          Left = 16
          Top = 160
          Width = 56
          Height = 13
          Caption = 'Description:'
        end
        object lFileSize: TLabel
          Left = 16
          Top = 16
          Width = 40
          Height = 13
          Caption = 'File size:'
        end
        object lFileSizeValue: TLabel
          Left = 96
          Top = 16
          Width = 6
          Height = 13
          Caption = '0'
        end
        object cbFileFormat: TComboBox
          Left = 96
          Top = 40
          Width = 233
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 0
          OnChange = cbFileFormatChange
        end
        object eDateTime: TEdit
          Left = 96
          Top = 68
          Width = 233
          Height = 21
          Enabled = False
          TabOrder = 1
          Text = 'eDateTime'
        end
        object eArtist: TEdit
          Left = 96
          Top = 96
          Width = 233
          Height = 21
          TabOrder = 2
          Text = 'eArtist'
        end
        object eCopyright: TEdit
          Left = 96
          Top = 124
          Width = 233
          Height = 21
          TabOrder = 3
          Text = 'eCopyright'
        end
        object mDescription: TMemo
          Left = 96
          Top = 152
          Width = 233
          Height = 113
          Lines.Strings = (
            'mDescription')
          TabOrder = 4
        end
      end
    end
    object tsDocument: TTabSheet
      Caption = 'Document'
      object gbDocument: TGroupBox
        Left = 0
        Top = 0
        Width = 345
        Height = 113
        Align = alTop
        Caption = 'Document'
        TabOrder = 0
        object lDocumentName: TLabel
          Left = 18
          Top = 20
          Width = 52
          Height = 13
          Caption = 'Document:'
        end
        object lPageName: TLabel
          Left = 16
          Top = 48
          Width = 57
          Height = 13
          Caption = 'Page name:'
        end
        object lPageNumber: TLabel
          Left = 16
          Top = 76
          Width = 66
          Height = 13
          Caption = 'Page number:'
        end
        object eDocumentName: TEdit
          Left = 96
          Top = 16
          Width = 233
          Height = 21
          TabOrder = 0
          Text = 'eDocumentName'
        end
        object ePageName: TEdit
          Left = 96
          Top = 44
          Width = 233
          Height = 21
          TabOrder = 1
          Text = 'ePageName'
        end
        object ePageNumber: TEdit
          Left = 96
          Top = 72
          Width = 57
          Height = 21
          TabOrder = 2
          Text = 'ePageNumber'
        end
      end
      object gbSystemInfo: TGroupBox
        Left = 0
        Top = 116
        Width = 345
        Height = 161
        Align = alBottom
        Caption = 'System Info'
        TabOrder = 1
        object lHostComputer: TLabel
          Left = 16
          Top = 20
          Width = 72
          Height = 13
          Caption = 'Host computer:'
        end
        object lMake: TLabel
          Left = 16
          Top = 48
          Width = 30
          Height = 13
          Caption = 'Make:'
        end
        object lModel: TLabel
          Left = 16
          Top = 76
          Width = 32
          Height = 13
          Caption = 'Model:'
        end
        object lSoftware: TLabel
          Left = 16
          Top = 104
          Width = 45
          Height = 13
          Caption = 'Software:'
        end
        object lSoftwareVersion: TLabel
          Left = 16
          Top = 132
          Width = 38
          Height = 13
          Caption = 'Version:'
        end
        object eHostComputer: TEdit
          Left = 96
          Top = 16
          Width = 233
          Height = 21
          TabOrder = 0
          Text = 'eHostComputer'
        end
        object eMake: TEdit
          Left = 96
          Top = 44
          Width = 233
          Height = 21
          TabOrder = 1
          Text = 'eMake'
        end
        object eModel: TEdit
          Left = 96
          Top = 72
          Width = 233
          Height = 21
          TabOrder = 2
          Text = 'eModel'
        end
        object eSoftware: TEdit
          Left = 96
          Top = 100
          Width = 233
          Height = 21
          TabOrder = 3
          Text = 'eSoftware'
        end
        object eVersion: TEdit
          Left = 96
          Top = 128
          Width = 233
          Height = 21
          TabOrder = 4
          Text = 'eVersion'
        end
      end
    end
    object tsExif: TTabSheet
      Caption = 'Exif'
      object sgExif: TStringGrid
        Left = 0
        Top = 0
        Width = 345
        Height = 277
        Align = alClient
        ColCount = 2
        DefaultRowHeight = 18
        FixedCols = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 0
        ColWidths = (
          129
          210)
      end
    end
    object tsGPS: TTabSheet
      Caption = 'GPS'
      object sgGPS: TStringGrid
        Left = 0
        Top = 0
        Width = 345
        Height = 277
        Align = alClient
        ColCount = 2
        DefaultRowHeight = 18
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ColWidths = (
          129
          210)
      end
    end
  end
end
