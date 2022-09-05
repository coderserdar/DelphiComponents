object DlgTwainPref: TDlgTwainPref
  Left = 388
  Top = 196
  ActiveControl = btnScan
  BorderStyle = bsDialog
  Caption = 'TWAIN Preferences'
  ClientHeight = 352
  ClientWidth = 288
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnScan: TButton
    Left = 8
    Top = 320
    Width = 81
    Height = 25
    Caption = '&Scan'
    Default = True
    ModalResult = 2
    TabOrder = 0
    OnClick = btnScanClick
  end
  object btnApply: TButton
    Left = 104
    Top = 320
    Width = 81
    Height = 25
    Caption = '&Apply'
    TabOrder = 1
    OnClick = btnApplyClick
  end
  object btnClose: TButton
    Left = 200
    Top = 320
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Close'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCloseClick
  end
  object PageControl: TPageControl
    Left = 8
    Top = 0
    Width = 273
    Height = 313
    ActivePage = tsSelect
    HotTrack = True
    TabOrder = 3
    OnChange = PageControlChange
    object tsSelect: TTabSheet
      Caption = ' Source '
      object gbSource: TGroupBox
        Left = 8
        Top = 8
        Width = 249
        Height = 129
        Caption = 'Default Source'
        TabOrder = 0
        object lProductVer: TLabel
          Left = 16
          Top = 80
          Width = 77
          Height = 13
          Caption = 'Product version:'
        end
        object lProdVerValue: TLabel
          Left = 104
          Top = 80
          Width = 9
          Height = 13
          Caption = ' - '
        end
        object lManufacturer: TLabel
          Left = 16
          Top = 56
          Width = 66
          Height = 13
          Caption = 'Manufacturer:'
        end
        object lManufactValue: TLabel
          Left = 104
          Top = 56
          Width = 9
          Height = 13
          Caption = ' - '
        end
        object lProtocolVer: TLabel
          Left = 16
          Top = 104
          Width = 79
          Height = 13
          Caption = 'Protocol version:'
        end
        object lProtVerValue: TLabel
          Left = 104
          Top = 104
          Width = 9
          Height = 13
          Caption = ' - '
        end
        object cbSource: TComboBox
          Left = 16
          Top = 24
          Width = 217
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbSourceChange
          OnDropDown = cbSourceDropDown
        end
      end
      object gbSettings: TGroupBox
        Left = 8
        Top = 144
        Width = 249
        Height = 129
        Caption = 'Settings'
        TabOrder = 1
        object lPredefine: TLabel
          Left = 16
          Top = 64
          Width = 54
          Height = 13
          Caption = '&Predefined:'
          FocusControl = cbPredefName
        end
        object cbDriverSetting: TCheckBox
          Left = 16
          Top = 24
          Width = 145
          Height = 17
          Caption = '&Use driver'#39's own settings'
          TabOrder = 0
          OnClick = cbDriverSettingClick
        end
        object cbPredefName: TComboBox
          Left = 80
          Top = 56
          Width = 153
          Height = 21
          Hint = 'Enter a name to store selection as a predefined setting.'
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          Sorted = True
          TabOrder = 1
          OnChange = cbPredefNameChange
        end
        object btnAdd: TButton
          Left = 80
          Top = 88
          Width = 75
          Height = 25
          Caption = 'A&dd'
          TabOrder = 2
          OnClick = btnAddClick
        end
        object btnDelete: TButton
          Left = 160
          Top = 88
          Width = 75
          Height = 25
          Caption = 'D&elete'
          TabOrder = 3
          OnClick = btnDeleteClick
        end
      end
    end
    object tsTransfer: TTabSheet
      Caption = ' Transfer '
      object gbTransfer: TGroupBox
        Left = 8
        Top = 8
        Width = 249
        Height = 129
        Caption = 'Transfer'
        TabOrder = 0
        object lMechanism: TLabel
          Left = 16
          Top = 32
          Width = 57
          Height = 13
          Caption = '&Mechanism:'
          FocusControl = cbTransfer
        end
        object cbTransfer: TComboBox
          Left = 80
          Top = 24
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'Native'
            'File'
            'Memory')
          TabOrder = 0
          OnChange = cbTransferChange
        end
        object cbDisableAfter: TCheckBox
          Left = 16
          Top = 64
          Width = 169
          Height = 17
          Caption = '&Disable after acquire'
          State = cbChecked
          TabOrder = 1
          OnClick = cbDisableAfterClick
        end
        object cbIndicators: TCheckBox
          Left = 16
          Top = 88
          Width = 169
          Height = 17
          Caption = 'S&how scanning indicators'
          State = cbChecked
          TabOrder = 2
          OnClick = cbIndicatorsClick
        end
      end
      object gbFile: TGroupBox
        Left = 8
        Top = 152
        Width = 249
        Height = 121
        Caption = 'File'
        TabOrder = 1
        object lFileName: TLabel
          Left = 16
          Top = 40
          Width = 48
          Height = 13
          Caption = 'File &name:'
          FocusControl = eFileName
        end
        object lFileFormat: TLabel
          Left = 16
          Top = 72
          Width = 51
          Height = 13
          Caption = 'File &format:'
          FocusControl = cbFileFormat
        end
        object eFileName: TEdit
          Left = 80
          Top = 32
          Width = 121
          Height = 21
          AutoSize = False
          TabOrder = 0
          Text = '.\MyFileName.tif'
          OnChange = eFileNameChange
        end
        object btnPathSelect: TButton
          Left = 212
          Top = 32
          Width = 21
          Height = 21
          Hint = 'Select file name'
          Caption = '...'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btnPathSelectClick
        end
        object cbFileFormat: TComboBox
          Left = 80
          Top = 64
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 2
          OnChange = cbFileFormatChange
        end
      end
    end
    object tsLayout: TTabSheet
      Caption = ' Layout '
      object gbSpatialRes: TGroupBox
        Left = 8
        Top = 8
        Width = 249
        Height = 153
        Caption = 'Resolution'
        TabOrder = 0
        object lUnit: TLabel
          Left = 16
          Top = 32
          Width = 22
          Height = 13
          Caption = '&Unit:'
          FocusControl = cbUnit
        end
        object lHoriz: TLabel
          Left = 16
          Top = 64
          Width = 50
          Height = 13
          Caption = '&Horizontal:'
          FocusControl = rsHoriz
        end
        object lVert: TLabel
          Left = 16
          Top = 96
          Width = 38
          Height = 13
          Caption = '&Vertical:'
          FocusControl = rsVert
        end
        object cbUnit: TComboBox
          Left = 80
          Top = 24
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'Inch'
            'Centimeter'
            'Picas'
            'Points'
            'Twips'
            'Pixel')
          TabOrder = 0
          OnChange = cbUnitChange
        end
        object cbAspect: TCheckBox
          Left = 16
          Top = 120
          Width = 113
          Height = 17
          Caption = '&Keep aspect ratio'
          State = cbChecked
          TabOrder = 3
          OnClick = cbAspectClick
        end
        object cbHoriz: TComboBox
          Left = 80
          Top = 56
          Width = 81
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 4
          Visible = False
          OnChange = cbHorizChange
        end
        object cbVert: TComboBox
          Left = 80
          Top = 88
          Width = 81
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 5
          Visible = False
          OnChange = cbVertChange
        end
        object rsHoriz: TmcmRealSpin
          Left = 80
          Top = 56
          Width = 73
          Height = 22
          Hint = 'min..max'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnChange = rsHorizChange
          Value = 300.000000000000000000
          Decimals = 2
          Increment = 1.000000000000000000
        end
        object rsVert: TmcmRealSpin
          Left = 80
          Top = 88
          Width = 73
          Height = 22
          Hint = 'min..max'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnChange = rsVertChange
          Value = 300.000000000000000000
          Decimals = 2
          Increment = 1.000000000000000000
        end
      end
      object gbAOI: TGroupBox
        Left = 8
        Top = 168
        Width = 249
        Height = 105
        Caption = 'AOI'
        TabOrder = 1
        object lLeft: TLabel
          Left = 16
          Top = 36
          Width = 21
          Height = 13
          Caption = '&Left:'
        end
        object lTop: TLabel
          Left = 136
          Top = 36
          Width = 22
          Height = 13
          Caption = 'To&p:'
        end
        object lRight: TLabel
          Left = 16
          Top = 68
          Width = 28
          Height = 13
          Caption = '&Right:'
        end
        object lBottom: TLabel
          Left = 136
          Top = 68
          Width = 36
          Height = 13
          Caption = '&Bottom:'
        end
        object rsLeft: TmcmRealSpin
          Left = 56
          Top = 27
          Width = 57
          Height = 22
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Value = 1.000000000000000000
          Decimals = 1
          Increment = 1.000000000000000000
        end
        object rsTop: TmcmRealSpin
          Left = 176
          Top = 27
          Width = 57
          Height = 22
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Value = 1.000000000000000000
          Decimals = 2
          Increment = 1.000000000000000000
        end
        object rsRight: TmcmRealSpin
          Left = 56
          Top = 59
          Width = 57
          Height = 22
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          Value = 1.000000000000000000
          Decimals = 2
          Increment = 1.000000000000000000
        end
        object rsBottom: TmcmRealSpin
          Left = 176
          Top = 59
          Width = 57
          Height = 22
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Value = 1.000000000000000000
          Decimals = 2
          Increment = 1.000000000000000000
        end
      end
    end
    object tsOrientation: TTabSheet
      Caption = ' Orientation '
      object gbOrientation: TGroupBox
        Left = 8
        Top = 8
        Width = 249
        Height = 161
        Caption = 'Orientation'
        TabOrder = 0
        object sb0degree: TSpeedButton
          Left = 16
          Top = 24
          Width = 25
          Height = 25
          GroupIndex = 10
          Flat = True
          Glyph.Data = {
            76020000424D7602000000000000760000002800000040000000100000000100
            0400000000000002000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777744447774444
            4477788887778888887774444777444444777444477744444477778477777844
            877777F877777F88F77777847777784487777784777778448777777447777844
            7777777887777F88777777744777784477777774477778447777777847777448
            7777777F8777788F777777784777744877777778477774487777777744444447
            7777777788888887777777774444444777777777444444477777777784774487
            77777777F87788F7777777778477448777777777847744877777777774474477
            7777777778878877777777777447447777777777744744777777777778444877
            777777777F888F77777777777844487777777777784448777777777777444777
            7777777777888777777777777744477777777777774447777777777777848777
            7777777777F8F777777777777784877777777777778487777777777777747777
            7777777777787777777777777774777777777777777477777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777}
          NumGlyphs = 4
          OnClick = sbDegreeClick
        end
        object sb90degree: TSpeedButton
          Tag = 90
          Left = 16
          Top = 56
          Width = 25
          Height = 25
          GroupIndex = 10
          Flat = True
          Glyph.Data = {
            76020000424D7602000000000000760000002800000040000000100000000100
            0400000000000002000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            4777777777777777877777777777777747777777777777774777777777777778
            477777777777777F877777777777777847777777777777784777777777777844
            4777777777777F88877777777777784447777777777778444777777777784447
            47777777777F8887877777777778444747777777777844474777777778444777
            777777777F888777777777777844477777777777784447777777777844474777
            7777777F88878777777777784447477777777778444747777777774444774777
            7777778888778777777777444477477777777744447747777777777844444777
            4777777F88888777877777784444477747777778444447774777777778444488
            477777777F8888FF877777777844448847777777784444884777777777784444
            47777777777F8888877777777778444447777777777844444777777777777844
            4777777777777F88877777777777784447777777777778444777777777777778
            477777777777777F877777777777777847777777777777784777777777777777
            4777777777777777877777777777777747777777777777774777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777}
          NumGlyphs = 4
          OnClick = sbDegreeClick
        end
        object sb180degree: TSpeedButton
          Tag = 180
          Left = 16
          Top = 88
          Width = 25
          Height = 25
          GroupIndex = 10
          Flat = True
          Glyph.Data = {
            76020000424D7602000000000000760000002800000040000000100000000100
            0400000000000002000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777747777
            7777777777787777777777777774777777777777777477777777777777848777
            7777777777F8F777777777777784877777777777778487777777777777444777
            7777777777888777777777777744477777777777774447777777777778444877
            777777777F888F77777777777844487777777777784448777777777774474477
            7777777778878877777777777447447777777777744744777777777784774487
            77777777F87788F7777777778477448777777777847744877777777744444447
            7777777788888887777777774444444777777777444444477777777847777448
            7777777F8777788F777777784777744877777778477774487777777447777844
            7777777887777F88777777744777784477777774477778447777778477777844
            877777F877777F88F77777847777784487777784777778448777744447774444
            4477788887778888887774444777444444777444477744444477777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777}
          NumGlyphs = 4
          OnClick = sbDegreeClick
        end
        object sb270degree: TSpeedButton
          Tag = 270
          Left = 16
          Top = 120
          Width = 25
          Height = 25
          GroupIndex = 10
          Flat = True
          Glyph.Data = {
            76020000424D7602000000000000760000002800000040000000100000000100
            0400000000000002000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777477777777
            7777777877777777777777747777777777777774777777777777777487777777
            77777778F7777777777777748777777777777774877777777777777444877777
            7777777888F77777777777744487777777777774448777777777777444448777
            777777788888F777777777744444877777777774444487777777777488444487
            77777778FF8888F7777777748844448777777774884444877777777477744444
            8777777877788888F77777747774444487777774777444448777777777747744
            4477777777787788887777777774774444777777777477444477777777747444
            8777777777787888F77777777774744487777777777474448777777777744487
            77777777777888F7777777777774448777777777777444877777777474448777
            777777787888F777777777747444877777777774744487777777777444877777
            7777777888F77777777777744487777777777774448777777777777487777777
            77777778F7777777777777748777777777777774877777777777777477777777
            7777777877777777777777747777777777777774777777777777777777777777
            7777777777777777777777777777777777777777777777777777}
          NumGlyphs = 4
          OnClick = sbDegreeClick
        end
        object l0degree: TLabel
          Left = 72
          Top = 32
          Width = 84
          Height = 13
          Caption = '0 degree (Portrait)'
        end
        object l90degree: TLabel
          Left = 72
          Top = 64
          Width = 48
          Height = 13
          Caption = '90 degree'
        end
        object l180degree: TLabel
          Left = 72
          Top = 96
          Width = 54
          Height = 13
          Caption = '180 degree'
        end
        object l270degree: TLabel
          Left = 72
          Top = 128
          Width = 116
          Height = 13
          Caption = '270 degree (Landscape)'
        end
      end
      object gbRotation: TGroupBox
        Left = 8
        Top = 184
        Width = 249
        Height = 73
        Caption = 'Rotation'
        TabOrder = 1
        object lDegree: TLabel
          Left = 16
          Top = 40
          Width = 43
          Height = 13
          Caption = '&Degrees:'
        end
        object cbRotation: TComboBox
          Left = 80
          Top = 32
          Width = 73
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 0
          Visible = False
          OnChange = cbRotationChange
        end
        object seRotation: TmcmRealSpin
          Left = 80
          Top = 32
          Width = 65
          Height = 22
          TabOrder = 1
          Value = 1.000000000000000000
          Decimals = 0
          Increment = 1.000000000000000000
        end
      end
    end
    object tsColor: TTabSheet
      Caption = ' Color '
      object gbColor: TGroupBox
        Left = 8
        Top = 8
        Width = 249
        Height = 265
        Caption = 'Color Settings'
        TabOrder = 0
        object lColorModel: TLabel
          Left = 16
          Top = 32
          Width = 27
          Height = 13
          Caption = 'Color:'
          FocusControl = cbPixelType
        end
        object cbPixelType: TComboBox
          Left = 80
          Top = 24
          Width = 153
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'Blacb & White'
            'Color 256'
            'Gray 256'
            'True Color 16.7 mill')
          Sorted = True
          TabOrder = 0
        end
      end
    end
  end
  object SaveDialogTwn: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist]
    Left = 180
    Top = 314
  end
end
