object DBGridTreeViewDemo_MainForm: TDBGridTreeViewDemo_MainForm
  Left = 188
  Top = 214
  Width = 962
  Height = 524
  Caption = 'DBGridTreeView and TreeDataset DEMO  (Delphi 6 Application)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    954
    490)
  PixelsPerInch = 96
  TextHeight = 13
  object dbgtvPopulation: TDBGridTreeView
    Left = 0
    Top = 200
    Width = 784
    Height = 186
    OnPopupListDropDown = dbgtvPopulationPopupListDropDown
    OnPopupListCloseUp = dbgtvPopulationPopupListCloseUp
    Anchors = []
    DataSource = dsPopulation
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnColExit = dbgtvPopulationColExit
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Width = 32
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LastName'
        Title.Caption = 'Last name'
        Width = 109
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FirstName'
        Title.Caption = 'First name'
        Width = 65
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'Sex'
        PickList.Strings = (
          'Female'
          'Male')
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'BirthDate'
        Title.Alignment = taCenter
        Title.Caption = 'Birth date'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BirthPlace'
        Title.Caption = 'Birth place'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Mother'
        Width = 180
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Father'
        Width = 170
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ResidencePlace'
        Title.Caption = 'Residence place'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DeathDate'
        Title.Caption = 'Death date'
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 3
    Top = 4
    Width = 781
    Height = 192
    TabOrder = 1
    object Bevel2: TBevel
      Left = 237
      Top = 8
      Width = 4
      Height = 178
      Style = bsRaised
    end
    object Bevel4: TBevel
      Left = 393
      Top = 7
      Width = 3
      Height = 179
      Style = bsRaised
    end
    object Label35: TLabel
      Left = 3
      Top = 165
      Width = 32
      Height = 16
      Caption = 'Color'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label36: TLabel
      Left = 243
      Top = 15
      Width = 150
      Height = 16
      Caption = 'TreeView parameters'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object Label26: TLabel
      Left = 245
      Top = 37
      Width = 66
      Height = 13
      Caption = 'ButtonStyle'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label27: TLabel
      Left = 245
      Top = 90
      Width = 53
      Height = 13
      Caption = 'LineStyle'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label28: TLabel
      Left = 245
      Top = 139
      Width = 96
      Height = 13
      Caption = 'TreeColumnAlign'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Bevel5: TBevel
      Left = 398
      Top = 161
      Width = 297
      Height = 1
      Style = bsRaised
    end
    object Label41: TLabel
      Left = 418
      Top = 45
      Width = 20
      Height = 13
      Caption = 'sex'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label42: TLabel
      Left = 520
      Top = 45
      Width = 56
      Height = 13
      Caption = 'first name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label45: TLabel
      Left = 667
      Top = 45
      Width = 55
      Height = 11
      AutoSize = False
      Caption = 'last name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label48: TLabel
      Left = 421
      Top = 82
      Width = 55
      Height = 13
      Caption = 'birth date'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label44: TLabel
      Left = 420
      Top = 119
      Width = 61
      Height = 13
      Caption = 'birth place'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label49: TLabel
      Left = 613
      Top = 119
      Width = 91
      Height = 13
      Caption = 'residence place'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LBirthDateRangeSpanSign: TLabel
      Left = 489
      Top = 97
      Width = 9
      Height = 16
      Caption = #247
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label52: TLabel
      Left = 614
      Top = 82
      Width = 62
      Height = 13
      Caption = 'death date'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LDeathDateRangeSpanSign: TLabel
      Left = 681
      Top = 97
      Width = 9
      Height = 16
      Caption = #247
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LFilteredRecordHandling: TLabel
      Left = 467
      Top = 5
      Width = 132
      Height = 13
      Caption = 'filtered record handling'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label30: TLabel
      Left = 414
      Top = 166
      Width = 130
      Height = 16
      Caption = 'Mother as parent view'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label31: TLabel
      Left = 567
      Top = 166
      Width = 127
      Height = 16
      Caption = 'Father as parent view'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 6
      Top = 4
      Width = 150
      Height = 16
      Caption = 'Standard grid options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object Label2: TLabel
      Left = 138
      Top = 149
      Width = 66
      Height = 16
      Caption = 'Fixed color'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LBirthDateRange: TLabel
      Left = 479
      Top = 82
      Width = 33
      Height = 13
      Caption = 'range'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LDeathDateRange: TLabel
      Left = 679
      Top = 82
      Width = 33
      Height = 13
      Caption = 'range'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lID: TLabel
      Left = 552
      Top = 6
      Width = 14
      Height = 13
      Caption = 'ID'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object acbGridColor: TAdvColorBox
      Left = 37
      Top = 164
      Width = 98
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      OnChange = acbGridColorChange
      AutoSize = False
      ButtonWidth = 17
      SelectedColor = clWindow
    end
    object acbButtonStyle: TAdvComboBox
      Left = 245
      Top = 51
      Width = 143
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      ButtonWidth = 17
      Alignment = taLeftJustify
      Required = True
      Items.Strings = (
        'bsRectangle'
        'bsTriangle1'
        'bsTriangle2')
      Sorted = False
      Style = csDropDownList
      OnItemIndexChanged = acbButtonStyleItemIndexChanged
      ItemIndex = -1
      Text = 'acbButtonStyle'
    end
    object acbLineStyle: TAdvComboBox
      Left = 245
      Top = 103
      Width = 143
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      ButtonWidth = 17
      Alignment = taLeftJustify
      Required = True
      Items.Strings = (
        'lsDotted'
        'lsSolid')
      Sorted = False
      Style = csDropDownList
      OnItemIndexChanged = acbLineStyleItemIndexChanged
      ItemIndex = -1
      Text = 'acbLineStyle'
    end
    object acbTreeColumnAlign: TAdvComboBox
      Left = 245
      Top = 154
      Width = 143
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 13
      ButtonWidth = 17
      Alignment = taLeftJustify
      Required = True
      Items.Strings = (
        'tcaAlignToTree'
        'tcaColumnSetting')
      Sorted = False
      Style = csDropDownList
      OnItemIndexChanged = acbTreeColumnAlignItemIndexChanged
      ItemIndex = -1
      Text = 'acbTreeColumnAlign'
    end
    object rbParent_as_Mother_View: TRadioButton
      Left = 399
      Top = 166
      Width = 14
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 35
      OnClick = rbParent_as_Mother_ViewClick
    end
    object rbParent_as_Father_View: TRadioButton
      Left = 552
      Top = 166
      Width = 14
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 36
      OnClick = rbParent_as_Father_ViewClick
    end
    object acbSex: TAdvComboBox
      Left = 403
      Top = 58
      Width = 72
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 16
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      Required = True
      Items.Strings = (
        'Female'
        'Male')
      Sorted = False
      Style = csDropDownList
      ItemIndex = 0
      Text = 'Female'
    end
    object adblcbFirstName: TAdvDBLookupComboBox
      Left = 503
      Top = 58
      Width = 124
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 18
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      EditingOptions = []
      KeyField = 'ID'
      ListField = 'Name'
      ListFieldIndex = 0
      ListSource = dsFrstNames
      NullValueKey = 0
    end
    object adblcbLastName: TAdvDBLookupComboBox
      Left = 650
      Top = 58
      Width = 124
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 20
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      EditingOptions = []
      KeyField = 'ID'
      ListField = 'Name'
      ListFieldIndex = 0
      ListSource = dsLastNames
      NullValueKey = 0
    end
    object adteBirthDateTo: TAdvDateTimeEdit
      Left = 497
      Top = 96
      Width = 87
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 23
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      DateFormat = dfLocaleDefault
      FocusNextControlOnMouseAcceptClosUp = False
    end
    object adteBirthDateFrom: TAdvDateTimeEdit
      Left = 403
      Top = 96
      Width = 87
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 22
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      DateFormat = dfLocaleDefault
      FocusNextControlOnMouseAcceptClosUp = False
    end
    object adblcbBirthPlace: TAdvDBLookupComboBox
      Left = 403
      Top = 133
      Width = 182
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 28
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      EditingOptions = []
      KeyField = 'ID'
      ListField = 'Town_Country'
      ListFieldIndex = 0
      ListSource = dsTowns
      NullValueKey = 0
    end
    object adblcbResidencePlace: TAdvDBLookupComboBox
      Left = 596
      Top = 133
      Width = 179
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 30
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      EditingOptions = []
      KeyField = 'ID'
      ListField = 'Town_Country'
      ListFieldIndex = 0
      ListSource = dsTowns
      NullValueKey = 0
    end
    object adteDeathDateTo: TAdvDateTimeEdit
      Left = 689
      Top = 96
      Width = 87
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 26
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      DateFormat = dfLocaleDefault
      FocusNextControlOnMouseAcceptClosUp = False
    end
    object adteDeathDateFrom: TAdvDateTimeEdit
      Left = 595
      Top = 96
      Width = 87
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 25
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      DateFormat = dfLocaleDefault
      FocusNextControlOnMouseAcceptClosUp = False
    end
    object chbDeathDateRange: TCheckBox
      Left = 596
      Top = 83
      Width = 12
      Height = 12
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 24
      OnClick = chbOtherFieldsClick
    end
    object acbFilteredRecordHandling: TAdvComboBox
      Left = 467
      Top = 19
      Width = 217
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 31
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      Required = True
      Items.Strings = (
        'frhAttachDescendandToNearestAncestor'
        'frhExcludeDescendandsFromTreeView')
      Sorted = False
      Style = csDropDownList
      OnItemIndexChanged = acbFilteredRecordHandlingItemIndexChanged
      ItemIndex = -1
      Text = 'acbFilteredRecordHandling'
    end
    object chbBirthDateRange: TCheckBox
      Left = 404
      Top = 83
      Width = 12
      Height = 12
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 21
      OnClick = chbOtherFieldsClick
    end
    object bApplyFilter: TButton
      Left = 685
      Top = 6
      Width = 88
      Height = 19
      Caption = 'apply FILTER'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 32
      OnClick = bApplyFilterClick
    end
    object bCancelFilter: TButton
      Left = 686
      Top = 25
      Width = 88
      Height = 19
      Caption = 'cancel FILTER'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 33
      OnClick = bCancelFilterClick
    end
    object bLocateNode: TButton
      Left = 685
      Top = 15
      Width = 88
      Height = 21
      Caption = 'Locate node'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 34
      OnClick = bLocateNodeClick
    end
    object chbSex: TCheckBox
      Left = 404
      Top = 45
      Width = 12
      Height = 12
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 15
      OnClick = chbOtherFieldsClick
    end
    object chbFirstName: TCheckBox
      Left = 504
      Top = 45
      Width = 12
      Height = 12
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 17
      OnClick = chbOtherFieldsClick
    end
    object chbLastName: TCheckBox
      Left = 651
      Top = 45
      Width = 12
      Height = 12
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 19
      OnClick = chbOtherFieldsClick
    end
    object chbBirthPlace: TCheckBox
      Left = 404
      Top = 120
      Width = 12
      Height = 12
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 27
      OnClick = chbOtherFieldsClick
    end
    object chbResidencePlace: TCheckBox
      Left = 597
      Top = 120
      Width = 12
      Height = 12
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 29
      OnClick = chbOtherFieldsClick
    end
    object bFullExpand: TButton
      Left = 700
      Top = 159
      Width = 75
      Height = 14
      Caption = 'full expand'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 37
      OnClick = bFullExpandClick
    end
    object bFullCollapse: TButton
      Left = 700
      Top = 173
      Width = 75
      Height = 14
      Caption = 'full collapse'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clBlack
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 38
      OnClick = bFullCollapseClick
    end
    object cb_dgAlwaysShowEditor: TCheckBox
      Left = 7
      Top = 54
      Width = 169
      Height = 17
      Caption = 'dgAlwaysShowEditor'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = cb_dgAlwaysShowEditorClick
    end
    object cb_dgIndicator: TCheckBox
      Left = 7
      Top = 24
      Width = 169
      Height = 16
      Caption = 'dgIndicator'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = cb_dgIndicatorClick
    end
    object cb_dgTitles: TCheckBox
      Left = 7
      Top = 39
      Width = 169
      Height = 17
      Caption = 'dgTitles'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = cb_dgTitlesClick
    end
    object cb_dgAlwaysShowSelection: TCheckBox
      Left = 7
      Top = 69
      Width = 169
      Height = 17
      Caption = 'dgAlwaysShowSelection'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = cb_dgAlwaysShowSelectionClick
    end
    object dg_dgColumnResize: TCheckBox
      Left = 7
      Top = 84
      Width = 169
      Height = 17
      Caption = 'dgColumnResize'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnClick = dg_dgColumnResizeClick
    end
    object cb_dgColLines: TCheckBox
      Left = 7
      Top = 99
      Width = 169
      Height = 17
      Caption = 'dgColLines'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = cb_dgColLinesClick
    end
    object cb_dgRowLines: TCheckBox
      Left = 7
      Top = 114
      Width = 169
      Height = 17
      Caption = 'dgRowLines'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      OnClick = cb_dgRowLinesClick
    end
    object cb_dgRowSelect: TCheckBox
      Left = 7
      Top = 129
      Width = 169
      Height = 17
      Caption = 'dgRowSelect'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 7
      OnClick = cb_dgRowSelectClick
    end
    object cb_dgMultiSelect: TCheckBox
      Left = 7
      Top = 144
      Width = 107
      Height = 17
      Caption = 'dgMultiSelect'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 8
      OnClick = cb_dgMultiSelectClick
    end
    object acbFixedColor: TAdvColorBox
      Left = 136
      Top = 164
      Width = 98
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      OnChange = acbFixedColorChange
      AutoSize = False
      ButtonWidth = 17
      SelectedColor = clBtnFace
    end
    object chbID: TCheckBox
      Left = 537
      Top = 6
      Width = 12
      Height = 12
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 39
      OnClick = chbIDClick
    end
    object aneID: TAdvNumericEdit
      Left = 537
      Top = 19
      Width = 74
      Height = 20
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 40
      AutoSize = False
      ButtonWidth = 17
      Rounding = rmCustomDefined
      FocusNextControlOnMouseAcceptClosUp = False
      DropDownEnabled = False
    end
    object acbFilter_or_Locate: TAdvComboBox
      Left = 400
      Top = 15
      Width = 62
      Height = 24
      FontColorWhenFocused = clBlue
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 14
      AutoSize = False
      ButtonWidth = 12
      Alignment = taLeftJustify
      Required = True
      Items.Strings = (
        'Filter'
        'Locate')
      Sorted = False
      Style = csDropDownList
      OnItemIndexChanged = acbFilter_or_LocateItemIndexChanged
      ItemIndex = 0
      Text = 'Filter'
    end
  end
  object dsPopulation: TDataSource
    DataSet = tdsPopulation
    Left = 8
    Top = 400
  end
  object XPManifest1: TXPManifest
    Left = 792
  end
  object mdsCountries: TMemoryDataset
    ReadOnly = False
    Left = 8
    Top = 368
    object mdsCountriesID: TSmallintField
      FieldName = 'ID'
    end
    object mdsCountriesName: TStringField
      FieldName = 'Name'
      Size = 45
    end
  end
  object mdsFirstNames: TMemoryDataset
    ReadOnly = False
    Left = 8
    Top = 337
    object mdsFirstNamesID: TSmallintField
      FieldName = 'ID'
    end
    object mdsFirstNamesName: TStringField
      FieldName = 'Name'
      Size = 30
    end
    object mdsFirstNamesSex: TStringField
      FieldName = 'Sex'
      Size = 1
    end
  end
  object mdsLastNames: TMemoryDataset
    ReadOnly = False
    Left = 40
    Top = 337
    object mdsLastNamesID: TIntegerField
      FieldName = 'ID'
    end
    object mdsLastNamesName: TStringField
      FieldName = 'Name'
      Size = 40
    end
  end
  object mdsPopulation: TMemoryDataset
    OnCalcFields = mdsPopulationCalcFields
    ProviderOptions = [poLoadDataOnOpen, poConstraints, poOrigNamesInCustomConstr, poCascadeUpdates]
    ReadOnly = False
    StorageModel = smFixedFieldsOffsets
    Left = 72
    Top = 400
    object AutoIncField1: TAutoIncField
      FieldName = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object StringField3: TStringField
      FieldName = 'Sex'
      Size = 1
    end
    object SmallintField2: TSmallintField
      FieldName = 'FirstName_ID'
    end
    object IntegerField1: TIntegerField
      FieldName = 'LastName_ID'
    end
    object DateField2: TDateField
      FieldName = 'BirthDate'
    end
    object IntegerField6: TIntegerField
      FieldName = 'BirthPlace_ID'
    end
    object IntegerField3: TIntegerField
      FieldName = 'Mother_ID'
    end
    object IntegerField5: TIntegerField
      FieldName = 'Father_ID'
    end
    object mdsPopulationResidencePlace_ID: TIntegerField
      FieldName = 'ResidencePlace_ID'
    end
    object DateField3: TDateField
      FieldName = 'DeathDate'
    end
    object mdsPopulationLast_and_first_name: TStringField
      FieldKind = fkCalculated
      FieldName = 'Last_and_first_name'
      Size = 35
      Calculated = True
    end
  end
  object dsTowns: TDataSource
    DataSet = mdsTowns
    Left = 72
    Top = 368
  end
  object dsFrstNames: TDataSource
    DataSet = mdsFirstNames
    Left = 8
    Top = 305
  end
  object dsLastNames: TDataSource
    DataSet = mdsLastNames
    Left = 40
    Top = 305
  end
  object tdsPopulation: TTreeDataset
    BeforePost = tdsPopulationBeforePost
    BeforeDelete = tdsPopulationBeforeDelete
    DefaultSrcDataset = mdsPopulation
    ProviderOptions = [poLoadDataOnOpen, poConstraints, poOrigNamesInCustomConstr, poCascadeUpdates]
    ReadOnly = False
    Expanded = False
    TreeParams.KeyField = 'ID'
    TreeParams.ParentField = 'Mother_ID'
    TreeParams.RootAsNode = True
    TreeParams.RootValue = Null
    TreeParams.TreeBuildMethod = tbmParentFieldSort
    Left = 40
    Top = 400
    object AutoIncField3: TAutoIncField
      FieldName = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
    end
    object StringField1: TStringField
      FieldName = 'Sex'
      Required = True
      Size = 1
    end
    object SmallintField3: TSmallintField
      FieldName = 'FirstName_ID'
    end
    object IntegerField7: TIntegerField
      FieldName = 'LastName_ID'
    end
    object DateField4: TDateField
      FieldName = 'BirthDate'
    end
    object IntegerField8: TIntegerField
      FieldName = 'BirthPlace_ID'
    end
    object IntegerField9: TIntegerField
      FieldName = 'Mother_ID'
    end
    object IntegerField10: TIntegerField
      FieldName = 'Father_ID'
    end
    object IntegerField11: TIntegerField
      FieldName = 'ResidencePlace_ID'
    end
    object DateField5: TDateField
      FieldName = 'DeathDate'
    end
    object StringField2: TStringField
      FieldKind = fkLookup
      FieldName = 'FirstName'
      LookupDataSet = mdsFirstNames
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'FirstName_ID'
      Required = True
      Size = 30
      Lookup = True
    end
    object StringField4: TStringField
      FieldKind = fkLookup
      FieldName = 'LastName'
      LookupDataSet = mdsLastNames
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'LastName_ID'
      Required = True
      Size = 40
      Lookup = True
    end
    object StringField5: TStringField
      FieldKind = fkLookup
      FieldName = 'BirthPlace'
      LookupDataSet = mdsTowns
      LookupKeyFields = 'ID'
      LookupResultField = 'Town_Country'
      KeyFields = 'BirthPlace_ID'
      Required = True
      Size = 30
      Lookup = True
    end
    object StringField6: TStringField
      FieldKind = fkLookup
      FieldName = 'ResidencePlace'
      LookupDataSet = mdsTowns
      LookupKeyFields = 'ID'
      LookupResultField = 'Town_Country'
      KeyFields = 'ResidencePlace_ID'
      Required = True
      Size = 30
      Lookup = True
    end
    object StringField7: TStringField
      FieldKind = fkLookup
      FieldName = 'Mother'
      LookupDataSet = mdsPopulation
      LookupKeyFields = 'ID'
      LookupResultField = 'Last_and_first_name'
      KeyFields = 'Mother_ID'
      Size = 35
      Lookup = True
    end
    object tdsPopulationFather: TStringField
      FieldKind = fkLookup
      FieldName = 'Father'
      LookupDataSet = mdsPopulation
      LookupKeyFields = 'ID'
      LookupResultField = 'Last_and_first_name'
      KeyFields = 'Father_ID'
      Size = 35
      Lookup = True
    end
  end
  object mdsTowns: TMemoryDataset
    OnCalcFields = mdsTownsCalcFields
    ReadOnly = False
    Left = 40
    Top = 368
    object IntegerField12: TIntegerField
      FieldName = 'ID'
    end
    object StringField9: TStringField
      FieldName = 'Name'
      Size = 40
    end
    object SmallintField4: TSmallintField
      FieldName = 'Country_ID'
    end
    object StringField10: TStringField
      FieldKind = fkCalculated
      FieldName = 'Town_Country'
      Origin = 'Name'
      Size = 30
      Calculated = True
    end
  end
end
