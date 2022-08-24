object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'TagCloud for VCL - Advanced Demo'
  ClientHeight = 529
  ClientWidth = 845
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 257
    Top = 0
    Width = 8
    Height = 510
    AutoSnap = False
    Beveled = True
    ExplicitLeft = 265
    ExplicitHeight = 511
  end
  object Panel2: TPanel
    Left = 265
    Top = 0
    Width = 580
    Height = 510
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 497
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 580
      Height = 37
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 0
      ExplicitWidth = 497
      DesignSize = (
        580
        37)
      object lbSearch: TLabel
        Left = 407
        Top = 11
        Width = 37
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Search:'
        ExplicitLeft = 318
      end
      object Bevel1: TBevel
        Left = 0
        Top = 35
        Width = 580
        Height = 2
        Align = alBottom
        Shape = bsTopLine
        ExplicitTop = -21
        ExplicitWidth = 491
      end
      object edFilter: TEdit
        Left = 453
        Top = 8
        Width = 121
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 0
        OnChange = edFilterChange
        ExplicitLeft = 370
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 486
      Width = 580
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 1
      ExplicitWidth = 497
      object tsPages: TTabSet
        Left = 0
        Top = 0
        Width = 580
        Height = 24
        Align = alClient
        BackgroundColor = clWindow
        DitherBackground = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        StartMargin = 4
        SelectedColor = clWindow
        Style = tsModernTabs
        Tabs.Strings = (
          ' 1 ')
        TabIndex = 0
        UnselectedColor = clBtnFace
        OnChange = tsPagesChange
        ExplicitWidth = 497
      end
    end
    object ScrollBox: TScrollBox
      Left = 0
      Top = 37
      Width = 580
      Height = 449
      HorzScrollBar.Visible = False
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 2
      ExplicitWidth = 497
      object TagCloud: TTagCloud
        Left = 0
        Top = 0
        Width = 580
        Height = 449
        Align = alClient
        AutoShrinkRows = True
        Color = clWhite
        ColSpacing = 4
        Direction = tcdHorizontal
        FixedColFullFrame = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Calibri'
        Font.Style = []
        HoverColor = clDefault
        HoverCursor = crHandPoint
        HoverStyle = [fsBold, fsUnderline]
        HoverEnlarge = True
        ItemFrame.BackColor = clNone
        ItemFrame.FrameColor = clDefault
        ItemFrame.FrameMargin = 2
        ItemFrame.FrameSize = 0
        ItemFrame.FrameStyle = psSolid
        ItemFrame.RoundedSize = 0
        HoverFrame.BackColor = clNone
        HoverFrame.FrameColor = clDefault
        HoverFrame.FrameMargin = 2
        HoverFrame.FrameSize = 0
        HoverFrame.FrameStyle = psSolid
        HoverFrame.RoundedSize = 0
        SelectedFrame.BackColor = clNone
        SelectedFrame.FrameColor = clDefault
        SelectedFrame.FrameMargin = 0
        SelectedFrame.FrameSize = 0
        SelectedFrame.FrameStyle = psSolid
        SelectedFrame.RoundedSize = 0
        LogScale = True
        MaxFontSize = 42
        Padding.Left = 8
        Padding.Top = 4
        Padding.Right = 8
        Padding.Bottom = 4
        SelectedColor = clNone
        PageIndex = 0
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        RowSpacing = 2
        ShowHint = True
        Sorted = True
        Transparent = True
        OnAdvancedCustomDrawItem = TagCloudAdvancedCustomDrawItem
        OnHoverChange = TagCloudHoverChange
        OnPageCountChanged = TagCloudPageCountChanged
        OnShow = TagCloudShow
        OnTagClick = TagCloudTagClick
        OnTagHint = TagCloudTagHint
        ExplicitTop = -2
        ExplicitWidth = 497
        ExplicitHeight = 443
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 510
    Width = 845
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 212
      end
      item
        Alignment = taCenter
        Width = 256
      end
      item
        Alignment = taCenter
        Width = 112
      end
      item
        Alignment = taCenter
        Width = 112
      end
      item
        Width = 50
      end>
    ExplicitWidth = 762
  end
  object pcMain: TPageControl
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 249
    Height = 502
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = TabSheet3
    Align = alLeft
    DoubleBuffered = False
    MultiLine = True
    ParentDoubleBuffered = False
    TabOrder = 2
    object TabSheet3: TTabSheet
      Caption = 'Testing options'
      ImageIndex = 2
      object Label4: TLabel
        Left = 3
        Top = 115
        Width = 51
        Height = 13
        Caption = 'Alignment:'
        Transparent = True
      end
      object cbLogScale: TCheckBox
        Left = 8
        Top = 13
        Width = 129
        Height = 17
        Caption = 'Logarithmic scale'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = SetOptions
      end
      object cbColorLevels: TCheckBox
        Left = 8
        Top = 48
        Width = 109
        Height = 17
        Caption = 'Use color levels'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbColorLevelsClick
      end
      object edIncr: TEdit
        Left = 138
        Top = 205
        Width = 53
        Height = 21
        TabOrder = 7
        Text = '1'
      end
      object cbIncr: TCheckBox
        Left = 8
        Top = 207
        Width = 129
        Height = 17
        Caption = 'Increment on click:'
        TabOrder = 6
      end
      object cbAutoSize: TCheckBox
        Left = 8
        Top = 154
        Width = 97
        Height = 17
        Caption = 'Auto size'
        TabOrder = 4
        OnClick = SetOptions
      end
      object cbAutoShrinkRows: TCheckBox
        Left = 8
        Top = 177
        Width = 109
        Height = 17
        Caption = 'Auto shrink rows'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = SetOptions
      end
      object cbFixedColWidth: TCheckBox
        Left = 8
        Top = 232
        Width = 129
        Height = 17
        Caption = 'Fixed tag width:'
        Checked = True
        State = cbChecked
        TabOrder = 8
        OnClick = SetOptions
      end
      object edFixedColWidth: TEdit
        Left = 138
        Top = 230
        Width = 53
        Height = 21
        TabOrder = 9
        Text = '224'
      end
      object cbCustomDraw: TCheckBox
        Left = 8
        Top = 264
        Width = 173
        Height = 17
        Caption = 'Custom drawing demo'
        Checked = True
        State = cbChecked
        TabOrder = 10
        OnClick = SetOptions
      end
      object cbAlignment: TComboBox
        Left = 74
        Top = 112
        Width = 87
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 2
        TabOrder = 3
        Text = 'Center'
        OnChange = SetOptions
        Items.Strings = (
          'Left'
          'Right'
          'Center')
      end
      object cbEnabled: TCheckBox
        Left = 8
        Top = 392
        Width = 113
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 12
        OnClick = SetOptions
      end
      object cbOpacity: TCheckBox
        Left = 8
        Top = 71
        Width = 205
        Height = 17
        Caption = 'Simulate opacity with color levels'
        TabOrder = 2
        OnClick = cbColorLevelsClick
      end
      object cbSortByValue: TCheckBox
        Left = 8
        Top = 353
        Width = 183
        Height = 17
        Caption = 'Sort by value'
        TabOrder = 13
        OnClick = cbSortByValueClick
      end
      object cbSpeedSort: TCheckBox
        Left = 8
        Top = 331
        Width = 205
        Height = 17
        Caption = 'Speed sort (UseSpeedSort property)'
        Checked = True
        State = cbChecked
        TabOrder = 14
        OnClick = SetOptions
      end
      object cbNativeFrames: TCheckBox
        Left = 8
        Top = 287
        Width = 205
        Height = 17
        Caption = 'Native frames drawing'
        Checked = True
        State = cbChecked
        TabOrder = 11
        OnClick = SetOptions
      end
    end
    object TabSheet1: TTabSheet
      BorderWidth = 4
      Caption = 'Demo data'
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 233
        Height = 37
        Align = alTop
        AutoSize = False
        Caption = 
          'You can use the following demonstration data   to see in the Tag' +
          'Cloud:'
        Transparent = True
        WordWrap = True
        ExplicitWidth = 231
      end
      object sbLoadCountries: TButton
        Left = 0
        Top = 48
        Width = 65
        Height = 25
        Hint = 'Load TagCloud items from the CSV file (population by countries)'
        Caption = 'Countries'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = sbLoadCountriesClick
      end
      object sbLoadCities: TButton
        Left = 71
        Top = 48
        Width = 65
        Height = 25
        Hint = 'Load TagCloud items from the CSV file (population by cities)'
        Caption = 'Cities'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = sbLoadCitiesClick
      end
      object sbHugeCSV: TButton
        Left = 142
        Top = 48
        Width = 89
        Height = 25
        Caption = '20 000 items'
        TabOrder = 2
        OnClick = sbHugeCSVClick
      end
    end
    object TabSheet2: TTabSheet
      BorderWidth = 4
      Caption = 'Data from text'
      ImageIndex = 1
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 233
        Height = 101
        Align = alTop
        AutoSize = False
        Caption = 
          'The following text can be used to parse and show the frequency o' +
          'f words in the TagCloud component.'#13#10#13#10'Paste any text into this f' +
          'ield and click "Parse".'
        Transparent = True
        WordWrap = True
        ExplicitWidth = 241
      end
      object mText: TMemo
        Left = 0
        Top = 101
        Width = 233
        Height = 196
        Align = alClient
        Lines.Strings = (
          'TagCloud is our VCL implementation of a '
          'well '
          'known "tag cloud" navigation element, '
          'that '
          'is widely used in an internet environment. '
          'It '
          'has all the common features for easy '
          'presentation of the list of data to the '
          'user, '
          'that you can expect from this kind of '
          'visual '
          'representation. '
          ''
          'TagCloud for VCL is designed for all Delphi '
          'versions from 5 to 2010. So if you want to '
          'give the users of your desktop '
          'applications '
          'the same features of navigation, that are '
          'familiar from the web environment, try this '
          'component. '
          ''
          ''
          'Data visualization in a tag cloud can be '
          'really useful in any information system, '
          'from '
          'a simple databases, over the information '
          'management, after the complex '
          'knowledge '
          'bases. '
          ''
          'Tag cloud navigational features are '
          'applicable to a various kinds of sidebar '
          'panels, and of course, it can be used to '
          'extend the search functionality of your '
          'applications. '
          ''
          'A brief overview of the component '
          'features'
          'Font size and / or the color scaling (custom '
          'color levels)'
          'Support for both linear and logarithmic '
          'scale'
          'Settings for alignment and spacing '
          'between '
          'tags'
          'Tag under cursor can have its own style'
          'Transparent background'
          'Drawing over the Aero Glass effect on the '
          'parent controls'
          'Mouse hover and mouse click events for '
          'tags'
          'Easy tag items definition, both in '
          'design-time and run-time'
          'Loading and saving the tags, by '
          'supporting '
          'the CSV files, TStrings and an array '
          'structure'
          'Manual sorting, or an automatic sorting '
          'when loading the tags'
          'Full source code available'
          ''
          ''
          'TTagCloud component is a descendant of '
          'TGraphicControl and has no other libraries '
          'or third-party component requirements. '
          'Works very fast and without the flickering '
          'in '
          'a double-buffered mode, as well as in the '
          'direct canvas painting mode.')
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Panel5: TPanel
        Left = 0
        Top = 297
        Width = 233
        Height = 169
        Align = alBottom
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        DesignSize = (
          233
          169)
        object Label3: TLabel
          Left = 0
          Top = 15
          Width = 69
          Height = 13
          Caption = 'Except words:'
          Transparent = True
          WordWrap = True
        end
        object sbParse: TButton
          Left = 0
          Top = 138
          Width = 75
          Height = 25
          Hint = 'Parse the above text and show the word frequency in the TagCloud'
          Caption = 'Parse'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = sbParseClick
        end
        object mExceptWords: TMemo
          Left = 0
          Top = 32
          Width = 233
          Height = 100
          Anchors = [akLeft, akTop, akRight]
          Lines.Strings = (
            'a'
            'an'
            'and'
            'are'
            'as'
            'be'
            'can'
            'for'
            'from'
            'has'
            'in'
            'into'
            'of'
            'or'
            'that'
            'the'
            'to'
            'what'
            'where'
            'which')
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object cbLowerCase: TCheckBox
          Left = 96
          Top = 144
          Width = 113
          Height = 17
          Caption = 'Lower case tags'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
      end
    end
  end
end
