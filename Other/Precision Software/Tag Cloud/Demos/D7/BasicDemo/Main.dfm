object frmMain: TfrmMain
  Left = 351
  Top = 141
  Width = 951
  Height = 628
  Caption = 'TagCloud for VCL - Basic Demo'
  Color = 16510953
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object tagCloud: TTagCloud
    Left = 229
    Top = 0
    Width = 485
    Height = 581
    Align = alClient
    AutoShrinkRows = True
    Color = clWhite
    ColSpacing = 8
    Direction = tcdHorizontal
    FixedColFullFrame = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    HoverColor = clDefault
    HoverCursor = crHandPoint
    HoverStyle = [fsUnderline]
    HoverEnlarge = False
    ItemFrame.BackColor = clNone
    ItemFrame.FrameColor = clDefault
    ItemFrame.FrameMargin = 0
    ItemFrame.FrameSize = 0
    ItemFrame.FrameStyle = psSolid
    ItemFrame.RoundedSize = 0
    HoverFrame.BackColor = clNone
    HoverFrame.FrameColor = clDefault
    HoverFrame.FrameMargin = 0
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
    MaxFontSize = 36
    SelectedColor = clDefault
    PageIndex = 0
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    RowSpacing = 4
    ShowHint = True
    Sorted = False
    Transparent = False
    OnHoverChange = tagCloudHoverChange
    OnTagClick = tagCloudTagClick
    OnTagHint = tagCloudTagHint
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 581
    Width = 943
    Height = 20
    Color = 16113363
    Panels = <
      item
        Width = 256
      end>
  end
  object pRight: TPanel
    Left = 714
    Top = 0
    Width = 229
    Height = 581
    Align = alRight
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 1
    object Bevel7: TBevel
      Left = 2
      Top = 468
      Width = 225
      Height = 2
      Align = alTop
      Shape = bsTopLine
    end
    object pHead1: TPanel
      Left = 2
      Top = 2
      Width = 225
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Visual options and scaling'
      Color = 16113363
      TabOrder = 0
      object Bevel2: TBevel
        Left = 0
        Top = 22
        Width = 225
        Height = 2
        Align = alBottom
        Shape = bsTopLine
      end
    end
    object Panel3: TPanel
      Left = 2
      Top = 26
      Width = 225
      Height = 281
      Align = alTop
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      object Bevel6: TBevel
        Left = 0
        Top = 279
        Width = 225
        Height = 2
        Align = alBottom
        Shape = bsTopLine
      end
      object lbFont: TLabel
        Left = 8
        Top = 15
        Width = 55
        Height = 13
        Caption = 'Font name:'
      end
      object Label10: TLabel
        Left = 8
        Top = 41
        Width = 72
        Height = 13
        Caption = 'Basic font size:'
      end
      object Label11: TLabel
        Left = 8
        Top = 66
        Width = 68
        Height = 13
        Caption = 'Max font size:'
      end
      object Label13: TLabel
        Left = 8
        Top = 99
        Width = 51
        Height = 13
        Caption = 'Alignment:'
      end
      object Label14: TLabel
        Left = 8
        Top = 126
        Width = 41
        Height = 13
        Caption = 'Spacing:'
      end
      object Label15: TLabel
        Left = 148
        Top = 128
        Width = 6
        Height = 13
        Caption = 'x'
      end
      object Label16: TLabel
        Left = 8
        Top = 183
        Width = 59
        Height = 13
        Caption = 'Hover style:'
      end
      object cbFont: TComboBox
        Left = 86
        Top = 12
        Width = 131
        Height = 21
        DropDownCount = 24
        ItemHeight = 13
        TabOrder = 0
        OnSelect = cbFontSelect
      end
      object cbFontSize: TComboBox
        Left = 86
        Top = 38
        Width = 59
        Height = 21
        DropDownCount = 24
        ItemHeight = 13
        TabOrder = 1
        Text = '7'
        OnChange = cbFontSizeChange
        Items.Strings = (
          '4'
          '5'
          '6'
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15'
          '16')
      end
      object cbMaxSize: TComboBox
        Left = 86
        Top = 63
        Width = 59
        Height = 21
        DropDownCount = 24
        ItemHeight = 13
        TabOrder = 2
        Text = '36'
        OnChange = cbMaxSizeChange
        Items.Strings = (
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15'
          '16'
          '17'
          '18'
          '19'
          '20'
          '21'
          '22'
          '23'
          '24'
          '25'
          '26'
          '27'
          '28'
          '29'
          '30'
          '31'
          '32'
          '33'
          '34'
          '35'
          '36'
          '37'
          '38'
          '39'
          '40'
          '41'
          '42'
          '43'
          '44')
      end
      object cbLogScale: TCheckBox
        Left = 8
        Top = 254
        Width = 209
        Height = 17
        Caption = 'Logarithmic scale for font sizes'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = cbLogScaleClick
      end
      object sbFont: TButton
        Left = 151
        Top = 36
        Width = 64
        Height = 23
        Caption = 'Font'
        TabOrder = 4
        OnClick = sbFontClick
      end
      object sbBackground: TButton
        Left = 151
        Top = 62
        Width = 64
        Height = 23
        Caption = 'Backgr.'
        TabOrder = 5
        OnClick = sbBackgroundClick
      end
      object cbAlignment: TComboBox
        Left = 86
        Top = 96
        Width = 131
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        OnChange = cbAlignmentChange
        Items.Strings = (
          'Left'
          'Center'
          'Right')
      end
      object edColSpacing: TEdit
        Left = 170
        Top = 123
        Width = 31
        Height = 21
        ReadOnly = True
        TabOrder = 7
        Text = '8'
      end
      object edRowSpacing: TEdit
        Left = 86
        Top = 123
        Width = 31
        Height = 21
        ReadOnly = True
        TabOrder = 8
        Text = '4'
      end
      object udColSpace: TUpDown
        Left = 201
        Top = 123
        Width = 15
        Height = 21
        Associate = edColSpacing
        Position = 8
        TabOrder = 9
        OnChangingEx = udColSpaceChangingEx
      end
      object udRowSpace: TUpDown
        Left = 117
        Top = 123
        Width = 15
        Height = 21
        Associate = edRowSpacing
        Min = -100
        Position = 4
        TabOrder = 10
        OnChangingEx = udRowSpaceChangingEx
      end
      object cbHoverEnlarge: TCheckBox
        Left = 86
        Top = 223
        Width = 67
        Height = 17
        Hint = 'Enlarge the item under cursor'
        Caption = 'Enlarge'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 11
        OnClick = cbHoverEnlargeClick
      end
      object cbHoverBold: TCheckBox
        Left = 86
        Top = 182
        Width = 54
        Height = 17
        Caption = 'Bold'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 12
        OnClick = cbHoverEnlargeClick
      end
      object cbHoverUnderline: TCheckBox
        Left = 146
        Top = 182
        Width = 71
        Height = 17
        Caption = 'Underline'
        Checked = True
        ParentShowHint = False
        ShowHint = False
        State = cbChecked
        TabOrder = 13
        OnClick = cbHoverEnlargeClick
      end
      object cbHoverHandCursor: TCheckBox
        Left = 86
        Top = 203
        Width = 87
        Height = 17
        Caption = 'Hand cursor'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 14
        OnClick = cbHoverEnlargeClick
      end
      object cbAutoShrinkRows: TCheckBox
        Left = 86
        Top = 148
        Width = 127
        Height = 17
        Caption = 'Auto shrink rows'
        Checked = True
        State = cbChecked
        TabOrder = 15
        OnClick = cbAutoShrinkRowsClick
      end
    end
    object Panel2: TPanel
      Left = 2
      Top = 470
      Width = 225
      Height = 106
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 2
      object Label3: TLabel
        Left = 8
        Top = 8
        Width = 209
        Height = 45
        Align = alTop
        AutoSize = False
        Caption = 
          'See the TagCloud component on the form with Aero Glass enabled (' +
          'Windows Vista and later, Delphi 2009 and later)'
        Enabled = False
        WordWrap = True
      end
      object Label12: TLabel
        Left = 8
        Top = 81
        Width = 72
        Height = 13
        Caption = 'Text glow size:'
        Enabled = False
      end
      object cbVistaGlass: TCheckBox
        Left = 8
        Top = 55
        Width = 125
        Height = 17
        Caption = 'Aero glass effect'
        Enabled = False
        TabOrder = 0
      end
      object cbGlow: TComboBox
        Left = 86
        Top = 78
        Width = 73
        Height = 21
        DropDownCount = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 1
        Text = '0'
        OnChange = cbGlowChange
        Items.Strings = (
          '0'
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15'
          '16'
          '17'
          '18'
          '19'
          '20')
      end
    end
    object pColors: TPanel
      Left = 2
      Top = 307
      Width = 225
      Height = 161
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 3
      object Label17: TLabel
        Left = 8
        Top = 8
        Width = 209
        Height = 49
        Align = alTop
        AutoSize = False
        Caption = 
          'This is a simple example of color leveling. In your project you ' +
          'can define as many color levels as you want.'
        WordWrap = True
      end
      object clrbx1: TColorBox
        Left = 86
        Top = 57
        Width = 131
        Height = 22
        Selected = clGray
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
        DropDownCount = 24
        ItemHeight = 16
        TabOrder = 0
        OnSelect = cbClr1Click
      end
      object cbClr1: TCheckBox
        Left = 11
        Top = 59
        Width = 69
        Height = 17
        Caption = '1. level'
        TabOrder = 1
        OnClick = cbClr1Click
      end
      object cbClr2: TCheckBox
        Left = 11
        Top = 83
        Width = 69
        Height = 17
        Caption = '2. level'
        TabOrder = 2
        OnClick = cbClr1Click
      end
      object clrbx2: TColorBox
        Left = 86
        Top = 81
        Width = 131
        Height = 22
        NoneColorColor = clWhite
        Selected = 16751932
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
        DropDownCount = 24
        ItemHeight = 16
        TabOrder = 3
        OnSelect = cbClr1Click
      end
      object cbClr3: TCheckBox
        Left = 11
        Top = 107
        Width = 69
        Height = 17
        Caption = '3. level'
        TabOrder = 4
        OnClick = cbClr1Click
      end
      object clrbx3: TColorBox
        Left = 86
        Top = 105
        Width = 131
        Height = 22
        NoneColorColor = clWhite
        Selected = 33023
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
        DropDownCount = 24
        ItemHeight = 16
        TabOrder = 5
        OnSelect = cbClr1Click
      end
      object cbClr4: TCheckBox
        Left = 11
        Top = 131
        Width = 69
        Height = 17
        Caption = '4. level'
        TabOrder = 6
        OnClick = cbClr1Click
      end
      object clrbx4: TColorBox
        Left = 86
        Top = 129
        Width = 131
        Height = 22
        NoneColorColor = clWhite
        Selected = clRed
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
        DropDownCount = 24
        ItemHeight = 16
        TabOrder = 7
        OnSelect = cbClr1Click
      end
    end
  end
  object pLeft: TPanel
    Left = 0
    Top = 0
    Width = 229
    Height = 581
    Align = alLeft
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 2
    object Panel1: TPanel
      Left = 2
      Top = 26
      Width = 225
      Height = 83
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 209
        Height = 37
        Align = alTop
        AutoSize = False
        Caption = 
          'You can use the following demonstration data to see in the TagCl' +
          'oud:'
        Transparent = False
        WordWrap = True
      end
      object sbLoadCSV: TButton
        Left = 8
        Top = 48
        Width = 65
        Height = 25
        Hint = 'Load TagCloud items from the CSV file (population by countries)'
        Caption = 'Countries'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = sbLoadCSVClick
      end
      object sbTasks: TButton
        Left = 80
        Top = 48
        Width = 65
        Height = 25
        Hint = 'Display the list of PC tasks as TagCloud items (memory usage)'
        Caption = 'PC tasks'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = sbTasksClick
      end
    end
    object pItem: TPanel
      Left = 2
      Top = 109
      Width = 225
      Height = 194
      Align = alTop
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
      object lbSelected: TLabel
        Left = 8
        Top = 12
        Width = 64
        Height = 13
        Caption = 'Selected item'
      end
      object lbCaption: TLabel
        Left = 8
        Top = 42
        Width = 41
        Height = 13
        Caption = 'Caption:'
      end
      object lbValue: TLabel
        Left = 8
        Top = 69
        Width = 30
        Height = 13
        Caption = 'Value:'
      end
      object lbHint: TLabel
        Left = 8
        Top = 96
        Width = 23
        Height = 13
        Caption = 'Hint:'
      end
      object lbTag: TLabel
        Left = 8
        Top = 123
        Width = 22
        Height = 13
        Caption = 'Tag:'
      end
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 225
        Height = 2
        Align = alTop
        Shape = bsTopLine
      end
      object Bevel3: TBevel
        Left = 0
        Top = 192
        Width = 225
        Height = 2
        Align = alBottom
        Shape = bsTopLine
      end
      object edCaption: TEdit
        Left = 61
        Top = 39
        Width = 153
        Height = 21
        TabOrder = 0
        OnChange = edValueChange
      end
      object edHint: TEdit
        Left = 61
        Top = 93
        Width = 153
        Height = 21
        TabOrder = 2
        OnChange = edValueChange
      end
      object edValue: TEdit
        Left = 61
        Top = 66
        Width = 153
        Height = 21
        Hint = 
          'Use up and down arrow keys (try also together with the Shift and' +
          ' Ctrl keys)'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = edValueChange
        OnKeyDown = edValueKeyDown
      end
      object edTag: TEdit
        Left = 61
        Top = 120
        Width = 153
        Height = 21
        TabOrder = 3
        OnChange = edValueChange
      end
      object sbAdd: TButton
        Left = 8
        Top = 156
        Width = 65
        Height = 25
        Hint = 'Add new item'
        Caption = 'New item'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = sbAddClick
      end
      object sbDelete: TButton
        Left = 78
        Top = 156
        Width = 65
        Height = 25
        Hint = 'Delete selected item'
        Caption = 'Delete'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = sbDeleteClick
      end
      object sbClear: TButton
        Left = 149
        Top = 156
        Width = 65
        Height = 25
        Hint = 'Clear all items'
        Caption = 'Clear items'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        OnClick = sbClearClick
      end
    end
    object pImport: TPanel
      Left = 2
      Top = 327
      Width = 225
      Height = 245
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 4
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 209
        Height = 49
        Align = alTop
        AutoSize = False
        Caption = 
          'You can import the TagCloud items from any CSV based file. Defin' +
          'e the options bellow and open your file.'
        Transparent = True
        WordWrap = True
      end
      object lbSep: TLabel
        Left = 8
        Top = 63
        Width = 76
        Height = 13
        Caption = 'Field separator:'
      end
      object Label4: TLabel
        Left = 8
        Top = 90
        Width = 104
        Height = 13
        Caption = 'Max. displayed items:'
      end
      object Label5: TLabel
        Left = 8
        Top = 116
        Width = 131
        Height = 13
        Caption = 'Indexes of fields in the file:'
      end
      object Label6: TLabel
        Left = 8
        Top = 133
        Width = 37
        Height = 13
        Caption = 'Caption'
      end
      object Label7: TLabel
        Left = 61
        Top = 133
        Width = 26
        Height = 13
        Caption = 'Value'
      end
      object Label8: TLabel
        Left = 114
        Top = 133
        Width = 19
        Height = 13
        Caption = 'Hint'
      end
      object Label9: TLabel
        Left = 167
        Top = 133
        Width = 18
        Height = 13
        Caption = 'Tag'
      end
      object sbOpenFile: TButton
        Left = 149
        Top = 211
        Width = 65
        Height = 25
        Hint = 'Load items from file'
        Caption = 'Open file'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
        OnClick = sbOpenFileClick
      end
      object cbAlphaSort: TCheckBox
        Left = 8
        Top = 220
        Width = 85
        Height = 17
        Hint = 'Sort items alphabetically'
        Caption = 'Sort items'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 8
      end
      object cbLowerCase: TCheckBox
        Left = 8
        Top = 200
        Width = 133
        Height = 17
        Hint = 'Convert the item captions to lower case'
        Caption = 'Lower case captions'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
      end
      object cbSkipFirstRow: TCheckBox
        Left = 8
        Top = 180
        Width = 113
        Height = 17
        Hint = 'Skip the first row, because it contains the header information'
        Caption = 'Skip the first row'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
      object edSep: TEdit
        Left = 153
        Top = 60
        Width = 61
        Height = 21
        TabOrder = 0
        Text = ';'
      end
      object edMaxItems: TEdit
        Left = 153
        Top = 87
        Width = 61
        Height = 21
        TabOrder = 1
        Text = '300'
      end
      object edFCaption: TEdit
        Left = 8
        Top = 149
        Width = 47
        Height = 21
        TabOrder = 2
        Text = '0'
      end
      object edFValue: TEdit
        Left = 61
        Top = 149
        Width = 47
        Height = 21
        TabOrder = 3
        Text = '1'
      end
      object edFHint: TEdit
        Left = 114
        Top = 149
        Width = 47
        Height = 21
        TabOrder = 4
      end
      object edFTag: TEdit
        Left = 167
        Top = 149
        Width = 47
        Height = 21
        TabOrder = 5
      end
    end
    object Panel4: TPanel
      Left = 2
      Top = 2
      Width = 225
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Demonstration data'
      Color = 16113363
      TabOrder = 0
      object Bevel4: TBevel
        Left = 0
        Top = 22
        Width = 225
        Height = 2
        Align = alBottom
        Shape = bsTopLine
      end
    end
    object Panel5: TPanel
      Left = 2
      Top = 303
      Width = 225
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Data from file'
      Color = 16113363
      TabOrder = 3
      object Bevel5: TBevel
        Left = 0
        Top = 22
        Width = 225
        Height = 2
        Align = alBottom
        Shape = bsTopLine
      end
    end
  end
  object odFile: TOpenDialog
    Filter = 'Text files (*.csv, *.txt)|*.csv;*.txt|All files (*.*)|*.*'
    Left = 312
    Top = 240
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    OnApply = dlgFontApply
    Left = 400
    Top = 240
  end
  object dlgColor: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 356
    Top = 240
  end
end
