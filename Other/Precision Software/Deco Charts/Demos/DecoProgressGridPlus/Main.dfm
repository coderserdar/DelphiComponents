object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'DecoProgressGridPlus Demo'
  ClientHeight = 598
  ClientWidth = 780
  Color = clBtnFace
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
  object pOptions: TPanel
    Left = 0
    Top = 0
    Width = 265
    Height = 598
    Align = alLeft
    BevelOuter = bvNone
    DoubleBuffered = True
    FullRepaint = False
    ParentBackground = False
    ParentColor = True
    ParentDoubleBuffered = False
    TabOrder = 0
    object Bevel1: TBevel
      Left = 263
      Top = 0
      Width = 2
      Height = 598
      Align = alRight
      Shape = bsRightLine
      ExplicitLeft = 0
      ExplicitHeight = 248
    end
    object gbBackground: TGroupBox
      Left = 4
      Top = 4
      Width = 253
      Height = 237
      Caption = 'Texts and background'
      TabOrder = 0
      object Label3: TLabel
        Left = 12
        Top = 186
        Width = 52
        Height = 13
        Caption = 'Back color:'
      end
      object shpBackColor: TShape
        Left = 104
        Top = 184
        Width = 47
        Height = 18
        Cursor = crHandPoint
        OnMouseDown = shpBackColorMouseDown
      end
      object Label7: TLabel
        Left = 12
        Top = 130
        Width = 26
        Height = 13
        Caption = 'Font:'
      end
      object Label8: TLabel
        Left = 12
        Top = 159
        Width = 65
        Height = 13
        Caption = 'Value format:'
      end
      object Label13: TLabel
        Left = 12
        Top = 56
        Width = 64
        Height = 13
        Caption = 'Caption font:'
      end
      object Label6: TLabel
        Left = 12
        Top = 27
        Width = 41
        Height = 13
        Caption = 'Caption:'
      end
      object edTitle: TEdit
        Left = 68
        Top = 24
        Width = 175
        Height = 21
        TabOrder = 0
        Text = 'Demo budget'
        OnChange = edTitleChange
      end
      object rgAlignment: TRadioGroup
        Left = 12
        Top = 79
        Width = 231
        Height = 37
        Caption = 'Caption alignment'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Left'
          'Center'
          'Right')
        TabOrder = 2
        OnClick = rgAlignmentClick
      end
      object sbFont: TButton
        Left = 104
        Top = 125
        Width = 139
        Height = 25
        Caption = 'Tahoma, 8pt'
        TabOrder = 3
        OnClick = sbFontClick
      end
      object edItemTextFormat: TEdit
        Left = 104
        Top = 156
        Width = 139
        Height = 21
        Hint = 
          'Any "Delphi format function" compatible string for floating-poin' +
          't value'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Text = '$%.0n'
        OnChange = edItemTextFormatChange
      end
      object sbCaptionFont: TButton
        Left = 104
        Top = 52
        Width = 139
        Height = 23
        Caption = 'Tahoma, 8pt'
        TabOrder = 1
        OnClick = sbCaptionFontClick
      end
      object cbShowColumnsHeader: TCheckBox
        Left = 113
        Top = 212
        Width = 137
        Height = 17
        Caption = 'Show columns header'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = cbShowColumnsHeaderClick
      end
      object cbShowCaption: TCheckBox
        Left = 12
        Top = 212
        Width = 100
        Height = 17
        Caption = 'Show caption'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = cbShowCaptionClick
      end
      object cbTransparent: TCheckBox
        Left = 160
        Top = 185
        Width = 90
        Height = 17
        Caption = 'Transparent'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = cbTransparentClick
      end
    end
    object GroupBox1: TGroupBox
      Left = 4
      Top = 407
      Width = 253
      Height = 154
      Caption = 'Frames and lines'
      TabOrder = 2
      object shpGridLineColor: TShape
        Left = 156
        Top = 77
        Width = 83
        Height = 18
        Cursor = crHandPoint
        Brush.Color = clBtnFace
        OnMouseDown = shpGridLineColorMouseDown
      end
      object Label10: TLabel
        Left = 156
        Top = 107
        Width = 49
        Height = 13
        Caption = 'Grid style:'
      end
      object Label1: TLabel
        Left = 12
        Top = 24
        Width = 60
        Height = 13
        Caption = 'Frame color:'
      end
      object shpFrame: TShape
        Left = 80
        Top = 22
        Width = 47
        Height = 18
        Cursor = crHandPoint
        Brush.Color = clBtnFace
        OnMouseDown = shpFrameMouseDown
      end
      object Label2: TLabel
        Left = 156
        Top = 24
        Width = 32
        Height = 13
        Caption = 'Width:'
      end
      object Label9: TLabel
        Left = 156
        Top = 61
        Width = 49
        Height = 13
        Caption = 'Grid color:'
      end
      object cbGridLineStyle: TComboBox
        Left = 156
        Top = 124
        Width = 85
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 2
        TabOrder = 7
        Text = 'Dot'
        OnSelect = cbGridLineStyleSelect
        Items.Strings = (
          'Solid'
          'Dash '
          'Dot'
          'DashDot'
          'DashDotDot')
      end
      object edFrameWidth: TEdit
        Left = 194
        Top = 21
        Width = 32
        Height = 21
        ReadOnly = True
        TabOrder = 0
        Text = '1'
      end
      object udFrameWidth: TUpDown
        Left = 226
        Top = 21
        Width = 15
        Height = 21
        Associate = edFrameWidth
        Max = 6
        Position = 1
        TabOrder = 1
        OnChangingEx = udFrameWidthChangingEx
      end
      object cbShowHorzGrid: TCheckBox
        Left = 12
        Top = 66
        Width = 133
        Height = 17
        Caption = 'Show horizontal grid'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = cbShowHorzGridClick
      end
      object cbShowFirstGridLine: TCheckBox
        Left = 12
        Top = 108
        Width = 125
        Height = 17
        Caption = 'Show first grid line'
        TabOrder = 5
        OnClick = cbShowFirstGridLineClick
      end
      object cbShowLastGridLine: TCheckBox
        Left = 12
        Top = 129
        Width = 113
        Height = 17
        Caption = 'Show last grid line'
        TabOrder = 6
        OnClick = cbShowLastGridLineClick
      end
      object cbShowVertGrid: TCheckBox
        Left = 12
        Top = 87
        Width = 133
        Height = 17
        Caption = 'Show vertical grid'
        TabOrder = 4
        OnClick = cbShowVertGridClick
      end
      object cbFrameProgress: TCheckBox
        Left = 12
        Top = 45
        Width = 129
        Height = 17
        Caption = 'Frame progress'
        TabOrder = 2
        OnClick = cbFrameProgressClick
      end
    end
    object gbStyle: TGroupBox
      Left = 3
      Top = 247
      Width = 253
      Height = 154
      Caption = 'Bars'
      TabOrder = 1
      object Label5: TLabel
        Left = 12
        Top = 27
        Width = 41
        Height = 13
        Caption = 'Bar size:'
      end
      object Label11: TLabel
        Left = 162
        Top = 47
        Width = 23
        Height = 13
        Caption = 'Size:'
      end
      object Label14: TLabel
        Left = 12
        Top = 103
        Width = 71
        Height = 13
        Caption = 'Gradient style:'
      end
      object Label4: TLabel
        Left = 12
        Top = 59
        Width = 60
        Height = 13
        Caption = 'Bar position:'
      end
      object Label12: TLabel
        Left = 137
        Top = 103
        Width = 79
        Height = 13
        Caption = 'Value inside bar:'
      end
      object shpBBackColor: TShape
        Left = 202
        Top = 77
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Brush.Color = clBtnHighlight
        OnMouseDown = shpBBackColorMouseDown
      end
      object Label15: TLabel
        Left = 137
        Top = 79
        Width = 52
        Height = 13
        Caption = 'Back color:'
      end
      object shpBackNone: TShape
        Left = 223
        Top = 77
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Hint = 'Click to select a transparent color for bar background'
        Brush.Color = clGray
        Brush.Style = bsDiagCross
        ParentShowHint = False
        ShowHint = True
        OnMouseDown = shpBackNoneMouseDown
      end
      object cbRounded: TCheckBox
        Left = 137
        Top = 24
        Width = 113
        Height = 17
        Caption = 'Rounded corners'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbRoundedClick
      end
      object edRoundCorners: TEdit
        Left = 194
        Top = 44
        Width = 32
        Height = 21
        Hint = '0 = standard rounded corners'
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 2
        Text = '0'
      end
      object udRoundCorners: TUpDown
        Left = 226
        Top = 44
        Width = 15
        Height = 21
        Hint = '0 = native corners'
        Associate = edRoundCorners
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnChangingEx = udRoundCornersChangingEx
      end
      object udBarSize: TUpDown
        Left = 109
        Top = 24
        Width = 15
        Height = 21
        Associate = edBarSize
        Min = 1
        Max = 64
        Position = 20
        TabOrder = 5
        OnChangingEx = udBarSizeChangingEx
      end
      object edBarSize: TEdit
        Left = 68
        Top = 24
        Width = 41
        Height = 21
        ReadOnly = True
        TabOrder = 0
        Text = '20'
      end
      object cbGradient: TComboBox
        Left = 12
        Top = 120
        Width = 116
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 4
        TabOrder = 6
        Text = 'Glass'
        OnSelect = cbGradientSelect
        Items.Strings = (
          'None'
          'Vertical'
          'Mirror'
          'Lighter mirror'
          'Glass')
      end
      object cbBarPosition: TComboBox
        Left = 12
        Top = 76
        Width = 116
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 4
        Text = 'Inline'
        OnSelect = cbBarPositionSelect
        Items.Strings = (
          'Inline'
          'Bottom'
          'Top')
      end
      object cbValueInBar: TComboBox
        Left = 137
        Top = 120
        Width = 107
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 2
        TabOrder = 7
        Text = 'Percent'
        OnSelect = cbValueInBarSelect
        Items.Strings = (
          'None'
          'Value'
          'Percent')
      end
    end
    object sbSaveAs: TButton
      Left = 4
      Top = 567
      Width = 85
      Height = 25
      Hint = 'Save the first grid as ...'
      Caption = 'Save as ...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = sbSaveAsClick
    end
  end
  object pMain: TPanel
    Left = 265
    Top = 0
    Width = 515
    Height = 598
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 0
      Top = 245
      Width = 515
      Height = 5
      Cursor = crVSplit
      Align = alTop
      AutoSnap = False
      Color = clBtnFace
      ParentColor = False
      ExplicitLeft = -3
      ExplicitTop = 306
    end
    object DecoProgressGrid1: TDecoProgressGridPlus
      Left = 0
      Top = 0
      Width = 515
      Height = 245
      Hint = 'Drag the progress bars to change their values ...'
      Align = alTop
      Alignment = taLeftJustify
      Caption = 'Demo budget'
      DoubleBuffered = True
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'Tahoma'
      CaptionFont.Style = []
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      ParentColor = True
      ParentDoubleBuffered = False
      ParentShowHint = False
      PopupMenu = pumGrid
      ShowHint = True
      TabOrder = 0
      TabStop = True
      OnClick = DecoProgressGrid1Click
      OnCompareItems = DecoProgressGrid1CompareItems
      OnDrawCell = DecoProgressGrid1DrawCell
      OnMouseDown = DecoProgressGrid1MouseDown
      OnMouseLeave = DecoProgressGrid1MouseLeave
      OnMouseMove = DecoProgressGrid1MouseMove
      OnMouseUp = DecoProgressGrid1MouseUp
      Columns = <
        item
          Kind = pgcCaption
          Width = 72
        end
        item
          Kind = pgcProgressBar
          Caption = '%%'
          Width = 100
          Alignment = taCenter
        end
        item
          Kind = pgcMaxValue
          Caption = 'Budget'
          Alignment = taRightJustify
        end
        item
          Caption = 'Actual'
          Alignment = taRightJustify
        end>
      Items = <
        item
          Caption = 'Products'
          Hint = 'Products budget'
          MaxValue = 300000.000000000000000000
          Value = 200000.000000000000000000
        end
        item
          Caption = 'Services'
          Hint = 'Services budget'
          MaxValue = 200000.000000000000000000
          Value = 180000.000000000000000000
        end
        item
          Caption = 'Payroll'
          Hint = 'Budget for payrolls'
          MaxValue = 90000.000000000000000000
          Value = 98000.000000000000000000
        end
        item
          Caption = 'Others'
          Hint = 'Other budgeted items'
          MaxValue = 100000.000000000000000000
          Value = 25000.000000000000000000
        end>
      Rounded = False
      Gradient = pggGlass
      ShowFirstGridLine = False
      ShowLastGridLine = False
      ValueTextFormat = '$%.0n'
      ShowValueInBar = ibvPercent
      Sorted = False
    end
    object DecoProgressGrid2: TDecoProgressGridPlus
      Left = 0
      Top = 250
      Width = 515
      Height = 348
      Align = alClient
      Alignment = taLeftJustify
      AutoSize = True
      BarPosition = pgbBottom
      Caption = 'Demo budget (in a different visual style)'
      DoubleBuffered = True
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'Tahoma'
      CaptionFont.Style = []
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      ParentColor = True
      ParentDoubleBuffered = False
      PopupMenu = pumGrid
      TabOrder = 1
      TabStop = True
      OnClick = DecoProgressGrid1Click
      Columns = <
        item
          Kind = pgcCaption
          Width = 72
        end
        item
          Kind = pgcProgressBar
          Caption = '%%'
        end
        item
          Kind = pgcMaxValue
          Caption = 'Budget'
          Width = 92
          Alignment = taRightJustify
        end
        item
          Kind = pgcDifference
          Caption = 'Left'
          Width = 92
          Alignment = taRightJustify
        end
        item
          Kind = pgcPercent
          Width = 44
          Alignment = taRightJustify
        end>
      Items = <
        item
          Caption = 'Products'
          Hint = 'Products budget'
          MaxValue = 300000.000000000000000000
          Value = 200000.000000000000000000
        end
        item
          Caption = 'Services'
          Hint = 'Services budget'
          MaxValue = 200000.000000000000000000
          Value = 180000.000000000000000000
        end
        item
          Caption = 'Payroll'
          Hint = 'Budget for payrolls'
          MaxValue = 90000.000000000000000000
          Value = 98000.000000000000000000
        end
        item
          Caption = 'Others'
          Hint = 'Other budgeted items'
          MaxValue = 100000.000000000000000000
          Value = 25000.000000000000000000
        end>
      BackColor = clWhite
      FrameColor = clGray
      Level1Color = 16759671
      Level2Color = 16749973
      Level3Color = 14773473
      ShowHorzGrid = False
      ShowFirstGridLine = False
      ShowLastGridLine = False
      ValueTextFormat = '%.0n USD'
      Sorted = False
    end
  end
  object dlgColor: TColorDialog
    Left = 412
    Top = 152
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdApplyButton]
    OnApply = dlgFontApply
    Left = 364
    Top = 152
  end
  object pumGrid: TPopupMenu
    OnPopup = pumGridPopup
    Left = 488
    Top = 152
    object miMenuTitle: TMenuItem
      Caption = 'Grid popup menu'
      Default = True
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miAdditem: TMenuItem
      Caption = 'Add item'
      OnClick = miAdditemClick
    end
    object miEditcaption: TMenuItem
      Caption = 'Edit caption'
      OnClick = miEditcaptionClick
    end
    object miEditValue: TMenuItem
      Caption = 'Edit value'
      OnClick = miEditValueClick
    end
    object miDeleteitem: TMenuItem
      Caption = 'Delete item'
      OnClick = miDeleteitemClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miSortAlpha: TMenuItem
      Caption = 'Sort by caption'
      OnClick = miSortAlphaClick
    end
    object miSortPercent: TMenuItem
      Caption = 'Sort by percent'
      OnClick = miSortPercentClick
    end
  end
end
