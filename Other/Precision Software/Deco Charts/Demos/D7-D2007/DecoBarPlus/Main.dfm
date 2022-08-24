object Form3: TForm3
  Left = 354
  Top = 153
  Width = 800
  Height = 603
  Caption = 'DecoBarPlus Demo'
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
    Height = 576
    Align = alLeft
    BevelOuter = bvNone
    FullRepaint = False
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    object Bevel1: TBevel
      Left = 263
      Top = 0
      Width = 2
      Height = 576
      Align = alRight
      Shape = bsRightLine
    end
    object gbBackground: TGroupBox
      Left = 4
      Top = 4
      Width = 253
      Height = 265
      Caption = 'Texts and background'
      TabOrder = 0
      object Label3: TLabel
        Left = 12
        Top = 210
        Width = 52
        Height = 13
        Caption = 'Back color:'
      end
      object shpBackColor: TShape
        Left = 104
        Top = 208
        Width = 47
        Height = 18
        Cursor = crHandPoint
        OnMouseDown = shpBackColorMouseDown
      end
      object Label7: TLabel
        Left = 12
        Top = 150
        Width = 49
        Height = 13
        Caption = 'Item font:'
      end
      object Label8: TLabel
        Left = 12
        Top = 179
        Width = 84
        Height = 13
        Caption = 'Item text format:'
      end
      object Label12: TLabel
        Left = 12
        Top = 51
        Width = 76
        Height = 13
        Caption = 'Caption format:'
      end
      object Label13: TLabel
        Left = 12
        Top = 76
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
        Text = 'Income last year'
        OnChange = edTitleChange
      end
      object rgAlignment: TRadioGroup
        Left = 12
        Top = 99
        Width = 231
        Height = 37
        Caption = 'Caption alignment'
        Columns = 3
        ItemIndex = 1
        Items.Strings = (
          'Left'
          'Center'
          'Right')
        TabOrder = 3
        OnClick = rgAlignmentClick
      end
      object sbFont: TButton
        Left = 104
        Top = 145
        Width = 139
        Height = 25
        Caption = 'Tahoma, 8pt'
        TabOrder = 4
        OnClick = sbFontClick
      end
      object edItemTextFormat: TEdit
        Left = 104
        Top = 176
        Width = 139
        Height = 21
        Hint = 
          'Item caption - %s'#13#10'Item value - any "Delphi format function" com' +
          'patible string for floating-point value'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Text = '%s: %.2n USD'
        OnChange = edItemTextFormatChange
      end
      object edCaptionFormat: TEdit
        Left = 104
        Top = 48
        Width = 139
        Height = 21
        Hint = 
          'Caption - %s'#13#10'Total value - any "Delphi format function" compati' +
          'ble string for floating-point value'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = '%s: %.2n USD'
        OnChange = edCaptionFormatChange
      end
      object sbCaptionFont: TButton
        Left = 104
        Top = 72
        Width = 139
        Height = 23
        Caption = 'Tahoma, 8pt'
        TabOrder = 2
        OnClick = sbCaptionFontClick
      end
      object cbShowLegend: TCheckBox
        Left = 137
        Top = 239
        Width = 97
        Height = 17
        Caption = 'Show legend'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = cbShowLegendClick
      end
      object cbShowCaption: TCheckBox
        Left = 12
        Top = 239
        Width = 100
        Height = 17
        Caption = 'Show caption'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = cbShowCaptionClick
      end
      object cbTransparent: TCheckBox
        Left = 161
        Top = 209
        Width = 88
        Height = 17
        Caption = 'Transparent'
        TabOrder = 8
        OnClick = cbTransparentClick
      end
    end
    object GroupBox1: TGroupBox
      Left = 4
      Top = 435
      Width = 253
      Height = 105
      Caption = 'Lines and frames'
      TabOrder = 2
      object Label9: TLabel
        Left = 12
        Top = 48
        Width = 54
        Height = 13
        Caption = 'Lines color:'
      end
      object shpTreeLinesColor: TShape
        Left = 80
        Top = 47
        Width = 47
        Height = 18
        Cursor = crHandPoint
        Brush.Color = clBtnShadow
        OnMouseDown = shpTreeLinesColorMouseDown
      end
      object Label10: TLabel
        Left = 12
        Top = 74
        Width = 54
        Height = 13
        Caption = 'Lines style:'
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
      object cbTreeLinesStyle: TComboBox
        Left = 80
        Top = 71
        Width = 161
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 2
        Text = 'Rectangular'
        OnSelect = cbTreeLinesStyleSelect
        Items.Strings = (
          'None'
          'Rectangular'
          'RoundedRects'
          'Curved'
          'StrightLines')
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
    end
    object gbStyle: TGroupBox
      Left = 4
      Top = 275
      Width = 253
      Height = 154
      Caption = 'Bars'
      TabOrder = 1
      object Label4: TLabel
        Left = 12
        Top = 51
        Width = 47
        Height = 13
        Caption = 'Strip size:'
      end
      object Label5: TLabel
        Left = 12
        Top = 27
        Width = 41
        Height = 13
        Caption = 'Bar size:'
      end
      object Label11: TLabel
        Left = 162
        Top = 51
        Width = 23
        Height = 13
        Caption = 'Size:'
      end
      object cbRounded: TCheckBox
        Left = 137
        Top = 24
        Width = 113
        Height = 17
        Caption = 'Rounded corners'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = cbRoundedClick
      end
      object edRoundCorners: TEdit
        Left = 194
        Top = 48
        Width = 32
        Height = 21
        Hint = '0 = standard rounded corners'
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 4
        Text = '0'
      end
      object udRoundCorners: TUpDown
        Left = 226
        Top = 48
        Width = 15
        Height = 21
        Hint = '0 = native corners'
        Associate = edRoundCorners
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnChangingEx = udRoundCornersChangingEx
      end
      object cbGradient: TCheckBox
        Left = 12
        Top = 83
        Width = 112
        Height = 17
        Caption = 'Gradients'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = cbGradientClick
      end
      object edStripSize: TEdit
        Left = 68
        Top = 48
        Width = 41
        Height = 21
        Hint = '0 = No strips'
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 1
        Text = '20'
      end
      object udStripSize: TUpDown
        Left = 109
        Top = 48
        Width = 15
        Height = 21
        Hint = '0 = No strips'
        Associate = edStripSize
        Max = 128
        ParentShowHint = False
        Position = 20
        ShowHint = True
        TabOrder = 2
        OnChangingEx = udStripSizeChangingEx
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
        TabOrder = 9
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
      object cbCutExpanded: TCheckBox
        Left = 12
        Top = 104
        Width = 139
        Height = 17
        Caption = 'Cut expanded bar'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = cbCutExpandedClick
      end
      object cbDarkenCollapsed: TCheckBox
        Left = 12
        Top = 125
        Width = 157
        Height = 17
        Caption = 'Darken collapsed bars'
        Checked = True
        State = cbChecked
        TabOrder = 8
        OnClick = cbDarkenCollapsedClick
      end
      object cbAutoColors: TCheckBox
        Left = 146
        Top = 83
        Width = 104
        Height = 17
        Hint = 
          'No colors are assigned for items in this demo. If you uncheck th' +
          'is option, '#13#10'all the items will have the same (white) color.'
        Caption = 'Auto coloring'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 10
        OnClick = cbAutoColorsClick
      end
      object cbAutoCalc: TCheckBox
        Left = 146
        Top = 104
        Width = 104
        Height = 17
        Caption = 'Auto calculate'
        Checked = True
        State = cbChecked
        TabOrder = 11
        OnClick = cbAutoCalcClick
      end
    end
    object sbSaveAs: TButton
      Left = 4
      Top = 546
      Width = 85
      Height = 25
      Caption = 'Save as ...'
      TabOrder = 3
      OnClick = sbSaveAsClick
    end
  end
  object pMain: TPanel
    Left = 265
    Top = 0
    Width = 527
    Height = 576
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object lvData: TListView
      Left = 0
      Top = 109
      Width = 527
      Height = 467
      Align = alClient
      BorderStyle = bsNone
      Columns = <
        item
          AutoSize = True
        end
        item
          Alignment = taRightJustify
          Width = 64
        end
        item
          Alignment = taRightJustify
          Width = 112
        end>
      RowSelect = True
      ParentColor = True
      ShowColumnHeaders = False
      TabOrder = 1
      ViewStyle = vsReport
      Visible = False
      OnKeyDown = lvDataKeyDown
    end
    object DecoBar1: TDecoBarPlus
      Left = 0
      Top = 0
      Width = 527
      Height = 109
      Align = alTop
      AllowExpandingNodes = True
      AutoCalcValues = True
      AutoSize = True
      Caption = 'Income last year'
      CaptionFont.Charset = DEFAULT_CHARSET
      CaptionFont.Color = clWindowText
      CaptionFont.Height = -11
      CaptionFont.Name = 'Tahoma'
      CaptionFont.Style = []
      CaptionFormat = '%s: %.2n USD'
      DoubleBuffered = True
      HoverCursor = crHandPoint
      Items = <
        item
          Caption = 'Products'
          Items = <
            item
              Caption = 'Cars'
              Items = <
                item
                  Caption = 'Audi'
                  Value = 110000.000000000000000000
                end
                item
                  Caption = 'BMW'
                  Value = 50000.000000000000000000
                end
                item
                  Caption = 'Mercedes'
                  Value = 80000.000000000000000000
                end>
              Value = 240000.000000000000000000
            end
            item
              Caption = 'Spare parts'
              Items = <
                item
                  Caption = 'Item 1'
                  Value = 20000.000000000000000000
                end
                item
                  Caption = 'Item 2'
                  Value = 10000.000000000000000000
                end
                item
                  Caption = 'Item 3'
                  Value = 30000.000000000000000000
                end>
              Value = 60000.000000000000000000
            end>
          Value = 300000.000000000000000000
        end
        item
          Caption = 'Services'
          Items = <
            item
              Caption = 'Repairs'
              Items = <
                item
                  Caption = 'Engines'
                  Value = 8000.000000000000000000
                end
                item
                  Caption = 'Brakes'
                  Value = 3500.000000000000000000
                end
                item
                  Caption = 'Chases'
                  Value = 2500.000000000000000000
                end>
              Value = 14000.000000000000000000
            end
            item
              Caption = 'Maintenance'
              Items = <
                item
                  Caption = 'Item 1'
                  Value = 20000.000000000000000000
                end
                item
                  Caption = 'Item 2'
                  Value = 4000.000000000000000000
                end
                item
                  Caption = 'Item 3'
                  Value = 18000.000000000000000000
                end>
              Value = 42000.000000000000000000
            end
            item
              Caption = 'Testing'
              Items = <
                item
                  Caption = 'Testing A'
                  Value = 2000.000000000000000000
                end
                item
                  Caption = 'Testing B'
                  Value = 8000.000000000000000000
                end>
              Value = 10000.000000000000000000
            end
            item
              Caption = 'Others'
              Value = 2700.000000000000000000
            end>
          Value = 68700.000000000000000000
        end
        item
          Caption = 'Others'
          Tag = 1
          Value = 20000.000000000000000000
        end>
      ItemTextFormat = '%s: %.2n USD'
      OnClick = DecoBar1Click
      OnCollapsed = DecoBar1Collapsed
      OnExpanded = DecoBar1Expanded
      OnExpanding = DecoBar1Expanding
      ParentColor = True
      PopupMenu = pumDecoBar
      TabOrder = 0
      TabStop = True
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
  object pumDecoBar: TPopupMenu
    OnPopup = pumDecoBarPopup
    Left = 488
    Top = 152
    object DecoBarpopupmenu1: TMenuItem
      Caption = 'DecoBar popup menu'
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
    object miCollapseall: TMenuItem
      Caption = 'Collapse all'
      OnClick = miCollapseallClick
    end
  end
end
