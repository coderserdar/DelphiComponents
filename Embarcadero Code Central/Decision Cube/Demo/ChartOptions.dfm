object frmChartOptions: TfrmChartOptions
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object pgChart: TPageControl
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    ActivePage = tabChart
    Align = alClient
    TabIndex = 1
    TabOrder = 0
    OnChange = pgChartChange
    object tabLegend: TTabSheet
      Caption = 'Legend'
      object Label1: TLabel
        Left = 136
        Top = 48
        Width = 32
        Height = 13
        Caption = 'Margin'
      end
      object chkLegendVisible: TCheckBox
        Left = 16
        Top = 16
        Width = 57
        Height = 17
        Caption = 'Visible'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkLegendVisibleClick
      end
      object rgLegenPosition: TRadioGroup
        Left = 16
        Top = 40
        Width = 113
        Height = 50
        Caption = 'Position'
        Columns = 2
        ItemIndex = 1
        Items.Strings = (
          'Left'
          'Right'
          'Top'
          'Bottom')
        TabOrder = 1
        OnClick = rgLegenPositionClick
      end
      object btnLegendFont: TButton
        Left = 16
        Top = 96
        Width = 75
        Height = 25
        Caption = 'Font'
        TabOrder = 2
        OnClick = btnLegendFontClick
      end
      object chkLegendResize: TCheckBox
        Left = 72
        Top = 16
        Width = 57
        Height = 17
        Caption = 'Resize'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = chkLegendResizeClick
      end
      object chkLegendInverted: TCheckBox
        Left = 128
        Top = 16
        Width = 65
        Height = 17
        Caption = 'Inverted'
        TabOrder = 4
        OnClick = chkLegendInvertedClick
      end
      object udLegendMargin: TUpDown
        Left = 185
        Top = 64
        Width = 15
        Height = 21
        Associate = edLegendMargin
        Min = 0
        Position = 0
        TabOrder = 5
        Wrap = False
        OnClick = udLegendMarginClick
      end
      object edLegendMargin: TEdit
        Left = 136
        Top = 64
        Width = 49
        Height = 21
        TabOrder = 6
        Text = '0'
      end
      object btnLegendBackColor: TButton
        Left = 96
        Top = 96
        Width = 75
        Height = 25
        Caption = 'Bg Color'
        TabOrder = 7
        OnClick = btnLegendBackColorClick
      end
    end
    object tabChart: TTabSheet
      Caption = 'Chart'
      ImageIndex = 1
      object cbSeries: TComboBox
        Left = 8
        Top = 8
        Width = 145
        Height = 21
        BevelKind = bkFlat
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbSeriesChange
      end
      object PageControl1: TPageControl
        Left = 0
        Top = 40
        Width = 312
        Height = 172
        ActivePage = tabBar
        Align = alBottom
        Style = tsFlatButtons
        TabOrder = 1
        object tabBar: TTabSheet
          Caption = 'tabBar'
          TabVisible = False
          object cbBarStyle: TComboBox
            Left = 8
            Top = 24
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = cbBarStyleChange
            Items.Strings = (
              'Rectangle'
              'Gradiant'
              'Pyramid'
              'Inv Pyramid'
              'Cilinder'
              'Ellipse'
              'Arrow')
          end
        end
      end
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 264
    Top = 24
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 264
    Top = 56
  end
end
