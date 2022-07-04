object SeriePropsForm: TSeriePropsForm
  Left = 287
  Top = 63
  ActiveControl = PageControl
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Properties: Serie'
  ClientHeight = 407
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    343
    407)
  PixelsPerInch = 96
  TextHeight = 13
  object OkBitBtn: TBitBtn
    Left = 102
    Top = 371
    Width = 73
    Height = 28
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkOK
  end
  object CancelBitBtn: TBitBtn
    Left = 182
    Top = 371
    Width = 73
    Height = 28
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkCancel
  end
  object HelpBitBtn: TBitBtn
    Left = 262
    Top = 371
    Width = 73
    Height = 28
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkHelp
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 327
    Height = 353
    ActivePage = DataTabSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    HotTrack = True
    Images = ImageList
    TabOrder = 0
    object DataTabSheet: TTabSheet
      Caption = 'Data'
      object ParameterPanel: TPanel
        Left = 4
        Top = 9
        Width = 307
        Height = 81
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 3
        Visible = False
        object IntervalsLabel: TLabel
          Left = 204
          Top = 15
          Width = 43
          Height = 13
          Caption = 'Intervals:'
          FocusControl = IntervalsSpinEdit
        end
        object CXLabel: TLabel
          Left = 89
          Top = 15
          Width = 25
          Height = 13
          Caption = '< X <'
        end
        object ParameterLabel: TLabel
          Left = 8
          Top = 40
          Width = 289
          Height = 33
          AutoSize = False
          Caption = 
            'Notice: X,Y expressions must depend only on the variable that re' +
            'presents current value of the parameter.'
          WordWrap = True
        end
        object PMinEdit: TEdit
          Left = 8
          Top = 12
          Width = 77
          Height = 21
          Hint = 
            'Minimal value of the parameter for the'#10'parametric (functional) s' +
            'eries.'
          HelpContext = 1303
          TabOrder = 0
          Text = '0'
        end
        object PMaxEdit: TEdit
          Left = 116
          Top = 12
          Width = 77
          Height = 21
          Hint = 
            'Maximal value of the parameter for the'#10'parametric (functional) s' +
            'eries.'
          HelpContext = 1304
          TabOrder = 1
          Text = '0'
        end
        object IntervalsSpinEdit: TSpinEdit
          Left = 252
          Top = 12
          Width = 45
          Height = 22
          Hint = 'Number of points in the parametric series.'
          HelpContext = 1105
          Increment = 10
          MaxValue = 1000
          MinValue = 1
          TabOrder = 2
          Value = 100
        end
      end
      object XGroupBox: TGroupBox
        Left = 4
        Top = 96
        Width = 307
        Height = 105
        Caption = ' &X axis: '
        TabOrder = 4
        object XColumnLabel: TLabel
          Left = 8
          Top = 20
          Width = 38
          Height = 13
          Caption = 'Column:'
          FocusControl = XColumnComboBox
        end
        object XExpressionLabel: TLabel
          Left = 8
          Top = 76
          Width = 54
          Height = 13
          Caption = 'Expression:'
          FocusControl = XExpressionComboBox
        end
        object XErrorBarColumnLabel: TLabel
          Left = 8
          Top = 46
          Width = 43
          Height = 13
          Caption = 'Error bar:'
          FocusControl = XErrorBarColumnComboBox
        end
        object XErrorBarColumnSpeedButton: TSpeedButton
          Left = 274
          Top = 44
          Width = 22
          Height = 22
          Flat = True
          Glyph.Data = {
            F6010000424DF601000000000000760000002800000030000000100000000100
            0400000000008001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888988888888889888878888888888788888888888888
            8888899988888888888887778888888888888977888888889888899998888888
            9888877778888888788899977888888888888899988888898888887778888887
            8888999977888879888888899988889988888887778888778888899977787798
            8888888899988998888888887778877888888899977779988888888889999988
            8888888887777788888888899977998888888888889998888888888888777888
            8888888899999888888888888999998888888888877777888888888889997788
            8888888899988998888888887778877888888888999997788888889999888899
            8888887777888877888888899988997788888999988888899888877778888887
            7888899998888997888889998888888889888777888888888788999988888899
            7888888888888888888888888888888888889998888888889888}
          NumGlyphs = 3
          OnClick = XErrorBarColumnSpeedButtonClick
        end
        object XColumnComboBox: TComboBox
          Left = 64
          Top = 16
          Width = 233
          Height = 21
          Hint = 
            'Lists columns of the worksheet associated with current'#10'series. T' +
            'his field will reset when you change worksheet.'#10'If no column sel' +
            'ected, series is treated as empty.'
          HelpContext = 1101
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object XExpressionComboBox: TExpressionComboBox
          Left = 64
          Top = 74
          Width = 233
          Height = 21
          Hint = 
            'Input new expression or select expressions from'#10'dropdown list. P' +
            'arameters a..z denote appropriate'#10'column values, cx/cy - selecte' +
            'd columns.'
          HelpContext = 1103
          ItemHeight = 13
          TabOrder = 2
          DesignSize = (
            233
            21)
        end
        object XErrorBarColumnComboBox: TComboBox
          Left = 64
          Top = 44
          Width = 210
          Height = 21
          Hint = 'Column in selected worksheet for error bar data'
          HelpContext = 1334
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
      end
      object YGroupBox: TGroupBox
        Left = 4
        Top = 208
        Width = 307
        Height = 105
        Caption = ' &Y axis: '
        TabOrder = 5
        object YColumnLabel: TLabel
          Left = 8
          Top = 20
          Width = 38
          Height = 13
          Caption = 'Column:'
          FocusControl = YColumnComboBox
        end
        object YExpressionLabel: TLabel
          Left = 8
          Top = 76
          Width = 54
          Height = 13
          Caption = 'Expression:'
          FocusControl = YExpressionComboBox
        end
        object YErrorBarColumnLabel: TLabel
          Left = 8
          Top = 46
          Width = 43
          Height = 13
          Caption = 'Error bar:'
          FocusControl = YErrorBarColumnComboBox
        end
        object YErrorBarColumnSpeedButton: TSpeedButton
          Left = 274
          Top = 44
          Width = 22
          Height = 22
          Flat = True
          Glyph.Data = {
            F6010000424DF601000000000000760000002800000030000000100000000100
            0400000000008001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888988888888889888878888888888788888888888888
            8888899988888888888887778888888888888977888888889888899998888888
            9888877778888888788899977888888888888899988888898888887778888887
            8888999977888879888888899988889988888887778888778888899977787798
            8888888899988998888888887778877888888899977779988888888889999988
            8888888887777788888888899977998888888888889998888888888888777888
            8888888899999888888888888999998888888888877777888888888889997788
            8888888899988998888888887778877888888888999997788888889999888899
            8888887777888877888888899988997788888999988888899888877778888887
            7888899998888997888889998888888889888777888888888788999988888899
            7888888888888888888888888888888888889998888888889888}
          NumGlyphs = 3
          OnClick = YErrorBarColumnSpeedButtonClick
        end
        object YColumnComboBox: TComboBox
          Left = 64
          Top = 16
          Width = 233
          Height = 21
          Hint = 
            'Lists columns of the worksheet associated with current'#10'series. T' +
            'his field will reset when you change worksheet.'#10'If no column sel' +
            'ected, series is treated as empty.'
          HelpContext = 1101
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object YExpressionComboBox: TExpressionComboBox
          Left = 64
          Top = 74
          Width = 233
          Height = 21
          Hint = 
            'Input new expression or select expressions from'#10'dropdown list. P' +
            'arameters a..z denote appropriate'#10'column values, cx/cy - selecte' +
            'd columns.'
          HelpContext = 1103
          ItemHeight = 13
          TabOrder = 2
          DesignSize = (
            233
            21)
        end
        object YErrorBarColumnComboBox: TComboBox
          Left = 64
          Top = 44
          Width = 210
          Height = 21
          Hint = 'Column in selected worksheet for error bar data'
          HelpContext = 1334
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
      end
      object DataPanel: TPanel
        Left = 4
        Top = 9
        Width = 307
        Height = 81
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 2
        object FirstLineLabel: TLabel
          Left = 8
          Top = 16
          Width = 22
          Height = 13
          Caption = '&First:'
          FocusControl = FirstLineSpinEdit
        end
        object LastLineLabel: TLabel
          Left = 106
          Top = 16
          Width = 23
          Height = 13
          Caption = '&Last:'
          FocusControl = LastLineSpinEdit
        end
        object InterleaveLabel: TLabel
          Left = 208
          Top = 16
          Width = 50
          Height = 13
          Caption = '&Interleave:'
          FocusControl = InterleaveSpinEdit
        end
        object WorksheetLabel: TLabel
          Left = 8
          Top = 48
          Width = 55
          Height = 13
          Caption = '&Worksheet:'
          FocusControl = WorksheetComboBox
        end
        object FirstLineSpinEdit: TSpinEdit
          Left = 32
          Top = 12
          Width = 66
          Height = 22
          Hint = 
            'Define range of lines in the worksheet associated'#10'with series. I' +
            'f first line is greater then last line or'#10'equal to -1, series wi' +
            'll be treated as "empty".'
          HelpContext = 1106
          MaxValue = 100000
          MinValue = -1
          TabOrder = 0
          Value = 0
        end
        object LastLineSpinEdit: TSpinEdit
          Left = 132
          Top = 12
          Width = 66
          Height = 22
          Hint = 
            'Define range of lines in the worksheet associated'#10'with series. I' +
            'f first line is greater then last line or'#10'equal to -1, series wi' +
            'll be treated as "empty".'
          HelpContext = 1106
          MaxValue = 100000
          MinValue = -1
          TabOrder = 1
          Value = 0
        end
        object InterleaveSpinEdit: TSpinEdit
          Left = 262
          Top = 12
          Width = 35
          Height = 22
          Hint = 
            'If plotting time is too long you may increase speed'#10'by setting t' +
            'his parameter more than 1.'
          HelpContext = 1107
          MaxValue = 99
          MinValue = 1
          TabOrder = 2
          Value = 1
        end
        object WorksheetComboBox: TComboBox
          Left = 64
          Top = 44
          Width = 233
          Height = 22
          Hint = 
            'Every serie displays data from associated worksheet.'#10'If no assoc' +
            'iated worksheet serie is treated as empty.'
          HelpContext = 1108
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Pitch = fpFixed
          Font.Style = []
          ItemHeight = 14
          ParentFont = False
          TabOrder = 3
          OnChange = WorksheetComboBoxChange
          OnDropDown = WorksheetComboBoxDropDown
        end
      end
      object DataRadioButton: TRadioButton
        Left = 16
        Top = 2
        Width = 82
        Height = 17
        Hint = 
          'If this button selected, series object displays'#10'the range of ele' +
          'ments of data from the associated'#10'worksheet. If no data selected' +
          ', series is empty.  '
        HelpContext = 1301
        Caption = 'Data - based'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButtonClick
      end
      object ParameterRadioButton: TRadioButton
        Left = 192
        Top = 2
        Width = 72
        Height = 17
        Hint = 
          'If this button selected, series is functional'#10'(parametric). X,Y ' +
          'coordinates are calculated'#10'using X,Y expressions and a given ran' +
          'ge of'#10'the parameter specified below.'
        HelpContext = 1302
        Caption = 'Parametric'
        TabOrder = 1
        OnClick = RadioButtonClick
      end
    end
    object ViewTabSheet: TTabSheet
      Caption = 'View'
      ImageIndex = 1
      DesignSize = (
        319
        324)
      object PreviewPaintBox: TPaintBox
        Left = 240
        Top = 188
        Width = 72
        Height = 29
        OnPaint = PreviewPaintBoxPaint
      end
      object CaptionLabel: TLabel
        Left = 12
        Top = 292
        Width = 39
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = '&Caption:'
        FocusControl = CaptionEdit
      end
      object AreaLabel: TLabel
        Left = 160
        Top = 228
        Width = 58
        Height = 13
        Caption = '&Area border:'
        FocusControl = AreaComboBox
      end
      object CaptionEdit: TEdit
        Left = 60
        Top = 290
        Width = 225
        Height = 21
        Hint = 'Use caption text to describe series data.'
        HelpContext = 1116
        Anchors = [akRight, akBottom]
        TabOrder = 10
        Text = 'CaptionEdit'
      end
      object PointsPanel: TPanel
        Left = 4
        Top = 9
        Width = 105
        Height = 212
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 1
        object PointSizeLabel: TLabel
          Left = 8
          Top = 20
          Width = 40
          Height = 13
          Caption = 'Size, px:'
          FocusControl = PointSizeSpinEdit
        end
        object PointTypeLabel: TLabel
          Left = 8
          Top = 46
          Width = 27
          Height = 13
          Caption = 'Type:'
          FocusControl = PointTypeComboBox
        end
        object PointsColorLabel: TLabel
          Left = 8
          Top = 72
          Width = 27
          Height = 13
          Caption = 'Color:'
          FocusControl = PointsColorGrid
        end
        object PointSizeSpinEdit: TSpinEdit
          Left = 58
          Top = 16
          Width = 37
          Height = 22
          Hint = 'Specify point size in pixels'
          HelpContext = 1110
          Increment = 2
          MaxValue = 21
          MinValue = 1
          TabOrder = 0
          Value = 3
          OnChange = PreviewPaintBoxPaint
        end
        object PointTypeComboBox: TComboBox
          Left = 46
          Top = 44
          Width = 49
          Height = 20
          Hint = 'Select point style'
          HelpContext = 1111
          Style = csOwnerDrawFixed
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 14
          ParentFont = False
          TabOrder = 1
          OnChange = PreviewPaintBoxPaint
          OnDrawItem = PointTypeComboBoxDrawItem
          Items.Strings = (
            'Square'
            'Circle'
            'Cross '
            'X cross'
            'Asterisk')
        end
        object PointsColorGrid: TColorGrid
          Left = 8
          Top = 89
          Width = 88
          Height = 80
          Hint = 
            'These controls used to change series colors. Pick'#10'color from the' +
            ' table or click button for more colors.'
          HelpContext = 1112
          ClickEnablesColor = True
          BackgroundEnabled = False
          TabOrder = 2
          OnChange = ColorGridChange
        end
        object PointsColorBitBtn: TBitBtn
          Left = 9
          Top = 173
          Width = 87
          Height = 28
          Hint = 
            'These controls used to change series colors. Pick'#10'color from the' +
            ' table or click button for more colors.'
          HelpContext = 1112
          Caption = 'More...'
          TabOrder = 3
          OnClick = PointsColorBitBtnClick
          Glyph.Data = {
            F6020000424DF60200000000000076000000280000003C000000140000000100
            0400000000008002000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777777777777770000780000000000
            0000000778000000000000000007778888888888888888870000788888888888
            8888880778888888888888888807700000000000000000870000786666666666
            6666660778666666666666666607788888888888888880870000782222222222
            2222220778222222222222222207766666666666666660870000782A2A2A2A2A
            2A2A2A07782A2A2A2A2A2A2A2A0772222222222222222087000078AAAAAAAAAA
            AAAAAA0778AAAAAAAAAAAAAAAA0772A2A2A2A2A2A2A2A087000078ABABABABAB
            ABABAB0778ABABABABABABABAB077AAAAAAAAAAAAAAAA087000078BBBBBBBBBB
            BBBBBB0778BBBBBBBBBBBBBBBB077ABABABABABABABAB087000078B9B9B9B9B9
            B9B9B90778B9B9B9B9B9B9B9B9077BBBBBBBBBBBBBBBB0870000789999999999
            99999907789999999999999999077B9B9B9B9B9B9B9B90870000789D9D9D9D9D
            9D9D9D07789D9D9D9D9D9D9D9D0779999999999999999087000078DDDDDDDDDD
            DDDDDD0778DDDDDDDDDDDDDDDD0779D9D9D9D9D9D9D9D0870000785555555555
            55555507785555555555555555077DDDDDDDDDDDDDDDD0870000784444444444
            444444077844444444444444440775555555555555555087000078CCCCCCCCCC
            CCCCCC0778CCCCCCCCCCCCCCCC0774444444444444444087000078EEEEEEEEEE
            EEEEEE0778EEEEEEEEEEEEEEEE077CCCCCCCCCCCCCCCC087000078FFFFFFFFFF
            FFFFFF0778FFFFFFFFFFFFFFFF077EEEEEEEEEEEEEEEE0870000788888888888
            88888887788888888888888888877FFFFFFFFFFFFFFFF0770000777777777777
            7777777777777777777777777777777777777777777777770000}
          NumGlyphs = 3
        end
      end
      object PointsCheckBox: TCheckBox
        Left = 16
        Top = 2
        Width = 81
        Height = 17
        Hint = 'Check this flag to make series points visible'
        HelpContext = 1109
        Caption = 'Show &Points'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = PointPanelCheckBoxClick
      end
      object LinePanel: TPanel
        Left = 119
        Top = 9
        Width = 112
        Height = 212
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 3
        object LineWidthLabel: TLabel
          Left = 8
          Top = 20
          Width = 48
          Height = 13
          Caption = 'Width, px:'
          FocusControl = LineWidthSpinEdit
        end
        object LineTypeLabel: TLabel
          Left = 8
          Top = 46
          Width = 27
          Height = 13
          Caption = 'Type:'
          FocusControl = LineTypeComboBox
        end
        object LineColorLabel: TLabel
          Left = 8
          Top = 72
          Width = 27
          Height = 13
          Caption = 'Color:'
          FocusControl = LineColorGrid
        end
        object LineWidthSpinEdit: TSpinEdit
          Left = 64
          Top = 16
          Width = 37
          Height = 22
          Hint = 'Set curve line width in pixels'
          HelpContext = 1114
          MaxValue = 5
          MinValue = 1
          TabOrder = 0
          Value = 1
          OnChange = PreviewPaintBoxPaint
        end
        object LineTypeComboBox: TComboBox
          Left = 48
          Top = 44
          Width = 52
          Height = 20
          Hint = 
            'Select line style. Note that under Windows 9.x'#10'line always solid' +
            ' if its width greater then 1 pixel.'
          HelpContext = 1115
          Style = csOwnerDrawFixed
          ItemHeight = 14
          TabOrder = 1
          OnChange = PreviewPaintBoxPaint
          OnDrawItem = LineTypeComboBoxDrawItem
          Items.Strings = (
            'Solid'
            'Dash'
            'Dot'
            'Dashdot')
        end
        object LineColorGrid: TColorGrid
          Left = 8
          Top = 89
          Width = 92
          Height = 80
          Hint = 
            'These controls used to change series colors. Pick'#10'color from the' +
            ' table or click button for more colors.'
          HelpContext = 1112
          BackgroundEnabled = False
          TabOrder = 2
          OnChange = ColorGridChange
        end
        object LineColorBitBtn: TBitBtn
          Left = 9
          Top = 172
          Width = 91
          Height = 28
          Hint = 
            'These controls used to change series colors. Pick'#10'color from the' +
            ' table or click button for more colors.'
          HelpContext = 1112
          Caption = 'More...'
          TabOrder = 3
          OnClick = LineColorBitBtnClick
          Glyph.Data = {
            F6020000424DF60200000000000076000000280000003C000000140000000100
            0400000000008002000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777777777777770000780000000000
            0000000778000000000000000007778888888888888888870000788888888888
            8888880778888888888888888807700000000000000000870000786666666666
            6666660778666666666666666607788888888888888880870000782222222222
            2222220778222222222222222207766666666666666660870000782A2A2A2A2A
            2A2A2A07782A2A2A2A2A2A2A2A0772222222222222222087000078AAAAAAAAAA
            AAAAAA0778AAAAAAAAAAAAAAAA0772A2A2A2A2A2A2A2A087000078ABABABABAB
            ABABAB0778ABABABABABABABAB077AAAAAAAAAAAAAAAA087000078BBBBBBBBBB
            BBBBBB0778BBBBBBBBBBBBBBBB077ABABABABABABABAB087000078B9B9B9B9B9
            B9B9B90778B9B9B9B9B9B9B9B9077BBBBBBBBBBBBBBBB0870000789999999999
            99999907789999999999999999077B9B9B9B9B9B9B9B90870000789D9D9D9D9D
            9D9D9D07789D9D9D9D9D9D9D9D0779999999999999999087000078DDDDDDDDDD
            DDDDDD0778DDDDDDDDDDDDDDDD0779D9D9D9D9D9D9D9D0870000785555555555
            55555507785555555555555555077DDDDDDDDDDDDDDDD0870000784444444444
            444444077844444444444444440775555555555555555087000078CCCCCCCCCC
            CCCCCC0778CCCCCCCCCCCCCCCC0774444444444444444087000078EEEEEEEEEE
            EEEEEE0778EEEEEEEEEEEEEEEE077CCCCCCCCCCCCCCCC087000078FFFFFFFFFF
            FFFFFF0778FFFFFFFFFFFFFFFF077EEEEEEEEEEEEEEEE0870000788888888888
            88888887788888888888888888877FFFFFFFFFFFFFFFF0770000777777777777
            7777777777777777777777777777777777777777777777770000}
          NumGlyphs = 3
        end
      end
      object LineCheckBox: TCheckBox
        Left = 128
        Top = 2
        Width = 73
        Height = 17
        Hint = 'Check this flag if you want to'#10'connect points with lines.'
        HelpContext = 1113
        Caption = 'Show &Line'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = PreviewPaintBoxPaint
      end
      object XAxisRadioGroup: TRadioGroup
        Left = 242
        Top = 4
        Width = 69
        Height = 69
        Hint = 'Select X axis for series'
        HelpContext = 1308
        Caption = ' &X Axis: '
        ItemIndex = 0
        Items.Strings = (
          'Bottom'
          'Top')
        TabOrder = 4
      end
      object YAxisRadioGroup: TRadioGroup
        Left = 242
        Top = 82
        Width = 69
        Height = 69
        Hint = 'Select Y axis for series'
        HelpContext = 1309
        Caption = ' &Y Axis: '
        ItemIndex = 0
        Items.Strings = (
          'Left'
          'Right')
        TabOrder = 5
      end
      object VisibleCheckBox: TCheckBox
        Left = 251
        Top = 160
        Width = 57
        Height = 17
        Hint = 'If not checked, series is invisible'
        HelpContext = 1307
        Caption = '&Visible'
        TabOrder = 6
      end
      object HTMLTextBitBtn: TBitBtn
        Left = 288
        Top = 289
        Width = 24
        Height = 24
        Hint = 'Invoke HTML Text Editor to apply'#10'formatting and insert symbols'
        HelpContext = 1333
        Anchors = [akRight, akBottom]
        TabOrder = 11
        OnClick = HTMLTextBitBtnClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
          8888888888888848888888888888884488888888884888844888888888488888
          4488888888848888844888888484888844888888448488844888888448884844
          8888884488884848888884488888488888888844888884888888888448888488
          8888888844888888888888888488888888888888888888888888}
      end
      object LeaderPanel: TPanel
        Left = 4
        Top = 236
        Width = 141
        Height = 45
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 8
        object LeaderLabel: TLabel
          Left = 8
          Top = 14
          Width = 40
          Height = 13
          Caption = 'Position:'
          Enabled = False
          FocusControl = LeaderSpinEdit
        end
        object LeaderSpinEdit: TSpinEdit
          Left = 60
          Top = 12
          Width = 69
          Height = 22
          Hint = 'You can change position of marker'#10'manually using this control'
          HelpContext = 1336
          Enabled = False
          MaxValue = 100000
          MinValue = -1
          TabOrder = 0
          Value = 0
        end
      end
      object LeaderCheckBox: TCheckBox
        Left = 16
        Top = 228
        Width = 85
        Height = 17
        Hint = 
          'Marker may be used to select data points visually and to'#10'identif' +
          'y current point in the series in the recording mode.'
        HelpContext = 1335
        Caption = 'Show &marker'
        TabOrder = 7
        OnClick = LeaderCheckBoxClick
      end
      object AreaComboBox: TComboBox
        Left = 160
        Top = 248
        Width = 152
        Height = 22
        Hint = 
          'Select border object to fill plot area'#10'between this object and s' +
          'eries curve.'
        HelpContext = 1337
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 9
        OnDrawItem = AreaComboBoxDrawItem
      end
    end
  end
  object ImageList: TImageList
    Left = 232
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000848400000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00008484000084
      840000FFFF0000FFFF00008484000084840000FFFF0000FFFF00008484000084
      840000FFFF000084840000000000000000000000000000000000000000000000
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00008484000084
      840000FFFF0000FFFF00008484000084840000FFFF0000FFFF00008484000084
      840000FFFF000084840000000000000000000000000000000000000000000000
      00000000000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00008484000084
      840000FFFF0000FFFF00008484000084840000FFFF0000FFFF00008484000084
      840000FFFF000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00008484000084
      840000FFFF0000FFFF00008484000084840000FFFF0000FFFF00008484000084
      840000FFFF000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00008484000084
      840000FFFF0000FFFF00008484000084840000FFFF0000FFFF00008484000084
      840000FFFF000084840000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00008484000084
      840000FFFF0000FFFF00008484000084840000FFFF0000FFFF00008484000084
      840000FFFF000084840000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF000084840000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      8003C03F000000008003C03F000000008003E07F000000008003F0FF00000000
      8003F987000000008003FF03000000008003FF03000000008003C10300000000
      8003C103000000008003C187000000008003C1FF00000000FFFFC1FF00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object ColorDialog: TColorDialog
    Left = 260
  end
end
