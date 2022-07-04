object AxisPropsForm: TAxisPropsForm
  Left = 341
  Top = 104
  ActiveControl = PageControl
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Properties: Axis'
  ClientHeight = 292
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object OkBitBtn: TBitBtn
    Left = 88
    Top = 252
    Width = 65
    Height = 28
    TabOrder = 2
    Kind = bkOK
  end
  object CancelBitBtn: TBitBtn
    Left = 160
    Top = 252
    Width = 69
    Height = 28
    TabOrder = 3
    Kind = bkCancel
  end
  object HelpBitBtn: TBitBtn
    Left = 236
    Top = 252
    Width = 65
    Height = 28
    TabOrder = 4
    Kind = bkHelp
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 293
    Height = 233
    ActivePage = ScaleTabSheet
    HotTrack = True
    Images = ImageList
    TabOrder = 0
    object ScaleTabSheet: TTabSheet
      Caption = 'Scale'
      object ExpressionLabel: TLabel
        Left = 8
        Top = 156
        Width = 54
        Height = 13
        Caption = '&Expression:'
        FocusControl = ExpressionComboBox
      end
      object ManualScalePanel: TPanel
        Left = 8
        Top = 73
        Width = 269
        Height = 76
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 3
        object MinScaleLabel: TLabel
          Left = 8
          Top = 15
          Width = 44
          Height = 13
          Caption = 'M&inimum:'
          FocusControl = MinScaleFloatEdit
        end
        object MaxScaleLabel: TLabel
          Left = 8
          Top = 47
          Width = 47
          Height = 13
          Caption = 'Ma&ximum:'
          FocusControl = MaxScaleFloatEdit
        end
        object MinScaleDateTimePicker: TDateTimePicker
          Left = 72
          Top = 12
          Width = 185
          Height = 21
          HelpContext = 1004
          Date = 38771.868960497680000000
          Format = 'HH:mm:ss    dd.MM.yyyy'
          Time = 38771.868960497680000000
          Kind = dtkTime
          TabOrder = 2
          Visible = False
          OnChange = ScaleDateTimePickerChange
        end
        object MaxScaleDateTimePicker: TDateTimePicker
          Left = 72
          Top = 44
          Width = 185
          Height = 21
          HelpContext = 1004
          Date = 38771.868960497680000000
          Format = 'HH:mm:ss    dd.MM.yyyy'
          Time = 38771.868960497680000000
          Kind = dtkTime
          TabOrder = 3
          Visible = False
          OnChange = ScaleDateTimePickerChange
        end
        object MinScaleFloatEdit: TFloatEdit
          Left = 72
          Top = 12
          Width = 185
          Height = 22
          Hint = 
            'These fields used to define axis ranges in manual'#10'scaling mode (' +
            'minimum and maximum).'
          HelpContext = 1004
          Increment = 1.000000000000000000
          TabOrder = 0
          OnChange = ScaleFloatEditChange
          FType = ffGeneral
          MinWidth = 15
          Decimals = 7
          Multiply = False
        end
        object MaxScaleFloatEdit: TFloatEdit
          Left = 72
          Top = 44
          Width = 185
          Height = 22
          Hint = 
            'These fields used to define axis ranges in manual'#10'scaling mode (' +
            'minimum and maximum).'
          HelpContext = 1004
          Increment = 1.000000000000000000
          TabOrder = 1
          OnChange = ScaleFloatEditChange
          FType = ffGeneral
          MinWidth = 15
          Decimals = 7
          Multiply = False
        end
      end
      object ManualScaleRadioButton: TRadioButton
        Left = 20
        Top = 67
        Width = 106
        Height = 15
        Hint = 
          'If manual scaling selected, axis ranges are defined'#10'by user. Not' +
          'e that points out of range are invisible.'
        HelpContext = 1003
        Caption = 'Set scale &manualy'
        TabOrder = 2
      end
      object AutoScalePanel: TPanel
        Left = 8
        Top = 12
        Width = 269
        Height = 45
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 1
        object MarginsLabel: TLabel
          Left = 8
          Top = 16
          Width = 40
          Height = 13
          Caption = 'Mar&gins:'
          FocusControl = MarginsFloatEdit
        end
        object MarginsFloatEdit: TFloatEdit
          Left = 72
          Top = 12
          Width = 65
          Height = 22
          Hint = 
            'If Autoscale selected, this field allows you to expand'#10'automatic' +
            'ally calculated axis range by given factor.'
          HelpContext = 1002
          Increment = 0.050000000000000000
          MaxValue = 0.250000000000000000
          TabOrder = 0
          FType = ffGeneral
          MinWidth = 10
          Decimals = 4
          Multiply = False
        end
      end
      object AutoScaleRadioButton: TRadioButton
        Left = 20
        Top = 6
        Width = 129
        Height = 15
        Hint = 
          'If this mode selected, axis scale (minimum and'#10'maximum) will be ' +
          'calculated automatically.'
        HelpContext = 1001
        Caption = 'Set scale &automatically'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object ExpressionComboBox: TExpressionComboBox
        Left = 8
        Top = 176
        Width = 269
        Height = 21
        Hint = 
          'If axis has an expression, point coordinates in all'#10'series will ' +
          'be recalculated according to this expression'#10'(see expression syn' +
          'tax for more details).'
        HelpContext = 1005
        ItemHeight = 13
        TabOrder = 4
        DesignSize = (
          269
          21)
      end
    end
    object LabelsTabSheet: TTabSheet
      Caption = 'Labels'
      ImageIndex = 1
      object CaptionLabel: TLabel
        Left = 8
        Top = 176
        Width = 39
        Height = 13
        Caption = '&Caption:'
        FocusControl = CaptionEdit
      end
      object DateTimeFormatPanel: TPanel
        Left = 8
        Top = 12
        Width = 269
        Height = 101
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 3
        Visible = False
        object DateTimeFormatLabel: TLabel
          Left = 12
          Top = 18
          Width = 89
          Height = 13
          Caption = 'Date/Time Format:'
          FocusControl = DateTimeFormatComboBox
        end
        object DateTimeFormatHelpLabel: TLabel
          Left = 12
          Top = 72
          Width = 245
          Height = 13
          Cursor = crHandPoint
          AutoSize = False
          Caption = 'Click to learn more about date and time formats'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          OnMouseEnter = DateTimeFormatHelpLabelMouseEnter
          OnMouseLeave = DateTimeFormatHelpLabelMouseLeave
        end
        object SampleLabel1: TLabel
          Left = 12
          Top = 48
          Width = 245
          Height = 13
          Alignment = taCenter
          AutoSize = False
          Caption = 'SampleLabel1'
        end
        object DateTimeFormatComboBox: TComboBox
          Left = 112
          Top = 16
          Width = 141
          Height = 21
          Hint = 
            'Input date or/and time format specifier string'#10'or select from se' +
            'veral predefined formats.'
          HelpContext = 20
          ItemHeight = 13
          TabOrder = 0
          Text = 'c'
          OnChange = DateTimeFormatComboBoxChange
          Items.Strings = (
            'c'
            'ddddd'
            'dddddd'
            'hh:nn.ss'
            'hh:nn.ss.zzz'
            'dd/mm/yyyy hh:nn.ss'
            'dd/mm/yyyy hh:nn.ss.zzz')
        end
      end
      object FontBitBtn: TBitBtn
        Left = 8
        Top = 124
        Width = 96
        Height = 32
        Hint = 'Click this button to change label'#10'font (and color) attributes.'
        HelpContext = 1009
        Caption = 'F&ont'
        TabOrder = 4
        OnClick = FontBitBtnClick
        Glyph.Data = {
          F6020000424DF60200000000000076000000280000003C000000140000000100
          0400000000008002000000000000000000001000000000000000000000000000
          BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7777777777777777777777777777777777777777777777770000777779997997
          7777777777777999799777777777777778888887777777770000777797779777
          7CC77C777777977797777CC77C77777799989977744444770000777797779777
          C77CC777777797779777C77CC777777977798777CC44C7770000777797779777
          C777C777777797779777C777C77777798779877C77CC47770000777779997977
          C77CC777777779997977C77CC77777798879877C777C47770000777777777777
          7CC7C7777777777777777CC7C77777779997977C77CC47770000777777777777
          7777C7777777777777777777C777777777777777CC7C477700007AAA27777AAA
          2777C7777AAA27777AAA2777C777788887788888777C477700007BAA27777AA2
          7777C7777BAA27777AA27777C777AAA28778AAA2777C4777000077BA22222AA2
          C777C77777BA22222AA2C777C777BAA28888AA27777C4777000077BAAAAAAA27
          7CCC777777BAAAAAAA277CCC77777BA22222AA2C777C77770000777BA777AA27
          77777777777BA777AA27777777777BAAAAAAA277CCC777770000777BAA7AA277
          77777777777BAA7AA2777777777777BA888AA2777777777700007777BA7AA277
          777777777777BA7AA2777777777777BAA8AA27777777777700007777BAAA2777
          777777777777BAAA277777777777777BA8AA277777777777000077777BAA2777
          7777777777777BAA277777777777777BAAA2777777777777000077777BA27777
          7777777777777BA27777777777777777BAA27777777777770000777777A77777
          77777777777777A77777777777777777BA277777777777770000777777777777
          777777777777777777777777777777777A777777777777770000}
        NumGlyphs = 3
      end
      object CaptionEdit: TEdit
        Left = 52
        Top = 172
        Width = 197
        Height = 21
        Hint = 
          'Axis may have caption. If this field is not'#10'empty, axis caption ' +
          'will be visible.'
        HelpContext = 1010
        TabOrder = 7
        Text = 'CaptionEdit'
      end
      object LabelsCheckBox: TCheckBox
        Left = 120
        Top = 124
        Width = 157
        Height = 17
        Hint = 
          'If not checked, anything but axis and axis title'#10'(ticks, labels ' +
          'and grid lines) are not visible.'
        HelpContext = 1019
        Caption = 'Labels and ticks are visible'
        TabOrder = 5
        OnClick = LabelsCheckBoxClick
      end
      object NumericFormatPanel: TPanel
        Left = 8
        Top = 12
        Width = 269
        Height = 101
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 2
        object WidthLabel: TLabel
          Left = 12
          Top = 20
          Width = 66
          Height = 13
          Caption = '&Minimal width:'
          FocusControl = WidthSpinEdit
        end
        object DigitsLabel: TLabel
          Left = 12
          Top = 48
          Width = 68
          Height = 13
          Caption = '&Decimal digits:'
          FocusControl = DigitsSpinEdit
        end
        object SampleLabel: TLabel
          Left = 12
          Top = 76
          Width = 137
          Height = 14
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'SampleLabel'
        end
        object WidthSpinEdit: TSpinEdit
          Left = 112
          Top = 16
          Width = 40
          Height = 22
          Hint = 'Minimal width (in digits) of the label'
          HelpContext = 1006
          MaxValue = 20
          MinValue = 4
          TabOrder = 0
          Value = 8
          OnChange = FormatChange
        end
        object DigitsSpinEdit: TSpinEdit
          Left = 112
          Top = 44
          Width = 40
          Height = 22
          Hint = 'Number of places after decimal point'
          HelpContext = 1007
          MaxValue = 15
          MinValue = 0
          TabOrder = 1
          Value = 4
          OnChange = FormatChange
        end
        object FormatRadioGroup: TRadioGroup
          Left = 171
          Top = 10
          Width = 86
          Height = 81
          Hint = 
            'Type of the numeric format of the labels. The most'#10'universal and' +
            ' convenient is default General format.'
          HelpContext = 1008
          Caption = ' &Type '
          ItemIndex = 0
          Items.Strings = (
            'General'
            'Fixed'
            'Scientific')
          TabOrder = 2
          OnClick = FormatChange
        end
      end
      object NumericFormatRadioButton: TRadioButton
        Left = 20
        Top = 5
        Width = 97
        Height = 17
        Hint = 'Format column values as numbers'
        HelpContext = 1031
        Caption = 'Numeric Format'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = FormatRadioButtonClick
      end
      object DateTimeRadioButton: TRadioButton
        Left = 164
        Top = 5
        Width = 93
        Height = 17
        Hint = 'Format column values as date/time'
        HelpContext = 1032
        Caption = 'Date and Time'
        TabOrder = 1
        OnClick = FormatRadioButtonClick
      end
      object HTMLTextBitBtn: TBitBtn
        Left = 253
        Top = 171
        Width = 24
        Height = 24
        Hint = 
          'Invoke HTML Text Editor to apply'#10'more formatting and insert symb' +
          'ols'
        HelpContext = 1333
        TabOrder = 8
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
      object ShowExpressionCheckBox: TCheckBox
        Left = 120
        Top = 140
        Width = 157
        Height = 21
        Hint = 
          'If checked, axis expression will  be'#10'displayed in { } brackets a' +
          'fter title'
        HelpContext = 1334
        Caption = 'Show expression in caption'
        TabOrder = 6
      end
    end
    object LineTabSheet: TTabSheet
      Caption = 'View'
      ImageIndex = 2
      object LineWidthLabel: TLabel
        Left = 8
        Top = 100
        Width = 48
        Height = 13
        Caption = '&Width, px:'
        FocusControl = LineWidthSpinEdit
      end
      object TickLengthLabel: TLabel
        Left = 8
        Top = 132
        Width = 61
        Height = 13
        Caption = 'Ticks len&gth:'
        FocusControl = TickLengthFloatEdit
      end
      object LineWidthSpinEdit: TSpinEdit
        Left = 68
        Top = 96
        Width = 45
        Height = 22
        Hint = 'Axis line width, in pixels.'
        HelpContext = 1014
        MaxValue = 5
        MinValue = 1
        TabOrder = 1
        Value = 1
      end
      object TicksGroupBox: TGroupBox
        Left = 8
        Top = 5
        Width = 117
        Height = 80
        HelpContext = 2009
        Caption = ' Ticks count: '
        TabOrder = 0
        object MajorTicksLabel: TLabel
          Left = 12
          Top = 20
          Width = 29
          Height = 13
          Caption = 'Ma&jor:'
          FocusControl = MajorTicksSpinEdit
        end
        object MinorTicksLabel: TLabel
          Left = 12
          Top = 52
          Width = 29
          Height = 13
          Caption = 'M&inor:'
          FocusControl = MinorTicksSpinEdit
        end
        object MajorTicksSpinEdit: TSpinEdit
          Left = 60
          Top = 16
          Width = 45
          Height = 22
          Hint = 
            'Number of major (long) ticks. Note that actual number'#10'of major t' +
            'icks may differ if tick layout is not Normal.'
          HelpContext = 1011
          MaxValue = 100
          MinValue = 2
          TabOrder = 0
          Value = 5
        end
        object MinorTicksSpinEdit: TSpinEdit
          Left = 60
          Top = 48
          Width = 45
          Height = 22
          Hint = 'Number of minor (short) ticks'#10'between two major ticks.'
          HelpContext = 1012
          MaxValue = 50
          MinValue = 1
          TabOrder = 1
          Value = 2
        end
      end
      object GridCheckBox: TCheckBox
        Left = 8
        Top = 180
        Width = 97
        Height = 17
        Hint = 
          'If checked, grid lines at the positions'#10'of major ticks will be d' +
          'isplayed.'
        HelpContext = 1015
        Caption = '&Show grid lines'
        TabOrder = 4
      end
      object LineColorGroupBox: TGroupBox
        Left = 136
        Top = 5
        Width = 141
        Height = 100
        HelpContext = 2010
        Caption = ' Color: '
        TabOrder = 5
        object LineColorGrid: TColorGrid
          Left = 8
          Top = 16
          Width = 72
          Height = 72
          Hint = 
            'These controls used to change axis line color. Pick'#10'color from t' +
            'he table or click button for more colors.'
          HelpContext = 1013
          BackgroundEnabled = False
          TabOrder = 0
          OnChange = LineColorGridChange
        end
        object LineColorBitBtn: TBitBtn
          Left = 84
          Top = 16
          Width = 45
          Height = 52
          HelpContext = 1013
          Caption = '&More...'
          TabOrder = 1
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
          Layout = blGlyphTop
          NumGlyphs = 3
        end
      end
      object VisibleCheckBox: TCheckBox
        Left = 145
        Top = 180
        Width = 97
        Height = 17
        Hint = 'If not checked, ALL axis elements are invisible.'
        HelpContext = 1018
        Caption = 'Axis is visible'
        TabOrder = 7
      end
      object TicksRadioGroup: TRadioGroup
        Left = 136
        Top = 108
        Width = 141
        Height = 65
        Hint = 'Select major ticks layout.'
        HelpContext = 1020
        Caption = ' Ticks layout: '
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Normal'
          'Smart'
          'Log10')
        TabOrder = 6
        OnClick = TicksRadioGroupClick
      end
      object InnerTicksCheckBox: TCheckBox
        Left = 8
        Top = 156
        Width = 113
        Height = 17
        Hint = 'If checked, ticks are painted inside axis frame.'
        HelpContext = 1335
        Caption = 'Ticks inside frame'
        TabOrder = 3
      end
      object TickLengthFloatEdit: TFloatEdit
        Left = 76
        Top = 128
        Width = 49
        Height = 22
        Hint = 'Major tick length relative'#10'to the label font height.'
        HelpContext = 1337
        Increment = 0.100000000000000000
        MaxValue = 10.000000000000000000
        TabOrder = 2
        Value = 1.000000000000000000
        FType = ffGeneral
        MinWidth = 10
        Decimals = 4
        Multiply = False
      end
    end
  end
  object LinkedCheckBox: TCheckBox
    Left = 12
    Top = 256
    Width = 57
    Height = 17
    Hint = 'Links labels in this axis to'#10'another paired axis'
    HelpContext = 1336
    Caption = 'Linked'
    TabOrder = 1
    OnClick = LinkedCheckBoxClick
  end
  object ImageList: TImageList
    Left = 196
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FF000000FF000000FF000000FF00000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FF0000007B0000007B0000007B0000007B0000007B0000007B00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF0000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FF0000007B0000007B0000007B0000007B0000007B0000007B00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FF0000007B0000007B0000007B0000007B0000007B0000007B00000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      000000000000FF00000000000000000000000000000000000000FF0000000000
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FF0000007B0000007B0000007B0000007B0000007B0000007B00000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000000000000000000000000000000000000000000000000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FF0000007B0000007B0000007B0000007B0000007B0000007B00000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF0000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FF0000007B0000007B0000007B0000007B0000007B0000007B00000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000000000000000000000000000000000000000000000000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF0000FF000000FF000000FF000000FF000000FF000000FF00000000
      FF000000FF000000FF0000000000000000000000000000000000FF0000000000
      000000000000FF00000000000000000000000000000000000000FF0000000000
      000000000000FF00000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000FFFFFF00000000000000000000000000007B
      7B0000000000007B7B00FFFFFF00007B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FF
      F7EFFFFFC03FFFFFF7EFFFFFC03F00FFC003FE7FE07FFFFFF00FFE7FF0FF00FF
      F00FFFFFF987FFFFF00FDBDBFF0300FFF00FE7E7FF03FFFFF00F8181C10300FF
      F00FE7E7C103FFFFC003DBDBC187FFFFF7EFFFFFC1FFFFFFF7EFFFFFC1FFFFFF
      FFFFFFFFFFFF0000FFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Left = 252
  end
  object ColorDialog: TColorDialog
    Left = 224
  end
end
