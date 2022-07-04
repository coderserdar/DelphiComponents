object PlotDemoForm: TPlotDemoForm
  Left = 133
  Top = 70
  Width = 647
  Height = 658
  ActiveControl = ComboBox1
  Caption = 'Plot Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 639
    Height = 524
    Align = alClient
  end
  object Plot1: TPlot
    Left = 0
    Top = 0
    Width = 639
    Height = 524
    Align = alClient
    OnMouseDown = Plot1MouseDown
    Transparent = False
    XAxis.Max = 100.000000000000000000
    XAxis.Font.Charset = RUSSIAN_CHARSET
    XAxis.Font.Color = clWindowText
    XAxis.Font.Height = -11
    XAxis.Font.Name = 'Arial'
    XAxis.Font.Style = []
    XAxis.MinorTicks = 3
    XAxis.MajorTicks = 5
    XAxis.Title = 'x axis'
    YAxis.Max = 10000.000000000000000000
    YAxis.Font.Charset = RUSSIAN_CHARSET
    YAxis.Font.Color = clWindowText
    YAxis.Font.Height = -11
    YAxis.Font.Name = 'Arial'
    YAxis.Font.Style = []
    YAxis.MinorTicks = 9
    YAxis.MajorTicks = 4
    YAxis.LabelWidth = 4
    YAxis.LabelDecimals = 3
    YAxis.AutoScale = True
    YAxis.Title = 'Y axis title'
    YAxis.SmartTicks = True
    XAxis2.Max = 10.000000000000000000
    XAxis2.Font.Charset = DEFAULT_CHARSET
    XAxis2.Font.Color = clWindowText
    XAxis2.Font.Height = -11
    XAxis2.Font.Name = 'MS Sans Serif'
    XAxis2.Font.Style = []
    XAxis2.MinorTicks = 2
    XAxis2.MajorTicks = 5
    XAxis2.Margins = 0.100000000000000000
    XAxis2.Title = 'secondary X axis'
    YAxis2.Min = 1.000000000000000000
    YAxis2.Max = 90.000000000000000000
    YAxis2.Font.Charset = RUSSIAN_CHARSET
    YAxis2.Font.Color = clWindowText
    YAxis2.Font.Height = -11
    YAxis2.Font.Name = 'Arial'
    YAxis2.Font.Style = [fsItalic]
    YAxis2.MinorTicks = 3
    YAxis2.AutoScale = True
    YAxis2.Title = 'secondary Y axis'
    YAxis2.SmartTicks = True
    Series = <
      item
        PointSize = 10
        LastLine = 100
        XColumn = 1
        YColumn = 2
        Pen.Color = clBlue
        Brush.Color = clAqua
        Text = 'Data serie'
        Container = Container1
      end
      item
        PointType = ptCircle
        XColumn = 1
        YColumn = 2
        Pen.Color = clRed
        Brush.Color = clYellow
        Text = 'serie 1'
        Container = Container1
      end
      item
        Text = 'serie 2'
      end>
    SerieIndex = 0
    LeftMargin = 62
    RightMargin = 44
    TopMargin = 42
    BottomMargin = 45
    Labels = <
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Text = 
          'Doubleclick the plot to bring up ThisSerie and'#13#10'Top/Left/Botto' +
          'm/Right Axes dialog boxes'
        X1 = 0.300000000000000000
        Y1 = 0.150000000000000000
      end
      item
        LabelKind = lkArrow
        Pen.Color = clRed
        Pen.Style = psDashDot
        Brush.Color = clYellow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Text = 'Arrow can have a text'
        X1 = 70.000000000000000000
        X2 = 50.000000000000000000
        Y1 = 1000.000000000000000000
        Y2 = 2500.000000000000000000
      end
      item
        LabelKind = lkLegend
        Pen.Color = clRed
        Pen.Style = psDot
        Brush.Color = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Text = 'This is a plot legend'
        X1 = 0.150000000000000000
        Y1 = 0.600000000000000000
      end
      item
        LabelKind = lkArrow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        XAxis = TopAxis
        YAxis = RightAxis
        X1 = 7.000000000000000000
        X2 = 7.000000000000000000
        Y1 = 20.000000000000000000
        Y2 = 40.000000000000000000
      end
      item
        LabelKind = lkBalloon
        Pen.Color = clGreen
        Brush.Color = clLime
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clTeal
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Style = [fsItalic]
        Text = 
          'Balloon is a hybrid of text and arrow labels.'#13#10'Its second coor' +
          'dinate scaled, first - not.'
        X1 = 0.120000000000000000
        X2 = 50.000000000000000000
        Y1 = 0.250000000000000000
        Y2 = 2500.000000000000000000
      end>
    OnHint = Plot1Hint
    OnError = Plot1Error
    OnGetPoint = Plot1GetPoint
    OnPointClick = Plot1PointClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 524
    Width = 639
    Height = 85
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 312
      Top = 36
      Width = 62
      Height = 13
      Caption = 'MouseMode:'
    end
    object Label2: TLabel
      Left = 312
      Top = 12
      Width = 47
      Height = 13
      Caption = 'ThisSerie:'
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 109
      Height = 21
      Caption = 'Load Background'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 124
      Top = 32
      Width = 93
      Height = 21
      Hint = 'Invoke Plot1.Copy'
      Caption = 'Copy'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button4: TButton
      Left = 8
      Top = 32
      Width = 109
      Height = 21
      Hint = 'Invoke Plot1.UndoZoom'
      Caption = 'Undo Zoom'
      TabOrder = 2
      OnClick = Button4Click
    end
    object ComboBox1: TComboBox
      Left = 380
      Top = 32
      Width = 121
      Height = 21
      Hint = 'Set Plot1.MouseMode'
      Style = csDropDownList
      DropDownCount = 12
      ItemHeight = 13
      TabOrder = 3
      OnChange = ComboBox1Change
      Items.Strings = (
        'pmNone'
        'pmAutoZoom'
        'pmZoom'
        'pmRuler'
        'pmUnZoom'
        'pmSelect'
        'pmPointClick'
        'pmPointEdit'
        'pmPointDelete'
        'pmTranslate'
        'pmMargins'
        'pmLabelEdit')
    end
    object ComboBox2: TComboBox
      Left = 380
      Top = 8
      Width = 121
      Height = 21
      Hint = 'Set Plot1.SerieIndex'
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
      OnChange = ComboBox2Change
    end
    object CheckBox1: TCheckBox
      Left = 124
      Top = 60
      Width = 93
      Height = 17
      Hint = 'Checked in Container1.OnChanged'
      Caption = 'Data modified'
      TabOrder = 5
    end
    object Button5: TButton
      Left = 124
      Top = 8
      Width = 93
      Height = 21
      Hint = 'Invoke Plot1.Delete'
      Caption = 'Delete'
      TabOrder = 6
      OnClick = Button5Click
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 60
      Width = 109
      Height = 17
      TabOrder = 7
      Visible = False
    end
    object ColorGrid1: TColorGrid
      Left = 232
      Top = 8
      Width = 72
      Height = 68
      BackgroundEnabled = False
      TabOrder = 8
      OnChange = ColorGrid1Change
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 609
    Width = 639
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    OnHint = ApplicationEvents1Hint
    Left = 120
    Top = 48
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 92
    Top = 48
  end
  object Container1: TContainer
    FileName = 'test.dat'
    DataType = dtRealData
    UpdateCaption = False
    AutoLoad = False
    OnProgress = Container1Progress
    OnChanged = Container1Changed
    Left = 148
    Top = 48
    Data = {
      0165000000020000000000000000000000000000000000000000020000000000
      000080FF3F0000000000000080FF3F0200000000000000800040000000000000
      008001400200000000000000C000400000000000000090024002000000000000
      00800140000000000000008003400200000000000000A0014000000000000000
      C803400200000000000000C00140000000000000009004400200000000000000
      E0014000000000000000C4044002000000000000008002400000000000000080
      0540020000000000000090024000000000000000A205400200000000000000A0
      024000000000000000C805400200000000000000B0024000000000000000F205
      400200000000000000C00240000000000000009006400200000000000000D002
      4000000000000000A906400200000000000000E0024000000000000000C40640
      0200000000000000F0024000000000000000E106400200000000000000800340
      0000000000000080074002000000000000008803400000000000008090074002
      0000000000000090034000000000000000A20740020000000000000098034000
      000000000080B407400200000000000000A0034000000000000000C807400200
      000000000000A8034000000000000080DC07400200000000000000B003400000
      0000000000F207400200000000000000B8034000000000000040840840020000
      0000000000C00340000000000000009008400200000000000000C80340000000
      000000409C08400200000000000000D0034000000000000000A9084002000000
      00000000D8034000000000000040B608400200000000000000E0034000000000
      000000C408400200000000000000E8034000000000000040D208400200000000
      000000F0034000000000000000E108400200000000000000F803400000000000
      0040F00840020000000000000080044000000000000000800940020000000000
      0000840440000000000000208809400200000000000000880440000000000000
      8090094002000000000000008C04400000000000002099094002000000000000
      0090044000000000000000A20940020000000000000094044000000000000020
      AB0940020000000000000098044000000000000080B409400200000000000000
      9C044000000000000020BE09400200000000000000A0044000000000000000C8
      09400200000000000000A4044000000000000020D209400200000000000000A8
      044000000000000080DC09400200000000000000AC044000000000000020E709
      400200000000000000B0044000000000000000F209400200000000000000B404
      4000000000000020FD09400200000000000000B8044000000000000040840A40
      0200000000000000BC0440000000000000108A0A400200000000000000C00440
      00000000000000900A400200000000000000C4044000000000000010960A4002
      00000000000000C80440000000000000409C0A400200000000000000CC044000
      000000000090A20A400200000000000000D0044000000000000000A90A400200
      000000000000D4044000000000000090AF0A400200000000000000D804400000
      0000000040B60A400200000000000000DC044000000000000010BD0A40020000
      0000000000E0044000000000000000C40A400200000000000000E40440000000
      00000010CB0A400200000000000000E8044000000000000040D20A4002000000
      00000000EC044000000000000090D90A400200000000000000F0044000000000
      000000E10A400200000000000000F4044000000000000090E80A400200000000
      000000F8044000000000000040F00A400200000000000000FC04400000000000
      0010F80A40020000000000000080054000000000000000800B40020000000000
      000082054000000000000008840B400200000000000000840540000000000000
      20880B400200000000000000860540000000000000488C0B4002000000000000
      0088054000000000000080900B4002000000000000008A0540000000000000C8
      940B4002000000000000008C054000000000000020990B400200000000000000
      8E0540000000000000889D0B40020000000000000090054000000000000000A2
      0B40020000000000000092054000000000000088A60B40020000000000000094
      054000000000000020AB0B400200000000000000960540000000000000C8AF0B
      40020000000000000098054000000000000080B40B4002000000000000009A05
      4000000000000048B90B4002000000000000009C054000000000000020BE0B40
      02000000000000009E054000000000000008C30B400200000000000000A00540
      00000000000000C80B400200000000000000A2054000000000000008CD0B4002
      00000000000000A4054000000000000020D20B400200000000000000A6054000
      000000000048D70B400200000000000000A8054000000000000080DC0B400200
      000000000000AA0540000000000000C8E10B400200000000000000AC05400000
      0000000020E70B400200000000000000AE054000000000000088EC0B40020000
      0000000000B0054000000000000000F20B400200000000000000B20540000000
      00000088F70B400200000000000000B4054000000000000020FD0B4002000000
      00000000B6054000000000000064810C400200000000000000B8054000000000
      000040840C400200000000000000BA054000000000000024870C400200000000
      000000BC0540000000000000108A0C400200000000000000BE05400000000000
      00048D0C400200000000000000C0054000000000000000900C40020000000000
      0000C2054000000000000004930C400200000000000000C40540000000000000
      10960C400200000000000000C6054000000000000024990C4002000000000000
      00C80540000000000000409C0C40}
  end
end
