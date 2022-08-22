object MainForm: TMainForm
  Left = 235
  Top = 116
  Width = 869
  Height = 661
  Caption = 'Path Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  HelpFile = '.\PathTest.hlp'
  Menu = MainMenu1
  OldCreateOrder = False
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 494
    Width = 861
    Height = 109
    Align = alBottom
    TabOrder = 0
    object lblTimes: TLabel
      Left = 704
      Top = 16
      Width = 7
      Height = 16
      Caption = '1'
    end
    object lblHeuristic: TLabel
      Left = 704
      Top = 48
      Width = 7
      Height = 16
      Caption = '1'
    end
    object btnFindPath: TButton
      Left = 424
      Top = 16
      Width = 75
      Height = 25
      Hint = 
        'Find path (non-A*)|Find Path Between Start and End (non-A*.  Doe' +
        's not consider terrain costs)'
      Action = actFindPath
      TabOrder = 0
    end
    object StatusBar1: TStatusBar
      Left = 1
      Top = 80
      Width = 859
      Height = 28
      Panels = <>
      SimplePanel = True
    end
    object btnStep: TButton
      Left = 424
      Top = 48
      Width = 75
      Height = 25
      Action = actStepPath
      TabOrder = 2
    end
    object btnClear: TButton
      Left = 720
      Top = 8
      Width = 75
      Height = 17
      Action = actClear
      TabOrder = 3
    end
    object btnReset: TButton
      Left = 720
      Top = 32
      Width = 75
      Height = 17
      Action = actReset
      TabOrder = 4
    end
    object gbBrushes: TGroupBox
      Left = 1
      Top = 1
      Width = 416
      Height = 79
      Align = alLeft
      Caption = '&Brushes'
      TabOrder = 5
      object sbStart: TSpeedButton
        Tag = 100
        Left = 8
        Top = 24
        Width = 41
        Height = 41
        Hint = 'Set Start|Select to place start point of path'
        AllowAllUp = True
        GroupIndex = 10
        Down = True
        Caption = '&Start'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033BBBBBBBBBB
          BB33337777777777777F33BB00BBBBBBBB33337F77333333F37F33BB0BBBBBB0
          BB33337F73F33337FF7F33BBB0BBBB000B33337F37FF3377737F33BBB00BB00B
          BB33337F377F3773337F33BBBB0B00BBBB33337F337F7733337F33BBBB000BBB
          BB33337F33777F33337F33EEEE000EEEEE33337F3F777FFF337F33EE0E80000E
          EE33337F73F77773337F33EEE0800EEEEE33337F37377F33337F33EEEE000EEE
          EE33337F33777F33337F33EEEEE00EEEEE33337F33377FF3337F33EEEEEE00EE
          EE33337F333377F3337F33EEEEEE00EEEE33337F33337733337F33EEEEEEEEEE
          EE33337FFFFFFFFFFF7F33EEEEEEEEEEEE333377777777777773}
        Layout = blGlyphTop
        NumGlyphs = 2
        OnClick = actSetTerrainExecute
      end
      object sbEnd: TSpeedButton
        Tag = 101
        Left = 56
        Top = 24
        Width = 41
        Height = 41
        Hint = 'Set Goal|Select to set end point of path'
        AllowAllUp = True
        GroupIndex = 10
        Caption = '&Goal'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333FFFFF3333333333000003333333333F777773FF333333008877700
          33333337733FFF773F33330887000777033333733F777FFF73F330880F9F9F07
          703337F37733377FF7F33080F00000F07033373733777337F73F087F0091100F
          77037F3737333737FF7F08090919110907037F737F3333737F7F0F0F0999910F
          07037F737F3333737F7F0F090F99190908037F737FF33373737F0F7F00FF900F
          780373F737FFF737F3733080F00000F0803337F73377733737F330F80F9F9F08
          8033373F773337733733330F8700078803333373FF77733F733333300FFF8800
          3333333773FFFF77333333333000003333333333377777333333}
        Layout = blGlyphTop
        NumGlyphs = 2
        OnClick = actSetTerrainExecute
      end
      object sbClear: TSpeedButton
        Left = 104
        Top = 24
        Width = 41
        Height = 41
        Hint = 'Set Clear|Select to draw clear terrain'
        AllowAllUp = True
        GroupIndex = 10
        Caption = '&Clear'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00500005000555
          555557777F777555F55500000000555055557777777755F75555005500055055
          555577F5777F57555555005550055555555577FF577F5FF55555500550050055
          5555577FF77577FF555555005050110555555577F757777FF555555505099910
          555555FF75777777FF555005550999910555577F5F77777775F5500505509990
          3055577F75F77777575F55005055090B030555775755777575755555555550B0
          B03055555F555757575755550555550B0B335555755555757555555555555550
          BBB35555F55555575F555550555555550BBB55575555555575F5555555555555
          50BB555555555555575F555555555555550B5555555555555575}
        Layout = blGlyphTop
        NumGlyphs = 2
        OnClick = actSetTerrainExecute
      end
      object sbWalls: TSpeedButton
        Tag = 5
        Left = 152
        Top = 24
        Width = 41
        Height = 41
        Hint = 'Set Walls|Select to draw walls (impassable terrain)'
        AllowAllUp = True
        GroupIndex = 10
        Caption = '&Walls'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          33333F3F3F3F3F3F3F3F03030303030303037373737373737373333333333333
          33333F3FFFFFFFFFFF3F03000000000003037377777777777F73330800808080
          03333F7F77F7F7F77F3F0308008080800303737F77F737F77F73330080080008
          03333F77F77F777F7F3F03000000000003037377777777777373333333333333
          33333F3F3F3F3F3F3F3F03030303030303037373737373737373333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          3333333333333333333333333333333333333333333333333333}
        Layout = blGlyphTop
        NumGlyphs = 2
        OnClick = actSetTerrainExecute
      end
      object pnlFour: TPanel
        Tag = 3
        Left = 326
        Top = 24
        Width = 41
        Height = 41
        Hint = 
          'Set forest|Select to draw forests (slower than brush, faster tha' +
          'n swamp)'
        HelpContext = 110
        BevelWidth = 2
        Caption = '&Forest'
        Color = clGreen
        TabOrder = 0
        OnClick = actSetTerrainExecute
      end
      object pnlThree: TPanel
        Tag = 2
        Left = 284
        Top = 24
        Width = 41
        Height = 41
        Hint = 
          'Set brush|Select to draw brush (slower than plains, faster than ' +
          'forest)'
        HelpContext = 120
        BevelWidth = 2
        Caption = '&Brush'
        Color = clOlive
        TabOrder = 1
        OnClick = actSetTerrainExecute
      end
      object pnlTwo: TPanel
        Tag = 1
        Left = 242
        Top = 24
        Width = 41
        Height = 41
        Hint = 
          'Set plains|Select to draw plains (slower than roads, faster than' +
          ' brush)'
        HelpContext = 130
        BevelWidth = 2
        Caption = 'Plai&n'
        Color = clLime
        TabOrder = 2
        OnClick = actSetTerrainExecute
      end
      object pnlOne: TPanel
        Left = 200
        Top = 24
        Width = 41
        Height = 41
        Hint = 'Set road|Select to draw roads (clear terrain)'
        HelpContext = 140
        BevelWidth = 2
        Caption = '&Road'
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = actSetTerrainExecute
      end
      object pnlFive: TPanel
        Tag = 4
        Left = 368
        Top = 24
        Width = 41
        Height = 41
        Hint = 'Set Swamp|Select to draw swamp (slowest terrain)'
        HelpContext = 150
        BevelWidth = 2
        Caption = 'Swam&p'
        Color = clGray
        TabOrder = 4
        OnClick = actSetTerrainExecute
      end
    end
    object btnAStarFindPath: TButton
      Left = 504
      Top = 16
      Width = 73
      Height = 25
      Action = actFindAStarPath
      TabOrder = 6
    end
    object tbRepetitions: TTrackBar
      Left = 584
      Top = 16
      Width = 121
      Height = 25
      Hint = 'Repetitions'
      HelpContext = 210
      Max = 100
      Min = 1
      Orientation = trHorizontal
      Frequency = 5
      Position = 1
      SelEnd = 0
      SelStart = 0
      TabOrder = 7
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = tbRepetitionsChange
    end
    object tbHeuristic: TTrackBar
      Left = 584
      Top = 48
      Width = 121
      Height = 25
      Hint = 'Set the Heuristic weight'
      HelpContext = 220
      Max = 100
      Min = 1
      Orientation = trHorizontal
      Frequency = 5
      Position = 10
      SelEnd = 0
      SelStart = 0
      TabOrder = 8
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = tbHeuristicChange
    end
    object btnAverage: TButton
      Left = 504
      Top = 48
      Width = 75
      Height = 25
      Hint = 'Set Heuristic to Terrain Average Cost (A*)'
      HelpContext = 90
      Caption = 'Average'
      TabOrder = 9
      OnClick = btnAverageClick
    end
    object btnRandomize: TButton
      Left = 720
      Top = 56
      Width = 75
      Height = 17
      Hint = 'Randomize start and stop locations'
      HelpContext = 100
      Caption = 'Randomize'
      TabOrder = 10
      OnClick = btnRandomizeClick
    end
    object BitBtn1: TBitBtn
      Left = 808
      Top = 16
      Width = 49
      Height = 49
      HelpContext = 1
      TabOrder = 11
      Kind = bkHelp
      Layout = blGlyphTop
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 861
    Height = 494
    ActivePage = tsSimple
    Align = alClient
    TabOrder = 1
    TabPosition = tpBottom
    OnChange = PageControl1Change
    object tsSimple: TTabSheet
      Hint = 'Draw Map'
      Caption = 'Simple'
      object sgMap: TStringGrid
        Left = 0
        Top = 0
        Width = 853
        Height = 463
        HelpContext = 230
        Align = alClient
        ColCount = 65
        DefaultColWidth = 12
        DefaultRowHeight = 12
        FixedCols = 0
        RowCount = 36
        FixedRows = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssNone
        TabOrder = 0
        OnDrawCell = DrawCell
        OnMouseDown = sgMapMouseDown
        OnMouseMove = sgMapMouseMove
        OnMouseUp = sgMapMouseUp
      end
    end
    object tsBitmap: TTabSheet
      Hint = 'Display gray scale bitmap (used for terrain costs)'
      Caption = 'Bitmap'
      ImageIndex = 1
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 853
        Height = 463
        Align = alClient
        TabOrder = 0
        object Image1: TPaintBox
          Left = 0
          Top = 0
          Width = 849
          Height = 459
          Align = alClient
          OnMouseDown = Image1MouseDown
          OnMouseMove = Image1MouseMove
          OnPaint = Image1Paint
        end
        object Image2: TImage
          Left = 480
          Top = 72
          Width = 105
          Height = 105
          Visible = False
        end
      end
    end
  end
  object ActionList1: TActionList
    Left = 344
    Top = 24
    object actFindPath: TAction
      Caption = 'Find &Path'
      HelpContext = 11
      Hint = 'Find Path|Find Path Between Start and End (non-A*)'
      ShortCut = 16464
      OnExecute = actFindPathExecute
    end
    object actStepPath: TAction
      Caption = '&One Step'
      HelpContext = 20
      Hint = 'Next Step|Try one step (non-A*)'
      OnExecute = actStepPathExecute
    end
    object actClear: TAction
      Caption = '&New'
      HelpContext = 30
      Hint = 'Clear map|Clear the map completely'
      ShortCut = 16462
      OnExecute = actClearExecute
    end
    object actReset: TAction
      Caption = '&Reset'
      HelpContext = 40
      Hint = 'Reset map|Reset the map (Clear paths and blocked terrain)'
      ShortCut = 16466
      OnExecute = actResetExecute
    end
    object actFindAStarPath: TAction
      Caption = 'Find (&A*)'
      HelpContext = 50
      Hint = 'Find Path (A*)|Use A* search to find the path'
      ShortCut = 16449
      OnExecute = actFindAStarPathExecute
    end
    object actSave: TAction
      Caption = '&Save'
      HelpContext = 60
      Hint = 'Save Map'
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actSaveAs: TAction
      Caption = 'Save &As...'
      HelpContext = 70
      Hint = 'Save Map as new filename'
      OnExecute = actSaveAsExecute
    end
    object actOpen: TAction
      Caption = '&Open'
      HelpContext = 80
      Hint = 'Open Map'
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
  end
  object ImageList1: TImageList
    Left = 224
    Top = 24
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001001000000000000008
      00000000000000000000000000000000000000000000E07FE07FE07FE07FE07F
      E07FE07FE07FE07FE07FE07FE07F000000000000000000000000000000000000
      0000000000000000000000000000000000000F3C0F3C0F3C0F3C0F3C00000000
      0F3C0F3C0F3C0000007C007C007C007C003C00000F3C0F3C0F3C0F3C0F3C0F3C
      0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C00000000E07FE07F00000000E07F
      E07FE07FE07FE07FE07FE07FE07F000000000000000000000000000000000000
      0000000000000000000000000000000000000F3C0F3C0F3C0F3C0F3C0000E07F
      0000E07F0000E03D00000F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0000
      00000F3C00000F3C0F3C0000007C0000E07F00000000E07FE07F0000E07FE07F
      E07FE07FE07FE07F0000E07FE07F000000000000000000000000000000000000
      0000000000000000000000000000000000000000E07FE07FE07F0F3C0F3C0F3C
      0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C
      0000E07FE07FE07FE03D0F3C0F3C0F3C0F3C00000000E07FE07FE07F0000E07F
      E07FE07FE07F000000000000E07F000000000000000000000000000000000000
      0000000000000000000000000000000000000F3C0F3C0F3C0F3C0F3C0F3C0F3C
      0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0000E07F0F3C0F3C0F3C
      0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C00000000E07FE07FE07F00000000
      E07FE07F00000000E07FE07FE07F000000000000000000000000000000000000
      0000000000000000000000000000000000000F3C0F3C0F3C0F3C0F3C0F3C0F3C
      0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C
      0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C0F3C00000000E07FE07FE07FE07F0000
      E07F00000000E07FE07FE07FE07F000000000000000000000000000000000000
      0000003C003C000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F00000000E07FE07FE07FE07F0000
      00000000E07FE07FE07FE07FE07F000000000000000000000000000000000000
      007C007C007C003C00000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F00000000FF03FF03FF03FF030000
      00000000FF03FF03FF03FF03FF03000000000000000000000000000000000000
      007C007C007C007C003C0000000000000000000000000000FF7FFF7F0000FF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F000000000000
      00000000000000000000FF7FFF7FFF7F000000000000FF03FF030000FF03F75E
      0000000000000000FF03FF03FF03000000000000000000000000000000000000
      0000007C007C007C0000E03D000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F00000000FF7FFF7F00000000FF7F00000000FF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F00000000FF03FF03FF030000F75E
      00000000FF03FF03FF03FF03FF03000000000000000000000000000000000000
      00000000007C0000E07F0000E03D00000000FF7FFF7FFF7FFF7FFF7F00000000
      FF7FFF7FFF7F0000007C007C007C007C003C0000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F00000000FF03FF03FF03FF030000
      00000000FF03FF03FF03FF03FF03000000000000000000000000000000000000
      000000000000E07F0000E07F0000E03D0000FF7FFF7FFF7FFF7FFF7F0000E07F
      0000E07F0000E03D0000FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F0000
      0000FF7F0000FF7FFF7F0000007C0000E07F00000000FF03FF03FF03FF03FF03
      00000000FF03FF03FF03FF03FF03000000000000000000000000000000000000
      0000000000000000E07F0000E07FE03DE03D0000E07FE07FE07FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F
      0000E07FE07FE07FE03DFF7FFF7FFF7FFF7F00000000FF03FF03FF03FF03FF03
      FF0300000000FF03FF03FF03FF03000000000000000000000000000000000000
      00000000000000000000E07FE07FE07FE03DFF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F0000E07FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F00000000FF03FF03FF03FF03FF03
      FF0300000000FF03FF03FF03FF03000000000000000000000000000000000000
      000000000000000000000000E07FE07FE07FFF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F00000000FF03FF03FF03FF03FF03
      FF03FF03FF03FF03FF03FF03FF03000000000000000000000000000000000000
      0000000000000000000000000000E07FE07FEF3DFF7F00000000000000000000
      00000000FF7FFF7FEF3DFF7FEF3DEF3DEF3DEF3DFF7F00000000000000000000
      00000000FF7FFF7FEF3DFF7FEF3DEF3DEF3D00000000FF03FF03FF03FF03FF03
      FF03FF03FF03FF03FF03FF03FF03000000000000000000000000000000000000
      00000000000000000000000000000000E07FEF3DFF7F00000000000000000000
      0000EF3DEF3DEF3DEF3DEF3DEF3DEF3D0000EF3DFF7F00000000000000000000
      0000EF3DEF3DEF3DEF3DEF3DEF3DEF3D0000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00C003847F00000000C00300EF00000000
      C00331BF00000000C00339FF00000000C003993F00000000C003CA1F00000000
      C003F40F00000000C0039C0700000000C003960300000000C003CB0100000000
      C003FF8000000000C003F7C000000000C003FFE000000000C003EFF000000000
      C003FFF800000000C003FFFC5A23570300000000000000000000000000000000
      000000000000}
  end
  object MainMenu1: TMainMenu
    Left = 248
    Top = 24
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Action = actClear
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Open1: TMenuItem
        Action = actOpen
      end
      object Save1: TMenuItem
        Action = actSave
      end
      object SaveAs1: TMenuItem
        Action = actSaveAs
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Terrain1: TMenuItem
      Caption = '&Terrain'
      object Road1: TMenuItem
        Caption = '&Road'
        GroupIndex = 50
        HelpContext = 140
        Hint = 'Set road|Select to draw roads (clear terrain)'
        RadioItem = True
        OnClick = actSetTerrainExecute
      end
      object Plain1: TMenuItem
        Tag = 1
        Caption = 'Plai&n'
        GroupIndex = 50
        HelpContext = 130
        Hint = 
          'Set plains|Select to draw plains (slower than roads, faster than' +
          ' brush)'
        RadioItem = True
        OnClick = actSetTerrainExecute
      end
      object Brush1: TMenuItem
        Tag = 2
        Caption = '&Brush'
        GroupIndex = 50
        HelpContext = 120
        Hint = 
          'Set brush|Select to draw brush (slower than plains, faster than ' +
          'forest)'
        RadioItem = True
        OnClick = actSetTerrainExecute
      end
      object Forest1: TMenuItem
        Tag = 3
        Caption = '&Forest'
        GroupIndex = 50
        HelpContext = 110
        Hint = 
          'Set forest|Select to draw forests (slower than brush, faster tha' +
          'n swamp)'
        RadioItem = True
        OnClick = actSetTerrainExecute
      end
      object Swamp1: TMenuItem
        Tag = 4
        Caption = 'Swam&p'
        GroupIndex = 50
        HelpContext = 150
        Hint = 'Set Swamp|Select to draw swamp (slowest terrain)'
        RadioItem = True
        OnClick = actSetTerrainExecute
      end
      object Wall1: TMenuItem
        Tag = 5
        Caption = '&Wall'
        GroupIndex = 50
        HelpContext = 160
        Hint = 'Set Walls|Select to draw walls (impassable terrain)'
        RadioItem = True
        OnClick = actSetTerrainExecute
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 50
      end
      object Start1: TMenuItem
        Tag = 100
        Caption = '&Start'
        Checked = True
        GroupIndex = 50
        HelpContext = 170
        Hint = 'Set Start|Select to place start point of path'
        RadioItem = True
        OnClick = actSetTerrainExecute
      end
      object Goal1: TMenuItem
        Tag = 101
        Caption = '&Goal'
        GroupIndex = 50
        HelpContext = 180
        Hint = 'Set Goal|Select to set end point of path'
        RadioItem = True
        OnClick = actSetTerrainExecute
      end
    end
    object Debug1: TMenuItem
      Caption = '&Debug'
      object Sleep1: TMenuItem
        Caption = '&Sleep'
        HelpContext = 190
        Hint = 
          'Set sleep time (in milliseconds) between expansion of nodes in A' +
          '* search (debug only)'
        object N01: TMenuItem
          Caption = '0'
          GroupIndex = 51
          RadioItem = True
          OnClick = N10001Click
        end
        object N101: TMenuItem
          Tag = 10
          Caption = '10'
          GroupIndex = 51
          RadioItem = True
          OnClick = N10001Click
        end
        object N251: TMenuItem
          Tag = 25
          Caption = '25'
          Checked = True
          GroupIndex = 51
          RadioItem = True
          OnClick = N10001Click
        end
        object N501: TMenuItem
          Tag = 50
          Caption = '50'
          GroupIndex = 51
          RadioItem = True
          OnClick = N10001Click
        end
        object N751: TMenuItem
          Tag = 75
          Caption = '75'
          GroupIndex = 51
          RadioItem = True
          OnClick = N10001Click
        end
        object N1001: TMenuItem
          Tag = 100
          Caption = '100'
          GroupIndex = 51
          RadioItem = True
          OnClick = N10001Click
        end
        object N2501: TMenuItem
          Tag = 250
          Caption = '250'
          GroupIndex = 51
          RadioItem = True
          OnClick = N10001Click
        end
        object N5001: TMenuItem
          Tag = 500
          Caption = '500'
          GroupIndex = 51
          RadioItem = True
          OnClick = N10001Click
        end
        object N10001: TMenuItem
          Tag = 1000
          Caption = '1000'
          GroupIndex = 51
          RadioItem = True
          OnClick = N10001Click
        end
      end
      object ShowSearches1: TMenuItem
        Caption = 'S&how Searches'
        HelpContext = 200
        Hint = 'Show searches for A*'
        OnClick = ShowSearches1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'map'
    Filter = 
      'Map Files|*.map;*.bmp|Map File (*.map)|*.map|Bitmap File (*.bmp)' +
      '|*.bmp|Text File (*.txt)|*.txt|Any File (*.*)|*.*'
    Left = 280
    Top = 24
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'map'
    Filter = 
      'Map File (*.map)|*.map|Text File (*.txt)|*.txt|Any File (*.*)|*.' +
      '*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 312
    Top = 24
  end
  object ApplicationEvents1: TApplicationEvents
    OnHint = ApplicationEvents1Hint
    Left = 192
    Top = 24
  end
  object SimplePathPlanner1: TSimplePathPlanner
    OnMapPassability = SimplePathPlanner1MapPassability
    OnBlockLocation = SimplePathPlanner1BlockLocation
    OnPrepareMap = SimplePathPlanner1PrepareMap
    OnValidLocationCheck = SimplePathPlanner1ValidLocationCheck
    StartX = 0
    StartY = 0
    EndX = 0
    EndY = 0
    Left = 192
    Top = 64
  end
  object AStarPathPlanner1: TAStarPathPlanner
    StateFactory = SearchableMap1
    Left = 224
    Top = 64
  end
  object SearchableMap1: TSearchableMap
    Width = 65
    Height = 36
    Weight = 1
    Left = 256
    Top = 64
  end
end
