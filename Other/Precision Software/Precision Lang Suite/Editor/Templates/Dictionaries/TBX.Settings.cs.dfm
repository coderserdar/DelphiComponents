object PSForm: TPSForm
  Left = 276
  Top = 108
  Caption = 'TBX (Term Base eXchange)'
  ClientHeight = 380
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001001800680300001600000028000000100000002000
    0000010018000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000CEA051
    CEA051000000000000000000CEA051CEA0510000000000000000000000000000
    00000000000000000000CE985CCEA051CEA051CEA051000000CEA051CEA051CE
    A051CEA051000000000000000000000000000000000000000000000000CE9C57
    CEA051CEA051CEA051CEA051CEA051CE9F510000000000000000000000000000
    00000000000000000000000000000000CEA051CEA051CEA051CEA051CEA05100
    0000000000000000000000000000000000000000000000000000000000000000
    000000CEA051CEA051CEA0510000000000000000000000000000000000000000
    00000000CECE87CECE87CECE87000000CE9E53CEA051CEA051CEA051CE9E536F
    B6CE6FB8CE6FB8CE6FB6CE000000000000000000CECE87CECE87CECE87CE9E53
    CEA051CEA051CE9E53CEA151CEA051CE9E536FB8CE6FB8CE6FB8CE6FABCE0000
    00000000CECE87CECE87CE9E53CE9E51CEA051CE9E536FB8CECE9E53CEA051CE
    A051CE9E536FB8CE6FB8CE6FB8CE000000000000CECE87CECE87CECE74CE9E53
    CEA0510000006FB8CE6FB6CECE9E53CE9E536FB8CE6FB8CE6FB8CE6FB3CE0000
    00000000CECE87CECE87CECE870000000000000000006FB8CE6FB8CE6FB8CE6F
    B8CE6FB8CE6FB8CE6FB3CE000000000000000000CECE87CECE87CECE87000000
    0000000000006FB8CE6FB8CE6FA3CE6FA6CE6FB8CE6FB8CE6FB8CE6F9ECE0000
    00000000CECE87CECE87CECE870000000000000000006FB8CE6FB8CE6F9BCE00
    00000000006FB8CE6FB8CE6FB8CECECE87CECE87CECE87CECE87CECE87CECE87
    CECE870000006FB8CE6FB8CE6FB8CE6FB8CE6FB8CE6FB8CE6FB8CE6FB0CECECE
    87CECE87CECE87CECE87CECE87CECE87CECE870000006FB8CE6FB8CE6FB8CE6F
    B8CE6FB3CE6FAECE6FB8CE000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    AC41F9CFAC41F087AC41F80FAC41FC1FAC41FE3FAC41C401AC41C000AC41C000
    AC41C100AC41C701AC41C700AC41C718AC410100AC410101AC41FFFFAC41}
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    522
    380)
  PixelsPerInch = 96
  TextHeight = 13
  object lbProgress: TLabel
    Left = 240
    Top = 320
    Width = 118
    Height = 13
    Caption = 'Prob'#237'h'#225' anal'#253'za souboru'
    Visible = False
  end
  object lbFileFilter: TLabel
    Left = 240
    Top = 304
    Width = 972
    Height = 13
    Caption = 
      'Podporovan'#233' typy soubor'#367' (*.tbx, *.xml, *.htm)|*.tbx;*.xml;*.htm' +
      ';*.html|TBX soubory (*.tbx)|*.tbx|XML soubory (*.xml)|*.xml|HTML' +
      ' soubory (*.htm, *.html)|*.htm;*.html|V'#353'echny soubory (*.*)|*.*'
    Visible = False
  end
  object Label1: TLabel
    Left = 16
    Top = 68
    Width = 67
    Height = 13
    Caption = 'Soubory TMX:'
  end
  object Label6: TLabel
    Left = 16
    Top = 312
    Width = 69
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'V'#237'ce informac'#237':'
  end
  object lbHomePage: TLabel
    Left = 96
    Top = 312
    Width = 182
    Height = 13
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'http://en.wikipedia.org/wiki/Tbx#TMX'
  end
  object Bevel2: TBevel
    AlignWithMargins = True
    Left = 0
    Top = 338
    Width = 522
    Height = 2
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 40
    Align = alBottom
    Shape = bsBottomLine
    ExplicitTop = 268
  end
  object sbOK: TButton
    Left = 356
    Top = 348
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 7
  end
  object sbCancel: TButton
    Left = 436
    Top = 348
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Storno'
    TabOrder = 8
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 522
    Height = 53
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 9
    object Bevel1: TBevel
      Left = 0
      Top = 51
      Width = 522
      Height = 2
      Align = alBottom
      Shape = bsBottomLine
      ExplicitTop = 3
      ExplicitWidth = 586
    end
    object Image1: TImage
      Left = 450
      Top = 0
      Width = 72
      Height = 51
      Align = alRight
      Center = True
      Picture.Data = {
        07544269746D617026240000424D262400000000000036000000280000003200
        00002E0000000100200000000000F02300000000000000000000000000000000
        0000FDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFEAE8E7FFE5E5
        E5FFFDFDFDFFF3F3F3FFB2B2B2FFDAD9D8FFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFEAE8
        E7FFC7C7C7FFC7C7C7FFF6D8D8FFB3B0AEFF979696FFA69B99FFD9D8D7FFFDFC
        F7FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFBE7E7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFB3B0AEFF979696FF9796
        96FF979696FFD9D8D7FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFE5E5E5FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFB3B0AEFF979696FF979696FF979696FFDAD9D8FFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFBE7E7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFB3B0AEFF979696FF979696FFA69B99FFDAD9
        D8FFFDFCF7FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFEAE8E7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFB3B0AEFF9796
        96FF979696FF979696FFDAD9D8FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFCF7FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFCF7FFEAE8E7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFE7C9C9FFC7C7C7FFC7C7C7FFC7C7C7FFE7C9C9FFC7C7
        C7FFC7C7C7FFB3B0AEFF979696FF979696FF979696FFDAD9D8FFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFCF7FFFDFDFDFFFDFDFDFFFDFD
        FDFFF4FBFBFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFE5E5E5FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC4B5AAFF958D8DFF958D8DFF9796
        96FFD4D4D3FFFDFCF7FFFDFCF7FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFD7D4EFFF9893
        F0FF9893F0FF9893F0FF9893F0FF9893F0FF9893F0FF9893F0FF8889FCFF8080
        D5FF736DE2FF8080D5FF736DE2FF8080D5FF736DE2FF736DE2FF8080D5FF736D
        E2FF736DE2FF8080D5FF736DE2FF8080D5FF736DE2FF8080D5FF736DE2FF686A
        C9FF686AC9FF5451C7FF686AC9FF837DF1FF9893F0FF8889FCFF9893F0FF9893
        F0FF8889FCFF9893F0FF9893F0FFA09FEDFFFDFCF7FFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFD6D5FAFF837DF1FF837DF1FF837DF1FF837DF1FF837DF1FF837DF1FF736D
        E2FF6B65DAFF6B65DAFF6B65DAFF6B65DAFF6B65DAFF6B65DAFF6B65DAFF6B65
        DAFF6B65DAFF6B65DAFF6B65DAFF6B65DAFF6B65DAFF6B65DAFF6B65DAFF6B65
        DAFF6B65DAFF736DE2FF6B65DAFF5451C7FF5451C7FF5451C7FF6B65DAFF837D
        F1FF837DF1FF837DF1FF837DF1FF837DF1FF837DF1FF9893F0FFFDFCF7FFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFCF7FFF4FBFBFFF0F0EEFFF0F0EEFFF1EDF5FFF3F3F3FFF3F3
        F3FFDAD9D8FFC0BFC1FFBFC0BFFFBCBBBCFFC7C7C7FFC0BFC1FFC0BFC1FFBFC0
        BFFFBFC0BFFFBFC0BFFFBCBBBCFFC7C7C7FFBCBBBCFFB8B7B6FFC7C7C7FFC0BF
        C1FFA8A6A6FF979696FFBCBBBCFFC7C7C7FFC7C7C7FFA8A6A6FF958D8DFF958D
        8DFF979696FFD9D8D7FFF0F0EEFFF0F0EEFFF0F0EEFFF1EDF5FFF0F0EEFFF0F0
        EEFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFD4D4D3FF676667FF383636FF403F
        40FF383636FF59595AFF59595AFF403F40FF383636FF312F31FF8E8D96FF807F
        82FF474647FF383636FF383636FF383636FF59595AFF9392A1FFA69B99FF403F
        40FF95A4A3FF59595AFF676667FF504E51FF312F31FF958D8DFFC7C7C7FF807F
        82FF312F31FF312F31FF6F706FFF979696FF676667FF474647FF383636FF403F
        40FF504E51FF8E8D96FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFC7C7
        C7FF201E22FF1E2123FF201E22FF93A19FFFC7C7C7FFA8A6A6FF242525FF1E21
        23FF958D8DFFC7C7C7FF807F82FF201E22FF201E22FF242525FFB2B2B2FFC7C7
        C7FFA69B99FF201E22FF242525FFA69B99FFC7C7C7FFB2B2B2FF242525FF2425
        25FFADACB0FFC7C7C7FF676667FF757677FF979696FF979696FF979696FF403F
        40FF201E22FF201E22FF6F706FFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFC7C7C7FF201E22FF1E1E21FF1E1E21FFA69B99FFC7C7C7FFC7C7
        C7FF59595AFF211E1EFF868586FFC7C7C7FF868586FF201E22FF201E22FF2425
        25FFB2B2B2FFC7C7C7FFA69B99FF1E2123FF504E51FFC7C7C7FFC7C7C7FFC7C7
        C7FF312F31FF201E22FF676667FFC7C7C7FF958D8DFF868586FFB3B0AEFF9796
        96FF979696FF312F31FF201E22FF201E22FF868586FFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFC7C7C7FF201E22FF201E22FF1E2123FFA69B
        99FFC7C7C7FFC7C7C7FF8E8D96FF201E22FF868586FFC7C7C7FF807F82FF201E
        22FF201E22FF242525FFB2B2B2FFC7C7C7FF979696FF201E22FF807F82FFC7C7
        C7FFC7C7C7FFC0BFC1FF312F31FF1E1E21FF403F40FFC0BFC1FFA8A6A6FF7576
        77FFC7C7C7FFB2B2B2FF958D8DFF242525FF1E1E21FF1E1E21FFA8A6A6FFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFB8B7B6FF1E2123FF201E
        22FF1E1E21FF979696FFC7C7C7FFC7C7C7FFB3B0AEFF242525FF958D8DFFC7C7
        C7FF807F82FF22211EFF201E22FF242525FFB2B2B2FFC7C7C7FF979696FF2425
        25FFADACB0FFC7C7C7FFC7C7C7FFC0BFC1FF242525FF201E22FF312F31FFBCBB
        BCFFB2B2B2FF676667FFC7C7C7FFC7C7C7FFA69B99FF242525FF242525FF201E
        22FFA8A6A6FFFDFDFDFFFDFCF7FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFE5E5E5FFA69B
        99FF201E22FF201E22FF1E2123FFA69B99FFC7C7C7FFC7C7C7FFC0BFC1FF3836
        36FF958D8DFFC7C7C7FF807F82FF201E22FF201E22FF242525FFB0AFB0FFC7C7
        C7FF979696FF312F31FFC0BFC1FFC7C7C7FFC7C7C7FFA8A6A6FF201E22FF1E1E
        21FF312F31FFBCBBBCFFBCBBBCFF474647FF676667FF676667FF504E51FF201E
        22FF201E22FF22211EFF807F82FFD4D4D3FFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFEAE8
        E7FFC7C7C7FF95A4A3FF1E1E21FF1E1E21FF1E1E21FF979696FFC7C7C7FFC7C7
        C7FFC7C7C7FF504E51FF868586FFC7C7C7FF807F82FF22211EFF201E22FF2425
        25FFB2B2B2FFC7C7C7FFA69B99FF474647FFC7C7C7FFC7C7C7FFC7C7C7FF7576
        77FF242525FF201E22FF383636FFC0BFC1FFC7C7C7FF59595AFFC7C7C7FFC7C7
        C7FF979696FF22211EFF201E22FF242525FF868586FF979696FFD9D8D7FFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFEAE8E7FFC7C7C7FFC7C7C7FFA69B99FF201E22FF201E22FF1E2123FFA69B
        99FFC7C7C7FFC7C7C7FFC7C7C7FF6F706FFF958D8DFFC7C7C7FF807F82FF201E
        22FF201E22FF242525FFB0AFB0FFC7C7C7FF979696FF676667FFC7C7C7FFC7C7
        C7FFC0BFC1FF403F40FF201E22FF1E1E21FF474647FFC7C7C7FFC7C7C7FF6766
        67FFC0BFC1FFC7C7C7FF868586FF201E22FF201E22FF312F31FF958D8DFF9796
        96FF979696FFD9D8D7FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFEAE8E7FFC7C7C7FFC7C7C7FFC7C7C7FFA8A6A6FF1E1E21FF1E21
        23FF1E1E21FF979696FFC7C7C7FFC7C7C7FFC7C7C7FF958D8DFF868586FFC7C7
        C7FF90817FFF201E22FF201E22FF242525FFB2B2B2FFC7C7C7FFA69B99FF7576
        77FFC7C7C7FFC7C7C7FFA69B99FF242525FF201E22FF1E1E21FF6F706FFFC7C7
        C7FFC7C7C7FF757677FFB3B0AEFFC7C7C7FF757677FF201E22FF201E22FF3836
        36FFB2B2B2FF979696FF979696FF979696FFD9D8D7FFFDFCF7FFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFEAE8E7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFA69B
        99FF201E22FF201E22FF1E2123FFA69B99FFC7C7C7FFC7C7C7FFC7C7C7FFA8A6
        A6FF868586FFC7C7C7FF807F82FF201E22FF201E22FF242525FFB3B0AEFFC7C7
        C7FF979696FF958D8DFFC7C7C7FFC7C7C7FF59595AFF201E22FF201E22FF201E
        22FFA69B99FFC7C7C7FFC7C7C7FF868586FF979696FFC7C7C7FF59595AFF201E
        22FF201E22FF504E51FFC7C7C7FFB8B7B6FF979696FF979696FF979696FFD4D4
        D3FFFDFDFDFFFDFDFDFFFDFDFDFFE5E5E5FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFA69B99FF1E2123FF1E2123FF201E22FF979696FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7B7B5FFC7C7C7FF868586FF201E22FF201E22FF2425
        25FFB2B2B2FFC7C7C7FFA8A6A6FFA8A6A6FFC7C7C7FFA8A6A6FF242525FF2221
        1EFF201E22FF312F31FFC7C7C7FFC7C7C7FFC7C7C7FFA69B99FF868586FFC7C7
        C7FF504E51FF201E22FF201E22FF59595AFFC7C7C7FFC7C7C7FFB3B0AEFF9796
        96FF979696FF979696FFD4D4D3FFFDFDFDFFEAE8E7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFA69B99FF201E22FF201E22FF1E2123FFA69B
        99FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF807F82FF201E
        22FF201E22FF242525FFB8B7B6FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF5959
        5AFF201E22FF1E1E21FF201E22FF676667FFD4D4D3FFC7C7C7FFC7C7C7FFB8B7
        B6FF6F706FFFC7C7C7FF403F40FF201E22FF211E1EFF6F706FFFC7C7C7FFC7C7
        C7FFC7C7C7FFB3B0AEFF979696FF979696FF979696FFDAD9D8FFEAEAEAFFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF95A4A3FF201E22FF1E21
        23FF201E22FF979696FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FF90817FFF1E2123FF201E22FF242525FFB2B2B2FFC7C7C7FFC7C7C7FFC7C7
        C7FFB3B0AEFF242525FF242525FF201E22FF242525FFADACB0FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FF676667FFC7C7C7FF312F31FF201E22FF201E22FF8685
        86FFC7C7C7FFC7C7C7FFC7C7C7FFB3B0AEFF979696FF979696FF95A4A3FFE5E5
        E5FFFDFDFDFFEAEAEAFFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFA69B
        99FF201E22FF201E22FF1E2123FFA69B99FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FF807F82FF201E22FF201E22FF242525FFB3B0AEFFC7C7
        C7FFC7C7C7FFC7C7C7FF676667FF201E22FF1E2123FF201E22FF504E51FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF59595AFFC0BFC1FF242525FF201E
        22FF201E22FF979696FFC7C7C7FFC7C7C7FFB1AFAEFF979696FF979696FFA69B
        99FFEAE8E7FFFDFDFDFFFDFDFDFFFDFDFDFFEAEAEAFFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFA69B99FF1E2123FF1E2123FF201E22FF979696FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF90817FFF201E22FF201E22FF2425
        25FFB2B2B2FFC7C7C7FFC7C7C7FFBFC0BFFF312F31FF201E22FF201E22FF2425
        25FFA69B99FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF59595AFFADAC
        B0FF242525FF201E22FF242525FFADACB0FFC7C7C7FFB1AFAEFF979696FF9796
        96FFA69B99FFE5E5E5FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFEAEA
        EAFFC7C7C7FFC7C7C7FFC7C7C7FFA69B99FF201E22FF201E22FF1E2123FFA69B
        99FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF807F82FF201E
        22FF201E22FF242525FFB3B0AEFFC7C7C7FFC7C7C7FF868586FF201E22FF201E
        22FF211E1EFF504E51FFC7C7C7FF979696FFA8A6A6FFC7C7C7FFC7C7C7FFC7C7
        C7FF6F706FFF868586FF201E22FF201E22FF242525FFB2B2B2FFB1AFAEFF9796
        96FF979696FFA69B99FFEAEAEAFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFEAEAEAFFC7C7C7FFC7C7C7FFA69B99FF201E22FF201E
        22FF1E2123FF979696FFC7C7C7FFC7C7C7FFD4D4D3FFC7C7C7FFC7C7C7FFC7C7
        C7FF90817FFF201E22FF201E22FF242525FFB2B2B2FFC7C7C7FFC7C7C7FF5959
        5AFF201E22FF201E22FF22211EFF979696FFC7C7C7FF757677FFA8A6A6FFC7C7
        C7FFC7C7C7FFC7C7C7FF868586FF676667FF1E1E21FF201E22FF312F31FFA8A6
        A6FF979696FF979696FFA69B99FFEAEAEAFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFEAEAEAFFC7C7C7FF95A4
        A3FF22211EFF201E22FF1E2123FFA69B99FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FF807F82FF201E22FF201E22FF242525FFB3B0AEFFC7C7
        C7FFC7C7C7FF312F31FF201E22FF201E22FF403F40FFC0BFC1FFC7C7C7FF6766
        67FFA8A6A6FFC7C7C7FFC7C7C7FFC7C7C7FFA69B99FF403F40FF201E22FF201E
        22FF383636FF979696FF979696FFA69B99FFEAE8E7FFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFEAEAEAFFA69B99FF1E2123FF201E22FF201E22FF979696FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFD4D4D3FF807F82FF22211EFF201E22FF2425
        25FFB2B2B2FFC7C7C7FFBCBBBCFF211E1EFF201E22FF201E22FF868586FFC7C7
        C7FFC7C7C7FF474647FFA8A6A6FFC7C7C7FFC7C7C7FFC7C7C7FFB8B7B6FF201E
        22FF201E22FF201E22FF403F40FF979696FF95A4A3FFEAEAEAFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFBCBBBCFF1E2123FF201E22FF1E2123FFA69B
        99FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF807F82FF201E
        22FF201E22FF242525FFB3B0AEFFC7C7C7FFADACB0FF201E22FF242525FF312F
        31FFB8B7B6FFC7C7C7FFC7C7C7FF312F31FFA8A6A6FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FF242525FF201E22FF1E1E21FF504E51FFA69B99FFEAE8E7FFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFC7C7C7FF201E22FF201E
        22FF1E2123FF979696FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FF90817FFF201E22FF201E22FF242525FFB2B2B2FFC7C7C7FFB1AFAEFF201E
        22FF201E22FF504E51FFC7C7C7FFC7C7C7FFB8B7B6FF22211EFFA8A6A6FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FF383636FF201E22FF1E1E21FF676667FFE5E5
        E5FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFC7C7
        C7FF1E2123FF1E1E21FF1E2123FFA69B99FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FF807F82FF201E22FF201E22FF242525FFB3B0AEFFC7C7
        C7FFBFC0BFFF242525FF201E22FF757677FFC7C7C7FFC7C7C7FF868586FF201E
        22FFA8A6A6FFC7C7C7FFC7C7C7FFC7C7C7FFB1AFAEFF474647FF1E1E21FF201E
        22FFA8A6A6FFFDFDFDFFFDFDFDFFF1EDF5FFDAD9D8FFF3F3F3FFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFC7C7C7FF201E22FF201E22FF201E22FFA69B99FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF868586FF211E1EFF201E22FF2425
        25FFB2B2B2FFC7C7C7FFC7C7C7FF403F40FF22211EFF868586FFC7C7C7FFC7C7
        C7FF59595AFF201E22FFA8A6A6FFC7C7C7FFC7C7C7FFB1AFAEFF979696FF504E
        51FF201E22FF201E22FFC0BFC1FFFDFDFDFFD8D7D8FF6F706FFF8E8D96FF7576
        77FFF3F3F3FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFC7C7C7FF201E22FF201E22FF1E2123FFB3B0
        AEFFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FF807F82FF201E
        22FF201E22FF242525FFB3B0AEFFC7C7C7FFC7C7C7FF807F82FF22211EFF7576
        77FFC7C7C7FFB8B7B6FF312F31FF1E1E21FFA8A6A6FFC7C7C7FFB1AFAEFF9796
        96FF979696FF676667FF1E1E21FF201E22FFD8D7D8FFFDFDFDFF979696FF8685
        86FF676667FF868586FFE5E5E5FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFE5E5E5FF868586FF201E22FF201E
        22FF201E22FF868586FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFA8A6
        A6FF59595AFF201E22FF201E22FF242525FF807F82FFB2B2B2FFC7C7C7FFC7C7
        C7FF504E51FF383636FF958D8DFF59595AFF868586FF312F31FFA8A6A6FFADAC
        B0FF979696FF979696FFA69B99FFB0AFB0FF1E2123FF312F31FFE5E5E5FFFDFD
        FDFFC7C7C7FF6F706FFF757677FF757677FFF3F3F3FFFDFCF7FFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFE5E5E5FFB2B2
        B2FFB2B2B2FFB2B2B2FFB2B2B2FFB2B2B2FFD8D7D8FFEAEAEAFFC7C7C7FFC7C7
        C7FFC7C7C7FFA8A6A6FF958D8DFF958D8DFF958D8DFF958D8DFF958D8DFFC7B7
        B5FFC7C7C7FFC7C7C7FFC7C7C7FF90817FFF676667FFA69B99FFC7C7C7FFA69B
        99FF979696FF979696FF979696FFA69B99FFEAEAEAFFEAEAEAFFB2B2B2FFBCBB
        BCFFFDFCF7FFFDFDFDFFF4FBFBFFD4D4D3FFBCBBBCFFE5E5E5FFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFEAEAEAFFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFB1AFAEFF979696FF958D8DFFA8A6A6FFEAE8E7FFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFEAE8E7FFD4D4D3FFC7C7C7FFC7C7C7FFC7C7
        C7FFD4D4D3FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFADACB0FF979696FF979696FFA8A6A6FFEAEAEAFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFEAEAEAFFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFB1AFAEFF979696FF979696FFA69B99FFEAEA
        EAFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFF0F0EEFFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFB1AFAEFF979696FF979696FFA69B
        99FFF0F0EEFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFEAEAEAFFC7C7C7FFC7C7C7FFC7C7
        C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFB1AFAEFF979696FF9796
        96FFA69B99FFEAEAEAFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFEAEA
        EAFFD4D4D3FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFB1AFAEFF9796
        96FF979696FFA8A6A6FFEAEAEAFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFEAEAEAFFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFB1AF
        AEFF979696FF979696FFA69B99FFEAEAEAFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFEAEAEAFFC7C7C7FFC7C7
        C7FFEAE8E7FFC7C7C7FF979696FFA8A6A6FFEAEAEAFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFEAEAEAFFE5E5E5FFFDFDFDFFFDFDFDFFD4D4D3FFEAEAEAFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
        FDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFF}
    end
    object Label2: TLabel
      Left = 24
      Top = 27
      Width = 302
      Height = 13
      Caption = 'Konfigurace parametr'#367' pro slovn'#237'ky TBX (Term Base eXchange)'
    end
    object Label3: TLabel
      Left = 16
      Top = 10
      Width = 48
      Height = 13
      Caption = 'Nastaven'#237
    end
  end
  object lvFiles: TListView
    Left = 16
    Top = 84
    Width = 402
    Height = 214
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'N'#225'zev souboru'
        Width = 264
      end
      item
        Caption = 'Jazyky'
        Width = 112
      end>
    HideSelection = False
    ReadOnly = True
    PopupMenu = pumFiles
    TabOrder = 0
    ViewStyle = vsReport
  end
  object sbAdd: TButton
    Left = 432
    Top = 84
    Width = 75
    Height = 25
    Action = acAdd
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object sbRemove: TButton
    Left = 432
    Top = 112
    Width = 75
    Height = 25
    Action = acRemove
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object sbLangs: TButton
    Left = 432
    Top = 140
    Width = 75
    Height = 25
    Action = acLangs
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object sbMoveUp: TButton
    Left = 432
    Top = 184
    Width = 75
    Height = 25
    Action = acMoveUp
    Anchors = [akTop, akRight]
    TabOrder = 4
  end
  object sbMoveDn: TButton
    Left = 432
    Top = 212
    Width = 75
    Height = 25
    Action = acMoveDn
    Anchors = [akTop, akRight]
    TabOrder = 5
  end
  object cbAllowCrossLangs: TCheckBox
    Left = 16
    Top = 352
    Width = 297
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Povolit k'#345#237#382'ov'#233' p'#345'eklady'
    TabOrder = 6
  end
  object pumFiles: TPopupMenu
    object miAdd: TMenuItem
      Action = acAdd
    end
    object miRemove: TMenuItem
      Action = acRemove
    end
    object MenuItem3: TMenuItem
      Caption = '-'
    end
    object miLangs: TMenuItem
      Action = acLangs
    end
    object MenuItem1: TMenuItem
      Caption = '-'
    end
    object miMoveUp: TMenuItem
      Action = acMoveUp
    end
    object miMoveDn: TMenuItem
      Action = acMoveDn
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object miCheckAll: TMenuItem
      Caption = 'Vybrat v'#353'e'
    end
    object miInvertChecks: TMenuItem
      Caption = 'Obr'#225'tit v'#253'b'#283'r'
    end
  end
  object ActionList1: TActionList
    object acAdd: TAction
      Caption = 'P'#345'idat'
      Hint = 'P'#345'idat nov'#253' TMX soubor do seznamu'
      ShortCut = 45
    end
    object acRemove: TAction
      Caption = 'Odebrat'
      Hint = 'Odebrat vybran'#253' TMX soubor ze seznamu'
      ShortCut = 16430
    end
    object acLangs: TAction
      Caption = 'Jazyky'
      Hint = 'Definujte k'#243'dy jazyk'#367' pro vybran'#253' soubor'
      ShortCut = 16397
    end
    object acMoveUp: TAction
      Caption = 'Nahoru'
      ShortCut = 16422
    end
    object acMoveDn: TAction
      Caption = 'Dol'#367
      ShortCut = 16424
    end
  end
end
