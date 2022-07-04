object NCOciErrorFrm: TNCOciErrorFrm
  Left = 229
  Top = 214
  Width = 446
  Height = 246
  BorderIcons = []
  Caption = 'Oracle8 Error'
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BasicPanel: TPanel
    Left = 0
    Top = 0
    Width = 438
    Height = 56
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ErrorText: TLabel
      Left = 49
      Top = 8
      Width = 381
      Height = 48
      Align = alClient
      Caption = 'ErrorText'
      WordWrap = True
    end
    object IconPanel: TPanel
      Left = 0
      Top = 8
      Width = 49
      Height = 48
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object IconImage: TImage
        Left = 6
        Top = 2
        Width = 34
        Height = 34
        Picture.Data = {
          055449636F6E0000010002002020100000000000E80200002600000020200200
          00000000300100000E0300002800000020000000400000000100040000000000
          0002000000000000000000000000000000000000000000000000800000800000
          00808000800000008000800080800000C0C0C000808080000000FF0000FF0000
          00FFFF00FF000000FF00FF00FFFF0000FFFFFF00000008888888888888888888
          8888880000008888888888888888888888888880003000000000000000000000
          0008888803BBBBBBBBBBBBBBBBBBBBBBBB7088883BBBBBBBBBBBBBBBBBBBBBBB
          BBB708883BBBBBBBBBBBBBBBBBBBBBBBBBBB08883BBBBBBBBBBBB7007BBBBBBB
          BBBB08803BBBBBBBBBBBB0000BBBBBBBBBB7088003BBBBBBBBBBB0000BBBBBBB
          BBB0880003BBBBBBBBBBB7007BBBBBBBBB708800003BBBBBBBBBBBBBBBBBBBBB
          BB088000003BBBBBBBBBBB0BBBBBBBBBB70880000003BBBBBBBBB707BBBBBBBB
          B08800000003BBBBBBBBB303BBBBBBBB7088000000003BBBBBBBB000BBBBBBBB
          0880000000003BBBBBBB70007BBBBBB708800000000003BBBBBB30003BBBBBB0
          88000000000003BBBBBB00000BBBBB70880000000000003BBBBB00000BBBBB08
          800000000000003BBBBB00000BBBB7088000000000000003BBBB00000BBBB088
          0000000000000003BBBB00000BBB708800000000000000003BBB70007BBB0880
          00000000000000003BBBBBBBBBB70880000000000000000003BBBBBBBBB08800
          000000000000000003BBBBBBBB7088000000000000000000003BBBBBBB088000
          0000000000000000003BBBBBB708800000000000000000000003BBBBB0880000
          00000000000000000003BBBB70800000000000000000000000003BB700000000
          0000000000000000000003330000000000000000F8000003F0000001C0000000
          80000000000000000000000000000001000000018000000380000003C0000007
          C0000007E000000FE000000FF000001FF000001FF800003FF800003FFC00007F
          FC00007FFE0000FFFE0000FFFF0001FFFF0001FFFF8003FFFF8003FFFFC007FF
          FFC007FFFFE00FFFFFE01FFFFFF07FFFFFF8FFFF280000002000000040000000
          0100010000000000800000000000000000000000000000000000000000000000
          FFFFFF000000000000000000000000003FFFFFC07FFFFFE07FFFFFF07FFCFFF0
          7FF87FE03FF87FE03FFCFFC01FFFFFC01FFDFF800FFDFF800FFDFF0007F8FF00
          07F8FE0003F8FE0003F07C0001F07C0001F0780000F0780000F070000078F000
          007FE000003FE000003FC000001FC000001F8000000F8000000F000000060000
          00000000FFFFFFFFFFFFFFFFC000001F8000000F000000070000000700000007
          000000078000000F8000000FC000001FC000001FE000003FE000003FF000007F
          F000007FF80000FFF80000FFFC0001FFFC0001FFFE0003FFFE0003FFFF0007FF
          FF0007FFFF800FFFFF800FFFFFC01FFFFFC01FFFFFE03FFFFFE03FFFFFF07FFF
          FFF8FFFF}
      end
    end
    object TopPanel: TPanel
      Left = 0
      Top = 0
      Width = 438
      Height = 8
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
    end
    object RightPanel: TPanel
      Left = 430
      Top = 8
      Width = 8
      Height = 48
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object DetailsPanel: TPanel
    Left = 0
    Top = 95
    Width = 438
    Height = 124
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvLowered
    TabOrder = 1
    object NativeLabel: TLabel
      Left = 7
      Top = 10
      Width = 59
      Height = 13
      Alignment = taRightJustify
      Caption = 'Server Error:'
    end
    object Label1: TLabel
      Left = 126
      Top = 10
      Width = 68
      Height = 13
      Alignment = taRightJustify
      Caption = 'Server Object:'
    end
    object DbMessageText: TMemo
      Left = 8
      Top = 35
      Width = 423
      Height = 49
      TabStop = False
      Color = clBtnFace
      Lines.Strings = (
        'DbMessageText')
      ReadOnly = True
      TabOrder = 1
    end
    object NativeResult: TEdit
      Left = 71
      Top = 8
      Width = 50
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
      Text = '00000'
    end
    object NextBtn: TBitBtn
      Left = 357
      Top = 91
      Width = 75
      Height = 25
      Caption = 'Next'
      TabOrder = 3
      OnClick = NextClick
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777777077777700077777700777770007777770607777000770000066077
        7000770666666607700077066666666070007706666666077000770000066077
        7000777777060777700077777700777770007777770777777000777777777777
        7000}
      Layout = blGlyphRight
    end
    object BackBtn: TBitBtn
      Left = 277
      Top = 91
      Width = 75
      Height = 25
      Caption = 'Back'
      TabOrder = 2
      OnClick = BackClick
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777777077777700077777007777770007777060777777000777066000007
        7000770666666607700070666666660770007706666666077000777066000007
        7000777706077777700077777007777770007777770777777000777777777777
        7000}
    end
    object ServerObject: TEdit
      Left = 199
      Top = 8
      Width = 232
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
    end
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 56
    Width = 438
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object OKBtn: TBitBtn
      Left = 357
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object DetailsBtn: TBitBtn
      Left = 261
      Top = 8
      Width = 90
      Height = 25
      Caption = 'Details'
      TabOrder = 1
      OnClick = DetailsBtnClick
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDD000000DDDDDDDDDDDDDDDDDD000000DDDDDDDDDDDDDDDDDD000000DD00
        0DDDD000DDDDDD000000D07E70DD07E70DDDDD000000D0E7E0DD0E7E0DDDDD00
        0000D0FE70DD0FE70DDDDD000000D0EFE0DD0EFE0DDDDD000000D8000D00D000
        8DDDDD000000DD0DDDDDDDDD0DDD0D000000DDD0DDD0DDDDD0DD0D000000DDDD
        0DD0DDDDDD0D0D000000DDDDD0D0DDDDDDD0DD000000DDDDDD0DDDDDDDDDDD00
        0000DDDDDDDDDDDDDDDDDD000000DDDDDDDDDDDDDDDDDD000000DDDDDDDDDDDD
        DDDDDD000000DDDDDDDDDDDDDDDDDD000000}
    end
  end
end
