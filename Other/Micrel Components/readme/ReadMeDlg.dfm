object ReadmeForm: TReadmeForm
  Left = 237
  Top = 118
  Width = 505
  Height = 374
  ActiveControl = AcceptIt
  BorderIcons = [biSystemMenu]
  Caption = 'ReadmeForm'
  Color = clBtnFace
  Constraints.MinHeight = 278
  Constraints.MinWidth = 442
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 402
    Top = 0
    Width = 95
    Height = 340
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    DesignSize = (
      95
      340)
    object Bevel1: TBevel
      Left = 10
      Top = 88
      Width = 78
      Height = 9
      Shape = bsBottomLine
    end
    object AcceptIt: TButton
      Left = 10
      Top = 48
      Width = 78
      Height = 27
      Anchors = [akTop, akRight]
      Caption = '&Akceptuji'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object EscapeIt: TButton
      Left = 10
      Top = 120
      Width = 78
      Height = 27
      Anchors = [akTop, akRight]
      Caption = '&Opustit'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 402
    Height = 340
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object BackgroundPanel: TPanel
      Left = 0
      Top = 41
      Width = 402
      Height = 299
      Align = alClient
      BevelOuter = bvLowered
      BorderWidth = 3
      Color = 14680063
      ParentBackground = False
      TabOrder = 1
      object ReadMeFile: TMemo
        Left = 4
        Top = 45
        Width = 394
        Height = 250
        Align = alClient
        BorderStyle = bsNone
        Color = 15728639
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object Panel4: TPanel
        Left = 4
        Top = 4
        Width = 394
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        BevelWidth = 2
        ParentColor = True
        TabOrder = 0
        object ShineBlock: TImage
          Left = 0
          Top = 0
          Width = 36
          Height = 41
          Align = alLeft
          AutoSize = True
          Center = True
          Picture.Data = {
            07544269746D617046030000424D460300000000000076000000280000002400
            0000240000000100040000000000D00200000000000000000000100000001000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD0000DDDDDDDDDDDDDDDDDDDD
            DDDDDDDDDDDDDDDD0000DDDDDDDDDDDDDDDDD8DDDDDDDDDDDDDDDDDD0000DDDD
            DDDDDDDDDDDD888DDDDDDDDDDDDDDDDD0000DDDDDDDDDDDDDDD88888DDDDDDDD
            DDDDDDDD0000DDDDDDDDDDDDDD8880888DDDDDDDDDDDDDDD0000DDDDDDDDDDDD
            D8880E0888DDDDDDDDDDDDDD0000DDDDDDDDDDDD8880EFE0888DDDDDDDDDDDDD
            0000DDDDDDDDDDD8880EFEFE0888DDDDDDDDDDDD0000DDDDDDDDDD8880EFEFEF
            E0888DDDDDDDDDDD0000DDDDDDDDD8880EFEFEFEFE0888DDDDDDDDDD0000DDDD
            DDDD8800000000000000088DDDDDDDDD0000DDDDDDD880BBBBBBBBBBBBBBB088
            DDDDDDDD0000DDDDDD8880B8000000000008B0888DDDDDDD0000DDDDDDD800B0
            FEFEFEFEFEF0B008DDDDDDDD0000DDDDDDD04FB0E777777777E0B0E0DDDDDDDD
            0000DDDDDD0E44F0FEFEFEFEFEF0B0FE0DDDDDDD0000DDDDDDD8444F7FEFEFEF
            EFE0B0E0DDDDDDDD0000DDDDD4444C44F7FE777777F0B00DDDDDDDDD0000DDDD
            DCCCCEC44F7FEFEFEFE0B0DDDDDDDDDD0000DDDDDEEEEEEC44F7FEFEFEF0B0DD
            DDDDDDDD0000DDDDD4444E444F7FE77777E0B0DDDDDDDDDD0000DDDDD4444C44
            87FEFEFEFE00B0DDDDDDDDDD0000DDDDDDDD444F78EFEFEFE0D0B0DDDDDDDDDD
            0000DDDDDDDD44F0DD77777777D0B0DDDDDDDDDD0000DDDDDDDD4FB0DDD8EFE0
            DDD0B0DDDDDDDDDD0000DDDDDDDDD0B0DDDD8E0DDDD0B0DDDDDDDDDD0000DDDD
            DDDDD0B0DD0000000DD0B0DDDDDDDDDD0000DDDDDDDDD0B80007F7770008B0DD
            DDDDDDDD0000DDDDDDDDD0BBBBB07F70BBBBB0DDDDDDDDDD0000DDDDDDDDDD00
            0000070000000DDDDDDDDDDD0000DDDDDDDDDDDDDDD0F770DDDDDDDDDDDDDDDD
            0000DDDDDDDDDDDDDDD00000DDDDDDDDDDDDDDDD0000DDDDDDDDDDDDDDDDDDDD
            DDDDDDDDDDDDDDDD0000DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD0000DDDD
            DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD0000}
          Transparent = True
        end
        object LegalCopyright: TLabel
          Left = 36
          Top = 0
          Width = 358
          Height = 41
          Align = alClient
          AutoSize = False
          Caption = 'LegalCopyright'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial Narrow'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          WordWrap = True
        end
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 402
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object ReadmeHeader: TLabel
        Left = 5
        Top = 5
        Width = 392
        Height = 31
        Align = alClient
        AutoSize = False
        Caption = 'ReadmeHeader'
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clBlack
        Font.Height = -24
        Font.Name = 'Times New Roman'
        Font.Style = [fsBold, fsItalic]
        ParentFont = False
      end
    end
  end
end
