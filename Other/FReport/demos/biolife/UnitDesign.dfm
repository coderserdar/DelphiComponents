object FormDesign: TFormDesign
  Left = 229
  Top = 226
  Width = 888
  Height = 698
  HorzScrollBar.Range = 712
  VertScrollBar.Range = 919
  ActiveControl = RLLabel2
  AutoScroll = False
  Caption = 'FormDesign'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object RLReport1: TRLReport
    Left = 4
    Top = 3
    Width = 816
    Height = 1056
    Borders.Sides = sdAll
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 12
    Font.Name = 'Arial'
    Font.Pitch = fpVariable
    Font.Style = []
    PageSetup.PaperSize = fpLetter
    Transparent = False
    AfterPrint = RLReport1AfterPrint
    BeforePrint = RLReport1BeforePrint
    OnDataCount = RLReport1DataCount
    OnNeedData = RLReport1NeedData
    object RLDetailGrid1: TRLDetailGrid
      Left = 39
      Top = 125
      Width = 738
      Height = 108
      ColCount = 2
      Transparent = False
      object RLText2: TRLLabel
        Left = 107
        Top = 7
        Width = 34
        Height = 16
      end
      object RLLabel3: TRLLabel
        Left = 107
        Top = 10
        Width = 34
        Height = 17
        Caption = 'SPECIES_NO'
        BeforePrint = RLLabel3BeforePrint
      end
      object RLText1: TRLLabel
        Left = 107
        Top = 31
        Width = 180
        Height = 17
        Caption = 'Common Name: COMMON_NAME'
        BeforePrint = RLText1BeforePrint
      end
      object RLText3: TRLLabel
        Left = 107
        Top = 52
        Width = 105
        Height = 16
        Caption = 'Category: CATEGORY'
        BeforePrint = RLText3BeforePrint
      end
      object RLText4: TRLLabel
        Left = 107
        Top = 73
        Width = 79
        Height = 16
        Caption = 'Length(cm): LENGTH_CM'
        BeforePrint = RLText4BeforePrint
      end
      object RLImage1: TRLImage
        Left = 7
        Top = 7
        Width = 91
        Height = 91
        Borders.Sides = sdAll
        Center = True
        Stretch = True
        BeforePrint = RLImage1BeforePrint
      end
    end
    object RLBand1: TRLBand
      Left = 39
      Top = 71
      Width = 738
      Height = 54
      BandType = btTitle
      Borders.Sides = sdCustom
      Borders.DrawLeft = False
      Borders.DrawTop = False
      Borders.DrawRight = False
      Borders.DrawBottom = True
      Transparent = False
      object RLLabel1: TRLLabel
        Left = 0
        Top = 0
        Width = 738
        Height = 53
        Align = faClient
        Borders.Sides = sdAll
        Borders.Width = 6
        Borders.Color = clGreen
        Caption = 'This is the report Title (BIOLIFE Table)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 44
        Font.Name = 'helvetica'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object RLBand2: TRLBand
      Left = 39
      Top = 233
      Width = 738
      Height = 16
      BandType = btFooter
      Transparent = False
      object RLSystemInfo1: TRLSystemInfo
        Left = 608
        Top = 0
        Width = 130
        Height = 16
        Align = faRight
        Alignment = taRightJustify
        Info = itPageNumber
        Text = 'Page No.: '
      end
      object RLLabel2: TRLLabel
        Left = 0
        Top = 0
        Width = 316
        Height = 16
        Align = faLeft
        Caption = 'Fortes Report v2.02 Copyright (c) 1999-2001  Fortes Informática'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 9
        Font.Name = 'arial'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
      end
    end
    object RLBand3: TRLBand
      Left = 39
      Top = 39
      Width = 738
      Height = 32
      BandType = btHeader
      Transparent = False
      object RLLabel4: TRLLabel
        Left = 4
        Top = 4
        Width = 187
        Height = 24
        Caption = 'This is the Header'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -21
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
      end
    end
  end
end
