object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Demonstration of using the tag cloud stylers'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object splLeft: TSplitter
    Left = 246
    Top = 0
    Height = 600
    AutoSnap = False
    ExplicitLeft = 356
    ExplicitTop = 220
    ExplicitHeight = 100
  end
  object pLeft: TPanel
    Left = 0
    Top = 0
    Width = 246
    Height = 600
    Align = alLeft
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    object tiStyles: TTagIndex
      Left = 5
      Top = 5
      Width = 236
      Height = 539
      Align = alClient
      Alignment = taLeftJustify
      Color = 1976878
      ColSpacing = 1
      CustomLabels.Strings = (
        'All styles|'
        'A'
        'B'
        'C'
        'D'
        'E'
        'F'
        'G'
        'H'
        'I'
        'J'
        'K'
        'L'
        'M'
        'N'
        'O'
        'P'
        'Q'
        'R'
        'S'
        'T'
        'U'
        'V'
        'W'
        'X'
        'Y'
        'Z')
      FixedColFullFrame = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6390668
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      HoverColor = clNone
      HoverCursor = crHandPoint
      HoverStyle = []
      HoverEnlarge = False
      ItemFrame.BackColor = clNone
      ItemFrame.FrameColor = clNone
      ItemFrame.FrameMargin = 1
      ItemFrame.FrameSize = 0
      ItemFrame.FrameStyle = psSolid
      ItemFrame.RoundedSize = 0
      HoverFrame.BackColor = clNone
      HoverFrame.FrameColor = clNone
      HoverFrame.FrameMargin = 1
      HoverFrame.FrameSize = 1
      HoverFrame.FrameStyle = psDot
      HoverFrame.RoundedSize = 0
      SelectedFrame.BackColor = clNone
      SelectedFrame.FrameColor = clNone
      SelectedFrame.FrameMargin = 1
      SelectedFrame.FrameSize = 1
      SelectedFrame.FrameStyle = psSolid
      SelectedFrame.RoundedSize = 0
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = 6390668
      LabelFont.Height = -12
      LabelFont.Name = 'Arial'
      LabelFont.Style = []
      LabelFrame.BackColor = clNone
      LabelFrame.FrameColor = clDefault
      LabelFrame.FrameMargin = 0
      LabelFrame.FrameSize = 0
      LabelFrame.FrameStyle = psSolid
      LabelFrame.RoundedSize = 0
      LabelsPosition = tlpTop
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      SelectedColor = clNone
      SelectedLabel = 'All styles'
      SelLabelColor = clDefault
      SelLabelFrame.BackColor = clNone
      SelLabelFrame.FrameColor = clDefault
      SelLabelFrame.FrameMargin = 1
      SelLabelFrame.FrameSize = 1
      SelLabelFrame.FrameStyle = psSolid
      SelLabelFrame.RoundedSize = 0
      SelLabelStyle = [fsBold]
      PageIndex = 0
      PageNumberStyle = pnsTabs
      ParentColor = False
      ParentFont = False
      RowSpacing = 1
      Transparent = False
      OnDblClick = tiStylesDblClick
      OnTagClick = tiStylesTagClick
      ExplicitWidth = 238
      ExplicitHeight = 557
    end
    object pOptions: TPanel
      Left = 5
      Top = 544
      Width = 236
      Height = 51
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 236
        Height = 2
        Align = alTop
        Shape = bsTopLine
        ExplicitWidth = 203
      end
      object cbIgnoreDims: TCheckBox
        Left = 6
        Top = 8
        Width = 198
        Height = 17
        Caption = 'Do not apply dimensional properties'
        TabOrder = 0
        OnClick = cbIgnoreDimsClick
      end
      object cbMultiSelect: TCheckBox
        Left = 6
        Top = 29
        Width = 219
        Height = 17
        Hint = 
          'Clicked items will be added to the selection until you click the' +
          'm again. Clear selection by clicking an empty area.'#13#10'Test this o' +
          'n style with tag background colors (like Berlin or Calm).'
        Caption = 'Allow multiple items to be selected'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
    end
  end
  object Panel1: TPanel
    Left = 249
    Top = 0
    Width = 651
    Height = 600
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    ParentBackground = False
    ParentColor = True
    TabOrder = 1
    object tagCloud: TTagCloud
      Left = 5
      Top = 5
      Width = 641
      Height = 412
      Align = alClient
      AutoShrinkRows = True
      Color = 1976878
      Colors = <
        item
          Color = 6390668
          BackColor = clNone
          FrameColor = clNone
        end
        item
          Color = 7902621
          BackColor = clNone
          FrameColor = clNone
        end
        item
          Color = 8625828
          BackColor = clNone
          FrameColor = clNone
        end
        item
          Color = 11321023
          BackColor = clNone
          FrameColor = clNone
        end
        item
          Color = 14147810
          BackColor = clNone
          FrameColor = clNone
        end
        item
          Color = 16251385
          BackColor = clNone
          FrameColor = clNone
        end>
      ColSpacing = 6
      Direction = tcdHorizontal
      FixedColFullFrame = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6390668
      Font.Height = -11
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      HoverColor = clNone
      HoverCursor = crHandPoint
      HoverStyle = [fsUnderline]
      HoverEnlarge = False
      ItemFrame.BackColor = clNone
      ItemFrame.FrameColor = clNone
      ItemFrame.FrameMargin = 0
      ItemFrame.FrameSize = 0
      ItemFrame.FrameStyle = psSolid
      ItemFrame.RoundedSize = 0
      HoverFrame.BackColor = clNone
      HoverFrame.FrameColor = clNone
      HoverFrame.FrameMargin = 0
      HoverFrame.FrameSize = 0
      HoverFrame.FrameStyle = psSolid
      HoverFrame.RoundedSize = 0
      SelectedFrame.BackColor = clNone
      SelectedFrame.FrameColor = clNone
      SelectedFrame.FrameMargin = 0
      SelectedFrame.FrameSize = 0
      SelectedFrame.FrameStyle = psSolid
      SelectedFrame.RoundedSize = 0
      Items = <
        item
          Caption = 'Item1'
          Value = 1
        end
        item
          Caption = 'Item2'
          Value = 4
        end
        item
          Caption = 'Item3'
          Value = 2
        end
        item
          Caption = 'Item4'
        end
        item
          Caption = 'Item5'
          Value = 3
        end
        item
          Caption = 'Begin'
          Value = 5
        end
        item
          Caption = 'Item7'
          Value = 2
        end
        item
          Caption = 'Item8'
          Value = 1
        end
        item
          Caption = 'Item9'
        end
        item
          Caption = 'Question'
          Value = 4
        end
        item
          Caption = 'Another'
          Value = 1
        end
        item
          Caption = 'Item12'
        end
        item
          Caption = 'beta'
          Value = 4
        end
        item
          Caption = 'Item14'
          Value = 5
        end
        item
          Caption = 'Item15'
          Value = 6
        end
        item
          Caption = 'Item16'
          Value = 1
        end
        item
          Caption = 'Item17'
        end
        item
          Caption = 'Item18'
          Value = 2
        end
        item
          Caption = 'Item19'
          Value = 2
        end
        item
          Caption = 'alpha'
          Value = 3
        end
        item
          Caption = 'Item21'
          Value = 1
        end
        item
          Caption = 'item22'
        end
        item
          Caption = 'Item23'
          Value = 5
        end
        item
          Caption = 'Item24'
          Value = 2
        end
        item
          Caption = 'Test'
          Value = 6
        end
        item
          Caption = 'Item26'
          Value = 3
        end
        item
          Caption = 'Item27'
        end>
      LogScale = False
      MaxFontSize = 26
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      SelectedColor = clNone
      Styler = tcStyler
      PageIndex = 0
      ParentColor = False
      ParentFont = False
      RowSpacing = 0
      ShrinkDiacritic = False
      Sorted = False
      Transparent = False
      OnMouseDown = tagCloudMouseDown
      OnTagClick = tagCloudTagClick
      ExplicitWidth = 516
      ExplicitHeight = 454
    end
    object splBottom: TSplitter
      Left = 5
      Top = 417
      Width = 641
      Height = 6
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 426
      ExplicitWidth = 686
    end
    object pBottom: TPanel
      Left = 5
      Top = 423
      Width = 641
      Height = 172
      Align = alBottom
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 0
      object tagCloudSmall: TTagCloud
        Left = 0
        Top = 0
        Width = 285
        Height = 172
        Align = alLeft
        AutoShrinkRows = True
        Color = 1976878
        Colors = <
          item
            Color = 6390668
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 7902621
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 8625828
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 11321023
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 14147810
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 16251385
            BackColor = clNone
            FrameColor = clNone
          end>
        ColSpacing = 6
        Direction = tcdHorizontal
        FixedColFullFrame = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 6390668
        Font.Height = -11
        Font.Name = 'Trebuchet MS'
        Font.Style = []
        HoverColor = clNone
        HoverCursor = crHandPoint
        HoverStyle = [fsUnderline]
        HoverEnlarge = False
        ItemFrame.BackColor = clNone
        ItemFrame.FrameColor = clNone
        ItemFrame.FrameMargin = 0
        ItemFrame.FrameSize = 0
        ItemFrame.FrameStyle = psSolid
        ItemFrame.RoundedSize = 0
        HoverFrame.BackColor = clNone
        HoverFrame.FrameColor = clNone
        HoverFrame.FrameMargin = 0
        HoverFrame.FrameSize = 0
        HoverFrame.FrameStyle = psSolid
        HoverFrame.RoundedSize = 0
        SelectedFrame.BackColor = clNone
        SelectedFrame.FrameColor = clNone
        SelectedFrame.FrameMargin = 0
        SelectedFrame.FrameSize = 0
        SelectedFrame.FrameStyle = psSolid
        SelectedFrame.RoundedSize = 0
        Items = <
          item
            Caption = 'Item1'
            Value = 1
          end
          item
            Caption = 'Item2'
            Value = 4
          end
          item
            Caption = 'Item3'
            Value = 2
          end
          item
            Caption = 'Item4'
          end
          item
            Caption = 'Item5'
            Value = 3
          end
          item
            Caption = 'Item6'
            Value = 5
          end
          item
            Caption = 'Item7'
            Value = 2
          end
          item
            Caption = 'Item8'
            Value = 1
          end
          item
            Caption = 'Item9'
          end
          item
            Caption = 'Item10'
            Value = 4
          end
          item
            Caption = 'Item11'
            Value = 1
          end
          item
            Caption = 'Item12'
          end
          item
            Caption = 'Item13'
            Value = 4
          end
          item
            Caption = 'Item14'
            Value = 5
          end
          item
            Caption = 'Item15'
            Value = 6
          end
          item
            Caption = 'Item16'
            Value = 1
          end
          item
            Caption = 'Item17'
          end
          item
            Caption = 'Item18'
            Value = 2
          end
          item
            Caption = 'Item19'
            Value = 2
          end
          item
            Caption = 'Item20'
            Value = 3
          end
          item
            Caption = 'Item21'
            Value = 1
          end
          item
            Caption = 'Item22'
          end
          item
            Caption = 'Item23'
            Value = 5
          end
          item
            Caption = 'Item24'
            Value = 2
          end
          item
            Caption = 'Item25'
            Value = 6
          end
          item
            Caption = 'Item26'
            Value = 3
          end
          item
            Caption = 'Item27'
          end>
        LogScale = False
        MaxFontSize = 26
        Padding.Left = 4
        Padding.Top = 4
        Padding.Right = 4
        Padding.Bottom = 4
        SelectedColor = clNone
        Styler = tcStyler
        PageIndex = 0
        ParentColor = False
        ParentFont = False
        RowSpacing = 0
        ShrinkDiacritic = False
        Sorted = False
        Transparent = False
        OnMouseDown = tagCloudMouseDown
        OnTagClick = tagCloudTagClick
      end
      object Splitter1: TSplitter
        Left = 285
        Top = 0
        Width = 6
        Height = 172
        AutoSnap = False
        ExplicitLeft = 321
      end
      object tagCloudFixed: TTagCloud
        Left = 291
        Top = 0
        Width = 350
        Height = 172
        Align = alClient
        Alignment = taLeftJustify
        AutoShrinkRows = True
        Color = 1976878
        Colors = <
          item
            Color = 6390668
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 7902621
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 8625828
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 11321023
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 14147810
            BackColor = clNone
            FrameColor = clNone
          end
          item
            Color = 16251385
            BackColor = clNone
            FrameColor = clNone
          end>
        ColSpacing = 6
        Direction = tcdHorizontal
        FixedColFullFrame = False
        FixedColWidth = 110
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 6390668
        Font.Height = -11
        Font.Name = 'Trebuchet MS'
        Font.Style = []
        HoverColor = clNone
        HoverCursor = crHandPoint
        HoverStyle = [fsUnderline]
        HoverEnlarge = False
        ItemFrame.BackColor = clNone
        ItemFrame.FrameColor = clNone
        ItemFrame.FrameMargin = 0
        ItemFrame.FrameSize = 0
        ItemFrame.FrameStyle = psSolid
        ItemFrame.RoundedSize = 0
        HoverFrame.BackColor = clNone
        HoverFrame.FrameColor = clNone
        HoverFrame.FrameMargin = 0
        HoverFrame.FrameSize = 0
        HoverFrame.FrameStyle = psSolid
        HoverFrame.RoundedSize = 0
        SelectedFrame.BackColor = clNone
        SelectedFrame.FrameColor = clNone
        SelectedFrame.FrameMargin = 0
        SelectedFrame.FrameSize = 0
        SelectedFrame.FrameStyle = psSolid
        SelectedFrame.RoundedSize = 0
        Items = <
          item
            Caption = 'Item1'
            Value = 1
          end
          item
            Caption = 'Item2'
            Value = 4
          end
          item
            Caption = 'Item3'
            Value = 2
          end
          item
            Caption = 'Item4'
          end
          item
            Caption = 'Item5'
            Value = 3
          end
          item
            Caption = 'Item6'
            Value = 5
          end
          item
            Caption = 'Item7'
            Value = 2
          end
          item
            Caption = 'Item8'
            Value = 1
          end
          item
            Caption = 'Item9'
          end
          item
            Caption = 'Item10'
            Value = 4
          end
          item
            Caption = 'Item11'
            Value = 1
          end
          item
            Caption = 'Item12'
          end
          item
            Caption = 'Item13'
            Value = 4
          end
          item
            Caption = 'Item14'
            Value = 5
          end
          item
            Caption = 'Item15'
            Value = 6
          end
          item
            Caption = 'Item16'
            Value = 1
          end
          item
            Caption = 'Item17'
          end
          item
            Caption = 'Item18'
            Value = 2
          end
          item
            Caption = 'Item19'
            Value = 2
          end
          item
            Caption = 'Item20'
            Value = 3
          end
          item
            Caption = 'Item21'
            Value = 1
          end
          item
            Caption = 'Item22'
          end
          item
            Caption = 'Item23'
            Value = 5
          end
          item
            Caption = 'Item24'
            Value = 2
          end
          item
            Caption = 'Item25'
            Value = 6
          end
          item
            Caption = 'Item26'
            Value = 3
          end
          item
            Caption = 'Item27'
          end>
        LogScale = False
        MaxFontSize = 26
        Padding.Left = 4
        Padding.Top = 4
        Padding.Right = 4
        Padding.Bottom = 4
        SelectedColor = clNone
        Styler = tcStyler
        PageIndex = 0
        ParentColor = False
        ParentFont = False
        RowSpacing = 0
        ShrinkDiacritic = False
        Sorted = False
        Transparent = False
        OnMouseDown = tagCloudMouseDown
        OnTagClick = tagCloudTagClick
        ExplicitLeft = 596
        ExplicitTop = 6
        ExplicitWidth = 321
      end
    end
  end
  object tcStyler: TTagCloudPrmStyler
    IgnoreDimensions = False
    OnStyleApplied = tcStylerStyleApplied
    Params.Strings = (
      '_stylename=Mustang'
      'Color=#2E2A1E'
      'Colors_0_Color=#8C8361'
      'Colors_1_Color=#9D9578'
      'Colors_2_Color=#A49E83'
      'Colors_3_Color=#BFBEAC'
      'Colors_4_Color=#E2E0D7'
      'Colors_5_Color=#F9F9F7'
      'Font_Color=#8C8361'
      'Font_Name=Trebuchet MS'
      'Font_Style='
      'HoverColor=clNone'
      'HoverEnlarge=0'
      'HoverFrame_BackColor=clNone'
      'HoverFrame_FrameColor=clNone'
      'HoverFrame_FrameMargin=0'
      'HoverFrame_FrameSize=0'
      'HoverFrame_FrameStyle=0'
      'HoverFrame_RoundedSize=0'
      'HoverStyle=underline'
      'ItemFrame_BackColor=clNone'
      'ItemFrame_FrameColor=clNone'
      'ItemFrame_FrameMargin=0'
      'ItemFrame_FrameSize=0'
      'ItemFrame_FrameStyle=0'
      'ItemFrame_RoundedSize=0'
      'SelectedColor=clNone'
      'SelectedFrame_BackColor=clNone'
      'SelectedFrame_FrameColor=clNone'
      'SelectedFrame_FrameMargin=0'
      'SelectedFrame_FrameSize=0'
      'SelectedFrame_FrameStyle=0'
      'SelectedFrame_RoundedSize=0'
      ''
      'AutoShrinkRows=1'
      'ColSpacing=6'
      'Direction=0'
      'Font_Size=8'
      'MaxFontSize=26'
      'RowSpacing=0')
    StyleName = 'Mustang'
    Left = 88
    Top = 452
  end
end
