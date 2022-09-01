object ColEditForm: TColEditForm
  Left = 81
  Top = 45
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'ColEditForm'
  ClientHeight = 380
  ClientWidth = 530
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel9: TBevel
    Left = 462
    Top = 215
    Width = 54
    Height = 27
  end
  object Bevel3: TBevel
    Left = 327
    Top = 183
    Width = 189
    Height = 27
  end
  object Bevel4: TBevel
    Left = 406
    Top = 278
    Width = 110
    Height = 77
  end
  object Bevel5: TBevel
    Left = 0
    Top = 0
    Width = 530
    Height = 4
    Align = alTop
    Shape = bsTopLine
  end
  object SpeedButtonIncFont: TSpeedButton
    Left = 464
    Top = 216
    Width = 25
    Height = 25
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000C40E0000C40E00001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
      DD0DDDDDDDDDDDDDD000DDDDDDDDDDDD000DDDDDDDDDDDD000DDDDDDDDDDDD00
      0DDDDDD80008DB80DDDDDD08888800BDDDDDD08F7F7F80DDDDDD88F7F9F7F88D
      DDDD087F797F780DDDDD08F99999F80DDDDD087F797F780DDDDD88F7F9F7F88D
      DDDDD08F7F7F80DDDDDDDD0888880DDDDDDDDDD80008DDDDDDDD}
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButtonIncFontClick
  end
  object SpeedButtonDecFont: TSpeedButton
    Left = 490
    Top = 216
    Width = 25
    Height = 25
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000C40E0000C40E00001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
      DD0DDDDDDDDDDDDDD000DDDDDDDDDDDD000DDDDDDDDDDDD000DDDDDDDDDDDD00
      0DDDDDD80008DB80DDDDDD08888800BDDDDDD08F7F7F80DDDDDD88F7F7F7F88D
      DDDD087F7F7F780DDDDD08F99999F80DDDDD087F7F7F780DDDDD88F7F7F7F88D
      DDDDD08F7F7F80DDDDDDDD0888880DDDDDDDDDD80008DDDDDDDD}
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButtonDecFontClick
  end
  object SGFields: TStringGrid
    Left = 16
    Top = 16
    Width = 497
    Height = 161
    ColCount = 10
    DefaultColWidth = 20
    DefaultRowHeight = 20
    FixedCols = 2
    RowCount = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowMoving, goEditing]
    ParentFont = False
    PopupMenu = PopupMenu1
    TabOrder = 7
    OnDrawCell = SGFieldsDrawCell
    OnMouseDown = SGFieldsMouseDown
    OnSelectCell = SGFieldsSelectCell
    OnSetEditText = SGFieldsSetEditText
    ColWidths = (
      20
      91
      199
      66
      41
      67
      45
      64
      41
      42)
    RowHeights = (
      20
      20)
  end
  object BitBtnOk: TBitBtn
    Left = 408
    Top = 279
    Width = 107
    Height = 25
    Cursor = crHandPoint
    Caption = 'Ok'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = BitBtnOkClick
    Kind = bkOK
  end
  object BitBtnCancel: TBitBtn
    Left = 408
    Top = 329
    Width = 107
    Height = 25
    Cursor = crHandPoint
    Caption = 'BitBtnCancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Kind = bkCancel
  end
  object BBDefault: TBitBtn
    Left = 408
    Top = 304
    Width = 107
    Height = 25
    Cursor = crHandPoint
    Caption = 'BBDefault'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = BBDefaultClick
    Kind = bkRetry
  end
  object CheckReadOnly: TCheckBox
    Left = 16
    Top = 184
    Width = 193
    Height = 17
    Caption = 'CheckReadOnly'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
  end
  object BitBtnInsert: TBitBtn
    Left = 422
    Top = 184
    Width = 93
    Height = 25
    Caption = 'BitBtnInsert'
    TabOrder = 4
    OnClick = BitBtnInsertClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333FF33333333FF333993333333300033377F3333333777333993333333
      300033F77FFF3333377739999993333333333777777F3333333F399999933333
      33003777777333333377333993333333330033377F3333333377333993333333
      3333333773333333333F333333333333330033333333F33333773333333C3333
      330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
      993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
      333333333337733333FF3333333C333330003333333733333777333333333333
      3000333333333333377733333333333333333333333333333333}
    NumGlyphs = 2
  end
  object BitBtnDelete: TBitBtn
    Left = 328
    Top = 184
    Width = 94
    Height = 25
    Caption = 'BitBtnDelete'
    TabOrder = 5
    OnClick = BitBtnDeleteClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333FF33333333333330003333333333333777333333333333
      300033FFFFFF3333377739999993333333333777777F3333333F399999933333
      3300377777733333337733333333333333003333333333333377333333333333
      3333333333333333333F333333333333330033333F33333333773333C3333333
      330033337F3333333377333CC3333333333333F77FFFFFFF3FF33CCCCCCCCCC3
      993337777777777F77F33CCCCCCCCCC399333777777777737733333CC3333333
      333333377F33333333FF3333C333333330003333733333333777333333333333
      3000333333333333377733333333333333333333333333333333}
    NumGlyphs = 2
  end
  object PageControl: TPageControl
    Left = 16
    Top = 216
    Width = 369
    Height = 153
    ActivePage = TabSheetOptions
    HotTrack = True
    TabIndex = 0
    TabOrder = 6
    object TabSheetOptions: TTabSheet
      Caption = 'TabSheetOptions'
      ImageIndex = 1
      object Bevel7: TBevel
        Left = 0
        Top = 0
        Width = 361
        Height = 125
        Align = alClient
      end
      object LabColCount: TLabel
        Left = 276
        Top = 8
        Width = 61
        Height = 13
        Alignment = taRightJustify
        Caption = 'LabColCount'
        Transparent = True
      end
      object LabTitleMinHeight: TLabel
        Left = 200
        Top = 48
        Width = 73
        Height = 41
        AutoSize = False
        Caption = 'LabTitle MinHeight'
        Transparent = True
        Layout = tlCenter
        WordWrap = True
      end
      object Check3dvInner: TSpeedButton
        Left = 48
        Top = 62
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 1554456
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
          77777777777777777777778F7778F7778F77778F7778F7778F77778F7778F777
          8F77778F7778F7778F77778F7778F7778F77778F7778F7778F77778F7778F777
          8F77778F7778F7778F77778F7778F7778F77778F7778F7778F77778F7778F777
          8F77778F7778F7778F77778F7778F7778F777777777777777777}
        ParentShowHint = False
        ShowHint = True
        OnClick = Check3dvInnerClick
      end
      object Check3dvOuter: TSpeedButton
        Left = 72
        Top = 62
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 456461
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
          7777777777777777777777F8777F8777F87777F8777F8777F87777F8777F8777
          F87777F8777F8777F87777F8777F8777F87777F8777F8777F87777F8777F8777
          F87777F8777F8777F87777F8777F8777F87777F8777F8777F87777F8777F8777
          F87777F8777F8777F87777F8777F8777F8777777777777777777}
        ParentShowHint = False
        ShowHint = True
        OnClick = Check3dvInnerClick
      end
      object CheckHORZLINE: TSpeedButton
        Left = 16
        Top = 40
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 767521
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333000000000000033300000000000
          0033383333383333383338333338333338333833333833333833300000000000
          0033300000000000003338333338333338333833333833333833383333383333
          3833300000000000003330000000000000333333333333333333}
        ParentShowHint = False
        ShowHint = True
      end
      object CHECKVertLine: TSpeedButton
        Left = 16
        Top = 62
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 346578
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333008888008888003300333300333
          3003300333300333300330033330033330033003333003333003300888800888
          8003300333300333300330033330033330033003333003333003300333300333
          3003300888800888800333333333333333333333333333333333}
        ParentShowHint = False
        ShowHint = True
      end
      object Check3dHOuter: TSpeedButton
        Left = 72
        Top = 40
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 567567567
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
          7777777777777777777778888888888888777FFFFFFFFFFFFF77777777777777
          77777777777777777777777777777777777778888888888888777FFFFFFFFFFF
          FF77777777777777777777777777777777777777777777777777788888888888
          88777FFFFFFFFFFFFF7777777777777777777777777777777777}
        ParentShowHint = False
        ShowHint = True
        OnClick = Check3dHInnerClick
      end
      object Check3dHInner: TSpeedButton
        Left = 48
        Top = 40
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 6547657
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
          777777777777777777777FFFFFFFFFFFFF777888888888888877777777777777
          7777777777777777777777777777777777777FFFFFFFFFFFFF77788888888888
          88777777777777777777777777777777777777777777777777777FFFFFFFFFFF
          FF77788888888888887777777777777777777777777777777777}
        ParentShowHint = False
        ShowHint = True
        OnClick = Check3dHInnerClick
      end
      object CheckTitle: TSpeedButton
        Left = 16
        Top = 8
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 456546
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
          777777777777777777777F777778F77777877F777778F77777877F777778F777
          77877F777778F77777877F777778F77777877F777778F77777877FFFFFFFFFFF
          FF877F888888888888877F777777777777877F707070707077877F7077707070
          77877F777777777777877FFFFFFFFFFFFF877777777777777777}
        ParentShowHint = False
        ShowHint = True
      end
      object CheckIndicator: TSpeedButton
        Left = 40
        Top = 8
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 797
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
          777778888888888888777F7777778F7777777F7777778F7777777F7777778F77
          77777F7077778F7777777F7007778F7777777F7000778F7777777F7000078F77
          77777F7000778F7777777F7007778F7777777F7077778F7777777F7777778F77
          77777F7777778F7777777FFFFFFFFFFFFF777777777777777777}
        ParentShowHint = False
        ShowHint = True
      end
      object CheckSelectROws: TSpeedButton
        Left = 88
        Top = 8
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 6789
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
          7777777777777777777777777777777777777F888F8888F888877F778F7778F7
          77877F778F7778F777877FFFFFFFFFFFFF877F888F8888F888877F448F4448F4
          44877F448F4448F444877FFFFFFFFFFFFF877F888F8888F888877F778F7778F7
          77877F778F7778F777877FFFFFFFFFFFFF877777777777777777}
        ParentShowHint = False
        ShowHint = True
      end
      object CheckMultiSelect: TSpeedButton
        Left = 112
        Top = 8
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 789
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003F888F8888F8
          88877F778F7778F777877F778F7778F777877FFFFFFFFFFFFFF77F888F8888F8
          88877F448F4448F444877F448F4448F444877FFFFFFFFFFFFF877F888F8888F8
          88877F448F4448F444877F448F4448F444877FFFFFFFFFFFFF877F888F8888F8
          88877F778F7778F777877F778F7778F777877FFFFFFFFFFFFF87}
        ParentShowHint = False
        ShowHint = True
      end
      object CheckCELLHint: TSpeedButton
        Left = 64
        Top = 8
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 78789
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333300000000000
          000330BBBBBBBBBBBB0330BBB00BBB0BBB0330BBB0B0B0B0BB0330BBB00BB0B0
          BB03330BB0BBB0B0BB033330BBBBBBBBBB0333330BBBBBBBBB03333330000000
          0003333333333333333333333333333333333333333333333333}
        ParentShowHint = False
        ShowHint = True
      end
      object LabCellROWS: TLabel
        Left = 200
        Top = 88
        Width = 73
        Height = 25
        AutoSize = False
        Caption = 'Lab Cell Rows'
        Transparent = True
        WordWrap = True
      end
      object LabTitleMinHeight1: TLabel
        Left = 344
        Top = 60
        Width = 8
        Height = 13
        Caption = '%'
      end
      object LabCellROWS1: TLabel
        Left = 344
        Top = 92
        Width = 8
        Height = 13
        Caption = '%'
      end
      object CheckAlwaysShowEditor: TSpeedButton
        Left = 136
        Top = 8
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 546456
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0037777BBBBBB7
          7777778FB7777778F777778F77777778FB777FFFFFFFFFFFFFF7788F88888888
          F887B78F00707778F77BB78F07707778F77BB78F70707778F77BB78F00707778
          F77BB78F77707778F77BB78F77707778F77B7B8F77707778F7B77FFFFFFFFFFF
          FFF7788F88888888F887778FB7777778F77777777BBBBBB77777}
        ParentShowHint = False
        ShowHint = True
      end
      object CheckAlwaysShowSelection: TSpeedButton
        Left = 160
        Top = 8
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 667575755
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
          7777777777777777777777777777777777777F888FBBBBF888877F77BF7778FB
          77877F7B8F7778F7B7877FFFFFFFFFFFFF877FB88F8888F88B877FB78F4448F7
          7B877FB78F4448F77B877FFFFFFFFFFFFF877FB88F8888F88B877F7B8F7778F7
          B7877F77BF7778FB77877FFFFFBBBBFFFF877777777777777777}
        ParentShowHint = False
        ShowHint = True
      end
      object CheckTabStop: TSpeedButton
        Left = 184
        Top = 8
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 22342643
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000C40E0000C40E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7777777777777777C777777777777777C77777777777C777C77777777777CC77
          C777777CCCCCCCC7C77777777777CC77C77777C77777C777C77777C777777777
          C77777C777C77777C77777C77CC77777777777C7CCCCCCC7777777C77CC77777
          777777C777C77777777777C77777777777777777777777777777}
        ParentShowHint = False
        ShowHint = True
      end
      object CheckEnter2Tab: TCheckBox
        Left = 16
        Top = 92
        Width = 169
        Height = 17
        Caption = 'CheckEnter2Tab'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object EditColCount: TUpDown
        Left = 321
        Top = 24
        Width = 16
        Height = 21
        Associate = EditColCountP
        Min = 0
        Position = 0
        TabOrder = 1
        Wrap = False
      end
      object EditColCountP: TEdit
        Left = 280
        Top = 24
        Width = 41
        Height = 21
        Cursor = crHandPoint
        TabOrder = 2
        Text = '0'
      end
      object EditTitleMinHeightP: TEdit
        Left = 280
        Top = 56
        Width = 41
        Height = 21
        Cursor = crHandPoint
        TabOrder = 3
        Text = '100'
      end
      object EditTitleMinHeight: TUpDown
        Left = 321
        Top = 56
        Width = 16
        Height = 21
        Associate = EditTitleMinHeightP
        Min = 1
        Max = 1000
        Increment = 5
        Position = 100
        TabOrder = 4
        Wrap = False
      end
      object EditCellRowsP: TEdit
        Left = 280
        Top = 86
        Width = 41
        Height = 21
        Cursor = crHandPoint
        TabOrder = 5
        Text = '100'
      end
      object EditCellRows: TUpDown
        Left = 321
        Top = 86
        Width = 16
        Height = 21
        Associate = EditCellRowsP
        Min = 1
        Max = 1000
        Increment = 5
        Position = 100
        TabOrder = 6
        Wrap = False
      end
    end
    object TabSheetColumns: TTabSheet
      Caption = 'TabSheetColumns'
      object Bevel6: TBevel
        Left = 0
        Top = 0
        Width = 361
        Height = 125
        Align = alClient
      end
      object Bevel2: TBevel
        Left = 144
        Top = 8
        Width = 209
        Height = 89
      end
      object Bevel1: TBevel
        Left = 16
        Top = 8
        Width = 105
        Height = 89
      end
      object LabelContents: TLabel
        Left = 152
        Top = 3
        Width = 68
        Height = 13
        Caption = 'LabelContents'
      end
      object LabelTitle: TLabel
        Left = 24
        Top = 3
        Width = 46
        Height = 13
        Caption = 'LabelTitle'
      end
      object EditColor: TBitBtn
        Left = 152
        Top = 22
        Width = 89
        Height = 29
        Cursor = crHandPoint
        Caption = 'Color'
        TabOrder = 0
        OnClick = EditColorClick
        Glyph.Data = {
          96010000424D9601000000000000760000002800000018000000180000000100
          0400000000002001000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          33333333333333333333333333333333333333F88888888888888888833333FA
          AAAA0EEEEE0DDDDD833333FAAAAA0EEEEE0DDDDD833333FAAAAA0EEEEE0DDDDD
          833333FAAAAA0EEEEE0DDDDD833333FAAAAA0EEEEE0DDDDD833333F000000000
          00000000833333F888880FFFFF000000833333F888880FFFFF000000833333F8
          88880FFFFF000000833333F888880FFFFF000000833333F888880FFFFF000000
          833333F00000000000000000833333F999990BBBBB0CCCCC833333F999990BBB
          BB0CCCCC833333F999990BBBBB0CCCCC833333F999990BBBBB0CCCCC833333F9
          99990BBBBB0CCCCC833333FFFFFFFFFFFFFFFFFFF33333333333333333333333
          3333333333333333333333333333333333333333333333333333}
      end
      object EditFixedColor: TBitBtn
        Left = 24
        Top = 22
        Width = 89
        Height = 29
        Cursor = crHandPoint
        Caption = 'Color'
        TabOrder = 1
        OnClick = EditFixedColorClick
        Glyph.Data = {
          96010000424D9601000000000000760000002800000018000000180000000100
          0400000000002001000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          33333333333333333333333333333333333333F88888888888888888833333FA
          AAAA0EEEEE0DDDDD833333FAAAAA0EEEEE0DDDDD833333FAAAAA0EEEEE0DDDDD
          833333FAAAAA0EEEEE0DDDDD833333FAAAAA0EEEEE0DDDDD833333F000000000
          00000000833333F888880FFFFF000000833333F888880FFFFF000000833333F8
          88880FFFFF000000833333F888880FFFFF000000833333F888880FFFFF000000
          833333F00000000000000000833333F999990BBBBB0CCCCC833333F999990BBB
          BB0CCCCC833333F999990BBBBB0CCCCC833333F999990BBBBB0CCCCC833333F9
          99990BBBBB0CCCCC833333FFFFFFFFFFFFFFFFFFF33333333333333333333333
          3333333333333333333333333333333333333333333333333333}
      end
      object EditFont: TBitBtn
        Left = 152
        Top = 54
        Width = 89
        Height = 29
        Cursor = crHandPoint
        Caption = 'Font'
        TabOrder = 2
        OnClick = EditFontClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000CE0E0000D80E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7777777744444444444477777777777777777777444447774444777777447777
          7447777777744777744777777777444444477777777774477447707770777744
          7447708780777774444778000877777744477707077777777447770707777777
          7777778087777777777777707777777777777777777777777777}
      end
      object EditTitleFont: TBitBtn
        Left = 24
        Top = 54
        Width = 89
        Height = 29
        Cursor = crHandPoint
        Caption = 'Font'
        TabOrder = 3
        OnClick = EditTitleFontClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000CE0E0000D80E00001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7777777744444444444477777777777777777777444447774444777777447777
          7447777777744777744777777777444444477777777774477447707770777744
          7447708780777774444778000877777744477707077777777447770707777777
          7777778087777777777777707777777777777777777777777777}
      end
      object CheckGridFixedColor: TCheckBox
        Left = 248
        Top = 28
        Width = 97
        Height = 17
        Caption = 'GridFixedColor'
        TabOrder = 4
      end
    end
    object TabSheetStrippedRows: TTabSheet
      Caption = 'TabSheetStrippedRows'
      ImageIndex = 2
      object Bevel8: TBevel
        Left = 0
        Top = 0
        Width = 361
        Height = 125
        Align = alClient
      end
      object LabStrippedRows: TLabel
        Left = 10
        Top = 11
        Width = 84
        Height = 30
        AutoSize = False
        Caption = 'LabStrippedRows'
        WordWrap = True
      end
      object LabelEach: TLabel
        Left = 104
        Top = 16
        Width = 51
        Height = 13
        Caption = 'LabelEach'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object LabelY: TLabel
        Left = 232
        Top = 16
        Width = 9
        Height = 13
        Caption = '---'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object LabelColor: TLabel
        Left = 10
        Top = 51
        Width = 45
        Height = 13
        Caption = 'labelcolor'
        WordWrap = True
      end
      object EditStrippedColor: TShape
        Left = 160
        Top = 48
        Width = 65
        Height = 25
        Cursor = crHandPoint
        Shape = stRoundRect
        OnMouseUp = EditStrippedColorMouseUp
      end
      object EditStrippedRowsP: TEdit
        Left = 160
        Top = 16
        Width = 49
        Height = 21
        Cursor = crHandPoint
        TabOrder = 0
        Text = '100'
      end
      object EditStrippedRows: TUpDown
        Left = 209
        Top = 16
        Width = 14
        Height = 21
        Associate = EditStrippedRowsP
        Min = 0
        Position = 100
        TabOrder = 1
        Wrap = False
      end
    end
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Options = [cdFullOpen]
    Left = 392
    Top = 128
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Options = [fdEffects, fdApplyButton]
    Left = 344
    Top = 128
  end
  object PopupMenu1: TPopupMenu
    Left = 272
    Top = 104
    object NInsert: TMenuItem
      Caption = 'NInsert'
      OnClick = NInsertClick
    end
    object NDelete: TMenuItem
      Caption = 'NDelete'
      OnClick = NDeleteClick
    end
  end
end
