object CopyTab2TabForm: TCopyTab2TabForm
  Left = 83
  Top = 133
  Width = 624
  Height = 398
  HorzScrollBar.Range = 383
  VertScrollBar.Range = 36
  ActiveControl = BtnOk
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'CopyTab2TabForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 377
    Top = 36
    Width = 6
    Height = 307
    Cursor = crHSplit
    Beveled = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 616
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 616
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object LabVar: TLabel
      Left = 128
      Top = 12
      Width = 36
      Height = 13
      Caption = 'Variant:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentFont = False
    end
    object SpeedButtonGetVar: TSpeedButton
      Left = 392
      Top = 7
      Width = 23
      Height = 22
      Cursor = crHandPoint
      Flat = True
      Glyph.Data = {
        5A010000424D5A01000000000000760000002800000013000000130000000100
        040000000000E4000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777777000007777777000000000007000007777777078888888807000007777
        7770F77777778070000077777770F99777778070000077777770FFFFFFFF7070
        0000777777700000000000700000777777777777777777700000777777777774
        4477777000007777777777CCC477777000007777447777CCC477777000007777
        C47777CCC47777700000777CC44444CCC4777770000077CCCCCCCCCCC4777770
        00007CCCCCCCCCCC47777770000077CCCCCCCCC4777777700000777CC4777777
        7777777000007777C7777777777777700000777777777777777777700000}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButtonGetVarClick
    end
    object SpeedButtonSetVar: TSpeedButton
      Left = 416
      Top = 7
      Width = 23
      Height = 22
      Cursor = crHandPoint
      Flat = True
      Glyph.Data = {
        5A010000424D5A01000000000000760000002800000013000000130000000100
        040000000000E4000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777777000007777777000000000007000007777777078888888807000007777
        7770F77777778070000077777770F99777778070000077777770FFFFFFFF7070
        0000777777700000000000700000777777777777777777700000777777777779
        1777777000007777777777999177777000007777777779999917777000007777
        7777999999917770000077777777779991777770000077711111119991777770
        0000779999999999917777700000779999999999177777700000779999999991
        777777700000777777777777777777700000777777777777777777700000}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButtonSetVarClick
    end
    object BtnOk: TBitBtn
      Left = 8
      Top = 6
      Width = 105
      Height = 25
      Caption = 'BtnOk'
      Default = True
      ModalResult = 1
      TabOrder = 0
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333333333333333333333333333333333FF333333333333
        3000333333FFFFF3F77733333000003000B033333777773777F733330BFBFB00
        E00033337FFF3377F7773333000FBFB0E000333377733337F7773330FBFBFBF0
        E00033F7FFFF3337F7773000000FBFB0E000377777733337F7770BFBFBFBFBF0
        E00073FFFFFFFF37F777300000000FB0E000377777777337F7773333330BFB00
        000033333373FF77777733333330003333333333333777333333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
    end
    object EditVarName: TComboBox
      Left = 192
      Top = 8
      Width = 193
      Height = 21
      Hint = 
        'Вариант настройки'#13#10'(новые варианты просто вписываются от руки).'#13 +
        #10'Сохранение производится автоматически.'
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnChange = EditVarNameChange
    end
  end
  object GridDestination: TStringGrid
    Left = 0
    Top = 36
    Width = 377
    Height = 307
    Align = alLeft
    ColCount = 3
    DefaultRowHeight = 20
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    PopupMenu = Menu
    ScrollBars = ssVertical
    TabOrder = 1
    OnDblClick = GridDestinationDblClick
    OnDragDrop = GridDestinationDragDrop
    OnDragOver = GridDestinationDragOver
    ColWidths = (
      205
      132
      13)
  end
  object Panel2: TPanel
    Left = 383
    Top = 36
    Width = 233
    Height = 307
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 0
      Top = 238
      Width = 233
      Height = 6
      Cursor = crVSplit
      Align = alBottom
      Beveled = True
    end
    object GridSource: TStringGrid
      Left = 0
      Top = 0
      Width = 233
      Height = 238
      Align = alClient
      ColCount = 1
      DefaultColWidth = 100
      DefaultRowHeight = 20
      DragMode = dmAutomatic
      FixedCols = 0
      PopupMenu = Menu
      ScrollBars = ssVertical
      TabOrder = 0
      OnDblClick = GridDestinationDblClick
      OnStartDrag = GridSourceStartDrag
      ColWidths = (
        206)
    end
    object Memo: TMemo
      Left = 0
      Top = 244
      Width = 233
      Height = 63
      Align = alBottom
      ReadOnly = True
      TabOrder = 1
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 343
    Width = 616
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object LabFilterFields: TLabel
      Left = 8
      Top = 7
      Width = 51
      Height = 14
      Caption = 'FilterFields'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 246
      Top = 7
      Width = 6
      Height = 14
      Caption = '='
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Pitch = fpVariable
      Font.Style = []
      ParentFont = False
    end
    object EditFilterName: TComboBox
      Left = 96
      Top = 3
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = EditFilterNameChange
    end
    object EditFilterValue: TComboBox
      Left = 255
      Top = 3
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      OnChange = EditFilterValueChange
    end
  end
  object Menu: TPopupMenu
    OnPopup = MenuPopup
    Left = 200
    Top = 168
    object NSet: TMenuItem
      Caption = 'NSet'
      OnClick = NSetClick
    end
    object NDelete: TMenuItem
      Caption = 'NUnSet'
      Enabled = False
      OnClick = NDeleteClick
    end
    object NClearAll: TMenuItem
      Caption = 'ClearAll'
      OnClick = NClearAllClick
    end
  end
end
