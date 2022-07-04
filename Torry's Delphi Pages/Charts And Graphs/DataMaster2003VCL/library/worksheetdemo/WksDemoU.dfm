object WKSDemoForm: TWKSDemoForm
  Left = 297
  Top = 175
  Width = 400
  Height = 341
  Caption = 'Worksheet demo'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 204
    Width = 392
    Height = 107
    Align = alBottom
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 62
      Height = 13
      Caption = 'Column titles:'
      FocusControl = TitlesMemo
    end
    object TitlesMemo: TMemo
      Left = 4
      Top = 24
      Width = 117
      Height = 77
      Lines.Strings = (
        'Value'
        'Square')
      TabOrder = 0
    end
    object CopyTitlesButton: TButton
      Left = 76
      Top = 4
      Width = 45
      Height = 17
      Caption = 'Copy'
      TabOrder = 1
      OnClick = CopyTitlesButtonClick
    end
    object AlignRightCheckBox: TCheckBox
      Left = 136
      Top = 4
      Width = 129
      Height = 21
      Caption = 'AlignRight property'
      TabOrder = 2
      OnClick = AlignRightCheckBoxClick
    end
    object DrawHeadersCheckBox: TCheckBox
      Left = 136
      Top = 20
      Width = 129
      Height = 17
      Caption = 'DrawHeaders property'
      TabOrder = 3
      OnClick = DrawHeadersCheckBoxClick
    end
    object CopyButton: TButton
      Left = 272
      Top = 8
      Width = 109
      Height = 25
      Caption = 'Copy to clipboard'
      TabOrder = 4
      OnClick = CopyButtonClick
    end
    object PasteButton: TButton
      Left = 272
      Top = 44
      Width = 109
      Height = 25
      Caption = 'Paste from clipboard'
      TabOrder = 5
      OnClick = PasteButtonClick
    end
    object InsLinesCheckBox: TCheckBox
      Left = 136
      Top = 37
      Width = 129
      Height = 13
      Caption = 'Insert Lines'
      TabOrder = 6
    end
    object DeleteButton: TButton
      Left = 272
      Top = 76
      Width = 109
      Height = 25
      Caption = 'Delete'
      TabOrder = 7
      OnClick = DeleteButtonClick
    end
    object OverwriteCheckBox: TCheckBox
      Left = 136
      Top = 53
      Width = 129
      Height = 17
      Caption = 'Overwrite'
      TabOrder = 8
    end
    object EditCheckBox: TCheckBox
      Left = 136
      Top = 70
      Width = 129
      Height = 17
      Caption = 'Editable'
      TabOrder = 9
      OnClick = EditCheckBoxClick
    end
    object ModifiedCheckBox: TCheckBox
      Left = 136
      Top = 86
      Width = 129
      Height = 17
      Caption = 'Data modified'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
    end
  end
  object Worksheet: TWorksheet
    Left = 0
    Top = 0
    Width = 392
    Height = 204
    Align = alClient
    ColCount = 27
    FixedColor = clTeal
    RowCount = 22
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goThumbTracking]
    TabOrder = 1
    Container = Container
    BlockColorF = clBlack
    BlockColorB = clAqua
    AlignRight = False
    DrawHeaders = False
    Header.Strings = (
      'a'
      'b'
      'c'
      'd'
      'e'
      'f'
      'g'
      'h'
      'i'
      'j'
      'k'
      'l'
      'm'
      'n'
      'o'
      'p'
      'q'
      'r'
      's'
      't'
      'u'
      'v'
      'w'
      'x'
      'y'
      'z')
  end
  object Container: TContainer
    FileName = 'NONAME0.dat'
    DataType = dtRealData
    UpdateCaption = False
    AutoLoad = True
    OnChanged = ContainerChanged
    Left = 172
    Top = 120
    Data = {
      0114000000020000000000000080FF3F0000000000000080FF3F020000000000
      0000800040000000000000008001400200000000000000C00040000000000000
      0090024002000000000000008001400000000000000080034002000000000000
      00A0014000000000000000C803400200000000000000C0014000000000000000
      9004400200000000000000E0014000000000000000C404400200000000000000
      80024000000000000000800540020000000000000090024000000000000000A2
      05400200000000000000A0024000000000000000C805400200000000000000B0
      024000000000000000F205400200000000000000C00240000000000000009006
      400200000000000000D0024000000000000000A906400200000000000000E002
      4000000000000000C406400200000000000000F0024000000000000000E10640
      0200000000000000800340000000000000008007400200000000000000880340
      00000000000080900740020000000000000090034000000000000000A2074002
      0000000000000098034000000000000080B407400200000000000000A0034000
      000000000000C80740}
  end
end
