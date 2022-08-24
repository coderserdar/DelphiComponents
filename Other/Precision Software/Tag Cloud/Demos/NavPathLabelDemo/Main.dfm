object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'NavPathLabel Demo'
  ClientHeight = 348
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Height = 348
    AutoSnap = False
    ExplicitLeft = 324
    ExplicitTop = 136
    ExplicitHeight = 100
  end
  object pLeft: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 348
    Align = alLeft
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object tvTree: TTreeView
      Left = 0
      Top = 0
      Width = 185
      Height = 348
      Align = alClient
      HideSelection = False
      Indent = 19
      TabOrder = 0
      OnChange = tvTreeChange
      Items.NodeData = {
        0302000000360000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        0003000000010C500072006F006400750063007400200074007200650065003A
        0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000300000001
        0E42007500730069006E00650073007300200074006F006F006C0073003E0000
        000000000000000000FFFFFFFFFFFFFFFF00000000000000000000000001104D
        006900630072006F0073006F006600740020004F006600660069006300650034
        0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000000001
        0B4F00700065006E0020004F0066006600690063006500340000000000000000
        000000FFFFFFFFFFFFFFFF000000000000000000000000010B51007500690063
        006B00200042006F006F006B0073003C0000000000000000000000FFFFFFFFFF
        FFFFFF000000000000000002000000010F44006500760065006C006F00700065
        007200200074006F006F006C0073004A0000000000000000000000FFFFFFFFFF
        FFFFFF000000000000000002000000011645006D006200610072006300610064
        00650072006F0020005200410044002000530074007500640069006F002A0000
        000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000010644
        0065006C00700068006900340000000000000000000000FFFFFFFFFFFFFFFF00
        0000000000000000000000010B43002B002B0020004200750069006C00640065
        0072004C0000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        00000001174D006900630072006F0073006F0066007400200056006900730075
        0061006C002000530074007500640069006F00300000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000200000001095500740069006C00690074
        006900650073003E0000000000000000000000FFFFFFFFFFFFFFFF0000000000
        000000000000000110500072006F00630065007300730020004500780070006C
        006F00720065007200320000000000000000000000FFFFFFFFFFFFFFFF000000
        000000000000000000010A5200610069006E006C0065006E0064006100720038
        0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000200000001
        0D53006500720076006900630065007300200074007200650065003400000000
        00000000000000FFFFFFFFFFFFFFFF000000000000000000000000010B440065
        00760065006C006F0070006D0065006E007400320000000000000000000000FF
        FFFFFFFFFFFFFF000000000000000000000000010A43006F006E00730075006C
        00740069006E006700}
    end
  end
  object pMain: TPanel
    Left = 188
    Top = 0
    Width = 455
    Height = 348
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    BorderStyle = bsSingle
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    object lbContent: TLabel
      Left = 6
      Top = 37
      Width = 439
      Height = 260
      Align = alClient
      Caption = 'lbContent'
      WordWrap = True
      OnMouseDown = lbContentMouseDown
      ExplicitWidth = 47
      ExplicitHeight = 13
    end
    object pPath: TPanel
      Left = 6
      Top = 6
      Width = 439
      Height = 31
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 0
      object lbPath: TNavPathLabel
        Left = 0
        Top = 0
        Width = 439
        Height = 13
        Align = alTop
        Caption = 'lbPath'
        HoverColor = clBlue
        HoverStyle = [fsUnderline]
        NavPathColor = clWindowText
        PathDelimiter = '\'
        NavPathDelimiter = '  a  '
        OnPathClick = lbPathPathClick
        ExplicitWidth = 30
      end
      object Bevel2: TBevel
        Left = 0
        Top = 23
        Width = 439
        Height = 8
        Align = alBottom
        Shape = bsTopLine
        ExplicitTop = 27
      end
    end
    object pOptions: TPanel
      Left = 6
      Top = 297
      Width = 439
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 15
        Width = 95
        Height = 13
        Caption = 'Path delimiter style:'
      end
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 439
        Height = 4
        Align = alTop
        Shape = bsTopLine
      end
      object cbNavPathDelim: TComboBox
        Left = 120
        Top = 12
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'Arrow'
        OnChange = cbNavPathDelimChange
        Items.Strings = (
          'Arrow'
          #187
          #171
          #8250
          #8249
          '>'
          '<'
          '::'
          ':')
      end
    end
  end
end
