object MainForm: TMainForm
  Left = 277
  Top = 128
  Width = 696
  Height = 480
  Caption = 'GSC Query Builder sample program'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 567
    Top = 0
    Width = 121
    Height = 224
    Align = alRight
    ItemHeight = 13
    Items.Strings = (
      'Customers'
      'Orders'
      'OrderItems'
      'Products'
      'Categories')
    MultiSelect = True
    TabOrder = 0
    OnMouseDown = ListBox1MouseDown
  end
  object GSCQBWorkArea: TGSCQBWorkArea
    Left = 0
    Top = 0
    Width = 567
    Height = 224
    Align = alClient
    AutoScroll = False
    TabOrder = 1
    ClosingClause = '/* This is the value of the ClosingClause property */'
    FieldGrid = GSCQBGrid
    OptionsClause = '/* This is the value of the OptionsClause property */ '
    TableList = ListBox1
    OnGetTableFields = GSCQBWorkAreaGetTableFields
    OnChange = GSCQBWorkAreaChange
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Table count'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Link count'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object SQLCommand: TMemo
    Left = 0
    Top = 364
    Width = 688
    Height = 89
    Align = alBottom
    Color = clBtnFace
    Lines.Strings = (
      'Click here to generate script')
    ReadOnly = True
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 344
    Width = 688
    Height = 20
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object CheckBox1: TCheckBox
      Left = 4
      Top = 2
      Width = 125
      Height = 17
      Caption = 'Use '#39'Group By'#39' clause'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
  end
  object GSCQBGrid: TGSCQBGrid
    Left = 0
    Top = 224
    Width = 688
    Height = 120
    Align = alBottom
    ColCount = 9
    DefaultRowHeight = 16
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goEditing]
    TabOrder = 4
    WorkArea = GSCQBWorkArea
    UseGroupBy = False
    OnChange = GSCQBWorkAreaChange
    ColWidths = (
      11
      100
      64
      100
      40
      80
      80
      64
      80)
  end
end
