object Form1: TForm1
  Left = 192
  Top = 114
  Width = 686
  Height = 621
  Caption = 'SmWinList'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 384
    Top = 48
    Width = 87
    Height = 13
    Caption = 'Count of Windows'
  end
  object Label2: TLabel
    Left = 384
    Top = 72
    Width = 103
    Height = 13
    Caption = 'Name Active Window'
  end
  object Label3: TLabel
    Left = 384
    Top = 96
    Width = 110
    Height = 13
    Caption = 'HWND Active Window'
  end
  object Label4: TLabel
    Left = 388
    Top = 125
    Width = 43
    Height = 13
    Caption = 'WinType'
  end
  object Button1: TButton
    Left = 384
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = Button1Click
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 8
    Width = 369
    Height = 561
    ColCount = 2
    FixedCols = 0
    TabOrder = 1
    ColWidths = (
      232
      86)
  end
  object Edit1: TEdit
    Left = 502
    Top = 45
    Width = 121
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 502
    Top = 69
    Width = 121
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 3
  end
  object Edit3: TEdit
    Left = 502
    Top = 93
    Width = 121
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 4
  end
  object ComboBox1: TComboBox
    Left = 504
    Top = 117
    Width = 97
    Height = 21
    Ctl3D = True
    ItemHeight = 13
    ItemIndex = 0
    ParentCtl3D = False
    TabOrder = 5
    Text = 'wnAll'
    Items.Strings = (
      'wnAll'
      'wnVisible')
  end
  object SmWinList1: TSmWinList
    WinType = wnAll
    Left = 472
    Top = 8
  end
end
