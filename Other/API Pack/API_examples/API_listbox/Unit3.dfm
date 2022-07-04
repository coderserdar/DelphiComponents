object Form1: TForm1
  Left = 202
  Top = 135
  Width = 550
  Height = 368
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    542
    334)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 215
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object HeaderControl1: THeaderControl
    Left = 8
    Top = 8
    Width = 509
    Height = 17
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    Sections = <
      item
        ImageIndex = -1
        Text = 'Name'
        Width = 100
      end
      item
        ImageIndex = -1
        Text = 'Phone'
        Width = 60
      end
      item
        ImageIndex = -1
        Text = 'eMail'
        Width = 200
      end>
    OnSectionResize = HeaderControl1SectionResize
    OnResize = HeaderControl1Resize
  end
  object API_listbox1: TAPI_listbox
    Left = 8
    Top = 24
    Width = 509
    Height = 185
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    Color = clWhite
    Columns = 0
    ItemHeight = 16
    TabOrder = 1
    LineColorSelected = clTeal
    LineColoring = True
    LineColorOdd = 13421772
    LineColorEven = clWhite
    MouseIsOver = False
    MouseOverColor = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    FontSelected.Charset = DEFAULT_CHARSET
    FontSelected.Color = clYellow
    FontSelected.Height = -11
    FontSelected.Name = 'Tahoma'
    FontSelected.Style = []
    FontDisabled.Charset = DEFAULT_CHARSET
    FontDisabled.Color = clWindowText
    FontDisabled.Height = -11
    FontDisabled.Name = 'MS Sans Serif'
    FontDisabled.Style = []
    DisableString = '<!--'
    ProgressExists = False
    ProgressColor = clLime
    ColumnSeparator = '||'
    ColumnDefaultWidth = 92
  end
  object Memo1: TMemo
    Left = 8
    Top = 237
    Width = 509
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
end
