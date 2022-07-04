object Form1: TForm1
  Left = 191
  Top = 111
  Width = 375
  Height = 490
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 8
    Top = 344
    Width = 121
    Height = 13
    Caption = 'Send to Notepad if open:'
  end
  object Label4: TLabel
    Left = 168
    Top = 344
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object API_listbox1: TAPI_listbox
    Left = 8
    Top = 8
    Width = 157
    Height = 325
    Style = lbOwnerDrawFixed
    Color = clWhite
    Columns = 0
    ItemHeight = 16
    TabOrder = 0
    LineColorSelected = clYellow
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
    FontSelected.Color = clWindowText
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
    Top = 360
    Width = 333
    Height = 89
    TabOrder = 1
    OnKeyDown = Memo1KeyDown
  end
  object Memo2: TMemo
    Left = 172
    Top = 8
    Width = 169
    Height = 325
    TabOrder = 2
  end
  object API_keylog1: TAPI_keylog
    Active = False
    OnKey = API_keylog1Key
    Left = 228
    Top = 4
  end
  object Timer1: TTimer
    Left = 260
    Top = 4
  end
end
