object Form2: TForm2
  Left = 197
  Top = 121
  Caption = 'Form2'
  ClientHeight = 167
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 83
    Height = 13
    Caption = 'Number of Rows:'
  end
  object Label2: TLabel
    Left = 8
    Top = 54
    Width = 97
    Height = 13
    Caption = 'Number of Columns:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 27
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '10'
  end
  object Edit2: TEdit
    Left = 8
    Top = 73
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '10'
  end
  object Button1: TButton
    Left = 8
    Top = 100
    Width = 121
    Height = 25
    Caption = 'Create Workbook'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 135
    Top = 131
    Width = 121
    Height = 25
    Caption = 'Import CSV Text'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 135
    Top = 8
    Width = 121
    Height = 86
    TabOrder = 4
    WordWrap = False
  end
  object Button4: TButton
    Left = 262
    Top = 8
    Width = 107
    Height = 25
    Caption = 'Save to File'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 262
    Top = 39
    Width = 107
    Height = 25
    Caption = 'Open from File'
    TabOrder = 6
    OnClick = Button5Click
  end
  object Button3: TButton
    Left = 135
    Top = 100
    Width = 121
    Height = 25
    Caption = 'Export CSV Text'
    TabOrder = 7
    OnClick = Button3Click
  end
  object Button6: TButton
    Left = 264
    Top = 72
    Width = 105
    Height = 25
    Caption = 'Sort by Col 0'
    TabOrder = 8
    OnClick = Button6Click
  end
  object API_workbook1: TAPI_workbook
    Author = 'nobody'
    Created = 39713.639626597220000000
    Editor = 'unknown'
    Modified = 40119.428725937500000000
    Pages = 1
    Page = 0
    Columns = 0
    Rows = 0
    Priority = TWB_text
    Left = 8
    Top = 128
  end
end
