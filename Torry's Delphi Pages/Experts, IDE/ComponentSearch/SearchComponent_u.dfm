object Form1: TForm1
  Left = 436
  Top = 276
  BorderStyle = bsToolWindow
  Caption = 'Search Component'
  ClientHeight = 315
  ClientWidth = 214
  Color = clBtnFace
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    214
    315)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 21
    Top = 10
    Width = 34
    Height = 13
    Caption = 'Search'
  end
  object Label2: TLabel
    Left = 24
    Top = 262
    Width = 77
    Height = 13
    Caption = 'Package Name:'
  end
  object Label3: TLabel
    Left = 110
    Top = 263
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 74
    Top = 286
    Width = 75
    Height = 25
    Anchors = []
    Caption = 'Add To Form'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 19
    Top = 39
    Width = 181
    Height = 217
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object Edit1: TEdit
    Left = 66
    Top = 8
    Width = 133
    Height = 21
    TabOrder = 2
    OnChange = Edit1Change
  end
end
