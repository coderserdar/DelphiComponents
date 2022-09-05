object Form1: TForm1
  Left = 293
  Top = 113
  BorderStyle = bsDialog
  Caption = 'Read Multi TIFF'
  ClientHeight = 425
  ClientWidth = 337
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object mcmImageCtrl1: TmcmImageCtrl
    Left = 8
    Top = 8
    Width = 321
    Height = 353
    BorderStyle = BS_SINGLE
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = True
  end
  object lNoImages: TLabel
    Left = 104
    Top = 400
    Width = 54
    Height = 13
    Caption = 'No Images:'
  end
  object btnRead: TButton
    Left = 8
    Top = 392
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 0
    OnClick = btnReadClick
  end
  object ScrollBar1: TScrollBar
    Left = 8
    Top = 368
    Width = 321
    Height = 16
    TabOrder = 1
    OnChange = ScrollBar1Change
  end
  object OpenDialog1: TOpenDialog
    Filter = 'TIFF|*.tif'
    Left = 16
    Top = 16
  end
end
