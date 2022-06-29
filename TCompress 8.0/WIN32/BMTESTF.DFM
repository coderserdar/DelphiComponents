object Form1: TForm1
  Left = 275
  Top = 514
  Width = 323
  Height = 248
  Caption = 'Compressed Bitmaps from Resources'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 241
    Height = 129
    Stretch = True
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 246
    Height = 113
    Lines.Strings = (
      'Program for testing loading of compressed '
      'resources (see also Selfextr.dpr and '
      'Selfxsml.dpr).'
      ''
      'Before you do ANYTHING, read the comments at '
      'the top of the  BMTESTF.PAS unit in this project.'
      ''
      'Then follow the instructions therein.')
    TabOrder = 2
  end
  object Button1: TButton
    Left = 8
    Top = 144
    Width = 75
    Height = 25
    Hint = 'Creates a compressed bitmap file (do first)'
    Caption = '&Create'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 178
    Top = 144
    Width = 75
    Height = 25
    Hint = 'Don'#39't do this until you'#39've followed instructions #1..#5 in full'
    Caption = '&Load'
    TabOrder = 1
    OnClick = Button2Click
  end
  object MyBitmap: TEdit
    Left = 88
    Top = 144
    Width = 81
    Height = 21
    TabOrder = 3
    Text = 'MyBitmap'
  end
  object Compress1: TCompress
    RegNumber = 0
    MakeDirectories = False
    ExceptionOnFileError = False
    Key = 0
    CheckSpaceBeforeExpand = False
    Left = 112
    Top = 40
  end
end
