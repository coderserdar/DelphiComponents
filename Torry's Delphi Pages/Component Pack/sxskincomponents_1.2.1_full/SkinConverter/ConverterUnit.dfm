object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'INI2SXS Convert'
  ClientHeight = 107
  ClientWidth = 201
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
    Top = 56
    Width = 37
    Height = 13
    Caption = 'INI-file:'
  end
  object Label2: TLabel
    Left = 8
    Top = 72
    Width = 40
    Height = 13
    Caption = 'SXS-file:'
  end
  object Label3: TLabel
    Left = 64
    Top = 40
    Width = 64
    Height = 13
    Caption = 'Loading time:'
  end
  object Label4: TLabel
    Left = 64
    Top = 56
    Width = 44
    Height = 13
    Caption = 'Unknown'
  end
  object Label5: TLabel
    Left = 64
    Top = 72
    Width = 44
    Height = 13
    Caption = 'Unknown'
  end
  object Label6: TLabel
    Left = 144
    Top = 40
    Width = 41
    Height = 13
    Caption = 'File size:'
  end
  object Label7: TLabel
    Left = 144
    Top = 56
    Width = 44
    Height = 13
    Caption = 'Unknown'
  end
  object Label8: TLabel
    Left = 144
    Top = 72
    Width = 44
    Height = 13
    Caption = 'Unknown'
  end
  object Label9: TLabel
    Left = 64
    Top = 88
    Width = 63
    Height = 13
    Caption = 'Compressed:'
  end
  object Label10: TLabel
    Left = 144
    Top = 88
    Width = 44
    Height = 13
    Caption = 'Unknown'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 185
    Height = 25
    Caption = 'Select INI-file to convert...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'INI-files|*.ini'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 328
    Top = 8
  end
end
