object fmAbout: TfmAbout
  Left = 387
  Top = 175
  ActiveControl = btOk
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 265
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbTMS: TLabel
    Left = 8
    Top = 200
    Width = 193
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    Caption = 'www.tmssoftware.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lbTMSClick
  end
  object Label1: TLabel
    Left = 8
    Top = 159
    Width = 193
    Height = 37
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'More information about this library and more Delphi / C++ Builde' +
      'r components:'
    WordWrap = True
  end
  object lbDevgems: TLabel
    Left = 216
    Top = 200
    Width = 193
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    Caption = 'www.tmssoftware.com/site/tmsdm.asp'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lbDevgemsClick
  end
  object Label3: TLabel
    Left = 216
    Top = 159
    Width = 193
    Height = 37
    Alignment = taCenter
    AutoSize = False
    Caption = 'More information about'#13#10'TMS Data Modeler:'
    WordWrap = True
  end
  object Bevel1: TBevel
    Left = 8
    Top = 224
    Width = 401
    Height = 2
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 401
    Height = 137
    Lines.Strings = (
      
        'This demo shows how to use TMS Data Modeler Library classes to o' +
        'pen a '
      
        'TMS Data Modeler file (.dgp file) and explore its content showin' +
        'g the database '
      'structure (tables, fields, stored procedures, etc.)'
      ''
      
        'TMS Data Modeler is a database modeling tool software from TMS w' +
        'hich '
      'was used to create the .dgp files in this demo.')
    ReadOnly = True
    TabOrder = 0
  end
  object btOk: TButton
    Left = 334
    Top = 233
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
