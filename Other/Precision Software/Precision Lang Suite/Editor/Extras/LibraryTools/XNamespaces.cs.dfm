object PSForm: TPSForm
  Left = 276
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Jmenn'#233' prostory (namespaces)'
  ClientHeight = 320
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 89
    Width = 382
    Height = 13
    Margins.Left = 0
    Margins.Top = 16
    Margins.Right = 0
    Margins.Bottom = 4
    Align = alTop
    Caption = 'Seznam jmenn'#253'ch prostor'#367' (namespaces):'
    ExplicitTop = 118
    ExplicitWidth = 204
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 382
    Height = 65
    Align = alTop
    Caption = 
      'Zde m'#367#382'ete definovat seznam zn'#225'm'#253'ch jmenn'#253'ch prostor'#367' (namespace' +
      's). Toto nastaven'#237' je d'#367'le'#382'it'#233' pro importy '#345'et'#283'zcov'#253'ch konstant ' +
      '(resourcestring) ze zdrojov'#253'ch soubor'#367' (pro Delphi 2009 a vy'#353#353#237')' +
      '.'#13#10#13#10'Aby se zm'#283'ny v nastaven'#237' mohly projevit, mus'#237'te restartovat' +
      ' PLS Editor.'
    WordWrap = True
    ExplicitWidth = 381
  end
  object mList: TMemo
    Left = 8
    Top = 106
    Width = 382
    Height = 173
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
    ExplicitTop = 136
    ExplicitWidth = 381
    ExplicitHeight = 141
  end
  object Panel1: TPanel
    Left = 8
    Top = 279
    Width = 382
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 278
    object sbOK: TButton
      Left = 227
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
    end
    object sbCancel: TButton
      Left = 307
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Storno'
      TabOrder = 1
    end
  end
end
