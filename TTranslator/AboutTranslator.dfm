object dlgAboutTranslator: TdlgAboutTranslator
  Left = 406
  Top = 219
  BorderStyle = bsDialog
  Caption = 'About TTranslator'
  ClientHeight = 327
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 486
    Height = 273
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    object GroupBox: TGroupBox
      Left = 5
      Top = 5
      Width = 476
      Height = 263
      Align = alClient
      Caption = 'About Translator'
      TabOrder = 0
      object Label1: TLabel
        Left = 16
        Top = 32
        Width = 208
        Height = 13
        Caption = 'The Translator Component is Developed by '
      end
      object lblPolycon: TLabel
        Left = 224
        Top = 32
        Width = 54
        Height = 13
        Cursor = crHandPoint
        Hint = 'http://www.polycon.fi'
        Caption = 'Polycon Ab'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = OnLabelClick
      end
      object Label3: TLabel
        Left = 16
        Top = 47
        Width = 48
        Height = 13
        Caption = 'Written by'
      end
      object lblLeif: TLabel
        Left = 67
        Top = 47
        Width = 70
        Height = 13
        Cursor = crHandPoint
        Hint = 'mailto:Leif.Esselstrom@polycon.fi'
        Caption = 'Leif Esselström'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = OnLabelClick
      end
      object Label5: TLabel
        Left = 16
        Top = 74
        Width = 201
        Height = 13
        Caption = 'Please send comments and suggestions to'
      end
      object lblTranslator: TLabel
        Left = 221
        Top = 74
        Width = 99
        Height = 13
        Cursor = crHandPoint
        Hint = 'mailto:translator@polycon.fi'
        Caption = 'translator@polycon.fi'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = OnLabelClick
      end
      object Label7: TLabel
        Left = 16
        Top = 88
        Width = 192
        Height = 13
        Caption = 'Check out the latest news at our website'
      end
      object Label8: TLabel
        Left = 16
        Top = 117
        Width = 433
        Height = 26
        Caption = 
          'The Translator is released as Open Source under the GPL license,' +
          ' and may be used free of charge within any application distribut' +
          'ed under the GPL license as described in the'
        WordWrap = True
      end
      object Label9: TLabel
        Left = 16
        Top = 174
        Width = 450
        Height = 26
        AutoSize = False
        Caption = 
          'The Translator is Built upon Db Unaware, an Internal Polycon Too' +
          'lkit that Makes our Lives Easier...'
        WordWrap = True
      end
      object Label10: TLabel
        Left = 16
        Top = 211
        Width = 450
        Height = 26
        AutoSize = False
        Caption = 
          'Translate Component Captions, Hints, other String and TStrings P' +
          'roperties, and Strings Used in Code by Clicking the Strings Prop' +
          'erty of the TTranslator in the Object Inspector'
        WordWrap = True
      end
      object Label2: TLabel
        Left = 17
        Top = 143
        Width = 133
        Height = 13
        Cursor = crHandPoint
        Hint = 'http://www.polycon.fi/translator/licensing.html'
        Caption = 'TTranslator Licensing Policy'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = OnLabelClick
      end
      object lblTranslatorHome: TLabel
        Left = 212
        Top = 88
        Width = 151
        Height = 13
        Cursor = crHandPoint
        Hint = 'http://www.polycon.fi/translator'
        Caption = 'http://www.polycon.fi/translator'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = OnLabelClick
      end
    end
  end
  object btnOk: TBitBtn
    Left = 200
    Top = 288
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
end
