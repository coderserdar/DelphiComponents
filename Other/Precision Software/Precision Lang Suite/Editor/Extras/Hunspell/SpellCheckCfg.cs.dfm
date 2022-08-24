object PSForm: TPSForm
  Left = 276
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Nastaven'#237' kontroly pravopisu'
  ClientHeight = 380
  ClientWidth = 439
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
  object Panel1: TPanel
    Left = 8
    Top = 339
    Width = 423
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object sbOK: TButton
      Left = 268
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
    end
    object sbCancel: TButton
      Left = 348
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Storno'
      TabOrder = 1
    end
  end
  object pcMain: TPageControl
    Left = 8
    Top = 8
    Width = 423
    Height = 331
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = 'Z'#225'kladn'#237
      object AutoClose: TCheckBox
        Left = 8
        Top = 12
        Width = 401
        Height = 17
        Caption = 'Zav'#345#237't dialog po dokon'#269'en'#237' kontroly'
        TabOrder = 0
      end
      object GlobalCustomDict: TCheckBox
        Left = 8
        Top = 36
        Width = 401
        Height = 17
        Caption = 'Pou'#382#237'vat jeden glob'#225'ln'#237' u'#382'ivatelsk'#253' slovn'#237'k'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object tsDicts: TTabSheet
      BorderWidth = 6
      Caption = 'Slovn'#237'ky'
      object Panel2: TPanel
        Left = 319
        Top = 0
        Width = 84
        Height = 291
        Align = alRight
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        object sbAddDict: TButton
          Left = 8
          Top = 0
          Width = 75
          Height = 25
          Caption = 'P'#345'idat'
          TabOrder = 0
        end
        object sbDelDict: TButton
          Left = 8
          Top = 28
          Width = 75
          Height = 25
          Caption = 'Smazat'
          TabOrder = 1
        end
      end
      object lvDicts: TListView
        Left = 0
        Top = 0
        Width = 319
        Height = 291
        Align = alClient
        Columns = <
          item
            Caption = 'Jazyk'
            Width = 212
          end
          item
            Caption = 'K'#243'd'
            Width = 86
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsCustom: TTabSheet
      BorderWidth = 6
      Caption = 'U'#382'ivatelsk'#233' slovn'#237'ky'
      object Panel3: TPanel
        Left = 319
        Top = 0
        Width = 84
        Height = 194
        Align = alRight
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        object sbDelCustDict: TButton
          Left = 8
          Top = 28
          Width = 75
          Height = 25
          Caption = 'Smazat'
          TabOrder = 1
        end
        object sbEditCustDict: TButton
          Left = 8
          Top = 0
          Width = 75
          Height = 25
          Caption = 'Upravit'
          TabOrder = 0
        end
      end
      object lvCustDicts: TListView
        Left = 0
        Top = 0
        Width = 319
        Height = 194
        Align = alClient
        Columns = <
          item
            Caption = 'Jazyk'
            Width = 212
          end
          item
            Caption = 'K'#243'd'
            Width = 86
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object mEditor: TMemo
        AlignWithMargins = True
        Left = 0
        Top = 202
        Width = 319
        Height = 89
        Margins.Left = 0
        Margins.Top = 8
        Margins.Right = 84
        Margins.Bottom = 0
        Align = alBottom
        ScrollBars = ssVertical
        TabOrder = 2
        Visible = False
        WordWrap = False
      end
    end
    object tsMapping: TTabSheet
      BorderWidth = 6
      Caption = 'P'#345'idru'#382'en'#237
      object Panel4: TPanel
        Left = 319
        Top = 0
        Width = 84
        Height = 291
        Align = alRight
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object sbAddMap: TButton
          Left = 8
          Top = 0
          Width = 75
          Height = 25
          Caption = 'P'#345'idat'
          TabOrder = 0
        end
        object sbDelMap: TButton
          Left = 8
          Top = 56
          Width = 75
          Height = 25
          Caption = 'Smazat'
          TabOrder = 2
        end
        object sbEditMap: TButton
          Left = 8
          Top = 28
          Width = 75
          Height = 25
          Caption = 'Upravit'
          TabOrder = 1
        end
      end
      object lvMap: TListView
        Left = 0
        Top = 0
        Width = 319
        Height = 291
        Align = alClient
        Columns = <
          item
            Caption = 'K'#243'd jazyka'
            Width = 96
          end
          item
            Caption = 'Slovn'#237'k'
            Width = 96
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
      end
      object cbTmp: TComboBox
        Left = 12
        Top = 232
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 2
        Text = 'cbTmp'
        Visible = False
      end
    end
  end
  object mConsts: TMemo
    Left = 28
    Top = 168
    Width = 301
    Height = 89
    Lines.Strings = (
      'Opravdu chcete odstranit vybran'#233' polo'#382'ky?'
      
        'Pro instalaci nov'#233'ho slovn'#237'ku zkop'#237'rujte Va'#353'e .AFF a .DIC soubor' +
        'y do n'#225'sleduj'#237'c'#237'ho adres'#225#345'e:'
      'Glob'#225'ln'#237' u'#382'ivatelsk'#253' slovn'#237'k'
      'Glob'#225'ln'#237' slovn'#237'k v'#253'raz'#367
      'Upravit'
      'Ulo'#382'it'
      'Storno'
      'Smazat'
      'Zadejte k'#243'd jazyka lokaliza'#269'n'#237'ho souboru:'
      'Zadejte k'#243'd jazyka p'#345'idru'#382'en'#233'ho slovn'#237'ku:')
    TabOrder = 2
    Visible = False
    WantReturns = False
    WordWrap = False
  end
end
