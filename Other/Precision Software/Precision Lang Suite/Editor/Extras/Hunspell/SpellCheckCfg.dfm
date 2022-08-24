object PSForm: TPSForm
  Left = 276
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Spell check options'
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
      Caption = 'Cancel'
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
      Caption = 'General'
      object AutoClose: TCheckBox
        Left = 8
        Top = 12
        Width = 401
        Height = 17
        Caption = 'Automatically close on successfull check'
        TabOrder = 0
      end
      object GlobalCustomDict: TCheckBox
        Left = 8
        Top = 36
        Width = 401
        Height = 17
        Caption = 'Use a single global custom dictionary'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object tsDicts: TTabSheet
      BorderWidth = 6
      Caption = 'Dictionaries'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          Caption = 'Add'
          TabOrder = 0
        end
        object sbDelDict: TButton
          Left = 8
          Top = 28
          Width = 75
          Height = 25
          Caption = 'Delete'
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
            Caption = 'Language'
            Width = 212
          end
          item
            Caption = 'Code'
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
      Caption = 'Custom dictionaries'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          Caption = 'Delete'
          TabOrder = 1
        end
        object sbEditCustDict: TButton
          Left = 8
          Top = 0
          Width = 75
          Height = 25
          Caption = 'Edit'
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
            Caption = 'Language'
            Width = 212
          end
          item
            Caption = 'Code'
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
      Caption = 'Associations'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          Caption = 'Add'
          TabOrder = 0
        end
        object sbDelMap: TButton
          Left = 8
          Top = 56
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 2
        end
        object sbEditMap: TButton
          Left = 8
          Top = 28
          Width = 75
          Height = 25
          Caption = 'Edit'
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
            Caption = 'Language code'
            Width = 96
          end
          item
            Caption = 'Used dictionary'
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
        ItemHeight = 0
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
      'Do you really want to delete selected items?'
      
        'To install a new dictionary, simply copy your .AFF and .DIC file' +
        's into the following folder:'
      'Global custom dictionary'
      'Global custom terms'
      'Edit'
      'Save'
      'Cancel'
      'Delete'
      'Enter the language code of localization file:'
      'Enter the language code of associated dictionary:')
    TabOrder = 2
    Visible = False
    WantReturns = False
    WordWrap = False
  end
end
