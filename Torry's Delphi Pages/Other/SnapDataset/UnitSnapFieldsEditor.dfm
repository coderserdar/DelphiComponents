object FormFieldEditor: TFormFieldEditor
  Left = 301
  Top = 276
  Width = 453
  Height = 251
  BorderStyle = bsSizeToolWin
  Caption = 'Fields editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TListBox
    Left = 0
    Top = 0
    Width = 357
    Height = 223
    Align = alClient
    ItemHeight = 13
    MultiSelect = True
    PopupMenu = PopupMenu
    TabOrder = 0
    OnClick = ListBoxClick
  end
  object Panel1: TPanel
    Left = 357
    Top = 0
    Width = 88
    Height = 223
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnAdd: TButton
      Left = 7
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Add ...'
      TabOrder = 0
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 7
      Top = 40
      Width = 75
      Height = 25
      Caption = '&Delete'
      TabOrder = 1
      OnClick = btnDeleteClick
    end
    object btnMoveUp: TButton
      Left = 7
      Top = 104
      Width = 75
      Height = 25
      Caption = 'Move &Up'
      TabOrder = 2
      OnClick = btnMoveUpClick
    end
    object btnMoveDown: TButton
      Left = 7
      Top = 136
      Width = 75
      Height = 25
      Caption = 'Move Dow&n'
      TabOrder = 3
      OnClick = btnMoveDownClick
    end
    object btnImport: TButton
      Left = 7
      Top = 72
      Width = 75
      Height = 25
      Caption = '&Import ...'
      TabOrder = 4
      OnClick = btnImportClick
    end
  end
  object PopupMenu: TPopupMenu
    Left = 112
    Top = 64
    object mnuAdd: TMenuItem
      Caption = 'Add'
      Hint = 'Add a new field'
      ShortCut = 45
      OnClick = btnAddClick
    end
    object mnuDelete: TMenuItem
      Caption = 'Delete'
      Hint = 'Delete selected field'
      ShortCut = 46
      OnClick = btnDeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuImport: TMenuItem
      Caption = 'Import ...'
      OnClick = btnImportClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mnuMoveUp: TMenuItem
      Caption = 'Move Up'
      OnClick = btnMoveUpClick
    end
    object mnuMoveDown: TMenuItem
      Caption = 'Move Down'
      OnClick = btnMoveDownClick
    end
  end
end
