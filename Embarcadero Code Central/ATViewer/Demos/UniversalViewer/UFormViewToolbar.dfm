object FormViewToolbar: TFormViewToolbar
  Left = 197
  Top = 219
  ActiveControl = ListAvail
  BorderStyle = bsDialog
  Caption = 'Customize toolbar'
  ClientHeight = 298
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object labAvail: TLabel
    Left = 8
    Top = 8
    Width = 87
    Height = 13
    Caption = 'Available buttons:'
    FocusControl = ListAvail
  end
  object labCurrent: TLabel
    Left = 294
    Top = 8
    Width = 81
    Height = 13
    Caption = 'Current buttons:'
    FocusControl = ListCurrent
  end
  object ListAvail: TListView
    Left = 8
    Top = 24
    Width = 193
    Height = 265
    Columns = <
      item
        Caption = 'Command'
        Width = 168
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = btnAddClick
    OnSelectItem = ListAvailSelectItem
  end
  object btnOk: TButton
    Left = 498
    Top = 24
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 498
    Top = 54
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object ListCurrent: TListView
    Left = 294
    Top = 24
    Width = 193
    Height = 265
    Columns = <
      item
        Caption = 'Command'
        Width = 168
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 3
    ViewStyle = vsReport
    OnDblClick = btnRemoveClick
    OnSelectItem = ListCurrentSelectItem
  end
  object btnAdd: TButton
    Left = 210
    Top = 24
    Width = 75
    Height = 23
    Caption = 'Add ->'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 210
    Top = 54
    Width = 75
    Height = 23
    Caption = '<- Remove'
    TabOrder = 2
    OnClick = btnRemoveClick
  end
  object btnReset: TButton
    Left = 498
    Top = 84
    Width = 75
    Height = 23
    Caption = 'Reset'
    TabOrder = 6
    OnClick = btnResetClick
  end
  object btnUp: TButton
    Left = 498
    Top = 128
    Width = 75
    Height = 23
    Caption = 'Up'
    TabOrder = 7
    OnClick = btnUpClick
  end
  object btnDown: TButton
    Left = 498
    Top = 158
    Width = 75
    Height = 23
    Caption = 'Down'
    TabOrder = 8
    OnClick = btnDownClick
  end
end
