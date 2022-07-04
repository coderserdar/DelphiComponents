object FormViewToolList: TFormViewToolList
  Left = 325
  Top = 189
  ActiveControl = List1
  BorderStyle = bsDialog
  Caption = 'User tools'
  ClientHeight = 230
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 104
    Top = 200
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 192
    Top = 200
    Width = 81
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 361
    Height = 189
    TabOrder = 0
    object List1: TListView
      Left = 8
      Top = 16
      Width = 257
      Height = 165
      Columns = <
        item
          Caption = 'Name'
          Width = 230
        end>
      ColumnClick = False
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      SmallImages = ImageList1
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = btnEditClick
      OnSelectItem = List1SelectItem
    end
    object btnAdd: TButton
      Left = 272
      Top = 16
      Width = 81
      Height = 23
      Caption = 'Add...'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnRemove: TButton
      Left = 272
      Top = 44
      Width = 81
      Height = 23
      Caption = 'Remove'
      TabOrder = 2
      OnClick = btnRemoveClick
    end
    object btnEdit: TButton
      Left = 272
      Top = 72
      Width = 81
      Height = 23
      Caption = 'Edit...'
      TabOrder = 3
      OnClick = btnEditClick
    end
    object btnUp: TButton
      Left = 272
      Top = 116
      Width = 81
      Height = 23
      Caption = 'Up'
      TabOrder = 4
      OnClick = btnUpClick
    end
    object btnDown: TButton
      Left = 272
      Top = 144
      Width = 81
      Height = 23
      Caption = 'Down'
      TabOrder = 5
      OnClick = btnDownClick
    end
  end
  object ImageList1: TImageList
    AllocBy = 10
    Left = 184
    Top = 4
  end
  object OpenDialog1: TTntOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 208
    Top = 4
  end
end
