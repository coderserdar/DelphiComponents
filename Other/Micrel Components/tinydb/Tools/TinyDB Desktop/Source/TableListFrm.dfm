object TableListForm: TTableListForm
  Left = 311
  Top = 193
  BorderStyle = bsDialog
  Caption = 'Select Table'
  ClientHeight = 234
  ClientWidth = 327
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object TableNameEdit: TEdit
    Left = 8
    Top = 9
    Width = 224
    Height = 22
    TabOrder = 0
    OnChange = TableNameEditChange
    OnKeyDown = TableNameEditKeyDown
  end
  object ListView: TListView
    Left = 8
    Top = 37
    Width = 224
    Height = 189
    Columns = <
      item
        Caption = 'No.'
      end
      item
        Caption = 'Table items'
        Width = 170
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    SmallImages = DBInfoForm.ImageList
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = ListViewChange
    OnDblClick = OkButtonClick
  end
  object OkButton: TButton
    Left = 243
    Top = 7
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 2
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 243
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
