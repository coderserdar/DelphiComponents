object RxResourceEditor: TRxResourceEditor
  Left = 198
  Top = 109
  BorderStyle = bsSizeToolWin
  Caption = 'Project Resources'
  ClientHeight = 303
  ClientWidth = 216
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PopupMenu = PopupMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 284
    Width = 216
    Height = 19
    Panels = <
      item
        Style = psOwnerDraw
        Width = 50
      end>
    OnDrawPanel = StatusBarDrawPanel
    ExplicitTop = 283
  end
  object ResTree: TTreeView
    Left = 0
    Top = 0
    Width = 216
    Height = 284
    Align = alClient
    Images = TreeImages
    Indent = 19
    ShowRoot = False
    TabOrder = 0
    OnChange = ResTreeChange
    OnCollapsed = ResTreeCollapsed
    OnDblClick = ResTreeDblClick
    OnEdited = ResTreeEdited
    OnEditing = ResTreeEditing
    OnExpanded = ResTreeExpanded
    OnKeyPress = ResTreeKeyPress
    ExplicitHeight = 283
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 8
    Top = 8
    object NewItem: TMenuItem
      Tag = 10
      Caption = '&New'
      object NewBitmapItem: TMenuItem
        Tag = 5
        Caption = '&Bitmap'
        OnClick = NewBitmapItemClick
      end
      object NewIconItem: TMenuItem
        Tag = 3
        Caption = '&Icon'
        OnClick = NewIconItemClick
      end
      object NewCursorItem: TMenuItem
        Tag = 4
        Caption = '&Cursor'
        OnClick = NewCursorItemClick
      end
      object NewUserDataItem: TMenuItem
        Tag = 2
        Caption = '&User Data'
        OnClick = NewUserDataItemClick
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object EditItem: TMenuItem
      Tag = 1
      Caption = '&Modify'
      OnClick = EditItemClick
    end
    object RenameItem: TMenuItem
      Tag = 14
      Caption = '&Rename'
      OnClick = RenameItemClick
    end
    object DeleteItem: TMenuItem
      Tag = 11
      Caption = '&Delete'
      OnClick = DeleteItemClick
    end
    object SaveItem: TMenuItem
      Tag = 12
      Caption = '&Save As...'
      OnClick = SaveItemClick
    end
    object PreviewItem: TMenuItem
      Tag = 13
      Caption = '&Preview'
      Visible = False
      OnClick = PreviewItemClick
    end
  end
  object TreeImages: TImageList
    AllocBy = 15
    Left = 36
    Top = 8
  end
  object OpenDlg: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist]
    Title = 'Load'
    Left = 64
    Top = 8
  end
  object SaveDlg: TSaveDialog
    Options = [ofHideReadOnly]
    Title = 'Save resource'
    Left = 92
    Top = 8
  end
  object Placement: TFormStorage
    UseRegistry = True
    StoredValues = <>
    Left = 120
    Top = 8
  end
end
