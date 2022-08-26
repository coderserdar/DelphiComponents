object PaletteCategories: TPaletteCategories
  Left = 511
  Top = 366
  BorderStyle = bsDialog
  Caption = 'Kategorien anpassen'
  ClientHeight = 334
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    360
    334)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 256
    Height = 13
    Caption = '                                                                '
    OnMouseEnter = Label1MouseEnter
    OnMouseLeave = Label1MouseLeave
  end
  object Label2: TLabel
    Left = 8
    Top = 320
    Width = 256
    Height = 13
    Caption = '                                                                '
    OnMouseEnter = Label1MouseEnter
    OnMouseLeave = Label1MouseLeave
  end
  object BtnOk: TButton
    Left = 276
    Top = 265
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
  end
  object BtnCancel: TButton
    Left = 276
    Top = 297
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Abbruch'
    ModalResult = 2
    TabOrder = 1
  end
  object MetaList: TTreeView
    Left = 8
    Top = 8
    Width = 256
    Height = 313
    Anchors = [akLeft, akTop, akRight]
    DragMode = dmAutomatic
    Images = PBar.MainImages
    Indent = 19
    SortType = stText
    TabOrder = 2
    OnChange = MetaListChange
    OnDragDrop = MetaListDragDrop
    OnDragOver = MetaListDragOver
    OnEdited = MetaListEdited
    OnEditing = MetaListEditing
    OnEndDrag = MetaListEndDrag
    OnKeyUp = MetaListKeyUp
    OnStartDrag = MetaListStartDrag
  end
  object BtnAdd: TButton
    Left = 276
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'BtnAdd'
    TabOrder = 3
    OnClick = BtnAddClick
  end
  object BtnDel: TButton
    Left = 276
    Top = 40
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'BtnDel'
    Enabled = False
    TabOrder = 4
    OnClick = BtnDelClick
  end
end
