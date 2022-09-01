object GSCQBTable: TGSCQBTable
  Left = 269
  Top = 132
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = 'GSCQBTable'
  ClientHeight = 163
  ClientWidth = 127
  Color = clBtnFace
  Constraints.MinHeight = 24
  Constraints.MinWidth = 50
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = TableMenu
  ShowHint = True
  OnMouseDown = CaptionPanelMouseDown
  OnResize = FormResize
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object UnLinkBtn: TSpeedButton
    Left = 16
    Top = 2
    Width = 16
    Height = 14
    Hint = 'Unlink table'
    Glyph.Data = {
      AE000000424DAE0000000000000076000000280000000C000000070000000100
      04000000000038000000E30E0000E30E0000100000001000000000000000CED6
      D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00111111111111
      0000111111111111000011111111111100001100000000110000110000000011
      000011111111111100001111111111110000}
    Layout = blGlyphTop
    OnClick = UnLinkBtnClick
  end
  object CloseBtn: TSpeedButton
    Left = 32
    Top = 2
    Width = 16
    Height = 14
    Hint = 'Close'
    Glyph.Data = {
      AE000000424DAE0000000000000076000000280000000C000000070000000100
      04000000000038000000D30E0000D30E0000100000001000000000000000CED6
      D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00110011110011
      0000111001100111000011110000111100001111100111110000111100001111
      000011100110011100001100111100110000}
    Layout = blGlyphTop
    OnClick = CloseBtnClick
  end
  object FieldList: TCheckListBox
    Left = 0
    Top = 18
    Width = 127
    Height = 145
    OnClickCheck = FieldListClickCheck
    Align = alBottom
    ItemHeight = 13
    PopupMenu = TableMenu
    TabOrder = 0
    OnDragDrop = FieldListDragDrop
    OnDragOver = FieldListDragOver
    OnMouseDown = FieldListMouseDown
  end
  object TableMenu: TPopupMenu
    Left = 16
    Top = 24
    OnPopup = TableMenuPopup
    object miSelectAll: TMenuItem
      Caption = 'Select all'
      OnClick = SelectAllClick
    end
    object miSelectNone: TMenuItem
      Caption = 'Select none'
      OnClick = UnSelectAllClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miSetTableAlias: TMenuItem
      Caption = 'Change table alias'
      OnClick = SetTableAliasClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miUnLink: TMenuItem
      Caption = 'Remove links'
      OnClick = UnLinkBtnClick
    end
    object miClose: TMenuItem
      Caption = 'Close'
      OnClick = CloseBtnClick
    end
  end
end
