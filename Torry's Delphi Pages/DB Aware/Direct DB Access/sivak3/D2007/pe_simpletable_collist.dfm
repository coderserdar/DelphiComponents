object fpe_st_collist: Tfpe_st_collist
  Left = 188
  Top = 88
  Width = 188
  Height = 311
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  BorderWidth = 4
  Caption = '  Column list'
  Color = clBtnFace
  Constraints.MinHeight = 128
  Constraints.MinWidth = 188
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pan: TPanel
    Left = 0
    Top = 231
    Width = 172
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      172
      38)
    object upb: TSpeedButton
      Left = 4
      Top = 14
      Width = 16
      Height = 16
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
        3333333333777F33333333333307033333333333337F7F333333333333070333
        33333333337F7F33333333333307033333333333337F7F333333333333070333
        33333333337F7F33333333333307033333333333FF7F7FFFF333333000070000
        3333333777737777F333333077777770333333373F3333373333333307777703
        333333337F33337F33333333077777033333333373F333733333333330777033
        3333333337F337F3333333333077703333333333373F37333333333333070333
        33333333337F7F33333333333307033333333333337373333333333333303333
        333333333337F333333333333330333333333333333733333333}
      NumGlyphs = 2
      OnClick = UpDownClick
    end
    object dnb: TSpeedButton
      Tag = 1
      Left = 20
      Top = 14
      Width = 16
      Height = 16
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
        333333333337F33333333333333033333333333333373F333333333333070333
        33333333337F7F33333333333307033333333333337373F33333333330777033
        3333333337F337F33333333330777033333333333733373F3333333307777703
        333333337F33337F33333333077777033333333373333373F333333077777770
        33333337FFFF3FF7F33333300007000033333337777F77773333333333070333
        33333333337F7F33333333333307033333333333337F7F333333333333070333
        33333333337F7F33333333333307033333333333337F7F333333333333070333
        33333333337F7F33333333333300033333333333337773333333}
      NumGlyphs = 2
      OnClick = UpDownClick
    end
    object Button1: TButton
      Left = 44
      Top = 10
      Width = 55
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object Cancel: TButton
      Left = 108
      Top = 10
      Width = 55
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object CheckCols: TCheckListBox
    Left = 0
    Top = 0
    Width = 172
    Height = 231
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsNone
    DragMode = dmAutomatic
    ItemHeight = 13
    PopupMenu = Popup
    TabOrder = 0
    OnDragDrop = CheckColsDragDrop
    OnDragOver = CheckColsDragOver
  end
  object Popup: TPopupMenu
    Left = 24
    Top = 40
    object Selectall1: TMenuItem
      Tag = 1
      Caption = 'Select all'
      OnClick = SelectClick
    end
    object Unselectall1: TMenuItem
      Caption = 'Unselect all'
      OnClick = SelectClick
    end
  end
end
