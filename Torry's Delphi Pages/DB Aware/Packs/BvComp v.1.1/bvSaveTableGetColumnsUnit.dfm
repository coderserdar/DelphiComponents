object SaveTableGetColumnsForm: TSaveTableGetColumnsForm
  Left = 75
  Top = 152
  Width = 420
  Height = 247
  BorderIcons = [biSystemMenu]
  Caption = 'SaveTableGetColumnsForm'
  Color = clBtnFace
  Constraints.MaxWidth = 420
  Constraints.MinWidth = 420
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000FFFF00000000000000000000
    000000087777F00000000000000000000000008777777F000000000000000000
    00000087777777F0000000000000000000000877777777F00000000000000000
    00000877777777F000000000000000000000087777777F000000000000000000
    000008777777F000000000000000000000000087777F00000000000000000000
    0000008777F0000000000000000000000000008777F000000000000000000000
    0000008777F0000000000000000000000000008777F000000000000000000000
    0000087777F00FFF000000000000000000000877777FF777F000000000000000
    00000877777777777F0000000000000000000877777777777F00000000000000
    00000877777777777F0000000000000000000877777777777F00000000000000
    00000877777777777F0000000000000000000087777777777F00000000000000
    0000008777887777F0000000000000000008F008888088880000000000000000
    00087F000000000000000000000000000008F08FF8FF0FF088F0000000000000
    00000087F87F877F877F00000000000000000088087F877F8777F00000000000
    00000000008087808777F0000000000000000000000008008777800000000000
    000000000000000008880000000000000000000000000000000000000000FFFF
    FFFFFFF0FFFFFFE07FFFFFC03FFFFFC01FFFFF801FFFFF801FFFFF803FFFFF80
    7FFFFFC0FFFFFFC1FFFFFFC1FFFFFFC1FFFFFFC1FFFFFF818FFFFF8007FFFF80
    03FFFF8003FFFF8003FFFF8003FFFF8003FFFFC003FFFFC007FFFE610FFFFE3F
    FFFFFE4091FFFFC000FFFFC8007FFFFD107FFFFFB07FFFFFF8FFFFFFFFFF}
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object FormGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 392
    Height = 147
    Align = alClient
    ColCount = 2
    DefaultRowHeight = 16
    RowCount = 6
    Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goRowMoving, goEditing]
    PopupMenu = PopupMenu
    ScrollBars = ssVertical
    TabOrder = 0
    OnDblClick = FormGridDblClick
    OnDrawCell = FormGridDrawCell
    OnMouseDown = FormGridMouseDown
    OnMouseMove = FormGridMouseMove
    OnMouseUp = FormGridMouseUp
    OnSelectCell = FormGridSelectCell
    ColWidths = (
      16
      353)
    RowHeights = (
      16
      16
      16
      16
      16
      16)
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 147
    Width = 412
    Height = 53
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object SpeedButtonCancel: TSpeedButton
      Left = 70
      Top = 1
      Width = 102
      Height = 32
      Cursor = crHandPoint
      Caption = 'Cancel'
      Flat = True
      Glyph.Data = {
        C6000000424DC60000000000000076000000280000000A0000000A0000000100
        0400000000005000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFF00
        0000FF99FFF99F000000FF999F999F000000FFF99999FF000000FFFF999FFF00
        0000FFF99999FF000000FF999F999F000000F999FFF999000000F99FFFFF9900
        0000FFFFFFFFFF000000}
      ParentShowHint = False
      ShowHint = True
      Spacing = 6
      OnClick = SpeedButtonCancelClick
    end
    object SpeedButton2: TSpeedButton
      Left = 0
      Top = 1
      Width = 65
      Height = 32
      Cursor = crHandPoint
      Caption = 'Ok'
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00CCCCCCC00000
        0000CCCCCCC099999990CCCCCC0099999990CCCCC0B090909090CCCC0BB09999
        9990CCC0BBB000000000CC0BBBB007777700C0BBBBB0077777000BBBBBB00000
        00000BBBBB0FFFFF0CCC0BB7B0FFFFF0CCCC0BBB0FFFFF040CCC0BB0FFFFF00B
        B0CC0B0C0FFF0CC000CC00CCC0F0CCCCCCCC0CCCCC0CCCCCCCCC}
      Margin = 8
      ParentShowHint = False
      ShowHint = True
      Spacing = 10
      OnClick = BitBtnOkClick
    end
    object SpeedButtonRestore: TSpeedButton
      Left = 174
      Top = 1
      Width = 35
      Height = 32
      Cursor = crHandPoint
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000C40E0000C40E00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666166666666
        666666917777777777766991111110000076999999991880C076999999990880
        C076699100000880C076609C00000000C07660CCCCCCCCCCC07660CC0000000C
        C07660C088888880C07660C088888880C07660C088888880C07660C088888880
        007660C088888880807660000000000000666666666666666666}
      ParentShowHint = False
      ShowHint = True
      Spacing = 6
      OnClick = SpeedButtonRestoreClick
    end
    object Panel2: TPanel
      Left = 412
      Top = 0
      Width = 0
      Height = 53
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object LabVersion: TLabel
        Left = 2
        Top = 4
        Width = 53
        Height = 13
        Caption = 'LabVersion'
      end
      object EditVersion: TEdit
        Left = 96
        Top = 0
        Width = 41
        Height = 21
        TabOrder = 0
        Text = '1'
      end
      object UpDown1: TUpDown
        Left = 137
        Top = 0
        Width = 13
        Height = 21
        Associate = EditVersion
        Min = 1
        Position = 1
        TabOrder = 1
        Wrap = False
      end
    end
    object CheckShowResult: TCheckBox
      Left = 8
      Top = 34
      Width = 209
      Height = 17
      Cursor = crHandPoint
      Caption = 'CheckShowResult'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsItalic]
      ParentFont = False
      State = cbChecked
      TabOrder = 1
    end
    object CheckUseBDEDriver: TCheckBox
      Left = 224
      Top = 34
      Width = 169
      Height = 17
      Cursor = crHandPoint
      Caption = 'CheckUseBDEDriver'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 2
      Visible = False
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 200
    Width = 412
    Height = 19
    Panels = <
      item
        Alignment = taRightJustify
        Bevel = pbNone
        Width = 0
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel3: TPanel
    Left = 392
    Top = 0
    Width = 20
    Height = 147
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 20
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object SpeedButtonUp: TSpeedButton
        Left = 2
        Top = 2
        Width = 18
        Height = 25
        Cursor = crHandPoint
        Glyph.Data = {
          F6000000424DF60000000000000076000000280000000C000000100000000100
          04000000000080000000CE0E0000D80E00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333330003333
          0000333330903333000033333090333300003333309033330000333330903333
          0000333330903333000033000090000300003309999999030000333099999033
          0000333099999033000033330999033300003333099903330000333330903333
          0000333330903333000033333303333300003333330333330000}
        OnClick = SpeedButtonUpClick
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 121
      Width = 20
      Height = 26
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object SpeedButtonDown: TSpeedButton
        Left = 2
        Top = 1
        Width = 18
        Height = 25
        Cursor = crHandPoint
        Glyph.Data = {
          F6000000424DF60000000000000076000000280000000B000000100000000100
          04000000000080000000CE0E0000D80E00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333330333330
          0000333330333330000033330903333000003333090333300000333099903330
          0000333099903330000033099999033000003309999903300000309999999030
          0000300009000030000033330903333000003333090333300000333309033330
          0000333309033330000033330903333000003333000333300000}
        OnClick = SpeedButtonDownClick
      end
    end
  end
  object Table: TTable
    Left = 52
    Top = 20
  end
  object PopupMenu: TPopupMenu
    Left = 192
    Top = 56
    object NPlus: TMenuItem
      Caption = 'NPlus'
      OnClick = NPlusClick
    end
    object NMinus: TMenuItem
      Caption = 'NMinus'
      OnClick = NPlusClick
    end
  end
end
