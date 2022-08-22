inherited DBTableForm: TDBTableForm
  Width = 392
  Height = 296
  Caption = 'Table'
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000000000000000000888888888888880087FF
    FFFFFFFFF80087F8888F8888F80087FFFFFFFFFFF80087F8888F8888F80087FF
    FFFFFFFFF80087F8888F8888F80087FFFFFFFFFFF8008788888888888800874C
    4C4C4F0F0800877777777777780088888888888888800000000000000000FFFF
    0000FFFF00000001000000010000000100000001000000010000000100000001
    0000000100000001000000010000000100000001000000010000FFFF0000}
  OldCreateOrder = True
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 384
    Height = 24
    BorderWidth = 1
    Caption = 'ToolBar'
    TabOrder = 0
    Wrapable = False
    object IndexPanel: TPanel
      Left = 0
      Top = 2
      Width = 132
      Height = 22
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object IndexLabel: TLabel
        Left = 1
        Top = 4
        Width = 28
        Height = 13
        Caption = 'Index'
      end
      object IndexComboBox: TComboBox
        Left = 28
        Top = 0
        Width = 102
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnClick = IndexComboBoxClick
      end
    end
    object ToolButton1: TToolButton
      Left = 132
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object DBNavigator: TDBNavigator
      Left = 140
      Top = 2
      Width = 210
      Height = 22
      DataSource = DataSource
      Flat = True
      Hints.Strings = (
        'First record'
        'Prior record'
        'Next record'
        'Last record'
        'Insert record'
        'Delete record'
        'Edit record'
        'Post edit'
        'Cancel edit'
        'Refresh data')
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object DataSource: TDataSource
    DataSet = TinyTable
    Left = 24
    Top = 72
  end
  object TinyTable: TTinyTable
    Left = 60
    Top = 72
  end
end
