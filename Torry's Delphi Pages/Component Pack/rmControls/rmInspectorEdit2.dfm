object frmInspectorItemTypes: TfrmInspectorItemTypes
  Left = 192
  Top = 107
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Choose Inspector Item Type'
  ClientHeight = 83
  ClientWidth = 260
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 6
    Width = 97
    Height = 13
    Caption = 'Inspector Item Type:'
  end
  object ComboBox1: TComboBox
    Left = 5
    Top = 22
    Width = 250
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'TrmCheckBoxInspectorItem'
      'TrmComboInspectorItem'
      'TrmComplexInspectorItem'
      'TrmDateInspectorItem'
      'TrmIntegerInspectorItem'
      'TrmStringInspectorItem')
  end
  object Button1: TButton
    Left = 102
    Top = 52
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 179
    Top = 52
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
