object frmBaseAddEditDlg: TfrmBaseAddEditDlg
  Left = 192
  Top = 107
  Width = 411
  Height = 255
  BorderIcons = []
  Caption = 'frmBaseAddEditDlg'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    403
    221)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 11
    Top = 16
    Width = 59
    Height = 13
    Caption = 'Change List:'
  end
  object lblDescription: TLabel
    Left = 11
    Top = 46
    Width = 56
    Height = 13
    Caption = 'Description:'
  end
  object btnOK: TButton
    Left = 242
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 324
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbxChangeLists: TComboBox
    Left = 79
    Top = 13
    Width = 318
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = 'Default'
    OnChange = cbxChangeListsChange
    Items.Strings = (
      'Default'
      'New')
  end
  object memDescription: TMemo
    Left = 11
    Top = 61
    Width = 386
    Height = 124
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
    OnChange = memDescriptionChange
  end
end
