object frmSubmitDialog: TfrmSubmitDialog
  Left = 192
  Top = 107
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Submit Changes To Perforce'
  ClientHeight = 277
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    455
    277)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 10
    Width = 96
    Height = 13
    Caption = 'Change Description:'
  end
  object Label2: TLabel
    Left = 8
    Top = 150
    Width = 24
    Height = 13
    Caption = 'Files:'
  end
  object btnOK: TButton
    Left = 292
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 376
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object memChangeDesc: TMemo
    Left = 8
    Top = 25
    Width = 439
    Height = 110
    ScrollBars = ssBoth
    TabOrder = 2
    OnChange = EnableOKButton
  end
  object clbxFiles: TCheckListBox
    Left = 8
    Top = 165
    Width = 439
    Height = 73
    OnClickCheck = EnableOKButton
    ItemHeight = 13
    PopupMenu = popCheckList
    TabOrder = 3
  end
  object chbxReopen: TCheckBox
    Left = 8
    Top = 252
    Width = 97
    Height = 17
    Caption = 'Reopen Files'
    TabOrder = 4
  end
  object popCheckList: TPopupMenu
    Left = 173
    Top = 189
    object miCheckAll: TMenuItem
      Caption = 'Check &All'
      ShortCut = 16449
      OnClick = CheckMenuOption
    end
    object miCheckNone: TMenuItem
      Caption = 'Check &None'
      OnClick = CheckMenuOption
    end
  end
end
