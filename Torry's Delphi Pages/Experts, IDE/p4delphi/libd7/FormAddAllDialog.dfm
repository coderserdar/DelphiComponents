inherited frmAddAllDialog: TfrmAddAllDialog
  Height = 367
  Caption = 'Add All Files In Project'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel [2]
    Left = 11
    Top = 195
    Width = 24
    Height = 13
    Caption = 'Files:'
  end
  inherited btnOK: TButton
    Top = 304
  end
  inherited btnCancel: TButton
    Top = 304
  end
  inherited memDescription: TMemo
    Anchors = [akLeft, akTop, akRight]
  end
  object chlbxFiles: TCheckListBox
    Left = 11
    Top = 209
    Width = 386
    Height = 89
    OnClickCheck = chlbxFilesClickCheck
    Anchors = [akLeft, akTop, akRight, akBottom]
    Flat = False
    ItemHeight = 13
    PopupMenu = popFiles
    TabOrder = 4
  end
  object popFiles: TPopupMenu
    Left = 165
    Top = 232
    object mntmSelectAll: TMenuItem
      Caption = 'Select &All'
    end
    object mntmInvertSel: TMenuItem
      Caption = '&Invert Selection'
    end
  end
end
