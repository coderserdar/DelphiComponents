inherited frmAddEditDialog: TfrmAddEditDialog
  Height = 265
  Caption = 'Perforce Add/Edit Dialog'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblDescription: TLabel
    Top = 41
  end
  object lblWarning: TLabel [2]
    Left = 180
    Top = 177
    Width = 192
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'You don'#39't have head revisions of all files.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  inherited btnOK: TButton
    Top = 202
    TabOrder = 4
  end
  inherited btnCancel: TButton
    Top = 202
    TabOrder = 5
  end
  inherited cbxChangeLists: TComboBox
    TabOrder = 0
  end
  inherited memDescription: TMemo
    Top = 56
    Height = 113
    TabOrder = 1
  end
  object chbxAutoLock: TCheckBox
    Left = 11
    Top = 194
    Width = 80
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Lock Files'
    TabOrder = 3
  end
  object chbxSyncFirst: TCheckBox
    Left = 11
    Top = 175
    Width = 158
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Sync to Head Revision First'
    TabOrder = 2
  end
end
