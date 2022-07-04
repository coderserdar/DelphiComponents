object frmSyncDialog: TfrmSyncDialog
  Left = 192
  Top = 107
  ActiveControl = rbtnHead
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Sync To Revision'
  ClientHeight = 151
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    422
    151)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 52
    Top = 94
    Width = 349
    Height = 13
    Caption = 
      'Label, Changelist Number, or Date (yyyy/mm/dd or yyyy/mm/dd:hh:m' +
      'm:ss)'
  end
  object btnOK: TButton
    Left = 258
    Top = 121
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object btnCancel: TButton
    Left = 342
    Top = 121
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object rbtnHead: TRadioButton
    Left = 12
    Top = 14
    Width = 113
    Height = 17
    Caption = 'Head Revision'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = RadioButtonClick
  end
  object rbtnRevNbr: TRadioButton
    Left = 12
    Top = 43
    Width = 113
    Height = 17
    Caption = 'Revision Number:'
    TabOrder = 1
    OnClick = RadioButtonClick
  end
  object rbtnOther: TRadioButton
    Left = 12
    Top = 72
    Width = 113
    Height = 17
    Caption = 'Other:'
    TabOrder = 2
    OnClick = RadioButtonClick
  end
  object edtRevNbr: TEdit
    Left = 123
    Top = 41
    Width = 121
    Height = 21
    Color = clBtnFace
    Enabled = False
    TabOrder = 3
    OnKeyPress = edtRevNbrKeyPress
  end
  object edtOther: TEdit
    Left = 123
    Top = 70
    Width = 288
    Height = 21
    Color = clBtnFace
    Enabled = False
    TabOrder = 4
    OnKeyPress = edtOtherKeyPress
  end
  object chbxForce: TCheckBox
    Left = 4
    Top = 129
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Force Sync'
    TabOrder = 5
  end
end
