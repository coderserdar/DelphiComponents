object psc_frm_setup_fields: Tpsc_frm_setup_fields
  Left = 424
  Top = 160
  BorderStyle = bsDialog
  Caption = 'Select Fields'
  ClientHeight = 240
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 6
    Width = 30
    Height = 13
    Caption = 'Fields:'
  end
  object PSCListBox1: TPSCListBox
    Left = 8
    Top = 24
    Width = 308
    Height = 175
    BorderStyle = bsNone
    BevelInner = bvLowered
    BevelKind = bkFlat
    NewItemChecked = False
    CheckBoxes = True
    TabOrder = 0
  end
  object Panel_Buttons: TPanel
    Left = 0
    Top = 199
    Width = 324
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button_Ok: TButton
      Left = 158
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button_Cancel: TButton
      Left = 241
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
