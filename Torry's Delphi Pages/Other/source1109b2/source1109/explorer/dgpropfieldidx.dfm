object frmPropFieldIdx: TfrmPropFieldIdx
  Left = 360
  Top = 146
  BorderStyle = bsDialog
  Caption = 'Properties Field'
  ClientHeight = 172
  ClientWidth = 205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 33
    Top = 101
    Width = 64
    Height = 13
    Caption = 'Size compare'
  end
  object BitBtn1: TBitBtn
    Left = 39
    Top = 141
    Width = 65
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 105
    Top = 141
    Width = 65
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object CheckBox1: TCheckBox
    Left = 30
    Top = 16
    Width = 130
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Ascend'
    TabOrder = 2
  end
  object CheckBox2: TCheckBox
    Left = 30
    Top = 42
    Width = 130
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Null top'
    TabOrder = 3
  end
  object CheckBox3: TCheckBox
    Left = 30
    Top = 67
    Width = 130
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Case'
    TabOrder = 4
  end
  object SpinEdit1: TSpinEdit
    Left = 103
    Top = 95
    Width = 57
    Height = 22
    MaxValue = 1024
    MinValue = 0
    TabOrder = 5
    Value = 0
  end
end
