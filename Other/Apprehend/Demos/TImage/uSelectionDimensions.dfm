object SelectionDimensionsDlg: TSelectionDimensionsDlg
  Left = 312
  Top = 234
  Caption = 'Selection Dimensions'
  ClientHeight = 146
  ClientWidth = 244
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 137
    Height = 128
    Caption = 'Selection Dimensions'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 25
      Width = 32
      Height = 13
      Caption = 'Width:'
    end
    object Label2: TLabel
      Left = 11
      Top = 76
      Width = 35
      Height = 13
      Caption = 'Height:'
    end
    object edWidth: TEdit
      Left = 8
      Top = 43
      Width = 56
      Height = 21
      PopupMenu = PopupMenu1
      TabOrder = 0
      Text = '100'
    end
    object UpDown1: TUpDown
      Left = 64
      Top = 43
      Width = 16
      Height = 21
      Associate = edWidth
      Min = 1
      Max = 10000
      Increment = 5
      PopupMenu = PopupMenu1
      Position = 100
      TabOrder = 1
      Thousands = False
    end
    object UpDown2: TUpDown
      Left = 67
      Top = 94
      Width = 16
      Height = 21
      Associate = EdHeight
      Min = 1
      Max = 10000
      Increment = 5
      PopupMenu = PopupMenu1
      Position = 100
      TabOrder = 2
      Thousands = False
    end
    object EdHeight: TEdit
      Left = 11
      Top = 94
      Width = 56
      Height = 21
      PopupMenu = PopupMenu1
      TabOrder = 3
      Text = '100'
    end
  end
  object OKBtn: TButton
    Left = 156
    Top = 13
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 156
    Top = 45
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PopupMenu1: TPopupMenu
    Left = 156
    Top = 75
    object Increment1: TMenuItem
      Caption = 'Increment'
      object N11: TMenuItem
        Caption = '1'
        GroupIndex = 1
        RadioItem = True
        OnClick = N11Click
      end
      object N51: TMenuItem
        Caption = '5'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = N51Click
      end
      object N101: TMenuItem
        Caption = '10'
        GroupIndex = 1
        RadioItem = True
        OnClick = N101Click
      end
      object N201: TMenuItem
        Caption = '20'
        GroupIndex = 1
        RadioItem = True
        OnClick = N201Click
      end
      object N401: TMenuItem
        Caption = '40'
        GroupIndex = 1
        RadioItem = True
        OnClick = N401Click
      end
      object N501: TMenuItem
        Caption = '50'
        GroupIndex = 1
        RadioItem = True
        OnClick = N501Click
      end
      object N1001: TMenuItem
        Caption = '100'
        GroupIndex = 1
        RadioItem = True
        OnClick = N1001Click
      end
    end
  end
end
