object OptionForm: TOptionForm
  Left = 255
  Top = 184
  BorderStyle = bsDialog
  Caption = 'Option'
  ClientHeight = 192
  ClientWidth = 250
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object OpenTableModeRadioGroup: TRadioGroup
    Left = 8
    Top = 7
    Width = 233
    Height = 50
    Caption = 'Open table mode'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Grid'
      'Card')
    TabOrder = 2
  end
  object MiscGroupBox: TGroupBox
    Left = 8
    Top = 64
    Width = 233
    Height = 85
    Caption = 'Misc'
    TabOrder = 3
    object RemSizeCheckBox: TCheckBox
      Left = 16
      Top = 24
      Width = 199
      Height = 17
      Caption = 'Remember window position+size'
      TabOrder = 0
    end
    object AddToMenuCheckBox: TCheckBox
      Left = 16
      Top = 52
      Width = 205
      Height = 17
      Caption = 'Add to content menu'
      TabOrder = 1
    end
  end
  object OkButton: TButton
    Left = 75
    Top = 158
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 166
    Top = 158
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = CancelButtonClick
  end
end
