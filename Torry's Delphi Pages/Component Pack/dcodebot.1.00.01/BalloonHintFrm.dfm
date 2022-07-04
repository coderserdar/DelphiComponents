object BalloonHintForm: TBalloonHintForm
  Left = 249
  Top = 177
  BorderStyle = bsDialog
  Caption = 'Balloon Hint Editor'
  ClientHeight = 373
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Background: TBackground
    Left = 8
    Top = 8
    Width = 273
    Height = 169
  end
  object GroupBox: TGroupBox
    Left = 288
    Top = 8
    Width = 233
    Height = 201
    Caption = 'Balloon Hint: '
    TabOrder = 0
    object CaptionLabel: TLabel
      Left = 16
      Top = 24
      Width = 39
      Height = 13
      Caption = 'C&aption:'
      FocusControl = CaptionEdit
    end
    object TextLabel: TLabel
      Left = 16
      Top = 72
      Width = 21
      Height = 13
      Caption = '&Text'
      FocusControl = TextMemo
    end
    object KindLabel: TLabel
      Left = 16
      Top = 152
      Width = 24
      Height = 13
      Caption = '&Kind:'
      FocusControl = KindBox
    end
    object PositionLabel: TLabel
      Left = 120
      Top = 152
      Width = 40
      Height = 13
      Caption = '&Position:'
      FocusControl = PositionBox
    end
    object CaptionEdit: TEdit
      Left = 16
      Top = 40
      Width = 201
      Height = 21
      TabOrder = 0
      OnChange = ParamsChange
    end
    object TextMemo: TMemo
      Left = 16
      Top = 88
      Width = 201
      Height = 55
      TabOrder = 1
      OnChange = ParamsChange
    end
    object KindBox: TComboBox
      Left = 16
      Top = 168
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = ParamsChange
      Items.Strings = (
        'Information'
        'Confirmation'
        'Warning'
        'Error'
        'Custom')
    end
    object PositionBox: TComboBox
      Left = 120
      Top = 168
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = ParamsChange
      Items.Strings = (
        'Top Left'
        'Top Right'
        'Bottom Left'
        'Bottom Right')
    end
  end
  object OKButtopn: TButton
    Left = 366
    Top = 216
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 446
    Top = 216
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ParamsEdit: TEdit
    Left = 8
    Top = 188
    Width = 273
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
end
