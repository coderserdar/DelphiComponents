object InstantADOConnectionDefEditForm: TInstantADOConnectionDefEditForm
  Left = 516
  Top = 282
  BorderStyle = bsDialog
  Caption = 'ADO Connection'
  ClientHeight = 242
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BottomBevel: TBevel
    Left = 0
    Top = 205
    Width = 362
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 362
    Height = 205
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object StreamFormatLabel: TLabel
      Left = 32
      Top = 152
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object DataLinkRadioButton: TRadioButton
      Left = 16
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Use Data &Link File'
      TabOrder = 0
      OnClick = DataChanged
    end
    object DataLinkEdit: TEdit
      Left = 33
      Top = 35
      Width = 280
      Height = 21
      TabOrder = 1
      OnChange = DataChanged
    end
    object DataLinkButton: TButton
      Left = 317
      Top = 35
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 2
      OnClick = DataLinkButtonClick
    end
    object ConnectionStringRadioButton: TRadioButton
      Left = 16
      Top = 72
      Width = 137
      Height = 17
      Caption = 'Use &Connection String'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = DataChanged
    end
    object ConnectionStringEdit: TEdit
      Left = 33
      Top = 91
      Width = 280
      Height = 21
      TabOrder = 4
      OnChange = DataChanged
    end
    object ConnectionStringButton: TButton
      Left = 317
      Top = 91
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 5
      OnClick = ConnectionStringButtonClick
    end
    object StreamFormatComboBox: TComboBox
      Left = 32
      Top = 168
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 7
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 32
      Top = 126
      Width = 150
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 6
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 207
    Width = 362
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 204
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 284
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
