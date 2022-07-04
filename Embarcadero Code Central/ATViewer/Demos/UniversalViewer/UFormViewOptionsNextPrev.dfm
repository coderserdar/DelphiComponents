object FormViewOptionsNextPrev: TFormViewOptionsNextPrev
  Left = 225
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Next / Previous commands'
  ClientHeight = 195
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 120
    Top = 164
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 208
    Top = 164
    Width = 81
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 393
    Height = 153
    Caption = 'Show files'
    TabOrder = 2
    object chkShowAll: TRadioButton
      Left = 16
      Top = 20
      Width = 369
      Height = 17
      Caption = 'Show all files'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = chkShowAllClick
    end
    object chkShowCurrent: TRadioButton
      Left = 16
      Top = 124
      Width = 369
      Height = 17
      Caption = 'Skip all except files of the same type as currently opened file'
      TabOrder = 1
      OnClick = chkShowAllClick
    end
    object chkShowCustom: TRadioButton
      Left = 16
      Top = 40
      Width = 369
      Height = 17
      Caption = 'Show only files of selected types'
      TabOrder = 2
      OnClick = chkShowAllClick
    end
    object chkTypeText: TCheckBox
      Left = 40
      Top = 56
      Width = 350
      Height = 17
      Caption = 'Text, RTF'
      TabOrder = 3
    end
    object chkTypeImages: TCheckBox
      Left = 40
      Top = 72
      Width = 350
      Height = 17
      Caption = 'Images'
      TabOrder = 4
    end
    object chkTypeMedia: TCheckBox
      Left = 40
      Top = 88
      Width = 350
      Height = 17
      Caption = 'Multimedia'
      TabOrder = 5
    end
    object chkTypeWeb: TCheckBox
      Left = 40
      Top = 104
      Width = 350
      Height = 17
      Caption = 'Internet'
      TabOrder = 6
    end
  end
end
