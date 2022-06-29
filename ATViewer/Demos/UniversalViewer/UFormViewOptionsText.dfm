object FormViewOptionsText: TFormViewOptionsText
  Left = 332
  Top = 270
  ActiveControl = chkDetect
  BorderStyle = bsDialog
  Caption = 'Text options'
  ClientHeight = 147
  ClientWidth = 306
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
  object btnCancel: TButton
    Left = 156
    Top = 116
    Width = 81
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 68
    Top = 116
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object boxText: TGroupBox
    Left = 8
    Top = 4
    Width = 289
    Height = 105
    TabOrder = 2
    object labDetect1: TLabel
      Left = 24
      Top = 37
      Width = 34
      Height = 13
      Caption = 'by &first'
      FocusControl = edDetectSize
    end
    object labDetect2: TLabel
      Left = 128
      Top = 37
      Width = 12
      Height = 13
      Caption = 'Kb'
    end
    object labDetectL1: TLabel
      Left = 24
      Top = 61
      Width = 63
      Height = 13
      Caption = '&maximal size:'
      FocusControl = edDetectLimit
    end
    object labDetectL2: TLabel
      Left = 144
      Top = 61
      Width = 81
      Height = 13
      Caption = 'Kb (0: don'#39't limit)'
    end
    object chkDetect: TCheckBox
      Left = 8
      Top = 16
      Width = 273
      Height = 17
      Caption = '&Auto-detect text files'
      TabOrder = 0
      OnClick = chkDetectClick
    end
    object edDetectSize: TEdit
      Left = 96
      Top = 34
      Width = 25
      Height = 21
      TabOrder = 1
    end
    object edDetectLimit: TEdit
      Left = 96
      Top = 58
      Width = 41
      Height = 21
      TabOrder = 2
    end
    object chkDetectOEM: TCheckBox
      Left = 24
      Top = 82
      Width = 257
      Height = 17
      Caption = 'Auto-detect &OEM codepage'
      TabOrder = 3
    end
  end
end
