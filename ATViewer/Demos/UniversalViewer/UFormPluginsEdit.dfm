object FormPluginsEdit: TFormPluginsEdit
  Left = 330
  Top = 273
  ActiveControl = edDetect
  BorderStyle = bsDialog
  Caption = 'Configure plugin'
  ClientHeight = 210
  ClientWidth = 393
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
  object btnOk: TButton
    Left = 111
    Top = 180
    Width = 82
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 199
    Top = 180
    Width = 82
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object boxMain: TGroupBox
    Left = 8
    Top = 4
    Width = 377
    Height = 169
    TabOrder = 0
    object labFilename: TLabel
      Left = 8
      Top = 12
      Width = 46
      Height = 13
      Caption = 'Filename:'
      FocusControl = edFilename
    end
    object labDetect: TLabel
      Left = 8
      Top = 52
      Width = 66
      Height = 13
      Caption = '&Detect string:'
      FocusControl = edDetect
    end
    object labEditIni: TLabel
      Left = 8
      Top = 120
      Width = 52
      Height = 13
      Caption = 'Config file:'
    end
    object edFilename: TEdit
      Left = 8
      Top = 28
      Width = 361
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
    object edDetect: TEdit
      Left = 7
      Top = 68
      Width = 362
      Height = 21
      TabOrder = 1
    end
    object btnDetectDefault: TButton
      Left = 8
      Top = 92
      Width = 105
      Height = 23
      Caption = 'D&efault'
      TabOrder = 2
      OnClick = btnDetectDefaultClick
    end
    object btnEditIni: TButton
      Left = 8
      Top = 136
      Width = 105
      Height = 23
      Caption = '&Edit...'
      TabOrder = 3
      OnClick = btnEditIniClick
    end
  end
end
