object FormPluginsAdd: TFormPluginsAdd
  Left = 250
  Top = 194
  BorderStyle = bsDialog
  Caption = 'Add plugins'
  ClientHeight = 216
  ClientWidth = 410
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
    Left = 120
    Top = 184
    Width = 82
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 208
    Top = 184
    Width = 82
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object boxPlugins: TGroupBox
    Left = 8
    Top = 4
    Width = 393
    Height = 173
    TabOrder = 0
    object labSource: TLabel
      Left = 32
      Top = 88
      Width = 20
      Height = 13
      Caption = '&File:'
      FocusControl = edPath1
    end
    object labSource2: TLabel
      Left = 32
      Top = 128
      Width = 144
      Height = 13
      Caption = 'Total Commander &executable:'
      FocusControl = edPath2
    end
    object chkSrcFolder: TRadioButton
      Left = 240
      Top = 8
      Width = 380
      Height = 17
      Caption = 'Add plugins from folder'
      Enabled = False
      TabOrder = 3
      Visible = False
      OnClick = chkSrcFolderClick
    end
    object chkSrcTC: TRadioButton
      Left = 16
      Top = 64
      Width = 350
      Height = 17
      Caption = 'Add plugins from TC'
      TabOrder = 2
      OnClick = chkSrcFolderClick
    end
    object chkSrcFile: TRadioButton
      Left = 16
      Top = 40
      Width = 350
      Height = 17
      Caption = 'Add single plugin file'
      TabOrder = 1
      OnClick = chkSrcFolderClick
    end
    object edPath1: TEdit
      Left = 32
      Top = 104
      Width = 289
      Height = 21
      TabOrder = 4
    end
    object btnBrowse1: TButton
      Left = 324
      Top = 104
      Width = 41
      Height = 21
      Caption = '...'
      TabOrder = 5
      OnClick = btnBrowse1Click
    end
    object edPath2: TEdit
      Left = 32
      Top = 144
      Width = 289
      Height = 21
      TabOrder = 6
    end
    object btnBrowse2: TButton
      Left = 324
      Top = 144
      Width = 41
      Height = 21
      Caption = '...'
      TabOrder = 7
      OnClick = btnBrowse2Click
    end
    object chkSrcZip: TRadioButton
      Left = 16
      Top = 16
      Width = 350
      Height = 17
      Caption = 'Add plugin from archive'
      TabOrder = 0
      OnClick = chkSrcFolderClick
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 360
    Top = 64
  end
end
