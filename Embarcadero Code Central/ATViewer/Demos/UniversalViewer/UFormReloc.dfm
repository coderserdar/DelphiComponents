object FormReloc: TFormReloc
  Left = 170
  Top = 321
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Ini relocation tool for Universal Viewer'
  ClientHeight = 243
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 128
    Top = 212
    Width = 82
    Height = 23
    Caption = 'Save'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 216
    Top = 212
    Width = 82
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 6
    Width = 409
    Height = 131
    Caption = ' Configuration files location '
    TabOrder = 0
    object Bevel1: TBevel
      Left = 8
      Top = 100
      Width = 393
      Height = 10
      Shape = bsTopLine
    end
    object chkAppData: TRadioButton
      Left = 8
      Top = 38
      Width = 390
      Height = 17
      Caption = '"&Application data" folder (%AppData%\ATViewer)'
      TabOrder = 1
      OnClick = chkDefaultClick
    end
    object chkDefault: TRadioButton
      Left = 8
      Top = 20
      Width = 390
      Height = 17
      Caption = '&No special location'
      TabOrder = 0
      OnClick = chkDefaultClick
    end
    object chkCustom: TRadioButton
      Left = 8
      Top = 56
      Width = 390
      Height = 17
      Caption = '&Custom folder:'
      TabOrder = 2
      OnClick = chkDefaultClick
    end
    object btnPath: TButton
      Left = 344
      Top = 74
      Width = 33
      Height = 21
      Caption = '...'
      TabOrder = 4
      OnClick = btnPathClick
    end
    object edPath: TEdit
      Left = 24
      Top = 74
      Width = 313
      Height = 21
      TabOrder = 3
    end
    object chkAllUsers: TCheckBox
      Left = 8
      Top = 106
      Width = 390
      Height = 17
      Caption = 'Apply for all &users on this system'
      TabOrder = 5
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 140
    Width = 409
    Height = 65
    Caption = ' Notes '
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 30
      Width = 343
      Height = 13
      Caption = 
        '- This tool doesn'#39't move configuration files from the old to new' +
        ' location.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 44
      Width = 286
      Height = 13
      Caption = '- After saving settings, Universal Viewer must be restarted.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 305
      Height = 13
      Caption = 
        '- If "Viewer.ini" file is present in the UV folder, it is always' +
        ' used.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object XPManifest1: TXPManifest
    Left = 288
    Top = 2
  end
end
