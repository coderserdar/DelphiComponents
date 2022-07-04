object fce_db_create: Tfce_db_create
  Left = 321
  Top = 325
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = '  Create new database'
  ClientHeight = 279
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    392
    279)
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 344
    Top = 40
    Width = 23
    Height = 22
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 88
    Height = 13
    Caption = 'New database file:'
  end
  object Label2: TLabel
    Left = 16
    Top = 184
    Width = 56
    Height = 13
    Caption = 'Description:'
  end
  object epath: TEdit
    Left = 16
    Top = 40
    Width = 329
    Height = 21
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    OnChange = epathChange
  end
  object encoding: TRadioGroup
    Left = 16
    Top = 96
    Width = 329
    Height = 74
    Caption = 'Encoding'
    ItemIndex = 0
    Items.Strings = (
      'UTF-8'
      'UTF-16 little endian'
      'UTF-16 big endian')
    TabOrder = 2
  end
  object Button1: TButton
    Left = 189
    Top = 246
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Enabled = False
    ModalResult = 1
    TabOrder = 4
  end
  object Cancel: TButton
    Left = 270
    Top = 246
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object edesc: TEdit
    Left = 16
    Top = 203
    Width = 329
    Height = 21
    MaxLength = 127
    TabOrder = 3
  end
  object ChecOver: TCheckBox
    Left = 16
    Top = 64
    Width = 241
    Height = 17
    Caption = 'Overwrite if exists'
    TabOrder = 1
  end
  object SaveDlg: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 352
    Top = 88
  end
end
