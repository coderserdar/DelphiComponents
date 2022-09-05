object FormFormatAssociate: TFormFormatAssociate
  Left = 321
  Top = 122
  BorderStyle = bsDialog
  Caption = 'File Format Association'
  ClientHeight = 210
  ClientWidth = 402
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lAssociateFile: TLabel
    Left = 8
    Top = 8
    Width = 305
    Height = 33
    AutoSize = False
    Caption = 
      'Select the file formats to associate with Image Toolkit for Delp' +
      'hi. '
    WordWrap = True
  end
  object btnOK: TButton
    Left = 320
    Top = 48
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 320
    Top = 80
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object clbFileFormats: TCheckListBox
    Left = 8
    Top = 48
    Width = 305
    Height = 153
    ItemHeight = 13
    TabOrder = 2
  end
  object btnSelectAll: TButton
    Left = 320
    Top = 112
    Width = 75
    Height = 25
    Caption = '&Select All'
    TabOrder = 3
    OnClick = btnSelectAllClick
  end
  object btnRemoveAll: TButton
    Left = 320
    Top = 144
    Width = 75
    Height = 25
    Caption = '&Remove All'
    TabOrder = 4
    OnClick = btnRemoveAllClick
  end
  object btnHelp: TButton
    Left = 320
    Top = 176
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 5
    OnClick = btnHelpClick
  end
end
