object FormViewToolParams: TFormViewToolParams
  Left = 276
  Top = 186
  ActiveControl = edCaption
  BorderStyle = bsDialog
  Caption = 'User tool properties'
  ClientHeight = 306
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 457
    Height = 265
    Caption = 'Tool properties'
    TabOrder = 0
    object labCommand: TLabel
      Left = 8
      Top = 42
      Width = 56
      Height = 13
      Caption = 'Application:'
      FocusControl = edCommand
    end
    object labParams: TLabel
      Left = 8
      Top = 66
      Width = 59
      Height = 13
      Caption = 'Parameters:'
      FocusControl = edParams
    end
    object labMacros: TLabel
      Left = 8
      Top = 106
      Width = 315
      Height = 13
      Caption = 
        'Parameters macros (double-click to add macro to parameters list)' +
        ':'
    end
    object labCaption: TLabel
      Left = 8
      Top = 18
      Width = 41
      Height = 13
      Caption = 'Caption:'
      FocusControl = edCaption
    end
    object labActions: TLabel
      Left = 8
      Top = 194
      Width = 93
      Height = 13
      Caption = 'Actions to perform:'
    end
    object labActions2: TLabel
      Left = 264
      Top = 208
      Width = 133
      Height = 13
      Caption = 'After executing application:'
    end
    object labActions1: TLabel
      Left = 88
      Top = 208
      Width = 140
      Height = 13
      Caption = 'Before executing application:'
    end
    object labParamsHint: TLabel
      Left = 88
      Top = 88
      Width = 254
      Height = 13
      Caption = 'Hint: add '#39'?'#39' as first character to confirm parameters.'
    end
    object edCommand: TTntEdit
      Left = 88
      Top = 40
      Width = 321
      Height = 21
      TabOrder = 1
    end
    object btnCommandBrowse: TButton
      Left = 416
      Top = 40
      Width = 33
      Height = 21
      Caption = '...'
      TabOrder = 2
      OnClick = btnCommandBrowseClick
    end
    object edParams: TTntEdit
      Left = 88
      Top = 64
      Width = 321
      Height = 21
      TabOrder = 3
    end
    object edCaption: TEdit
      Left = 88
      Top = 16
      Width = 321
      Height = 21
      TabOrder = 0
    end
    object chkSelectAll: TCheckBox
      Left = 88
      Top = 224
      Width = 150
      Height = 17
      Caption = 'Select all'
      TabOrder = 5
    end
    object chkCopy: TCheckBox
      Left = 88
      Top = 240
      Width = 150
      Height = 17
      Caption = 'Copy'
      TabOrder = 6
    end
    object chkExit: TCheckBox
      Left = 264
      Top = 224
      Width = 128
      Height = 17
      Caption = 'Exit'
      TabOrder = 7
    end
    object listMacros: TListBox
      Left = 88
      Top = 124
      Width = 161
      Height = 65
      ItemHeight = 13
      Items.Strings = (
        '"{FileName}"'
        '"{FileDir}"'
        '"{FileNameOnly}"'
        '{FileNameShort}'
        '{FileNameOnlyShort}'
        '{PosLine}'
        '{PosOffset}')
      TabOrder = 4
      OnDblClick = listMacros33DblClick
    end
  end
  object btnOk: TButton
    Left = 152
    Top = 276
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 240
    Top = 276
    Width = 81
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object OpenDialog1: TTntOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 424
    Top = 80
  end
end
