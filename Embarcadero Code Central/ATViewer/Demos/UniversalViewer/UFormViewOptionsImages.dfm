object FormViewOptionsImages: TFormViewOptionsImages
  Left = 305
  Top = 135
  ActiveControl = chkUseIView
  BorderStyle = bsDialog
  Caption = 'Libraries'
  ClientHeight = 278
  ClientWidth = 353
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
    Left = 180
    Top = 248
    Width = 81
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnOk: TButton
    Left = 92
    Top = 248
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object boxIView: TGroupBox
    Left = 8
    Top = 4
    Width = 337
    Height = 157
    TabOrder = 0
    object labExeIView: TLabel
      Left = 24
      Top = 38
      Width = 99
      Height = 13
      Caption = 'Application filename:'
      FocusControl = btnBrowseExe
    end
    object labExtIView: TLabel
      Left = 24
      Top = 78
      Width = 75
      Height = 13
      Caption = 'File extensions:'
      FocusControl = edExtIView
    end
    object chkUseIView: TCheckBox
      Left = 8
      Top = 16
      Width = 321
      Height = 17
      Caption = 'Use IView'
      TabOrder = 0
      OnClick = chkUseIViewClick
    end
    object edExeIView: TEdit
      Left = 24
      Top = 52
      Width = 257
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
    end
    object btnBrowseExe: TButton
      Left = 286
      Top = 52
      Width = 41
      Height = 21
      Caption = '...'
      TabOrder = 2
      OnClick = btnBrowseExeClick
    end
    object edExtIView: TEdit
      Left = 24
      Top = 92
      Width = 257
      Height = 21
      TabOrder = 3
    end
    object chkPriority: TCheckBox
      Left = 24
      Top = 116
      Width = 305
      Height = 33
      Caption = 'IrfanView/XnView has higher priority'
      TabOrder = 4
      WordWrap = True
    end
  end
  object boxIJL: TGroupBox
    Left = 8
    Top = 164
    Width = 337
    Height = 77
    TabOrder = 1
    object labExtIJL: TLabel
      Left = 24
      Top = 34
      Width = 75
      Height = 13
      Caption = 'File extensions:'
      FocusControl = edExtIJL
    end
    object chkUseIJL: TCheckBox
      Left = 8
      Top = 16
      Width = 321
      Height = 17
      Caption = 'Use IJL'
      TabOrder = 0
      OnClick = chkUseIJLClick
    end
    object edExtIJL: TEdit
      Left = 24
      Top = 48
      Width = 257
      Height = 21
      TabOrder = 1
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.exe|*.exe'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 304
    Top = 80
  end
end
