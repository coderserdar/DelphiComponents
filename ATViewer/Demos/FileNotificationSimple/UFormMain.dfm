object FormMain: TFormMain
  Left = 214
  Top = 180
  BorderStyle = bsDialog
  Caption = 'ATFileNotificationSimple Demo (Unicode enabled)'
  ClientHeight = 143
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Watch file:'
  end
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 41
    Height = 13
    Caption = 'Options:'
  end
  object Label3: TLabel
    Left = 56
    Top = 76
    Width = 98
    Height = 13
    Caption = 'Refresh delay, msec'
  end
  object edFileName: TTntEdit
    Left = 8
    Top = 24
    Width = 225
    Height = 21
    TabOrder = 0
    Text = 'C:\config.sys'
  end
  object btnBrowseFile: TButton
    Left = 240
    Top = 24
    Width = 49
    Height = 23
    Caption = '...'
    TabOrder = 1
    OnClick = btnBrowseFileClick
  end
  object btnWatchFile: TButton
    Left = 296
    Top = 24
    Width = 49
    Height = 23
    Caption = 'Watch'
    TabOrder = 2
    OnClick = btnWatchFileClick
  end
  object btnClose: TButton
    Left = 256
    Top = 112
    Width = 89
    Height = 23
    Caption = 'Close'
    TabOrder = 4
    OnClick = btnCloseClick
  end
  object edDelay: TEdit
    Left = 8
    Top = 72
    Width = 41
    Height = 21
    TabOrder = 3
    Text = '1000'
  end
  object OpenDialog1: TTntOpenDialog
    InitialDir = 'C:\'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 312
    Top = 48
  end
end
