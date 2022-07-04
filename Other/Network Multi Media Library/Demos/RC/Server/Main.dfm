object frmMain: TfrmMain
  Left = 359
  Top = 203
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Screen Share Server'
  ClientHeight = 56
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 180
    Top = 6
    Width = 58
    Height = 13
    Caption = 'Server IP:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 372
    Top = 6
    Width = 69
    Height = 13
    Caption = 'Server Port:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cbActive: TCheckBox
    Left = 9
    Top = 8
    Width = 64
    Height = 17
    Action = acActive
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object edIP: TEdit
    Left = 177
    Top = 23
    Width = 177
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object cbEnableRC: TCheckBox
    Left = 9
    Top = 31
    Width = 160
    Height = 17
    Action = acEnableRC
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    State = cbChecked
    TabOrder = 2
  end
  object edPort: TSpinEdit
    Left = 370
    Top = 22
    Width = 79
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 80
  end
  object Server: TNMMRCServer
    Port = 80
    OnCustomCommand = ServerCustomCommand
    Left = 295
  end
  object ActionList: TActionList
    Left = 263
    object acShowThreadInfo: TAction
      Caption = 'acShowThreadInfo'
      ShortCut = 16468
      OnExecute = acShowThreadInfoExecute
    end
    object acActive: TAction
      Caption = 'Active'
      OnExecute = acActiveExecute
    end
    object acEnableRC: TAction
      Caption = 'Enable Remote Control'
      Checked = True
      OnExecute = acEnableRCExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 232
    object mnuState: TMenuItem
      Caption = 'State'
      object Active1: TMenuItem
        Action = acActive
      end
      object mnuEnableRC: TMenuItem
        Action = acEnableRC
      end
    end
    object mnuAbout: TMenuItem
      Caption = 'About'
      OnClick = mnuAboutClick
    end
    object mnuExit: TMenuItem
      Caption = 'Exit'
      OnClick = mnuExitClick
    end
  end
end
