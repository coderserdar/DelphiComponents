object frmMain: TfrmMain
  Left = 390
  Top = 215
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Voice Broadcast Server Demo'
  ClientHeight = 93
  ClientWidth = 285
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 13
    Top = 5
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
  object Label3: TLabel
    Left = 205
    Top = 5
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
    Left = 11
    Top = 55
    Width = 64
    Height = 17
    Caption = 'Active'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = cbActiveClick
  end
  object edIP: TEdit
    Left = 10
    Top = 22
    Width = 177
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object edPort: TEdit
    Left = 200
    Top = 22
    Width = 74
    Height = 21
    ReadOnly = True
    TabOrder = 2
  end
  object ActionList: TActionList
    Left = 136
    Top = 16
    object acShowThreadInfo: TAction
      Caption = 'acShowThreadInfo'
      ShortCut = 16468
      OnExecute = acShowThreadInfoExecute
    end
  end
  object Server: TNMMVoiceBroadcastServer
    OnAuthentication = ServerAuthentication
    Left = 192
    Top = 24
  end
  object MainMenu: TMainMenu
    Left = 240
    Top = 8
    object mnuSettings: TMenuItem
      Caption = 'Settings'
      OnClick = mnuSettingsClick
    end
    object mnuUsers: TMenuItem
      Caption = 'Users'
      OnClick = mnuUsersClick
    end
    object mnuBroadcastMessage: TMenuItem
      Caption = 'Broadcast message'
      OnClick = mnuBroadcastMessageClick
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
