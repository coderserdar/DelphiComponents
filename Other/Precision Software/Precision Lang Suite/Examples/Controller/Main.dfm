object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 394
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object sbChildWindow: TButton
    Left = 100
    Top = 32
    Width = 172
    Height = 25
    Caption = 'sbChildWindow'
    TabOrder = 0
    OnClick = sbChildWindowClick
  end
  object sbOpenTest: TButton
    Left = 100
    Top = 63
    Width = 172
    Height = 25
    Caption = 'sbOpenTest'
    TabOrder = 1
    OnClick = sbOpenTestClick
  end
  object sbTestMessage: TButton
    Left = 100
    Top = 94
    Width = 172
    Height = 25
    Caption = 'sbTestMessage'
    TabOrder = 2
    OnClick = sbTestMessageClick
  end
  object mmMain: TMainMenu
    Left = 272
    Top = 200
    object miFile: TMenuItem
      Caption = 'miFile'
      object miLanguages: TMenuItem
        Caption = 'miLanguages'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Caption = 'miExit'
        OnClick = miExitClick
      end
    end
  end
  object plsController1: TplsController
    OnBeforeLangChange = plsController1BeforeLangChange
    OnError = plsController1Error
    OnInitLangManager = plsController1InitLangManager
    OnLanguageChanged = plsController1LanguageChanged
    Left = 376
    Top = 148
  end
end
