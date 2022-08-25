object Form1: TForm1
  Left = 451
  Top = 213
  Width = 484
  Height = 372
  Caption = 'MDI Demo (TPJWdwState)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 15
  object StatusBar1: TStatusBar
    Left = 0
    Top = 295
    Width = 468
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Status Bar'
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 468
    Height = 29
    ButtonHeight = 23
    ButtonWidth = 73
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Caption = 'ToolButton1'
      ImageIndex = 0
    end
    object ToolButton3: TToolButton
      Left = 73
      Top = 2
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton2: TToolButton
      Left = 81
      Top = 2
      Caption = 'ToolButton2'
      ImageIndex = 1
    end
  end
  object MainMenu1: TMainMenu
    Left = 140
    Top = 52
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
  object PJWdwState1: TPJWdwState
    AutoSaveRestore = True
    Options = [woFitWorkArea]
    IniRootDir = rdExeDir
    Section = 'MainForm'
    Left = 120
    Top = 112
  end
end
