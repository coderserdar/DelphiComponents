object Form1: TForm1
  Left = 271
  Top = 97
  Width = 377
  Height = 182
  Caption = 'TCompress Self-extracting Installer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -14
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 12
    Top = 7
    Width = 198
    Height = 18
    AutoSize = False
    Caption = '&Target Directory'
  end
  object TargetDirectory: TEdit
    Left = 8
    Top = 27
    Width = 265
    Height = 29
    TabOrder = 0
    OnExit = TargetDirectoryExit
  end
  object RunProgram: TCheckBox
    Left = 13
    Top = 56
    Width = 122
    Height = 21
    Caption = '&Run program'
    TabOrder = 1
  end
  object ViewReadMe: TCheckBox
    Left = 135
    Top = 56
    Width = 142
    Height = 21
    Caption = '&View Readme.txt'
    TabOrder = 2
  end
  object Install: TButton
    Left = 94
    Top = 83
    Width = 75
    Height = 27
    Caption = 'Install'
    TabOrder = 3
    OnClick = InstallClick
  end
end
