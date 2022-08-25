object MainForm: TMainForm
  Left = 240
  Top = 130
  Caption = 'Windows XP Embedded  EWF Testing - 8th April 2009'
  ClientHeight = 302
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 270
    Width = 72
    Height = 13
    Caption = 'Persistent Data'
  end
  object Log: TMemo
    Left = 5
    Top = 5
    Width = 426
    Height = 256
    Lines.Strings = (
      'Log')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object doExit: TButton
    Left = 450
    Top = 265
    Width = 86
    Height = 25
    Caption = 'Exit'
    TabOrder = 1
    OnClick = doExitClick
  end
  object doEnable: TButton
    Left = 445
    Top = 90
    Width = 91
    Height = 25
    Caption = 'Enable Overlay'
    TabOrder = 2
    OnClick = doEnableClick
  end
  object doDisable: TButton
    Left = 445
    Top = 50
    Width = 91
    Height = 25
    Caption = 'Disable Overlay'
    TabOrder = 3
    OnClick = doDisableClick
  end
  object VolNr: TSpinEdit
    Left = 465
    Top = 15
    Width = 46
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 1
  end
  object doCommit: TButton
    Left = 445
    Top = 130
    Width = 91
    Height = 25
    Caption = 'Commit Overlay'
    TabOrder = 5
    OnClick = doCommitClick
  end
  object PersData: TEdit
    Left = 105
    Top = 270
    Width = 271
    Height = 21
    MaxLength = 32
    TabOrder = 6
  end
  object doSavePers: TButton
    Left = 445
    Top = 170
    Width = 91
    Height = 25
    Caption = 'Save Persistent'
    TabOrder = 7
    OnClick = doSavePersClick
  end
  object doClear: TButton
    Left = 445
    Top = 210
    Width = 91
    Height = 25
    Caption = 'Clear Command'
    TabOrder = 8
    OnClick = doClearClick
  end
end
