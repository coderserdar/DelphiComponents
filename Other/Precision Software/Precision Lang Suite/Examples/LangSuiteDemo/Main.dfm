object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Precision Language Suite for VCL'
  ClientHeight = 338
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  PopupMenu = pumLanguages
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbField1: TLabel
    Left = 8
    Top = 16
    Width = 36
    Height = 13
    Alignment = taRightJustify
    Caption = 'lbField1'
  end
  object lbLabelName: TLabel
    Left = 8
    Top = 312
    Width = 56
    Height = 13
    Caption = 'LabelName:'
  end
  object lbName: TLabel
    Left = 124
    Top = 312
    Width = 35
    Height = 13
    Caption = 'Default'
  end
  object lbField2: TLabel
    Left = 8
    Top = 39
    Width = 36
    Height = 13
    Alignment = taRightJustify
    Caption = 'lbField2'
  end
  object sbMsgWarn: TButton
    Left = 112
    Top = 76
    Width = 99
    Height = 25
    Caption = 'Warning'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = sbMsgWarnClick
  end
  object sbMsgInfo: TButton
    Left = 8
    Top = 76
    Width = 99
    Height = 25
    Caption = 'Information'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = sbMsgInfoClick
  end
  object sbProperty: TButton
    Left = 216
    Top = 76
    Width = 99
    Height = 25
    Caption = 'Property'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = sbPropertyClick
  end
  object mTest: TMemo
    Left = 324
    Top = 13
    Width = 213
    Height = 89
    TabOrder = 4
  end
  object lvTest: TListView
    Left = 8
    Top = 140
    Width = 529
    Height = 153
    Columns = <
      item
        Caption = 'Column 1'
        Width = 64
      end
      item
        Caption = 'Column 2'
        Width = 64
      end
      item
        Caption = 'Column 3'
        Width = 64
      end>
    TabOrder = 5
    ViewStyle = vsReport
  end
  object sbChildWindow: TButton
    Left = 8
    Top = 108
    Width = 99
    Height = 25
    Caption = 'Child window'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = sbChildWindowClick
  end
  object edField2: TEdit
    Left = 50
    Top = 36
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'edField2'
  end
  object sbMessage1: TButton
    Left = 112
    Top = 108
    Width = 99
    Height = 25
    Caption = 'Message 1'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = sbMessage1Click
  end
  object sbMessage2: TButton
    Left = 216
    Top = 108
    Width = 99
    Height = 25
    Caption = 'Message 2'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    OnClick = sbMessage2Click
  end
  object sbFont: TButton
    Left = 462
    Top = 305
    Width = 75
    Height = 25
    Caption = 'Font'
    TabOrder = 9
    OnClick = sbFontClick
  end
  object sbResStr: TButton
    Left = 324
    Top = 108
    Width = 99
    Height = 25
    Caption = 'Resource string'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = sbResStrClick
  end
  object pumLanguages: TPopupMenu
    Left = 264
    Top = 292
  end
  object mmMain: TMainMenu
    Left = 340
    Top = 292
    object miFile: TMenuItem
      Caption = 'File'
      object miLanguage: TMenuItem
        Caption = 'Language'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Caption = 'Exit'
        OnClick = miExitClick
      end
    end
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 396
    Top = 292
  end
end
