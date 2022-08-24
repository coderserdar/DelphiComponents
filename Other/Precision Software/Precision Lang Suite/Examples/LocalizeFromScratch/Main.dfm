object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Precision Language Suite for VCL'
  ClientHeight = 479
  ClientWidth = 555
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
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbField1: TLabel
    Left = 15
    Top = 252
    Width = 36
    Height = 13
    Alignment = taRightJustify
    Caption = 'lbField1'
  end
  object lbLabelName: TLabel
    Left = 13
    Top = 452
    Width = 56
    Height = 13
    Caption = 'LabelName:'
  end
  object lbName: TLabel
    Left = 129
    Top = 452
    Width = 35
    Height = 13
    Caption = 'Default'
  end
  object lbField2: TLabel
    Left = 15
    Top = 275
    Width = 36
    Height = 13
    Alignment = taRightJustify
    Caption = 'lbField2'
  end
  object lbInfo: TLabel
    Left = 13
    Top = 8
    Width = 529
    Height = 233
    AutoSize = False
    Caption = 
      'This simple project demonstrates an embedded localization feautu' +
      'res of Precision Language Suite.'#13#10#13#10'By adding only three lines o' +
      'f code into your main form, you can start localize your applicat' +
      'ion "on the fly".'#13#10'1) Run this demo application'#13#10'2) Press the Al' +
      't+F2 hot-key'#13#10'3) Click on any component on the form (later you c' +
      'an try click while pressing the Shift key)'#13#10'4) A simple editing ' +
      'grid with properties and texts in the current language are displ' +
      'ayed'#13#10'5) Modify the texts you want and confirm the dialog'#13#10'    (' +
      'use double-click or Shift+Enter key to edit multi-line texts)'#13#10'6' +
      ') Now see the newly created language file in application folder ' +
      '(or Langs subfolder)'#13#10'7) To make your localization fully functio' +
      'nal, now you have to add "loading" part of code to your forms,'#13#10 +
      '    as described in the help, or as you can see in another demo ' +
      'projects.'#13#10#13#10'Embedded localization offers a lot of options to se' +
      'tup, such as executing the editor directly,'#13#10'listing also the lo' +
      'calized constants, saving in so called UserMode (to the .lngu fi' +
      'les), etc.'#13#10#13#10'Embedded localization is available for registered ' +
      'users only!'
    WordWrap = True
  end
  object sbMsgWarn: TButton
    Left = 117
    Top = 304
    Width = 99
    Height = 25
    Caption = 'Warning'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object sbMsgInfo: TButton
    Left = 13
    Top = 304
    Width = 99
    Height = 25
    Caption = 'Information'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object sbProperty: TButton
    Left = 221
    Top = 304
    Width = 99
    Height = 25
    Caption = 'Property'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object mTest: TMemo
    Left = 329
    Top = 272
    Width = 213
    Height = 89
    TabOrder = 4
  end
  object lvTest: TListView
    Left = 13
    Top = 372
    Width = 529
    Height = 65
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
    Left = 13
    Top = 336
    Width = 99
    Height = 25
    Caption = 'Child window'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = sbChildWindowClick
  end
  object edField2: TEdit
    Left = 57
    Top = 272
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'edField2'
  end
  object sbMessage1: TButton
    Left = 117
    Top = 336
    Width = 99
    Height = 25
    Caption = 'Message 1'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
  end
  object sbMessage2: TButton
    Left = 221
    Top = 336
    Width = 99
    Height = 25
    Caption = 'Message 2'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
  end
  object pumLanguages: TPopupMenu
    Left = 269
    Top = 432
  end
  object mmMain: TMainMenu
    Left = 345
    Top = 432
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
end
