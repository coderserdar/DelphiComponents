object Form1: TForm1
  Left = 171
  Top = 107
  Width = 396
  Height = 431
  Caption = 'Test Short Cuts'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 80
    Width = 138
    Height = 13
    Caption = 'Select Special Folder to View'
  end
  object Label2: TLabel
    Left = 15
    Top = 340
    Width = 20
    Height = 13
    Caption = 'xxxx'
  end
  object Label3: TLabel
    Left = 15
    Top = 365
    Width = 20
    Height = 13
    Caption = 'xxxx'
  end
  object Label4: TLabel
    Left = 260
    Top = 210
    Width = 99
    Height = 26
    Caption = 'Adds this program to the desktop'
    WordWrap = True
  end
  object ListFolders: TListBox
    Left = 10
    Top = 10
    Width = 236
    Height = 61
    ItemHeight = 13
    Items.Strings = (
      'Network and Dial-Up Connections'
      'Printers'
      'Control Panel')
    TabOrder = 0
    OnClick = ListFoldersClick
  end
  object ListItems: TListBox
    Left = 10
    Top = 105
    Width = 236
    Height = 226
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = ListItemsDblClick
  end
  object doClose: TButton
    Left = 265
    Top = 305
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = doCloseClick
  end
  object doAddScut: TButton
    Left = 270
    Top = 175
    Width = 76
    Height = 25
    Caption = 'Program'
    TabOrder = 3
    OnClick = doAddScutClick
  end
  object FolderId: TSpinEdit
    Left = 275
    Top = 10
    Width = 51
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 4
  end
  object doFolderId: TButton
    Left = 265
    Top = 45
    Width = 75
    Height = 25
    Caption = 'Folder Id'
    TabOrder = 5
    OnClick = doFolderIdClick
  end
end
