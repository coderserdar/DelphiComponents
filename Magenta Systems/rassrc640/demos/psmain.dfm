object MainForm: TMainForm
  Left = 266
  Top = 166
  Caption = 'Protected Storage Manager'
  ClientHeight = 385
  ClientWidth = 652
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 193
    Top = 0
    Width = 4
    Height = 385
    Color = clGrayText
    ParentColor = False
    ResizeStyle = rsUpdate
    ExplicitHeight = 394
  end
  object LogMemo: TMemo
    Left = 197
    Top = 0
    Width = 455
    Height = 385
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object TreeView: TTreeView
    Left = 0
    Top = 0
    Width = 193
    Height = 385
    Align = alLeft
    Indent = 19
    PopupMenu = PopupMenu1
    ReadOnly = True
    TabOrder = 1
    OnChange = TreeViewChange
    OnKeyUp = TreeViewKeyUp
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 8
    Top = 8
    object UpdateItem: TMenuItem
      Caption = 'Update'
      OnClick = UpdateItemClick
    end
    object DeleteItem: TMenuItem
      Caption = 'Delete'
      OnClick = DeleteItemClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 8
    object FileItem: TMenuItem
      Caption = 'File'
      object ExitItem: TMenuItem
        Caption = 'Exit'
        OnClick = ExitItemClick
      end
    end
  end
end
