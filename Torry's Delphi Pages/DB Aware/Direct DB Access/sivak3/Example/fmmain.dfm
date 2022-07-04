object fmain: Tfmain
  Left = 299
  Top = 172
  Width = 367
  Height = 640
  BorderIcons = [biSystemMenu, biMinimize]
  BorderWidth = 4
  Caption = 'fmain'
  Color = clWindow
  Constraints.MaxWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TreeObj: TTreeView
    Left = 0
    Top = 40
    Width = 351
    Height = 494
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Ctl3D = True
    HideSelection = False
    Indent = 19
    ParentCtl3D = False
    PopupMenu = Popup
    ReadOnly = True
    RightClickSelect = True
    TabOrder = 0
    OnChange = TreeObjChange
    OnClick = TreeObjClick
    OnCollapsing = TreeObjCollapsing
    OnDblClick = TreeObjDblClick
    OnGetSelectedIndex = TreeObjGetSelectedIndex
  end
  object botp: TPanel
    Left = 0
    Top = 546
    Width = 351
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    Color = 16640730
    TabOrder = 1
  end
  object ppu: TPanel
    Left = 0
    Top = 0
    Width = 351
    Height = 40
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Database'
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PopupMenu = Popup
    TabOrder = 2
    object ToolBar: TToolBar
      Left = 72
      Top = 0
      Width = 233
      Height = 34
      Align = alNone
      ButtonHeight = 32
      ButtonWidth = 32
      Caption = 'ToolBar'
      Color = 16640730
      EdgeBorders = [ebBottom]
      Flat = True
      Images = dm.ImagesBtn
      ParentColor = False
      TabOrder = 0
      Transparent = True
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = aConnect
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton2: TToolButton
        Left = 32
        Top = 0
        Action = aDisconnect
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton3: TToolButton
        Left = 64
        Top = 0
        Action = aRegNew
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton4: TToolButton
        Left = 96
        Top = 0
        Action = aRegEdit
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton6: TToolButton
        Left = 128
        Top = 0
        Action = aRegDel
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton8: TToolButton
        Left = 160
        Top = 0
        Action = aSQL
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton9: TToolButton
        Left = 192
        Top = 0
        Action = aRefresh
        ParentShowHint = False
        ShowHint = True
      end
    end
  end
  object ppb: TPanel
    Left = 0
    Top = 534
    Width = 351
    Height = 12
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 3
  end
  object al: TActionList
    Images = dm.ImagesBtn
    Left = 8
    Top = 104
    object aConnect: TAction
      Caption = 'Connect to database'
      Hint = 'Connect to database'
      ImageIndex = 0
      OnExecute = aConnectExecute
    end
    object aDisconnect: TAction
      Caption = 'Disconnect'
      Hint = 'Disconnect'
      ImageIndex = 27
      OnExecute = aDisconnectExecute
    end
    object aRegEdit: TAction
      Caption = 'Edit database registration'
      Hint = 'Edit database registration'
      ImageIndex = 21
      OnExecute = aRegEditExecut
    end
    object aRegNew: TAction
      Caption = 'New database registration'
      Hint = 'New database registration'
      ImageIndex = 20
      OnExecute = aRegNewExecute
    end
    object aRegDel: TAction
      Caption = 'Delete registration'
      Hint = 'Delete registration'
      ImageIndex = 22
      OnExecute = aRegDelExecute
    end
    object aSQL: TAction
      Caption = 'SQL command(s)'
      ImageIndex = 1
      OnExecute = aSQLExecute
    end
    object aRefresh: TAction
      Caption = 'Refresh'
      ImageIndex = 28
      OnExecute = aRefreshExecute
    end
  end
  object Popup: TPopupMenu
    Left = 8
    Top = 48
    object aConnect1: TMenuItem
      Action = aConnect
    end
    object Disconnect1: TMenuItem
      Action = aDisconnect
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Editdatabaseregistration1: TMenuItem
      Action = aRegEdit
    end
    object Newdatabaseregistration1: TMenuItem
      Action = aRegNew
    end
    object Deleteregistration1: TMenuItem
      Action = aRegDel
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object SQL1: TMenuItem
      Action = aSQL
    end
    object Refresh1: TMenuItem
      Action = aRefresh
    end
  end
  object MainMenu1: TMainMenu
    Left = 56
    Top = 48
    object File1: TMenuItem
      Caption = 'File'
      object Connecttodatabase1: TMenuItem
        Action = aConnect
      end
      object Disconnect2: TMenuItem
        Action = aDisconnect
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Newdatabaseregistration2: TMenuItem
        Action = aRegNew
      end
      object Editdatabaseregistration2: TMenuItem
        Action = aRegEdit
      end
      object Deleteregistration2: TMenuItem
        Action = aRegDel
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object SQL2: TMenuItem
        Action = aSQL
      end
      object Refresh2: TMenuItem
        Action = aRefresh
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
end
