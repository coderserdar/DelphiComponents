inherited PBar: TPBar
  Left = 465
  Top = 161
  Width = 361
  Height = 620
  Caption = 'PBar'
  ParentFont = False
  Font.Height = -13
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Panel1: TPanel [0]
    Left = 0
    Top = 24
    Width = 333
    Height = 504
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ListView1: TListView
      Left = 8
      Top = 32
      Width = 97
      Height = 97
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      Color = cl3DLight
      Columns = <
        item
          AutoSize = True
        end>
      IconOptions.AutoArrange = True
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      Visible = False
      OnClick = Liste1Click
      OnCompare = Liste1Compare
      OnContextPopup = Liste1ContextPopup
      OnDblClick = Liste1DblClick
      OnGetImageIndex = Liste1GetImageIndex
      OnKeyUp = Liste1KeyUp
      OnMouseDown = Liste1MouseDown
      OnMouseUp = Liste1MouseUp
      OnSelectItem = ListView1SelectItem
    end
    object ListView2: TListView
      Left = 112
      Top = 32
      Width = 97
      Height = 97
      BevelInner = bvSpace
      BevelKind = bkTile
      BorderStyle = bsNone
      Color = cl3DLight
      Columns = <
        item
          AutoSize = True
        end>
      IconOptions.AutoArrange = True
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 1
      ViewStyle = vsReport
      Visible = False
      OnClick = Liste1Click
      OnCompare = Liste1Compare
      OnContextPopup = Liste1ContextPopup
      OnDblClick = Liste1DblClick
      OnKeyUp = Liste1KeyUp
      OnMouseDown = Liste1MouseDown
      OnMouseUp = Liste1MouseUp
      OnSelectItem = ListView1SelectItem
    end
    object Header: THeaderControl
      Left = 0
      Top = 0
      Width = 333
      Height = 20
      FullDrag = False
      Images = ImageList
      Sections = <
        item
          ImageIndex = 0
          MaxWidth = 25
          MinWidth = 25
          Width = 25
        end
        item
          AutoSize = True
          ImageIndex = 16
          Text = 'Standard'
          Width = 258
        end
        item
          Alignment = taRightJustify
          ImageIndex = 24
          MaxWidth = 25
          MinWidth = 25
          Width = 25
        end
        item
          Alignment = taRightJustify
          ImageIndex = 23
          MaxWidth = 25
          MinWidth = 25
          Width = 25
        end>
      OnSectionClick = HeaderSectionClick
    end
    object TreeView1: TTreeView
      Left = 8
      Top = 144
      Width = 97
      Height = 97
      BevelInner = bvNone
      BevelKind = bkSoft
      BorderStyle = bsNone
      Color = cl3DLight
      Indent = 31
      ReadOnly = True
      ShowButtons = False
      ShowLines = False
      ShowRoot = False
      TabOrder = 3
      Visible = False
      OnChange = TreeView1Change
      OnClick = Liste1Click
      OnCompare = TreeView1Compare
      OnContextPopup = Liste1ContextPopup
      OnDblClick = Liste1DblClick
      OnGetImageIndex = TreeView1GetImageIndex
      OnKeyUp = Liste1KeyUp
      OnMouseDown = Liste1MouseDown
      OnMouseUp = Liste1MouseUp
    end
    object TreeView2: TTreeView
      Left = 112
      Top = 144
      Width = 97
      Height = 97
      BevelInner = bvNone
      BevelKind = bkSoft
      BorderStyle = bsNone
      Color = cl3DLight
      Indent = 31
      ReadOnly = True
      ShowButtons = False
      ShowRoot = False
      TabOrder = 4
      Visible = False
      OnChange = TreeView1Change
      OnClick = Liste1Click
      OnCompare = TreeView1Compare
      OnContextPopup = Liste1ContextPopup
      OnDblClick = Liste1DblClick
      OnKeyUp = Liste1KeyUp
      OnMouseDown = Liste1MouseDown
      OnMouseUp = Liste1MouseUp
    end
  end
  object SearchPanel: TPanel [1]
    Left = 0
    Top = 528
    Width = 353
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    PopupMenu = CfgSearch
    TabOrder = 1
    DesignSize = (
      353
      39)
    object Edit: TComboBox
      Left = 8
      Top = 8
      Width = 298
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      Sorted = True
      TabOrder = 0
      OnChange = EditChange
    end
    object BtnGo: TButton
      Left = 313
      Top = 5
      Width = 35
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Go'
      Default = True
      TabOrder = 1
      OnClick = BtnGoClick
    end
  end
  object A2ZBar: TPanel [2]
    Left = 333
    Top = 24
    Width = 20
    Height = 504
    Align = alRight
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PopupMenu = CfgA2Z
    TabOrder = 2
    object Button1: TSpeedButton
      Left = 0
      Top = 0
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'A'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button2: TSpeedButton
      Left = 0
      Top = 18
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'B'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button3: TSpeedButton
      Left = 0
      Top = 36
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'C'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button4: TSpeedButton
      Left = 0
      Top = 54
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'D'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button5: TSpeedButton
      Left = 0
      Top = 72
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'E'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button6: TSpeedButton
      Left = 0
      Top = 90
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'F'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button7: TSpeedButton
      Left = 0
      Top = 108
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'G'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button8: TSpeedButton
      Left = 0
      Top = 126
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'H'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button9: TSpeedButton
      Left = 0
      Top = 144
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'I'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button10: TSpeedButton
      Left = 0
      Top = 162
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'J'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button11: TSpeedButton
      Left = 0
      Top = 180
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'K'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button12: TSpeedButton
      Left = 0
      Top = 198
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'L'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button13: TSpeedButton
      Left = 0
      Top = 216
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'M'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button14: TSpeedButton
      Left = 0
      Top = 234
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'N'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button15: TSpeedButton
      Left = 0
      Top = 252
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'O'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button16: TSpeedButton
      Left = 0
      Top = 270
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'P'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button17: TSpeedButton
      Left = 0
      Top = 288
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Q'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button18: TSpeedButton
      Left = 0
      Top = 306
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'R'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button19: TSpeedButton
      Left = 0
      Top = 324
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'S'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button20: TSpeedButton
      Left = 0
      Top = 360
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'U'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button21: TSpeedButton
      Left = 0
      Top = 342
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'T'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button22: TSpeedButton
      Left = 0
      Top = 378
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'V'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button23: TSpeedButton
      Left = 0
      Top = 396
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'W'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button24: TSpeedButton
      Left = 0
      Top = 414
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'X'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button25: TSpeedButton
      Left = 0
      Top = 432
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Y'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
    object Button26: TSpeedButton
      Left = 0
      Top = 450
      Width = 20
      Height = 18
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Z'
      Flat = True
      PopupMenu = CfgA2Z
      OnClick = A2ZClick
    end
  end
  object StatusBar: TStatusBar [3]
    Left = 0
    Top = 567
    Width = 353
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 40
      end
      item
        Alignment = taCenter
        Width = 50
      end>
    ParentColor = True
  end
  object ToolBar: TToolBar [4]
    Left = 0
    Top = 0
    Width = 353
    Height = 24
    AutoSize = True
    Caption = 'ToolBar'
    Flat = True
    Images = ImageList
    PopupMenu = CfgBtn
    TabOrder = 4
    object aCatActualBtn: TToolButton
      Tag = 1
      Left = 0
      Top = 0
      Action = aCatActual
      Grouped = True
      Style = tbsCheck
    end
    object aCatAllBtn: TToolButton
      Tag = 2
      Left = 23
      Top = 0
      Action = aCatAll
      Grouped = True
      Style = tbsCheck
    end
    object aCatTreeBtn: TToolButton
      Tag = 3
      Left = 46
      Top = 0
      Action = aCatTree
      Grouped = True
      Style = tbsCheck
    end
    object aCatFormBtn: TToolButton
      Tag = 4
      Left = 69
      Top = 0
      Action = aCatForm
      Grouped = True
      Style = tbsCheck
    end
    object aCatHistoryBtn: TToolButton
      Tag = 5
      Left = 92
      Top = 0
      Action = aCatHistory
      Grouped = True
      Style = tbsCheck
    end
    object aCatFavoritesBtn: TToolButton
      Tag = 6
      Left = 115
      Top = 0
      Action = aCatFavorites
      Grouped = True
      Style = tbsCheck
    end
    object ToolButton7: TToolButton
      Left = 138
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 7
      Style = tbsSeparator
    end
    object aCompAddBtn: TToolButton
      Tag = 7
      Left = 146
      Top = 0
      Action = aCompAddInfo
    end
    object aCompInfoBtn: TToolButton
      Tag = 8
      Left = 169
      Top = 0
      Action = aCompInfo
    end
    object aCompHelpBtn: TToolButton
      Tag = 9
      Left = 192
      Top = 0
      Action = aCompHelp
    end
    object aCompAddFavBtn: TToolButton
      Tag = 10
      Left = 215
      Top = 0
      Action = aCompAddFav
    end
    object aCompDelFavBtn: TToolButton
      Tag = 11
      Left = 238
      Top = 0
      Action = aCompDelFav
    end
    object ToolButton12: TToolButton
      Left = 261
      Top = 0
      Width = 8
      Caption = 'ToolButton12'
      ImageIndex = 14
      Style = tbsSeparator
    end
    object aConfCatBtn: TToolButton
      Tag = 12
      Left = 269
      Top = 0
      Action = aConfCat
    end
    object aConfPBBtn: TToolButton
      Tag = 13
      Left = 292
      Top = 0
      Action = aConfPB
    end
    object aAboutBtn: TToolButton
      Tag = 14
      Left = 315
      Top = 0
      Action = aAbout
    end
  end
  inherited DockActionList: TActionList
    Top = 424
  end
  object FavMenu: TPopupMenu
    Images = ImageList
    Left = 40
    Top = 358
    object aCompAddInfo2: TMenuItem
      Action = aCompAddInfo
    end
    object aCompInfo2: TMenuItem
      Action = aCompInfo
    end
    object aCompHelp2: TMenuItem
      Action = aCompHelp
    end
    object aCompAddFav2: TMenuItem
      Action = aCompAddFav
    end
    object aCompDelFav2: TMenuItem
      Action = aCompDelFav
    end
  end
  object CfgBtn: TPopupMenu
    Images = ImageList
    Left = 8
    Top = 389
    object CfgBtnShow: TMenuItem
      object CfgBtn1: TMenuItem
        Tag = 1
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn2: TMenuItem
        Tag = 2
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn3: TMenuItem
        Tag = 3
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn4: TMenuItem
        Tag = 4
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn5: TMenuItem
        Tag = 5
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn6: TMenuItem
        Tag = 6
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn7: TMenuItem
        Tag = 7
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn8: TMenuItem
        Tag = 8
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn9: TMenuItem
        Tag = 9
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn10: TMenuItem
        Tag = 10
        AutoCheck = True
        OnClick = CfgBtn10Click
      end
      object CfgBtn11: TMenuItem
        Tag = 11
        OnClick = CfgBtn10Click
      end
      object CfgBtn12: TMenuItem
        Tag = 12
        OnClick = CfgBtn10Click
      end
      object CfgBtn13: TMenuItem
        Tag = 13
        OnClick = CfgBtn10Click
      end
      object CfgBtn14: TMenuItem
        Tag = 14
        OnClick = CfgBtn10Click
      end
    end
    object CfgBtnPos: TMenuItem
      object CfgBtnTop: TMenuItem
        Tag = 1
        AutoCheck = True
        RadioItem = True
        OnClick = CfgBtnRightClick
      end
      object CfgBtnLeft: TMenuItem
        Tag = 2
        AutoCheck = True
        RadioItem = True
        OnClick = CfgBtnRightClick
      end
      object CfgBtnRight: TMenuItem
        Tag = 3
        AutoCheck = True
        RadioItem = True
        OnClick = CfgBtnRightClick
      end
    end
    object CfgBtnViz: TMenuItem
      OnClick = CfgBtnVizClick
    end
  end
  object CfgPal: TPopupMenu
    Images = ImageList
    Left = 40
    Top = 389
    object CfgPalShow: TMenuItem
      GroupIndex = 1
      object CfgPalViewKachel: TMenuItem
        Tag = 1
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = CfgPalViewListClick
      end
      object CfgPalViewSymbol: TMenuItem
        Tag = 2
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = CfgPalViewListClick
      end
      object CfgPalViewList: TMenuItem
        Tag = 3
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = CfgPalViewListClick
      end
      object CfgPalViewText: TMenuItem
        Tag = 4
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = CfgPalViewListClick
      end
    end
    object CfgPalText: TMenuItem
      GroupIndex = 1
      object CfgPalTextBig: TMenuItem
        Tag = 1
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = CfgPalTextSmallClick
      end
      object CfgPalTextMiddle: TMenuItem
        Tag = 2
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = CfgPalTextSmallClick
      end
      object CfgPalTextSmall: TMenuItem
        Tag = 3
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
        OnClick = CfgPalTextSmallClick
      end
    end
    object CfgPalHottrack: TMenuItem
      AutoCheck = True
      GroupIndex = 1
      OnClick = CfgPalHottrackClick
    end
  end
  object CfgA2Z: TPopupMenu
    Images = ImageList
    Left = 72
    Top = 389
    object CfgA2ZFilter: TMenuItem
      Tag = 3
      AutoCheck = True
      OnClick = CfgA2ZShowClick
    end
    object CfgA2ZLeft: TMenuItem
      Tag = 1
      AutoCheck = True
      GroupIndex = 1
      RadioItem = True
      OnClick = CfgA2ZShowClick
    end
    object CfgA2ZRight: TMenuItem
      Tag = 2
      AutoCheck = True
      GroupIndex = 1
      RadioItem = True
      OnClick = CfgA2ZShowClick
    end
    object CfgA2ZShow: TMenuItem
      Tag = 4
      AutoCheck = True
      GroupIndex = 1
      OnClick = CfgA2ZShowClick
    end
  end
  object CfgHist: TPopupMenu
    Images = ImageList
    Left = 104
    Top = 389
    object CfgHistSave: TMenuItem
      Tag = 1
      AutoCheck = True
      OnClick = CfgHistSaveClick
    end
    object CfgHistClear: TMenuItem
      Tag = 2
      OnClick = CfgHistClearClick
    end
  end
  object CfgSearch: TPopupMenu
    Images = ImageList
    Left = 136
    Top = 389
    object CfgSearchTop: TMenuItem
      Tag = 1
      AutoCheck = True
      GroupIndex = 1
      RadioItem = True
      OnClick = CfgSearchBottomClick
    end
    object CfgSearchBottom: TMenuItem
      Tag = 2
      AutoCheck = True
      GroupIndex = 1
      RadioItem = True
      OnClick = CfgSearchBottomClick
    end
    object CfgSearchShow: TMenuItem
      Tag = 3
      AutoCheck = True
      GroupIndex = 1
      OnClick = CfgSearchBottomClick
    end
  end
  object TabMenu: TPopupMenu
    Images = ImageList
    Left = 8
    Top = 357
    object MnuTearLine: TMenuItem
      Caption = '-'
    end
    object aCatActual1: TMenuItem
      Action = aCatActual
    end
    object aCatAll1: TMenuItem
      Action = aCatAll
    end
    object aCatTree1: TMenuItem
      Action = aCatTree
    end
    object aCatForm1: TMenuItem
      Action = aCatForm
    end
    object aCatHistory1: TMenuItem
      Action = aCatHistory
    end
    object aCatFavorites1: TMenuItem
      Action = aCatFavorites
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object aCompAddInfo1: TMenuItem
      Action = aCompAddInfo
    end
    object aCompInfo1: TMenuItem
      Action = aCompInfo
    end
    object aCompHelp1: TMenuItem
      Action = aCompHelp
    end
    object aCompAddFav1: TMenuItem
      Action = aCompAddFav
    end
    object aCompDelFav1: TMenuItem
      Action = aCompDelFav
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object aConfCat1: TMenuItem
      Action = aConfCat
    end
    object aConfPB1: TMenuItem
      Action = aConfPB
    end
    object aAbout1: TMenuItem
      Action = aAbout
    end
  end
  object ActionList: TActionList
    Images = ImageList
    Left = 40
    Top = 424
    object aCatActual: TAction
      Caption = 'aCatActual'
      ImageIndex = 1
      OnExecute = aCatActualExecute
    end
    object aCatAll: TAction
      Caption = 'aCatAll'
      ImageIndex = 2
      OnExecute = aCatAllExecute
    end
    object aCatTree: TAction
      Caption = 'aCatTree'
      ImageIndex = 3
      OnExecute = aCatTreeExecute
    end
    object aCatForm: TAction
      Caption = 'aCatForm'
      ImageIndex = 4
      OnExecute = aCatFormExecute
    end
    object aCatHistory: TAction
      Caption = 'aCatHistory'
      ImageIndex = 5
      OnExecute = aCatHistoryExecute
    end
    object aCatFavorites: TAction
      Caption = 'aCatFavorites'
      ImageIndex = 6
      OnExecute = aCatFavoritesExecute
    end
    object aCompAddInfo: TAction
      Caption = 'aCompAddInfo'
      ImageIndex = 7
      OnExecute = aCompAddInfoExecute
    end
    object aCompInfo: TAction
      Caption = 'aCompInfo'
      ImageIndex = 8
      OnExecute = aCompInfoExecute
    end
    object aCompHelp: TAction
      Caption = 'aCompHelp'
      ImageIndex = 9
      OnExecute = aCompHelpExecute
    end
    object aCompAddFav: TAction
      Caption = 'aCompAddFav'
      ImageIndex = 10
      OnExecute = aCompAddFavExecute
    end
    object aCompDelFav: TAction
      Caption = 'aCompDelFav'
      ImageIndex = 11
      OnExecute = aCompDelFavExecute
    end
    object aConfCat: TAction
      Caption = 'aConfCat'
      ImageIndex = 12
      OnExecute = aConfCatExecute
    end
    object aConfPB: TAction
      Caption = 'aConfPB'
      ImageIndex = 13
      OnExecute = aConfPBExecute
    end
    object aAbout: TAction
      Caption = 'aAbout'
      ImageIndex = 14
      OnExecute = aAboutExecute
    end
  end
  object ImageList: TImageList
    Left = 72
    Top = 424
  end
end
