object FormBrowse: TFormBrowse
  Left = 350
  Top = 143
  AutoScroll = False
  Caption = 'Browse'
  ClientHeight = 493
  ClientWidth = 666
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  KeyPreview = True
  PopupMenu = PopupMenu
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterWin: TSplitter
    Left = 177
    Top = 0
    Width = 3
    Height = 474
    Cursor = crHSplit
    Color = clBtnFace
    ParentColor = False
  end
  object mcmThumbView: TmcmThumbView
    Left = 180
    Top = 0
    Width = 486
    Height = 474
    HorzScrollBar.Increment = 101
    HorzScrollBar.Tracking = True
    VertScrollBar.Increment = 122
    VertScrollBar.Tracking = True
    Align = alClient
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentCtl3D = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    TabStop = True
    OnClick = mcmThumbViewClick
    AllowMultiSelection = True
    EnableDirMonitor = True
    ExternalDragDrop = True
    SortMethod = TSM_FileName
    ThumbBorderStyle = BS_RAISED
    ThumbColor = clBtnFace
    ThumbHeight = 80
    ThumbSpace = 14
    ThumbWidth = 80
    OnBeforeLoadThumb = ThumbViewBeforeLoadThumb
    OnProgress = mcmThumbViewScanProgress
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 474
    Width = 666
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Text = 'Press Esc to cancel and F5 to refresh.'
        Width = 50
      end>
    SimplePanel = False
  end
  object PanelPath: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 474
    Align = alLeft
    TabOrder = 0
    object DirectoryListBox: TDirectoryListBox
      Left = 1
      Top = 21
      Width = 175
      Height = 452
      Align = alClient
      ItemHeight = 16
      TabOrder = 1
      OnChange = DirectoryListBoxChange
    end
    object PanelDir: TPanel
      Left = 1
      Top = 1
      Width = 175
      Height = 20
      Align = alTop
      TabOrder = 0
      object DriveComboBox: TDriveComboBox
        Left = 0
        Top = 0
        Width = 169
        Height = 19
        DirList = DirectoryListBox
        TabOrder = 0
      end
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 192
    Top = 8
    object DeleteItem: TMenuItem
      Caption = 'Delete permanently '
      OnClick = DeleteItemClick
    end
    object ToRecyclebinItem: TMenuItem
      Caption = 'Delete to recycle bin'
      OnClick = ToRecyclebinItemClick
    end
    object RenameItem: TMenuItem
      Caption = 'Rename'
      OnClick = RenameItemClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SortMenu: TMenuItem
      Caption = 'Sort'
      object ByNameItem: TMenuItem
        Tag = 1
        Caption = 'By name'
        OnClick = OnSortClick
      end
      object ByExtensionItem: TMenuItem
        Tag = 2
        Caption = 'By extension'
        OnClick = OnSortClick
      end
      object ByDateTimeItem: TMenuItem
        Tag = 3
        Caption = 'By date/time'
        OnClick = OnSortClick
      end
      object BySizeItem: TMenuItem
        Tag = 4
        Caption = 'By size'
        OnClick = OnSortClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object AscendingItem: TMenuItem
        Caption = 'Ascending'
        OnClick = SortDirItemClick
      end
      object DescendingItem: TMenuItem
        Tag = 1
        Caption = 'Descending'
        OnClick = SortDirItemClick
      end
    end
    object ShowMultiPagesItem: TMenuItem
      Caption = 'Show Multi Pages'
      OnClick = ShowMultiPagesItemClick
    end
  end
end
