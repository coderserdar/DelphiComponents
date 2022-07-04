object FormView: TFormView
  Left = 187
  Top = 115
  Width = 734
  Height = 454
  Caption = 'ATBinHex Demo'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 300
    Top = 0
    Width = 4
    Height = 420
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 420
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 140
      Top = 0
      Width = 4
      Height = 420
      Cursor = crHSplit
    end
    object TreeEx: TVirtualExplorerTreeview
      Left = 0
      Top = 0
      Width = 140
      Height = 420
      Active = True
      Align = alLeft
      ColumnDetails = cdUser
      DefaultNodeHeight = 17
      DragHeight = 250
      DragWidth = 150
      FileSizeFormat = fsfExplorer
      FileSort = fsFileType
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Shell Dlg'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      HintMode = hmHint
      ParentColor = False
      RootFolder = rfDrives
      TabOrder = 0
      TabStop = True
      TreeOptions.AutoOptions = [toAutoScroll]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toToggleOnDblClick]
      TreeOptions.PaintOptions = [toShowButtons, toShowTreeLines, toUseBlendedImages, toGhostedIfUnfocused]
      TreeOptions.SelectionOptions = [toLevelSelectConstraint]
      TreeOptions.VETShellOptions = [toContextMenus]
      TreeOptions.VETSyncOptions = [toCollapseTargetFirst, toExpandTarget, toSelectTarget]
      TreeOptions.VETMiscOptions = [toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toBrowseExecuteZipFolder, toChangeNotifierThread, toRemoveContextMenuShortCut]
      TreeOptions.VETImageOptions = [toImages, toThreadedImages, toMarkCutAndCopy]
      Columns = <>
    end
    object LVEx: TVirtualExplorerListview
      Left = 144
      Top = 0
      Width = 156
      Height = 420
      Active = True
      Align = alClient
      ColumnDetails = cdShellColumns
      DefaultNodeHeight = 17
      DragHeight = 250
      DragWidth = 150
      FileObjects = [foFolders, foNonFolders, foHidden]
      FileSizeFormat = fsfExplorer
      FileSort = fsFileType
      Header.AutoSizeIndex = -1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Shell Dlg'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
      Header.SortColumn = 0
      HintMode = hmHint
      Indent = 0
      ParentColor = False
      RootFolder = rfCustom
      RootFolderCustomPath = 'C:\'
      TabOrder = 1
      TabStop = True
      TreeOptions.AutoOptions = [toAutoScroll]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toReportMode, toToggleOnDblClick]
      TreeOptions.PaintOptions = [toShowTreeLines, toUseBlendedImages, toGhostedIfUnfocused]
      TreeOptions.SelectionOptions = [toMultiSelect, toRightClickSelect, toSiblingSelectConstraint]
      TreeOptions.VETFolderOptions = [toHideRootFolder]
      TreeOptions.VETShellOptions = [toRightAlignSizeColumn, toContextMenus, toShellColumnMenu]
      TreeOptions.VETSyncOptions = [toCollapseTargetFirst, toExpandTarget, toSelectTarget]
      TreeOptions.VETMiscOptions = [toBrowseExecuteFolder, toBrowseExecuteFolderShortcut, toBrowseExecuteZipFolder, toChangeNotifierThread, toExecuteOnDblClk]
      TreeOptions.VETImageOptions = [toImages, toThreadedImages, toMarkCutAndCopy]
      OnChange = LVExChange
      ColumnMenuItemCount = 8
      VirtualExplorerTreeview = TreeEx
    end
  end
  object Panel2: TPanel
    Left = 304
    Top = 0
    Width = 422
    Height = 420
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object GroupBox1: TGroupBox
      Left = 0
      Top = 81
      Width = 422
      Height = 339
      Align = alClient
      Caption = ' Viewer '
      TabOrder = 1
      object Viewer: TATBinHex
        Left = 2
        Top = 15
        Width = 418
        Height = 322
        Cursor = crIBeam
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Caption = 'No file loaded'
        Color = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = True
        Visible = False
        FontOEM.Charset = OEM_CHARSET
        FontOEM.Color = clWindowText
        FontOEM.Height = -12
        FontOEM.Name = 'Terminal'
        FontOEM.Style = []
        TextWidth = 65
        TextWidthHex = 12
        TextWidthFit = True
        TextWidthFitHex = True
        TextWidthFitUHex = True
      end
    end
    object GroupBox2: TGroupBox
      Left = 0
      Top = 0
      Width = 422
      Height = 81
      Align = alTop
      Caption = ' Options '
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 14
        Width = 30
        Height = 13
        Caption = 'Mode:'
      end
      object Label2: TLabel
        Left = 8
        Top = 44
        Width = 108
        Height = 13
        Caption = 'Current mode options:'
      end
      object chkModeText: TRadioButton
        Left = 16
        Top = 26
        Width = 57
        Height = 17
        Caption = 'Text'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = chkModeTextClick
      end
      object chkModeBinary: TRadioButton
        Left = 72
        Top = 26
        Width = 57
        Height = 17
        Caption = 'Binary'
        TabOrder = 1
        OnClick = chkModeBinaryClick
      end
      object chkModeHex: TRadioButton
        Left = 128
        Top = 26
        Width = 57
        Height = 17
        Caption = 'Hex'
        TabOrder = 2
        OnClick = chkModeHexClick
      end
      object chkModeUnicode: TRadioButton
        Left = 184
        Top = 26
        Width = 65
        Height = 17
        Caption = 'Unicode'
        TabOrder = 3
        OnClick = chkModeUnicodeClick
      end
      object chkModeUHex: TRadioButton
        Left = 248
        Top = 26
        Width = 89
        Height = 17
        Caption = 'Unicode/Hex'
        TabOrder = 4
        OnClick = chkModeUHexClick
      end
      object chkOEM: TCheckBox
        Left = 96
        Top = 58
        Width = 73
        Height = 17
        Caption = 'OEM font'
        TabOrder = 6
        OnClick = chkOEMClick
      end
      object chkWordWrap: TCheckBox
        Left = 16
        Top = 58
        Width = 80
        Height = 17
        Caption = 'Word wrap'
        TabOrder = 5
        OnClick = chkWordWrapClick
      end
      object chkEnabled: TCheckBox
        Left = 168
        Top = 58
        Width = 65
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = chkEnabledClick
      end
      object chkNonPrint: TCheckBox
        Left = 232
        Top = 58
        Width = 89
        Height = 17
        Caption = 'Non-printable'
        TabOrder = 8
        OnClick = chkNonPrintClick
      end
    end
  end
end
