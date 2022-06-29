object FormView: TFormView
  Left = 135
  Top = 108
  Width = 800
  Height = 500
  Caption = 'ATViewer Browser Demo'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 300
    Top = 0
    Width = 4
    Height = 466
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 466
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 140
      Top = 0
      Width = 4
      Height = 466
      Cursor = crHSplit
    end
    object TreeEx: TVirtualExplorerTreeview
      Left = 0
      Top = 0
      Width = 140
      Height = 466
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
      VirtualExplorerListview = LVEx
      Columns = <>
    end
    object LVEx: TVirtualExplorerListview
      Left = 144
      Top = 0
      Width = 156
      Height = 466
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
    Width = 488
    Height = 466
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object boxViewer: TGroupBox
      Left = 0
      Top = 113
      Width = 488
      Height = 353
      Align = alClient
      Caption = ' Viewer '
      TabOrder = 1
      object Viewer: TATViewer
        Left = 2
        Top = 15
        Width = 484
        Height = 336
        Align = alClient
        BevelOuter = bvNone
        Caption = 'No file loaded'
        TabOrder = 0
        TabStop = True
        MediaFit = True
        TextWrap = True
        TextWidth = 65
        TextWidthFit = True
        TextWidthFitHex = True
        TextFont.Charset = DEFAULT_CHARSET
        TextFont.Color = clWindowText
        TextFont.Height = -12
        TextFont.Name = 'Courier New'
        TextFont.Style = []
        TextFontOEM.Charset = OEM_CHARSET
        TextFontOEM.Color = clWindowText
        TextFontOEM.Height = -12
        TextFontOEM.Name = 'Terminal'
        TextFontOEM.Style = []
      end
    end
    object boxOptions: TGroupBox
      Left = 0
      Top = 0
      Width = 488
      Height = 113
      Align = alTop
      Caption = ' Options '
      TabOrder = 0
      object boxModeOptions: TGroupBox
        Left = 8
        Top = 64
        Width = 473
        Height = 41
        Caption = ' Current mode options '
        TabOrder = 0
        object pnText: TPanel
          Left = 2
          Top = 35
          Width = 469
          Height = 20
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object chkTextWrap: TCheckBox
            Left = 8
            Top = 2
            Width = 81
            Height = 17
            Caption = 'Word wrap'
            TabOrder = 0
            OnClick = chkTextWrapClick
          end
          object chkTextOEM: TCheckBox
            Left = 96
            Top = 2
            Width = 81
            Height = 17
            Caption = 'OEM font'
            TabOrder = 1
            OnClick = chkTextOEMClick
          end
        end
        object pnMedia: TPanel
          Left = 2
          Top = 15
          Width = 469
          Height = 20
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object labScale: TLabel
            Left = 208
            Top = 3
            Width = 29
            Height = 13
            Caption = 'Scale:'
          end
          object chkMediaFit: TCheckBox
            Left = 8
            Top = 2
            Width = 89
            Height = 17
            Caption = 'Fit to window'
            TabOrder = 0
            OnClick = chkMediaFitClick
          end
          object TrackBar1: TTrackBar
            Left = 240
            Top = 2
            Width = 225
            Height = 18
            LineSize = 50
            Max = 700
            Min = 5
            Orientation = trHorizontal
            PageSize = 100
            Frequency = 100
            Position = 100
            SelEnd = 0
            SelStart = 0
            TabOrder = 2
            ThumbLength = 14
            TickMarks = tmBottomRight
            TickStyle = tsAuto
            OnChange = TrackBar1Change
          end
          object chkImageResample: TCheckBox
            Left = 104
            Top = 2
            Width = 81
            Height = 17
            Caption = 'Resampling'
            TabOrder = 1
            OnClick = chkImageResampleClick
          end
        end
      end
      object boxMode: TGroupBox
        Left = 8
        Top = 12
        Width = 473
        Height = 53
        Caption = ' Mode '
        TabOrder = 1
        object chkModeDetect: TCheckBox
          Left = 8
          Top = 14
          Width = 193
          Height = 17
          Caption = 'Auto-detect mode on opening'
          TabOrder = 0
          OnClick = chkModeDetectClick
        end
        object chkModeText: TRadioButton
          Left = 8
          Top = 32
          Width = 57
          Height = 17
          Caption = 'Text'
          TabOrder = 1
          OnClick = chkModeTextClick
        end
        object chkModeBinary: TRadioButton
          Left = 64
          Top = 32
          Width = 57
          Height = 17
          Caption = 'Binary'
          TabOrder = 2
          OnClick = chkModeBinaryClick
        end
        object chkModeHex: TRadioButton
          Left = 120
          Top = 32
          Width = 57
          Height = 17
          Caption = 'Hex'
          TabOrder = 3
          OnClick = chkModeHexClick
        end
        object chkModeMedia: TRadioButton
          Left = 176
          Top = 32
          Width = 73
          Height = 17
          Caption = 'Multimedia'
          TabOrder = 4
          OnClick = chkModeMediaClick
        end
        object chkModeWeb: TRadioButton
          Left = 248
          Top = 32
          Width = 65
          Height = 17
          Caption = 'Internet'
          TabOrder = 5
          OnClick = chkModeWebClick
        end
        object chkModeUnicode: TRadioButton
          Left = 312
          Top = 32
          Width = 65
          Height = 17
          Caption = 'Unicode'
          TabOrder = 6
          OnClick = chkModeUnicodeClick
        end
        object chkModeRTF: TRadioButton
          Left = 376
          Top = 32
          Width = 73
          Height = 17
          Caption = 'RTF/UTF-8'
          TabOrder = 7
          OnClick = chkModeRTFClick
        end
      end
    end
  end
end
