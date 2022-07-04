object Form1: TForm1
  Left = 468
  Top = 308
  Caption = 'KGridDemo'
  ClientHeight = 619
  ClientWidth = 810
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PCMain: TPageControl
    Left = 0
    Top = 0
    Width = 810
    Height = 619
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Main demo'
      DesignSize = (
        802
        591)
      object GBSpecial: TGroupBox
        Left = 539
        Top = 513
        Width = 256
        Height = 68
        Anchors = [akTop, akRight]
        Caption = 'Special features'
        TabOrder = 0
        object CBEnabled: TCheckBox
          Left = 131
          Top = 17
          Width = 119
          Height = 17
          Caption = 'Grid is enabled'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = CBEnabledClick
        end
        object CBAlignLastCol: TCheckBox
          Left = 9
          Top = 18
          Width = 119
          Height = 17
          Caption = 'Align last column'
          TabOrder = 0
          OnClick = CBAlignLastColCLick
        end
        object CBAutosizeGrid: TCheckBox
          Left = 9
          Top = 40
          Width = 238
          Height = 17
          Caption = 'Autosize rows on column width change'
          TabOrder = 2
          OnClick = CBAutosizeGridClick
        end
      end
      object GBSelection: TGroupBox
        Left = 539
        Top = 385
        Width = 256
        Height = 45
        Anchors = [akTop, akRight]
        Caption = 'Selection'
        TabOrder = 1
        object CBRowSelect: TCheckBox
          Left = 9
          Top = 18
          Width = 116
          Height = 17
          Caption = 'Row selection'
          TabOrder = 0
          OnClick = CBRowSelectClick
        end
        object CBRangeSelect: TCheckBox
          Left = 131
          Top = 18
          Width = 116
          Height = 17
          Caption = 'Range selection'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = CBRangeSelectClick
        end
      end
      object GBEditing: TGroupBox
        Left = 539
        Top = 334
        Width = 256
        Height = 45
        Anchors = [akTop, akRight]
        Caption = 'Editing cells'
        TabOrder = 2
        object CBEditing: TCheckBox
          Left = 10
          Top = 18
          Width = 118
          Height = 17
          Caption = 'Editing'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CBEditingClick
        end
        object CBNoSelEditedText: TCheckBox
          Left = 131
          Top = 18
          Width = 119
          Height = 17
          Caption = 'Select edited text'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = CBNoSelEditedTextClick
        end
      end
      object GBColsRows: TGroupBox
        Left = 539
        Top = 237
        Width = 256
        Height = 91
        Anchors = [akTop, akRight]
        Caption = 'Column/row related features'
        TabOrder = 3
        object CBColMoving: TCheckBox
          Left = 9
          Top = 18
          Width = 119
          Height = 17
          Caption = 'Movable columns'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CBColMovingClick
        end
        object CBRowMoving: TCheckBox
          Left = 131
          Top = 18
          Width = 116
          Height = 17
          Caption = 'Movable rows'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = CBRowMovingClick
        end
        object CBColSizing: TCheckBox
          Left = 9
          Top = 41
          Width = 119
          Height = 17
          Caption = 'Sizeable columns'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = CBColSizingClick
        end
        object CBRowSizing: TCheckBox
          Left = 131
          Top = 41
          Width = 116
          Height = 17
          Caption = 'Sizeable rows'
          TabOrder = 3
          OnClick = CBRowSizingClick
        end
        object CBColSorting: TCheckBox
          Left = 9
          Top = 64
          Width = 119
          Height = 17
          Caption = 'Sortable columns'
          TabOrder = 4
          OnClick = CBColSortingClick
        end
        object CBRowSorting: TCheckBox
          Left = 131
          Top = 64
          Width = 119
          Height = 17
          Caption = 'Sortable rows'
          Checked = True
          State = cbChecked
          TabOrder = 5
          OnClick = CBRowSortingClick
        end
      end
      object Button2: TButton
        Left = 670
        Top = 133
        Width = 125
        Height = 25
        Action = ACSplit
        Anchors = [akTop, akRight]
        TabOrder = 4
      end
      object BUMerge: TButton
        Left = 539
        Top = 133
        Width = 125
        Height = 25
        Action = ACMerge
        Anchors = [akTop, akRight]
        TabOrder = 5
      end
      object BUSortRows: TButton
        Left = 539
        Top = 102
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Sort rows randomly'
        TabOrder = 6
        OnClick = BUSortRowsClick
      end
      object BUInsertSortedRow: TButton
        Left = 670
        Top = 102
        Width = 125
        Height = 25
        Action = ACInsertSortedRow
        Anchors = [akTop, akRight]
        TabOrder = 7
      end
      object BUModifyCell: TButton
        Left = 670
        Top = 71
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Modify random cell'
        TabOrder = 8
        OnClick = BUModifyCellClick
      end
      object BUDeleteCol: TButton
        Left = 539
        Top = 71
        Width = 125
        Height = 25
        Action = ACDeleteCol
        Anchors = [akTop, akRight]
        TabOrder = 9
      end
      object BUAddColAfter: TButton
        Left = 539
        Top = 40
        Width = 125
        Height = 25
        Action = ACAddColAfter
        Anchors = [akTop, akRight]
        TabOrder = 10
      end
      object BUDeleteRows: TButton
        Left = 670
        Top = 40
        Width = 125
        Height = 25
        Action = ACDeleteRows
        Anchors = [akTop, akRight]
        TabOrder = 11
      end
      object BUAdd5000Rows: TButton
        Left = 670
        Top = 8
        Width = 125
        Height = 25
        Action = ACAdd5000Rows
        Anchors = [akTop, akRight]
        TabOrder = 12
      end
      object BUAddColBefore: TButton
        Left = 539
        Top = 8
        Width = 125
        Height = 25
        Action = ACAddColBefore
        Anchors = [akTop, akRight]
        TabOrder = 13
      end
      object GBAppearance: TGroupBox
        Left = 539
        Top = 436
        Width = 256
        Height = 71
        Anchors = [akTop, akRight]
        Caption = 'Cell appearance'
        TabOrder = 14
        object CBThemedCells: TCheckBox
          Left = 9
          Top = 18
          Width = 119
          Height = 17
          Caption = 'OS themes in cells'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CBThemedCellsClick
        end
        object CBMouseOverCells: TCheckBox
          Left = 9
          Top = 36
          Width = 119
          Height = 30
          Caption = 'Cell highlighting'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = CBMouseOverCellsClick
        end
        object CBClippedCells: TCheckBox
          Left = 131
          Top = 18
          Width = 119
          Height = 17
          Caption = 'Clipped cells'
          TabOrder = 1
          OnClick = CBClippedCellsClick
        end
        object CBDoubleBufferedCells: TCheckBox
          Left = 131
          Top = 36
          Width = 119
          Height = 30
          Caption = 'Double buffered cells'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = CBDoubleBufferedCellsClick
        end
      end
      object KGrid1: TKGrid
        Left = 0
        Top = 0
        Width = 531
        Height = 591
        Align = alLeft
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 8
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Options = [goAlwaysShowEditor, goColMoving, goColSizing, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goIndicateHiddenCells, goMouseCanHideCells, goMouseOverCells, goRangeSelect, goRowMoving, goRowSorting, goTabs, goThemes, goThemedCells, goVertLine]
        OptionsEx = [gxEnterAppendsRow, gxEnterWraps, gxFixedCellClickSelect, gxTabAppendsRow, gxTabWraps]
        ParentFont = False
        RowCount = 8
        TabOrder = 15
        OnBeginColSizing = KGrid1BeginColSizing
        OnBeginRowSizing = KGrid1BeginColSizing
        OnColWidthsChanged = KGrid1ColWidthsChanged
        OnCompareCells = KGrid1CompareCells
        OnDrawCell = KGrid1DrawCell
        OnEditorCreate = KGrid1EditorCreate
        OnEditorDataFromGrid = KGrid1EditorDataFromGrid
        OnEditorDataToGrid = KGrid1EditorDataToGrid
        OnEditorKeyPreview = KGrid1EditorKeyPreview
        OnEditorResize = KGrid1EditorResize
        OnEditorSelect = KGrid1EditorSelect
        OnMeasureCell = KGrid1MeasureCell
        OnMouseDblClickCell = KGrid1MouseDblClickCell
        OnPrintPaint = KGrid1PrintPaint
        ExplicitLeft = 2
        ColWidths = (
          64
          64
          64
          65
          64
          64
          64
          64)
        RowHeights = (
          21
          21
          21
          21
          21
          21
          21
          21)
      end
      object BUPreview: TButton
        Left = 539
        Top = 164
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Preview...'
        TabOrder = 16
        OnClick = BUPreviewClick
      end
      object BUPrint: TButton
        Left = 669
        Top = 164
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Print...'
        TabOrder = 17
        OnClick = BUPrintClick
      end
      object BUAutoSizeCol: TButton
        Left = 539
        Top = 195
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Autosize random col.'
        TabOrder = 18
        OnClick = BUAutoSizeColClick
      end
      object BUAutosizeRow: TButton
        Left = 669
        Top = 195
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Autosize random row'
        TabOrder = 19
        OnClick = BUAutosizeRowClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Merged header cells demo'
      ImageIndex = 1
      object KGrid2: TKGrid
        Left = 0
        Top = 0
        Width = 802
        Height = 591
        Align = alClient
        ColCount = 12
        FixedRows = 3
        Options = [goAlwaysShowEditor, goClippedCells, goColSizing, goDrawFocusSelected, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeaderAlignment, goIndicateHiddenCells, goMouseCanHideCells, goMouseOverCells, goRangeSelect, goRowMoving, goRowSorting, goTabs, goThemes, goThemedCells, goVertLine]
        RowCount = 20
        TabOrder = 0
        OnDrawCell = KGrid2DrawCell
        OnEditorDataToGrid = KGrid2EditorDataToGrid
        ColWidths = (
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64)
        RowHeights = (
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21)
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Virtual grid demo'
      ImageIndex = 2
      object KGrid3: TKGrid
        Left = 0
        Top = 0
        Width = 802
        Height = 591
        Align = alClient
        ColCount = 12
        Options = [goAlwaysShowEditor, goClippedCells, goColMoving, goColSizing, goColSorting, goDrawFocusSelected, goEditing, goEnterMoves, goFixedHorzLine, goFixedVertLine, goHeader, goHeaderAlignment, goHorzLine, goIndicateHiddenCells, goMouseCanHideCells, goMouseOverCells, goRangeSelect, goRowMoving, goRowSorting, goTabs, goThemes, goThemedCells, goVertLine, goVirtualGrid]
        RangeSelectStyle = rsMS_Excel
        RowCount = 20
        TabOrder = 0
        OnCompareCells = KGrid3CompareCells
        OnDrawCell = KGrid3DrawCell
        OnEditorCreate = KGrid3EditorCreate
        OnEditorDataFromGrid = KGrid3EditorDataFromGrid
        OnEditorDataToGrid = KGrid3EditorDataToGrid
        OnExchangeCols = KGrid3ExchangeCols
        OnExchangeRows = KGrid3ExchangeRows
        ColWidths = (
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64)
        RowHeights = (
          21
          21
          24
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21
          21)
      end
    end
  end
  object ActionList1: TActionList
    Left = 640
    Top = 8
    object ACAddColBefore: TAction
      Caption = 'Add column before...'
      OnExecute = ACAddColBeforeExecute
    end
    object ACAddColAfter: TAction
      Caption = 'Add column after...'
      OnExecute = ACAddColBeforeExecute
    end
    object ACDeleteCol: TAction
      Caption = 'Delete column...'
      OnExecute = ACDeleteColExecute
      OnUpdate = ACDeleteColUpdate
    end
    object ACAdd5000Rows: TAction
      Caption = 'Add 5000 rows'
      OnExecute = ACAdd5000RowsExecute
    end
    object ACDeleteRows: TAction
      Caption = 'Delete selected rows'
      OnExecute = ACDeleteRowsExecute
      OnUpdate = ACDeleteRowsUpdate
    end
    object ACInsertSortedRow: TAction
      Caption = 'Insert sorted row'
      OnExecute = ACInsertSortedRowExecute
      OnUpdate = ACInsertSortedRowUpdate
    end
    object ACMerge: TAction
      Caption = 'Merge selected'
      OnExecute = ACMergeExecute
      OnUpdate = ACMergeUpdate
    end
    object ACSplit: TAction
      Caption = 'Split merged cell'
      OnExecute = ACSplitExecute
      OnUpdate = ACSplitUpdate
    end
  end
  object PSDMain: TKPrintSetupDialog
    Control = KGrid1
    SelAvail = False
    Left = 608
    Top = 8
  end
  object PPDMain: TKPrintPreviewDialog
    Control = KGrid1
    Left = 672
    Top = 8
  end
end
