object TestForm: TTestForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'TestForm'
  ClientHeight = 573
  ClientWidth = 792
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
  object TestSubjectGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 241
    Height = 161
    Caption = 'Test Subject'
    TabOrder = 0
    TabStop = True
    object ClassNameLabel: TLabel
      Left = 45
      Top = 24
      Width = 56
      Height = 13
      Alignment = taRightJustify
      Caption = 'Class Name'
    end
    object TypeLabel: TLabel
      Left = 66
      Top = 56
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'Type'
    end
    object Label1: TLabel
      Left = 11
      Top = 104
      Width = 79
      Height = 13
      Alignment = taRightJustify
      Caption = 'Natural item type'
    end
    object Label2: TLabel
      Left = 13
      Top = 128
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'Natural key type'
    end
    object FixedSizeLabel: TLabel
      Left = 50
      Top = 80
      Width = 48
      Height = 13
      Alignment = taRightJustify
      Caption = 'Fixed Size'
    end
    object ClassNameComboBox: TComboBox
      Left = 104
      Top = 24
      Width = 121
      Height = 21
      Style = csDropDownList
      DropDownCount = 16
      ItemHeight = 13
      TabOrder = 0
      OnChange = ClassNameComboBoxChange
      Items.Strings = (
        'TArray'
        'THashIntegerMap'
        'THashMap'
        'THashStringMap'
        'THashSet'
        'TListMap'
        'TListSet'
        'TPArrayBag'
        'TPArrayList'
        'TPArrayMap'
        'TPArraySet'
        '(Array)'
        '(TList)')
    end
    object TypeEdit: TEdit
      Left = 104
      Top = 56
      Width = 121
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
    end
    object NaturalItemTypeEdit: TEdit
      Left = 104
      Top = 104
      Width = 121
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
    end
    object FixedSizeCheckBox: TCheckBox
      Left = 104
      Top = 80
      Width = 15
      Height = 17
      TabStop = False
      Alignment = taLeftJustify
      Enabled = False
      TabOrder = 3
    end
    object NaturalKeyTypeEdit: TEdit
      Left = 104
      Top = 128
      Width = 121
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
    end
  end
  object CloseButton: TButton
    Left = 712
    Top = 540
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = CloseButtonClick
  end
  object TestPageControl: TPageControl
    Left = 264
    Top = 8
    Width = 433
    Height = 561
    ActivePage = FunctionalTabSheet
    TabOrder = 2
    object FunctionalTabSheet: TTabSheet
      Caption = 'Functional Test'
      object FunctionalTestButton: TButton
        Left = 8
        Top = 8
        Width = 129
        Height = 25
        Caption = 'Run functional test'
        TabOrder = 0
        OnClick = FunctionalTestButtonClick
      end
      object FunctionalTestListBox: TListBox
        Left = 16
        Top = 56
        Width = 393
        Height = 473
        TabStop = False
        ItemHeight = 13
        ParentColor = True
        TabOrder = 1
      end
    end
    object PerformanceTabSheet: TTabSheet
      Caption = 'Performance Test'
      ImageIndex = 1
      object CollectionSizeLabel: TLabel
        Left = 8
        Top = 24
        Width = 69
        Height = 13
        Alignment = taRightJustify
        Caption = 'Collection Size'
      end
      object IterationsLabel: TLabel
        Left = 34
        Top = 56
        Width = 43
        Height = 13
        Alignment = taRightJustify
        Caption = 'Iterations'
      end
      object PerfTestStatusLabel: TLabel
        Left = 160
        Top = 96
        Width = 96
        Height = 13
        Caption = 'PerfTestStatusLabel'
      end
      object CollectionSizeComboBox: TComboBox
        Left = 88
        Top = 24
        Width = 73
        Height = 21
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        Text = '1000'
        Items.Strings = (
          '10'
          '100'
          '1000'
          '10000'
          '50'
          '500'
          '5000')
      end
      object ComparisonTestButton: TButton
        Left = 288
        Top = 24
        Width = 129
        Height = 25
        Caption = 'Compare all classes...'
        TabOrder = 1
        OnClick = ComparisonTestButtonClick
      end
      object PerfIterationsComboBox: TComboBox
        Left = 88
        Top = 56
        Width = 73
        Height = 21
        ItemHeight = 13
        Sorted = True
        TabOrder = 2
        Text = '1'
        Items.Strings = (
          '1'
          '10'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9')
      end
      object PerformanceTestButton: TButton
        Left = 16
        Top = 88
        Width = 129
        Height = 25
        Caption = 'Run performance test'
        TabOrder = 3
        OnClick = PerformanceTestButtonClick
      end
      object PerformanceTestStringGrid: TStringGrid
        Left = 16
        Top = 136
        Width = 401
        Height = 397
        ColCount = 2
        DefaultRowHeight = 14
        FixedCols = 0
        FixedRows = 0
        GridLineWidth = 0
        Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect]
        ParentColor = True
        ScrollBars = ssVertical
        TabOrder = 4
      end
    end
    object MemoryLeakTabSheet: TTabSheet
      Caption = 'Memory Leak'
      ImageIndex = 2
      object Iterations2Label: TLabel
        Left = 10
        Top = 24
        Width = 43
        Height = 13
        Alignment = taRightJustify
        Caption = 'Iterations'
      end
      object LeakTestStatusLabel: TLabel
        Left = 144
        Top = 64
        Width = 77
        Height = 13
        Caption = 'TestStatusLabel'
      end
      object LeakIterationsComboBox: TComboBox
        Left = 64
        Top = 24
        Width = 73
        Height = 21
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        Text = '100'
        Items.Strings = (
          '1'
          '10'
          '100'
          '1000'
          '10000'
          '100000')
      end
      object MemoryLeakButton: TButton
        Left = 8
        Top = 56
        Width = 129
        Height = 25
        Caption = 'Run memory leak test'
        TabOrder = 1
        OnClick = MemoryLeakButtonClick
      end
      object MemoryLeakTestStringGrid: TStringGrid
        Left = 16
        Top = 88
        Width = 401
        Height = 445
        ColCount = 2
        DefaultRowHeight = 14
        FixedCols = 0
        FixedRows = 0
        GridLineWidth = 0
        Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect]
        ParentColor = True
        ScrollBars = ssVertical
        TabOrder = 2
      end
    end
  end
end
