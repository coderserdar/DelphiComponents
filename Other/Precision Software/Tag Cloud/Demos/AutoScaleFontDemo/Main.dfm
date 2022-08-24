object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'TagCloud for VCL - AutoScaleFont Demo'
  ClientHeight = 516
  ClientWidth = 771
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Width = 8
    Height = 516
    AutoSnap = False
    Beveled = True
    ExplicitLeft = 265
    ExplicitHeight = 511
  end
  object tags: TTagCloud
    Left = 193
    Top = 0
    Width = 578
    Height = 516
    Align = alClient
    AutoShrinkRows = True
    Color = clWhite
    Colors = <
      item
        Color = clSilver
      end
      item
        Color = clSkyBlue
      end
      item
        Color = 15114852
      end
      item
        Color = 16744448
      end>
    ColSpacing = 16
    HoverStyle = [fsUnderline]
    LogScale = True
    MaxFontSize = 17
    Padding.Left = 12
    Padding.Top = 12
    Padding.Right = 12
    Padding.Bottom = 12
    ParentColor = False
    Sorted = False
    OnTagClick = tagsTagClick
    OnTagDblClick = tagsTagDblClick
    ExplicitLeft = 191
  end
  object pLeft: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 516
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 96
      Width = 68
      Height = 13
      Caption = 'Min. font size:'
    end
    object Label2: TLabel
      Left = 8
      Top = 152
      Width = 72
      Height = 13
      Caption = 'Max. font size:'
    end
    object rgVAlign: TRadioGroup
      Left = 8
      Top = 329
      Width = 163
      Height = 89
      Caption = 'Vertical alignment'
      ItemIndex = 0
      Items.Strings = (
        'Top'
        'Center'
        'Bottom')
      TabOrder = 0
      OnClick = rgVAlignClick
    end
    object sbRand: TButton
      Left = 8
      Top = 13
      Width = 141
      Height = 25
      Caption = 'Generate items randomly'
      TabOrder = 1
      OnClick = sbRandClick
    end
    object cbAutoScaleFont: TCheckBox
      Left = 8
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Auto-scale font'
      TabOrder = 2
      OnClick = cbAutoScaleFontClick
    end
    object rgAlignment: TRadioGroup
      Left = 8
      Top = 228
      Width = 163
      Height = 89
      Caption = 'Alignment'
      ItemIndex = 1
      Items.Strings = (
        'Left'
        'Center'
        'Right')
      TabOrder = 3
      OnClick = rgAlignmentClick
    end
    object tbMinFontSize: TTrackBar
      Left = 2
      Top = 115
      Width = 160
      Height = 29
      Max = 80
      PageSize = 10
      Frequency = 10
      Position = 8
      PositionToolTip = ptRight
      ShowSelRange = False
      TabOrder = 4
      ThumbLength = 12
      OnChange = tbMinFontSizeChange
    end
    object tbMaxFontSize: TTrackBar
      Left = 2
      Top = 171
      Width = 160
      Height = 29
      Max = 80
      PageSize = 10
      Frequency = 10
      Position = 17
      PositionToolTip = ptRight
      ShowSelRange = False
      TabOrder = 5
      ThumbLength = 12
      OnChange = tbMaxFontSizeChange
    end
  end
  object DblClickTimer: TTimer
    Enabled = False
    Interval = 150
    OnTimer = DblClickTimerTimer
    Left = 360
    Top = 348
  end
end
