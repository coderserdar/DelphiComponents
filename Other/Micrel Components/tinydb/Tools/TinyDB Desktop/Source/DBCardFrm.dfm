inherited DBCardForm: TDBCardForm
  Height = 133
  Caption = 'Index'
  PixelsPerInch = 96
  TextHeight = 13
  inherited ToolBar: TToolBar
    Height = 26
    EdgeBorders = [ebBottom]
    TabOrder = 1
    inherited IndexPanel: TPanel
      inherited IndexComboBox: TComboBox
        TabStop = False
      end
    end
  end
  object ScrollBox: TScrollBox [1]
    Left = 0
    Top = 26
    Width = 384
    Height = 53
    HorzScrollBar.Tracking = True
    HorzScrollBar.Visible = False
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 0
  end
  object BottomPanel: TPanel [2]
    Left = 0
    Top = 79
    Width = 384
    Height = 20
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 2
    object RecNoScrollBar: TScrollBar
      Left = 1
      Top = 1
      Width = 271
      Height = 18
      Align = alClient
      Max = 0
      PageSize = 0
      TabOrder = 0
      TabStop = False
      OnChange = RecNoScrollBarChange
    end
    object RecNoPanel: TPanel
      Left = 272
      Top = 1
      Width = 111
      Height = 18
      Align = alRight
      BevelOuter = bvNone
      Caption = '1/100'
      TabOrder = 1
    end
  end
  inherited DataSource: TDataSource
    Left = 12
    Top = 32
  end
  inherited TinyTable: TTinyTable
    AfterScroll = TinyTableAfterScroll
    AfterPost = TinyTableAfterPost
    Left = 48
    Top = 32
  end
end
