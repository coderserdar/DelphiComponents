object FormHistogram: TFormHistogram
  Left = 346
  Top = 123
  BorderStyle = bsDialog
  Caption = 'Histogram'
  ClientHeight = 256
  ClientWidth = 343
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object HistogramView: TmcmImageCtrl
    Left = 8
    Top = 8
    Width = 260
    Height = 132
    BorderStyle = BS_SINGLE
    Color = clWhite
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = False
  end
  object lChannel: TLabel
    Left = 8
    Top = 196
    Width = 42
    Height = 13
    Caption = 'C&hannel:'
    FocusControl = cbChannel
  end
  object lInputMin: TLabel
    Left = 8
    Top = 144
    Width = 46
    Height = 13
    Caption = 'Input min:'
  end
  object lInputMax: TLabel
    Left = 216
    Top = 144
    Width = 49
    Height = 13
    Caption = 'Input max:'
  end
  object lOutputMin: TLabel
    Left = 280
    Top = 96
    Width = 54
    Height = 13
    Caption = 'Output min:'
  end
  object lOutputMax: TLabel
    Left = 280
    Top = 8
    Width = 57
    Height = 13
    Caption = 'Output max:'
  end
  object lGamma: TLabel
    Left = 112
    Top = 144
    Width = 36
    Height = 13
    Caption = 'Gamma'
    Visible = False
  end
  object mcmShape: TmcmShape
    Left = 10
    Top = 10
    Width = 256
    Height = 128
    Cursor = crCross
    Angle = 26
    Length = 300
    Pen.Color = clGray
    Shape = ST_POLYGON
    OnMouseDown = mcmShapeMouseDown
    OnMouseMove = mcmShapeMouseMove
    OnMouseUp = mcmShapeMouseUp
  end
  object btnOK: TButton
    Left = 8
    Top = 224
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 88
    Top = 224
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object cbChannel: TComboBox
    Left = 64
    Top = 192
    Width = 97
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Items.Strings = (
      'Intensity'
      'Red'
      'Green'
      'Blue')
    TabOrder = 2
    OnChange = cbChannelChange
  end
  object rsGamma: TmcmRealSpin
    Left = 112
    Top = 160
    Width = 49
    Height = 22
    TabOrder = 3
    Visible = False
    Value = 1.000000000000000000
    MaxValue = 10.000000000000000000
    MinValue = -10.000000000000000000
    Decimals = 2
    Increment = 1.000000000000000000
  end
  object btnReset: TButton
    Left = 256
    Top = 224
    Width = 75
    Height = 25
    Caption = '&Reset'
    TabOrder = 4
    OnClick = btnResetClick
  end
  object btnAuto: TButton
    Left = 176
    Top = 224
    Width = 75
    Height = 25
    Caption = '&Auto'
    TabOrder = 5
    OnClick = btnAutoClick
  end
  object seInputMin: TmcmIntSpin
    Left = 8
    Top = 160
    Width = 49
    Height = 22
    MaxLength = 3
    TabOrder = 6
    OnChange = seInOutMaxMinChange
    Value = 0
    MaxValue = 255
    MinValue = 0
  end
  object seInputMax: TmcmIntSpin
    Left = 216
    Top = 160
    Width = 49
    Height = 22
    MaxLength = 3
    TabOrder = 7
    OnChange = seInOutMaxMinChange
    Value = 255
    MaxValue = 255
    MinValue = 0
  end
  object seOutputMax: TmcmIntSpin
    Left = 280
    Top = 24
    Width = 49
    Height = 22
    MaxLength = 3
    TabOrder = 8
    OnChange = seInOutMaxMinChange
    Value = 255
    MaxValue = 255
    MinValue = 0
  end
  object seOutputMin: TmcmIntSpin
    Left = 280
    Top = 112
    Width = 49
    Height = 22
    MaxLength = 3
    TabOrder = 9
    OnChange = seInOutMaxMinChange
    Value = 0
    MaxValue = 255
    MinValue = 0
  end
end
