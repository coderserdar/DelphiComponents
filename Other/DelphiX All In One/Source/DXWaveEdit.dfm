object DelphiXWaveEditForm: TDelphiXWaveEditForm
  Left = 354
  Top = 156
  BorderStyle = bsDialog
  Caption = 'Wave Editor'
  ClientHeight = 226
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 8
    Top = 8
    Width = 249
    Height = 209
    Shape = bsFrame
  end
  object PlayImage: TImage
    Left = 16
    Top = 264
    Width = 16
    Height = 16
    AutoSize = True
    Picture.Data = {
      07544269746D6170F6000000424DF60000000000000076000000280000001000
      0000100000000100040000000000800000000000000000000000100000001000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00777777777777777777777777777777777777777777777777777707777777
      7777777700077777777777770000077777777777000000077777777700000000
      0777777700000007777777770000077777777777000777777777777707777777
      7777777777777777777777777777777777777777777777777777777777777777
      7777}
    Visible = False
  end
  object StopImage: TImage
    Left = 40
    Top = 264
    Width = 16
    Height = 16
    AutoSize = True
    Picture.Data = {
      07544269746D6170F6000000424DF60000000000000076000000280000001000
      0000100000000100040000000000800000000000000000000000100000001000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00777777777777777777777777777777777777777777777777777777777777
      7777777700000000777777770000000077777777000000007777777700000000
      7777777700000000777777770000000077777777000000007777777700000000
      7777777777777777777777777777777777777777777777777777777777777777
      7777}
    Visible = False
  end
  object OKButton: TButton
    Left = 264
    Top = 8
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 264
    Top = 40
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = CancelButtonClick
  end
  object ClearButton: TButton
    Left = 176
    Top = 184
    Width = 73
    Height = 25
    Caption = '&Clear'
    TabOrder = 4
    OnClick = ClearButtonClick
  end
  object SaveButton: TButton
    Left = 96
    Top = 184
    Width = 73
    Height = 25
    Caption = '&Save...'
    TabOrder = 3
    OnClick = SaveButtonClick
  end
  object LoadButton: TButton
    Left = 16
    Top = 184
    Width = 73
    Height = 25
    Caption = '&Load...'
    TabOrder = 2
    OnClick = LoadButtonClick
  end
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 233
    Height = 161
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 5
    object TestButton: TSpeedButton
      Left = 16
      Top = 116
      Width = 25
      Height = 25
      Enabled = False
      OnClick = TestButtonClick
    end
    object LengthLabel: TLabel
      Left = 16
      Top = 16
      Width = 36
      Height = 13
      Caption = 'Length:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object FrequencyLabel: TLabel
      Left = 16
      Top = 34
      Width = 24
      Height = 13
      Caption = 'Freq:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object TypeLabel: TLabel
      Left = 16
      Top = 52
      Width = 27
      Height = 13
      Caption = 'Type:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object SizeLabel: TLabel
      Left = 16
      Top = 70
      Width = 23
      Height = 13
      Caption = 'Size:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LengthValueLabel: TLabel
      Left = 72
      Top = 16
      Width = 33
      Height = 13
      Caption = 'Length'
    end
    object FrequencyValueLabel: TLabel
      Left = 72
      Top = 34
      Width = 50
      Height = 13
      Caption = 'Frequency'
    end
    object TypeValueLabel: TLabel
      Left = 72
      Top = 52
      Width = 24
      Height = 13
      Caption = 'Type'
    end
    object SizeValueLabel: TLabel
      Left = 72
      Top = 70
      Width = 20
      Height = 13
      Caption = 'Size'
    end
    object TrackBar: TTrackBar
      Left = 48
      Top = 116
      Width = 169
      Height = 25
      Max = 50
      Orientation = trHorizontal
      PageSize = 0
      Frequency = 0
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 0
      ThumbLength = 20
      TickMarks = tmBottomRight
      TickStyle = tsNone
      OnChange = TrackBarChange
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'wav'
    Filter = 'Wave (*.wav)|*.wav|All files (*.*)|*.*'
    Options = [ofOverwritePrompt]
    Left = 312
    Top = 80
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave (*.wav)|*.wav|All files (*.*)|*.*'
    Options = [ofFileMustExist]
    Left = 280
    Top = 80
  end
  object Timer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerTimer
    Left = 312
    Top = 112
  end
  object DXSound: TDXSound
    AutoInitialize = False
    Options = [soGlobalFocus, soExclusive]
    OnFinalize = DXSoundFinalize
    OnInitialize = DXSoundInitialize
    Left = 280
    Top = 112
  end
end
