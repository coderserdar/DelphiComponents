object DelphiXMidiEditForm: TDelphiXMidiEditForm
  Left = 288
  Top = 239
  BorderStyle = bsDialog
  Caption = 'Midi Editor'
  ClientHeight = 226
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
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
    object LengthLabel: TLabel
      Left = 16
      Top = 16
      Width = 19
      Height = 13
      Caption = 'File:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object SizeLabel: TLabel
      Left = 16
      Top = 38
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
    object filenamelabel: TLabel
      Left = 72
      Top = 16
      Width = 42
      Height = 13
      Caption = 'Filename'
    end
    object SizeValueLabel: TLabel
      Left = 72
      Top = 38
      Width = 20
      Height = 13
      Caption = 'Size'
    end
  end
  object btnPlay: TBitBtn
    Left = 111
    Top = 128
    Width = 25
    Height = 25
    TabOrder = 6
    OnClick = btnPlayClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      7777777777777777777777777777777777777777077777777777777700077777
      7777777700000777777777770000000777777777000000000777777700000007
      7777777700000777777777770007777777777777077777777777777777777777
      7777777777777777777777777777777777777777777777777777}
  end
  object btnStop: TBitBtn
    Left = 136
    Top = 128
    Width = 25
    Height = 25
    TabOrder = 7
    OnClick = btnStopClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      7777777777777777777777777777777777777777777777777777777700000000
      7777777700000000777777770000000077777777000000007777777700000000
      7777777700000000777777770000000077777777000000007777777777777777
      7777777777777777777777777777777777777777777777777777}
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'mid'
    Filter = 'Midi (*.mid)|*.mid|All files (*.*)|*.*'
    Options = [ofOverwritePrompt]
    Title = 'Save MIDI file.'
    Left = 312
    Top = 80
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'mid'
    Filter = 'Midi (*.mid)|*.mid|All files (*.*)|*.*'
    Options = [ofFileMustExist]
    Title = 'Load MIDI file.'
    Left = 280
    Top = 80
  end
  object DXSound1: TDXSound
    AutoInitialize = True
    Options = []
    Left = 272
    Top = 120
  end
  object DXMusic1: TDXMusic
    DXSound = DXSound1
    Midis = <>
    Left = 304
    Top = 120
  end
end
