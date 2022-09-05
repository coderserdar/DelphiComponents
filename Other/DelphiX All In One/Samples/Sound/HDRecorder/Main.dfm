object MainForm: TMainForm
  Left = 203
  Top = 108
  BorderStyle = bsDialog
  Caption = 'HD Recoder'
  ClientHeight = 110
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SizeLabel: TLabel
    Left = 96
    Top = 88
    Width = 29
    Height = 13
    Caption = '0 byte'
  end
  object StartButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Start'
    TabOrder = 0
    OnClick = StartButtonClick
  end
  object StopButton: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'S&top'
    Enabled = False
    TabOrder = 1
    OnClick = StopButtonClick
  end
  object FileNameEdit: TEdit
    Left = 96
    Top = 56
    Width = 201
    Height = 21
    TabOrder = 2
    Text = 'c:\test.wav'
  end
  object FormatBox: TComboBox
    Left = 96
    Top = 32
    Width = 273
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
  end
  object DriverBox: TComboBox
    Left = 96
    Top = 8
    Width = 273
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnChange = DriverBoxChange
  end
  object BrowseButton: TButton
    Left = 304
    Top = 56
    Width = 65
    Height = 25
    Caption = 'Browse'
    TabOrder = 5
    OnClick = BrowseButtonClick
  end
  object CloseButton: TButton
    Left = 8
    Top = 72
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 6
    OnClick = CloseButtonClick
  end
  object SaveDialog: TSaveDialog
    Filter = 'Wave (*.wav)|*.wav|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist]
    Left = 144
    Top = 48
  end
end
