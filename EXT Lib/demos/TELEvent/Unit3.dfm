object Form3: TForm3
  Left = 227
  Top = 145
  ActiveControl = Button2
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 256
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 224
    Top = 40
    Width = 29
    Height = 13
    Caption = 'Colors'
  end
  object Label2: TLabel
    Left = 344
    Top = 40
    Width = 99
    Height = 13
    Caption = 'Main form back color'
  end
  object Memo1: TMemo
    Left = 16
    Top = 8
    Width = 201
    Height = 153
    Lines.Strings = (
      'Sample memo field'
      'Sample memo field'
      'Sample memo field'
      'Sample memo field')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 224
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Change font'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 304
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 384
    Top = 224
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = Button3Click
  end
  object ColorGrid1: TColorGrid
    Left = 224
    Top = 56
    Width = 112
    Height = 104
    BackgroundIndex = 15
    TabOrder = 4
    OnChange = ColorGrid1Change
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 176
    Width = 193
    Height = 17
    Caption = 'Auto open new document on start'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 32
    Top = 200
    Width = 193
    Height = 17
    Caption = 'Show annotation in this document'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object ColorGrid2: TColorGrid
    Left = 344
    Top = 56
    Width = 112
    Height = 104
    BackgroundIndex = 7
    ForegroundEnabled = False
    TabOrder = 7
    OnChange = ColorGrid1Change
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 280
    Top = 168
  end
  object ELEventSender1: TELEventSender
    Left = 248
    Top = 168
  end
end
