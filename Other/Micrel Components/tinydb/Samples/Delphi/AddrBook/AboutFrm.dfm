object AboutForm: TAboutForm
  Left = 319
  Top = 272
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 156
  ClientWidth = 260
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 7
    Top = 3
    Width = 246
    Height = 112
    Enabled = False
    TabOrder = 0
    object Label1: TLabel
      Left = 17
      Top = 20
      Width = 86
      Height = 13
      Caption = 'Mini AddrBook 1.0'
    end
    object Memo1: TMemo
      Left = 15
      Top = 44
      Width = 222
      Height = 57
      BorderStyle = bsNone
      Color = clBtnFace
      Lines.Strings = (
        'Note: This is a sample for TinyDB.'
        ''
        'URL: http://www.tinydb.com'
        'Email: support@tinydb.com')
      ReadOnly = True
      TabOrder = 0
    end
  end
  object Button1: TButton
    Left = 178
    Top = 123
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
