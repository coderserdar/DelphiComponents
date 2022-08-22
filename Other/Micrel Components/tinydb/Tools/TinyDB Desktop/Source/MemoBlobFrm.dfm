object MemoBlobForm: TMemoBlobForm
  Left = 192
  Top = 133
  Caption = 'MemoBlobForm'
  ClientHeight = 208
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 341
    Height = 174
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object Memo: TMemo
      Left = 6
      Top = 6
      Width = 329
      Height = 162
      Align = alClient
      TabOrder = 0
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 174
    Width = 341
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 267
      Top = 1
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object Button1: TButton
      Left = 179
      Top = 1
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
