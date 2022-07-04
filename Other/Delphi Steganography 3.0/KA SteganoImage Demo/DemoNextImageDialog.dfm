object NextImageForm: TNextImageForm
  Left = 416
  Top = 364
  Width = 439
  Height = 133
  Caption = 'Please choose next image for processing'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 8
    Width = 84
    Height = 13
    Caption = '&Select next image'
    FocusControl = Edit1
  end
  object Label2: TLabel
    Left = 14
    Top = 24
    Width = 327
    Height = 13
    Caption = 
      '................................................................' +
      '.............................................'
  end
  object Edit1: TEdit
    Left = 14
    Top = 41
    Width = 331
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 350
    Top = 40
    Width = 75
    Height = 25
    Caption = '&Browse >>'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 350
    Top = 71
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 270
    Top = 71
    Width = 75
    Height = 25
    Caption = 'Abort'
    TabOrder = 3
    OnClick = Button3Click
  end
end
