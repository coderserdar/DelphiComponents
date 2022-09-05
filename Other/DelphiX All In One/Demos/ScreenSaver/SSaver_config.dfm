object SConfig: TSConfig
  Left = 188
  Top = 111
  BorderStyle = bsDialog
  Caption = 'Dancing Lights Config'
  ClientHeight = 279
  ClientWidth = 469
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnCreate = FormCreate
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 104
    Width = 53
    Height = 13
    Caption = 'Resolution:'
    Transparent = True
    OnMouseMove = FormMouseMove
  end
  object Label2: TLabel
    Left = 224
    Top = 104
    Width = 48
    Height = 13
    Caption = 'Coded by:'
    Transparent = True
    OnMouseMove = FormMouseMove
  end
  object mail: TLabel
    Left = 280
    Top = 104
    Width = 84
    Height = 13
    Cursor = crHandPoint
    Caption = 'Slavisa Milojkovic'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    OnClick = mailClick
    OnMouseMove = mailMouseMove
  end
  object Image: TImage
    Left = 0
    Top = 0
    Width = 465
    Height = 97
    Stretch = True
    OnMouseMove = FormMouseMove
  end
  object res: TComboBox
    Left = 64
    Top = 104
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Items.Strings = (
      '640x480'
      '800x600'
      '1024x768')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 104
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 1
    OnClick = Button1Click
    OnMouseMove = FormMouseMove
  end
  object Button2: TButton
    Left = 192
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 2
    OnClick = Button2Click
    OnMouseMove = FormMouseMove
  end
  object Button3: TButton
    Left = 280
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button3Click
    OnMouseMove = FormMouseMove
  end
  object GroupBox: TGroupBox
    Left = 8
    Top = 136
    Width = 185
    Height = 89
    Caption = ' Number of lights '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnMouseMove = FormMouseMove
    object num: TSpinEdit
      Left = 16
      Top = 24
      Width = 41
      Height = 22
      MaxLength = 2
      MaxValue = 10
      MinValue = 1
      TabOrder = 0
      Value = 1
      OnMouseMove = FormMouseMove
    end
    object rand: TCheckBox
      Left = 16
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Random number'
      TabOrder = 1
      OnClick = randClick
      OnMouseMove = FormMouseMove
    end
  end
  object back: TCheckBox
    Left = 208
    Top = 208
    Width = 121
    Height = 17
    Caption = 'Draw background'
    TabOrder = 5
    OnClick = backClick
  end
end
