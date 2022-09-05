object DelphiXSpriteEditForm: TDelphiXSpriteEditForm
  Left = 309
  Top = 201
  BorderStyle = bsDialog
  Caption = 'Sprite Init Editor'
  ClientHeight = 373
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 8
    Top = 8
    Width = 273
    Height = 361
    Shape = bsFrame
  end
  object OKButton: TButton
    Left = 288
    Top = 8
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 288
    Top = 40
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ClearButton: TButton
    Left = 288
    Top = 344
    Width = 73
    Height = 25
    Caption = '&Clear'
    TabOrder = 2
    Visible = False
  end
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 257
    Height = 345
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 3
    object LAlpha: TLabel
      Left = 8
      Top = 74
      Width = 69
      Height = 13
      Caption = 'Alpha (0..255):'
      FocusControl = EAlpha
    end
    object LAngle: TLabel
      Left = 8
      Top = 98
      Width = 69
      Height = 13
      Caption = 'Angle (0..255):'
      FocusControl = EAngle
    end
    object LAnimCount: TLabel
      Left = 23
      Top = 122
      Width = 54
      Height = 13
      Caption = 'AnimCount:'
      FocusControl = EAnimCount
    end
    object LAnimPos: TLabel
      Left = 33
      Top = 170
      Width = 44
      Height = 13
      Caption = 'AnimPos:'
      FocusControl = EAnimPos
    end
    object LAnimSpeed: TLabel
      Left = 20
      Top = 194
      Width = 57
      Height = 13
      Caption = 'AnimSpeed:'
      FocusControl = EAnimSpeed
    end
    object LAnimStart: TLabel
      Left = 29
      Top = 218
      Width = 48
      Height = 13
      Caption = 'AnimStart:'
      FocusControl = EAnimStart
    end
    object LHeight: TLabel
      Left = 43
      Top = 242
      Width = 34
      Height = 13
      Caption = 'Height:'
      FocusControl = EHeight
    end
    object LMapHeight: TLabel
      Left = 142
      Top = 99
      Width = 55
      Height = 13
      Caption = 'MapHeight:'
      FocusControl = EMapHeight
    end
    object LMapWidth: TLabel
      Left = 145
      Top = 123
      Width = 52
      Height = 13
      Caption = 'MapWidth:'
      FocusControl = EMapWidth
    end
    object LWidth: TLabel
      Left = 46
      Top = 266
      Width = 31
      Height = 13
      Caption = 'Width:'
      FocusControl = EWidth
    end
    object LX: TLabel
      Left = 187
      Top = 267
      Width = 10
      Height = 13
      Caption = 'X:'
      FocusControl = EX
    end
    object LY: TLabel
      Left = 187
      Top = 291
      Width = 10
      Height = 13
      Caption = 'Y:'
      FocusControl = EY
    end
    object LZ: TLabel
      Left = 187
      Top = 315
      Width = 10
      Height = 13
      Caption = 'Z:'
      FocusControl = EZ
    end
    object Label1: TLabel
      Left = 18
      Top = 50
      Width = 59
      Height = 13
      Caption = 'Blend mode:'
      FocusControl = EBlendMode
    end
    object Label2: TLabel
      Left = 8
      Top = 6
      Width = 83
      Height = 13
      Caption = 'Available Images:'
      FocusControl = eImage
    end
    object Label3: TLabel
      Left = 156
      Top = 51
      Width = 41
      Height = 13
      Caption = 'CenterX:'
      FocusControl = ECenterX
    end
    object Label4: TLabel
      Left = 156
      Top = 75
      Width = 41
      Height = 13
      Caption = 'CenterY:'
      FocusControl = ECenterY
    end
    object Label5: TLabel
      Left = 16
      Top = 315
      Width = 61
      Height = 13
      Caption = 'Texture filter:'
      FocusControl = ETexFilter
    end
    object EAlpha: TEdit
      Left = 80
      Top = 71
      Width = 48
      Height = 21
      TabOrder = 3
      Text = '255'
    end
    object EAngle: TEdit
      Left = 80
      Top = 95
      Width = 48
      Height = 21
      TabOrder = 4
      Text = '0'
    end
    object EAnimCount: TEdit
      Left = 80
      Top = 119
      Width = 48
      Height = 21
      TabOrder = 5
      Text = '0'
    end
    object EAnimPos: TEdit
      Left = 80
      Top = 167
      Width = 48
      Height = 21
      TabOrder = 7
      Text = '0'
    end
    object EAnimSpeed: TEdit
      Left = 80
      Top = 191
      Width = 48
      Height = 21
      TabOrder = 8
      Text = '0'
    end
    object EAnimStart: TEdit
      Left = 80
      Top = 215
      Width = 48
      Height = 21
      TabOrder = 9
      Text = '0'
    end
    object EHeight: TEdit
      Left = 80
      Top = 239
      Width = 48
      Height = 21
      TabOrder = 10
      Text = '0'
    end
    object EMapHeight: TEdit
      Left = 200
      Top = 96
      Width = 48
      Height = 21
      TabOrder = 15
      Text = '0'
    end
    object EMapWidth: TEdit
      Left = 200
      Top = 120
      Width = 48
      Height = 21
      TabOrder = 16
      Text = '0'
    end
    object EWidth: TEdit
      Left = 80
      Top = 263
      Width = 48
      Height = 21
      TabOrder = 11
      Text = '0'
    end
    object EX: TEdit
      Left = 200
      Top = 264
      Width = 48
      Height = 21
      TabOrder = 22
      Text = '0'
    end
    object EY: TEdit
      Left = 200
      Top = 288
      Width = 48
      Height = 21
      TabOrder = 23
      Text = '0'
    end
    object EZ: TEdit
      Left = 200
      Top = 312
      Width = 48
      Height = 21
      TabOrder = 24
      Text = '0'
    end
    object EBlendMode: TComboBox
      Left = 80
      Top = 47
      Width = 72
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'Draw'
        'Blend'
        'Add'
        'Sub')
    end
    object eImage: TComboBox
      Left = 8
      Top = 22
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = eImageChange
      OnExit = eImageExit
    end
    object ECenterX: TEdit
      Left = 200
      Top = 48
      Width = 48
      Height = 21
      TabOrder = 13
      Text = '0'
    end
    object ECenterY: TEdit
      Left = 200
      Top = 72
      Width = 48
      Height = 21
      TabOrder = 14
      Text = '0'
    end
    object chbBlurImage: TCheckBox
      Left = 59
      Top = 288
      Width = 69
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Blur Image'
      TabOrder = 12
    end
    object chbAnimLooped: TCheckBox
      Left = 48
      Top = 144
      Width = 80
      Height = 17
      Alignment = taLeftJustify
      Caption = 'AnimLooped'
      TabOrder = 6
    end
    object chbMoved: TCheckBox
      Left = 194
      Top = 144
      Width = 54
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Moved'
      TabOrder = 17
    end
    object chbCollisioned: TCheckBox
      Left = 177
      Top = 168
      Width = 71
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Collisioned'
      TabOrder = 18
    end
    object chbPixelCheck: TCheckBox
      Left = 175
      Top = 192
      Width = 73
      Height = 17
      Alignment = taLeftJustify
      Caption = 'PixelCheck'
      TabOrder = 19
    end
    object chbTile: TCheckBox
      Left = 209
      Top = 216
      Width = 39
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Tile'
      TabOrder = 20
    end
    object chbVisible: TCheckBox
      Left = 196
      Top = 240
      Width = 52
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Visible'
      TabOrder = 21
    end
    object btnMapEdit: TButton
      Left = 173
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Map Edit'
      Enabled = False
      TabOrder = 1
      OnClick = MapEditButtonClick
    end
    object ETexFilter: TComboBox
      Left = 80
      Top = 312
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 25
      Items.Strings = (
        'POINT'
        'LINEAR'
        'FLATCUBIC'
        'GAUSSIANCUBIC'
        'ANISOTROPIC')
    end
  end
end
