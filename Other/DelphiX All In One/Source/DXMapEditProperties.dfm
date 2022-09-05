object DelphiXMapEditPropertiesForm: TDelphiXMapEditPropertiesForm
  Left = 575
  Top = 327
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Chip property'
  ClientHeight = 265
  ClientWidth = 131
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 131
    Height = 265
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 0
    object LAlpha: TLabel
      Left = 10
      Top = 193
      Width = 74
      Height = 13
      Caption = 'Alpha (0..255):'
      FocusControl = EAlpha
    end
    object LAnimCount: TLabel
      Left = 21
      Top = 106
      Width = 63
      Height = 13
      Caption = 'Anim. Count:'
      FocusControl = EAnimCount
    end
    object LAnimSpeed: TLabel
      Left = 20
      Top = 125
      Width = 64
      Height = 13
      Caption = 'Anim. Speed:'
      FocusControl = EAnimSpeed
    end
    object LAnimStart: TLabel
      Left = 26
      Top = 68
      Width = 58
      Height = 13
      Caption = 'Anim. Start:'
      FocusControl = EAnimStart
    end
    object LAnimPos: TLabel
      Left = 33
      Top = 87
      Width = 51
      Height = 13
      Caption = 'Anim. Pos:'
      FocusControl = EAnimPos
    end
    object EAlpha: TEdit
      Left = 84
      Top = 190
      Width = 43
      Height = 18
      AutoSize = False
      TabOrder = 0
      Text = '255'
    end
    object EAnimCount: TEdit
      Left = 84
      Top = 103
      Width = 43
      Height = 18
      AutoSize = False
      TabOrder = 1
      Text = '0'
    end
    object EAnimSpeed: TEdit
      Left = 84
      Top = 122
      Width = 43
      Height = 18
      AutoSize = False
      TabOrder = 2
      Text = '0'
    end
    object EAnimStart: TEdit
      Left = 84
      Top = 65
      Width = 43
      Height = 18
      AutoSize = False
      TabOrder = 3
      Text = '0'
    end
    object EAnimPos: TEdit
      Left = 84
      Top = 84
      Width = 43
      Height = 18
      AutoSize = False
      TabOrder = 4
      Text = '0'
    end
    object Panel2: TPanel
      Left = 2
      Top = 2
      Width = 127
      Height = 23
      Align = alTop
      BevelOuter = bvLowered
      BorderWidth = 2
      Caption = 'Chip (%d,%d)'
      Color = 16776176
      TabOrder = 5
    end
    object chbCollisioned: TCheckBox
      Left = 16
      Top = 31
      Width = 97
      Height = 17
      Caption = 'Collisioned Chip'
      TabOrder = 6
    end
    object chbAnimated: TCheckBox
      Left = 16
      Top = 46
      Width = 105
      Height = 17
      Caption = 'Animation On/Off'
      TabOrder = 7
    end
    object rgBlendMode: TRadioGroup
      Left = 3
      Top = 141
      Width = 124
      Height = 47
      Caption = ' Blend mode: '
      Columns = 2
      Items.Strings = (
        'Draw'
        'Blend'
        'Add'
        'Sub')
      TabOrder = 8
    end
    object btnOK: TBitBtn
      Left = 4
      Top = 244
      Width = 56
      Height = 19
      Caption = 'OK'
      Default = True
      TabOrder = 9
      OnClick = btnOKClick
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000E0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        77007777777777777700777774F7777777007777444F777777007774444F7777
        770077444F44F77777007444F7744F777700774F77774F7777007777777774F7
        770077777777774F7700777777777774F7007777777777774700777777777777
        7700}
      Spacing = -1
    end
    object btnCancel: TBitBtn
      Left = 68
      Top = 244
      Width = 56
      Height = 19
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 10
      Visible = False
      OnClick = btnCancelClick
      Glyph.Data = {
        EE000000424DEE000000000000007600000028000000100000000F0000000100
        0400000000007800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777777777777777770F77770F7777777777777000F7777770F7777000F777770
        F777777000F77700F7777777000F700F77777777700000F77777777777000F77
        77777777700000F777777777000F70F77777770000F77700F77770000F777770
        0F77700F7777777700F77777777777777777}
      Spacing = -1
    end
    object GroupBox1: TGroupBox
      Left = 3
      Top = 208
      Width = 124
      Height = 33
      Caption = ' Flip/Mirror: '
      TabOrder = 11
      object chbFlip: TCheckBox
        Left = 8
        Top = 13
        Width = 38
        Height = 17
        Caption = 'Flip'
        TabOrder = 0
      end
      object chbMirror: TCheckBox
        Left = 56
        Top = 13
        Width = 57
        Height = 17
        Caption = 'Mirror'
        TabOrder = 1
      end
    end
  end
end
