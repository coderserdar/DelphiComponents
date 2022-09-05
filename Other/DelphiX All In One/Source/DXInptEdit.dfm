object DelphiXInputEditForm: TDelphiXInputEditForm
  Left = 329
  Top = 188
  ActiveControl = OKButton
  BorderStyle = bsDialog
  Caption = 'TDXInput Editor'
  ClientHeight = 244
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PopupMenu = PopupMenu
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 208
    Top = 216
    Width = 84
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 296
    Top = 216
    Width = 84
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = CancelButtonClick
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 369
    Height = 201
    ActivePage = JoyTabSheet
    TabOrder = 2
    object JoyTabSheet: TTabSheet
      Caption = 'Joystick'
      object GroupBox2: TGroupBox
        Left = 8
        Top = 8
        Width = 345
        Height = 57
        Caption = 'General'
        TabOrder = 0
        object Label5: TLabel
          Left = 8
          Top = 22
          Width = 17
          Height = 13
          Caption = 'No.'
        end
        object Bevel1: TBevel
          Left = 138
          Top = 15
          Width = 2
          Height = 31
        end
        object IDEdit: TSpinEdit
          Left = 48
          Top = 18
          Width = 81
          Height = 22
          MaxValue = 255
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = EditChange
        end
        object AutoCenter: TCheckBox
          Left = 152
          Top = 12
          Width = 185
          Height = 17
          Caption = 'Centering of stick'
          TabOrder = 1
          OnClick = EditChange
        end
        object ForceFeedback: TCheckBox
          Left = 152
          Top = 31
          Width = 153
          Height = 17
          Caption = 'Force feedback'
          TabOrder = 2
          OnClick = EditChange
        end
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 72
        Width = 153
        Height = 97
        Caption = 'Dead zone'
        TabOrder = 1
        object Label7: TLabel
          Left = 8
          Top = 23
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object Label8: TLabel
          Left = 8
          Top = 47
          Width = 10
          Height = 13
          Caption = 'Y:'
        end
        object Label9: TLabel
          Left = 8
          Top = 71
          Width = 10
          Height = 13
          Caption = 'Z:'
        end
        object Label11: TLabel
          Left = 83
          Top = 24
          Width = 51
          Height = 13
          Caption = '% invalidity'
        end
        object Label12: TLabel
          Left = 83
          Top = 48
          Width = 51
          Height = 13
          Caption = '% invalidity'
        end
        object Label13: TLabel
          Left = 83
          Top = 72
          Width = 51
          Height = 13
          Caption = '% invalidity'
        end
        object DeadZoneZ: TSpinEdit
          Left = 32
          Top = 68
          Width = 49
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = EditChange
        end
        object DeadZoneY: TSpinEdit
          Left = 32
          Top = 44
          Width = 49
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = EditChange
        end
        object DeadZoneX: TSpinEdit
          Left = 32
          Top = 20
          Width = 49
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = EditChange
        end
      end
      object GroupBox4: TGroupBox
        Left = 168
        Top = 72
        Width = 153
        Height = 97
        Caption = 'Range'
        TabOrder = 2
        object Label4: TLabel
          Left = 8
          Top = 23
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object Label6: TLabel
          Left = 8
          Top = 47
          Width = 10
          Height = 13
          Caption = 'Y:'
        end
        object Label10: TLabel
          Left = 8
          Top = 71
          Width = 10
          Height = 13
          Caption = 'Z:'
        end
        object RangeZ: TSpinEdit
          Left = 30
          Top = 68
          Width = 73
          Height = 22
          MaxValue = 1000000
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = EditChange
        end
        object RangeY: TSpinEdit
          Left = 30
          Top = 44
          Width = 73
          Height = 22
          MaxValue = 1000000
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = EditChange
        end
        object RangeX: TSpinEdit
          Left = 30
          Top = 20
          Width = 73
          Height = 22
          MaxValue = 1000000
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = EditChange
        end
      end
    end
    object KeyTabSheet: TTabSheet
      Caption = 'Keyboard'
      object GroupBox1: TGroupBox
        Left = 8
        Top = 8
        Width = 345
        Height = 161
        Caption = 'Key assign'
        TabOrder = 0
        object Label3: TLabel
          Left = 166
          Top = 92
          Width = 27
          Height = 13
          Caption = 'Key 3'
        end
        object Label2: TLabel
          Left = 166
          Top = 60
          Width = 27
          Height = 13
          Caption = 'Key 2'
        end
        object Label1: TLabel
          Left = 166
          Top = 28
          Width = 27
          Height = 13
          Caption = 'Key 1'
        end
        object StateListBox: TListBox
          Left = 8
          Top = 24
          Width = 143
          Height = 129
          ItemHeight = 13
          TabOrder = 0
          OnClick = StateListBoxClick
        end
        object KeyComboBox1: TComboBox
          Left = 200
          Top = 24
          Width = 129
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 1
          OnChange = EditChange
        end
        object KeyComboBox2: TComboBox
          Left = 200
          Top = 56
          Width = 129
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 2
          OnChange = EditChange
        end
        object KeyComboBox3: TComboBox
          Left = 200
          Top = 88
          Width = 129
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 3
          OnChange = EditChange
        end
      end
    end
  end
  object PopupMenu: TPopupMenu
    Left = 8
    Top = 216
    object Player1: TMenuItem
      Caption = '&Single player'
      OnClick = Player1Click
    end
    object Player2_1: TMenuItem
      Caption = 'Multi player &1'
      OnClick = Player2_1Click
    end
    object Player2_2: TMenuItem
      Caption = 'Multi player &2'
      OnClick = Player2_2Click
    end
  end
end
