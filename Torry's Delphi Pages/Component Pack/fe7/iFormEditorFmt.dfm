object FormEditorFmt: TFormEditorFmt
  Left = 225
  Top = 78
  BorderStyle = bsToolWindow
  Caption = 'Form editor properties'
  ClientHeight = 396
  ClientWidth = 276
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
  object BitBtn1: TBitBtn
    Left = 8
    Top = 368
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 88
    Top = 368
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object BitBtn3: TBitBtn
    Left = 168
    Top = 368
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkHelp
  end
  object ZAL: TPageControl
    Left = 0
    Top = 0
    Width = 276
    Height = 361
    ActivePage = SH_OPTIONS
    Align = alTop
    TabOrder = 3
    object SH_EDITOR: TTabSheet
      Caption = 'Editor'
      object Label1: TLabel
        Left = 8
        Top = 136
        Width = 67
        Height = 13
        Caption = 'Color selected'
      end
      object Label2: TLabel
        Left = 8
        Top = 160
        Width = 76
        Height = 13
        Caption = 'Color multiselect'
      end
      object Label3: TLabel
        Left = 8
        Top = 216
        Width = 168
        Height = 13
        Caption = 'Pen for select, move, size rectangle'
      end
      object Label7: TLabel
        Left = 104
        Top = 8
        Width = 57
        Height = 13
        Caption = 'Handle type'
      end
      object Label18: TLabel
        Left = 8
        Top = 184
        Width = 65
        Height = 13
        Caption = 'Shadow color'
      end
      object GB_GRID: TGroupBox
        Left = 3
        Top = 32
        Width = 265
        Height = 93
        Caption = 'Grid'
        TabOrder = 0
        object Label4: TLabel
          Left = 8
          Top = 24
          Width = 154
          Height = 13
          Caption = 'Horizontal space between points'
        end
        object Label5: TLabel
          Left = 8
          Top = 48
          Width = 142
          Height = 13
          Caption = 'Vertical space between points'
        end
        object Label6: TLabel
          Left = 8
          Top = 72
          Width = 65
          Height = 13
          Caption = 'Grid pint color'
        end
        object GRID_X: TEdit
          Left = 192
          Top = 16
          Width = 65
          Height = 21
          TabOrder = 0
          Text = 'GRID_X'
        end
        object GRID_Y: TEdit
          Left = 192
          Top = 40
          Width = 65
          Height = 21
          TabOrder = 1
          Text = 'GRID_Y'
        end
        object GRID_COLOR: TpsColorComboBox
          Left = 120
          Top = 67
          Width = 137
          Height = 21
          ItemIndex = 0
          TabOrder = 2
          TypeOfBox = tyColor
          ItemOffset = 0
        end
      end
      object GridVisible: TCheckBox
        Left = 8
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Grid visible'
        TabOrder = 1
      end
      object ColorSelected: TpsColorComboBox
        Left = 128
        Top = 128
        Width = 137
        Height = 21
        ItemIndex = 0
        TabOrder = 2
        TypeOfBox = tyColor
        ItemOffset = 0
      end
      object ColorMultiSelect: TpsColorComboBox
        Left = 128
        Top = 152
        Width = 137
        Height = 21
        ItemIndex = 0
        TabOrder = 3
        TypeOfBox = tyColor
        ItemOffset = 0
      end
      object LinesPen: TpsPenBrushEditor
        Left = 3
        Top = 232
        Width = 265
        Height = 101
        TabOrder = 4
        OffsetLabels = 5
        OffsetControls = 118
        VerticalOffset = 24
        BoldStyle = True
        EditorType = etPenEditor
      end
      object CB_HANDLETYPE: TComboBox
        Left = 176
        Top = 8
        Width = 92
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 5
        Items.Strings = (
          'htStandard'
          'htSquare'
          'ht3DRect'
          'hrCircle'
          'htRectAngle')
      end
      object ColorShadow: TpsColorComboBox
        Left = 128
        Top = 176
        Width = 137
        Height = 21
        ItemIndex = 0
        TabOrder = 6
        TypeOfBox = tyColor
        ItemOffset = 0
      end
    end
    object SH_OPTIONS: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      object CL_OPTIONS: TCheckListBox
        Left = 3
        Top = 18
        Width = 265
        Height = 279
        ItemHeight = 13
        Items.Strings = (
          'Click on the form clear selection'
          'Simple multiselect'
          'Adjust move to grid'
          'Confirm delete'
          'Move or size direct'
          'Hint window when move or size change'
          'Standard property editor'
          'Enable move outside parent'
          'Use auto popup menu'
          'Save changes before open new file'
          'Accept keys (DEL, INSERT, UP, DOWN ...)'
          'Show popup objects menu after pressing INS key'
          'Dbl click on the form -> Show editor properties'
          'Show option icons')
        TabOrder = 0
      end
    end
    object SH_HINT: TTabSheet
      Caption = 'Hint'
      ImageIndex = 2
      object Label8: TLabel
        Left = 8
        Top = 8
        Width = 257
        Height = 49
        AutoSize = False
        Caption = 
          'For using this feature you must enable "Hint window when move or' +
          ' size change" on Options page too.'
        WordWrap = True
      end
      object Label9: TLabel
        Left = 8
        Top = 72
        Width = 45
        Height = 13
        Caption = 'Hint color'
      end
      object Label10: TLabel
        Left = 8
        Top = 104
        Width = 40
        Height = 13
        Caption = 'Hint font'
      end
      object Label15: TLabel
        Left = 8
        Top = 144
        Width = 23
        Height = 13
        Caption = 'Style'
      end
      object Shape1: TShape
        Left = 8
        Top = 176
        Width = 257
        Height = 3
      end
      object Label16: TLabel
        Left = 8
        Top = 200
        Width = 46
        Height = 13
        Caption = 'Undo limit'
      end
      object Label17: TLabel
        Left = 8
        Top = 232
        Width = 106
        Height = 13
        Caption = 'Memory currently used'
      end
      object Undo_Memory: TLabel
        Left = 136
        Top = 232
        Width = 102
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Undo_Memory'
      end
      object Label19: TLabel
        Left = 8
        Top = 256
        Width = 83
        Height = 13
        Caption = 'Undo items count'
      end
      object Undo_Items: TLabel
        Left = 136
        Top = 256
        Width = 102
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Undo_Items'
      end
      object HintColor: TpsColorComboBox
        Left = 120
        Top = 64
        Width = 145
        Height = 21
        ItemIndex = 0
        TabOrder = 0
        TypeOfBox = tyColor
        ItemOffset = 0
      end
      object Hint_Font: TBitBtn
        Left = 120
        Top = 96
        Width = 145
        Height = 25
        Caption = 'Change font'
        TabOrder = 1
        OnClick = Hint_FontClick
      end
      object Hint_Style: TComboBox
        Left = 120
        Top = 136
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          'Left top'
          'Left bottom'
          'Right top'
          'Right bottom'
          'Float')
      end
      object UndoMax: TEdit
        Left = 120
        Top = 192
        Width = 121
        Height = 21
        TabOrder = 3
      end
    end
    object SH_RULER: TTabSheet
      Caption = 'Ruler'
      ImageIndex = 3
      object RULER_PNL: TPanel
        Left = 0
        Top = 24
        Width = 265
        Height = 305
        TabOrder = 0
        object Label11: TLabel
          Left = 16
          Top = 120
          Width = 48
          Height = 13
          Caption = 'Line width'
        end
        object Label12: TLabel
          Left = 16
          Top = 144
          Width = 62
          Height = 13
          Caption = 'Start from (%)'
        end
        object Label13: TLabel
          Left = 16
          Top = 168
          Width = 59
          Height = 13
          Caption = 'End ruler (%)'
        end
        object Label14: TLabel
          Left = 16
          Top = 192
          Width = 23
          Height = 13
          Caption = 'Style'
        end
        object RULER_LEFT: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Left'
          TabOrder = 0
        end
        object RULER_TOP: TCheckBox
          Left = 16
          Top = 32
          Width = 97
          Height = 17
          Caption = 'Top'
          TabOrder = 1
        end
        object RULER_RIGHT: TCheckBox
          Left = 16
          Top = 48
          Width = 97
          Height = 17
          Caption = 'Right'
          TabOrder = 2
        end
        object RULER_BOTTOM: TCheckBox
          Left = 16
          Top = 64
          Width = 97
          Height = 17
          Caption = 'Bottom'
          TabOrder = 3
        end
        object RULER_NUMBERS: TCheckBox
          Left = 16
          Top = 240
          Width = 97
          Height = 17
          Caption = 'Show numbers'
          TabOrder = 4
        end
        object RULER_LINEWIDTH: TEdit
          Left = 192
          Top = 120
          Width = 50
          Height = 21
          TabOrder = 5
          Text = 'RULER_LINEWIDTH'
        end
        object RULER_START: TEdit
          Left = 192
          Top = 144
          Width = 50
          Height = 21
          TabOrder = 6
          Text = 'RULER_START'
        end
        object RULER_END: TEdit
          Left = 192
          Top = 168
          Width = 50
          Height = 21
          TabOrder = 7
          Text = 'RULER_END'
        end
        object RULER_STYLE: TComboBox
          Left = 16
          Top = 208
          Width = 241
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 8
          Items.Strings = (
            'Only full marks (1cm, 2cm, 3cm ...)'
            'Full and half marks (1cm, 1.5cm, 2cm, ...)'
            'Decimal (1cm, 3small poits, 1.5 cm, 3small poits, 2cm...) ')
        end
        object RULER_PEN: TpsPenBrushEditor
          Left = 96
          Top = 8
          Width = 169
          Height = 101
          TabOrder = 9
          OffsetLabels = 5
          OffsetControls = 60
          VerticalOffset = 24
          BoldStyle = True
          EditorType = etPenEditor
        end
        object RULER_FONT: TButton
          Left = 144
          Top = 232
          Width = 113
          Height = 25
          Caption = 'Font'
          TabOrder = 10
          OnClick = RULER_FONTClick
        end
      end
      object E_RULER_VISIBLE: TCheckBox
        Left = 0
        Top = 0
        Width = 97
        Height = 17
        Caption = 'Visible'
        TabOrder = 1
        OnClick = E_RULER_VISIBLEClick
      end
    end
  end
end
