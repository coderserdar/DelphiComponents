object PaperModelSetupFmt: TPaperModelSetupFmt
  Left = 335
  Top = 20
  BorderStyle = bsDialog
  Caption = 'Paper model properties'
  ClientHeight = 412
  ClientWidth = 339
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
  object ZAL: TPageControl
    Left = 0
    Top = 0
    Width = 339
    Height = 381
    ActivePage = SH_BASIC
    Align = alClient
    MultiLine = True
    TabOrder = 0
    object SH_BASIC: TTabSheet
      Caption = 'Basic properties'
      object Label10: TLabel
        Left = 16
        Top = 24
        Width = 54
        Height = 13
        Caption = 'Shape type'
      end
      object PG: TPageControl
        Left = 8
        Top = 64
        Width = 321
        Height = 281
        ActivePage = SH_STANDARD
        TabOrder = 1
        object SH_STANDARD: TTabSheet
          Caption = 'SH_STANDARD'
          TabVisible = False
          object Label1: TLabel
            Left = 16
            Top = 80
            Width = 56
            Height = 13
            Caption = 'Paper width'
            FocusControl = E_PAPERWIDTH
          end
          object Label2: TLabel
            Left = 16
            Top = 104
            Width = 60
            Height = 13
            Caption = 'Paper height'
            FocusControl = E_PAPERHEIGHT
          end
          object Label3: TLabel
            Left = 16
            Top = 128
            Width = 54
            Height = 13
            Caption = 'Label width'
            FocusControl = E_LABELWIDTH
          end
          object Label4: TLabel
            Left = 16
            Top = 152
            Width = 58
            Height = 13
            Caption = 'Label height'
            FocusControl = E_LABELHEIGHT
          end
          object Label5: TLabel
            Left = 16
            Top = 176
            Width = 52
            Height = 13
            Caption = 'Left margin'
            FocusControl = E_MARGINLEFT
          end
          object Label6: TLabel
            Left = 16
            Top = 200
            Width = 53
            Height = 13
            Caption = 'Top margin'
            FocusControl = E_MARGINTOP
          end
          object Label7: TLabel
            Left = 16
            Top = 224
            Width = 153
            Height = 13
            Caption = 'Horizontal space between labels'
            FocusControl = E_SPACEX
          end
          object Label8: TLabel
            Left = 16
            Top = 248
            Width = 141
            Height = 13
            Caption = 'Vertical space between labels'
            FocusControl = E_SPACEY
          end
          object Label19: TLabel
            Left = 16
            Top = 32
            Width = 70
            Height = 13
            Caption = 'Producer code'
            FocusControl = E_ProducerCode
          end
          object Label20: TLabel
            Left = 16
            Top = 56
            Width = 72
            Height = 13
            Caption = 'Producer name'
            FocusControl = E_ProducerName
          end
          object Label21: TLabel
            Left = 16
            Top = 8
            Width = 55
            Height = 13
            Caption = 'Label name'
            FocusControl = E_LabelName
          end
          object E_PAPERWIDTH: TEdit
            Left = 245
            Top = 72
            Width = 60
            Height = 21
            BorderStyle = bsNone
            TabOrder = 3
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_PAPERHEIGHT: TEdit
            Left = 245
            Top = 96
            Width = 60
            Height = 21
            BorderStyle = bsNone
            TabOrder = 4
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_LABELWIDTH: TEdit
            Left = 245
            Top = 120
            Width = 60
            Height = 21
            BorderStyle = bsNone
            TabOrder = 5
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_LABELHEIGHT: TEdit
            Left = 245
            Top = 144
            Width = 60
            Height = 21
            BorderStyle = bsNone
            TabOrder = 6
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_MARGINLEFT: TEdit
            Left = 245
            Top = 168
            Width = 60
            Height = 21
            BorderStyle = bsNone
            TabOrder = 7
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_MARGINTOP: TEdit
            Left = 245
            Top = 192
            Width = 60
            Height = 21
            BorderStyle = bsNone
            TabOrder = 8
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_SPACEX: TEdit
            Left = 245
            Top = 216
            Width = 60
            Height = 21
            BorderStyle = bsNone
            TabOrder = 9
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_SPACEY: TEdit
            Left = 245
            Top = 240
            Width = 60
            Height = 21
            BorderStyle = bsNone
            TabOrder = 10
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_ProducerCode: TEdit
            Left = 112
            Top = 24
            Width = 193
            Height = 21
            BorderStyle = bsNone
            TabOrder = 1
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_ProducerName: TEdit
            Left = 112
            Top = 48
            Width = 193
            Height = 21
            BorderStyle = bsNone
            TabOrder = 2
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
          object E_LabelName: TEdit
            Left = 112
            Top = 0
            Width = 193
            Height = 21
            BorderStyle = bsNone
            TabOrder = 0
            OnEnter = E_PAPERWIDTHEnter
            OnExit = E_PAPERWIDTHExit
          end
        end
        object SH_NOSTANDARD: TTabSheet
          Caption = 'SH_NOSTANDARD'
          ImageIndex = 1
          TabVisible = False
          object SpeedButton1: TSpeedButton
            Left = 2
            Top = 3
            Width = 100
            Height = 22
            Caption = 'Add'
            Flat = True
            OnClick = SpeedButton1Click
          end
          object SpeedButton2: TSpeedButton
            Left = 210
            Top = 3
            Width = 100
            Height = 22
            Caption = 'Delete all'
            Flat = True
            OnClick = SpeedButton2Click
          end
          object SpeedButton3: TSpeedButton
            Left = 106
            Top = 3
            Width = 100
            Height = 22
            Caption = 'Delete'
            Flat = True
            OnClick = SpeedButton3Click
          end
          object GR: TStringGrid
            Left = 0
            Top = 32
            Width = 313
            Height = 239
            Align = alBottom
            BorderStyle = bsNone
            DefaultRowHeight = 16
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
            TabOrder = 0
            ColWidths = (
              64
              64
              64
              64
              64)
          end
        end
      end
      object E_LABELGRID: TCheckBox
        Left = 16
        Top = 48
        Width = 161
        Height = 17
        Caption = 'Stardard label paper'
        TabOrder = 0
        OnClick = E_LABELGRIDClick
      end
      object E_SHAPETYPE: TpsColorComboBox
        Left = 160
        Top = 16
        Width = 145
        Height = 22
        TabOrder = 2
        TypeOfBox = tyShape
        SelectedColor = clBlack
        SelectedBrushImage = -1
        SelectedShape = stRectangle
        SelectedPenStyle = psSolid
        SelectedCopyMode = 0
        SelectedPenMode = pmBlack
        UserColor = clBlack
        ItemStyle = scBoth
        SelectedDrive = 'R'
        SelectedImage = 0
        ItemHeight = 16
        ItemIndex = 0
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Colors'
      ImageIndex = 4
      object Label11: TLabel
        Left = 8
        Top = 16
        Width = 176
        Height = 13
        Caption = 'Visible and unused label, free for print'
        FocusControl = E_LabelBrush
      end
      object Label12: TLabel
        Left = 8
        Top = 104
        Width = 110
        Height = 13
        Caption = 'Label unusable for print'
        FocusControl = E_UsedBrush
      end
      object Label9: TLabel
        Left = 8
        Top = 208
        Width = 54
        Height = 13
        Caption = 'Paper color'
        FocusControl = E_PaperColor
      end
      object Label17: TLabel
        Left = 8
        Top = 248
        Width = 65
        Height = 13
        Caption = 'Shadow color'
        FocusControl = E_ShadowColor
      end
      object Label18: TLabel
        Left = 8
        Top = 296
        Width = 67
        Height = 13
        Caption = 'Shadow width'
        FocusControl = E_ShadowSize
      end
      object E_LabelBrush: TpsPenBrushEditor
        Left = 8
        Top = 32
        Width = 185
        Height = 53
        TabOrder = 0
        OnEnter = E_PAPERWIDTHEnter
        OnExit = E_PAPERWIDTHExit
        OffsetLabels = 5
        OffsetControls = 80
        VerticalOffset = 24
        BoldStyle = True
        EditorType = etBrushEditor
      end
      object E_UsedBrush: TpsPenBrushEditor
        Left = 8
        Top = 120
        Width = 185
        Height = 53
        TabOrder = 1
        OnEnter = E_PAPERWIDTHEnter
        OnExit = E_PAPERWIDTHExit
        OffsetLabels = 5
        OffsetControls = 80
        VerticalOffset = 24
        BoldStyle = True
        EditorType = etBrushEditor
      end
      object E_PaperColor: TpsColorComboBox
        Left = 8
        Top = 224
        Width = 177
        Height = 22
        TabOrder = 2
        TypeOfBox = tyColor
        SelectedColor = clBlack
        SelectedBrushImage = -1
        SelectedShape = stRectangle
        SelectedPenStyle = psSolid
        SelectedCopyMode = 0
        SelectedPenMode = pmBlack
        UserColor = clBlack
        ItemStyle = scBoth
        SelectedDrive = 'U'
        SelectedImage = 0
        OnEnter = E_PAPERWIDTHEnter
        OnExit = E_PAPERWIDTHExit
        ItemHeight = 16
        ItemIndex = 0
      end
      object E_ShadowColor: TpsColorComboBox
        Left = 8
        Top = 264
        Width = 145
        Height = 22
        TabOrder = 3
        TypeOfBox = tyColor
        SelectedColor = clBlack
        SelectedBrushImage = -1
        SelectedShape = stRectangle
        SelectedPenStyle = psSolid
        SelectedCopyMode = 0
        SelectedPenMode = pmBlack
        UserColor = clBlack
        ItemStyle = scBoth
        SelectedDrive = 'U'
        SelectedImage = 0
        OnEnter = E_PAPERWIDTHEnter
        OnExit = E_PAPERWIDTHExit
        ItemHeight = 16
        ItemIndex = 0
      end
      object E_ShadowSize: TEdit
        Left = 96
        Top = 288
        Width = 41
        Height = 21
        TabOrder = 4
        OnEnter = E_PAPERWIDTHEnter
        OnExit = E_PAPERWIDTHExit
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Misc.'
      ImageIndex = 2
      object E_FocusPen: TpsPenBrushEditor
        Left = 8
        Top = 32
        Width = 185
        Height = 101
        TabOrder = 1
        OnEnter = E_PAPERWIDTHEnter
        OnExit = E_PAPERWIDTHExit
        OffsetLabels = 5
        OffsetControls = 80
        VerticalOffset = 24
        BoldStyle = True
        EditorType = etPenEditor
      end
      object E_FocusEnabled: TCheckBox
        Left = 8
        Top = 14
        Width = 121
        Height = 17
        Caption = 'Use focus label'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object E_CutLinesPen: TpsPenBrushEditor
        Left = 8
        Top = 168
        Width = 185
        Height = 101
        TabOrder = 3
        OnEnter = E_PAPERWIDTHEnter
        OnExit = E_PAPERWIDTHExit
        OffsetLabels = 5
        OffsetControls = 80
        VerticalOffset = 24
        BoldStyle = True
        EditorType = etPenEditor
      end
      object E_CutLines: TCheckBox
        Left = 8
        Top = 151
        Width = 97
        Height = 16
        Caption = 'Use cut lines'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
      end
      object E_ClickMode: TRadioGroup
        Left = 8
        Top = 272
        Width = 185
        Height = 57
        Caption = 'Click action'
        Items.Strings = (
          'Change visible '
          'Change free for print')
        TabOrder = 4
      end
    end
    object SH_ABOUT: TTabSheet
      Caption = 'About'
      ImageIndex = 3
      object Label22: TLabel
        Left = 8
        Top = 24
        Width = 321
        Height = 201
        AutoSize = False
        Caption = 'Label22'
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 381
    Width = 339
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 8
      Top = 3
      Width = 103
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 120
      Top = 3
      Width = 103
      Height = 25
      Caption = '&Apply'
      Default = True
      TabOrder = 1
      OnClick = BitBtn2Click
      NumGlyphs = 2
    end
    object BitBtn3: TBitBtn
      Left = 232
      Top = 3
      Width = 103
      Height = 25
      TabOrder = 2
      Kind = bkCancel
    end
  end
end
