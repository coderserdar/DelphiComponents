object cal_demo_frm_calpanel: Tcal_demo_frm_calpanel
  Left = 282
  Top = 127
  Width = 544
  Height = 560
  Caption = 'Calendar Panel'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel_CalPanel_Main: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 526
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel_Manager: TPanel
      Left = 337
      Top = 32
      Width = 199
      Height = 494
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Label_PanelKind: TLabel
        Left = 8
        Top = 13
        Width = 54
        Height = 13
        Caption = 'Panel Kind:'
      end
      object Bevel2: TBevel
        Left = 0
        Top = 0
        Width = 2
        Height = 494
        Align = alLeft
        Shape = bsLeftLine
      end
      object ComboBox_PanelKind: TComboBox
        Left = 88
        Top = 8
        Width = 104
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = ComboBox_PanelKindChange
      end
      object CheckBox_ShowDateEdit: TCheckBox
        Left = 8
        Top = 42
        Width = 185
        Height = 17
        Caption = 'Show Date Edit'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = CheckBox_ShowDateEditClick
      end
    end
    object Panel_CalendarPanel: TPanel
      Left = 0
      Top = 32
      Width = 337
      Height = 494
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object PSCCalendarPanel1: TPSCCalendarPanel
        Left = 0
        Top = 0
        Width = 337
        Height = 494
        Align = alClient
        FullRepaint = False
        TabOrder = 0
      end
    end
    object Panel_Buttons: TPanel
      Left = 0
      Top = 0
      Width = 536
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object Bevel1: TBevel
        Left = 0
        Top = 30
        Width = 536
        Height = 2
        Align = alBottom
        Shape = bsBottomLine
      end
    end
  end
end
