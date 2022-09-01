object psc_frm_setup: Tpsc_frm_setup
  Left = 289
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Automatic Formatting'
  ClientHeight = 310
  ClientWidth = 379
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel_Buttons: TPanel
    Left = 0
    Top = 274
    Width = 379
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button_Ok: TButton
      Left = 133
      Top = 5
      Width = 75
      Height = 23
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button_Cancel: TButton
      Left = 214
      Top = 5
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Button_Apply: TButton
      Left = 295
      Top = 5
      Width = 75
      Height = 23
      Caption = 'Apply'
      TabOrder = 2
      OnClick = Button_ApplyClick
    end
  end
  object Panel_Setup: TPanel
    Left = 0
    Top = 129
    Width = 379
    Height = 145
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object GroupBox_Properties: TGroupBox
      Left = 10
      Top = 1
      Width = 360
      Height = 142
      Caption = 'Properties of selected item'
      TabOrder = 0
      object Label_Name: TLabel
        Left = 8
        Top = 20
        Width = 31
        Height = 13
        Caption = 'Name:'
        Enabled = False
      end
      object RadioGroup_Format: TRadioGroup
        Left = 228
        Top = 12
        Width = 122
        Height = 62
        Caption = 'Format'
        Enabled = False
        ItemIndex = 0
        Items.Strings = (
          'All fields'
          'Selected fields')
        TabOrder = 9
        OnClick = RadioGroup_FormatClick
      end
      object PSCColorEdit1: TPSCColorEdit
        Left = 80
        Top = 45
        Width = 137
        Height = 21
        OnChange = PSCColorEdit1Change
        Options = [cboShowDefaultNone, cboShowBkgndColors, cboShowMoreColors]
        Enabled = False
        TabOrder = 2
      end
      object BitBtn_Condition: TBitBtn
        Left = 228
        Top = 110
        Width = 122
        Height = 23
        Caption = 'Condition...'
        TabOrder = 11
        OnClick = BitBtn_ConditionClick
      end
      object BitBtn_SelectFields: TBitBtn
        Left = 228
        Top = 82
        Width = 122
        Height = 23
        Caption = 'Select Fields...'
        Enabled = False
        TabOrder = 10
        OnClick = BitBtn_SelectFieldsClick
      end
      object PSCEdit_Name: TPSCEdit
        Left = 80
        Top = 16
        Width = 137
        Height = 21
        TabOrder = 0
        Text = ''
        OnChange = PSCEdit_NameChange
      end
      object PSCColorEdit2: TPSCColorEdit
        Left = 80
        Top = 74
        Width = 137
        Height = 21
        OnChange = PSCColorEdit2Change
        Options = [cboShowFontColors, cboShowMoreColors]
        Enabled = False
        TabOrder = 4
      end
      object CheckBox_Bold: TCheckBox
        Left = 8
        Top = 102
        Width = 68
        Height = 17
        AllowGrayed = True
        Caption = 'Bold'
        Enabled = False
        TabOrder = 5
        OnClick = CheckBox_BoldClick
      end
      object CheckBox_Italic: TCheckBox
        Left = 96
        Top = 102
        Width = 68
        Height = 17
        AllowGrayed = True
        Caption = 'Italic'
        Enabled = False
        TabOrder = 6
        OnClick = CheckBox_ItalicClick
      end
      object CheckBox_Underline: TCheckBox
        Left = 8
        Top = 118
        Width = 68
        Height = 17
        AllowGrayed = True
        Caption = 'Underline'
        Enabled = False
        TabOrder = 7
        OnClick = CheckBox_UnderlineClick
      end
      object CheckBox_StrikeOut: TCheckBox
        Left = 96
        Top = 118
        Width = 68
        Height = 17
        AllowGrayed = True
        Caption = 'Strike Out'
        Enabled = False
        TabOrder = 8
        OnClick = CheckBox_StrikeOutClick
      end
      object CheckBox_UseColor: TCheckBox
        Left = 8
        Top = 47
        Width = 70
        Height = 17
        Caption = 'Color:'
        TabOrder = 1
        OnClick = CheckBox_UseColorClick
      end
      object CheckBox_UseTextColor: TCheckBox
        Left = 8
        Top = 76
        Width = 70
        Height = 17
        Caption = 'Text Color:'
        TabOrder = 3
        OnClick = CheckBox_UseTextColorClick
      end
    end
  end
  object Panel_Main: TPanel
    Left = 0
    Top = 0
    Width = 379
    Height = 129
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PSCListBox_RecordsInfo: TPSCListBox
      Left = 8
      Top = 8
      Width = 277
      Height = 114
      BorderStyle = bsNone
      BevelInner = bvLowered
      BevelOuter = bvRaised
      BevelKind = bkFlat
      Options = [fboCanIndent, fboLookupDSOpen, lboAlwaysShowSelection, fboAutoSizePopups]
      HideFocus = False
      CheckBoxes = True
      TabOrder = 0
      OnClick = PSCListBox_RecordsInfoClick
    end
    object Panel2: TPanel
      Left = 287
      Top = 0
      Width = 92
      Height = 129
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Button_Add: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 23
        Caption = 'Add'
        TabOrder = 0
        OnClick = Button_AddClick
      end
      object Button_Delete: TButton
        Left = 8
        Top = 34
        Width = 75
        Height = 23
        Caption = 'Delete'
        Enabled = False
        TabOrder = 1
        OnClick = Button_DeleteClick
      end
      object Button_MoveUp: TButton
        Left = 8
        Top = 72
        Width = 75
        Height = 23
        Caption = 'Move Up'
        Enabled = False
        TabOrder = 2
        OnClick = Button_MoveUpClick
      end
      object Button_MoveDown: TButton
        Left = 8
        Top = 99
        Width = 75
        Height = 23
        Caption = 'Move Down'
        Enabled = False
        TabOrder = 3
        OnClick = Button_MoveDownClick
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 48
    Top = 8
  end
  object PSCFltDlg1: TPSCFltDlg
    HideApplyButton = False
    Left = 16
    Top = 8
  end
end
