object psc_frm_setup_preferences: Tpsc_frm_setup_preferences
  Left = 109
  Top = 104
  Align = alClient
  BorderStyle = bsDialog
  Caption = 'Setup preferences'
  ClientHeight = 480
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object Panel_AdditionPreferences: TPanel
    Left = 471
    Top = 5
    Width = 428
    Height = 393
    BevelOuter = bvNone
    TabOrder = 0
    object Label_WeekDayCase: TLabel
      Left = 8
      Top = 100
      Width = 81
      Height = 13
      Caption = 'Week Day Case:'
    end
    object Label_DayVertAlign: TLabel
      Left = 8
      Top = 127
      Width = 70
      Height = 13
      Caption = 'Day Vert Align:'
    end
    object Lable_DayHorzAlign: TLabel
      Left = 8
      Top = 154
      Width = 73
      Height = 13
      Caption = 'Day Horz Align:'
    end
    object Label_SelectKind: TLabel
      Left = 8
      Top = 10
      Width = 57
      Height = 13
      Caption = 'Select Kind:'
    end
    object Label_HeaderCase: TLabel
      Left = 8
      Top = 43
      Width = 65
      Height = 13
      Caption = 'Header Case:'
    end
    object Label_WeekDayNames: TLabel
      Left = 8
      Top = 70
      Width = 85
      Height = 13
      Caption = 'Week Day Name:'
    end
    object Lable_DayHeaderHorzAlign: TLabel
      Left = 8
      Top = 181
      Width = 86
      Height = 13
      Caption = 'Day Header Align:'
    end
    object ComboBox_WeekDayCase: TComboBox
      Left = 111
      Top = 96
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnChange = ComboBox_WeekDayCaseChange
    end
    object ComboBox_DayVertAlign: TComboBox
      Left = 111
      Top = 123
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnChange = ComboBox_DayVertAlignChange
    end
    object ComboBox_DayHorzAlign: TComboBox
      Left = 111
      Top = 150
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      OnChange = ComboBox_DayHorzAlignChange
    end
    object CheckBox_ShowFocusRect: TCheckBox
      Left = 248
      Top = 128
      Width = 193
      Height = 17
      Caption = 'Show Focus Rect'
      TabOrder = 10
      OnClick = CheckBox_ShowFocusRectClick
    end
    object CheckBox_ShowHeader: TCheckBox
      Left = 248
      Top = 152
      Width = 193
      Height = 17
      Caption = 'Show Header'
      TabOrder = 11
      OnClick = CheckBox_ShowHeaderClick
    end
    object CheckBox_ShowFooter: TCheckBox
      Left = 248
      Top = 176
      Width = 193
      Height = 17
      Caption = 'Show Footer'
      TabOrder = 12
      OnClick = CheckBox_ShowFooterClick
    end
    object ComboBox_SelectKind: TComboBox
      Left = 111
      Top = 6
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboBox_SelectKindChange
    end
    object ComboBox_HeaderCase: TComboBox
      Left = 111
      Top = 39
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = ComboBox_HeaderCaseChange
    end
    object ComboBox_WeekDayNames: TComboBox
      Left = 111
      Top = 66
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      OnChange = ComboBox_WeekDayNamesChange
    end
    object CheckBox_ShowArrowEdges: TCheckBox
      Left = 248
      Top = 8
      Width = 193
      Height = 17
      Caption = 'Show Arrow Edges'
      TabOrder = 6
      OnClick = CheckBox_ShowArrowEdgesClick
    end
    object CheckBox_NavigatuionButtons: TCheckBox
      Left = 248
      Top = 80
      Width = 193
      Height = 17
      Caption = 'Show Navigation Buttons'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = CheckBox_NavigatuionButtonsClick
    end
    object CheckBox_MonthDividers: TCheckBox
      Left = 248
      Top = 104
      Width = 193
      Height = 17
      Caption = 'Show Month Dividers'
      TabOrder = 9
      OnClick = CheckBox_MonthDividersClick
    end
    object CheckBox_ShowMonthPopup: TCheckBox
      Left = 248
      Top = 200
      Width = 193
      Height = 17
      Caption = 'Show Year Popup'
      Checked = True
      State = cbChecked
      TabOrder = 13
      OnClick = CheckBox_ShowMonthPopupClick
    end
    object CheckBox_NavButtonsTimer: TCheckBox
      Left = 248
      Top = 56
      Width = 193
      Height = 17
      Caption = 'Use Navigation Buttons Timer'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = CheckBox_NavButtonsTimerClick
    end
    object ComboBox_DayHeaderHorzAlign: TComboBox
      Left = 111
      Top = 177
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 14
      OnChange = ComboBox_DayHeaderHorzAlignChange
    end
    object CheckBox_WeekNumbers: TCheckBox
      Left = 248
      Top = 32
      Width = 193
      Height = 17
      Caption = 'Show Week Numbers'
      TabOrder = 15
      OnClick = CheckBox_WeekNumbersClick
    end
  end
  object Panel_Preferences: TPanel
    Left = 0
    Top = 0
    Width = 457
    Height = 387
    BevelOuter = bvNone
    TabOrder = 1
    object Label_CalendarStyle: TLabel
      Left = 8
      Top = 10
      Width = 71
      Height = 13
      Caption = 'Calendar Style:'
    end
    object Label_PopupType: TLabel
      Left = 8
      Top = 196
      Width = 60
      Height = 13
      Caption = 'Popup Style:'
    end
    object LabelArrowStyle: TLabel
      Left = 8
      Top = 86
      Width = 56
      Height = 13
      Caption = 'Arrow Style:'
    end
    object Label_SelectionLength: TLabel
      Left = 8
      Top = 169
      Width = 82
      Height = 13
      Caption = 'Selection Range:'
    end
    object Label_SelectStyle: TLabel
      Left = 8
      Top = 113
      Width = 59
      Height = 13
      Caption = 'Select Style:'
    end
    object Label_TodayStyle: TLabel
      Left = 8
      Top = 139
      Width = 59
      Height = 13
      Caption = 'Today Style:'
    end
    object Label_CalInHeigth: TLabel
      Left = 8
      Top = 226
      Width = 96
      Height = 13
      Caption = 'Calendars In Height:'
    end
    object Label_CalInWidth: TLabel
      Left = 8
      Top = 252
      Width = 93
      Height = 13
      Caption = 'Calendars In Width:'
    end
    object Label_FirstWeekOfYear: TLabel
      Left = 8
      Top = 277
      Width = 91
      Height = 13
      Caption = 'First Week of Year:'
    end
    object Label_FirstDayOfWeek: TLabel
      Left = 8
      Top = 302
      Width = 88
      Height = 13
      Caption = 'First Day of Week:'
    end
    object CheckBox_MultiSelect: TCheckBox
      Left = 248
      Top = 8
      Width = 188
      Height = 17
      Caption = 'Allow Multi Select'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = CheckBox_MultiSelectClick
    end
    object ComboBox_CalendarStyle: TComboBox
      Left = 111
      Top = 6
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboBox_CalendarStyleChange
    end
    object CheckBox_MaxDateLimit: TCheckBox
      Left = 8
      Top = 61
      Width = 95
      Height = 17
      Caption = 'Limit Max Date:'
      TabOrder = 3
      OnClick = CheckBox_MaxDateLimitClick
    end
    object CheckBox_MinDateLimit: TCheckBox
      Left = 8
      Top = 35
      Width = 95
      Height = 17
      Caption = 'Limit Min Date:'
      TabOrder = 1
      OnClick = CheckBox_MinDateLimitClick
    end
    object PSCDateEdit_MinDate: TPSCDateEdit
      Left = 111
      Top = 31
      Width = 118
      Height = 21
      Date = 37405.000000000000000000
      Kind = cpkDate
      Enabled = False
      TabOrder = 2
      OnChange = PSCDateEdit_MinDateChange
    end
    object PSCDateEdit_MaxDate: TPSCDateEdit
      Left = 111
      Top = 57
      Width = 118
      Height = 21
      Date = 37405.000000000000000000
      Kind = cpkDate
      Enabled = False
      TabOrder = 4
      OnChange = PSCDateEdit_MaxDateChange
    end
    object ComboBox_PopupType: TComboBox
      Left = 111
      Top = 193
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 9
      OnChange = ComboBox_PopupTypeChange
    end
    object CheckBox_Flat: TCheckBox
      Left = 248
      Top = 104
      Width = 188
      Height = 17
      Caption = 'Flat'
      Checked = True
      State = cbChecked
      TabOrder = 14
      OnClick = CheckBox_FlatClick
    end
    object CheckBox_KeyMoving: TCheckBox
      Left = 248
      Top = 32
      Width = 188
      Height = 17
      Caption = 'Non Persistent Selection'
      Checked = True
      State = cbChecked
      TabOrder = 11
      OnClick = CheckBox_KeyMovingClick
    end
    object CheckBox_ReadOnly: TCheckBox
      Left = 248
      Top = 248
      Width = 188
      Height = 17
      Caption = 'ReadOnly'
      TabOrder = 20
      OnClick = CheckBox_ReadOnlyClick
    end
    object CheckBox_ShowToday: TCheckBox
      Left = 248
      Top = 128
      Width = 188
      Height = 17
      Caption = 'Circle Today Date'
      Checked = True
      State = cbChecked
      TabOrder = 15
      OnClick = CheckBox_ShowTodayClick
    end
    object ComboBox_ArrowStyle: TComboBox
      Left = 111
      Top = 82
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      OnChange = ComboBox_ArrowStyleChange
    end
    object CheckBox_SideClickSelectsWeek: TCheckBox
      Left = 248
      Top = 80
      Width = 188
      Height = 17
      Caption = 'Side Click Selects Week'
      Checked = True
      State = cbChecked
      TabOrder = 13
      OnClick = CheckBox_SideClickSelectsWeekClick
    end
    object CheckBox_ExtendedSelect: TCheckBox
      Left = 248
      Top = 56
      Width = 188
      Height = 17
      Caption = 'Extended Select'
      Checked = True
      State = cbChecked
      TabOrder = 12
      OnClick = CheckBox_ExtendedSelectClick
    end
    object CheckBox_ShortMonth: TCheckBox
      Left = 248
      Top = 224
      Width = 188
      Height = 17
      Caption = 'Use Short Month Names'
      TabOrder = 19
      OnClick = CheckBox_ShortMonthClick
    end
    object PSCEdit_SelectionLength: TPSCEdit
      Left = 111
      Top = 165
      Width = 118
      Height = 21
      BtnKind = bkUpDown
      ButtonsVisible = True
      TabOrder = 8
      Text = '42'
      OnButtonClick = PSCEdit_SelectionLengthButtonClick
      OnChange = PSCEdit_SelectionLengthChange
      OnKeyPress = PSCEdit_SelectionLengthKeyPress
    end
    object GroupBox_WorkDays: TGroupBox
      Left = 8
      Top = 334
      Width = 433
      Height = 41
      Caption = ' Calendar Work Week '
      TabOrder = 21
      object CheckBox_WorkDay3: TCheckBox
        Left = 130
        Top = 16
        Width = 50
        Height = 17
        Caption = 'Wed'
        TabOrder = 2
        OnClick = CheckBox_WorkDay3Click
      end
      object CheckBox_WorkDay1: TCheckBox
        Left = 16
        Top = 16
        Width = 50
        Height = 17
        Caption = 'Mon'
        TabOrder = 0
        OnClick = CheckBox_WorkDay1Click
      end
      object CheckBox_WorkDay2: TCheckBox
        Left = 73
        Top = 16
        Width = 50
        Height = 17
        Caption = 'Tue'
        TabOrder = 1
        OnClick = CheckBox_WorkDay2Click
      end
      object CheckBox_WorkDay4: TCheckBox
        Left = 188
        Top = 16
        Width = 50
        Height = 17
        Caption = 'Thu'
        TabOrder = 3
        OnClick = CheckBox_WorkDay4Click
      end
      object CheckBox_WorkDay5: TCheckBox
        Left = 245
        Top = 16
        Width = 50
        Height = 17
        Caption = 'Fri'
        TabOrder = 4
        OnClick = CheckBox_WorkDay5Click
      end
      object CheckBox_WorkDay6: TCheckBox
        Left = 302
        Top = 16
        Width = 50
        Height = 17
        Caption = 'Sat'
        TabOrder = 5
        OnClick = CheckBox_WorkDay6Click
      end
      object CheckBox_WorkDay7: TCheckBox
        Left = 360
        Top = 16
        Width = 50
        Height = 17
        Caption = 'Sun'
        TabOrder = 6
        OnClick = CheckBox_WorkDay7Click
      end
    end
    object ComboBox_SelectStyle: TComboBox
      Left = 111
      Top = 109
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 6
      OnChange = ComboBox_SelectStyleChange
    end
    object ComboBox_TodayStyle: TComboBox
      Left = 111
      Top = 135
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 7
      OnChange = ComboBox_TodayStyleChange
    end
    object CheckBox_WantEsc: TCheckBox
      Left = 248
      Top = 152
      Width = 188
      Height = 17
      Caption = 'Want Esc'
      TabOrder = 16
      OnClick = CheckBox_WantEscClick
    end
    object CheckBox_PastDaysAsGrayed: TCheckBox
      Left = 248
      Top = 176
      Width = 188
      Height = 17
      Caption = 'Past Days as Grayed'
      TabOrder = 17
      OnClick = CheckBox_PastDaysAsGrayedClick
    end
    object CheckBox_AlterEvenOdd: TCheckBox
      Left = 248
      Top = 200
      Width = 188
      Height = 17
      Caption = 'Alter Even Odd'
      TabOrder = 18
      OnClick = CheckBox_AlterEvenOddClick
    end
    object PSCEdit_CalendarsInHeight: TPSCEdit
      Left = 111
      Top = 222
      Width = 118
      Height = 21
      BtnKind = bkUpDown
      ButtonsVisible = True
      TabOrder = 22
      Text = ''
      OnButtonClick = PSCEdit_CalendarsInHeightButtonClick
      OnChange = PSCEdit_CalendarsInHeightChange
      OnKeyPress = PSCEdit_SelectionLengthKeyPress
    end
    object PSCEditCalendarsInWidth: TPSCEdit
      Left = 111
      Top = 248
      Width = 118
      Height = 21
      BtnKind = bkUpDown
      ButtonsVisible = True
      TabOrder = 23
      Text = ''
      OnButtonClick = PSCEditCalendarsInWidthButtonClick
      OnChange = PSCEditCalendarsInWidthChange
      OnKeyPress = PSCEdit_SelectionLengthKeyPress
    end
    object ComboBox_FirstWeekOfYear: TComboBox
      Left = 111
      Top = 273
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 24
      OnChange = ComboBox_FirstWeekOfYearChange
    end
    object ComboBox_FirstDayOfWeek: TComboBox
      Left = 111
      Top = 298
      Width = 118
      Height = 21
      Style = csDropDownList
      TabOrder = 25
      OnChange = ComboBox_FirstDayOfWeekChange
      Items.Strings = (
        'Default')
    end
    object CheckBox_ShowVertLines: TCheckBox
      Left = 248
      Top = 272
      Width = 193
      Height = 17
      Caption = 'Show Vertical Lines'
      TabOrder = 26
      OnClick = CheckBox_ShowVertLinesClick
    end
    object CheckBox_ShowHorzLines: TCheckBox
      Left = 248
      Top = 296
      Width = 193
      Height = 17
      Caption = 'Show Horizontal Lines'
      TabOrder = 27
      OnClick = CheckBox_ShowHorzLinesClick
    end
  end
  object PSCCalendar_Preferences: TPSCCalendar
    Left = 304
    Top = 408
    Width = 313
    Height = 65
    TabOrder = 2
    Visible = False
    StartDate = 37438.000000000000000000
    Colors.WeekNumbersFont.Name = 'Tahoma'
    Colors.WeekNumbersFont.Size = 6
    Colors.WeekDaysFont.Name = 'Tahoma'
    Colors.HeaderFont.Name = 'Tahoma'
    Colors.DaysFont.Name = 'Tahoma'
    Colors.HolidaysFont.Name = 'Tahoma'
  end
end
