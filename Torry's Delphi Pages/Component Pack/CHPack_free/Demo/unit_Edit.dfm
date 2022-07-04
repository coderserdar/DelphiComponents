object frmCHEdit: TfrmCHEdit
  Left = 266
  Top = 274
  Width = 832
  Height = 412
  Caption = 'CH Edit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 30
    Width = 56
    Height = 13
    Caption = 'Lower Case'
  end
  object Label2: TLabel
    Left = 12
    Top = 84
    Width = 56
    Height = 13
    Caption = 'Upper Case'
  end
  object Label3: TLabel
    Left = 12
    Top = 138
    Width = 78
    Height = 13
    Caption = 'First Upper Case'
  end
  object Label4: TLabel
    Left = 168
    Top = 30
    Width = 61
    Height = 13
    Caption = 'Always Beep'
  end
  object Label5: TLabel
    Left = 168
    Top = 84
    Width = 70
    Height = 13
    Caption = 'No Enter Beep'
  end
  object Label21: TLabel
    Left = 168
    Top = 138
    Width = 125
    Height = 13
    Caption = 'Normal Edit Beep on Enter'
  end
  object Label6: TLabel
    Left = 168
    Top = 210
    Width = 135
    Height = 78
    Caption = 
      'This option is dependent '#13#10'from your Operating System.'#13#10#13#10'To emp' +
      'loy this option, make '#13#10'sure that the Beep-Sound is '#13#10'activated ' +
      'in your OS.'
  end
  object Label7: TLabel
    Left = 168
    Top = 192
    Width = 48
    Height = 13
    Caption = 'Caution:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label12: TLabel
    Left = 330
    Top = 246
    Width = 46
    Height = 13
    Caption = 'Key:  Strg'
  end
  object Label11: TLabel
    Left = 330
    Top = 192
    Width = 48
    Height = 13
    Caption = 'Key:  Shift'
  end
  object Label10: TLabel
    Left = 330
    Top = 138
    Width = 48
    Height = 13
    Caption = 'Key:  ESC'
  end
  object Label9: TLabel
    Left = 330
    Top = 84
    Width = 52
    Height = 13
    Caption = 'Key:  Enter'
  end
  object Label8: TLabel
    Left = 330
    Top = 30
    Width = 39
    Height = 13
    Caption = 'Key:  Alt'
  end
  object Label13: TLabel
    Left = 516
    Top = 30
    Width = 57
    Height = 13
    Caption = 'Only Integer'
  end
  object Label14: TLabel
    Left = 516
    Top = 84
    Width = 58
    Height = 13
    Caption = 'Only Double'
  end
  object Label15: TLabel
    Left = 684
    Top = 30
    Width = 44
    Height = 13
    Caption = 'Select All'
  end
  object Label16: TLabel
    Left = 684
    Top = 84
    Width = 52
    Height = 13
    Caption = 'Cursor First'
  end
  object Label17: TLabel
    Left = 684
    Top = 138
    Width = 53
    Height = 13
    Caption = 'Cursor Last'
  end
  object Label18: TLabel
    Left = 684
    Top = 186
    Width = 68
    Height = 13
    Caption = 'Custom Select'
  end
  object Label19: TLabel
    Left = 684
    Top = 246
    Width = 48
    Height = 13
    Caption = 'Caution:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label20: TLabel
    Left = 684
    Top = 264
    Width = 111
    Height = 78
    Caption = 
      'Only TAB key do this '#13#10'option !'#13#10#13#10'Focus with Mouse click'#13#10'touch' +
      ' the cursor to the'#13#10'Click-Position.'
  end
  object CHEdit1: TCHEdit
    Left = 12
    Top = 48
    Width = 121
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmLowerCase
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 0
    Text = 'hello world'
  end
  object CHEdit2: TCHEdit
    Left = 12
    Top = 102
    Width = 121
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmUppercase
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 1
    Text = 'HELLO WORLD'
  end
  object CHEdit3: TCHEdit
    Left = 12
    Top = 156
    Width = 121
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmFirstUpper
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 2
    Text = 'Hello world'
  end
  object CHEdit4: TCHEdit
    Left = 168
    Top = 48
    Width = 139
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebAlwaysBeep
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 3
  end
  object CHEdit5: TCHEdit
    Left = 168
    Top = 102
    Width = 139
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebNoEnterBeep
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 4
    Text = 'Do focus and press ENTER'
  end
  object CHEdit17: TCHEdit
    Left = 168
    Top = 156
    Width = 139
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 5
    Text = 'Do focus and press ENTER'
  end
  object CHEdit10: TCHEdit
    Left = 330
    Top = 264
    Width = 139
    Height = 21
    OnKeyStrgPress = CHEdit10KeyStrgPress
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = False
    Keys.Enter = False
    Keys.ESC = False
    Keys.Strg = True
    Keys.Shift = False
    TabOrder = 10
    Text = 'Do focus and press STRG'
  end
  object CHEdit9: TCHEdit
    Left = 330
    Top = 210
    Width = 139
    Height = 21
    OnKeyShiftPress = CHEdit9KeyShiftPress
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = False
    Keys.Enter = False
    Keys.ESC = False
    Keys.Strg = False
    Keys.Shift = True
    TabOrder = 9
    Text = 'Do focus and press SHIFT'
  end
  object CHEdit8: TCHEdit
    Left = 330
    Top = 156
    Width = 139
    Height = 21
    OnKeyESCPress = CHEdit8KeyESCPress
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = False
    Keys.Enter = False
    Keys.ESC = True
    Keys.Strg = False
    Keys.Shift = False
    TabOrder = 8
    Text = 'Do focus and press ESC'
  end
  object CHEdit7: TCHEdit
    Left = 330
    Top = 102
    Width = 139
    Height = 21
    OnKeyEnterPress = CHEdit7KeyEnterPress
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = False
    Keys.Enter = True
    Keys.ESC = False
    Keys.Strg = False
    Keys.Shift = False
    TabOrder = 7
    Text = 'Do focus and press ENTER'
  end
  object CHEdit6: TCHEdit
    Left = 330
    Top = 48
    Width = 139
    Height = 21
    OnKeyAltPress = CHEdit6KeyAltPress
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = False
    Keys.ESC = False
    Keys.Strg = False
    Keys.Shift = False
    TabOrder = 6
    Text = 'Do focus and press ALT'
  end
  object CHEdit11: TCHEdit
    Left = 516
    Top = 48
    Width = 121
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttInteger
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 11
    Text = '123'
  end
  object CHEdit12: TCHEdit
    Left = 516
    Top = 102
    Width = 121
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttDouble
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 12
    Text = '1.234,00'
  end
  object CHEdit13: TCHEdit
    Left = 684
    Top = 48
    Width = 121
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smAll
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 13
    Text = 'Focus select all'
  end
  object CHEdit14: TCHEdit
    Left = 684
    Top = 102
    Width = 121
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smFirst
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 14
    Text = 'Focus do cursor first'
  end
  object CHEdit15: TCHEdit
    Left = 684
    Top = 156
    Width = 121
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smLast
    SelectLength = 0
    SelectPos = 0
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 15
    Text = 'Focus do cursor last'
  end
  object CHEdit16: TCHEdit
    Left = 684
    Top = 204
    Width = 121
    Height = 21
    Alignment = taLeftJustify
    BeepOption = ebStandard
    CaseMode = cmNormal
    Focus = False
    Focuscolor = clInfoBk
    Mask = '#,##0.00'
    SelectMode = smCustom
    SelectLength = 6
    SelectPos = 6
    ShowCursor = True
    TextTyp = ttNormal
    Keys.Alt = True
    Keys.Enter = True
    Keys.ESC = True
    Keys.Strg = True
    Keys.Shift = True
    TabOrder = 16
    Text = 'Focus select  me'
  end
  object MainMenu1: TMainMenu
    Left = 750
    object close1: TMenuItem
      Caption = 'Close'
      OnClick = close1Click
    end
    object Info1: TMenuItem
      Caption = 'Info'
      OnClick = Info1Click
    end
  end
end
