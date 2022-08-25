object SysLogClientForm: TSysLogClientForm
  Left = 70
  Top = 450
  Width = 415
  Height = 477
  Caption = 'SysLog Client - http://www.overbyte.be'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TShape
    Left = 317
    Top = 86
    Width = 5
    Height = 2
    Brush.Color = clBlack
  end
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 407
    Height = 297
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 13
      Top = 247
      Width = 85
      Height = 13
      Caption = 'Destination server'
    end
    object Label2: TLabel
      Left = 35
      Top = 8
      Width = 63
      Height = 13
      Caption = 'Message text'
    end
    object Label3: TLabel
      Left = 66
      Top = 31
      Width = 32
      Height = 13
      Caption = 'Facility'
    end
    object Label4: TLabel
      Left = 60
      Top = 55
      Width = 38
      Height = 13
      Caption = 'Severity'
    end
    object Label5: TLabel
      Left = 31
      Top = 199
      Width = 67
      Height = 13
      Caption = 'Process name'
    end
    object Label6: TLabel
      Left = 19
      Top = 223
      Width = 79
      Height = 13
      Caption = 'Process ID (PID)'
    end
    object Label7: TLabel
      Left = 50
      Top = 175
      Width = 48
      Height = 13
      Caption = 'Hostname'
    end
    object Shape1: TShape
      Left = 295
      Top = 175
      Width = 2
      Height = 63
      Brush.Color = clBlack
    end
    object Shape2: TShape
      Left = 297
      Top = 205
      Width = 5
      Height = 2
      Brush.Color = clBlack
    end
    object Shape4: TShape
      Left = 295
      Top = 32
      Width = 2
      Height = 36
      Brush.Color = clBlack
    end
    object Shape3: TShape
      Left = 297
      Top = 50
      Width = 5
      Height = 2
      Brush.Color = clBlack
    end
    object Label8: TLabel
      Left = 5
      Top = 127
      Width = 93
      Height = 13
      Caption = 'Day / Month / Year'
    end
    object Shape5: TShape
      Left = 296
      Top = 145
      Width = 6
      Height = 2
      Brush.Color = clBlack
    end
    object Shape6: TShape
      Left = 295
      Top = 126
      Width = 2
      Height = 40
      Brush.Color = clBlack
    end
    object Label9: TLabel
      Left = 128
      Top = 128
      Width = 5
      Height = 13
      Caption = '/'
    end
    object Label10: TLabel
      Left = 44
      Top = 151
      Width = 54
      Height = 13
      Caption = 'HH:MM:SS'
    end
    object Label11: TLabel
      Left = 128
      Top = 151
      Width = 3
      Height = 13
      Caption = ':'
    end
    object Label12: TLabel
      Left = 160
      Top = 151
      Width = 3
      Height = 13
      Caption = ':'
    end
    object Label13: TLabel
      Left = 158
      Top = 128
      Width = 5
      Height = 13
      Caption = '/'
    end
    object Label14: TLabel
      Left = 41
      Top = 78
      Width = 57
      Height = 13
      Caption = 'Message ID'
    end
    object Label15: TLabel
      Left = 23
      Top = 102
      Width = 75
      Height = 13
      Caption = 'Structured Data'
    end
    object Shape7: TShape
      Left = 297
      Top = 97
      Width = 5
      Height = 2
      Brush.Color = clBlack
    end
    object Shape8: TShape
      Left = 295
      Top = 79
      Width = 2
      Height = 36
      Brush.Color = clBlack
    end
    object TextEdit: TEdit
      Left = 104
      Top = 5
      Width = 269
      Height = 21
      TabOrder = 0
      Text = 'TextEdit'
    end
    object FacilityComboBox: TComboBox
      Left = 104
      Top = 28
      Width = 188
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      Text = 'FacilityComboBox'
    end
    object SeverityComboBox: TComboBox
      Left = 104
      Top = 52
      Width = 188
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      Text = 'SeverityComboBox'
    end
    object ProcessNameEdit: TEdit
      Left = 104
      Top = 196
      Width = 188
      Height = 21
      Hint = 'Alphanumeric characters, no space allowed.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      Text = 'ProcessNameEdit'
    end
    object ProcessIDEdit: TEdit
      Left = 104
      Top = 220
      Width = 188
      Height = 21
      Hint = 'Alphanumeric characters, no space allowed.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      Text = 'ProcessIDEdit'
    end
    object ServerEdit: TEdit
      Left = 104
      Top = 244
      Width = 188
      Height = 21
      TabOrder = 18
      Text = 'ServerEdit'
    end
    object ProcessMySelfCheckBox: TCheckBox
      Left = 306
      Top = 198
      Width = 65
      Height = 17
      Caption = 'MySelf'
      Checked = True
      State = cbChecked
      TabOrder = 17
      OnClick = ProcessMySelfCheckBoxClick
    end
    object SendButton: TButton
      Left = 298
      Top = 270
      Width = 75
      Height = 21
      Caption = 'Send'
      Default = True
      TabOrder = 19
      OnClick = SendButtonClick
    end
    object HostnameEdit: TEdit
      Left = 104
      Top = 172
      Width = 188
      Height = 21
      Hint = 'Alphanumeric characters, no space allowed.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      Text = 'HostnameEdit'
    end
    object DefaultPRICheckBox: TCheckBox
      Left = 306
      Top = 43
      Width = 65
      Height = 17
      Caption = 'Default'
      TabOrder = 3
      OnClick = DefaultPRICheckBoxClick
    end
    object NowCheckBox: TCheckBox
      Left = 306
      Top = 137
      Width = 65
      Height = 17
      Caption = 'Now'
      TabOrder = 13
      OnClick = NowCheckBoxClick
    end
    object MonthEdit: TEdit
      Left = 136
      Top = 124
      Width = 21
      Height = 21
      TabOrder = 8
      Text = 'MonthEdit'
    end
    object DayEdit: TEdit
      Left = 104
      Top = 124
      Width = 21
      Height = 21
      TabOrder = 7
      Text = 'DayEdit'
    end
    object HourEdit: TEdit
      Left = 105
      Top = 148
      Width = 21
      Height = 21
      TabOrder = 10
      Text = 'HourEdit'
    end
    object MinEdit: TEdit
      Left = 135
      Top = 148
      Width = 21
      Height = 21
      TabOrder = 11
      Text = 'MinEdit'
    end
    object SecEdit: TEdit
      Left = 166
      Top = 148
      Width = 21
      Height = 21
      TabOrder = 12
      Text = 'SecEdit'
    end
    object YearEdit: TEdit
      Left = 166
      Top = 124
      Width = 35
      Height = 21
      TabOrder = 9
      Text = 'YearEdit'
    end
    object MsgIDEdit: TEdit
      Left = 104
      Top = 75
      Width = 188
      Height = 21
      Hint = 'Alphanumeric characters, no space allowed.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'MsgIDEdit'
    end
    object StructDataEdit: TEdit
      Left = 104
      Top = 99
      Width = 188
      Height = 21
      Hint = 
        'Structured data must be enclosed in '#39'[]'#39'. Doiuble quote, backsla' +
        'sh and  closing bracket in values must be escaped by prefixing w' +
        'ith a backslash.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'StructDataEdit'
    end
    object RFCPanel: TPanel
      Left = 308
      Top = 75
      Width = 77
      Height = 45
      BevelOuter = bvNone
      TabOrder = 6
      object RFC3164RadioButton: TRadioButton
        Left = 0
        Top = 7
        Width = 70
        Height = 17
        Caption = 'RFC 3164'
        TabOrder = 0
        OnClick = RFC3164RadioButtonClick
      end
      object RFC5424RadioButton: TRadioButton
        Left = 0
        Top = 23
        Width = 70
        Height = 17
        Caption = 'RFC 5424'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = RFC5424RadioButtonClick
      end
    end
    object SetDefaultButton: TButton
      Left = 104
      Top = 270
      Width = 188
      Height = 21
      Caption = 'Set above to default test values'
      TabOrder = 20
      OnClick = SetDefaultButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 297
    Width = 407
    Height = 146
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
