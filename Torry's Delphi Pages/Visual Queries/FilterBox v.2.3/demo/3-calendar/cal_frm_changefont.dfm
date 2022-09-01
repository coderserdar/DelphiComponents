object cal_frm_fontchange: Tcal_frm_fontchange
  Left = 275
  Top = 123
  Width = 535
  Height = 327
  Caption = 'Compare Sizes'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label_PSCCalendar: TLabel
    Left = 32
    Top = 48
    Width = 70
    Height = 13
    Caption = 'TPSCCalendar'
  end
  object Label_MonthCalendar: TLabel
    Left = 256
    Top = 48
    Width = 79
    Height = 13
    Caption = 'TMonthCalendar'
  end
  object Button_Font: TButton
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Font'
    TabOrder = 0
    OnClick = Button_FontClick
  end
  object MonthCalendar1: TMonthCalendar
    Left = 256
    Top = 72
    Width = 204
    Height = 153
    AutoSize = True
    MultiSelect = True
    Date = 37431.468058356490000000
    EndDate = 37431.000000000000000000
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 252
    Width = 527
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Label_PSCCalendarSize: TLabel
      Left = 40
      Top = 4
      Width = 150
      Height = 28
      AutoSize = False
      Caption = 'PSCCalendar Size'
      WordWrap = True
    end
    object Label_MonthCalendarSize: TLabel
      Left = 240
      Top = 4
      Width = 150
      Height = 28
      AutoSize = False
      Caption = 'MonthCalendar Size'
      WordWrap = True
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 136
    Top = 16
  end
  object ImageList1: TImageList
    Left = 192
    Top = 8
  end
end
