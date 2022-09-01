object cal_demo_frm_main: Tcal_demo_frm_main
  Left = 340
  Top = 113
  Caption = 'Calendar Demo'
  ClientHeight = 534
  ClientWidth = 534
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
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl_Main: TPageControl
    Left = 0
    Top = 0
    Width = 534
    Height = 534
    ActivePage = TabSheet_Calendar
    Align = alClient
    MultiLine = True
    TabOrder = 0
    object TabSheet_Calendar: TTabSheet
      Caption = 'Calendar'
    end
    object TabSheet_DateEdit: TTabSheet
      Caption = 'Date Edit'
    end
    object TabSheet_MonthBox: TTabSheet
      Caption = 'MonthsBox'
    end
    object TabSheet_CalPanel: TTabSheet
      Caption = 'Calendar Panel'
    end
    object TabSheet_PSCEdit: TTabSheet
      Caption = 'Date Up/Down'
    end
  end
end
