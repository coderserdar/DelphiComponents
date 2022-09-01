object cal_demo_frm_calendar: Tcal_demo_frm_calendar
  Left = 315
  Top = 124
  Width = 544
  Height = 545
  Caption = 'Calendar'
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
  object Panel_Calendar_Main: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 511
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel_ToolBar: TPanel
      Left = 0
      Top = 0
      Width = 536
      Height = 511
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Panel_Buttons: TPanel
        Left = 0
        Top = 0
        Width = 536
        Height = 32
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object SpeedButton_Setup: TSpeedButton
          Left = 4
          Top = 3
          Width = 25
          Height = 25
          Hint = 'Calendar Setup'
          Flat = True
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            88888888888888800888837088888809B0888B370888809B990888B3708809B9
            9088888B37001B9908888888B3019B90888888888B3B1B0888888888880FB088
            8088888880FB3708070888880F88B37078088800F8888B777888808F88888077
            7888878F8888878888888F088888088888888888888808888888}
          ParentShowHint = False
          ShowHint = True
          OnClick = SpeedButton_SetupClick
        end
        object Label_CalendarStyle: TLabel
          Left = 34
          Top = 8
          Width = 26
          Height = 13
          Caption = 'Style:'
        end
        object SpeedButton_PrintCalendar: TSpeedButton
          Left = 501
          Top = 3
          Width = 25
          Height = 25
          Hint = 'Print Calendar'
          Flat = True
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            04000000000080000000CE0E0000D80E00001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00222222222222
            22222200000000000222208888888880802200000000000008020888888BBB88
            0002088888877788080200000000000008800888888888808080200000000008
            0800220FFFFFFFF080802220F00000F000022220FFFFFFFF022222220F00000F
            022222220FFFFFFFF02222222000000000222222222222222222}
          ParentShowHint = False
          ShowHint = True
          OnClick = SpeedButton_PrintCalendarClick
        end
        object Bevel1: TBevel
          Left = 0
          Top = 30
          Width = 536
          Height = 2
          Align = alBottom
          Shape = bsBottomLine
        end
        object ComboBox_CalendarStyle: TComboBox
          Left = 64
          Top = 5
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = ComboBox_CalendarStyleChange
        end
      end
    end
  end
end
