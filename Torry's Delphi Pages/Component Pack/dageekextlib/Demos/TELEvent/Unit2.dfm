object Form2: TForm2
  Left = 192
  Top = 107
  Width = 278
  Height = 189
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    000000000000008FFFFFFFFF7000008F0777770F7000008F7FFF7F7F7000008F
    7777777F7000008F7FFF7F7F7000008F7777777F7000008F7FFF7F7F7000008F
    0777770F7000008FFFFFFFFF7000008F0000F77F7000008F0000FFFF7000008F
    FFFFF7888000008FFFFFFF8F800000888888888800000000000000000000C003
    0000C0030000C0030000C0030000C0030000C0030000C0030000C0030000C003
    0000C0030000C0030000C0030000C0030000C0070000C00F0000FFFF0000}
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 270
    Height = 162
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = Memo1Change
  end
  object ELEvent1: TELEvent
    Source = Form3.ELEventSender1
    OnEvent = ELEvent1Event
    Left = 128
    Top = 32
  end
end
