object frmProps: TfrmProps
  Left = 630
  Top = 296
  Width = 196
  Height = 303
  HorzScrollBar.Tracking = True
  BorderIcons = [biSystemMenu]
  Caption = 'Properties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000FFFFFFFFFF000000F00F00000F000000FFFFFFFFFF000000F00
    F00000F000000FFFFFFFFFF000000FFFFFFF0FF000000F00FFF070F000000F07
    0F07070000440FF0707070777044000007070777774400000070777777440000
    000777777044000000000000004400000000000000000000000000000000000F
    0000000F0000000F0000000F0000000F0000000F0000000F0000000F00000004
    00000000000000000000F8000000FC000000FE040000FFFF0000FFFF0000}
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object PropInsp: TELPropertyInspector
    Left = 0
    Top = 0
    Width = 188
    Height = 276
    Splitter = 84
    Align = alClient
    TabOrder = 0
    OnFilterProp = PropInspFilterProp
    OnModified = PropInspModified
  end
end
