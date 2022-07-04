object FTDropDownForm: TFTDropDownForm
  Left = 194
  Top = 168
  BorderStyle = bsNone
  BorderWidth = 1
  Caption = 'DropDownForm'
  ClientHeight = 155
  ClientWidth = 131
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
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 131
    Height = 155
    Align = alClient
    Columns = <>
    HotTrack = True
    ShowColumnHeaders = False
    SmallImages = ImageS
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = ListView1Click
  end
  object ImageS: TImageList
    Left = 96
    Top = 8
  end
end
