object Form1: TForm1
  Left = 299
  Top = 382
  Width = 242
  Height = 310
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  DesignSize = (
    234
    276)
  PixelsPerInch = 96
  TextHeight = 13
  object API_trackbar1: TAPI_trackbar
    Left = 8
    Top = 8
    Width = 217
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    Maximum = 100.000000000000000000
    Value = 20.000000000000000000
    ColorBorder = clBtnFace
    ColorBackground = clBtnFace
    ColorThumb = clBlack
    ColorThumbBorder = clSilver
    ThumbWidth = 20
    ThumbHeight = 25
    ColorTrack1 = clGreen
    ColorTrack1Border = clSilver
    ColorTrack2 = clRed
    ColorTrack2Border = clSilver
    TrackHeight = 20
    Orientation = TTO_horizontal
    OnChange = API_trackbar1Change
    Steps = 0
  end
  object tAPI_progressbar1: TAPI_progressbar
    Left = 32
    Top = 40
    Width = 169
    Height = 201
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Max = 100.000000000000000000
    Position = 20.000000000000000000
    ColorBack = clBlack
    ColorStart = clLime
    ColorEnd = clRed
    ValueAsText = False
    AddToText = '%'
    Vertical = True
    BorderWidth = 5
    ColorBorder = clGray
    Alignment = taCenter
    Steps = 5
  end
  object API_progressbar1: TAPI_progressbar
    Left = 32
    Top = 248
    Width = 166
    Height = 22
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Max = 100.000000000000000000
    ColorBack = clBlack
    ColorStart = clLime
    ColorEnd = clRed
    ValueAsText = False
    AddToText = '%'
    Vertical = False
    BorderWidth = 1
    ColorBorder = clGray
    Alignment = taCenter
    Steps = 0
  end
end
