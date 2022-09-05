object FormSelectView: TFormSelectView
  Left = 405
  Top = 163
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'FormSelectView'
  ClientHeight = 330
  ClientWidth = 336
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnMouseUp = FormMouseUp
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 336
    Height = 330
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 2
    TabOrder = 0
    OnMouseUp = FormMouseUp
    object Panel2: TPanel
      Left = 4
      Top = 4
      Width = 328
      Height = 322
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      OnMouseUp = FormMouseUp
      object mcmImageCtrl: TmcmImageCtrl
        Left = 0
        Top = 0
        Width = 328
        Height = 322
        Align = alClient
        ParentColor = False
        Scale = 1.000000000000000000
        ScaleToFit = True
        OnMouseUp = FormMouseUp
      end
      object mcmRegion: TmcmRegion
        Left = 24
        Top = 32
        Width = 65
        Height = 57
        Cursor = crHandPoint
        AllowResize = False
        LinePen.Color = clGray
        LinePen.Mode = pmMaskPenNot
        LinePen.Style = psDot
        MaxHeight = 357
        MaxWidth = 344
        Scale = 1.000000000000000000
        OnMouseUp = FormMouseUp
        OnMoved = mcmRegionMoved
      end
    end
  end
end
