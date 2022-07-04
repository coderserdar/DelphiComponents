object fpe_masterlinks: Tfpe_masterlinks
  Left = 235
  Top = 235
  Width = 500
  Height = 418
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  BorderWidth = 4
  Caption = '  Master-detail links'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    484
    376)
  PixelsPerInch = 96
  TextHeight = 13
  object ValueList: TValueListEditor
    Left = 0
    Top = 0
    Width = 400
    Height = 376
    Align = alLeft
    BorderStyle = bsNone
    Ctl3D = True
    DefaultColWidth = 188
    KeyOptions = [keyUnique]
    ParentCtl3D = False
    ScrollBars = ssVertical
    TabOrder = 0
    ColWidths = (
      188
      210)
  end
  object Button1: TButton
    Left = 408
    Top = 32
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object Cancel: TButton
    Left = 408
    Top = 64
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
