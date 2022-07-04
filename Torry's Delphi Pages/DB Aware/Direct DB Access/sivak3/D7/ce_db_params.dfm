object fce_db_params: Tfce_db_params
  Left = 287
  Top = 251
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  BorderWidth = 4
  Caption = '  Database parameters'
  ClientHeight = 226
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    484
    226)
  PixelsPerInch = 96
  TextHeight = 13
  object ValueList: TValueListEditor
    Left = 0
    Top = 0
    Width = 484
    Height = 177
    Align = alTop
    BorderStyle = bsNone
    Ctl3D = True
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    KeyOptions = [keyUnique]
    ParentCtl3D = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    TitleCaptions.Strings = (
      'Pragma'
      'Value')
    ColWidths = (
      319
      163)
  end
  object Button1: TButton
    Left = 312
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object Cancel: TButton
    Left = 392
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
