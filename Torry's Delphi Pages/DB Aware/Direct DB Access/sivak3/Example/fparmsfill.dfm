object manFillParms: TmanFillParms
  Left = 305
  Top = 313
  Width = 400
  Height = 300
  BorderIcons = [biSystemMenu]
  BorderWidth = 4
  Caption = 'Fill input parametters for query'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 217
    Width = 384
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      384
      41)
    object Button1: TButton
      Left = 216
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Exec query'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 296
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object parms: TValueListEditor
    Left = 0
    Top = 0
    Width = 384
    Height = 217
    Align = alClient
    BorderStyle = bsNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    TitleCaptions.Strings = (
      'Parametter name'
      'Value')
    ColWidths = (
      152
      230)
  end
end
