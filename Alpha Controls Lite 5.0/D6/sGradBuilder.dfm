object GradBuilder: TGradBuilder
  Left = 328
  Top = 269
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Gradient builder'
  ClientHeight = 236
  ClientWidth = 292
  Color = 13750737
  Constraints.MaxHeight = 263
  Constraints.MaxWidth = 300
  Constraints.MinHeight = 263
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TsLabel
    Left = 156
    Top = 6
    Width = 129
    Height = 99
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'For gradient changing use the panel located at the left. Click o' +
      'n this bar for new point creation, click right mouse button on t' +
      'his point for color changing or point removing.'
    ParentFont = False
    Layout = tlCenter
    WordWrap = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object PaintPanel: TsPanel
    Left = 8
    Top = 8
    Width = 93
    Height = 220
    BevelOuter = bvNone
    Caption = ' '
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 4
    SkinData.SkinSection = 'EDIT'
    object PaintBox1: TPaintBox
      Left = 0
      Top = 0
      Width = 93
      Height = 220
      Align = alClient
      OnPaint = PaintBox1Paint
    end
  end
  object Panel2: TsPanel
    Left = 122
    Top = 8
    Width = 29
    Height = 220
    BorderWidth = 4
    Caption = ' '
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 5
    OnClick = Panel2Click
    SkinData.SkinSection = 'PANEL_LOW'
    object TemplatePanel: TsPanel
      Left = 4
      Top = 107
      Width = 19
      Height = 28
      Caption = ' '
      DragCursor = crHandPoint
      PopupMenu = PopupMenu1
      TabOrder = 0
      Visible = False
      OnDblClick = TemplatePanelDblClick
      OnMouseDown = TemplatePanelMouseDown
      OnMouseMove = TemplatePanelMouseMove
      OnMouseUp = TemplatePanelMouseUp
      SkinData.SkinSection = 'PANEL'
    end
  end
  object ComboBox1: TsComboBox
    Left = 188
    Top = 112
    Width = 75
    Height = 21
    Alignment = taLeftJustify
    BoundLabel.Caption = 'Direction'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclTopLeft
    BoundLabel.MaxWidth = 0
    SkinData.SkinSection = 'ALPHACOMBOBOX'
    Style = csDropDownList
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ItemIndex = -1
    ParentFont = False
    TabOrder = 1
    OnChange = ComboBox1Change
    Items.Strings = (
      'Vertical'
      'Horizontal')
  end
  object BitBtn1: TsButton
    Left = 188
    Top = 174
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = BitBtn1Click
    SkinData.SkinSection = 'BUTTON'
  end
  object sButton1: TsButton
    Left = 188
    Top = 148
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = sButton1Click
    SkinData.SkinSection = 'BUTTON'
  end
  object sButton2: TsButton
    Left = 188
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
    OnClick = sButton2Click
    SkinData.SkinSection = 'BUTTON'
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 248
    Top = 32
    object Changecolor1: TMenuItem
      Caption = 'Change color'
      Default = True
      OnClick = Changecolor1Click
    end
    object Delete1: TMenuItem
      Caption = 'Delete'
      OnClick = Delete1Click
    end
  end
  object sSkinProvider1: TsSkinProvider
    BorderIcons = []
    CaptionAlignment = taCenter
    SkinData.SkinSection = 'FORM'
    MakeSkinMenu = False
    ShowAppIcon = False
    TitleButtons = <>
    Left = 210
    Top = 16
  end
end
