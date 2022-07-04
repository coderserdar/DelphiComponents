object ExceptionDialog: TExceptionDialog
  Left = 363
  Top = 284
  ActiveControl = OkBtn
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'ExceptionDialog'
  ClientHeight = 302
  ClientWidth = 492
  Color = clBtnFace
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ShapeDetailsMemo: TShape
    Left = 1
    Top = 112
    Width = 489
    Height = 189
    Anchors = [akLeft, akTop, akRight, akBottom]
    Brush.Color = clBtnFace
    Pen.Color = clBtnShadow
    Pen.Style = psDot
  end
  object Bevel1: TBevel
    Left = 3
    Top = 110
    Width = 488
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    Visible = False
  end
  object ShapeTextLabel: TShape
    Left = 52
    Top = 3
    Width = 360
    Height = 107
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clBtnFace
    Pen.Color = clBtnShadow
    Pen.Style = psDot
  end
  object KillBtn: TcxButton
    Left = 415
    Top = 82
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = '&Kill App'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 74
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    TabStop = False
    Visible = False
    OnClick = KillBtnClick
    Colors.Default = 15391994
    Colors.Normal = 15391994
    Colors.Hot = 11500012
    Colors.Pressed = 7165069
  end
  object fImage: TcxImage
    Left = 0
    Top = 0
    TabStop = False
    Enabled = False
    Properties.GraphicTransparency = gtTransparent
    Style.BorderColor = clBtnShadow
    Style.BorderStyle = ebsSingle
    Style.Color = clBtnFace
    Style.Edges = [bLeft, bTop]
    Style.HotTrack = False
    Style.Shadow = False
    StyleDisabled.BorderColor = clBtnShadow
    StyleDisabled.TextColor = clWindowText
    TabOrder = 0
    Visible = False
    Height = 112
    Width = 53
  end
  object TextLabel: TcxMemo
    Left = 56
    Top = 8
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'TextLabel')
    ParentFont = False
    Properties.ReadOnly = True
    Style.BorderColor = clBtnFace
    Style.BorderStyle = ebsNone
    Style.Color = clBtnFace
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -11
    Style.Font.Name = 'Tahoma'
    Style.Font.Style = [fsBold]
    Style.HotTrack = False
    Style.TransparentBorder = False
    Style.IsFontAssigned = True
    StyleDisabled.BorderColor = clBtnFace
    TabOrder = 1
    Height = 99
    Width = 356
  end
  object DetailsMemo: TcxMemo
    Left = 2
    Top = 113
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'DetailsMemo')
    ParentFont = False
    Properties.ReadOnly = True
    Properties.ScrollBars = ssBoth
    Style.BorderColor = clBtnFace
    Style.BorderStyle = ebsNone
    Style.Color = clBtnFace
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -13
    Style.Font.Name = 'Tahoma'
    Style.Font.Style = []
    Style.HotTrack = False
    Style.TransparentBorder = False
    Style.IsFontAssigned = True
    StyleDisabled.BorderColor = clBtnFace
    StyleFocused.BorderColor = clBtnFace
    TabOrder = 6
    Height = 187
    Width = 487
  end
  object OkBtn: TcxButton
    Left = 414
    Top = 4
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    Colors.Default = 15455958
    Colors.Normal = clBtnFace
    Colors.Hot = 15455958
    Colors.Pressed = 11454416
    LookAndFeel.Kind = lfUltraFlat
  end
  object CopyButton: TcxButton
    Left = 414
    Top = 30
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = '&Copy all'
    TabOrder = 3
    OnClick = CopyButtonClick
    Colors.Default = 15455958
    Colors.Normal = clBtnFace
    Colors.Hot = 15455958
    Colors.Pressed = 11454416
  end
  object DetailsBtn: TcxButton
    Left = 414
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = '&Details'
    TabOrder = 4
    OnClick = DetailsBtnClick
    Colors.Default = 15455958
    Colors.Normal = clBtnFace
    Colors.Hot = 15455958
    Colors.Pressed = 11454416
  end
  object LErrorCount: TcxLabel
    Left = 454
    Top = 84
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = '1'
    ParentFont = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clBlack
    Style.Font.Height = -11
    Style.Font.Name = 'Tahoma'
    Style.Font.Style = [fsBold]
    Style.TransparentBorder = False
    Style.IsFontAssigned = True
    Visible = False
    Height = 17
    Width = 35
  end
  object LErrorCountCaption: TcxLabel
    Left = 414
    Top = 84
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'Errors:'
    ParentFont = False
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = 8404992
    Style.Font.Height = -11
    Style.Font.Name = 'Tahoma'
    Style.Font.Style = [fsBold]
    Style.TransparentBorder = False
    Style.IsFontAssigned = True
    Visible = False
    Height = 17
    Width = 41
  end
end
