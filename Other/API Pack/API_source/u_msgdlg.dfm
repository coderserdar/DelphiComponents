object f_msgdlg: Tf_msgdlg
  Left = 189
  Top = 231
  AlphaBlend = True
  AlphaBlendValue = 0
  ClientHeight = 141
  ClientWidth = 335
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    335
    141)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 320
    Height = 92
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object API_grbutton1: TAPI_grbutton
    Left = 8
    Top = 108
    Width = 320
    Height = 25
    Anchors = [akLeft, akTop, akBottom]
    BorderWidth = 1
    Caption = 'Ok'
    Color = clBtnFace
    Enabled = True
    TabOrder = 0
    VerticalAlignment = taVerticalCenter
    OnClick = BitBtn2Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = clSilver
    ColorDown = 10132122
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clWhite
    Elliptic = True
    CornerRadius = 10
    ShowCaption = True
    WordWrap = False
  end
  object Memo1: TMemo
    Left = 12
    Top = 12
    Width = 309
    Height = 81
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Color = clBtnFace
    TabOrder = 1
  end
  object API_abform1: TAPI_abform
    Active = True
    AlphaStart = 0
    AlphaStop = 255
    TimeDelay = 1
    StepSize = 5
    Left = 16
    Top = 16
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 48
    Top = 16
  end
end
