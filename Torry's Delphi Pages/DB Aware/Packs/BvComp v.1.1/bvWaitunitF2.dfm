object WaitForm3: TWaitForm3
  Left = 266
  Top = 193
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 60
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 341
    Height = 60
    Align = alClient
    BevelWidth = 2
    TabOrder = 0
    object Panel: TPanel
      Left = 2
      Top = 2
      Width = 337
      Height = 56
      Align = alClient
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      object Bevel: TBevel
        Left = 4
        Top = 4
        Width = 50
        Height = 48
        Shape = bsRightLine
      end
      object LabText: TLabel
        Left = 56
        Top = 4
        Width = 273
        Height = 51
        Alignment = taCenter
        AutoSize = False
        Caption = '???'
        Color = clBtnFace
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
        Layout = tlCenter
        WordWrap = True
      end
      object AnimateGlobe: TAnimate
        Left = 4
        Top = 4
        Width = 48
        Height = 48
        Active = False
        AutoSize = False
        Color = clBtnFace
        ParentColor = False
        StopFrame = 23
      end
      object ProgressBar: TProgressBar
        Left = 56
        Top = 45
        Width = 275
        Height = 8
        Min = 0
        Max = 100
        Step = 1
        TabOrder = 0
        Visible = False
      end
    end
  end
end
