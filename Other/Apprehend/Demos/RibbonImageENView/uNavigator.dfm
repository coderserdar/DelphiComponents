object FormNavigator: TFormNavigator
  Left = 2
  Top = 1
  BorderStyle = bsToolWindow
  Caption = 'Navigator'
  ClientHeight = 190
  ClientWidth = 193
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object ImageEnViewNavigator: TImageEnView
    Left = 0
    Top = 0
    Width = 193
    Height = 154
    Background = 16244694
    ParentCtl3D = False
    EnableInteractionHints = True
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 154
    Width = 193
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object Button1: TButton
      Left = 3
      Top = 3
      Width = 88
      Height = 25
      Caption = 'Zoom In'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 99
      Top = 3
      Width = 88
      Height = 25
      Caption = 'Zoom Out'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
