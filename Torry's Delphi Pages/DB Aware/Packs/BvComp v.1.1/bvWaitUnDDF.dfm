object WaitDDForm: TWaitDDForm
  Left = 145
  Top = 148
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 43
  ClientWidth = 343
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
    Width = 343
    Height = 43
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 3
    TabOrder = 0
    object Panel: TPanel
      Left = 5
      Top = 5
      Width = 333
      Height = 33
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 2
      TabOrder = 0
      object Lab: TLabel
        Left = 2
        Top = 2
        Width = 329
        Height = 21
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        Caption = '??'
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
      object ProgressBar: TProgressBar
        Left = 2
        Top = 23
        Width = 329
        Height = 8
        Align = alBottom
        Min = 0
        Max = 100
        Step = 1
        TabOrder = 0
        Visible = False
      end
    end
  end
end
