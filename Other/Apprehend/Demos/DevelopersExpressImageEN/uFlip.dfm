object FormFlip: TFormFlip
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Flip'
  ClientHeight = 323
  ClientWidth = 265
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TcxLabel
    Left = 8
    Top = 12
    Caption = 'Result'
    Transparent = True
  end
  object RadioGroup1: TcxRadioGroup
    Left = 8
    Top = 224
    Caption = 'Flip'
    Properties.Items = <
      item
        Caption = 'Horzontally'
      end
      item
        Caption = 'Vertically'
      end>
    Style.LookAndFeel.NativeStyle = True
    StyleDisabled.LookAndFeel.NativeStyle = True
    StyleFocused.LookAndFeel.NativeStyle = True
    StyleHot.LookAndFeel.NativeStyle = True
    TabOrder = 0
    Transparent = True
    OnClick = RadioGroup1Click
    Height = 57
    Width = 120
  end
  object ImageEnView1: TImageEnView
    Left = 8
    Top = 31
    Width = 245
    Height = 187
    Cursor = 1779
    Background = clWhite
    ParentCtl3D = False
    ScrollBars = ssNone
    MouseInteract = [miZoom]
    AutoFit = True
    BackgroundStyle = iebsChessboard
    EnableInteractionHints = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object MergeAlpha1: TcxCheckBox
    Left = 162
    Top = 224
    Caption = 'Merge Alpha'
    Style.LookAndFeel.NativeStyle = True
    StyleDisabled.LookAndFeel.NativeStyle = True
    StyleFocused.LookAndFeel.NativeStyle = True
    StyleHot.LookAndFeel.NativeStyle = True
    TabOrder = 2
    Transparent = True
    Width = 91
  end
  object Panel1: TPanel
    Left = 0
    Top = 287
    Width = 265
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 3
    object Button1: TcxButton
      Left = 7
      Top = 6
      Width = 70
      Height = 25
      Caption = 'Ok'
      Default = True
      LookAndFeel.NativeStyle = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TcxButton
      Left = 83
      Top = 6
      Width = 70
      Height = 25
      Caption = 'Cancel'
      LookAndFeel.NativeStyle = True
      ModalResult = 2
      TabOrder = 1
    end
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Kind = lfFlat
    NativeStyle = False
    Left = 26
    Top = 112
  end
end
