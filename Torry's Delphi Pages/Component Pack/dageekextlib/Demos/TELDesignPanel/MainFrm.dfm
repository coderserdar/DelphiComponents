object frmMain: TfrmMain
  Left = 293
  Top = 97
  Width = 692
  Height = 422
  Caption = 'TELDesignPanel demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 75
    Top = 8
    Width = 50
    Height = 22
    GroupIndex = 1
    Caption = 'Label'
  end
  object SpeedButton2: TSpeedButton
    Left = 129
    Top = 8
    Width = 50
    Height = 22
    GroupIndex = 1
    Caption = 'Button'
  end
  object SpeedButton3: TSpeedButton
    Left = 18
    Top = 8
    Width = 50
    Height = 22
    GroupIndex = 1
    Down = True
    Caption = 'Select'
  end
  object SpeedButton4: TSpeedButton
    Left = 184
    Top = 8
    Width = 50
    Height = 22
    GroupIndex = 1
    Caption = 'Memo'
  end
  object SpeedButton5: TSpeedButton
    Left = 238
    Top = 8
    Width = 50
    Height = 22
    GroupIndex = 1
    Caption = 'CheckBox'
  end
  object LMDDesignPanel1: TELDesignPanel
    Left = 17
    Top = 40
    Width = 462
    Height = 333
    BorderStyle = bsNone
    AutoScroll = True
    TabOrder = 0
    TabStop = True
  end
  object Button1: TButton
    Tag = 1
    Left = 323
    Top = 7
    Width = 162
    Height = 25
    TabOrder = 1
    OnClick = Button1Click
  end
  object ELPropertyInspector1: TELPropertyInspector
    Left = 486
    Top = 39
    Width = 188
    Height = 338
    Splitter = 84
    TabOrder = 2
  end
  object ELDesigner1: TELDesigner
    DesignPanel = LMDDesignPanel1
    ClipboardFormat = 'Extension Library designer components'
    OnControlInserting = ELDesigner1ControlInserting
    OnControlInserted = ELDesigner1ControlInserted
    OnChangeSelection = ELDesigner1ChangeSelection
    Left = 18
    Top = 59
  end
end
