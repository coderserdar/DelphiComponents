object Form1: TForm1
  Left = 196
  Top = 118
  Width = 639
  Height = 493
  Caption = 'FrameGrab Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    631
    459)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 463
    Top = 8
    Width = 104
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Select Capture Driver:'
  end
  object API_FrameGrab1: TAPI_framegrab
    Left = 0
    Top = 0
    Width = 456
    Height = 459
    Align = alLeft
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'API_FrameGrab1'
    Color = clBlack
    TabOrder = 0
    OnMouseDown = API_FrameGrab1MouseDown
    OnMouseUp = API_FrameGrab1MouseUp
    DriverOpen = False
    DriverIndex = -1
    VideoOverlay = False
    VideoPreview = False
    PreviewScaleToWindow = True
    PreviewScaleProportional = True
    PreviewRate = 30
    MicroSecPerFrame = 66667
    FrameRate = 15
    CapAudio = False
    VideoFileName = 'Video.avi'
    SingleImageFile = 'Capture.bmp'
    CapTimeLimit = 0
    CapIndexSize = 0
    CapToFile = False
    BufferFileSize = 0
    object Shape1: TShape
      Left = 32
      Top = 32
      Width = 209
      Height = 161
      Brush.Style = bsClear
      Pen.Color = clWhite
      Pen.Style = psDot
    end
  end
  object ComboBox1: TComboBox
    Left = 464
    Top = 24
    Width = 152
    Height = 21
    Anchors = [akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnChange = ComboBox1Change
  end
  object Button1: TButton
    Left = 463
    Top = 80
    Width = 154
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Start/Stop Capturing'
    Enabled = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 463
    Top = 104
    Width = 154
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Video Source Dialog'
    Enabled = False
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button2: TButton
    Left = 463
    Top = 128
    Width = 154
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Video Format Dialog'
    Enabled = False
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 463
    Top = 152
    Width = 154
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Display Dialog'
    Enabled = False
    TabOrder = 5
    OnClick = Button4Click
  end
  object CheckBox1: TCheckBox
    Left = 463
    Top = 56
    Width = 65
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Overlay'
    Enabled = False
    TabOrder = 6
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 535
    Top = 56
    Width = 73
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Preview'
    Enabled = False
    TabOrder = 7
    OnClick = CheckBox2Click
  end
  object Button5: TButton
    Left = 463
    Top = 192
    Width = 154
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Grab Frame'
    Enabled = False
    TabOrder = 8
    OnClick = Button5Click
  end
  object Panel1: TPanel
    Left = 464
    Top = 232
    Width = 153
    Height = 129
    TabOrder = 9
    object PaintBox1: TPaintBox
      Left = 1
      Top = 1
      Width = 151
      Height = 127
      Align = alClient
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 392
    Top = 8
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 472
    Top = 240
  end
end
