object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Form1'
  ClientHeight = 534
  ClientWidth = 604
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 601
    Height = 505
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Settings'
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 190
        Height = 13
        Caption = 'Select COM port to use with 232SDA12:'
      end
      object ComboBox1: TComboBox
        Left = 8
        Top = 40
        Width = 145
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'COM1'
        Items.Strings = (
          'COM1'
          'COM2'
          'COM3'
          'COM4'
          'COM5'
          'COM6'
          'COM7'
          'COM8')
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Chart'
      ImageIndex = 1
      object Label2: TLabel
        Left = 8
        Top = 264
        Width = 96
        Height = 13
        Caption = 'Analog input values:'
      end
      object API_linechart1: TAPI_linechart
        Left = 8
        Top = 8
        Width = 577
        Height = 241
        Caption = 'API_linechart1'
        TabOrder = 0
        HistoryCount = 100
        NumOfLines = 11
        BackgroundColor = clBlack
        BackgroundImage.Data = {07544269746D617000000000}
        Grid = True
        GridColor = clGray
        GridCountH = 10
        GridCountV = 20
        ZoomAuto = True
        ShowMinMax = False
        Gradient = False
        GradientStart = clBlack
        GradientEnd = clGray
        GradientStyle = gsRightToLeft
      end
      object ListBox1: TListBox
        Left = 16
        Top = 288
        Width = 121
        Height = 169
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 1
      end
    end
  end
  object API_grbutton1: TAPI_grbutton
    Left = 8
    Top = 512
    Width = 100
    Height = 22
    BorderWidth = 1
    Caption = 'start'
    Color = clBtnFace
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton1Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    ColorBorder = 8556441
    GradientEnd = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton2: TAPI_grbutton
    Left = 496
    Top = 512
    Width = 100
    Height = 22
    BorderWidth = 1
    Caption = 'close'
    Color = clBtnFace
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton2Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    ColorBorder = 8556441
    GradientEnd = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 120
    Top = 512
    Width = 369
    Height = 25
    TabOrder = 3
    object Label3: TLabel
      Left = 8
      Top = 8
      Width = 3
      Height = 13
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 504
    Top = 8
  end
end
