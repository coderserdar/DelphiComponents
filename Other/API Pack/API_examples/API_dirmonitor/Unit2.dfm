object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Directory Monitor Example'
  ClientHeight = 265
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    557
    265)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Folder:'
  end
  object Label2: TLabel
    Left = 8
    Top = 69
    Width = 21
    Height = 13
    Caption = 'Log:'
  end
  object API_checkbox1: TAPI_checkbox
    Left = 8
    Top = 45
    Width = 189
    Height = 18
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Style = LSCheck
    Wordwrap = True
    Alignment = taLeftJustify
    Caption = 'Include sub folders'
    Checked = True
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    CheckColorOn = clGreen
    CheckColorOff = clRed
    CheckHeight = 10
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 460
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Button1: TButton
    Left = 474
    Top = 22
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 1
    OnClick = Button1Click
  end
  object API_memo1: TAPI_memo
    Left = 8
    Top = 88
    Width = 541
    Height = 165
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
    MouseOverColor = clWindow
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'Tahoma'
    FontMouseOver.Style = []
    MaxLines = 0
  end
  object API_DirMonitor1: TAPI_DirMonitor
    Active = False
    Subs = True
    Options = [fmFilename, fmDirname, fmLastWrite]
    OnChange = API_DirMonitor1Change
    Left = 248
    Top = 44
  end
  object API_files1: TAPI_files
    ShowProgress = True
    Confirmation = False
    Left = 216
    Top = 44
  end
end
