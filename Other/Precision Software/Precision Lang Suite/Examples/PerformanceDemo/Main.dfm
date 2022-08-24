object TfrmPerf: TTfrmPerf
  Left = 0
  Top = 0
  Caption = 'Precision Language Suite Performance Demo'
  ClientHeight = 348
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    535
    348)
  PixelsPerInch = 96
  TextHeight = 13
  object lbCount: TLabel
    Left = 12
    Top = 76
    Width = 86
    Height = 13
    Caption = 'Count of buttons:'
  end
  object lbInfo: TLabel
    Left = 12
    Top = 8
    Width = 509
    Height = 49
    AutoSize = False
    Caption = 
      '1) Enter the count of buttons and click "Create".'#13#10'2) Then click' +
      ' "Load" to load the file into the Language Manager.'#13#10'3) Then cli' +
      'ck "LangVCL" to apply the temporary localization to this form.'
    WordWrap = True
  end
  object lbCreateCap: TLabel
    Left = 12
    Top = 320
    Width = 60
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Create time:'
  end
  object lbCreate: TLabel
    Left = 78
    Top = 320
    Width = 5
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '?'
  end
  object lbLoadCap: TLabel
    Left = 189
    Top = 320
    Width = 50
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Load time:'
  end
  object lbLoad: TLabel
    Left = 255
    Top = 320
    Width = 5
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '?'
  end
  object lbLangVCLCap: TLabel
    Left = 356
    Top = 320
    Width = 68
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'LangVCL time:'
  end
  object lbLangVCL: TLabel
    Left = 430
    Top = 320
    Width = 5
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '?'
  end
  object edCount: TEdit
    Left = 112
    Top = 73
    Width = 73
    Height = 21
    TabOrder = 0
    Text = '50'
  end
  object sbBtns: TScrollBox
    Left = 12
    Top = 108
    Width = 421
    Height = 197
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 1
  end
  object sbCreate: TButton
    Left = 446
    Top = 108
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 2
    OnClick = sbCreateClick
  end
  object sbLoad: TButton
    Left = 446
    Top = 139
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 3
    OnClick = sbLoadClick
  end
  object sbApply: TButton
    Left = 446
    Top = 170
    Width = 75
    Height = 25
    Caption = 'LangVCL'
    TabOrder = 4
    OnClick = sbApplyClick
  end
end
