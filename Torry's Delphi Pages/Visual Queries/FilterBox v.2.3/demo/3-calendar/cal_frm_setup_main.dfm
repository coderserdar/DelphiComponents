object psc_frm_setup_main: Tpsc_frm_setup_main
  Left = 439
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Calendar Properties'
  ClientHeight = 464
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl_Manager: TPageControl
    Left = 5
    Top = 5
    Width = 452
    Height = 418
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
    end
    object TabSheet2: TTabSheet
      Caption = 'Additional'
    end
    object TabSheet3: TTabSheet
      Caption = 'Holidays'
      ImageIndex = 2
    end
    object TabSheet4: TTabSheet
      Caption = 'Fonts and Colors'
      ImageIndex = 3
    end
  end
  object Panel_Button: TPanel
    Left = 0
    Top = 423
    Width = 462
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button_OK: TButton
      Left = 216
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button_Cancel: TButton
      Left = 296
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Button_Apply: TButton
      Left = 376
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Apply'
      Enabled = False
      TabOrder = 2
      OnClick = Button_ApplyClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 462
    Height = 5
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
  end
  object Panel3: TPanel
    Left = 0
    Top = 5
    Width = 5
    Height = 418
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 3
  end
  object Panel4: TPanel
    Left = 457
    Top = 5
    Width = 5
    Height = 418
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 4
  end
end
