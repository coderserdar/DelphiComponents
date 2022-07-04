object Form2: TForm2
  Left = 203
  Top = 201
  BorderStyle = bsDialog
  Caption = 'TELInstanceChecker demo'
  ClientHeight = 171
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 369
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'If you see this form then this is NOT first instance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 0
    Top = 26
    Width = 377
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'of this application running in system'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 189
    Height = 13
    Caption = 'You can post data string to first instance'
  end
  object Label4: TLabel
    Left = 16
    Top = 72
    Width = 159
    Height = 13
    Caption = 'Type it in edit and click Ok button'
  end
  object Bevel1: TBevel
    Left = 2
    Top = 160
    Width = 383
    Height = 3
    Shape = bsTopLine
  end
  object Label5: TLabel
    Left = 8
    Top = 153
    Width = 80
    Height = 13
    Caption = 'Extension Library'
    Enabled = False
  end
  object Edit1: TEdit
    Left = 8
    Top = 96
    Width = 369
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 112
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 192
    Top = 128
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
