object FormViewOptionsGutter: TFormViewOptionsGutter
  Left = 259
  Top = 185
  ActiveControl = chkShowGutter
  BorderStyle = bsDialog
  Caption = 'Gutter & line numbers'
  ClientHeight = 313
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 172
    Top = 284
    Width = 81
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOk: TButton
    Left = 84
    Top = 284
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object boxGutter: TGroupBox
    Left = 8
    Top = 4
    Width = 321
    Height = 273
    TabOrder = 0
    object Bevel1: TBevel
      Left = 8
      Top = 40
      Width = 305
      Height = 9
      Shape = bsTopLine
    end
    object labLineKb: TLabel
      Left = 88
      Top = 128
      Width = 42
      Height = 13
      Caption = 'Kb of file'
    end
    object labLineSize: TLabel
      Left = 24
      Top = 108
      Width = 84
      Height = 13
      Caption = 'Only for the first:'
      FocusControl = edLineSize
    end
    object labFontShow: TLabel
      Left = 88
      Top = 85
      Width = 22
      Height = 13
      Caption = 'Font'
    end
    object labFont: TLabel
      Left = 24
      Top = 68
      Width = 26
      Height = 13
      Caption = 'Font:'
      FocusControl = btnFont
    end
    object labLineCount: TLabel
      Left = 24
      Top = 148
      Width = 78
      Height = 13
      Caption = 'Max lines count:'
      FocusControl = edLineCount
    end
    object labLineStep: TLabel
      Left = 24
      Top = 188
      Width = 26
      Height = 13
      Caption = 'Step:'
      FocusControl = edLineStep
    end
    object chkShowGutter: TCheckBox
      Left = 8
      Top = 16
      Width = 289
      Height = 17
      Caption = 'Show gutter on the left'
      TabOrder = 0
    end
    object chkShowLines: TCheckBox
      Left = 8
      Top = 48
      Width = 289
      Height = 17
      Caption = 'Show line numbers on gutter'
      TabOrder = 1
      OnClick = chkShowLinesClick
    end
    object chkLineExt: TCheckBox
      Left = 24
      Top = 228
      Width = 273
      Height = 17
      Caption = 'Only for these file extensions:'
      TabOrder = 6
    end
    object edLineExt: TEdit
      Left = 40
      Top = 244
      Width = 257
      Height = 21
      TabOrder = 7
    end
    object edLineSize: TEdit
      Left = 40
      Top = 124
      Width = 41
      Height = 21
      TabOrder = 3
    end
    object btnFont: TButton
      Left = 40
      Top = 82
      Width = 41
      Height = 21
      Caption = '...'
      TabOrder = 2
      OnClick = btnFontClick
    end
    object edLineCount: TEdit
      Left = 40
      Top = 164
      Width = 41
      Height = 21
      TabOrder = 4
    end
    object edLineStep: TEdit
      Left = 40
      Top = 204
      Width = 41
      Height = 21
      TabOrder = 5
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg'
    Font.Style = []
    Left = 224
    Top = 4
  end
end
