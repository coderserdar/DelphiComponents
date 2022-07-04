object ExceptionDialog: TExceptionDialog
  Left = 369
  Top = 274
  Width = 491
  Height = 378
  ActiveControl = OkBtn
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'ExceptionDialog'
  Color = clBtnFace
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ShapeDetailsMemo: TShape
    Left = 1
    Top = 106
    Width = 480
    Height = 234
    Anchors = [akLeft, akTop, akRight, akBottom]
    Brush.Color = clBtnFace
    Pen.Color = clGray
    Pen.Style = psDot
  end
  object Bevel1: TBevel
    Left = 3
    Top = 106
    Width = 479
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    Visible = False
  end
  object ShapeTextLabel: TShape
    Left = 52
    Top = 3
    Width = 349
    Height = 104
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clBtnFace
    Pen.Color = clGray
    Pen.Style = psDot
  end
  object LErrorCountCaption: TLabel
    Left = 352
    Top = 82
    Width = 37
    Height = 13
    Caption = 'Errors:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8404992
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object LErrorCount: TLabel
    Left = 396
    Top = 82
    Width = 27
    Height = 13
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object KillBtn: TButton
    Left = 403
    Top = 81
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Kill App'
    TabOrder = 5
    TabStop = False
    Visible = False
    OnClick = KillBtnClick
  end
  object OkBtn: TButton
    Left = 403
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object DetailsMemo: TMemo
    Left = 3
    Top = 109
    Width = 476
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WantReturns = False
    WordWrap = False
  end
  object DetailsBtn: TButton
    Left = 403
    Top = 55
    Width = 75
    Height = 25
    Hint = 'Show or hide additional information|'
    Anchors = [akTop, akRight]
    Caption = '&Details'
    TabOrder = 2
    OnClick = DetailsBtnClick
  end
  object TextLabel: TMemo
    Left = 56
    Top = 7
    Width = 336
    Height = 68
    Hint = 'Use Ctrl+C to copy the report to the clipboard'
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Ctl3D = True
    Lines.Strings = (
      'TextLabel')
    ParentColor = True
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 0
    WantReturns = False
  end
  object CopyButton: TButton
    Left = 403
    Top = 29
    Width = 75
    Height = 25
    Hint = 'Copy report to clipboard'
    Anchors = [akTop, akRight]
    Caption = '&Copy all'
    TabOrder = 3
    OnClick = CopyButtonClick
  end
end
