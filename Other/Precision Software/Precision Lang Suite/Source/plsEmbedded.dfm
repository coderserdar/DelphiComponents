object frmEmbeddedEditor: TfrmEmbeddedEditor
  Left = 0
  Top = 0
  ActiveControl = sgItems
  BorderStyle = bsSizeToolWin
  Caption = 'Embedded localization editor'
  ClientHeight = 226
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pBottom: TPanel
    Left = 0
    Top = 185
    Width = 392
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      392
      41)
    object sbOK: TButton
      Left = 228
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object sbCancel: TButton
      Left = 309
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object sgItems: TStringGrid
    Left = 0
    Top = 0
    Width = 392
    Height = 185
    Align = alClient
    ColCount = 3
    Ctl3D = True
    DefaultColWidth = 128
    DefaultRowHeight = 19
    DefaultDrawing = False
    FixedCols = 2
    RowCount = 2
    Options = [goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
    ParentCtl3D = False
    TabOrder = 0
    OnDblClick = sgItemsDblClick
    OnDrawCell = sgItemsDrawCell
    OnGetEditText = sgItemsGetEditText
    OnKeyDown = sgItemsKeyDown
    OnSetEditText = sgItemsSetEditText
  end
end
