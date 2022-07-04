object FormMain: TFormMain
  Left = 423
  Top = 200
  Width = 334
  Height = 502
  BorderWidth = 8
  Caption = 'JSON'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 302
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonOpenFile: TButton
      Left = 113
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Open file...'
      TabOrder = 0
      OnClick = ButtonOpenFileClick
    end
  end
  object TreeView: TTreeView
    Left = 0
    Top = 36
    Width = 302
    Height = 412
    Align = alClient
    BorderStyle = bsNone
    Indent = 19
    TabOrder = 0
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'json'
    Filter = 'JSON Files (*.json)|*.json|All Files (*.*)|*.*'
    Left = 32
  end
end
