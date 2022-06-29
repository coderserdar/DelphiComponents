object FormMain: TFormMain
  Left = 496
  Top = 242
  BorderWidth = 8
  Caption = 'JSON'
  ClientHeight = 447
  ClientWidth = 348
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
    Width = 348
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 302
    DesignSize = (
      348
      36)
    object ButtonOpenFile: TButton
      Left = 113
      Top = 4
      Width = 121
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Open file...'
      TabOrder = 0
      OnClick = ButtonOpenFileClick
      ExplicitWidth = 75
    end
  end
  object TreeView: TTreeView
    Left = 0
    Top = 36
    Width = 348
    Height = 411
    Align = alClient
    BorderStyle = bsNone
    Indent = 19
    TabOrder = 1
    ExplicitWidth = 302
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'json'
    Filter = 'JSON Files (*.json)|*.json|All Files (*.*)|*.*'
    Left = 32
  end
end
