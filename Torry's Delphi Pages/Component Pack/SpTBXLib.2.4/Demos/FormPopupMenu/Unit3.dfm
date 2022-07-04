object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 105
  ClientWidth = 174
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 174
    Height = 105
    Align = alClient
    BorderStyle = bsNone
    HideSelection = False
    HotTrack = True
    Images = ImageList1
    Indent = 19
    StateImages = ImageList1
    TabOrder = 0
    OnMouseUp = TreeView1MouseUp
  end
  object ImageList1: TImageList
    Left = 48
    Top = 64
  end
end
