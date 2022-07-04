object PktDumpForm: TPktDumpForm
  Left = 169
  Top = 171
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PktDumpForm'
  ClientHeight = 438
  ClientWidth = 669
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    669
    438)
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 669
    Height = 209
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        Caption = '#'
        Width = 23
      end
      item
        Caption = 'Channel'
        Width = 80
      end
      item
        Caption = 'Length'
        Width = 80
      end
      item
        Caption = 'Seq'
        Width = 80
      end
      item
        Caption = 'SNAC'
        Width = 235
      end
      item
        Caption = 'CMD'
        Width = 150
      end>
    ReadOnly = True
    RowSelect = True
    SmallImages = MainForm.IconList
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = ListView1Click
    OnSelectItem = ListView1SelectItem
  end
  object Memo1: TMemo
    Left = 0
    Top = 216
    Width = 669
    Height = 222
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
