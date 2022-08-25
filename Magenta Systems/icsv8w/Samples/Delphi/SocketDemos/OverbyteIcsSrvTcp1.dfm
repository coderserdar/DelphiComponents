object GetGroupsForm: TGetGroupsForm
  Left = 345
  Top = 180
  Caption = 'ICS Tcp Server - http://www.overbyte.be'
  ClientHeight = 241
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 404
    Height = 41
    Align = alTop
    TabOrder = 0
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 404
    Height = 200
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    TabOrder = 1
  end
end
