object TemplateForm: TTemplateForm
  Left = 530
  Top = 117
  Width = 453
  Height = 299
  Caption = 'TemplateForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 445
    Height = 41
    Align = alTop
    TabOrder = 0
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 445
    Height = 224
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    TabOrder = 1
  end
end
