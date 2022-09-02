object frmDict: TfrmDict
  Left = 178
  Top = 189
  Width = 696
  Height = 480
  Caption = 'Dictionary'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 688
    Height = 426
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'Courier'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object PrintDialog1: TPrintDialog
    Options = [poPrintToFile, poPageNums, poSelection]
    Left = 198
    Top = 42
  end
  object MainMenu1: TMainMenu
    Left = 144
    Top = 123
    object Print1: TMenuItem
      Caption = 'Print'
      object Print2: TMenuItem
        Caption = 'Print'
        OnClick = Print2Click
      end
    end
  end
end
