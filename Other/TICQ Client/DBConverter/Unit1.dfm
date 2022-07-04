object Form1: TForm1
  Left = 297
  Top = 80
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'TICQDb Example'
  ClientHeight = 357
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 152
    Top = 288
    Width = 75
    Height = 33
    Caption = 'Start parsing'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 328
    Width = 385
    Height = 13
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 385
    Height = 281
    ActivePage = TabSheet1
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Options'
      object ListView1: TListView
        Left = 0
        Top = 0
        Width = 377
        Height = 253
        Align = alClient
        Columns = <
          item
            AutoSize = True
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Contacts'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 377
        Height = 253
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Messages'
      ImageIndex = 2
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 377
        Height = 253
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Self info'
      ImageIndex = 3
      object Memo3: TMemo
        Left = 0
        Top = 0
        Width = 377
        Height = 253
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 342
    Width = 385
    Height = 15
    Panels = <
      item
        Width = -1
      end>
  end
  object ICQDb1: TICQDb
    OnError = ICQDb1Error
    OnParsingStarted = ICQDb1ParsingStarted
    OnParsingFinished = ICQDb1ParsingFinished
    OnProgress = ICQDb1Progress
    OnContactFound = ICQDb1ContactFound
    OnSelfInfoFound = ICQDb1SelfInfoFound
    OnMessageFound = ICQDb1MessageFound
    Left = 8
    Top = 288
  end
end
