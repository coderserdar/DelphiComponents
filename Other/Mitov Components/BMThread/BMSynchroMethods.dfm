object SynchroMethodsForm: TSynchroMethodsForm
  Left = 105
  Top = 111
  Caption = 'Synchro methods'
  ClientHeight = 302
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 283
    Width = 395
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitTop = 294
    ExplicitWidth = 403
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 395
    Height = 283
    ActivePage = ThreadSynchroTabSheet
    Align = alClient
    TabOrder = 1
    OnChange = PageControlChange
    ExplicitWidth = 403
    ExplicitHeight = 294
    object ThreadSynchroTabSheet: TTabSheet
      Caption = 'Thread synchro methods'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ThreadEventsListView: TListView
        Left = 0
        Top = 0
        Width = 293
        Height = 266
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 120
          end
          item
            Caption = 'Parameters'
          end>
        HideSelection = False
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = ThreadEventsListViewChange
        OnDblClick = ThreadEventsListViewDblClick
        OnEdited = ThreadEventsListViewEdited
        OnKeyPress = ThreadEventsListViewKeyPress
        OnMouseUp = ThreadEventsListViewMouseUp
      end
      object Panel1: TPanel
        Left = 293
        Top = 0
        Width = 102
        Height = 266
        Align = alRight
        BevelOuter = bvLowered
        TabOrder = 1
        object CreateGroupBox: TGroupBox
          Left = 6
          Top = 40
          Width = 89
          Height = 81
          Caption = ' Create '
          TabOrder = 0
          object BtnNoDataEvent: TBitBtn
            Left = 8
            Top = 16
            Width = 73
            Height = 25
            Caption = '&No data'
            TabOrder = 0
            OnClick = BtnNoDataEventClick
          end
          object BtnDataEvent: TBitBtn
            Left = 8
            Top = 48
            Width = 73
            Height = 25
            Caption = 'With &data'
            TabOrder = 1
            OnClick = BtnDataEventClick
          end
        end
        object RenameButton1: TBitBtn
          Left = 14
          Top = 128
          Width = 75
          Height = 25
          Caption = '&Rename'
          TabOrder = 1
          OnClick = RenameButton1Click
        end
        object ShowButton1: TBitBtn
          Left = 14
          Top = 160
          Width = 75
          Height = 25
          Caption = '&Show'
          TabOrder = 2
          OnClick = ThreadEventsListViewDblClick
        end
        object Panel3: TPanel
          Left = 1
          Top = 190
          Width = 100
          Height = 75
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 3
          object CopyCallButton1: TBitBtn
            Left = 14
            Top = 10
            Width = 75
            Height = 25
            Caption = '&Copy call'
            TabOrder = 0
            OnClick = CopyCallButton1Click
          end
          object GenCallButton1: TBitBtn
            Left = 14
            Top = 42
            Width = 75
            Height = 25
            Caption = '&Paste call'
            TabOrder = 1
            OnClick = GenCallButton1Click
          end
        end
        object RefreshButton1: TBitBtn
          Left = 14
          Top = 10
          Width = 75
          Height = 25
          Caption = 'Re&fresh'
          TabOrder = 4
          OnClick = RefreshButton1Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Standard notify events'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object StdEventsListView: TListView
        Left = 0
        Top = 0
        Width = 293
        Height = 266
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 120
          end
          item
            Caption = 'Parameters'
          end>
        HideSelection = False
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = ThreadEventsListViewChange
        OnDblClick = ThreadEventsListViewDblClick
        OnEdited = ThreadEventsListViewEdited
        OnKeyPress = ThreadEventsListViewKeyPress
      end
      object Panel2: TPanel
        Left = 293
        Top = 0
        Width = 102
        Height = 266
        Align = alRight
        BevelOuter = bvLowered
        TabOrder = 1
        object NewStdButton: TBitBtn
          Left = 14
          Top = 56
          Width = 75
          Height = 25
          Caption = '&New'
          TabOrder = 0
          OnClick = BtnNewStdEventClick
        end
        object RenameButton2: TBitBtn
          Left = 14
          Top = 128
          Width = 75
          Height = 25
          Caption = '&Rename'
          TabOrder = 1
          OnClick = RenameButton1Click
        end
        object ShowButton2: TBitBtn
          Left = 14
          Top = 160
          Width = 75
          Height = 25
          Caption = '&Show'
          TabOrder = 2
          OnClick = ThreadEventsListViewDblClick
        end
        object Panel4: TPanel
          Left = 1
          Top = 190
          Width = 100
          Height = 75
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 3
          object CopyCallButton2: TBitBtn
            Left = 14
            Top = 10
            Width = 75
            Height = 25
            Caption = '&Copy call'
            TabOrder = 0
            OnClick = CopyCallButton1Click
          end
          object GenCallButton2: TBitBtn
            Left = 14
            Top = 42
            Width = 75
            Height = 25
            Caption = '&Paste call'
            TabOrder = 1
            OnClick = GenCallButton1Click
          end
        end
        object RefreshButton2: TBitBtn
          Left = 14
          Top = 10
          Width = 75
          Height = 25
          Caption = 'Re&fresh'
          TabOrder = 4
          OnClick = RefreshButton1Click
        end
      end
    end
  end
  object Timer1: TTimer
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 347
    Top = 216
  end
end
