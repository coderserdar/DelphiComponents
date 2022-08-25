object ViewQuForm: TViewQuForm
  Left = 0
  Top = 0
  Caption = 'View Mail Queue (double click to delete item)'
  ClientHeight = 274
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object QuListView: TListView
    Left = 0
    Top = 0
    Width = 635
    Height = 233
    Hint = 'Double click to delete item from queue!!!'
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Item'
        MaxWidth = 60
        MinWidth = 50
      end
      item
        AutoSize = True
        Caption = 'Last Response'
        MaxWidth = 250
        MinWidth = 100
      end
      item
        AutoSize = True
        Caption = 'Next Attempt'
        MaxWidth = 120
        MinWidth = 120
      end
      item
        AutoSize = True
        Caption = 'Mail To'
        MaxWidth = 250
        MinWidth = 100
      end
      item
        AutoSize = True
        Caption = 'Mail From'
        MaxWidth = 200
        MinWidth = 100
      end
      item
        AutoSize = True
        Caption = 'Subject'
        MaxWidth = 250
        MinWidth = 100
      end
      item
        AutoSize = True
        Caption = 'Size'
        MaxWidth = 60
        MinWidth = 50
      end
      item
        AutoSize = True
        Caption = 'EML File'
        MaxWidth = 200
        MinWidth = 110
      end
      item
        AutoSize = True
        Caption = 'Queued At'
        MaxWidth = 120
        MinWidth = 120
      end
      item
        AutoSize = True
        Caption = 'Last Attempt'
        MaxWidth = 120
        MinWidth = 120
      end
      item
        AutoSize = True
        Caption = 'Attpts'
        MaxWidth = 60
        MinWidth = 60
      end
      item
        AutoSize = True
        Caption = 'Pri'
        MaxWidth = 50
        MinWidth = 50
      end
      item
        AutoSize = True
        Caption = 'Method'
        MaxWidth = 150
        MinWidth = 100
      end
      item
        AutoSize = True
        Caption = 'SMTP Servers'
        MaxWidth = 300
        MinWidth = 150
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = QuListViewDblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 233
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 1
    object LabelState: TLabel
      Left = 195
      Top = 15
      Width = 65
      Height = 13
      Caption = 'Queue State:'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object doUpdate: TButton
      Left = 20
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Update'
      TabOrder = 0
      OnClick = doUpdateClick
    end
    object doClose: TButton
      Left = 110
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = doCloseClick
    end
  end
end
