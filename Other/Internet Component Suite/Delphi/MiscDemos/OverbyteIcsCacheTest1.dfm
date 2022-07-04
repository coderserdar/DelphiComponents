object CacheTestForm: TCacheTestForm
  Left = 295
  Top = 143
  Caption = 'Cache Test'
  ClientHeight = 326
  ClientWidth = 440
  Color = clBtnFace
  Constraints.MinHeight = 226
  Constraints.MinWidth = 364
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    440
    326)
  PixelsPerInch = 96
  TextHeight = 13
  object lbResult: TLabel
    Left = 6
    Top = 8
    Width = 38
    Height = 13
    Caption = 'lbResult'
  end
  object lbRoot: TLabel
    Left = 178
    Top = 8
    Width = 31
    Height = 13
    Caption = 'lbRoot'
  end
  object lbCount: TLabel
    Left = 304
    Top = 8
    Width = 37
    Height = 13
    Caption = 'lbCount'
  end
  object lbCaption: TLabel
    Left = 8
    Top = 30
    Width = 3
    Height = 13
  end
  object btnInsert: TButton
    Left = 235
    Top = 286
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'Insert key'
    TabOrder = 0
    OnClick = btnInsertClick
  end
  object btnListTree: TButton
    Left = 8
    Top = 204
    Width = 75
    Height = 23
    Hint = 'List entries to memo (may take a while)'
    Anchors = [akLeft, akBottom]
    Caption = 'List'
    TabOrder = 1
    OnClick = btnListTreeClick
  end
  object btnSearch: TButton
    Left = 159
    Top = 286
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'Search Key'
    TabOrder = 2
    OnClick = btnSearchClick
  end
  object SpinEdit1: TSpinEdit
    Left = 311
    Top = 258
    Width = 121
    Height = 22
    Anchors = [akRight, akBottom]
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 100000
  end
  object Edit1: TEdit
    Left = 311
    Top = 286
    Width = 121
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 4
    Text = '997'
  end
  object btnOldest: TButton
    Left = 8
    Top = 230
    Width = 75
    Height = 23
    Hint = 'Display oldest cache entry'
    Anchors = [akLeft, akBottom]
    Caption = 'Oldest'
    TabOrder = 5
    OnClick = btnOldestClick
  end
  object btnFill: TButton
    Left = 235
    Top = 258
    Width = 75
    Height = 23
    Hint = 'Fill the cache with n entries'
    Anchors = [akRight, akBottom]
    Caption = 'Fill n'
    TabOrder = 6
    OnClick = btnFillClick
  end
  object btnRemove: TButton
    Left = 83
    Top = 286
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'Remove Key'
    TabOrder = 7
    OnClick = btnRemoveClick
  end
  object btnClear: TButton
    Left = 160
    Top = 204
    Width = 75
    Height = 23
    Hint = 'Clear the cache faster'
    Anchors = [akLeft, akBottom]
    Caption = 'Clear (faster)'
    TabOrder = 8
    OnClick = btnClearClick
  end
  object btnDeleteRoot: TButton
    Left = 84
    Top = 204
    Width = 75
    Height = 23
    Hint = 'Clear the cache'
    Anchors = [akLeft, akBottom]
    Caption = 'Clear (roots)'
    TabOrder = 9
    OnClick = btnDeleteRootClick
  end
  object btnClearMemo: TButton
    Left = 357
    Top = 204
    Width = 75
    Height = 23
    Hint = 'Clear memo'
    Anchors = [akRight, akBottom]
    Caption = 'Clear Memo'
    TabOrder = 10
    OnClick = btnClearMemoClick
  end
  object btnPerform: TButton
    Left = 84
    Top = 230
    Width = 75
    Height = 23
    Hint = 'Run a simple performance test'
    Anchors = [akLeft, akBottom]
    Caption = 'Perform'
    TabOrder = 11
    OnClick = btnPerformClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 44
    Width = 440
    Height = 150
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 12
    OnChange = Memo1Change
  end
  object btnFlush: TButton
    Left = 235
    Top = 230
    Width = 75
    Height = 23
    Hint = 'Delete entries older than..'
    Anchors = [akRight, akBottom]
    Caption = 'Flush (up to)'
    TabOrder = 13
    OnClick = btnFlushClick
  end
  object DateTimePicker1: TDateTimePicker
    Left = 311
    Top = 231
    Width = 121
    Height = 21
    Anchors = [akRight, akBottom]
    Date = 38813.845154629630000000
    Time = 38813.845154629630000000
    Kind = dtkTime
    TabOrder = 14
  end
end
