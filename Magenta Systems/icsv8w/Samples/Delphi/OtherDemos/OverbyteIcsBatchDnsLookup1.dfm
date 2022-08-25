object BatchDnsLookupForm: TBatchDnsLookupForm
  Left = 202
  Top = 118
  BorderStyle = bsToolWindow
  Caption = 
    'ICS batch async DNS lookup new API (IPv6 and IPv4) - V8.64 12th ' +
    'March 2020'
  ClientHeight = 621
  ClientWidth = 786
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
  OnShow = FormShow
  DesignSize = (
    786
    621)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 556
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Min. #'
    ExplicitTop = 298
  end
  object Label2: TLabel
    Left = 69
    Top = 556
    Width = 35
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Max. #'
    ExplicitTop = 298
  end
  object Label3: TLabel
    Left = 8
    Top = 537
    Width = 295
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Lookup threads shared by all instances in this thread context '
    ExplicitTop = 279
  end
  object Label4: TLabel
    Left = 8
    Top = 517
    Width = 149
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Number of TWSocket instances'
    ExplicitTop = 259
  end
  object Label5: TLabel
    Left = 8
    Top = 576
    Width = 62
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'SocketFamily'
    ExplicitTop = 318
  end
  object Label6: TLabel
    Left = 132
    Top = 556
    Width = 157
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '(Max - Min time out after 60 sec)'
    ExplicitTop = 298
  end
  object Label7: TLabel
    Left = 8
    Top = 8
    Width = 135
    Height = 13
    Caption = 'International Domain Names'
  end
  object Label8: TLabel
    Left = 190
    Top = 8
    Width = 135
    Height = 13
    Caption = 'IPv4 or IPv6 Address Result'
  end
  object Label9: TLabel
    Left = 381
    Top = 8
    Width = 122
    Height = 13
    Caption = 'Punycode ASCII Encoded'
  end
  object Label10: TLabel
    Left = 592
    Top = 8
    Width = 83
    Height = 13
    Caption = 'Unicode Decoded'
  end
  object StartButton: TButton
    Left = 223
    Top = 590
    Width = 88
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Start Lookup'
    TabOrder = 6
    OnClick = StartButtonClick
  end
  object DnsNamesMemo: TMemo
    Left = 8
    Top = 27
    Width = 168
    Height = 481
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'DnsNamesMemo')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnDblClick = DnsNamesMemoDblClick
  end
  object ResultMemo: TMemo
    Left = 190
    Top = 27
    Width = 185
    Height = 481
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'ResultMemo')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object MinEdit: TEdit
    Left = 40
    Top = 553
    Width = 23
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Text = '0'
  end
  object MaxEdit: TEdit
    Left = 105
    Top = 553
    Width = 23
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    Text = '2'
  end
  object InstancesEdit: TEdit
    Left = 159
    Top = 514
    Width = 36
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Text = '3'
  end
  object SocketFamilyComboBox: TComboBox
    Left = 8
    Top = 592
    Width = 157
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 5
    Text = 'sfAny'
    OnChange = SocketFamilyComboBoxChange
    Items.Strings = (
      'sfAny'
      'sfAnyIPv4'
      'sfAnyIPv6'
      'sfIPv4 (old API)'
      'sfIPv6')
  end
  object UseThread: TCheckBox
    Left = 210
    Top = 517
    Width = 131
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Use Thread (new API)'
    TabOrder = 7
  end
  object IDNMemo: TMemo
    Left = 381
    Top = 27
    Width = 205
    Height = 481
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 8
    WordWrap = False
  end
  object doIDNEncode: TButton
    Left = 335
    Top = 590
    Width = 86
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'IDN Encode'
    TabOrder = 9
    OnClick = doIDNEncodeClick
  end
  object DecodeMemo: TMemo
    Left = 592
    Top = 27
    Width = 184
    Height = 481
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 10
    WordWrap = False
  end
end
