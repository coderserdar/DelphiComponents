object NsLookupForm: TNsLookupForm
  Left = 200
  Top = 281
  Caption = 'Name Server Lookup'
  ClientHeight = 230
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 65
    Width = 536
    Height = 165
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 65
    Align = alTop
    TabOrder = 1
    object DnsEdit: TEdit
      Left = 24
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'DnsEdit'
    end
    object NameEdit: TEdit
      Left = 24
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'NameEdit'
    end
    object MXLookupButton: TButton
      Left = 160
      Top = 8
      Width = 75
      Height = 21
      Caption = '&MXLookup'
      TabOrder = 2
      OnClick = MXLookupButtonClick
    end
    object ClearDisplayBitBtn: TBitBtn
      Left = 246
      Top = 8
      Width = 25
      Height = 25
      TabOrder = 3
      OnClick = ClearDisplayBitBtnClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777700000000000777770FFFFFFFFF0777770FFFFFFFFF0777770FFFFFFFFF07
        77770FFFFFFFF00777770F0F00FF0B0777770FFFFFF0BFB077770FFFFF0BFBFB
        07770F00F08FBFBFB0770FFFFFF8FBFBFB070FF00F0F8FBFBFB00FFFFFFFF8FB
        FBF80F00F0FF0F8FBF870FFFFFFFFF08F8770000000000078777}
    end
    object ALookupButton: TButton
      Left = 160
      Top = 32
      Width = 75
      Height = 21
      Caption = '&ALookup'
      TabOrder = 4
      OnClick = ALookupButtonClick
    end
  end
  object DnsQuery1: TDnsQuery
    Port = '53'
    Proto = 'udp'
    MultiThreaded = False
    OnRequestDone = DnsQuery1RequestDone
    Left = 248
    Top = 88
  end
end
