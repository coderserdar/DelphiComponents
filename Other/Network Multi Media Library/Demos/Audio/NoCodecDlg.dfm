object frmNoCodec: TfrmNoCodec
  Left = 409
  Top = 239
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'No codec'
  ClientHeight = 184
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 141
    Top = 152
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object Panel1: TPanel
    Left = 9
    Top = 7
    Width = 331
    Height = 137
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 13
      Top = 11
      Width = 305
      Height = 79
      AutoSize = False
      Caption = 
        'This software was designed to use Speex freeware codec. It can a' +
        'lso work with some others (e.g. Mp3, Vorbis etc) but before test' +
        'ing it with other than Speex codecs take care to avoid any heari' +
        'ng damage: set your headphones volume lower or use speakers - mi' +
        'susing of some codecs can result playing a loud noise instead of' +
        ' proper sounds.'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 13
      Top = 98
      Width = 210
      Height = 13
      Caption = 'You can download Speex for Windows from:'
    end
    object lbCodecUrl: TLabel
      Left = 13
      Top = 113
      Width = 291
      Height = 13
      Caption = 'http://www.republika.pl/roed/speexw/download/speexw.exe'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      OnClick = lbCodecUrlClick
    end
  end
end
