object frmCodecsInfo: TfrmCodecsInfo
  Left = 230
  Top = 188
  Width = 718
  Height = 405
  Caption = 'Codecs info'
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 181
    Height = 378
    Align = alLeft
    TabOrder = 0
    object lbCodecs: TListBox
      Left = 1
      Top = 48
      Width = 179
      Height = 290
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
      OnClick = lbCodecsClick
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 179
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object lbACMver: TLabel
        Left = 8
        Top = 8
        Width = 38
        Height = 13
        Caption = 'ACMver'
      end
      object lblCodecs: TLabel
        Left = 9
        Top = 27
        Width = 36
        Height = 13
        Caption = 'Codecs'
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 338
      Width = 179
      Height = 39
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 181
    Top = 0
    Width = 529
    Height = 378
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 529
      Height = 378
      Align = alClient
      TabOrder = 0
      object memCodecDetails: TMemo
        Left = 1
        Top = 48
        Width = 527
        Height = 290
        Align = alClient
        BevelInner = bvNone
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object pnCodecDetails: TPanel
        Left = 1
        Top = 1
        Width = 527
        Height = 47
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Codec details'
        TabOrder = 1
      end
      object Panel6: TPanel
        Left = 1
        Top = 338
        Width = 527
        Height = 39
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
      end
    end
  end
end
