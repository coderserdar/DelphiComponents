object fconnect: Tfconnect
  Left = 376
  Top = 327
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  BorderWidth = 4
  Caption = '  Register database'
  ClientHeight = 358
  ClientWidth = 384
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 360
    Top = 72
    Width = 21
    Height = 21
    Flat = True
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      5555555555555555555555555555555555555555555555555555555555555555
      555555555555555555555555555555555555555FFFFFFFFFF555550000000000
      55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
      B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
      000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
      555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
      55555575FFF75555555555700007555555555557777555555555555555555555
      5555555555555555555555555555555555555555555555555555}
    NumGlyphs = 2
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 80
    Height = 13
    Caption = 'Database file:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 256
    Width = 64
    Height = 13
    Caption = 'DLL driver:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SpeedButton2: TSpeedButton
    Left = 360
    Top = 272
    Width = 21
    Height = 21
    Flat = True
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00370777033333
      3330337F3F7F33333F3787070003333707303F737773333373F7007703333330
      700077337F3333373777887007333337007733F773F333337733700070333333
      077037773733333F7F37703707333300080737F373333377737F003333333307
      78087733FFF3337FFF7F33300033330008073F3777F33F777F73073070370733
      078073F7F7FF73F37FF7700070007037007837773777F73377FF007777700730
      70007733FFF77F37377707700077033707307F37773F7FFF7337080777070003
      3330737F3F7F777F333778080707770333333F7F737F3F7F3333080787070003
      33337F73FF737773333307800077033333337337773373333333}
    NumGlyphs = 2
    OnClick = SpeedButton2Click
  end
  object Label3: TLabel
    Left = 8
    Top = 4
    Width = 30
    Height = 13
    Caption = 'Title:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edb: TEdit
    Left = 8
    Top = 72
    Width = 352
    Height = 21
    BevelKind = bkTile
    BorderStyle = bsNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    OnChange = eChange
  end
  object edll: TEdit
    Left = 8
    Top = 272
    Width = 352
    Height = 21
    BevelKind = bkFlat
    BorderStyle = bsNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 3
    OnChange = eChange
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 128
    Width = 352
    Height = 81
    Caption = 'New database encoding'
    Ctl3D = True
    Items.Strings = (
      'UTF-8'
      'UTF-16 little endian'
      'UTF-16 big endian')
    ParentCtl3D = False
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 312
    Width = 384
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    Color = 16640730
    TabOrder = 4
    object Button1: TButton
      Left = 192
      Top = 12
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 280
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object checkexists: TCheckBox
    Left = 8
    Top = 104
    Width = 337
    Height = 17
    Caption = 'Create new database'
    TabOrder = 1
  end
  object etitle: TEdit
    Left = 8
    Top = 20
    Width = 352
    Height = 21
    BevelKind = bkFlat
    BorderStyle = bsNone
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 5
    OnChange = eChange
  end
  object checkforeign: TCheckBox
    Left = 8
    Top = 224
    Width = 337
    Height = 17
    Caption = 'Foreign Key Support'
    TabOrder = 6
  end
  object OpenD: TOpenDialog
    Filter = 'Database file (*.dbs, *.db3)|*.dbs;*.db3'
    Left = 312
    Top = 32
  end
end
