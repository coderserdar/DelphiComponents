object TDelphiXDIBEffectEditForm: TTDelphiXDIBEffectEditForm
  Left = 293
  Top = 183
  BorderStyle = bsDialog
  Caption = 'Special effect'
  ClientHeight = 368
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel3: TBevel
    Left = 8
    Top = 8
    Width = 489
    Height = 321
    Shape = bsFrame
  end
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 473
    Height = 305
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 1
    object Bevel1: TBevel
      Left = 8
      Top = 39
      Width = 258
      Height = 258
    end
    object Image1: TImage
      Left = 9
      Top = 40
      Width = 32
      Height = 32
      AutoSize = True
    end
    object LSpokes: TLabel
      Left = 8
      Top = 8
      Width = 36
      Height = 13
      Caption = 'Spokes'
      FocusControl = Spokes
    end
    object LRaHUE: TLabel
      Left = 160
      Top = 8
      Width = 66
      Height = 13
      Caption = 'Random HUE'
      FocusControl = RaHUE
    end
    object LCentr: TLabel
      Left = 312
      Top = 8
      Width = 57
      Height = 13
      Caption = 'Nova radius'
      FocusControl = Centr
    end
    object Label4: TLabel
      Left = 276
      Top = 44
      Width = 11
      Height = 13
      Caption = 'R:'
    end
    object Label5: TLabel
      Left = 276
      Top = 76
      Width = 11
      Height = 13
      Caption = 'G:'
    end
    object Label6: TLabel
      Left = 276
      Top = 108
      Width = 10
      Height = 13
      Caption = 'B:'
    end
    object Label7: TLabel
      Left = 274
      Top = 140
      Width = 13
      Height = 13
      Hint = 'Random spoke...'
      Caption = 'rS:'
      FocusControl = randSpok
      ParentShowHint = False
      ShowHint = True
    end
    object Label8: TLabel
      Left = 274
      Top = 164
      Width = 14
      Height = 13
      Hint = 'Random gauss...'
      Caption = 'rG:'
      FocusControl = RandGauss
      ParentShowHint = False
      ShowHint = True
    end
    object rS_max: TSpeedButton
      Left = 334
      Top = 136
      Width = 36
      Height = 21
      Caption = 'max'
      Spacing = -1
      OnClick = rS_maxClick
    end
    object rG_max: TSpeedButton
      Left = 334
      Top = 160
      Width = 36
      Height = 21
      Caption = 'max'
      Spacing = -1
      OnClick = rG_maxClick
    end
    object Bevel2: TBevel
      Left = 272
      Top = 187
      Width = 192
      Height = 65
    end
    object SpokesPlus: TSpeedButton
      Left = 124
      Top = 5
      Width = 17
      Height = 17
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        700077777777777770007777700077777000777770C077777000777770C07777
        7000770000C000077000770CCCCCCC077000770000C000077000777770C07777
        7000777770C07777700077777000777770007777777777777000777777777777
        7000}
      OnClick = SpokesPlusClick
    end
    object SpokesMinus: TSpeedButton
      Left = 140
      Top = 5
      Width = 17
      Height = 17
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777777777777700077777777777770007777777777777000777777777777
        70007700000000077000770CCCCCCC0770007700000000077000777777777777
        7000777777777777700077777777777770007777777777777000777777777777
        7000}
      OnClick = SpokesMinusClick
    end
    object RaHUEPlus: TSpeedButton
      Left = 276
      Top = 5
      Width = 17
      Height = 17
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        700077777777777770007777700077777000777770C077777000777770C07777
        7000770000C000077000770CCCCCCC077000770000C000077000777770C07777
        7000777770C07777700077777000777770007777777777777000777777777777
        7000}
      OnClick = RaHUEPlusClick
    end
    object RaHUEMinus: TSpeedButton
      Left = 292
      Top = 5
      Width = 17
      Height = 17
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777777777777700077777777777770007777777777777000777777777777
        70007700000000077000770CCCCCCC0770007700000000077000777777777777
        7000777777777777700077777777777770007777777777777000777777777777
        7000}
      OnClick = RaHUEMinusClick
    end
    object CentrPlus: TSpeedButton
      Left = 428
      Top = 5
      Width = 17
      Height = 17
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        700077777777777770007777700077777000777770C077777000777770C07777
        7000770000C000077000770CCCCCCC077000770000C000077000777770C07777
        7000777770C07777700077777000777770007777777777777000777777777777
        7000}
      OnClick = CentrPlusClick
    end
    object CentrMinus: TSpeedButton
      Left = 444
      Top = 5
      Width = 17
      Height = 17
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777777777777700077777777777770007777777777777000777777777777
        70007700000000077000770CCCCCCC0770007700000000077000777777777777
        7000777777777777700077777777777770007777777777777000777777777777
        7000}
      OnClick = CentrMinusClick
    end
    object LName: TLabel
      Left = 283
      Top = 200
      Width = 112
      Height = 13
      Caption = 'Picture name (required):'
      FocusControl = eName
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object eR: TEdit
      Left = 289
      Top = 40
      Width = 35
      Height = 21
      TabOrder = 0
      Text = '16'
      OnChange = ImageChange
    end
    object eG: TEdit
      Left = 289
      Top = 72
      Width = 35
      Height = 21
      TabOrder = 1
      Text = '48'
      OnChange = ImageChange
    end
    object eB: TEdit
      Left = 289
      Top = 104
      Width = 35
      Height = 21
      TabOrder = 2
      Text = '255'
      OnChange = ImageChange
    end
    object randSpok: TEdit
      Left = 289
      Top = 136
      Width = 44
      Height = 21
      TabOrder = 3
    end
    object RandGauss: TEdit
      Left = 289
      Top = 160
      Width = 44
      Height = 21
      TabOrder = 4
    end
    object PictSize: TRadioGroup
      Left = 385
      Top = 44
      Width = 73
      Height = 121
      Caption = ' Size: '
      ItemIndex = 0
      Items.Strings = (
        '256x256'
        '128x128'
        '64x64'
        '32x62'
        '16x16'
        '8x8')
      TabOrder = 5
      OnClick = ImageChange
    end
    object R_updown: TSpinButton
      Left = 326
      Top = 41
      Width = 20
      Height = 20
      DownGlyph.Data = {
        0E010000424D0E01000000000000360000002800000009000000060000000100
        200000000000D800000000000000000000000000000000000000008080000080
        8000008080000080800000808000008080000080800000808000008080000080
        8000008080000080800000808000000000000080800000808000008080000080
        8000008080000080800000808000000000000000000000000000008080000080
        8000008080000080800000808000000000000000000000000000000000000000
        0000008080000080800000808000000000000000000000000000000000000000
        0000000000000000000000808000008080000080800000808000008080000080
        800000808000008080000080800000808000}
      TabOrder = 6
      UpGlyph.Data = {
        0E010000424D0E01000000000000360000002800000009000000060000000100
        200000000000D800000000000000000000000000000000000000008080000080
        8000008080000080800000808000008080000080800000808000008080000080
        8000000000000000000000000000000000000000000000000000000000000080
        8000008080000080800000000000000000000000000000000000000000000080
        8000008080000080800000808000008080000000000000000000000000000080
        8000008080000080800000808000008080000080800000808000000000000080
        8000008080000080800000808000008080000080800000808000008080000080
        800000808000008080000080800000808000}
      OnDownClick = R_updownDownClick
      OnUpClick = R_updownUpClick
    end
    object G_updown: TSpinButton
      Left = 326
      Top = 73
      Width = 20
      Height = 20
      DownGlyph.Data = {
        0E010000424D0E01000000000000360000002800000009000000060000000100
        200000000000D800000000000000000000000000000000000000008080000080
        8000008080000080800000808000008080000080800000808000008080000080
        8000008080000080800000808000000000000080800000808000008080000080
        8000008080000080800000808000000000000000000000000000008080000080
        8000008080000080800000808000000000000000000000000000000000000000
        0000008080000080800000808000000000000000000000000000000000000000
        0000000000000000000000808000008080000080800000808000008080000080
        800000808000008080000080800000808000}
      TabOrder = 8
      UpGlyph.Data = {
        0E010000424D0E01000000000000360000002800000009000000060000000100
        200000000000D800000000000000000000000000000000000000008080000080
        8000008080000080800000808000008080000080800000808000008080000080
        8000000000000000000000000000000000000000000000000000000000000080
        8000008080000080800000000000000000000000000000000000000000000080
        8000008080000080800000808000008080000000000000000000000000000080
        8000008080000080800000808000008080000080800000808000000000000080
        8000008080000080800000808000008080000080800000808000008080000080
        800000808000008080000080800000808000}
      OnDownClick = G_updownDownClick
      OnUpClick = G_updownUpClick
    end
    object B_updown: TSpinButton
      Left = 326
      Top = 105
      Width = 20
      Height = 20
      DownGlyph.Data = {
        0E010000424D0E01000000000000360000002800000009000000060000000100
        200000000000D800000000000000000000000000000000000000008080000080
        8000008080000080800000808000008080000080800000808000008080000080
        8000008080000080800000808000000000000080800000808000008080000080
        8000008080000080800000808000000000000000000000000000008080000080
        8000008080000080800000808000000000000000000000000000000000000000
        0000008080000080800000808000000000000000000000000000000000000000
        0000000000000000000000808000008080000080800000808000008080000080
        800000808000008080000080800000808000}
      TabOrder = 7
      UpGlyph.Data = {
        0E010000424D0E01000000000000360000002800000009000000060000000100
        200000000000D800000000000000000000000000000000000000008080000080
        8000008080000080800000808000008080000080800000808000008080000080
        8000000000000000000000000000000000000000000000000000000000000080
        8000008080000080800000000000000000000000000000000000000000000080
        8000008080000080800000808000008080000000000000000000000000000080
        8000008080000080800000808000008080000080800000808000000000000080
        8000008080000080800000808000008080000080800000808000008080000080
        800000808000008080000080800000808000}
      OnDownClick = B_updownDownClick
      OnUpClick = B_updownUpClick
    end
    object Spokes: TProgressBar
      Left = 8
      Top = 24
      Width = 150
      Height = 9
      Min = 1
      Max = 1024
      Position = 255
      TabOrder = 9
      OnMouseMove = SpokesMouseMove
    end
    object RaHUE: TProgressBar
      Left = 160
      Top = 24
      Width = 150
      Height = 9
      Min = 0
      Max = 360
      TabOrder = 10
      OnMouseMove = RaHUEMouseMove
    end
    object Centr: TProgressBar
      Left = 312
      Top = 24
      Width = 150
      Height = 9
      Min = 1
      Max = 100
      Position = 1
      TabOrder = 11
      OnMouseMove = CentrMouseMove
    end
    object eName: TEdit
      Left = 283
      Top = 216
      Width = 161
      Height = 21
      TabOrder = 12
    end
  end
  object Button1: TButton
    Left = 264
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Save as...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object btnOK: TButton
    Left = 344
    Top = 336
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 421
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object SavePictureDialog1: TSavePictureDialog
    DefaultExt = '.bmp'
    Filter = 
      'All (*.dib;*.jpg;*.jpeg;*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf)|*.' +
      'dib;*.jpg;*.jpeg;*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf|Device Ind' +
      'ependent Bitmap (*.dib)|*.dib|JPEG Image File (*.jpg)|*.jpg|JPEG' +
      ' Image File (*.jpeg)|*.jpeg|JPEG Image File (*.jpg)|*.jpg|JPEG I' +
      'mage File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.' +
      'ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf'
    Title = 'Save NOVA picture effect'
    Left = 160
    Top = 304
  end
end
