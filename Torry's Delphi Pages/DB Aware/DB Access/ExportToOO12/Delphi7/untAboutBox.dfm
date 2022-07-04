object frmAbout: TfrmAbout
  Left = 416
  Top = 279
  Width = 329
  Height = 264
  BorderIcons = []
  Caption = 'About'
  Color = clMedGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 20
    Top = 16
    Width = 281
    Height = 161
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clAqua
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 8
      Top = 8
      Width = 57
      Height = 49
      Picture.Data = {
        07544269746D617076020000424D760200000000000076000000280000002000
        0000200000000100040000000000000200000000000000000000100000000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00000000000000000000000000000000000EE8787878EEEEEEE03F30878EEE
        EEE00EE8787878EEEEEEE03F30878EEEEEE00EE8787878EEEEEEE03F30878EEE
        EEE00EE8787878EEEEEEE03F30878EEEEEE00887787877788888803F3088787E
        EEE00788787878878887803F3088887EEEE00788887888878887803F3088887E
        EEE00877888887788888703F308887EEEEE00888777778888888037883088888
        8EE007777777777777703787883087777EE00888888888888803787FF8830888
        888008888888888880378777778830888880077777777788037873F3F3F87808
        88E00888888888803787FFFFFFFF8830EEE00887777778800001111111111100
        EEE00888888888888899B999B99999EEEEE00888888888888899B9B99BB9B9EE
        EEE0088888888888899BB9BB99BB99EEEEE0078888888888899B999B999999EE
        EEE0087788888778899B9B9BB9BB99EEEEE00888778778888E9B9B9BB9999EEE
        EEE0088888788888EE9B99B9BB9BEEEEEEE00EE8888888EEEEE999B9999EEEEE
        EEE00EEEE888EEEEEEEE99BB999EEEEEEEE00EEEEE8EEEEEEEEEE999B9EEEEEE
        EEE00EEEEE8EEEEEEEEEEEE999EEEEEEEEE00EEEEE8EEEEEEEEEEEEE99EEEEEE
        EEE00EEEEE8EEEEEEEEEEEEE9EEEEEEEEEE00EEEEE8EEEEEEEEEEEEEEEEEEEEE
        EEE00EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE00000000000000000000000000000
        0000}
      Stretch = True
      IsControl = True
    end
    object ProductName: TLabel
      Left = 72
      Top = 8
      Width = 71
      Height = 17
      Caption = 'Product : '
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'CenturySchTEE'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object Version: TLabel
      Left = 72
      Top = 40
      Width = 54
      Height = 17
      Caption = 'Version'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'CenturySchTEE'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object Copyright: TLabel
      Left = 8
      Top = 80
      Width = 69
      Height = 17
      Caption = 'Copyright'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'CenturySchTEE'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object Label1: TLabel
      Left = 80
      Top = 24
      Width = 186
      Height = 14
      Caption = 'Export DataSet To OpenOffice Sheet'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'CenturySchTEE'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
    object Label3: TLabel
      Left = 24
      Top = 100
      Width = 113
      Height = 15
      Caption = 'All Rights Reserved !'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'CenturySchTEE'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
    object Label2: TLabel
      Left = 88
      Top = 56
      Width = 23
      Height = 19
      Caption = '1.0'
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'CenturySchTEE'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object StaticText1: TStaticText
      Left = 8
      Top = 128
      Width = 265
      Height = 20
      Alignment = taCenter
      Caption = 'TSoft E-mail : tsoft@atw.hu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
  end
  object OKButton: TButton
    Left = 123
    Top = 196
    Width = 75
    Height = 25
    Caption = 'OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
  end
end
