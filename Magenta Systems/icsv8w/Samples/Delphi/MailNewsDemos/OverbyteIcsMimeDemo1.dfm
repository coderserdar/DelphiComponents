object MimeDecodeForm: TMimeDecodeForm
  Left = 0
  Top = 41
  Caption = 'MimeDecodeForm'
  ClientHeight = 462
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 719
    Height = 101
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 16
      Height = 13
      Caption = 'File'
    end
    object Label2: TLabel
      Left = 4
      Top = 40
      Width = 21
      Height = 13
      Caption = 'Text'
    end
    object FileEdit: TEdit
      Left = 32
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'FileEdit'
    end
    object DecodeButton: TButton
      Left = 166
      Top = 8
      Width = 71
      Height = 21
      Caption = '&Decode file'
      Default = True
      TabOrder = 1
      OnClick = DecodeButtonClick
    end
    object ClearButton: TButton
      Left = 248
      Top = 8
      Width = 73
      Height = 21
      Caption = '&Clear'
      TabOrder = 2
      OnClick = ClearButtonClick
    end
    object TextEdit: TEdit
      Left = 32
      Top = 36
      Width = 121
      Height = 21
      TabOrder = 3
      Text = 'TextEdit'
    end
    object Decode64Button: TButton
      Left = 164
      Top = 36
      Width = 75
      Height = 21
      Caption = 'Decode64'
      TabOrder = 4
      OnClick = Decode64ButtonClick
    end
    object Encode64Button: TButton
      Left = 248
      Top = 36
      Width = 75
      Height = 21
      Caption = 'Encode64'
      TabOrder = 5
      OnClick = Encode64ButtonClick
    end
    object DecAutoHeaderButton: TButton
      Left = 338
      Top = 8
      Width = 123
      Height = 21
      Caption = 'Decode &Auto Headers'
      TabOrder = 6
      OnClick = DecAutoHeaderButtonClick
    end
    object DecOneHeaderButton: TButton
      Left = 338
      Top = 35
      Width = 123
      Height = 21
      Caption = 'Decode &One Header'
      TabOrder = 7
      OnClick = DecOneHeaderButtonClick
    end
    object EncodeOneHdrButton: TButton
      Left = 473
      Top = 35
      Width = 123
      Height = 21
      Caption = '&Encode One Header'
      TabOrder = 8
      OnClick = EncodeOneHdrButtonClick
    end
    object DecodeFileExButton: TButton
      Left = 473
      Top = 9
      Width = 123
      Height = 21
      Caption = 'Decode &File Extended'
      TabOrder = 9
      OnClick = DecodeFileExButtonClick
    end
    object IgnoreBlankParts: TCheckBox
      Left = 602
      Top = 12
      Width = 109
      Height = 17
      Caption = 'Ignore Blank Parts'
      TabOrder = 10
    end
    object doCreateMimeList: TButton
      Left = 13
      Top = 68
      Width = 93
      Height = 25
      Caption = 'Mime from List'
      TabOrder = 11
      OnClick = doCreateMimeListClick
    end
    object doMimefromReg: TButton
      Left = 112
      Top = 68
      Width = 99
      Height = 25
      Caption = 'Mime from Registry'
      TabOrder = 12
      OnClick = doMimefromRegClick
    end
    object doMimeFromTypes: TButton
      Left = 217
      Top = 68
      Width = 106
      Height = 25
      Caption = 'Mime from Types File'
      TabOrder = 13
      OnClick = doMimeFromTypesClick
    end
    object doMimeTestExtn: TButton
      Left = 329
      Top = 68
      Width = 99
      Height = 25
      Caption = 'Test Extensions'
      TabOrder = 14
      OnClick = doMimeTestExtnClick
    end
    object doMimeTestContent: TButton
      Left = 434
      Top = 68
      Width = 99
      Height = 25
      Caption = 'Test Contents'
      TabOrder = 15
      OnClick = doMimeTestContentClick
    end
  end
  object MimeDecode1: TMimeDecodeW
    OnHeaderBegin = MimeDecode1HeaderBegin
    OnHeaderLine = MimeDecode1HeaderLine
    OnHeaderEnd = MimeDecode1HeaderEnd
    OnPartHeaderBegin = MimeDecode1PartHeaderBegin
    OnPartHeaderLine = MimeDecode1PartHeaderLine
    OnPartHeaderEnd = MimeDecode1PartHeaderEnd
    OnPartBegin = MimeDecode1PartBegin
    OnPartLine = MimeDecode1PartLine
    OnPartEnd = MimeDecode1PartEnd
    OnInlineDecodeBegin = MimeDecode1InlineDecodeBegin
    OnInlineDecodeLine = MimeDecode1InlineDecodeLine
    OnInlineDecodeEnd = MimeDecode1InlineDecodeEnd
    Left = 51
    Top = 125
  end
  object MimeDecodeEx1: TMimeDecodeEx
    MaxParts = 10
    SkipBlankParts = False
    Left = 107
    Top = 123
  end
  object MimeTypesList1: TMimeTypesList
    LoadOSonDemand = True
    MimeTypesFile = '/etc/mime.types'
    DefaultTypes.Strings = (
      '.htm=text/html'
      '.html=text/html'
      '.gif=image/gif'
      '.bmp=image/bmp'
      '.jpg=image/jpeg'
      '.jpeg=image/jpeg'
      '.tif=image/tiff'
      '.tiff=image/tiff'
      '.txt=text/plain'
      '.css=text/css'
      '.wav=audio/x-wav'
      '.ico=image/x-icon'
      '.wml=text/vnd.wap.wml'
      '.wbmp=image/vnd.wap.wbmp'
      '.wmlc=application/vnd.wap.wmlc'
      '.wmlscript=text/vnd.wap.wmlscript'
      '.wmlscriptc=application/vnd.wap.wmlscriptc'
      '.pdf=application/pdf'
      '.png=image/png'
      '.xml=application/xml'
      '.xhtml=application/xhtml+xml'
      '.zip=application/zip'
      '.exe=application/x-msdownload'
      '.msi=application/x-msdownload'
      '.bin=application/octet-stream'
      '.iso=application/octet-stream')
    MimeTypeSrc = MTypeList
    UnknownType = 'application/octet-stream'
    Left = 155
    Top = 125
  end
end
