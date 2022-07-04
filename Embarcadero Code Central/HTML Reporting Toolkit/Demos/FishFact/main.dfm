object FactFrm: TFactFrm
  Left = 294
  Top = 245
  Width = 645
  Height = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object WebBrowserControl1: TWebBrowserControl
    Left = 0
    Top = 0
    Width = 637
    Height = 374
    Align = alClient
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnTitleChange = WebBrowserControl1TitleChange
    UIFlags = [uiNo3DBorder, uiFlatScrollbar]
    DCTLFlags = []
    CSSStyles.Strings = (
      'body      { font-family: "Verdana, Arial, Helvetica";'
      '            font-size: "10pt" }'
      ''
      'table     { font-family: "Verdana, Tahoma"; font-size: "11px" }')
    ControlData = {
      4C000000D6410000A72600000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object WebProvider1: TWebProvider
    XMLParserLevel = MSXML20
    Active = False
    Items = <
      item
        XSLT = False
        CacheTemplates = False
        MimeType = 'text/html'
        Enabled = True
        Default = False
        Location = './page.htm'
        Name = 'WebDisp1'
        OnAction = WebProvider1Items0Action
      end
      item
        XSLT = False
        CacheTemplates = False
        Enabled = True
        Default = False
        Location = './image.bmp'
        Name = 'WebDisp2'
        OnAction = WebProvider1Items1Action
      end>
    RefreshDelay = 300
    Protocol = 'local'
    Cached = False
    Left = 72
    Top = 32
  end
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'biolife.db'
    Left = 104
    Top = 32
    object Table1SpeciesNo: TFloatField
      FieldName = 'Species No'
    end
    object Table1Category: TStringField
      FieldName = 'Category'
      Size = 15
    end
    object Table1Common_Name: TStringField
      FieldName = 'Common_Name'
      Size = 30
    end
    object Table1SpeciesName: TStringField
      FieldName = 'Species Name'
      Size = 40
    end
    object Table1Lengthcm: TFloatField
      FieldName = 'Length (cm)'
    end
    object Table1Length_In: TFloatField
      FieldName = 'Length_In'
    end
    object Table1Notes: TMemoField
      FieldName = 'Notes'
      BlobType = ftMemo
      Size = 50
    end
    object Table1Graphic: TGraphicField
      FieldName = 'Graphic'
      BlobType = ftGraphic
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 136
    Top = 32
    object NextRecord1: TMenuItem
      Caption = 'Next Record'
      OnClick = NextRecord1Click
    end
    object PriorRecord1: TMenuItem
      Caption = 'Prior Record'
      OnClick = PriorRecord1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object FirstRecord1: TMenuItem
      Caption = 'First Record'
      OnClick = FirstRecord1Click
    end
    object LastRecord1: TMenuItem
      Caption = 'Last Record'
      OnClick = LastRecord1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Print1: TMenuItem
      Caption = 'Print'
      OnClick = Print1Click
    end
    object Preview1: TMenuItem
      Caption = 'Preview'
      OnClick = Preview1Click
    end
  end
end
