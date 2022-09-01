object Form1: TForm1
  Left = 197
  Top = 218
  BorderStyle = bsSingle
  Caption = 'Demo Virtual Dataset'
  ClientHeight = 378
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 104
    Top = 14
    Width = 37
    Height = 13
    Caption = 'Jump to'
  end
  object Label2: TLabel
    Left = 16
    Top = 360
    Width = 18
    Height = 13
    Caption = '???'
  end
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open dataset'
    TabOrder = 0
    OnClick = Button1Click
  end
  object edRecNo: TEdit
    Left = 152
    Top = 10
    Width = 81
    Height = 21
    TabOrder = 1
    OnKeyDown = edRecNoKeyDown
  end
  object btnRecNoGo: TButton
    Left = 232
    Top = 10
    Width = 33
    Height = 21
    Caption = 'go'
    TabOrder = 2
    OnClick = btnRecNoGoClick
  end
  object DBGrid1: TDBGrid
    Left = 16
    Top = 40
    Width = 320
    Height = 129
    DataSource = DataSource1
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Name'
        Width = 133
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Age'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Birthday'
        Width = 84
        Visible = True
      end>
  end
  object GroupBox1: TGroupBox
    Left = 342
    Top = 40
    Width = 273
    Height = 233
    Caption = 'Properties'
    TabOrder = 4
    object Label3: TLabel
      Left = 8
      Top = 18
      Width = 28
      Height = 13
      Caption = 'Name'
    end
    object Label4: TLabel
      Left = 8
      Top = 58
      Width = 19
      Height = 13
      Caption = 'Age'
    end
    object Label5: TLabel
      Left = 8
      Top = 98
      Width = 38
      Height = 13
      Caption = 'Birthday'
    end
    object DBEdit1: TDBEdit
      Left = 8
      Top = 32
      Width = 121
      Height = 21
      DataField = 'Name'
      DataSource = DataSource1
      TabOrder = 0
    end
    object DBEdit2: TDBEdit
      Left = 8
      Top = 72
      Width = 121
      Height = 21
      DataField = 'Age'
      DataSource = DataSource1
      TabOrder = 1
    end
    object DBEdit3: TDBEdit
      Left = 8
      Top = 112
      Width = 121
      Height = 21
      DataField = 'Birthday'
      DataSource = DataSource1
      TabOrder = 2
    end
    object DBMemo1: TDBMemo
      Left = 8
      Top = 144
      Width = 249
      Height = 81
      DataField = 'memo'
      DataSource = DataSource1
      TabOrder = 3
    end
  end
  object DBGrid3: TDBGrid
    Left = 16
    Top = 184
    Width = 321
    Height = 113
    DataSource = DataSource2
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Description'
        Width = 206
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OrderDate'
        Width = 78
        Visible = True
      end>
  end
  object Memo1: TMemo
    Left = 14
    Top = 301
    Width = 601
    Height = 33
    TabStop = False
    BorderStyle = bsNone
    Color = clInfoBk
    Lines.Strings = (
      
        'This page demonstrates two TSnapVirtualDataset components connec' +
        'ted to eachother in a master/detail configuration.')
    ReadOnly = True
    TabOrder = 6
  end
  object Button2: TButton
    Left = 542
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 7
    OnClick = Button2Click
  end
  object DataSource1: TDataSource
    DataSet = vdsNamesDataset
    Left = 24
    Top = 344
  end
  object DataSource2: TDataSource
    DataSet = vdsOrdersDataset
    Left = 112
    Top = 344
  end
  object vdsNamesDataset: TSnapVirtualDataset
    AfterScroll = vdsNamesDatasetAfterScroll
    OnGetDataValue = vdsNamesDatasetGetDataValue
    OnGetDataCount = vdsNamesDatasetGetDataCount
    OnGetDataBlobValue = vdsNamesDatasetGetDataBlobValue
    OnPostData = vdsNamesDatasetPostData
    OnDeleteData = vdsNamesDatasetDeleteData
    Left = 56
    Top = 344
    object vdsNamesDatasetName: TStringField
      FieldName = 'Name'
      Size = 50
    end
    object vdsNamesDatasetAge: TIntegerField
      FieldName = 'Age'
    end
    object vdsNamesDatasetBirthday: TDateTimeField
      FieldName = 'Birthday'
    end
    object vdsNamesDatasetmemo: TMemoField
      FieldName = 'memo'
      BlobType = ftMemo
    end
  end
  object vdsOrdersDataset: TSnapVirtualDataset
    BeforeInsert = vdsOrdersDatasetBeforeInsert
    MasterSource = DataSource1
    OnGetDataValue = vdsOrdersDatasetGetDataValue
    OnGetDataCount = vdsOrdersDatasetGetDataCount
    OnPostData = vdsOrdersDatasetPostData
    OnDeleteData = vdsOrdersDatasetDeleteData
    Left = 144
    Top = 344
    object vdsOrdersDatasetDescription: TStringField
      FieldName = 'Description'
      Size = 50
    end
    object vdsOrdersDatasetOrderDate: TDateField
      FieldName = 'OrderDate'
    end
  end
end
