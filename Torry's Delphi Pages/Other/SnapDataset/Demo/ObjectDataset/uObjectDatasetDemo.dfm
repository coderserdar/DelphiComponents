object FrmObjectDatasetDemo: TFrmObjectDatasetDemo
  Left = 150
  Top = 82
  Width = 961
  Height = 649
  Caption = 'Object Dataset Demo'
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
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 953
    Height = 200
    Align = alClient
    DataSource = dsMain
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Available'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Boolean'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ByteBool'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'WordBool'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LongBool'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Shortint'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Byte'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Smallint'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Word'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Integer'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Longint'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Cardinal'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LongWord'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Int64'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Character'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'AnsiName'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'WideName'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShortName'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Date'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Time'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DateTime'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Real'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Money'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Double'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Extended'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Picture'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Memo'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SubItems'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Calc'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Calc2'
        Visible = True
      end>
  end
  object Panel2: TPanel
    Left = 0
    Top = 200
    Width = 953
    Height = 421
    Align = alBottom
    TabOrder = 1
    object Label4: TLabel
      Left = 8
      Top = 48
      Width = 32
      Height = 13
      Caption = 'Memo:'
      FocusControl = DBMemo1
    end
    object Label21: TLabel
      Left = 8
      Top = 194
      Width = 36
      Height = 13
      Caption = 'Picture:'
      FocusControl = DBImage1
    end
    object LabelInfo: TLabel
      Left = 1
      Top = 407
      Width = 951
      Height = 13
      Align = alBottom
      Caption = 'LabelInfo'
    end
    object DBMemo1: TDBMemo
      Left = 8
      Top = 60
      Width = 369
      Height = 125
      DataField = 'Memo'
      DataSource = dsMain
      TabOrder = 0
    end
    object DBNavigator1: TDBNavigator
      Left = 4
      Top = 4
      Width = 230
      Height = 25
      DataSource = dsMain
      TabOrder = 1
    end
    object Button1: TButton
      Left = 239
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Insert some'
      TabOrder = 2
      OnClick = Button1Click
    end
    object CheckBox1: TCheckBox
      Left = 86
      Top = 34
      Width = 105
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Filter on Available'
      TabOrder = 3
      OnClick = CheckBox1Click
    end
    object DBCheckBox1: TDBCheckBox
      Left = 384
      Top = 12
      Width = 87
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Available'
      DataField = 'Available'
      DataSource = dsMain
      TabOrder = 4
      ValueChecked = 'True'
      ValueUnchecked = 'False'
    end
    object DBImage1: TDBImage
      Left = 8
      Top = 210
      Width = 361
      Height = 191
      DataField = 'Picture'
      DataSource = dsMain
      TabOrder = 5
    end
    object GroupBox1: TGroupBox
      Left = 597
      Top = 226
      Width = 332
      Height = 187
      Caption = 'Sub items'
      TabOrder = 6
      object Label22: TLabel
        Left = 9
        Top = 163
        Width = 9
        Height = 13
        Caption = 'Id'
        FocusControl = DBEdit11
      end
      object Label23: TLabel
        Left = 97
        Top = 163
        Width = 28
        Height = 13
        Caption = 'Name'
        FocusControl = DBEdit12
      end
      object DBGrid2: TDBGrid
        Left = 7
        Top = 16
        Width = 314
        Height = 113
        DataSource = dsSubItems
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete]
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBEdit11: TDBEdit
        Left = 25
        Top = 159
        Width = 64
        Height = 21
        DataField = 'Id'
        DataSource = dsSubItems
        TabOrder = 1
      end
      object DBEdit12: TDBEdit
        Left = 129
        Top = 159
        Width = 192
        Height = 21
        DataField = 'Name'
        DataSource = dsSubItems
        TabOrder = 2
      end
      object DBNavigator2: TDBNavigator
        Left = 8
        Top = 131
        Width = 230
        Height = 25
        DataSource = dsSubItems
        TabOrder = 3
      end
    end
    object GroupBox2: TGroupBox
      Left = 598
      Top = 5
      Width = 331
      Height = 221
      Caption = 'Numbers'
      TabOrder = 7
      object Label7: TLabel
        Left = 9
        Top = 15
        Width = 123
        Height = 13
        Caption = 'Byte 0..255 unsigned 8-bit'
        FocusControl = DBEditByte
      end
      object Label8: TLabel
        Left = 9
        Top = 83
        Width = 146
        Height = 13
        Caption = 'Word 0..65535 unsigned 16-bit'
        FocusControl = DBEditWord
      end
      object Label9: TLabel
        Left = 9
        Top = 38
        Width = 141
        Height = 13
        Caption = 'Shortint -128..127 signed 8-bit'
        FocusControl = DBEditShort
      end
      object Label10: TLabel
        Left = 9
        Top = 128
        Width = 228
        Height = 13
        Caption = 'Integer -2147483648..2147483647 signed 32-bit'
        FocusControl = DBEditInteger
      end
      object Label11: TLabel
        Left = 9
        Top = 60
        Width = 171
        Height = 13
        Caption = 'Smallint -32768..32767 signed 16-bit'
        FocusControl = DBEditSmall
      end
      object Label12: TLabel
        Left = 9
        Top = 195
        Width = 156
        Height = 13
        Caption = 'Int64 -2^63..2^63-1 signed 64-bit'
        FocusControl = DBEditInt64
      end
      object Label13: TLabel
        Left = 9
        Top = 173
        Width = 188
        Height = 13
        Caption = 'Cardinal 0..4294967295 unsigned 32-bit'
        FocusControl = DBEditCardinal
      end
      object Label14: TLabel
        Left = 9
        Top = 105
        Width = 230
        Height = 13
        Caption = 'Longint -2147483648..2147483647 signed 32-bit'
      end
      object Label15: TLabel
        Left = 9
        Top = 150
        Width = 197
        Height = 13
        Caption = 'Longword 0..4294967295 unsigned 32-bit'
      end
      object DBEditByte: TDBEdit
        Left = 254
        Top = 15
        Width = 64
        Height = 21
        DataField = 'Byte'
        DataSource = dsMain
        TabOrder = 0
      end
      object DBEditWord: TDBEdit
        Left = 254
        Top = 81
        Width = 64
        Height = 21
        DataField = 'Word'
        DataSource = dsMain
        TabOrder = 1
      end
      object DBEditShort: TDBEdit
        Left = 254
        Top = 37
        Width = 64
        Height = 21
        DataField = 'Shortint'
        DataSource = dsMain
        TabOrder = 2
      end
      object DBEditInteger: TDBEdit
        Left = 254
        Top = 125
        Width = 64
        Height = 21
        DataField = 'Integer'
        DataSource = dsMain
        TabOrder = 3
      end
      object DBEditSmall: TDBEdit
        Left = 254
        Top = 59
        Width = 64
        Height = 21
        DataField = 'Smallint'
        DataSource = dsMain
        TabOrder = 4
      end
      object DBEditInt64: TDBEdit
        Left = 254
        Top = 191
        Width = 64
        Height = 21
        DataField = 'Int64'
        DataSource = dsMain
        TabOrder = 5
      end
      object DBEditCardinal: TDBEdit
        Left = 254
        Top = 169
        Width = 64
        Height = 21
        DataField = 'Cardinal'
        DataSource = dsMain
        TabOrder = 6
      end
      object DBEditLongint: TDBEdit
        Left = 254
        Top = 103
        Width = 64
        Height = 21
        DataField = 'Longint'
        DataSource = dsMain
        TabOrder = 7
      end
      object DBEditLongword: TDBEdit
        Left = 254
        Top = 147
        Width = 64
        Height = 21
        DataField = 'LongWord'
        DataSource = dsMain
        TabOrder = 8
      end
    end
    object GroupBox3: TGroupBox
      Left = 385
      Top = 216
      Width = 204
      Height = 114
      Caption = 'Strings'
      TabOrder = 8
      object Label2: TLabel
        Left = 5
        Top = 39
        Width = 48
        Height = 13
        Caption = 'AnsiName'
        FocusControl = DBEdit2
      end
      object Label3: TLabel
        Left = 5
        Top = 63
        Width = 53
        Height = 13
        Caption = 'ShortName'
        FocusControl = DBEdit3
      end
      object Label17: TLabel
        Left = 5
        Top = 86
        Width = 53
        Height = 13
        Caption = 'WideName'
        FocusControl = DBEdit7
      end
      object Label24: TLabel
        Left = 5
        Top = 20
        Width = 46
        Height = 13
        Caption = 'Character'
        FocusControl = DBEdit10
      end
      object DBEdit2: TDBEdit
        Left = 69
        Top = 38
        Width = 125
        Height = 21
        DataField = 'AnsiName'
        DataSource = dsMain
        TabOrder = 0
      end
      object DBEdit3: TDBEdit
        Left = 70
        Top = 60
        Width = 125
        Height = 21
        DataField = 'ShortName'
        DataSource = dsMain
        TabOrder = 1
      end
      object DBEdit7: TDBEdit
        Left = 70
        Top = 83
        Width = 125
        Height = 21
        DataField = 'WideName'
        DataSource = dsMain
        TabOrder = 2
      end
      object DBEdit10: TDBEdit
        Left = 69
        Top = 15
        Width = 27
        Height = 21
        DataField = 'Character'
        DataSource = dsMain
        TabOrder = 3
      end
    end
    object GroupBox4: TGroupBox
      Left = 384
      Top = 32
      Width = 206
      Height = 179
      Caption = 'Date'#39's/Reals'
      TabOrder = 9
      object Label1: TLabel
        Left = 5
        Top = 20
        Width = 23
        Height = 13
        Caption = 'Date'
        FocusControl = DBEdit1
      end
      object Label5: TLabel
        Left = 5
        Top = 40
        Width = 23
        Height = 13
        Caption = 'Time'
        FocusControl = DBEdit4
      end
      object Label6: TLabel
        Left = 5
        Top = 61
        Width = 42
        Height = 13
        Caption = 'Datetime'
        FocusControl = DBEdit5
      end
      object Label16: TLabel
        Left = 5
        Top = 83
        Width = 22
        Height = 13
        Caption = 'Real'
        FocusControl = DBEdit6
      end
      object Label18: TLabel
        Left = 5
        Top = 105
        Width = 34
        Height = 13
        Caption = 'Double'
        FocusControl = DBEdit8
      end
      object Label19: TLabel
        Left = 5
        Top = 127
        Width = 45
        Height = 13
        Caption = 'Extended'
        FocusControl = DBEdit9
      end
      object Label20: TLabel
        Left = 5
        Top = 150
        Width = 42
        Height = 13
        Caption = 'Currency'
        FocusControl = EditMoney
      end
      object DBEdit1: TDBEdit
        Left = 72
        Top = 14
        Width = 125
        Height = 21
        DataField = 'Date'
        DataSource = dsMain
        TabOrder = 0
      end
      object DBEdit4: TDBEdit
        Left = 72
        Top = 37
        Width = 125
        Height = 21
        DataField = 'Time'
        DataSource = dsMain
        TabOrder = 1
      end
      object DBEdit5: TDBEdit
        Left = 72
        Top = 59
        Width = 125
        Height = 21
        DataField = 'Datetime'
        DataSource = dsMain
        TabOrder = 2
      end
      object DBEdit6: TDBEdit
        Left = 72
        Top = 81
        Width = 125
        Height = 21
        DataField = 'Real'
        DataSource = dsMain
        TabOrder = 3
      end
      object DBEdit8: TDBEdit
        Left = 72
        Top = 103
        Width = 125
        Height = 21
        DataField = 'Double'
        DataSource = dsMain
        TabOrder = 4
      end
      object DBEdit9: TDBEdit
        Left = 72
        Top = 124
        Width = 125
        Height = 21
        DataField = 'Extended'
        DataSource = dsMain
        TabOrder = 5
      end
      object EditMoney: TDBEdit
        Left = 72
        Top = 146
        Width = 125
        Height = 21
        DataField = 'Money'
        DataSource = dsMain
        TabOrder = 6
      end
    end
    object rgObjectClass: TRadioGroup
      Left = 382
      Top = 336
      Width = 203
      Height = 65
      Caption = 'ObjectInstance Class'
      ItemIndex = 0
      Items.Strings = (
        'TCollection'
        'TObjectList')
      TabOrder = 10
      OnClick = rgObjectClassClick
    end
  end
  object dsMain: TDataSource
    OnDataChange = dsMainDataChange
    Left = 196
    Top = 84
  end
  object dsSubItems: TDataSource
    Left = 256
    Top = 80
  end
  object SnapObjectDataset1: TSnapObjectDataset
    StringWidth = 255
    Left = 168
    Top = 80
  end
end
