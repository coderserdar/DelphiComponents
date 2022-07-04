object Myform: TMyform
  Left = 350
  Top = 200
  Caption = 'Myform'
  ClientHeight = 533
  ClientWidth = 738
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 208
    Top = 0
    Width = 5
    Height = 533
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 208
    Height = 533
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
    object ExtDBNavigator1: TExtDBNavigator
      Left = 1
      Top = 1
      Width = 206
      Height = 20
      DataSource = Datasource
      DeleteQuestion = 'Confirmez-vous l'#39'effacement de l'#39'enregistrement ?'
      Align = alTop
      TabOrder = 0
      Orientation = noHorizontal
      GlyphSize = gsSmall
    end
    object Noms: TDBGrid
      Left = 1
      Top = 21
      Width = 206
      Height = 511
      Align = alClient
      DataSource = Datasource
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          Title.Caption = 'Noms'
          Width = 88
          Visible = True
        end
        item
          Expanded = False
          Title.Caption = 'Pr'#233'noms'
          Width = 88
          Visible = True
        end>
    end
  end
  object Panel2: TPanel
    Left = 213
    Top = 0
    Width = 525
    Height = 533
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    object FWLabel1: TFWLabel
      Left = 19
      Top = 108
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'Color'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = 10
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ColorFocus = clMaroon
    end
    object FWLabel2: TFWLabel
      Left = 19
      Top = 143
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'NumEdit'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = 10
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ColorFocus = clMaroon
    end
    object FWLabel3: TFWLabel
      Left = 19
      Top = 177
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'Date'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = 10
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ColorFocus = clMaroon
    end
    object FWLabel4: TFWLabel
      Left = 19
      Top = 211
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'Edit'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = 10
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ColorFocus = clMaroon
    end
    object FWLabel5: TFWLabel
      Left = 21
      Top = 64
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'Nom'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = 10
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ColorFocus = clMaroon
    end
    object FWLabel6: TFWLabel
      Left = 21
      Top = 24
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'Prenom'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = 10
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ColorFocus = clMaroon
    end
    object Splitter2: TSplitter
      Left = 1
      Top = 243
      Width = 523
      Height = 5
      Cursor = crVSplit
      Align = alBottom
    end
    object ExtColorCombo: TExtColorCombo
      Left = 118
      Top = 96
      Width = 221
      Height = 22
      Language = lgEnglish
      HTMLcolor = '#FFFFFF'
      ColorReadOnly = clInfoText
      MyLabel = FWLabel1
      AlwaysSame = False
      Color = clMoneyGreen
      ItemHeight = 16
      TabOrder = 2
    end
    object ExtNumEdit1: TExtNumEdit
      Left = 118
      Top = 133
      Width = 221
      Height = 21
      AlwaysSame = False
      ColorLabel = clMaroon
      ColorReadOnly = clInfoText
      MyLabel = FWLabel2
      Color = clMoneyGreen
      TabOrder = 3
    end
    object FWEdit: TFWEdit
      Left = 118
      Top = 201
      Width = 224
      Height = 21
      Color = clMoneyGreen
      TabOrder = 4
      Text = 'FWEdit'
      MyLabel = FWLabel4
      AlwaysSame = False
    end
    object DBListView: TDBListView
      Left = 1
      Top = 248
      Width = 523
      Height = 284
      Align = alBottom
      Columns = <
        item
          Caption = 'Nom'
          MinWidth = 120
          Width = 120
        end
        item
          Caption = 'Prenom'
        end>
      DragMode = dmAutomatic
      GridLines = True
      MultiSelect = True
      RowSelect = True
      TabOrder = 6
      ColumnsOrder = '0=120,1=50'
      Groups = <>
      ExtendedColumns = <
        item
        end
        item
        end>
      Datasource = Datasource
      DataKeyUnit = 'Cle'
      DataFieldsDisplay = 'Nom;Prenom'
      DataTableUnit = 'fiche'
    end
    object FWDBEdit: TFWDBEdit
      Left = 118
      Top = 56
      Width = 221
      Height = 21
      Color = clMoneyGreen
      DataField = 'Nom'
      DataSource = Datasource
      TabOrder = 1
      MyLabel = FWLabel5
      AlwaysSame = False
    end
    object FWDBEdit2: TFWDBEdit
      Left = 118
      Top = 16
      Width = 221
      Height = 21
      Color = clMoneyGreen
      DataField = 'Prenom'
      DataSource = Datasource
      TabOrder = 0
      MyLabel = FWLabel6
      AlwaysSame = False
    end
    object FWMemo: TFWMemo
      Left = 371
      Top = 21
      Width = 136
      Height = 209
      Color = clMoneyGreen
      Lines.Strings = (
        'FWMemo')
      TabOrder = 5
    end
    object FWDateEdit1: TFWDateTimePicker
      Left = 118
      Top = 167
      Width = 219
      Height = 21
      Date = 40426.720261122680000000
      Time = 40426.720261122680000000
      Color = clInfoBk
      TabOrder = 7
    end
  end
  object OnFormInfoIni: TOnFormInfoIni
    SauvePosObjects = True
    SauveEditObjets = [feTEdit, feTComboValue, feTDateEdit, feTMemo]
    SauvePosForm = True
    Left = 80
    Top = 176
  end
  object Datasource: TDataSource
    DataSet = DbfNoms
    Left = 56
    Top = 288
  end
  object DbfNoms: TDbf
    IndexDefs = <>
    TableLevel = 4
    AfterPost = DbfNomsAfterPost
    Left = 112
    Top = 288
  end
end
