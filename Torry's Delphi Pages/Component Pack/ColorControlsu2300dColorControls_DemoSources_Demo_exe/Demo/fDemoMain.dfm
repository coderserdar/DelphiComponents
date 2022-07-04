object fmDemoMain: TfmDemoMain
  Left = 240
  Top = 170
  Width = 992
  Height = 670
  Caption = 'Color controls v 1.0 by Kuzovkov Peter demo'
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
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 984
    Height = 643
    ActivePage = tsDBControls
    Align = alClient
    TabOrder = 0
    object tsListBox: TTabSheet
      Caption = 'TColorListBox, TColorCheckListBox'
      DesignSize = (
        976
        615)
      object Label1: TLabel
        Left = 5
        Top = 5
        Width = 221
        Height = 13
        Caption = 'Here is the example of the Default color preset:'
      end
      object Label2: TLabel
        Left = 305
        Top = 5
        Width = 125
        Height = 13
        Caption = 'And this is the Blue preset:'
      end
      object Label3: TLabel
        Left = 5
        Top = 295
        Width = 87
        Height = 13
        Caption = 'Green color preset'
      end
      object Label4: TLabel
        Left = 305
        Top = 295
        Width = 190
        Height = 13
        Caption = 'And the one with user-defined properties'
      end
      object ColorListBox1: TColorListBox
        Left = 5
        Top = 25
        Width = 291
        Height = 256
        Style = lbOwnerDrawFixed
        ItemHeight = 16
        Items.Strings = (
          'Item number 1'
          'Item number 2'
          'Item number 3'
          'Item number 4'
          'Some other item'
          'And another one'
          'Another item with some name'
          'Just text'
          'I'#39'm running out of imagination'
          'It happend'
          'That'#39's all'
          'Item number 5'
          'Item number 6'
          'Item number 7'
          'Item number 8'
          'Item number 9'
          'Item number 10'
          'Item number 11'
          'Item number 12'
          'Item number 13'
          'Item number 14'
          'Item number 15'
          'Item number 16'
          'Item number 17'
          'Item number 18'
          'Item number 19'
          'Item number 20')
        MultiSelect = True
        TabOrder = 0
        ColorPresets = ColorPresets_Default
      end
      object ColorListBox2: TColorListBox
        Left = 5
        Top = 315
        Width = 291
        Height = 296
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        Items.Strings = (
          'Item number 1'
          'Item number 2'
          'Item number 3'
          'Item number 4'
          'Some other item'
          'And another one'
          'Another item with some name'
          'Just text'
          'I'#39'm running out of imagination'
          'It happend'
          'That'#39's all'
          'Item number 5'
          'Item number 6'
          'Item number 7'
          'Item number 8'
          'Item number 9'
          'Item number 10'
          'Item number 11'
          'Item number 12'
          'Item number 13'
          'Item number 14'
          'Item number 15'
          'Item number 16'
          'Item number 17'
          'Item number 18'
          'Item number 19'
          'Item number 20')
        MultiSelect = True
        TabOrder = 1
        ColorPresets = ColorPresets_green
      end
      object ColorListBox3: TColorCheckListBox
        Left = 305
        Top = 25
        Width = 663
        Height = 256
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        Items.Strings = (
          'Item number 1'
          'Item number 2'
          'Item number 3'
          'Item number 4'
          'Some other item'
          'And another one'
          'Another item with some name'
          'Just text'
          'I'#39'm running out of imagination'
          'It happend'
          'That'#39's all'
          'Item number 5'
          'Item number 6'
          'Item number 7'
          'Item number 8'
          'Item number 9'
          'Item number 10'
          'Item number 11'
          'Item number 12'
          'Item number 13'
          'Item number 14'
          'Item number 15'
          'Item number 16'
          'Item number 17'
          'Item number 18'
          'Item number 19'
          'Item number 20')
        Style = lbOwnerDrawFixed
        TabOrder = 2
        ColorPresets = ColorPresets_blue
      end
      object ColorListBox4: TColorListBox
        Left = 305
        Top = 315
        Width = 663
        Height = 295
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Items.Strings = (
          'Item number 1'
          'Item number 2'
          'Item number 3'
          'Item number 4'
          'Some other item'
          'And another one'
          'Another item with some name'
          'Just text'
          'I'#39'm running out of imagination'
          'It happend'
          'That'#39's all'
          'Item number 5'
          'Item number 6'
          'Item number 7'
          'Item number 8'
          'Item number 9'
          'Item number 10'
          'Item number 11'
          'Item number 12'
          'Item number 13'
          'Item number 14'
          'Item number 15'
          'Item number 16'
          'Item number 17'
          'Item number 18'
          'Item number 19'
          'Item number 20')
        MultiSelect = True
        TabOrder = 3
        ColorPresets = ColorPresets_user
      end
    end
    object tsAnotherControls: TTabSheet
      Caption = 'Grids and TColor_ComboBox'
      ImageIndex = 1
      DesignSize = (
        976
        615)
      object Label5: TLabel
        Left = 5
        Top = 5
        Width = 362
        Height = 13
        Caption = 
          'The example of TColorValueListEditor. Note, that cell editor has' +
          ' it'#39's own color'
      end
      object Label6: TLabel
        Left = 520
        Top = 5
        Width = 119
        Height = 13
        Caption = 'This is a TColorStringGrid'
      end
      object Label7: TLabel
        Left = 5
        Top = 340
        Width = 117
        Height = 13
        Caption = 'This is a TColorDrawGrid'
      end
      object Label8: TLabel
        Left = 385
        Top = 340
        Width = 171
        Height = 13
        Caption = 'Some examples of TColorComboBox'
      end
      object Label9: TLabel
        Left = 375
        Top = 365
        Width = 61
        Height = 13
        Caption = 'Blue scheme'
      end
      object Label10: TLabel
        Left = 375
        Top = 435
        Width = 69
        Height = 13
        Caption = 'Green scheme'
      end
      object Label11: TLabel
        Left = 771
        Top = 365
        Width = 74
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Default scheme'
      end
      object Label12: TLabel
        Left = 771
        Top = 435
        Width = 62
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'User scheme'
      end
      object ColorValueListEditor1: TColorValueListEditor
        Left = 5
        Top = 25
        Width = 366
        Height = 301
        Strings.Strings = (
          'Field 1=123'
          'Field 2=Some string value'
          'Field 3=qqqq'
          'Field 4=4444'
          'Field 5=False'
          'Field 6=[aa, bb, cc]'
          'Field 7=qqq'
          'Field 8=String val'
          'Field 9=qqq'
          'Field 10=qqq')
        TabOrder = 0
        ColorPresets_Key = ColorPresets_blue
        ColorPresets_Value = ColorPresets_Default
        ColWidths = (
          88
          272)
      end
      object ColorStringGrid1: TColorStringGrid
        Left = 375
        Top = 25
        Width = 596
        Height = 301
        Anchors = [akLeft, akTop, akRight]
        FixedCols = 0
        RowCount = 7
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 1
        ColorPresets = ColorPresets_red
        ColWidths = (
          64
          90
          83
          73
          94)
        RowHeights = (
          24
          45
          24
          38
          24
          24
          24)
      end
      object ColorDrawGrid1: TColorDrawGrid
        Left = 5
        Top = 360
        Width = 366
        Height = 247
        Anchors = [akLeft, akTop, akBottom]
        TabOrder = 2
        ColorPresets = ColorPresets_green
      end
      object Color_ComboBox1: TColor_ComboBox
        Left = 375
        Top = 385
        Width = 381
        Height = 22
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 16
        TabOrder = 3
        Items.Strings = (
          'Item number 1'
          'Item number 2'
          'Item number 3'
          'Item number 4'
          'Some other item'
          'And another one'
          'Another item with some name'
          'Just text'
          'I'#39'm running out of imagination'
          'It happend'
          'That'#39's all'
          'Item number 5'
          'Item number 6'
          'Item number 7'
          'Item number 8'
          'Item number 9'
          'Item number 10'
          'Item number 11'
          'Item number 12'
          'Item number 13'
          'Item number 14'
          'Item number 15'
          'Item number 16'
          'Item number 17'
          'Item number 18'
          'Item number 19'
          'Item number 20'
          '')
        ColorPresets = ColorPresets_blue
      end
      object Color_ComboBox2: TColor_ComboBox
        Left = 770
        Top = 385
        Width = 201
        Height = 22
        Style = csOwnerDrawFixed
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 4
        Items.Strings = (
          'Item number 1'
          'Item number 2'
          'Item number 3'
          'Item number 4'
          'Some other item'
          'And another one'
          'Another item with some name'
          'Just text'
          'I'#39'm running out of imagination'
          'It happend'
          'That'#39's all'
          'Item number 5'
          'Item number 6'
          'Item number 7'
          'Item number 8'
          'Item number 9'
          'Item number 10'
          'Item number 11'
          'Item number 12'
          'Item number 13'
          'Item number 14'
          'Item number 15'
          'Item number 16'
          'Item number 17'
          'Item number 18'
          'Item number 19'
          'Item number 20'
          '')
        ColorPresets = ColorPresets_Default
      end
      object Color_ComboBox3: TColor_ComboBox
        Left = 375
        Top = 455
        Width = 381
        Height = 22
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 32
        ItemHeight = 16
        TabOrder = 5
        Items.Strings = (
          'Item number 1'
          'Item number 2'
          'Item number 3'
          'Item number 4'
          'Some other item'
          'And another one'
          'Another item with some name'
          'Just text'
          'I'#39'm running out of imagination'
          'It happend'
          'That'#39's all'
          'Item number 5'
          'Item number 6'
          'Item number 7'
          'Item number 8'
          'Item number 9'
          'Item number 10'
          'Item number 11'
          'Item number 12'
          'Item number 13'
          'Item number 14'
          'Item number 15'
          'Item number 16'
          'Item number 17'
          'Item number 18'
          'Item number 19'
          'Item number 20'
          '')
        ColorPresets = ColorPresets_green
      end
      object Color_ComboBox4: TColor_ComboBox
        Left = 770
        Top = 455
        Width = 201
        Height = 22
        Style = csOwnerDrawFixed
        Anchors = [akTop, akRight]
        DropDownCount = 32
        ItemHeight = 16
        TabOrder = 6
        Items.Strings = (
          'Item number 1'
          'Item number 2'
          'Item number 3'
          'Item number 4'
          'Some other item'
          'And another one'
          'Another item with some name'
          'Just text'
          'I'#39'm running out of imagination'
          'It happend'
          'That'#39's all'
          'Item number 5'
          'Item number 6'
          'Item number 7'
          'Item number 8'
          'Item number 9'
          'Item number 10'
          'Item number 11'
          'Item number 12'
          'Item number 13'
          'Item number 14'
          'Item number 15'
          'Item number 16'
          'Item number 17'
          'Item number 18'
          'Item number 19'
          'Item number 20'
          '')
        ColorPresets = ColorPresets_user
      end
    end
    object tsDBControls: TTabSheet
      Caption = 'Database controls'
      ImageIndex = 2
      DesignSize = (
        976
        615)
      object Label13: TLabel
        Left = 5
        Top = 5
        Width = 97
        Height = 13
        Caption = 'TColorDBComboBox'
      end
      object Label14: TLabel
        Left = 5
        Top = 55
        Width = 80
        Height = 13
        Caption = 'TColorDBListBox'
      end
      object Label15: TLabel
        Left = 245
        Top = 5
        Width = 326
        Height = 13
        Caption = 
          'TColorDBGrid. Note that columns can have their own color propert' +
          'ies'
      end
      object ColorDBGrid1: TColorDBGrid
        Left = 245
        Top = 25
        Width = 721
        Height = 581
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsPeople
        Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        ColorPresets = ColorPresets_Default
        RepaintOnMouseWeel = False
        Columns = <
          item
            Expanded = False
            FieldName = 'ipeopleid'
            Title.Caption = #8470
            Width = 39
            Visible = True
            ColorPresets = ColorPresets_red
          end
          item
            Expanded = False
            FieldName = 'vcName'
            Title.Caption = 'Name'
            Width = 129
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'vcSurname'
            Title.Caption = 'Surname'
            Width = 135
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'dtBirthdate'
            Title.Caption = 'Birth date'
            Visible = True
            ColorPresets = ColorPresets_blue
          end
          item
            Expanded = False
            FieldName = 'vcAdress'
            Title.Caption = 'Adress'
            Width = 305
            Visible = True
            ColorPresets = ColorPresets_green
          end>
      end
      object ColorDBListBox1: TColorDBListBox
        Left = 5
        Top = 75
        Width = 231
        Height = 531
        Anchors = [akLeft, akTop, akBottom]
        DataField = 'vcSurname'
        DataSource = dsPeople
        ItemHeight = 16
        Items.Strings = (
          'Oksana'
          'Viktor'
          'Tamara'
          'Julia'
          'Olena'
          'Anatolij'
          'Zoja'
          'Anna'
          'Margarita'
          'Georgi'
          'Evgenij'
          'Stella'
          'Tatjana'
          'Irina'
          'Raisa'
          'Agnia')
        Style = lbOwnerDrawFixed
        TabOrder = 1
        ColorPresets = ColorPresets_Default
      end
      object ColorDBComboBox1: TColorDBComboBox
        Left = 5
        Top = 25
        Width = 231
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        Items.Strings = (
          'Djachenko'
          'Zima'
          'Kalinka'
          'Kovalenko'
          'Kozak'
          'Kuripko'
          'Ljasota'
          'Moskalenko'
          'Novicka'
          'Olhovska')
        TabOrder = 2
        ColorPresets = ColorPresets_Default
      end
    end
  end
  object dsPeople: TDataSource
    DataSet = quPeople
    Left = 185
    Top = 315
  end
  object ADOConnection: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=DemoDB.mdb;Persist ' +
      'Security Info=False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 275
    Top = 220
  end
  object quAdresses: TADOQuery
    Connection = ADOConnection
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'select a.vcStreet +'#39'  '#39'+ a.decHouse+'#39'  '#39'+a.decAppartment as aa '
      'from adresses a '
      'where a.iAdressesID = 1'
      ' ')
    Left = 205
    Top = 300
  end
  object quCity: TADOQuery
    Active = True
    Connection = ADOConnection
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'select * from city')
    Left = 260
    Top = 270
  end
  object quPeople: TADOQuery
    Active = True
    Connection = ADOConnection
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'select '
      'p.ipeopleid, p.vcSurname, p.vcName, p.dtBirthdate,'
      
        '(select a.vcStreet +'#39'  '#39'+ a.decHouse+'#39',  '#39'+a.decAppartment as aa' +
        ' '
      'from adresses a '
      'where a.iAdressesID = p.IAdressesID) as vcAdress'
      ' from people p')
    Left = 185
    Top = 270
  end
  object dsAdresses: TDataSource
    DataSet = quAdresses
    Left = 220
    Top = 315
  end
  object dsCity: TDataSource
    DataSet = quCity
    Left = 260
    Top = 315
  end
  object ColorPresets_Default: TColorPresets
    ColorActive1 = 15925247
    ColorActive2 = 14811134
    ColorInactive1 = 16119285
    ColorInactive2 = 15263976
    ColorSelectedActive = 16692118
    ColorSelectedInactive = 14269627
    ColorMultiSelectedActive1 = 16767957
    ColorMultiSelectedActive2 = 16762286
    ColorMultiSelectedInactive1 = 14670802
    ColorMultiSelectedInactive2 = 13880512
    ColorInplaceEditor = 14024667
    FontActive1.Charset = DEFAULT_CHARSET
    FontActive1.Color = clWindowText
    FontActive1.Height = -11
    FontActive1.Name = 'MS Sans Serif'
    FontActive1.Style = []
    FontActive2.Charset = DEFAULT_CHARSET
    FontActive2.Color = clWindowText
    FontActive2.Height = -11
    FontActive2.Name = 'MS Sans Serif'
    FontActive2.Style = []
    FontInactive1.Charset = DEFAULT_CHARSET
    FontInactive1.Color = clWindowText
    FontInactive1.Height = -11
    FontInactive1.Name = 'MS Sans Serif'
    FontInactive1.Style = []
    FontInactive2.Charset = DEFAULT_CHARSET
    FontInactive2.Color = clWindowText
    FontInactive2.Height = -11
    FontInactive2.Name = 'MS Sans Serif'
    FontInactive2.Style = []
    FontSelectedActive.Charset = DEFAULT_CHARSET
    FontSelectedActive.Color = clWindowText
    FontSelectedActive.Height = -11
    FontSelectedActive.Name = 'MS Sans Serif'
    FontSelectedActive.Style = []
    FontSelectedInactive.Charset = DEFAULT_CHARSET
    FontSelectedInactive.Color = clWindowText
    FontSelectedInactive.Height = -11
    FontSelectedInactive.Name = 'MS Sans Serif'
    FontSelectedInactive.Style = []
    FontMultiSelectedActive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive1.Color = clWindowText
    FontMultiSelectedActive1.Height = -11
    FontMultiSelectedActive1.Name = 'MS Sans Serif'
    FontMultiSelectedActive1.Style = []
    FontMultiSelectedActive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive2.Color = clWindowText
    FontMultiSelectedActive2.Height = -11
    FontMultiSelectedActive2.Name = 'MS Sans Serif'
    FontMultiSelectedActive2.Style = []
    FontMultiSelectedInactive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive1.Color = clWindowText
    FontMultiSelectedInactive1.Height = -11
    FontMultiSelectedInactive1.Name = 'MS Sans Serif'
    FontMultiSelectedInactive1.Style = []
    FontMultiSelectedInactive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive2.Color = clWindowText
    FontMultiSelectedInactive2.Height = -11
    FontMultiSelectedInactive2.Name = 'MS Sans Serif'
    FontMultiSelectedInactive2.Style = []
    FramePenActive.Color = 12698066
    FramePenInactive.Color = 11842740
    FrameOverride = True
    _ColorPreset = cpDefault
    _FontPreset = fpUser
    Left = 359
    Top = 59
  end
  object ColorPresets_red: TColorPresets
    ColorActive1 = 15924479
    ColorActive2 = 14874622
    ColorInactive1 = 15856378
    ColorInactive2 = 15002091
    ColorSelectedActive = 5879295
    ColorSelectedInactive = 11119028
    ColorMultiSelectedActive1 = 12119038
    ColorMultiSelectedActive2 = 9949439
    ColorMultiSelectedInactive1 = 14211288
    ColorMultiSelectedInactive2 = 13224393
    ColorInplaceEditor = 14021119
    FontActive1.Charset = DEFAULT_CHARSET
    FontActive1.Color = clWindowText
    FontActive1.Height = -11
    FontActive1.Name = 'MS Sans Serif'
    FontActive1.Style = []
    FontActive2.Charset = DEFAULT_CHARSET
    FontActive2.Color = clWindowText
    FontActive2.Height = -11
    FontActive2.Name = 'MS Sans Serif'
    FontActive2.Style = []
    FontInactive1.Charset = DEFAULT_CHARSET
    FontInactive1.Color = clWindowText
    FontInactive1.Height = -11
    FontInactive1.Name = 'MS Sans Serif'
    FontInactive1.Style = []
    FontInactive2.Charset = DEFAULT_CHARSET
    FontInactive2.Color = clWindowText
    FontInactive2.Height = -11
    FontInactive2.Name = 'MS Sans Serif'
    FontInactive2.Style = []
    FontSelectedActive.Charset = DEFAULT_CHARSET
    FontSelectedActive.Color = clWindowText
    FontSelectedActive.Height = -11
    FontSelectedActive.Name = 'MS Sans Serif'
    FontSelectedActive.Style = []
    FontSelectedInactive.Charset = DEFAULT_CHARSET
    FontSelectedInactive.Color = clWindowText
    FontSelectedInactive.Height = -11
    FontSelectedInactive.Name = 'MS Sans Serif'
    FontSelectedInactive.Style = []
    FontMultiSelectedActive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive1.Color = clWindowText
    FontMultiSelectedActive1.Height = -11
    FontMultiSelectedActive1.Name = 'MS Sans Serif'
    FontMultiSelectedActive1.Style = []
    FontMultiSelectedActive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive2.Color = clWindowText
    FontMultiSelectedActive2.Height = -11
    FontMultiSelectedActive2.Name = 'MS Sans Serif'
    FontMultiSelectedActive2.Style = []
    FontMultiSelectedInactive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive1.Color = clWindowText
    FontMultiSelectedInactive1.Height = -11
    FontMultiSelectedInactive1.Name = 'MS Sans Serif'
    FontMultiSelectedInactive1.Style = []
    FontMultiSelectedInactive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive2.Color = clWindowText
    FontMultiSelectedInactive2.Height = -11
    FontMultiSelectedInactive2.Name = 'MS Sans Serif'
    FontMultiSelectedInactive2.Style = []
    FramePenActive.Color = 12698066
    FramePenInactive.Color = 12698066
    FrameOverride = True
    _ColorPreset = cpRed
    _FontPreset = fpUser
    Left = 359
    Top = 94
  end
  object ColorPresets_blue: TColorPresets
    ColorActive1 = 16775666
    ColorActive2 = 16773345
    ColorInactive1 = 16119285
    ColorInactive2 = 15263976
    ColorSelectedActive = 16692118
    ColorSelectedInactive = 14269627
    ColorMultiSelectedActive1 = 16767957
    ColorMultiSelectedActive2 = 16762286
    ColorMultiSelectedInactive1 = 14670802
    ColorMultiSelectedInactive2 = 13880512
    ColorInplaceEditor = 16642007
    FontActive1.Charset = DEFAULT_CHARSET
    FontActive1.Color = clWindowText
    FontActive1.Height = -11
    FontActive1.Name = 'MS Sans Serif'
    FontActive1.Style = []
    FontActive2.Charset = DEFAULT_CHARSET
    FontActive2.Color = clWindowText
    FontActive2.Height = -11
    FontActive2.Name = 'MS Sans Serif'
    FontActive2.Style = []
    FontInactive1.Charset = DEFAULT_CHARSET
    FontInactive1.Color = clWindowText
    FontInactive1.Height = -11
    FontInactive1.Name = 'MS Sans Serif'
    FontInactive1.Style = []
    FontInactive2.Charset = DEFAULT_CHARSET
    FontInactive2.Color = clWindowText
    FontInactive2.Height = -11
    FontInactive2.Name = 'MS Sans Serif'
    FontInactive2.Style = []
    FontSelectedActive.Charset = DEFAULT_CHARSET
    FontSelectedActive.Color = clWindowText
    FontSelectedActive.Height = -11
    FontSelectedActive.Name = 'MS Sans Serif'
    FontSelectedActive.Style = []
    FontSelectedInactive.Charset = DEFAULT_CHARSET
    FontSelectedInactive.Color = clWindowText
    FontSelectedInactive.Height = -11
    FontSelectedInactive.Name = 'MS Sans Serif'
    FontSelectedInactive.Style = []
    FontMultiSelectedActive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive1.Color = clWindowText
    FontMultiSelectedActive1.Height = -11
    FontMultiSelectedActive1.Name = 'MS Sans Serif'
    FontMultiSelectedActive1.Style = []
    FontMultiSelectedActive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive2.Color = clWindowText
    FontMultiSelectedActive2.Height = -11
    FontMultiSelectedActive2.Name = 'MS Sans Serif'
    FontMultiSelectedActive2.Style = []
    FontMultiSelectedInactive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive1.Color = clWindowText
    FontMultiSelectedInactive1.Height = -11
    FontMultiSelectedInactive1.Name = 'MS Sans Serif'
    FontMultiSelectedInactive1.Style = []
    FontMultiSelectedInactive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive2.Color = clWindowText
    FontMultiSelectedInactive2.Height = -11
    FontMultiSelectedInactive2.Name = 'MS Sans Serif'
    FontMultiSelectedInactive2.Style = []
    FramePenActive.Color = 12698066
    FramePenInactive.Color = 11842740
    FrameOverride = True
    _ColorPreset = cpBlue
    _FontPreset = fpUser
    Left = 359
    Top = 129
  end
  object ColorPresets_green: TColorPresets
    ColorActive1 = 15925243
    ColorActive2 = 14876403
    ColorInactive1 = 16119285
    ColorInactive2 = 15263976
    ColorSelectedActive = 6355334
    ColorSelectedInactive = 10730147
    ColorMultiSelectedActive1 = 12058579
    ColorMultiSelectedActive2 = 8454063
    ColorMultiSelectedInactive1 = 13624019
    ColorMultiSelectedInactive2 = 12702656
    ColorInplaceEditor = 14024667
    FontActive1.Charset = DEFAULT_CHARSET
    FontActive1.Color = clWindowText
    FontActive1.Height = -11
    FontActive1.Name = 'MS Sans Serif'
    FontActive1.Style = []
    FontActive2.Charset = DEFAULT_CHARSET
    FontActive2.Color = clWindowText
    FontActive2.Height = -11
    FontActive2.Name = 'MS Sans Serif'
    FontActive2.Style = []
    FontInactive1.Charset = DEFAULT_CHARSET
    FontInactive1.Color = clWindowText
    FontInactive1.Height = -11
    FontInactive1.Name = 'MS Sans Serif'
    FontInactive1.Style = []
    FontInactive2.Charset = DEFAULT_CHARSET
    FontInactive2.Color = clWindowText
    FontInactive2.Height = -11
    FontInactive2.Name = 'MS Sans Serif'
    FontInactive2.Style = []
    FontSelectedActive.Charset = DEFAULT_CHARSET
    FontSelectedActive.Color = clWindowText
    FontSelectedActive.Height = -11
    FontSelectedActive.Name = 'MS Sans Serif'
    FontSelectedActive.Style = []
    FontSelectedInactive.Charset = DEFAULT_CHARSET
    FontSelectedInactive.Color = clWindowText
    FontSelectedInactive.Height = -11
    FontSelectedInactive.Name = 'MS Sans Serif'
    FontSelectedInactive.Style = []
    FontMultiSelectedActive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive1.Color = clWindowText
    FontMultiSelectedActive1.Height = -11
    FontMultiSelectedActive1.Name = 'MS Sans Serif'
    FontMultiSelectedActive1.Style = []
    FontMultiSelectedActive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive2.Color = clWindowText
    FontMultiSelectedActive2.Height = -11
    FontMultiSelectedActive2.Name = 'MS Sans Serif'
    FontMultiSelectedActive2.Style = []
    FontMultiSelectedInactive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive1.Color = clWindowText
    FontMultiSelectedInactive1.Height = -11
    FontMultiSelectedInactive1.Name = 'MS Sans Serif'
    FontMultiSelectedInactive1.Style = []
    FontMultiSelectedInactive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive2.Color = clWindowText
    FontMultiSelectedInactive2.Height = -11
    FontMultiSelectedInactive2.Name = 'MS Sans Serif'
    FontMultiSelectedInactive2.Style = []
    FramePenActive.Color = 12698066
    FramePenInactive.Color = 11842740
    FrameOverride = True
    _ColorPreset = cpGreen
    _FontPreset = fpUser
    Left = 359
    Top = 165
  end
  object ColorPresets_user: TColorPresets
    ColorActive1 = 15925247
    ColorActive2 = 14811134
    ColorInactive1 = 16445938
    ColorInactive2 = 15851742
    ColorSelectedActive = 16692118
    ColorSelectedInactive = 14269627
    ColorMultiSelectedActive1 = 16767957
    ColorMultiSelectedActive2 = 16762286
    ColorMultiSelectedInactive1 = 14670802
    ColorMultiSelectedInactive2 = 13880512
    ColorInplaceEditor = 14024667
    FontActive1.Charset = DEFAULT_CHARSET
    FontActive1.Color = clNavy
    FontActive1.Height = -11
    FontActive1.Name = 'MS Sans Serif'
    FontActive1.Style = []
    FontActive2.Charset = DEFAULT_CHARSET
    FontActive2.Color = 2022675
    FontActive2.Height = -11
    FontActive2.Name = 'MS Sans Serif'
    FontActive2.Style = []
    FontInactive1.Charset = DEFAULT_CHARSET
    FontInactive1.Color = clSilver
    FontInactive1.Height = -11
    FontInactive1.Name = 'MS Sans Serif'
    FontInactive1.Style = []
    FontInactive2.Charset = DEFAULT_CHARSET
    FontInactive2.Color = clGray
    FontInactive2.Height = -11
    FontInactive2.Name = 'MS Sans Serif'
    FontInactive2.Style = []
    FontSelectedActive.Charset = DEFAULT_CHARSET
    FontSelectedActive.Color = clWindowText
    FontSelectedActive.Height = -11
    FontSelectedActive.Name = 'MS Sans Serif'
    FontSelectedActive.Style = [fsBold]
    FontSelectedInactive.Charset = DEFAULT_CHARSET
    FontSelectedInactive.Color = clWindowText
    FontSelectedInactive.Height = -11
    FontSelectedInactive.Name = 'MS Sans Serif'
    FontSelectedInactive.Style = [fsBold, fsItalic]
    FontMultiSelectedActive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive1.Color = clWindowText
    FontMultiSelectedActive1.Height = -11
    FontMultiSelectedActive1.Name = 'MS Sans Serif'
    FontMultiSelectedActive1.Style = []
    FontMultiSelectedActive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedActive2.Color = clWindowText
    FontMultiSelectedActive2.Height = -11
    FontMultiSelectedActive2.Name = 'MS Sans Serif'
    FontMultiSelectedActive2.Style = []
    FontMultiSelectedInactive1.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive1.Color = clWindowText
    FontMultiSelectedInactive1.Height = -11
    FontMultiSelectedInactive1.Name = 'MS Sans Serif'
    FontMultiSelectedInactive1.Style = []
    FontMultiSelectedInactive2.Charset = DEFAULT_CHARSET
    FontMultiSelectedInactive2.Color = clWindowText
    FontMultiSelectedInactive2.Height = -11
    FontMultiSelectedInactive2.Name = 'MS Sans Serif'
    FontMultiSelectedInactive2.Style = []
    FramePenActive.Color = 12698066
    FramePenInactive.Color = 11842740
    FramePenInactive.Style = psDot
    FrameOverride = True
    _ColorPreset = cpUser
    _FontPreset = fpUser
    Left = 359
    Top = 199
  end
end
