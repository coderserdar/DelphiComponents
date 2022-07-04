object dlgData: TdlgData
  Left = 212
  Top = 158
  ActiveControl = Button3
  BorderStyle = bsDialog
  Caption = 'Data for report'
  ClientHeight = 295
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 120
    Top = 264
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object Button2: TButton
    Left = 200
    Top = 264
    Width = 75
    Height = 25
    Caption = '<< Back'
    Enabled = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 280
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Next >>'
    Default = True
    TabOrder = 3
    OnClick = Button3Click
  end
  object Panel2: TPanel
    Left = 8
    Top = 8
    Width = 337
    Height = 241
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object Label2: TLabel
      Left = 43
      Top = 45
      Width = 214
      Height = 13
      Caption = 'You can build report with table or query data. '
    end
    object Label3: TLabel
      Left = 43
      Top = 61
      Width = 209
      Height = 13
      Caption = 'Chouse below wich type of data will be used'
    end
    object Label11: TLabel
      Left = 40
      Top = 168
      Width = 205
      Height = 13
      Caption = 'Click '#39'Back'#39' to return to the previous screen'
    end
    object Label12: TLabel
      Left = 40
      Top = 182
      Width = 94
      Height = 13
      Caption = 'or '#39'Next'#39' to continue'
    end
    object RadioButton1: TRadioButton
      Left = 91
      Top = 98
      Width = 113
      Height = 17
      Caption = 'Table'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 91
      Top = 122
      Width = 113
      Height = 17
      Caption = 'Query'
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 337
    Height = 241
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object Label1: TLabel
      Left = 136
      Top = 16
      Width = 56
      Height = 13
      Caption = 'Alias names'
    end
    object Label7: TLabel
      Left = 8
      Top = 32
      Width = 110
      Height = 13
      Caption = 'Select an alias from the'
    end
    object Label8: TLabel
      Left = 8
      Top = 46
      Width = 104
      Height = 13
      Caption = 'Alias names list for the'
    end
    object Label9: TLabel
      Left = 8
      Top = 60
      Width = 103
      Height = 13
      Caption = 'report data, then click'
    end
    object Label10: TLabel
      Left = 8
      Top = 74
      Width = 81
      Height = 13
      Caption = 'Next to continue.'
    end
    object ListBox1: TListBox
      Left = 136
      Top = 32
      Width = 193
      Height = 201
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
      OnDblClick = ListBox1DblClick
    end
  end
  object Panel3: TPanel
    Left = 8
    Top = 8
    Width = 337
    Height = 241
    BevelOuter = bvNone
    TabOrder = 5
    Visible = False
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 337
      Height = 241
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      object Label4: TLabel
        Left = 144
        Top = 16
        Width = 76
        Height = 13
        Caption = 'Table names list'
      end
      object Label13: TLabel
        Left = 8
        Top = 40
        Width = 83
        Height = 13
        Caption = 'Select Table from'
      end
      object Label14: TLabel
        Left = 8
        Top = 54
        Width = 103
        Height = 13
        Caption = 'Table names list, then'
      end
      object Label15: TLabel
        Left = 8
        Top = 68
        Width = 87
        Height = 13
        Caption = 'click '#39'Next'#39' to data'
      end
      object Label16: TLabel
        Left = 8
        Top = 82
        Width = 93
        Height = 13
        Caption = 'preview or '#39'Back'#39' to'
      end
      object Label17: TLabel
        Left = 8
        Top = 96
        Width = 112
        Height = 13
        Caption = 'return to previous page.'
      end
      object ListBox2: TListBox
        Left = 144
        Top = 32
        Width = 185
        Height = 201
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBox2Click
        OnDblClick = ListBox2DblClick
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 337
      Height = 241
      BevelOuter = bvNone
      Caption = 'Panel4'
      TabOrder = 1
      Visible = False
      object Label5: TLabel
        Left = 16
        Top = 16
        Width = 132
        Height = 13
        Caption = 'Enter query SQL text below:'
      end
      object Label18: TLabel
        Left = 16
        Top = 220
        Width = 296
        Height = 13
        Caption = 
          'Click '#39'Next'#39' to data preview or '#39'Back'#39' to return to previous pag' +
          'e'
      end
      object Memo1: TMemo
        Left = 16
        Top = 32
        Width = 305
        Height = 177
        TabOrder = 0
      end
    end
  end
  object Panel6: TPanel
    Left = 8
    Top = 8
    Width = 337
    Height = 241
    BevelOuter = bvNone
    Caption = 'Panel4'
    TabOrder = 6
    Visible = False
    object Label6: TLabel
      Left = 8
      Top = 14
      Width = 90
      Height = 13
      Caption = 'Data preview page'
    end
    object Label19: TLabel
      Left = 8
      Top = 224
      Width = 308
      Height = 13
      Caption = 
        'Click '#39'Ok'#39' to chouse this data or '#39'Back'#39' to return to previous p' +
        'age.'
    end
    object DBGrid1: TDBGrid
      Left = 8
      Top = 64
      Width = 313
      Height = 153
      DataSource = DataSource1
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object DBNavigator1: TDBNavigator
      Left = 8
      Top = 32
      Width = 240
      Height = 25
      DataSource = DataSource1
      Flat = True
      TabOrder = 1
    end
  end
  object DataSource1: TDataSource
    Left = 16
    Top = 272
  end
end
