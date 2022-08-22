object Form1: TForm1
  Left = 441
  Top = 182
  BorderStyle = bsDialog
  Caption = 'DEMO - jbDBF maker'
  ClientHeight = 286
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 32
    Width = 241
    Height = 225
  end
  object Label1: TLabel
    Left = 24
    Top = 33
    Width = 28
    Height = 13
    Caption = 'result:'
  end
  object Label2: TLabel
    Left = 24
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Path'
  end
  object Button1: TButton
    Left = 256
    Top = 12
    Width = 89
    Height = 33
    Caption = 'End Demo'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 256
    Top = 192
    Width = 89
    Height = 33
    Caption = 'New,idx'
    TabOrder = 8
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 256
    Top = 228
    Width = 89
    Height = 33
    Caption = 'Find'
    Enabled = False
    TabOrder = 9
    OnClick = Button3Click
  end
  object Edit1: TEdit
    Left = 24
    Top = 48
    Width = 121
    Height = 21
    Color = clWhite
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 269
    Width = 351
    Height = 17
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 10
    object Panel2: TPanel
      Left = 230
      Top = 0
      Width = 121
      Height = 17
      Align = alRight
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      BevelWidth = 2
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 230
      Height = 17
      Align = alClient
      BevelOuter = bvLowered
      BevelWidth = 2
      TabOrder = 1
      object Gauge1: TGauge
        Left = 2
        Top = 2
        Width = 226
        Height = 13
        Align = alClient
        BackColor = clBtnFace
        BorderStyle = bsNone
        ForeColor = clBlue
        Progress = 0
      end
    end
  end
  object ListBox1: TListBox
    Left = 24
    Top = 136
    Width = 185
    Height = 113
    ItemHeight = 13
    TabOrder = 7
    Visible = False
  end
  object RadioButton1: TRadioButton
    Left = 24
    Top = 72
    Width = 73
    Height = 17
    Hint = 'Vyhled'#225'v'#225' jednu polo'#382'ku...'
    Caption = 'One field'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    TabStop = True
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 120
    Top = 72
    Width = 81
    Height = 17
    Hint = 'Vyhled'#225'v'#225' v'#237'ce polo'#382'ek...'
    Caption = 'Many fields'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = RadioButton2Click
  end
  object Edit2: TEdit
    Left = 56
    Top = 5
    Width = 193
    Height = 21
    TabOrder = 0
  end
  object Edit3: TEdit
    Left = 24
    Top = 96
    Width = 73
    Height = 21
    TabOrder = 5
    Text = '1'
    OnExit = Edit3Exit
  end
  object Edit4: TEdit
    Left = 120
    Top = 96
    Width = 73
    Height = 21
    TabOrder = 6
    Text = '100'
    Visible = False
    OnExit = Edit3Exit
  end
  object Button4: TButton
    Left = 264
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Test (down)'
    TabOrder = 11
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 264
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Test (up)'
    TabOrder = 12
    OnClick = Button5Click
  end
  object DBF1: TjbDBF
    DBFields = <
      item
        FieldName = 'CISLO'
        FieldType = jbDbf
        Tag = 0
        FieldLength = 10
        Places = 0
        IndexType = dbfUnique
        IndexSort = dbfAscending
        DataFieldIdent = 0
        IndexName = 'CISLO.IDX'
      end
      item
        FieldName = 'RETEZ'
        FieldType = jbDbf
        Tag = 0
        FieldLength = 33
        Places = 0
        IndexType = dbfDuplicates
        IndexSort = dbfAscending
        DataFieldIdent = 0
        IndexName = 'RETEZ.IDX'
      end
      item
        FieldType = dbtFloat
        Tag = 0
        FieldLength = 10
        Places = 2
        IndexType = dbfDuplicates
        IndexSort = dbfAscending
        DataFieldIdent = 0
      end>
    StoreByIndex = True
    ReadOnly = False
    SaveOnClose = False
    OnOpened = DBF1Opened
    OnClosed = DBF1Closed
    Left = 288
  end
end
