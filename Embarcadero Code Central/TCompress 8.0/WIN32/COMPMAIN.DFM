object Form1: TForm1
  Left = 329
  Top = 152
  Width = 523
  Height = 542
  Caption = 'TCompress Demonstration'
  Color = clGreen
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    088888888888888888888880000000000F4F7777777777777777078000000000
    0F0F78877777777777770780000000000F0F7FFFFFFFFFFFFFF7078000000000
    0F0F70000000000000070780000000000F0F7FFFFFFFFFFFFFF7078000000000
    0F0F70000000000000070780000000000F0F7777777777777777078000000000
    0F0FFFFFFFFFFFFFFFFF0780000000000F000000000000000000078000000000
    0F7777777777777777777780000000000F700700007007070070778000000000
    0F7000000000000000007780000000000F700000000090000000778000000000
    0F7000000099990000007780000000000F777777099090907777778000000000
    0F7777770000900077777780000000000F777777000090007777778000000000
    0F7777770000900077777780000000000F777777000090007777778000000000
    0F7777770000000077777780000000000FFFFFFFFFFFFFFFFFFFFF7000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000
    000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000
    000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000
    000FF000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFF}
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClick = FormClick
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 515
    Height = 508
    Align = alClient
    BevelOuter = bvNone
    Color = clGreen
    TabOrder = 0
    object Shape1: TShape
      Left = 1
      Top = 322
      Width = 502
      Height = 1
    end
    object DBText1: TDBText
      Left = 184
      Top = 327
      Width = 138
      Height = 17
      Alignment = taCenter
      DataField = 'Common_Name'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Image1: TImage
      Left = 171
      Top = 210
      Width = 165
      Height = 105
      DragMode = dmAutomatic
      Stretch = True
      OnDragDrop = CDBImage1DragDrop
      OnDragOver = CDBImage1DragOver
    end
    object Memo1: TMemo
      Left = 177
      Top = 245
      Width = 154
      Height = 32
      Cursor = crArrow
      TabStop = False
      Alignment = taCenter
      BorderStyle = bsNone
      Color = clBtnFace
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Lines.Strings = (
        'Drag images here to display'
        'them in a simple TImage')
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      OnDragDrop = CDBImage1DragDrop
      OnDragOver = CDBImage1DragOver
    end
    object Memo2: TMemo
      Left = 74
      Top = 351
      Width = 354
      Height = 96
      Alignment = taCenter
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      Lines.Strings = (
        'BLOB Database compression demonstration'
        'currently disabled...'
        ''
        'Set the DBDEMOS alias to point to the location of'
        'the BIOLIFE.DB table (usually \DELPHI\DEMOS\DATA).')
      ParentFont = False
      TabOrder = 1
      Visible = False
    end
    object DBNavigator1: TDBNavigator
      Left = 207
      Top = 480
      Width = 88
      Height = 21
      DataSource = DataSource1
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
      TabOrder = 2
    end
    object CMethod: TRadioGroup
      Left = 202
      Top = 0
      Width = 102
      Height = 89
      Hint = 'Select the desired compression method'
      Caption = 'Compression'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ItemIndex = 3
      Items.Strings = (
        'None'
        'RLE'
        'LZH1'
        'LZH5')
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = CMethodClick
    end
    object GroupBox1: TGroupBox
      Left = 5
      Top = 0
      Width = 162
      Height = 263
      Hint = 'Drag files to and from the file list'
      Caption = 'Disk Files'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = GroupBox1Click
      OnDragDrop = DLDragDrop
      OnDragOver = DLDragOver
      object FL: TFileListBox
        Left = 3
        Top = 112
        Width = 155
        Height = 113
        DragMode = dmAutomatic
        ItemHeight = 16
        MultiSelect = True
        ShowGlyphs = True
        TabOrder = 0
        OnClick = FLClick
        OnDragDrop = DLDragDrop
        OnDragOver = DLDragOver
      end
      object DL: TDirectoryListBox
        Left = 3
        Top = 43
        Width = 155
        Height = 67
        FileList = FL
        ItemHeight = 16
        TabOrder = 1
        OnDragDrop = DLDragDrop
        OnDragOver = DLDragOver
      end
      object DCB: TDriveComboBox
        Left = 3
        Top = 19
        Width = 155
        Height = 22
        DirList = DL
        TabOrder = 2
        OnDragDrop = DLDragDrop
        OnDragOver = DLDragOver
      end
      object Memo3: TMemo
        Left = 3
        Top = 227
        Width = 155
        Height = 33
        Cursor = crArrow
        TabStop = False
        Alignment = taCenter
        BorderStyle = bsNone
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Drag these files to the archive,'
          'images or memo, & vice versa')
        ParentFont = False
        ReadOnly = True
        TabOrder = 3
      end
    end
    object ArchiveGroup: TGroupBox
      Left = 348
      Top = 0
      Width = 162
      Height = 263
      Hint = 'Drag the entire archive too if you wish...'
      Caption = 'Archived Files'
      Color = clBtnFace
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnDragDrop = archivefileDragDrop
      OnDragOver = archivefileDragOver
      object ArchiveLabel: TLabel
        Left = 57
        Top = 22
        Width = 46
        Height = 16
        Caption = 'Archive:'
        DragMode = dmAutomatic
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label2: TLabel
        Left = 54
        Top = 70
        Width = 55
        Height = 16
        Caption = 'Contains:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object archivefile: TEdit
        Left = 4
        Top = 40
        Width = 155
        Height = 24
        Hint = 'Edit archive name as required'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = 'C:\COMPDEMO.ARC'
        OnChange = archivefileChange
        OnDragDrop = archivefileDragDrop
        OnDragOver = archivefileDragOver
      end
      object ListBox1: TListBox
        Left = 4
        Top = 88
        Width = 155
        Height = 137
        Hint = 'Click filename for file compression information'
        DragMode = dmAutomatic
        ItemHeight = 16
        MultiSelect = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = ListBox1Click
        OnDragDrop = archivefileDragDrop
        OnDragOver = archivefileDragOver
      end
      object Memo4: TMemo
        Left = 4
        Top = 227
        Width = 154
        Height = 33
        Cursor = crArrow
        TabStop = False
        Alignment = taCenter
        BorderStyle = bsNone
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Lines.Strings = (
          'Drag these files to the file list,'
          'trashcan, images or memo')
        ParentFont = False
        ReadOnly = True
        TabOrder = 2
      end
    end
    object Fishname: TDBEdit
      Left = 56
      Top = 326
      Width = 121
      Height = 20
      TabStop = False
      AutoSize = False
      BorderStyle = bsNone
      Color = clBtnFace
      Ctl3D = False
      DataField = 'Common_Name'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 6
      Visible = False
    end
    object Memo5: TMemo
      Left = 9
      Top = 480
      Width = 186
      Height = 14
      Cursor = crArrow
      TabStop = False
      Alignment = taCenter
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Lines.Strings = (
        'Drag this to the archive or file list')
      ParentFont = False
      ReadOnly = True
      TabOrder = 7
    end
    object Memo6: TMemo
      Left = 312
      Top = 480
      Width = 186
      Height = 14
      Cursor = crArrow
      TabStop = False
      Alignment = taCenter
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Lines.Strings = (
        'Drag to the archive, file list or image')
      ParentFont = False
      ReadOnly = True
      TabOrder = 8
    end
    object Button1: TButton
      Left = 453
      Top = 286
      Width = 50
      Height = 29
      Caption = 'About'
      TabOrder = 9
      OnClick = Button1Click
    end
    object Panel1: TPanel
      Left = 202
      Top = 85
      Width = 102
      Height = 121
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 10
      OnClick = Panel1Click
      object Bevel1: TBevel
        Left = 37
        Top = 83
        Width = 28
        Height = 31
        ParentShowHint = False
        ShowHint = False
      end
      object Time: TLabel
        Left = 16
        Top = 58
        Width = 73
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Percentage: TLabel
        Left = 6
        Top = 22
        Width = 89
        Height = 16
        Alignment = taCenter
        AutoSize = False
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object TimeLabel: TLabel
        Left = 18
        Top = 43
        Width = 67
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Time'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 6
        Top = 5
        Width = 89
        Height = 16
        Caption = '% Compressed'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Trashcan: TImage
        Left = 38
        Top = 84
        Width = 26
        Height = 29
        Hint = 'Drag files here to delete them'
        DragMode = dmAutomatic
        ParentShowHint = False
        Picture.Data = {
          07544269746D617076020000424D760200000000000076000000280000001E00
          0000200000000100040000000000000200000000000000000000100000001000
          000000000000000080000080000000808000800000008000800080800000C0C0
          C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
          FF00EEEEEE000000000000000000EEEEEE00EEEEEE000000000000000000EEEE
          EE00EEEEEE00FF88888888888800EEEEEE00EEEEEE00FF88888888888800EEEE
          EE00EEEEEE00FF00770077008800EEEEEE00EEEEEE00FF00770077008800EEEE
          EE00EEEEEE00FF00770088008800EEEEEE00EEEEEE00FF00770088008800EEEE
          EE00EEEEEE00FF00770077008800EEEEEE00EEEEEE00FF00770077008800EEEE
          EE00EEEEEE00FF00770088008800EEEEEE00EEEEEE00FF00770088008800EEEE
          EE00EEEEEE00FF00770077008800EEEEEE00EEEEEE00FF00770077008800EEEE
          EE00EE00EE00FF00770088008800EE00EE00EE00EE00FF00770088008800EE00
          EE00EEEE0000FF0077007700880000EEEE00EEEE0000FF0077007700880000EE
          EE00EEEEEE00FF00770088008800EEEEEE00EEEEEE00FF00770088008800EEEE
          EE00EEEEEE008800880088008800EEEEEE00EEEEEE008800880088008800EEEE
          EE00EEEE0000000000000000000000EEEE00EEEE0000000000000000000000EE
          EE00EEEE00FF777777888888888800EEEE00EEEE00FF777777888888888800EE
          EE00EEEE0000000000000000000000EEEE00EEEE0000000000000000000000EE
          EE00EEEEEEEEEE0088888800EEEEEEEEEE00EEEEEEEEEE0088888800EEEEEEEE
          EE00EEEEEEEEEE0000000000EEEEEEEEEE00EEEEEEEEEE0000000000EEEEEEEE
          EE00}
        ShowHint = True
        Stretch = True
        OnDragDrop = TrashcanDragDrop
        OnDragOver = TrashcanDragOver
      end
    end
    object Button2: TButton
      Left = 340
      Top = 286
      Width = 50
      Height = 29
      Caption = 'Help'
      TabOrder = 11
      OnClick = Button2Click
    end
    object CDBImage1: TCDBImage
      Left = 256
      Top = 354
      Width = 241
      Height = 113
      DataField = 'Graphic'
      DataSource = DataSource1
      DragMode = dmAutomatic
      TabOrder = 12
      OnDragDrop = CDBImage1DragDrop
      OnDragOver = CDBImage1DragOver
      CompressionMethod = coNone
      CompressSource = Compress1
    end
    object CDBMemo1: TCDBMemo
      Left = 8
      Top = 354
      Width = 241
      Height = 113
      CompressionMethod = coNone
      CompressSource = Compress1
      DataField = 'Notes'
      DataSource = DataSource1
      DragMode = dmAutomatic
      TabOrder = 13
      OnDragDrop = CDBMemo1DragDrop
      OnDragOver = CDBMemo1DragOver
    end
    object Button3: TButton
      Left = 397
      Top = 286
      Width = 50
      Height = 29
      Hint = 'Registration information'
      Caption = 'Reg'#39'n'
      TabOrder = 14
      OnClick = Button3Click
    end
  end
  object Table1: TTable
    AfterPost = Table1AfterPost
    DatabaseName = 'DBDEMOS'
    TableName = 'BIOLIFE.DB'
    Left = 51
    Top = 284
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
    object CDBImage1Graphic: TCGraphicField
      FieldName = 'Graphic'
      BlobType = ftGraphic
      CompressSource = Compress1
      CompressionMethod = coNone
    end
    object CDBMemo1Notes: TCMemoField
      FieldName = 'Notes'
      BlobType = ftMemo
      Size = 50
      CompressSource = Compress1
      CompressionMethod = coNone
    end
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 83
    Top = 284
  end
  object Compress1: TCompress
    RegNumber = 0
    OnCheckFile = Compress1CheckFile
    OnShowProgress = Compress1ShowProgress
    MakeDirectories = False
    ExceptionOnFileError = False
    Key = 0
    CheckSpaceBeforeExpand = False
    Left = 20
    Top = 283
  end
end
