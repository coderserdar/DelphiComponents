object Form1: TForm1
  Left = 203
  Top = 117
  Width = 1078
  Height = 783
  Caption = 'DSToolBar Demo'
  Color = 4210752
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DockTop: TDsDockSite
    Left = 0
    Top = 0
    Width = 1070
    Height = 89
    AutoSize = True
    Align = alTop
    Color = 16103305
    ColorTo = 16768734
    PopupMenu = PopupMenu2
    object MdToolBar1: TDSToolBar
      Left = 10
      Top = 65
      Width = 207
      Height = 22
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16768734
      ColorTo = 16103305
      object MdToolButton1: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton2: TDSToolButton
        Left = 23
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton7: TDSToolButton
        Left = 46
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton8: TDSToolButton
        Left = 115
        Top = 0
        Width = 23
        Height = 22
        Action = actNew
        Flat = True
        Down = False
        Hot = False
        Check = True
        Images = ImageList1
        ImageIndex = 5
        DropButton = False
        ShowHint = True
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton9: TDSToolButton
        Left = 184
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton10: TDSToolButton
        Left = 161
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton11: TDSToolButton
        Left = 92
        Top = 0
        Width = 23
        Height = 22
        Action = actNew
        Flat = True
        Down = False
        Hot = False
        Check = True
        Images = ImageList1
        ImageIndex = 5
        DropButton = False
        ShowHint = True
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton13: TDSToolButton
        Left = 138
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        Caption = 'actNew'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
        OnClick = actNewExecute
      end
      object MdToolButton12: TDSToolButton
        Left = 69
        Top = 0
        Width = 23
        Height = 22
        Action = actNew
        Flat = True
        Down = False
        Hot = False
        Check = True
        Images = ImageList1
        ImageIndex = 5
        DropButton = False
        ShowHint = True
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
    end
    object MdToolBar2: TDSToolBar
      Left = 229
      Top = 65
      Width = 132
      Height = 22
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16768734
      ColorTo = 16103305
      object MdToolButton3: TDSToolButton
        Left = 0
        Top = 0
        Width = 51
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'Caption'
        ShowImage = False
        ShowText = True
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton4: TDSToolButton
        Left = 109
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton5: TDSToolButton
        Left = 86
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton6: TDSToolButton
        Left = 51
        Top = 0
        Width = 35
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 2
        DropDownMenu = PopupMenu1
        DropButton = True
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
    end
    object MdToolBar3: TDSToolBar
      Left = 373
      Top = 65
      Width = 141
      Height = 22
      AutoSize = True
      DragKind = dkDock
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16768734
      ColorTo = 16103305
      object MdToolButton14: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Enabled = False
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 5
        DropDownMenu = PopupMenu1
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton15: TDSToolButton
        Left = 118
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 5
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ComboBox3: TComboBox
        Left = 23
        Top = 0
        Width = 95
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'Blue tones'
        OnChange = ComboBox3Change
        Items.Strings = (
          'Blue tones'
          'Gray tones')
      end
    end
    object MdToolBar4: TDSToolBar
      Left = 864
      Top = 2
      Width = 204
      Height = 13
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16768734
      ColorTo = 16103305
      object ProgressBar1: TProgressBar
        Left = 0
        Top = 0
        Width = 204
        Height = 13
        Position = 50
        TabOrder = 0
      end
    end
    object barMenu: TDSToolBar
      Left = 10
      Top = 2
      Width = 188
      Height = 19
      AutoSize = True
      DragKind = dkDock
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Menu = MainMenu1
      Color = 16768734
      ColorTo = 16103305
    end
    object MdToolBar6: TDSToolBar
      Left = 10
      Top = 25
      Width = 243
      Height = 36
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16768734
      ColorTo = 16103305
      object MdToolButton18: TDSToolButton
        Left = 41
        Top = 0
        Width = 41
        Height = 36
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 5
        DropButton = False
        Caption = 'Test2'
        ShowImage = True
        ShowText = True
        TextToImage = tipOverlayImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton19: TDSToolButton
        Left = 0
        Top = 0
        Width = 41
        Height = 36
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'Test1'
        ShowImage = True
        ShowText = True
        TextToImage = tipBelowImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton23: TDSToolButton
        Left = 82
        Top = 0
        Width = 41
        Height = 36
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 5
        DropButton = False
        Caption = 'Test3'
        ShowImage = True
        ShowText = True
        TextToImage = tipAboveImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton24: TDSToolButton
        Left = 123
        Top = 0
        Width = 60
        Height = 36
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'Test4'
        ShowImage = True
        ShowText = True
        TextToImage = tipBeforeImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MdToolButton25: TDSToolButton
        Left = 183
        Top = 0
        Width = 60
        Height = 36
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'Test5'
        ShowImage = True
        ShowText = True
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
    end
  end
  object DockLeft: TDsDockSite
    Left = 0
    Top = 89
    Width = 27
    Height = 620
    AutoSize = True
    Align = alLeft
    Color = 16103305
    ColorTo = 16768734
    PopupMenu = PopupMenu2
    object ToolBar4: TDSToolBar
      Left = 2
      Top = 132
      Width = 23
      Height = 66
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Vertical = True
      Color = 16768734
      ColorTo = 16103305
      object ToolButton18: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'ToolButton18'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton19: TDSToolButton
        Left = 0
        Top = 22
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton19'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton20: TDSToolButton
        Left = 0
        Top = 44
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton20'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
    end
    object ToolBar5: TDSToolBar
      Left = 2
      Top = 210
      Width = 23
      Height = 88
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Vertical = True
      Color = 16768734
      ColorTo = 16103305
      object ToolButton21: TDSToolButton
        Left = 0
        Top = 66
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'ToolButton21'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton22: TDSToolButton
        Left = 0
        Top = 44
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton22'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton23: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton23'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton24: TDSToolButton
        Left = 0
        Top = 22
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        Caption = 'ToolButton24'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
    end
    object ToolBar6: TDSToolBar
      Left = 2
      Top = 10
      Width = 23
      Height = 110
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Vertical = True
      Color = 16768734
      ColorTo = 16103305
      object ToolButton13: TDSToolButton
        Left = 0
        Top = 88
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'ToolButton13'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton14: TDSToolButton
        Left = 0
        Top = 66
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton14'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton15: TDSToolButton
        Left = 0
        Top = 44
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton15'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton16: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = True
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        Caption = 'ToolButton16'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton17: TDSToolButton
        Left = 0
        Top = 22
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = True
        Group = DsToolButtonGroup1
        Images = ImageList1
        ImageIndex = 4
        DropButton = False
        Caption = 'ToolButton17'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
    end
  end
  object DockRight: TDsDockSite
    Left = 1043
    Top = 89
    Width = 27
    Height = 620
    AutoSize = True
    Align = alRight
    Color = 16768734
    ColorTo = 16103305
    PopupMenu = PopupMenu2
    object ToolBar10: TDSToolBar
      Left = 2
      Top = 10
      Width = 23
      Height = 110
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Vertical = True
      Color = 16103305
      ColorTo = 16768734
      object ToolButton25: TDSToolButton
        Left = 0
        Top = 88
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'ToolButton25'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton26: TDSToolButton
        Left = 0
        Top = 66
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton26'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton27: TDSToolButton
        Left = 0
        Top = 44
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton27'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton28: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        Caption = 'ToolButton28'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton29: TDSToolButton
        Left = 0
        Top = 22
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 4
        DropButton = False
        Caption = 'ToolButton29'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
    end
    object ToolBar11: TDSToolBar
      Left = 2
      Top = 132
      Width = 23
      Height = 66
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Vertical = True
      Color = 16103305
      ColorTo = 16768734
      object ToolButton30: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'ToolButton30'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton31: TDSToolButton
        Left = 0
        Top = 22
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton31'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton32: TDSToolButton
        Left = 0
        Top = 44
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton32'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
    end
    object ToolBar12: TDSToolBar
      Left = 2
      Top = 210
      Width = 23
      Height = 88
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Vertical = True
      Color = 16103305
      ColorTo = 16768734
      object ToolButton33: TDSToolButton
        Left = 0
        Top = 66
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'ToolButton33'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton34: TDSToolButton
        Left = 0
        Top = 44
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton34'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton35: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton35'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
      object ToolButton36: TDSToolButton
        Left = 0
        Top = 22
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        Caption = 'ToolButton36'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toVertical
        PopupSide = popBottom
      end
    end
  end
  object DockBottom: TDsDockSite
    Left = 0
    Top = 709
    Width = 1070
    Height = 40
    AutoSize = True
    Align = alBottom
    Color = 16768734
    ColorTo = 16103305
    PopupMenu = PopupMenu2
    object ToolBar1: TDSToolBar
      Left = 608
      Top = 2
      Width = 69
      Height = 22
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16103305
      ColorTo = 16765371
      object ToolButton46: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Enabled = False
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'ToolButton46'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton47: TDSToolButton
        Left = 23
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton47'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton48: TDSToolButton
        Left = 46
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton48'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
    end
    object ToolBar8: TDSToolBar
      Left = 481
      Top = 2
      Width = 115
      Height = 22
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16103305
      ColorTo = 16765371
      object ToolButton41: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'ToolButton41'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton42: TDSToolButton
        Left = 23
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton42'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton43: TDSToolButton
        Left = 46
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton43'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton44: TDSToolButton
        Left = 69
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        Caption = 'ToolButton44'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton45: TDSToolButton
        Left = 92
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 4
        DropButton = False
        Caption = 'ToolButton45'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
    end
    object ToolBar3: TDSToolBar
      Left = 114
      Top = 2
      Width = 92
      Height = 22
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16103305
      ColorTo = 16765371
      object ToolButton37: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Action = actNew
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 5
        DropButton = False
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton38: TDSToolButton
        Left = 23
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton38'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton40: TDSToolButton
        Left = 46
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        Caption = 'ToolButton40'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton39: TDSToolButton
        Left = 69
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton39'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
    end
    object ToolBar2: TDSToolBar
      Left = 10
      Top = 2
      Width = 92
      Height = 22
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16103305
      ColorTo = 16765371
      object ToolButton9: TDSToolButton
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton9'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton12: TDSToolButton
        Left = 23
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 4
        DropButton = False
        Caption = 'ToolButton12'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton10: TDSToolButton
        Left = 46
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 2
        DropButton = False
        Caption = 'ToolButton10'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object MyToolButton1: TDSToolButton
        Left = 69
        Top = 0
        Width = 23
        Height = 22
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropDownMenu = PopupMenu1
        DropButton = False
        Caption = 'ToolButton11'
        ShowImage = True
        ShowText = False
        TextToImage = tipAfterImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
    end
    object ToolBar7: TDSToolBar
      Left = 218
      Top = 2
      Width = 251
      Height = 36
      AutoSize = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Color = 16103305
      ColorTo = 16765371
      object ToolButton4: TDSToolButton
        Left = 216
        Top = 0
        Width = 35
        Height = 36
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 0
        DropButton = False
        Caption = 'Test'
        ShowImage = True
        ShowText = True
        TextToImage = tipBelowImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton5: TDSToolButton
        Left = 144
        Top = 0
        Width = 72
        Height = 36
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 1
        DropButton = False
        Caption = 'ToolButton5'
        ShowImage = True
        ShowText = True
        TextToImage = tipBelowImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton7: TDSToolButton
        Left = 72
        Top = 0
        Width = 72
        Height = 36
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 3
        DropButton = False
        Caption = 'ToolButton7'
        ShowImage = True
        ShowText = True
        TextToImage = tipBelowImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
      object ToolButton6: TDSToolButton
        Left = 0
        Top = 0
        Width = 72
        Height = 36
        Flat = True
        Down = False
        Hot = False
        Check = False
        Images = ImageList1
        ImageIndex = 2
        DropDownMenu = PopupMenu1
        DropButton = False
        Caption = 'ToolButton6'
        ShowImage = True
        ShowText = True
        TextToImage = tipBelowImage
        TextOrientation = toHorizontal
        PopupSide = popBottom
      end
    end
  end
  object pnlMain: TPanel
    Left = 27
    Top = 89
    Width = 1016
    Height = 620
    Align = alClient
    BevelOuter = bvNone
    Color = 4210752
    TabOrder = 4
    object btnSave: TButton
      Left = 184
      Top = 72
      Width = 137
      Height = 25
      Caption = 'Save current positions'
      TabOrder = 0
      OnClick = btnSaveClick
    end
  end
  object ImageList1: TImageList
    Left = 280
    Top = 111
    Bitmap = {
      494C010106000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000007F7F7F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000FFFF00000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      00007F7F7F0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000FFFF000000
      000000000000000000000000000000FFFF000000000000000000000000007F7F
      7F0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000000000000000000000FF
      FF000000000000000000000000000000000000000000000000000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF00000000000000000000000000FFFFFF00000000007F7F7F000000
      0000FFFFFF0000000000000000000000000000FFFF0000FFFF0000FFFF000000
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
      000000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF0000000000FFFFFF00000000007F7F7F000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF0000000000FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000007F7F
      7F00000000000000000000FFFF0000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF0000000000000000000000000000000000000000007F7F7F0000FF
      FF000000000000000000000000000000000000000000000000000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000FFFF
      FF0000000000000000000000000000000000000000007F7F7F0000FFFF000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      000000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      00000000000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BFBFBF00BFBFBF00BFBF
      BF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBF
      BF00BFBFBF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      00000000000000000000000000000000000000000000BFBFBF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BFBFBF00BFBFBF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007F7F
      7F007F7F7F0000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      80000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      8000000000000000000000000000000000000000000000000000BFBFBF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000BFBFBF00BFBFBF000000000000000000FF000000000000000000
      00000000000000000000000000000000000000000000000000007F7F7F007F7F
      7F00000000000000000000000000000000000000000000000000000080000000
      8000000080000000800000008000000080000000800000008000000000000000
      8000000080000000000000000000000000000000000000000000000080000000
      8000000080000000800000008000000080000000800000008000000000000000
      8000000080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF0000000000BFBFBF0000000000FF00000000000000FF000000FF00
      000000000000000000000000000000000000000000007F7F7F007F7F7F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000080000000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000800000008000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF000000000000000000BFBFBF0000000000FF00000000000000000000000000
      0000FF0000000000000000000000000000007F7F7F007F7F7F00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000007F7F7F000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF000000000000008000000080000000000000000000000000007F7F7F000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00000000000000800000008000000000000000000000000000FFFFFF000000
      000000000000FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF0000000000BFBFBF0000000000FF000000FF000000000000000000
      0000000000000000000000000000FFFFFF007F7F7F007F7F7F007F7F7F007F7F
      7F007F7F7F007F7F7F007F7F7F00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00000000000000000000008000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF000000000000008000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF0000000000BFBFBF000000000000000000FF000000FF000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF007F7F7F0000000000000000000000000000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF007F7F7F0000000000000000000000000000000000BFBF
      BF00BFBFBF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBF
      BF00BFBFBF00FFFFFF000000000000000000000000000000FF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000FF0000000000000000000000000000000000FF00
      000000000000FF000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF007F7F7F0000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000000000000000FF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000FF00000000000000000000000000FF0000000000
      000000000000FF000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000FF0000000000
      000000000000FF000000000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF007F7F7F000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000FFFFFF000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000FF0000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF007F7F7F0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000008003FF7F000000008003DE7900000000
      8003AE73000000008003D6E7000000008003EE2F000000008003FC1F00000000
      8003F80F00000000800380080000000080031001000000008003F01F00000000
      8003F83F000000008003E477000000008003CF6B0000000080079E7500000000
      800FBE7B00000000801FFEFF00000000FFFFFFFF8007FFFFE01FE01F0003FFE7
      C00FC00F0001FFC78007800780108F8F00030003000007000001000100003200
      8000800080000000C000C00080008000E000E0000000F900F000F0000000E100
      F801F8010000C900FC01F8010000C900FE01F801C001C300FF1FF807C001E300
      FFFFF807C007FF01FFFFFC7FE3FFFF0300000000000000000000000000000000
      000000000000}
  end
  object PopupMenu1: TPopupMenu
    Left = 200
    Top = 110
    object miTest1: TMenuItem
      Caption = 'Test'
      OnClick = miTest1Click
    end
    object miTest2: TMenuItem
      Caption = 'Test'
      OnClick = miTest1Click
    end
    object miTest3: TMenuItem
      Caption = 'Test'
      OnClick = miTest1Click
    end
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 120
    Top = 110
    object actNew: TAction
      AutoCheck = True
      Caption = 'actNew'
      Hint = 'TestHint'
      ImageIndex = 5
      OnExecute = actNewExecute
    end
    object actCheck: TAction
      Caption = 'actCheck'
      OnExecute = actCheckExecute
    end
  end
  object DsToolButtonGroup1: TDSToolButtonGroup
    AllowAllUp = False
    Left = 81
    Top = 110
  end
  object PopupMenu2: TPopupMenu
    Left = 240
    Top = 110
    object N2: TMenuItem
      Caption = '-'
    end
    object miLockTop: TMenuItem
      Caption = 'LockTop'
      OnClick = miLockTopClick
    end
    object miLockLeft: TMenuItem
      Caption = 'LockLeft'
      OnClick = miLockLeftClick
    end
    object miLockRight: TMenuItem
      Caption = 'LockRight'
      OnClick = miLockRightClick
    end
    object miLockBottom: TMenuItem
      Caption = 'LockBottom'
      OnClick = miLockBottomClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miLockall: TMenuItem
      Caption = 'Lock all'
      OnClick = miLockallClick
    end
    object miUnlockAll: TMenuItem
      Caption = 'Unlock All'
      OnClick = miUnlockAllClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 160
    Top = 110
    object miFile: TMenuItem
      Caption = 'File'
      object actNew1: TMenuItem
        Action = actNew
        AutoCheck = True
      end
      object N11: TMenuItem
        Caption = '1'
        OnClick = N11Click
      end
      object N21: TMenuItem
        Caption = '2'
      end
    end
    object miEdit: TMenuItem
      Caption = 'Edit'
      object asdfasdf1: TMenuItem
        Caption = 'asdfasdf'
      end
      object asdfasfd1: TMenuItem
        Caption = 'asdfasfd'
      end
      object asdfasdf2: TMenuItem
        Caption = 'asdfasdf'
      end
    end
    object miSearch: TMenuItem
      Caption = 'Search'
      object Find1: TMenuItem
        Caption = 'Find'
      end
    end
    object miView: TMenuItem
      Caption = 'View'
      object Hide1: TMenuItem
        Caption = 'Hide'
      end
    end
    object miAbout: TMenuItem
      Caption = 'About'
      object Help1: TMenuItem
        Caption = 'Help'
      end
    end
  end
  object DsManager1: TDsManager
    Left = 43
    Top = 110
  end
end
