object MainForm: TMainForm
  Left = 484
  Top = 191
  Width = 310
  Height = 316
  Anchors = []
  Caption = '������'
  Color = clGray
  DefaultMonitor = dmMainForm
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '����'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 302
    Height = 24
    Anchors = []
    AutoSize = True
    ButtonHeight = 20
    ButtonWidth = 67
    Caption = 'ToolBar1'
    Color = clBtnFace
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    Flat = True
    ParentColor = False
    ShowCaptions = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      AllowAllUp = True
      AutoSize = True
      Caption = 'Delphi���'
      ImageIndex = 0
      OnClick = DelphiStyleClick
    end
    object ToolButton2: TToolButton
      Left = 71
      Top = 0
      AllowAllUp = True
      AutoSize = True
      Caption = 'VC++���'
      ImageIndex = 1
      OnClick = VCStyleClick
    end
    object ToolButton3: TToolButton
      Left = 130
      Top = 0
      AllowAllUp = True
      AutoSize = True
      Caption = 'VID���'
      ImageIndex = 2
      OnClick = VIDStyleClick
    end
    object ToolButton4: TToolButton
      Left = 183
      Top = 0
      Caption = 'VS.NET���'
      ImageIndex = 3
      OnClick = DockForm4Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 251
    Width = 302
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 24
    Width = 302
    Height = 227
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = '����'
    Font.Style = []
    Lines.Strings = (
      
        '��ӭ���ʹ��DockPresident�ؼ������Demo����������ʾ�ؼ���ʹ�÷�' +
        '����'
      
        '�����ڳ���Ĺ������������ĸ���ť��'#39'Delphi���'#39','#39'VC++���'#39','#39'VID��' +
        '��'#39'��'#39'VS.NET���'#39'��'
      #39'Delphi���'#39'��������Delphiͣ�����Ĵ��塣'
      #39'VC++���'#39'��������Visual C++ͣ�����Ĵ��塣'
      #39'VID���'#39'��������Visual InterDevͣ�����Ĵ��塣'
      #39'VS.NET���'#39'��������Visual Studio.NETͣ�����Ĵ��塣'
      
        '����û�ϣ������Delphi��ͣ�������������ڵ�ʱ��������������' +
        #39'CnDockServer1'#39'�ؼ���DockStyle�������ó�CnDelphiDockStyle1;Ȼ��' +
        '�������ڵ��'#39'Delphi���'#39'��ť����ͣ������,�����ͣ�������϶�����' +
        '���帽���Ϳ���ʵ��Delphi��ͣ�����'
      
        '����û�ϣ������Visual C++��ͣ�������������ڵ�ʱ�����������' +
        '���'#39'CnDockServer1'#39'�ؼ���DockStyle�������ó�CnVCDockStyle1;Ȼ��' +
        '�������ڵ��'#39'VC++���'#39'��ť����ͣ����,�����ͣ�������϶���������' +
        '�����Ϳ���ʵ��Visual C++��ͣ�����'
      
        '����û�ϣ������Visual InterDev��ͣ�������������ڵ�ʱ�����' +
        '���������'#39'CnDockServer1'#39'�ؼ���DockStyle�������ó�CnVIDDockStyle' +
        '1;Ȼ���������ڵ��'#39'VID���'#39'��ť����ͣ������,�����ͣ�������϶���' +
        '�����帽���Ϳ���ʵ��Visual InterDev��ͣ�����'
      
        '����û�ϣ������Visual Studio.NET��ͣ�������������ڵ�ʱ���' +
        '�����������'#39'CnDockServer1'#39'�ؼ���DockStyle�������ó�CnVSNETDockS' +
        'tyle1;Ȼ���������ڵ��'#39'VSNET���'#39'��ť����ͣ������,�����ͣ������' +
        '�϶��������帽���Ϳ���ʵ��Visual Studio.NET��ͣ�����')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
    WordWrap = False
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Left = 112
    Top = 48
    object DockForm_Menu: TMenuItem
      Caption = 'ͣ������'
      object DelphiStyle: TMenuItem
        Caption = 'Delphi���'
        OnClick = DelphiStyleClick
      end
      object VCStyle: TMenuItem
        Caption = 'VC++���'
        OnClick = VCStyleClick
      end
      object VIDStyle: TMenuItem
        Caption = 'VID���'
        OnClick = VIDStyleClick
      end
      object VSNETStyle: TMenuItem
        Caption = 'VSNET���'
      end
    end
    object ShowWindow_Menu: TMenuItem
      Caption = '��ʾ����'
    end
    object DockInfo_Menu: TMenuItem
      Caption = 'ͣ����Ϣ'
      object SaveToFile: TMenuItem
        Caption = '�洢���ļ�'
        OnClick = SaveToFileClick
      end
      object LoadFromFile: TMenuItem
        Caption = '���ļ���ԭ'
        OnClick = LoadFromFileClick
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object SaveToReg: TMenuItem
        Caption = '�洢��ע���'
        OnClick = SaveToRegClick
      end
      object LoadFromReg: TMenuItem
        Caption = '��ע���ԭ'
        OnClick = LoadFromRegClick
      end
    end
    object DockStyle_Menu: TMenuItem
      Caption = 'ͣ�����'
      Visible = False
      object Default: TMenuItem
        Caption = 'Ĭ��'
        OnClick = DefaultClick
      end
      object DelphiDockStyle: TMenuItem
        Caption = 'Delphi'
        OnClick = DelphiDockStyleClick
      end
      object VCDockStyle: TMenuItem
        Caption = 'Visual C++'
        OnClick = VCDockStyleClick
      end
      object VIDDockStyle: TMenuItem
        Caption = 'Visual InterDev'
        OnClick = VIDDockStyleClick
      end
    end
    object DockOption_Menu: TMenuItem
      Caption = 'ͣ��ѡ��'
      object TopDocked: TMenuItem
        Caption = '�ϱ߿�ͣ��'
        OnClick = TopDockedClick
      end
      object BottomDocked: TMenuItem
        Caption = '�±߿�ͣ��'
        OnClick = BottomDockedClick
      end
      object LeftDocked: TMenuItem
        Caption = '��߿�ͣ��'
        OnClick = LeftDockedClick
      end
      object RightDocked: TMenuItem
        Caption = '�ұ߿�ͣ��'
        OnClick = RightDockedClick
      end
      object N31: TMenuItem
        Caption = '-'
      end
      object AllDocked: TMenuItem
        Caption = 'ȫ�ֿ�ͣ��'
        OnClick = AllDockedClick
      end
    end
    object Set_Menu: TMenuItem
      Caption = '����'
      Visible = False
      object N10: TMenuItem
        Caption = '��ʾ�����˵�'
        OnClick = N10Click
      end
      object N11: TMenuItem
        Caption = '�Ƿ������ʾ'
        OnClick = N11Click
      end
      object N12: TMenuItem
        Caption = '��Ҷ�������ķ��'
        object N13: TMenuItem
          Tag = 1
          Caption = 'bsDialog'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object N14: TMenuItem
          Tag = 2
          Caption = 'bsNone'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object N15: TMenuItem
          Tag = 3
          Caption = 'bsSingle'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object N16: TMenuItem
          Tag = 4
          Caption = 'bsSizeable'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object N17: TMenuItem
          Tag = 5
          Caption = 'bsSizeToolWin'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object bsToolWindow1: TMenuItem
          Tag = 6
          Caption = 'bsToolWindow'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
      end
      object N18: TMenuItem
        Caption = 'ƽ�̷������ķ��'
        object bsDialog1: TMenuItem
          Tag = 1
          Caption = 'bsDialog'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsNone1: TMenuItem
          Tag = 2
          Caption = 'bsNone'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsSingle1: TMenuItem
          Tag = 3
          Caption = 'bsSingle'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsSizeable1: TMenuItem
          Tag = 4
          Caption = 'bsSizeable'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsSizeToolWin1: TMenuItem
          Tag = 5
          Caption = 'bsSizeToolWin'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsToolWindow2: TMenuItem
          Tag = 6
          Caption = 'bsToolWindow'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 192
    Top = 48
    object N5: TMenuItem
      Tag = 1
      Caption = '�ϱ�'
      OnClick = N5Click
    end
    object N6: TMenuItem
      Tag = 2
      Caption = '���'
      OnClick = N5Click
    end
    object N7: TMenuItem
      Tag = 3
      Caption = '�±�'
      OnClick = N5Click
    end
    object N8: TMenuItem
      Tag = 4
      Caption = '�ұ�'
      OnClick = N5Click
    end
  end
  object CnDockServer1: TCnDockServer
    LeftSplitterStyle.Cursor = crHSplit
    LeftSplitterStyle.ParentColor = False
    LeftSplitterStyle.Size = 4
    TopSplitterStyle.Cursor = crVSplit
    TopSplitterStyle.ParentColor = False
    TopSplitterStyle.Size = 4
    RightSplitterStyle.Cursor = crHSplit
    RightSplitterStyle.ParentColor = False
    RightSplitterStyle.Size = 4
    BottomSplitterStyle.Cursor = crVSplit
    BottomSplitterStyle.ParentColor = False
    BottomSplitterStyle.Size = 4
    DockStyle = CnVSNETDockStyle1
    Left = 32
    Top = 48
  end
  object CnDelphiDockStyle1: TCnDelphiDockStyle
    ConjoinServerOption.GrabbersSize = 12
    ConjoinServerOption.SplitterWidth = 4
    Left = 32
    Top = 112
  end
  object CnVCDockStyle1: TCnVCDockStyle
    ConjoinServerOption.GrabbersSize = 15
    ConjoinServerOption.SplitterWidth = 4
    Left = 112
    Top = 112
  end
  object CnVIDDockStyle1: TCnVIDDockStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = [fsBold]
    ConjoinServerOption.InactiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.InactiveFont.Color = 13160660
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = [fsBold]
    ConjoinServerOption.TextAlignment = taLeftJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = 15780518
    ConjoinServerOption.InactiveTitleStartColor = clGray
    ConjoinServerOption.InactiveTitleEndColor = clSilver
    ConjoinServerOption.TextEllipsis = True
    ConjoinServerOption.SystemInfo = True
    TabServerOption.TabPosition = tpBottom
    TabServerOption.ActiveSheetColor = clBtnFace
    TabServerOption.InactiveSheetColor = clBtnShadow
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Sans Serif'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = clWhite
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Sans Serif'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = False
    Left = 192
    Top = 112
  end
  object PopupMenu2: TPopupMenu
    OnPopup = PopupMenu2Popup
    Left = 112
    Top = 176
    object ClientTopDocked: TMenuItem
      Caption = '�ϱ߿�ͣ��'
      Checked = True
      OnClick = ClientTopDockedClick
    end
    object ClientBottomDocked: TMenuItem
      Caption = '�±߿�ͣ��'
      Checked = True
      OnClick = ClientBottomDockedClick
    end
    object ClientLeftDocked: TMenuItem
      Caption = '��߿�ͣ��'
      Checked = True
      OnClick = ClientLeftDockedClick
    end
    object ClientRightDocked: TMenuItem
      Caption = '�ұ߿�ͣ��'
      Checked = True
      OnClick = ClientRightDockedClick
    end
    object N20: TMenuItem
      Caption = '-'
    end
    object ClientEachOtherDocked: TMenuItem
      Caption = '�໥��ͣ��'
      Checked = True
      OnClick = ClientEachOtherDockedClick
    end
    object ClientAllDocked: TMenuItem
      Caption = 'ȫ�ֿ�ͣ��'
      Checked = True
      OnClick = ClientAllDockedClick
    end
    object N21: TMenuItem
      Caption = '-'
    end
    object ClientDockorFloat: TMenuItem
      Caption = 'ͣ��'
      OnClick = ClientDockorFloatClick
    end
    object ClientHide: TMenuItem
      Caption = '����'
      OnClick = ClientHideClick
    end
  end
  object CnVSNETDockStyle1: TCnVSNETDockStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = []
    ConjoinServerOption.InactiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.InactiveFont.Color = clBlack
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = []
    ConjoinServerOption.TextAlignment = taLeftJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = 6956042
    ConjoinServerOption.InactiveTitleStartColor = clBtnFace
    ConjoinServerOption.InactiveTitleEndColor = clBtnFace
    ConjoinServerOption.TextEllipsis = True
    ConjoinServerOption.SystemInfo = True
    TabServerOption.TabPosition = tpBottom
    TabServerOption.ActiveSheetColor = clBtnFace
    TabServerOption.InactiveSheetColor = clInfoBk
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Sans Serif'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = 5395794
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Sans Serif'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = True
    ChannelOption.ActivePaneSize = 150
    ChannelOption.ShowImage = True
    Left = 32
    Top = 176
  end
end
