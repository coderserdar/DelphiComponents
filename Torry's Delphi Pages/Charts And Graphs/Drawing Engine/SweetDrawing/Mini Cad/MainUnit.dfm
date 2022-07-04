object MainForm: TMainForm
  Left = 138
  Top = 129
  Width = 902
  Height = 645
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Mini CAD - [New]'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SCCAD1: TSCDeEditor
    Left = 0
    Top = 49
    Width = 640
    Height = 531
    Align = alClient
    BorderProps.Border = scdcbLowered
    BorderProps.Color = clBtnFace
    Color = 16769505
    ControlKeys = [scskAlt, scskCtrl]
    CursorGuide.Color = clPurple
    CursorGuide.Style = psDot
    DefaultFont.Name = 'MS Sans Serif'
    Frame.Color = 3982847
    Frame.ShadowColor = 10724259
    Guide.Color = clYellow
    Guide.Mode = pmXor
    KeyMoveSizeStep = 1
    Layer.Width = 600
    Layer.Height = 600
    MoveSize.LineStyle = psSolid
    Rulers.Font.Charset = TURKISH_CHARSET
    Rulers.Font.Color = clWindowText
    Rulers.Font.Height = -9
    Rulers.Font.Name = 'Microsoft Sans Serif'
    Rulers.Font.Style = []
    Scrollbars.Style = scssOffice12
    SelectionRect.LineColor = clBlue
    TabOrder = 0
    TabStop = True
    UndoLimit = 10
    OnChange = SCCAD1Change
    OnClipboardChange = SCCAD1ClipboardChange
    OnEditStateChange = SCCAD1EditStateChange
    OnEndNewShape = SCCAD1EndNewShape
    OnGuideChange = SCCAD1GuideChange
    OnNewShapeClass = SCCAD1EditStateChange
    OnSelectionChange = SCCAD1SelectionChange
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 580
    Width = 894
    Height = 19
    AutoHint = True
    Panels = <>
    SimplePanel = False
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 894
    Height = 24
    Caption = 'ToolBar1'
    Flat = True
    Images = ImageList1
    TabOrder = 2
    Wrapable = False
    object ButtonNew: TToolButton
      Left = 0
      Top = 0
      Action = FileNew
      ParentShowHint = False
      ShowHint = True
    end
    object ButtonOpen: TToolButton
      Left = 23
      Top = 0
      Action = FileOpen
      ParentShowHint = False
      ShowHint = True
    end
    object ButtonSave: TToolButton
      Left = 46
      Top = 0
      Action = FileSave
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton10: TToolButton
      Left = 69
      Top = 0
      Width = 8
      Caption = 'ToolButton8'
      ImageIndex = 30
      Style = tbsSeparator
    end
    object ButtonUndo: TToolButton
      Left = 77
      Top = 0
      Action = EditUndo
    end
    object ButtonRedo: TToolButton
      Left = 100
      Top = 0
      Action = EditRedo
    end
    object ToolButton7: TToolButton
      Left = 123
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 30
      Style = tbsSeparator
    end
    object ButtonCut: TToolButton
      Left = 131
      Top = 0
      Action = EditCut
      ParentShowHint = False
      ShowHint = True
    end
    object ButtonCopy: TToolButton
      Left = 154
      Top = 0
      Action = EditCopy
      ParentShowHint = False
      ShowHint = True
    end
    object ButtonPaste: TToolButton
      Left = 177
      Top = 0
      Action = EditPaste
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton5: TToolButton
      Left = 200
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 30
      Style = tbsSeparator
    end
    object ButtonBringToFront: TToolButton
      Left = 208
      Top = 0
      Action = ArrangeBringToFront
      ParentShowHint = False
      ShowHint = True
    end
    object ButtonBringForward: TToolButton
      Left = 231
      Top = 0
      Action = ArrangeBringForward
      ParentShowHint = False
      ShowHint = True
    end
    object ButtonSendToBack: TToolButton
      Left = 254
      Top = 0
      Action = ArrangeSendToBack
      ParentShowHint = False
      ShowHint = True
    end
    object ButtonSendBackward: TToolButton
      Left = 277
      Top = 0
      Action = ArrangeSendBackward
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton32: TToolButton
      Left = 300
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 27
      Style = tbsSeparator
    end
    object ButtonGroup: TToolButton
      Left = 308
      Top = 0
      Action = ArrangeGroup
      ParentShowHint = False
      ShowHint = True
    end
    object ButtonUngroup: TToolButton
      Left = 331
      Top = 0
      Action = ArrangeUngroup
      ParentShowHint = False
      ShowHint = True
    end
  end
  object ToolBar3: TToolBar
    Left = 0
    Top = 24
    Width = 894
    Height = 25
    Caption = 'ToolBar1'
    Flat = True
    Images = ImageList1
    TabOrder = 3
    Wrapable = False
    object ButtonSelect: TToolButton
      Left = 0
      Top = 0
      Action = ToolSelect
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ButtonRect: TToolButton
      Left = 31
      Top = 0
      Action = InsertRect
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonCircle: TToolButton
      Left = 54
      Top = 0
      Action = InsertCircle
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonEllipse: TToolButton
      Left = 77
      Top = 0
      Action = InsertEllipse
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonArc: TToolButton
      Left = 100
      Top = 0
      Action = InsertArc
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonPolygon: TToolButton
      Left = 123
      Top = 0
      Action = InsertPolygon
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonBezier: TToolButton
      Left = 146
      Top = 0
      Action = InsertBezier
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonFreeHand: TToolButton
      Left = 169
      Top = 0
      Action = InsertFreehand
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonText: TToolButton
      Left = 192
      Top = 0
      Action = InsertText
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonLabel: TToolButton
      Left = 215
      Top = 0
      Action = InsertLabel
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonPicture: TToolButton
      Left = 238
      Top = 0
      Action = InsertPicture
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ToolButton1: TToolButton
      Left = 261
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 21
      Style = tbsSeparator
    end
    object ButtonPan: TToolButton
      Left = 269
      Top = 0
      Action = ToolPan
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonZoom: TToolButton
      Left = 292
      Top = 0
      Caption = 'Zoom'
      DropdownMenu = PopupMenu1
      ImageIndex = 40
      Style = tbsDropDown
      OnClick = ViewZoomInExecute
    end
    object ButtonZoomOut: TToolButton
      Left = 328
      Top = 0
      Action = ViewZoomOut
    end
    object ToolButton6: TToolButton
      Left = 351
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 22
      Style = tbsSeparator
    end
    object ButtonPen: TToolButton
      Left = 359
      Top = 0
      Action = ToolPen
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ButtonRemovePoint: TToolButton
      Left = 382
      Top = 0
      Action = ToolRemovePoint
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object ToolButton3: TToolButton
      Left = 405
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 22
      Style = tbsSeparator
    end
    object ButtonDelete: TToolButton
      Left = 413
      Top = 0
      Action = EditDelete
      ParentShowHint = False
      ShowHint = True
    end
  end
  object SCSplitter1: TSCSplitter
    Left = 640
    Top = 49
    Width = 8
    Height = 531
    AlignSplitter = scsaRight
    Control = SCGroupContainer2
    HotZone.Color = 14003078
    HotZone.Size = 52
    HotZone.Style = scshMacromedia
    Hottrack = False
    ResizeStyle = scrsPattern
  end
  object SCGroupContainer2: TSCGroupContainer
    Left = 648
    Top = 49
    Width = 246
    Height = 531
    Align = alRight
    EndColor = clBtnShadow
    Gradient = scgNone
    Indent = 0
    Scrollbar.ShowState = scsbHide
    Scrollbar.Style = scssOffice12
    Spacing = 0
    TabOrder = 5
    Style = scasOutlookBar2k
    object SCAdvPanel4: TSCAdvPanel
      Left = 0
      Top = 0
      Width = 244
      Height = 503
      ShowCaptionLine = False
      Bevel = sccbNone
      Caption = 'Properties'
      CaptionSettings.ButtonColors.Color = clBtnShadow
      CaptionSettings.ButtonColors.DisabledColor = clBtnShadow
      CaptionSettings.ButtonColors.DownColor = clBtnShadow
      CaptionSettings.ButtonColors.HotColor = clBtnShadow
      CaptionSettings.ButtonColors.IconColor = clBtnHighlight
      CaptionSettings.ButtonColors.IconDisabledColor = clBtnFace
      CaptionSettings.ButtonColors.IconDownColor = clBtnFace
      CaptionSettings.ButtonColors.IconHotColor = clBtnFace
      CaptionSettings.DefaultProps.Color = clBtnFace
      CaptionSettings.DefaultProps.EndColor = clHighlight
      CaptionSettings.DefaultProps.Gradient = scgNone
      CaptionSettings.DefaultProps.LineColor = clNone
      CaptionSettings.DisabledProps.Color = clBtnFace
      CaptionSettings.DisabledProps.EndColor = clBtnShadow
      CaptionSettings.DisabledProps.Gradient = scgNone
      CaptionSettings.DisabledProps.LineColor = clBtnText
      CaptionSettings.Font.Charset = DEFAULT_CHARSET
      CaptionSettings.Font.Color = clWindowText
      CaptionSettings.Font.Height = -11
      CaptionSettings.Font.Name = 'MS Sans Serif'
      CaptionSettings.Font.Style = []
      CaptionSettings.HotProps.Color = clBtnFace
      CaptionSettings.HotProps.EndColor = 11166794
      CaptionSettings.HotProps.Gradient = scgNone
      CaptionSettings.HotProps.LineColor = clNone
      CaptionSettings.Style = scgcSoftRaised
      Color = clBtnShadow
      ExpandedHeight = 288
      RectDefaultButtons = True
      Rounded = False
      ShowClose = False
      ShowRoller = False
      TabOrder = 0
      Style = scasOutlookBar2k
      object Panel2: TPanel
        Left = 0
        Top = 26
        Width = 244
        Height = 477
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object SCGroupContainer1: TSCGroupContainer
          Left = 0
          Top = 0
          Width = 244
          Height = 477
          Align = alClient
          BorderProps.Border = sccbNone
          Color = clBtnFace
          Scrollbar.Style = scssOffice12
          TabOrder = 0
          Style = scasXPBar
          object SCAdvPanel1: TSCAdvPanel
            Left = 9
            Top = 9
            Width = 210
            Height = 123
            Caption = 'General'
            CaptionSettings.Font.Charset = DEFAULT_CHARSET
            CaptionSettings.Font.Color = clHighlight
            CaptionSettings.Font.Height = -11
            CaptionSettings.Font.Name = 'MS Sans Serif'
            CaptionSettings.Font.Style = [fsBold]
            ExpandedHeight = 123
            ShowClose = False
            TabOrder = 0
            Style = scasXPBar
            object Label7: TLabel
              Left = 8
              Top = 42
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Name:'
              Layout = tlCenter
            end
            object Label8: TLabel
              Left = 8
              Top = 66
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Text:'
              Layout = tlCenter
            end
            object Label9: TLabel
              Left = 8
              Top = 90
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Caption:'
              Layout = tlCenter
            end
            object SCFrameEdit1: TSCFrameEdit
              Left = 88
              Top = 40
              Width = 109
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Style = scesOffice12
              TabOrder = 0
              OnExit = SCFrameEdit1Exit
              OnKeyDown = SCFrameEdit1KeyDown
            end
            object SCFrameEdit2: TSCFrameEdit
              Left = 88
              Top = 64
              Width = 109
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Style = scesOffice12
              TabOrder = 1
              OnExit = SCFrameEdit2Exit
              OnKeyDown = SCFrameEdit2KeyDown
            end
            object SCFrameEdit3: TSCFrameEdit
              Left = 88
              Top = 88
              Width = 109
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              Style = scesOffice12
              TabOrder = 2
              OnExit = SCFrameEdit3Exit
              OnKeyDown = SCFrameEdit3KeyDown
            end
          end
          object SCAdvPanel2: TSCAdvPanel
            Left = 9
            Top = 488
            Width = 210
            Height = 102
            Caption = 'Brush'
            CaptionSettings.Font.Charset = DEFAULT_CHARSET
            CaptionSettings.Font.Color = clHighlight
            CaptionSettings.Font.Height = -11
            CaptionSettings.Font.Name = 'MS Sans Serif'
            CaptionSettings.Font.Style = [fsBold]
            ExpandedHeight = 102
            ShowClose = False
            TabOrder = 1
            Style = scasXPBar
            object Label1: TLabel
              Left = 8
              Top = 42
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Style:'
              Layout = tlCenter
            end
            object Label2: TLabel
              Left = 8
              Top = 66
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Color:'
              Layout = tlCenter
            end
            object SCBrushStyleCombobox1: TSCBrushStyleCombobox
              Left = 92
              Top = 40
              Width = 105
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              BrushColor = clLime
              PopupProps.Font.Charset = DEFAULT_CHARSET
              PopupProps.Font.Color = clWindowText
              PopupProps.Font.Height = -11
              PopupProps.Font.Name = 'MS Sans Serif'
              PopupProps.Font.Style = []
              Style = scesOffice12
              TabOrder = 0
              OnSetStyle = SCBrushStyleCombobox1SetStyle
            end
            object SCPopupColors1: TSCPopupColors
              Left = 92
              Top = 67
              Width = 105
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              AutoCaption = 'Clear'
              CustomColors = <>
              SelectedColor = clLime
              Style = scesOffice12
              TabOrder = 1
              OnMoreButtonClick = SCPopupColors1MoreButtonClick
              OnSelectedColorChange = SCPopupColors1SelectedColorChange
            end
          end
          object SCAdvPanel3: TSCAdvPanel
            Left = 9
            Top = 598
            Width = 210
            Height = 144
            Caption = 'Pen'
            CaptionSettings.Font.Charset = DEFAULT_CHARSET
            CaptionSettings.Font.Color = clHighlight
            CaptionSettings.Font.Height = -11
            CaptionSettings.Font.Name = 'MS Sans Serif'
            CaptionSettings.Font.Style = [fsBold]
            ExpandedHeight = 144
            ShowClose = False
            TabOrder = 2
            Style = scasXPBar
            object Label3: TLabel
              Left = 8
              Top = 42
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Style:'
              Layout = tlCenter
            end
            object Label4: TLabel
              Left = 8
              Top = 90
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Color:'
              Layout = tlCenter
            end
            object Label5: TLabel
              Left = 8
              Top = 66
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Mode:'
              Layout = tlCenter
            end
            object Label6: TLabel
              Left = 8
              Top = 114
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Width:'
              Layout = tlCenter
            end
            object SCPopupColors2: TSCPopupColors
              Left = 92
              Top = 88
              Width = 105
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              AutoCaption = 'Clear'
              CustomColors = <>
              SelectedColor = clBlue
              Style = scesOffice12
              TabOrder = 0
              OnMoreButtonClick = SCPopupColors2MoreButtonClick
              OnSelectedColorChange = SCPopupColors2SelectedColorChange
            end
            object SCPenStyleCombobox1: TSCPenStyleCombobox
              Left = 92
              Top = 40
              Width = 105
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              PenColor = clBlue
              PopupProps.Font.Charset = DEFAULT_CHARSET
              PopupProps.Font.Color = clWindowText
              PopupProps.Font.Height = -11
              PopupProps.Font.Name = 'MS Sans Serif'
              PopupProps.Font.Style = []
              Style = scesOffice12
              StyleWidth = 48
              TabOrder = 1
              OnSetStyle = SCPenStyleCombobox1SetStyle
            end
            object SCPenModeCombobox1: TSCPenModeCombobox
              Left = 92
              Top = 64
              Width = 105
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              BackColor = clBlue
              PenColor = 33023
              PopupProps.Font.Charset = DEFAULT_CHARSET
              PopupProps.Font.Color = clWindowText
              PopupProps.Font.Height = -11
              PopupProps.Font.Name = 'MS Sans Serif'
              PopupProps.Font.Style = []
              Style = scesOffice12
              TabOrder = 2
              OnSetMode = SCPenModeCombobox1SetMode
            end
            object SCIntSpinEdit1: TSCIntSpinEdit
              Left = 92
              Top = 112
              Width = 105
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              IntValue = 0
              MaxValue = 20
              Style = scesOffice12
              TabOrder = 3
              OnChange = SCIntSpinEdit1Change
            end
          end
          object SCAdvPanel6: TSCAdvPanel
            Left = 9
            Top = 267
            Width = 210
            Height = 213
            Caption = 'Gradient'
            CaptionSettings.Font.Charset = DEFAULT_CHARSET
            CaptionSettings.Font.Color = clHighlight
            CaptionSettings.Font.Height = -11
            CaptionSettings.Font.Name = 'MS Sans Serif'
            CaptionSettings.Font.Style = [fsBold]
            ExpandedHeight = 213
            ShowClose = False
            TabOrder = 3
            Style = scasXPBar
            object Label11: TLabel
              Left = 8
              Top = 46
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Start Color:'
              Layout = tlCenter
            end
            object Label10: TLabel
              Left = 8
              Top = 70
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'End Color:'
              Layout = tlCenter
            end
            object Label12: TLabel
              Left = 8
              Top = 126
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Style:'
              Layout = tlCenter
            end
            object Label13: TLabel
              Left = 8
              Top = 158
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Rotation:'
              Layout = tlCenter
            end
            object Label14: TLabel
              Left = 8
              Top = 182
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Shift:'
              Layout = tlCenter
            end
            object SCPopupColors3: TSCPopupColors
              Left = 92
              Top = 41
              Width = 105
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              AutoCaption = 'Clear'
              CustomColors = <>
              SelectedColor = clLime
              Style = scesOffice12
              TabOrder = 0
              OnMoreButtonClick = SCPopupColors3MoreButtonClick
              OnSelectedColorChange = SCPopupColors3SelectedColorChange
            end
            object SCPopupColors4: TSCPopupColors
              Left = 92
              Top = 65
              Width = 105
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              AutoCaption = 'Clear'
              CustomColors = <>
              SelectedColor = clWhite
              Style = scesOffice12
              TabOrder = 1
              OnMoreButtonClick = SCPopupColors4MoreButtonClick
              OnSelectedColorChange = SCPopupColors4SelectedColorChange
            end
            object SCCheckbox1: TSCCheckbox
              Left = 92
              Top = 92
              Width = 115
              Height = 17
              Caption = 'Reverse colors'
              Style = sccsXP
              TabOrder = 2
              OnClick = SCCheckbox1Click
            end
            object SCCombobox1: TSCCombobox
              Left = 92
              Top = 120
              Width = 105
              Height = 21
              IsDropDown = True
              Items.Strings = (
                'None'
                'RadialC'
                'RadialT'
                'RadialB'
                'RadialL'
                'RadialR'
                'RadialTL'
                'RadialTR'
                'RadialBL'
                'RadialBR'
                'LinearH'
                'LinearV'
                'ReflectedH'
                'ReflectedV'
                'DiagonalLF'
                'DiagonalLB'
                'DiagonalRF'
                'DiagonalRB'
                'ArrowL'
                'ArrowR'
                'ArrowU'
                'ArrowD'
                'Diamond'
                'Butterfly')
              PopupProps.Font.Charset = DEFAULT_CHARSET
              PopupProps.Font.Color = clWindowText
              PopupProps.Font.Height = -11
              PopupProps.Font.Name = 'Tahoma'
              PopupProps.Font.Style = []
              Style = scesOffice12
              TabOrder = 3
              Text = 'None'
              UseUndo = False
              OnChange = SCCombobox1Change
            end
            object SCIntSpinEdit2: TSCIntSpinEdit
              Left = 92
              Top = 153
              Width = 105
              Height = 21
              CanEdit = False
              CanSelect = False
              Colors.FocusColor = clHighlight
              Colors.FocusTextColor = clHighlightText
              IntValue = 0
              MinValue = -100
              MaxValue = 100
              Style = scesOffice12
              TabOrder = 4
              UseCaret = False
              OnChange = SCIntSpinEdit2Change
            end
            object SCIntSpinEdit3: TSCIntSpinEdit
              Left = 92
              Top = 178
              Width = 105
              Height = 21
              CanEdit = False
              CanSelect = False
              Colors.FocusColor = clHighlight
              Colors.FocusTextColor = clHighlightText
              IntValue = 0
              MinValue = -100
              MaxValue = 100
              Style = scesOffice12
              TabOrder = 5
              UseCaret = False
              OnChange = SCIntSpinEdit3Change
            end
          end
          object SCAdvPanel7: TSCAdvPanel
            Left = 9
            Top = 140
            Width = 210
            Height = 119
            Caption = 'Picture'
            CaptionSettings.Font.Charset = DEFAULT_CHARSET
            CaptionSettings.Font.Color = clHighlight
            CaptionSettings.Font.Height = -11
            CaptionSettings.Font.Name = 'MS Sans Serif'
            CaptionSettings.Font.Style = [fsBold]
            ExpandedHeight = 119
            ShowClose = False
            TabOrder = 4
            Style = scasXPBar
            object SCSpeedButton2: TSCSpeedButton
              Left = 112
              Top = 78
              Width = 89
              Height = 25
              Anchors = [akTop, akRight]
              Caption = 'Clear picture'
              Enabled = False
              Style = scbsOffice12
              OnClick = SCSpeedButton2Click
            end
            object SCSpeedButton1: TSCSpeedButton
              Left = 8
              Top = 78
              Width = 97
              Height = 25
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Load picture...'
              Enabled = False
              Style = scbsOffice12
              OnClick = SCSpeedButton1Click
            end
            object Label15: TLabel
              Left = 8
              Top = 44
              Width = 72
              Height = 17
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'Style:'
              Layout = tlCenter
            end
            object SCCombobox2: TSCCombobox
              Left = 92
              Top = 40
              Width = 105
              Height = 21
              IsDropDown = True
              Items.Strings = (
                'Center'
                'Default'
                'Tiled'
                'Stretch')
              PopupProps.Font.Charset = DEFAULT_CHARSET
              PopupProps.Font.Color = clWindowText
              PopupProps.Font.Height = -11
              PopupProps.Font.Name = 'Tahoma'
              PopupProps.Font.Style = []
              Style = scesOffice12
              TabOrder = 0
              Text = 'None'
              UseUndo = False
              OnChange = SCCombobox2Change
            end
          end
        end
      end
    end
    object SCAdvPanel5: TSCAdvPanel
      Left = 0
      Top = 503
      Width = 244
      Height = 26
      ShowCaptionLine = False
      Bevel = sccbNone
      Caption = 'Layers'
      CaptionSettings.ButtonColors.Color = clBtnShadow
      CaptionSettings.ButtonColors.DisabledColor = clBtnShadow
      CaptionSettings.ButtonColors.DownColor = clBtnShadow
      CaptionSettings.ButtonColors.HotColor = clBtnShadow
      CaptionSettings.ButtonColors.IconColor = clBtnHighlight
      CaptionSettings.ButtonColors.IconDisabledColor = clBtnFace
      CaptionSettings.ButtonColors.IconDownColor = clBtnFace
      CaptionSettings.ButtonColors.IconHotColor = clBtnFace
      CaptionSettings.DefaultProps.Color = clBtnFace
      CaptionSettings.DefaultProps.EndColor = clHighlight
      CaptionSettings.DefaultProps.Gradient = scgNone
      CaptionSettings.DefaultProps.LineColor = clNone
      CaptionSettings.DisabledProps.Color = clBtnFace
      CaptionSettings.DisabledProps.EndColor = clBtnShadow
      CaptionSettings.DisabledProps.Gradient = scgNone
      CaptionSettings.DisabledProps.LineColor = clBtnText
      CaptionSettings.Font.Charset = DEFAULT_CHARSET
      CaptionSettings.Font.Color = clWindowText
      CaptionSettings.Font.Height = -11
      CaptionSettings.Font.Name = 'MS Sans Serif'
      CaptionSettings.Font.Style = []
      CaptionSettings.HotProps.Color = clBtnFace
      CaptionSettings.HotProps.EndColor = 11166794
      CaptionSettings.HotProps.Gradient = scgNone
      CaptionSettings.HotProps.LineColor = clNone
      CaptionSettings.Style = scgcSoftRaised
      Color = clBtnShadow
      Expanded = False
      RectDefaultButtons = True
      Rounded = False
      ShowClose = False
      ShowRoller = False
      TabOrder = 1
      Style = scasOutlookBar2k
      object ToolBar2: TToolBar
        Left = 0
        Top = 54
        Width = 244
        Height = 25
        Caption = 'ToolBar1'
        Color = clBtnFace
        Flat = True
        Images = ImageList2
        ParentColor = False
        TabOrder = 0
        object ToolButton4: TToolButton
          Left = 0
          Top = 0
          Caption = 'ToolButton1'
          ImageIndex = 0
          OnClick = ToolButton4Click
        end
        object ToolButton8: TToolButton
          Left = 23
          Top = 0
          Caption = 'ToolButton2'
          ImageIndex = 1
          OnClick = ToolButton8Click
        end
      end
      object LayerManager: TSCDeLayerManager
        Left = 0
        Top = 79
        Width = 244
        Height = 452
        Align = alClient
        BorderProps.Border = scdcbMetal
        Editor = SCCAD1
        Scrollbars.Style = scssOffice12
        TabOrder = 1
      end
    end
  end
  object ImageList1: TImageList
    Left = 519
    Top = 116
    Bitmap = {
      494C01012A002C00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000B0000000010020000000000000B0
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000010104A0010104A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000010104A0010104A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000010104A006363A5003939840010104A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000010104A006363A5003939840010104A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      84008484C6006363A5006363A50010104A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      84008484C6006363A5006363A50010104A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000039398400B5B5
      EF008484C6008484C60010104A00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000039398400B5B5
      EF008484C6008484C60010104A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDA59400D67B
      4A00EF6B2900EF6B2100EF6B2100EF6B2900D67342006363D600FFFFFF00B5B5
      EF00B5B5EF003939840000000000000000000000000000000000BDA59400D67B
      4A00EF6B2900EF6B2100EF6B2100EF6B2900D67342006363D600FFFFFF00B5B5
      EF00B5B5EF003939840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CE8C7300E75A2900F794
      6B00FFCEB500FFEFEF00FFEFEF00FFCEB500F7946B00DE5A29006363D600FFFF
      FF003939840000000000000000000000000000000000CE8C7300E75A2900F794
      6B00FFCEB500FFEFEF00FFEFEF00FFCEB500F7946B00DE5A29006363D600FFFF
      FF00393984000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CEAD9C00DE4A2100F79C8400FFFF
      F700FFFFF700FFFFF700FFFFF700FFFFF700FFFFF700F79C8400DE4A21006363
      D60000000000000000000000000000000000CEAD9C00DE4A2100F79C8400FFFF
      F700FFFFF700FFFFF700FFFFF700FFFFF700FFFFF700F79C8400DE4A21006363
      D600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CE524200EF735A00FFF7DE00FFF7
      DE00FFF7DE000000000000000000FFF7DE00FFF7DE00FFF7DE00EF735A00CE52
      420000000000000000000000000000000000CE524200EF735A00FFF7DE00FFF7
      DE00FFF7DE00FFF7DE00FFF7DE00FFF7DE00FFF7DE00FFF7DE00EF735A00CE52
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D6211000F7AD8400FFEFC600FFEF
      C600FFEFC6008400840000000000FFEFC600FFEFC600FFEFC600F7AD8400D621
      100000000000000000000000000000000000D6211000F7AD8400FFEFC600FFEF
      C600FFEFC600FFEFC600FFEFC600FFEFC600FFEFC600FFEFC600F7AD8400D621
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CE080000FFD69C00FFEFAD000000
      00000000000084008400000000000000000000000000FFEFAD00FFD69C00CE08
      000000000000000000000000000000000000CE080000FFD69C00FFEFAD000000
      00000000000000000000000000000000000000000000FFEFAD00FFD69C00CE08
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CE000000FFCE8C00FFF7C6008400
      84008400840084008400840084008400840000000000FFF7C600FFCE8C00CE00
      000000000000000000000000000000000000CE000000FFCE8C00FFF7C6008400
      84008400840084008400840084008400840000000000FFF7C600FFCE8C00CE00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CE101000EF9C6B00FFF7D600FFEF
      C600FFE79C008400840000000000FFE79C00FFEFC600FFF7D600EF9C6B00CE10
      100000000000000000000000000000000000CE101000EF9C6B00FFF7D600FFEF
      C600FFE79C00FFE79C00FFE79C00FFE79C00FFEFC600FFF7D600EF9C6B00CE10
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D6424200DE523900FFEFB500FFFF
      E700FFF7D6008400840000000000FFF7D600FFFFE700FFEFB500DE523900D642
      420000000000000000000000000000000000D6424200DE523900FFEFB500FFFF
      E700FFF7D600FFEFC600FFEFC600FFF7D600FFFFE700FFEFB500DE523900D642
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EFBDBD00CE101000E7734A00FFEF
      B500FFF7D600FFFFF700FFFFF700FFF7D600FFEFB500E7734A00CE101000EFBD
      BD0000000000000000000000000000000000EFBDBD00CE101000E7734A00FFEF
      B500FFF7D600FFFFF700FFFFF700FFF7D600FFEFB500E7734A00CE101000EFBD
      BD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E7949400CE101000DE52
      3900EF9C6B00FFCE8C00FFCE8C00EF9C6B00DE523900CE101000E79494000000
      00000000000000000000000000000000000000000000E7949400CE101000DE52
      3900EF9C6B00FFCE8C00FFCE8C00EF9C6B00DE523900CE101000E79494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F7C6C600D64A
      4A00CE101000CE000000CE000000CE101000D64A4A00F7C6C600000000000000
      0000000000000000000000000000000000000000000000000000F7C6C600D64A
      4A00CE101000CE000000CE000000CE101000D64A4A00F7C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000846B5200213142002131420021314200213142002131
      4200213142002131420021314200213142000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000094736300F7E7D600F7DED600E7D6C600E7CEC600D6C6
      B500D6BDB500D6BDB500D6B5AD00213142000000000000000000000000000000
      0000000000008C8C8C007B7B84007B736B007B736B007B7B84008C8C8C000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFA58400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFA58400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000063849400526373004252
      6300314252002131420094736300F7E7D600B5A59400B5A59400B5A59400B5A5
      9400B5A59400B5A59400B5A59400213142000000000000000000000000007B7B
      84006B6B63006B635A006B5A52006B5A52006B5A52006B5A52006B635A006B6B
      63007B7B84000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F78C5200D67B52000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D67B5200F78C5200000000000000000000000000000000000000
      0000000000000000000000000000000000007384940073DEF70052DEFF0042CE
      F70018C6EF0010B5DE00947B6300F7EFE700F7DED600E7D6C600E7CEC600D6C6
      B500D6BDB500D6BDB500D6B5AD0021314200000000000000000073736B007B73
      6B00ADA59C00D6D6CE00F7F7F700F7F7F700F7F7F700F7F7F700D6D6CE00ADA5
      9C007B736B0073736B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F78C5200D67B
      5A00000000000000000000000000000000000000000000000000000000000000
      0000D67B5A00F78C520000000000000000000000000000000000000000000000
      000000000000000000000000000000000000738C94007BDEF70063DEFF004AD6
      F70029CEEF0010BDEF00A5847300F7F7F700C6B5A500C6B5A500C6ADA500B5A5
      9400D6C6B500D6BDB500D6BDB500213142000000000073737300847B7300CECE
      CE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CECECE00847B7B0073737300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C67B5A00F78C
      5200FFA58400000000000000000000000000000000000000000000000000FFA5
      8400F78C5200C67B5A0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000848C94008CE7F70073DEFF005AD6
      F7004AD6F70018C6EF00A58C8400FFFFFF00F7F7F700F7EFE700F7DED600E7D6
      C600735A520063524200524A4200424242008C8C8C00736B6300CECEC600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00CECEC600736B63008C8C8C000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E77B
      4200F7946300000000000000000000000000000000000000000000000000F794
      6300E77B42000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008494A50094E7F70084E7FF006BDE
      F7005AD6F70042CEEF00B5948400FFFFFF00C6B5A500C6B5A500C6ADA500F7E7
      D60084635200D6CEC6006352420000000000736B6B00A59C9400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A59C9400736B6B000000000000000000E77B4200D673
      4200C66B4200B5633100B55A310094523100000000000000000000000000C66B
      4200F78C5200FFA57B0000000000000000000000000000000000FFA57B00F78C
      5200C66B420000000000000000000000000094523100B55A3100B5633100C66B
      4200D6734200E77B42000000000000000000849CA50094E7F70094E7FF007BDE
      F7007BDEF7004AD6F700B59C9400FFFFFF00FFFFFF00FFFFFF00F7F7F700F7EF
      E700846B52008463520000000000000000005A525200CECECE00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CECECE005A5252000000000000000000E7845200E773
      3100F7845200F79C7300E794630000000000000000000000000000000000C66B
      4200E7845A00F794630000000000000000000000000000000000F7946300E784
      5A00C66B420000000000000000000000000000000000E7946300F79C7300F784
      5200E7733100E78452000000000000000000849CA500A5EFF7009CEFFF0084DE
      F70084DEF7005AD6F700B5A59400B59C9400B5948400A58C8400A5847300947B
      6300947363000000000000000000000000004A423900E7E7DE00F7F7EF00F7F7
      EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7
      EF00F7F7EF00F7F7EF00E7E7DE004A4239000000000000000000E7946300F784
      5200F7946300F7A57300AD7B6B0000000000000000000000000000000000C66B
      4200E77B4A00FFA57B0000000000000000000000000000000000FFA57B00E77B
      4A00C66B420000000000000000000000000000000000AD7B6B00F7A57300F794
      6300F7845200E7946300000000000000000094A5A500B5F7FF00A5EFFF0094E7
      F70094E7F70073DEF7005AD6F7004AD6F70042CEEF0018C6EF0010BDEF0010B5
      DE003139420000000000000000000000000052524A00BDBDAD00EFEFE700EFEF
      E700EFEFE700EFEFE700EFEFE700EFEFE700EFEFE700EFEFE700EFEFE700EFEF
      E700EFEFE700EFEFE700BDBDAD0052524A000000000000000000E79C7300F79C
      7300F7A57300EF946B00B5634200B5634200000000000000000000000000A563
      3900E77B4A00FFBD9C0000000000000000000000000000000000FFBD9C00E77B
      4A00A5633900000000000000000000000000B5634200B5634200EF946B00F7A5
      7300F79C7300E79C7300000000000000000094A5B500B5F7FF00A5F7FF006384
      94006373840052738400526B7300526B730052637300425A73002173940010B5
      DE00424A630000000000000000000000000073736B0084847300E7E7D600E7E7
      D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7
      D600E7E7D600E7E7D6008484730073736B000000000000000000F7AD84000000
      0000FFBD9C00F7B59400E7946300C6734200C66B4200BD845A00B57B5200A563
      4200F78C5200EFC6AD0000000000000000000000000000000000EFC6AD00F78C
      5200A5634200B57B5200BD845A00C66B4200C6734200E7946300F7B59400FFBD
      9C0000000000F7AD8400000000000000000094ADB500B5F7FF00B5F7FF00638C
      940094CED60094EFF70084DEE70063CEE700529CB500425A63002184A50018C6
      EF00525A7300000000000000000000000000000000005A524A00ADA59400E7DE
      CE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DE
      CE00E7DECE00ADA594005A524A00000000000000000000000000F7AD94000000
      000000000000FFBD9C00F7B59400EF946B00D6734200B56B4200B56B3900E77B
      4A00EFBDA500000000000000000000000000000000000000000000000000EFBD
      A500E77B4A00B56B3900B56B4200D6734200EF946B00F7B59400FFBD9C000000
      000000000000F7AD9400000000000000000094ADB500B5F7F700B5F7FF00A5F7
      F700739CA500A5F7F7006B84840084CED600527384003163840042CEEF004AD6
      F7005263730000000000000000000000000000000000848484005A524A00A59C
      8C00EFEFDE00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00EFEF
      DE00A59C8C005A524A0084848400000000000000000000000000000000000000
      00000000000000000000FFBD9C00F7B59400EFB59C00EFB58C00EFB58C00EFBD
      AD00000000000000000000000000000000000000000000000000000000000000
      0000EFBDAD00EFB58C00EFB58C00EFB59C00F7B59400FFBD9C00000000000000
      0000000000000000000000000000000000000000000094ADB50094ADB50094AD
      B5006394A500A5EFF700A5EFF70094DEE700426B7300738C9400848C9400738C
      940000000000000000000000000000000000000000000000000084848400524A
      4A007B736300A59C8C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00A59C8C007B73
      6300524A4A008484840000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFCEBD00FFBDA500F7CEB5000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F7CEB500FFBDA500FFCEBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084B5C60084B5C60084A5B5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000073737300524A4A0039312900393129003931290039312900524A4A007373
      7300000000000000000000000000000000000000000000000000000000000000
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
      00000000000000000000C6734200B55A3100B55A3100A5522100A5522100944A
      2100944A210094422100AD296B00000000000000000000000000DE429C00735A
      5200B55A5200B55A5200B55A5200B55A5200B55A5200B55A5200B55A5200B55A
      5200735A52007339420073393100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009C6B42006342390000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C67B5200FFEFE700FFDED600F7CEC600F7C6B500F7B5
      A500F7A59400F7A59400AD3152000000000000000000D66B7300F7949400735A
      5200C6ADAD00D6C6BD00E7DED600FFEFEF00FFEFEF00FFE7DE00F7DED600F7DE
      D600735A5200C65A52007339420000000000000000005A5A6300313131004A4A
      4A00000000000000000000000000000000000000000000000000000000004A4A
      4A00313131005A5A63000000000000000000000000000000000000000000D68C
      6B00B563420094523100634A3100000000000000000063423900634229000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C67B5200FFFFF700FFEFE700FFDED600F7CEC600F7C6
      B500F7B5A500F7A59400AD3152000000000000000000D67B7300FFA5A500735A
      5200BDA59C00C6ADAD00D6C6BD00E7DED600FFEFEF00FFEFEF00FFE7DE00F7DE
      D600735A5200C65A520084424200000000000000000000000000313131003131
      3100000000000000000000000000000000000000000000000000000000003131
      310031313100000000000000000000000000000000000000000000000000E784
      520000000000000000008452310000000000C68C6B00B5634200634231003929
      2900000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D68C6300FFFFFF00E7946300D6845200D6845200D684
      5200D6845200F7B5A500AD3152000000000000000000D67B8400FFADB500735A
      5200A5847B00BDA59C00C6ADAD00D6C6BD00E7DED600FFEFEF00FFEFEF00FFE7
      DE00735A5200C65A520084424200000000000000000000000000393939003131
      31005A5A630000000000000000000000000000000000000000005A5A63003131
      310039393900000000000000000000000000000000000000000000000000E78C
      520000000000000000009452310000000000D67B520000000000000000007342
      310000000000000000000000000000000000B5A59400634A3100634A3100634A
      3100634A3100634A3100D6947300FFFFFF00FFFFFF00FFF7F700F7E7D600F7D6
      C600F7CEBD00F7C6B500AD394A000000000000000000D67B8400FFB5B500735A
      5200735A5200735A5200735A5200735A5200735A5200735A5200735A5200735A
      5200735A5200C65A5200944A42000000000000000000000000006B7373003131
      3100393939006B6B6B007373730073737B00737373006B6B6B00393939003131
      31006B737300000000000000000000000000000000000000000000000000DEA5
      7B00E78C5200B56B420094523100AD7B6B00D67B520000000000000000009452
      310000000000000000000000000000000000B5A59400FFF7F700F7E7D600E7DE
      D600E7D6C600E7CEC600E7A58400FFFFFF00F7AD8400E79C7300E7946300D684
      5200D6845200F7CEC600B53939000000000000000000E7848400FFB5B500FFB5
      B500FFA5A500E7848400E77B8400D6737300D6737300D66B7300C6636300C663
      6300C65A5200C65A5200944A4200000000000000000000000000000000003131
      3100313131003131310031313100313131003131310031313100313131003131
      3100000000000000000000000000000000000000000000000000000000000000
      0000EF946B00E7845A00A5634A009C634200C6734A00D67B5200C66B4200BD9C
      8C0000000000000000000000000000000000B5A59400FFF7F700F7E7D600E7DE
      D600E7D6C600E7CEC600E7AD9400FFFFFF00FFFFFF00FFFFFF00FFFFF700F7EF
      E700FFDED600FFDED600AD4A31000000000000000000E78C9400FFB5B500FFB5
      B500FFB5B500FFA5A500E77B8400D6737300D6737300D66B7300C6636300C663
      6300C65A5200C65A5200944A4200000000000000000000000000000000005252
      5200313131004242420000000000000000000000000042424200313131005252
      5200000000000000000000000000000000000000000000000000000000000000
      000000000000EFA58C00E7845200D69C7B00D67B5200D67B5A00D6A58C000000
      000000000000000000000000000000000000B5A59400FFFFF700E7B58400E7A5
      7300E7A57300D69C7300E7BDA500FFFFFF00FFB59400FFB59400F7DED600E794
      6300B55A3100B55A3100A54A21000000000000000000E7848400FFB5B500FFBD
      B500D6636300D6636300D6636300C6524200B5523100B54A3100A5422100A539
      1000C6636300C6636300944A5200000000000000000000000000000000000000
      0000313131003131310000000000000000000000000031313100313131000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084736B0073635200846B520000000000000000000000
      000000000000000000000000000000000000C6AD9400FFFFFF00FFFFF700F7F7
      F700F7EFE700F7E7D600F7C6A500FFFFFF00FFFFFF00FFFFFF00FFFFFF00F79C
      7300F7CEB500B55A3100000000000000000000000000E7848400FFB5B500D66B
      6300FFFFFF00FFFFFF00F7DED600F7DED600E7D6C600E7D6C600E7CEC600E7CE
      C600A5391000D66B7300A5525200000000000000000000000000000000000000
      0000393939003131310063636300000000006363630031313100393939000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009484730084736300736352007B7B7300000000000000
      000000000000000000000000000000000000C6ADA500FFFFFF00FFCEA500F7BD
      9400E7B58400E7A57300F7CEB500FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7AD
      8400C66B310000000000000000000000000000000000E7848400FFB5B500D673
      7300FFFFFF00FFFFFF00FFFFFF00F7EFE700F7DED600E7D6C600E7CEC600E7D6
      C600A5422100E77B8400A5525200000000000000000000000000000000000000
      000063636B00313131003939390000000000393939003131310063636B000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6B5AD00A59C9400B5ADA5009484730073635200000000000000
      000000000000000000000000000000000000C6B5A500FFFFFF00FFFFFF00FFFF
      FF00FFF7F700F7EFE700F7CEB500F7C6B500F7C6B500F7BDA500F7B59400F7B5
      94000000000000000000000000000000000000000000E7848400FFB5B500E77B
      7300FFFFFF00FFFFFF00FFFFFF00F7F7F700F7EFE700F7DED600E7D6C600E7D6
      C600B54A3100E77B8400A5525200000000000000000000000000000000000000
      0000000000003131310031313100525A5A003131310031313100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BDA5A500A594940000000000BDADA5009C8C7300AD9C8C000000
      000000000000000000000000000000000000D6BDB500FFFFFF00FFDEC600FFD6
      B500F7E7D600B5A59400AD9C8C00A5947B009C84730000000000000000000000
      00000000000000000000000000000000000000000000E7848400FFB5B500FF94
      9400FFFFFF00FFFFFF00FFFFFF00FFFFF700F7F7F700F7EFE700F7DED600E7D6
      C600C65A5200E77B8400B55A6300000000000000000000000000000000000000
      0000000000004A4A4A003131310031313100313131004A4A4A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B59C9400BDADAD000000000000000000AD9C8C00AD9C94000000
      000000000000000000000000000000000000D6C6B500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6AD9400D6CEC600947363000000000000000000000000000000
      00000000000000000000000000000000000000000000F7949400FFB5B500FF94
      9400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700F7DE
      D600C65A5200E7848400B55A6300000000000000000000000000000000000000
      0000000000000000000031313100313131003131310000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A59C9400000000000000000000000000C6B5B500B5A59C000000
      000000000000000000000000000000000000E7C6B500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6B5A500A5846300000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F7949400F7949400E79C
      A500E79CA500E79CA500E79CA500E7949400E7949400E78C9400E7848400D67B
      8400D67B7300D67B7300D6737300000000000000000000000000000000000000
      0000000000000000000039393900313131003939390000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B5B5AD0000000000000000000000000000000000ADA5A5000000
      000000000000000000000000000000000000E7C6B500E7C6B500D6C6B500D6C6
      B500D6BDB500D6B5A50000000000000000000000000000000000000000000000
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
      000000000000000000000000FF000000FF000084000000840000008400000084
      000000840000008400000000FF000000FF000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000084000000840000008400000084000000840000008400000084
      0000008400000000FF000000FF00000000000000000000000000000000000000
      000000000000000000000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF00000000FF000000FF000000000000000000C6A59C00FFFF
      F700FFF7E700FFEFE700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6
      AD00EFCE9400F7D69C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000084000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF00000000FF000000FF00000000000000000000000000000000000000
      000000000000000000000084000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000FF0000008400000000000000000000C6A59C00FFFF
      FF00FFFFF700FFF7E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6
      B500EFCE9C00EFCE9C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000840000000000000000000000000000000000000000
      000000000000000000000084000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000FF0000008400000000000000000000C6ADA500FFFF
      FF00FFFFFF00FFFFF700FFEFE700F7E7D600F7E7D600F7E7CE00F7DEC600F7DE
      BD00EFCE9C00EFCE9C00B58484000000000029ADD60029ADD60031B5DE0021AD
      D60021ADD60021ADD60018A5CE001894BD001894BD001894BD00188CAD00107B
      9C00107394000000000000000000000000000000000000000000000000000000
      0000000000000084000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000840000000000000000000000000000000000000000
      000000000000000000000084000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000FF0000008400000000000000000000C6ADA500FFFF
      FF00FFFFFF00FFFFFF00FFF7EF00FFEFE700F7E7D600F7E7CE00F7E7C600F7DE
      C600F7D6AD00EFCE9400B58484000000000029ADD600219CBD0084E7FF0084E7
      FF0084E7FF0084E7FF0073DEFF006BD6FF0052CEFF0052CEFF0042C6F70042C6
      F700107394000000000000000000000000000000000000000000000000000000
      0000000000000084000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000840000000000000000000000000000000000000000
      000000000000000000000084000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000FF0000008400000000000000000000CEB5AD00FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7E7
      C600F7D6B500EFCE9C00B58484000000000029ADD60084C6D600219CBD0084E7
      FF0084E7FF0084E7FF0084E7FF0084E7FF0063D6FF0052CEFF0052CEFF0042C6
      F70042C6F7001073940000000000000000000000000000000000000000000000
      0000000000000084000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000840000000000000000000000000000000000000000
      000000000000000000000084000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000FF0000008400000000000000000000D6B5AD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7EF00FFEFE700F7E7D600F7E7
      CE00F7DEBD00F7D6A500B58484000000000029ADD60084D6F700219CBD0084E7
      FF0084E7FF0084E7FF0084E7FF0084E7FF0073DEFF0063D6FF0063D6FF0052CE
      FF0042C6F7001073940000000000000000000000000084848400848484008484
      8400848484000084000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000840000000000000000FF000000FF00848484008484
      840084848400848484000000FF000000FF0000FF000000FF000000FF000000FF
      000000FF000000FF00000000FF000000FF000000000000000000D6BDB500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7F700FFEFE700FFEF
      E700F7E7D600F7D6AD00B58484000000000029ADD60084DEF70084CEE70021A5
      CE0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0073D6FF0063DEFF0063D6
      FF0063D6FF0042C6F7001073940000000000000000008484840084FFFF0084FF
      FF0084FFFF000084000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000840000000000000000FF000000FF0084FFFF0084FF
      FF0084FFFF0084FFFF000000FF000000FF000084000000840000008400000084
      000000840000008400000000FF000000FF000000000000000000D6BDB500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7F700F7F7
      EF00EFEFDE00E7DEC600B58484000000000029ADD60094E7F70094E7FF0021A5
      CE0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0073DEFF0063DEFF0063DE
      FF0063D6FF0063D6FF001073940000000000000000008484840084FFFF0084FF
      FF0084FFFF000084000000840000008400000084000000840000008400000084
      0000008400000084000000840000000000008484840084FFFF0084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084848400000000000000
      0000000000000000000000000000000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F7F7EF00E7DEC600B58484000000000029ADD60094E7F700A5EFFF0084CE
      E70029ADD60084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7
      FF0073DEFF0073DEFF0052CEFF0010739400000000008484840084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084848400000000000000
      0000000000000000000000000000000000008484840084FFFF0084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084848400000000000000
      0000000000000000000000000000000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700F7F7EF00EFEF
      DE00E7DEC600E7DEC600B58484000000000029ADD600A5EFF700A5EFFF00A5EF
      FF0029ADD60029ADD60029ADD60021A5CE00219CBD00218CB500218CB5002184
      A5002184A50018849C0018849C0010739400000000008484840084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084848400000000000000
      0000000000000000000000000000000000008484840084FFFF0084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084848400000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7E700C6A5
      9400B5948C00B58C8400B58484000000000029ADD600A5EFF700A5F7FF00A5EF
      FF00A5EFFF0084DEFF0063DEFF0063DEFF008CEFFF0094EFFF0073DEF70063DE
      FF0010849C00000000000000000000000000000000008484840084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084848400000000000000
      0000000000000000000000000000000000008484840084FFFF0084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084848400000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7EF00EFE7D600BD8C
      7300F7E7C600F7E7C600C6846B000000000029ADD600A5F7F700B5F7F700A5F7
      FF00A5EFFF00A5EFFF0073DEFF0018ADD60018A5C60018A5C60018A5C600109C
      B5001094AD00000000000000000000000000000000000000FF000000FF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084848400000000000000
      0000000000000000FF000000FF00000000008484840084FFFF0084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF0084FFFF0084848400000000000000
      0000000000000000000000000000000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7EFE700EFE7CE00C694
      7B00F7E7C600CE947300000000000000000021ADD600A5D6E700B5F7F700B5F7
      F700A5F7FF00A5EFFF00109CB500000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF008484
      8400848484008484840084848400848484008484840084848400000000000000
      0000000000000000FF000000FF00000000000000FF000000FF0084FFFF0084FF
      FF0084FFFF0084FFFF0084FFFF0084FFFF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00EFE7D600E7DEC600C694
      7B00CE9C84000000000000000000000000000000000031B5DE0029ADD60018A5
      C60018A5C600109CB50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF00848484008484
      8400848484008484840084848400848484000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008C9494007B8484007B8484007B84
      84008C9494000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009CA5A5006B4242009C9C9C009C9C9C009C9C
      9C006B4242009CA5A50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008C8C8C006B3131009C8484009C8484009C84
      84006B3131008C8C8C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      00000000000000000000000000009C6363006B3131006B4242006B4242006B42
      42006B3131009C63630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      00000000000000000000000000006B3131006B3131006B3131006B3131006B31
      31006B3131006B31310000000000000000000000000000000000000000000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BDBDBD006B6B6B00BDBD
      BD00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000848484008484840000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      00000000000000000000000000007B84840000000000CECECE006B6B6B00CECE
      CE00000000007B84840000000000000000000000000000000000000000000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000084848400848484000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CECECE00E7E7E700CECECE00E7E7
      E700CECECE000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000848484008484840000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00BDBDBD00FFFF
      FF00FFFFFF000000000000000000000000000000000084848400848484008484
      8400848484008484840000000000FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      000000000000000000000000FF0000000000E7E7E700FFFFFF0084848400FFFF
      FF00E7E7E7000000000000000000000000000000000084848400848484008484
      840084848400848484000000000000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00000000000000FF000000FF008C94940000000000FFFFFF006B6B6B00FFFF
      FF00000000008C94940000000000000000000000000084848400848484008484
      8400848484008484840000000000FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF00000000000000000000000000BDBDBD006B6B6B00BDBD
      BD00000000000000000000000000000000000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000000000000000949C9C00000000006B6B6B000000
      0000949C9C000000000000000000000000000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00000000000000FF000000FF00000000000000000063636300000000006363
      6300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      000000000000000000000000FF000000FF00000000008C949C00000000008C94
      9C00000000000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000007B73
      6B00635A52005A524A005A524A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000635A5200A5AD
      AD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006B6363009CA5A5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      840084848400848484000000000000000000946B6300946B6300946B6300946B
      6300946B6300946B6300946B6300946B6300946B6300946B6300946B6300946B
      6300946B6300946B6300946B6300000000000000000000000000000000000000
      00005A5A5A001818180018181800181818001818180018181800181818001818
      180018181800000000000000000000000000000000005A524A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      840084848400848484000000000000000000946B63005ABD5A005ABD5A005ABD
      5A005ABD5A005ABD5A005ABD5A005ABD5A005ABD5A0063A55200DED6A500DED6
      A500DED6A500DED6A500946B6300000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE0018181800000000000000000000000000000000006B635A00949494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004A42310039211000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840000000000000000008C6B5A0052B5520052B5520052B5
      520052B5520052B5520052B5520052B55200218C2100BDCE9400BDCE9400BDCE
      9400BDCE940094BD73008C6B5A00000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE0018181800000000000000000000000000000000009C9C9C0063635A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000031211000524A3900000000000000000000000000000000000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000848484008484840000000000000000008C635A0042A5420042A5420042A5
      420042A5420042A5420042A5420010841000ADC68400ADC68400ADC68400ADC6
      840084B56300FFDEBD008C635A00000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE001818180000000000000000000000000000000000000000006B635A008C8C
      8C00000000000000000000000000000000000000000000000000312110000000
      0000312110003121100000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      000084848400848484000000000000000000845A5200107B1000299429002994
      29002994290029942900107B1000FFDEBD006BAD5200A5BD7B00A5BD7B006BAD
      5200FFDEBD00FFDEBD00845A5200000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE00181818000000000000000000000000000000000000000000000000004A39
      3100A5ADAD000000000000000000000000000000000000000000291808002918
      1000291810003931290000000000000000000000000000000000000000000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000007B5A5200FFDEBD00107B1000188C
      1800188C1800107B1000FFDEBD00FFDEBD00FFDEBD0063A5520063A55200FFDE
      BD00FFDEBD00FFDEBD007B5A5200000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE00181818000000000000000000000000000000000000000000000000000000
      000042393100A5ADAD0000000000000000000000000000000000211808002118
      0800211808002118080021180800000000000000000000000000000000000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000007B524A00FFD6AD00FFD6AD00107B
      1000107B1000FFD6AD00FFD6AD00FFD6AD00FFD6AD00FFD6AD00FFD6AD00FFD6
      AD00FFD6AD00FFD6AD007B524A00000000000000000000000000000000000000
      00005A5A5A00FFCECE009C633100FFCECE009C6331009C9C63009C6331009C9C
      6300181818000000000000000000000000000000000000000000000000000000
      00000000000042393100A5ADAD00000000000000000000000000211008002110
      0800211008002110080000000000000000000000000084848400848484000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      000000000000000000000000000000000000734A4200FFCE9400FFCE9400FFCE
      9400FFCE9400FFCE9400FFCE9400FFCE9400FFCE9400FFCE9400FFCE9400FFCE
      9400FFCE9400FFCE9400734A4200000000000000000000000000000000005A5A
      5A00633100006331000063310000633100006331000063310000633100006331
      0000633100001818180000000000000000000000000000000000000000000000
      0000000000000000000039312900A5ADAD000000000000000000181008001810
      0800181008000000000000000000000000000000000084848400848484000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      000000000000000000000000000000000000734A4200FFBD7B00FFBD7B00FFBD
      7B00FFBD7B00FFBD7300FFBD7B00FFBD7B00FFBD7B00FFBD7300FFBD7300FFBD
      7300FFBD7B00FFBD7B00734A4200000000000000000000000000000000005A5A
      5A00FFCECE00FFEFCE00F7F7F700FFEFCE00FFCECE00FFEFCE00FFCECE00FFCE
      CE00FFCECE001818180000000000000000000000000000000000000000000000
      0000000000000000000000000000393129008C94940000000000100808001008
      0800000000000000000000000000000000000000000084848400848484000000
      000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000006B423900FFAD5A00FFAD5A00FFAD
      5A00FFAD5A00FFAD5A00FFAD5A00FFAD5A00FFAD5A00FFAD5A00FFAD5A00FFAD
      5A00FFAD5A00FFAD5A006B423900000000000000000000000000000000000000
      00005A5A5A005A5A5A005A5A5A009C633100FFCECE009C6331005A5A5A005A5A
      5A008484840000000000000000000000000000000000000000004A4A42004A42
      4200848C8C00000000000000000000000000393129007B848400100800000000
      0000000000000000000000000000000000000000000084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006B4231006B423100634231006B42
      31006B4231006B4231006342310063423900634239006B4239006B4239006342
      39006B423900634239006B423100000000000000000000000000000000000000
      0000000000000000000000000000181818001818180018181800000000000000
      00000000000000000000000000000000000000000000524A4200000000000000
      0000312921008484840000000000000000000000000039312900000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004A4A4200393931007B8484007B84840039312900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000312921004A4A420000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A56B630084736B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000010104A0010104A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AD946300EFEFDE00A56B6300A56B
      630084736B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000000000000000000000000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000010104A006363A5003939840010104A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A59C8400AD946300EFEFDE00CEBD
      9C00A56B6300A56B630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000000000000000000000000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      84008484C6006363A5006363A50010104A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AD946300D6CEAD00F7F7
      E700D6C6AD00C6AD9400A56B6300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000039398400B5B5
      EF008484C6008484C60010104A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A59C8C00AD946300FFFF
      FF00DEDEBD00D6C6AD00C6AD9400A56B63000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDA59400D67B
      4A00EF6B2900EF6B2100EF6B2100EF6B2900D67342006363D600FFFFFF00B5B5
      EF00B5B5EF00393984000000000000000000000000000000000094ADAD00639C
      9C0094ADAD0063CEFF0063CEFF0063CEFF0063CEFF0063CEFF0063CEFF0063CE
      FF0063CEFF0063CEFF0000000000000000000000000000000000AD946300DEDE
      BD00FFFFFF00DEDEBD00D6C6AD00C6AD9400A56B630000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CE8C7300E75A2900F794
      6B00FFCEB500FFEFEF00FFEFEF00FFCEB500F7946B00DE5A29006363D600FFFF
      FF00393984000000000000000000000000000000000000000000000000000000
      000094ADAD009CCEFF009CCEFF009CCEFF009CCEFF009CCEFF009CCEFF009CCE
      FF009CCEFF009CCEFF000000000000000000000000000000000000000000AD94
      6300DEDEBD00FFFFFF00DEDEBD00D6C6AD00C6AD9400A56B6300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CEAD9C00DE4A2100F79C8400FFFF
      F700FFFFF700FFFFF700FFFFF700FFFFF700FFFFF700F79C8400DE4A21006363
      D600000000000000000000000000000000000000000000000000FF9C9C000000
      000094ADAD0094ADAD0094ADAD009CCEFF009CCEFF009CCEFF009CCEFF009CCE
      FF009CCEFF009CCEFF0000000000000000000000000000000000000000000000
      0000AD946300DEDEBD00FFFFFF00DEDEBD00D6C6AD00C6AD9400A56B63000000
      000010104A000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CE524200EF735A00FFF7DE00FFF7
      DE00FFF7DE00FFF7DE00FFF7DE00FFF7DE00FFF7DE00FFF7DE00EF735A00CE52
      4200000000000000000000000000000000000000000000000000FF9C9C000000
      0000000000000000000094ADAD009CCEFF009CCEFF009CCEFF009CCEFF009CCE
      FF009CCEFF009CCEFF0000000000000000000000000000000000000000000000
      000000000000AD946300DEDEBD00FFFFFF00DEDEBD00D6C6AD00C6AD94001010
      4A003939840010104A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D6211000F7AD8400FFEFC600FFEF
      C600FFEFC600FFEFC600FFEFC600FFEFC600FFEFC600FFEFC600F7AD8400D621
      100000000000000000000000000000000000FF9C9C00FF9C9C00FF9C9C00FF9C
      9C00FF9C9C0000000000C6D6EF00C6D6EF00C6D6EF00C6D6EF00C6D6EF00C6D6
      EF00C6D6EF00C6D6EF0000000000000000000000000000000000000000000000
      00000000000000000000AD946300DEDEBD00FFFFFF00DEDEBD0010104A003939
      84003939840010104A000000000000000000000000000000FF000000FF000000
      0000000000000000FF000000FF000000000000000000000000000000FF000000
      FF0000000000000000000000FF000000FF00CE080000FFD69C00FFEFAD00FFEF
      AD00FFEFAD00FFEFAD00FFEFAD00FFEFAD00FFEFAD00FFEFAD00FFD69C00CE08
      0000000000000000000000000000000000000000000000000000FF9C9C000000
      000094ADAD00C6D6EF00C6D6EF00C6D6EF00C6D6EF00C6D6EF00C6D6EF00C6D6
      EF00C6D6EF00C6D6EF0000000000000000000000000000000000000000000000
      0000000000000000000000000000AD946300DEDEBD00393984008484C6006363
      A50010104A0010104A000000000000000000000000000000FF000000FF000000
      0000000000000000FF000000FF000000000000000000000000000000FF000000
      FF0000000000000000000000FF000000FF00CE000000FFCE8C00FFF7C600FFE7
      A500FFE79C00FFE79C00FFE79C00FFE79C00FFE7A500FFF7C600FFCE8C00CE00
      0000000000000000000000000000000000000000000000000000FF9C9C000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000039398400CECEF7009C9CDE001010
      4A006363A5003939840010104A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CE101000EF9C6B00FFF7D600FFEF
      C600FFE79C00FFE79C00FFE79C00FFE79C00FFEFC600FFF7D600EF9C6B00CE10
      1000000000000000000000000000000000000000000000000000316363003163
      9C0031639C0031639C0031639C0031639C00319C9C0031636300316363003163
      6300316363003163630000000000000000000000000000000000000000000000
      00000000000000000000000000006363D600FFFFFF009494E70039398400B5B5
      EF008484C6006363A5003939840010104A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D6424200DE523900FFEFB500FFFF
      E700FFF7D600FFEFC600FFEFC600FFF7D600FFFFE700FFEFB500DE523900D642
      4200000000000000000000000000000000000000000000000000000000003163
      6300C6D6EF00D6E7E700D6E7E700D6E7E7003163630084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006363D6006363D6006363D600FFFF
      FF00B5B5EF008484C6006363A50010104A00000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF00EFBDBD00CE101000E7734A00FFEF
      B500FFF7D600FFFFF700FFFFF700FFF7D600FFEFB500E7734A00CE101000EFBD
      BD00000000000000000000000000000000000000000000000000000000000000
      0000316363003163630031636300316363008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006363
      D600FFFFFF00B5B5EF008484C60039398400000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF0000000000E7949400CE101000DE52
      3900EF9C6B00FFCE8C00FFCE8C00EF9C6B00DE523900CE101000E79494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006363D6006363D6007373CE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F7C6C600D64A
      4A00CE101000CE000000CE000000CE101000D64A4A00F7C6C600000000000000
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
      000000000000000000000000000000000000000000006B5A52006B5A52007B73
      6B00000000006B5A52006B5A52007B736B00000000006B5A52006B5A52007B73
      6B00000000006B5A52006B5A5200000000000000000000000000000000007B7B
      7B00635A5200635A5200635A5200635A4A00635A4A00635A4A00635A5200635A
      4A007B7B7B000000000000000000000000000000000000000000635A5200635A
      5200635A5200635A5200635A5200635A5200635A5200635A5200635A5200635A
      5200635A5200635A52000000000000000000000000006363B5005252B5005252
      B5005252B5005252B5005252B5005252B5000000000000000000000000000000
      00000000000000000000000000000000000000000000635A5200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000635A5200000000000000000000000000000000006352
      4A00CECECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CECE
      CE0063524A000000000000000000000000000000000000000000635A4A00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00635A4A0000000000000000004A4AB500C6C6EF00ADADEF00ADAD
      EF00ADADEF00ADADEF00ADADEF008C8CC6004A4AB50000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000736B6300000000000000000000000000737373005A52
      4A00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF005A524A0073737300000000000000000000000000000000005A524A00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF005A524A0000000000000000005A5AAD00C6C6EF00BDBDF700BDBD
      F700BDBDF700BDBDF700BDBDF7008484BD009C9CCE004A4AAD00000000000000
      000000000000000000000000000000000000000000006B635A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000524A4200CECE
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CECEC600524A4200000000000000000000000000000000005A524200FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF005A5242000000000000000000000000004242A500D6DEF700C6C6
      F700C6C6F700C6C6F700C6C6F700ADADE7008484BD00BDBDDE004242A5000000
      000000000000000000000000000000000000000000005A4A4200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005A4A4200000000000000000073737300524A3900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00524A390073737300000000000000000000000000524A4200FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00524A42000000000000000000000000005252A500B5B5DE00DEDE
      FF00DEDEFF00DEDEFF00DEDEFF00DEDEEF008484BD009C9CCE00BDBDDE004242
      9C000000000000000000000000000000000000000000524A4200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000524A420000000000000000004A423900CEC6C600FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00CEC6C6004A42390000000000000000000000000052423900F7F7
      EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7
      EF00F7F7EF00524239000000000000000000000000000000000039429400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BDBDDE008484BD009C9CCE00BDBD
      DE00424294000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000635A5200000000000000000042393100FFFFF700FFFF
      F700FFFFF700FFFFF700FFFFF700FFFFF700FFFFF700FFFFF700FFFFF700FFFF
      F700FFFFF700FFFFF700423931000000000000000000000000004A423900EFEF
      E700EFEFE700EFEFE700EFEFE700EFEFE700EFEFE700EFEFE700EFEFE700EFEF
      E700EFEFE7004A423900000000000000000000000000000000004A4A9C00D6D6
      F700ADADEF00ADADEF00ADADEF00ADADEF00ADADEF007B7BB5008C8CC6009C9C
      CE00B5B5D6004A4A9C000000000000000000000000005A524A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000042393100BDBDB500F7F7
      EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7
      EF00F7F7EF00BDBDB5004239310000000000000000000000000042393100E7E7
      D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7
      D600E7E7D6004239310000000000000000000000000000000000000000004239
      8C00DEDEFF00BDBDF700BDBDF700BDBDF700BDBDF700BDBDF7007B7BB5008C8C
      C600ADADD6005252940000000000000000000000000042393100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004239310000000000000000008C8C8C0039312900EFE7
      DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7
      DE00EFE7DE00393129008C8C8C0000000000000000000000000042393100E7DE
      CE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DE
      CE00E7DECE004239310000000000000000000000000000000000000000000000
      000042398C00E7E7FF00C6C6F700C6C6F700C6C6F700C6C6F700C6C6F7007B7B
      BD009C9CCE00B5B5D60042429400000000000000000039312900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003931290000000000000000000000000031292100ADAD
      9C00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DE
      CE00ADAD9C00312929000000000000000000000000000000000039312900DED6
      BD00DED6BD00DED6BD00DED6BD00DED6BD00DED6BD00DED6BD00DED6BD00DED6
      BD00DED6BD003931290000000000000000000000000000000000000000000000
      00000000000042398C00E7E7FF00C6C6F700C6C6F700C6C6F700C6C6F700C6C6
      F7008484BD009C9CCE0052529400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000524A42000000000000000000000000008C8C8C003129
      2100DED6C600DED6C600DED6C600DED6C600DED6C600DED6C600DED6C600DED6
      C600312921008C8C8C000000000000000000000000000000000031292100FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF003129210000000000000000000000000000000000000000000000
      0000000000000000000042398C00EFEFFF00DEDEFF00DEDEFF00DEDEFF00DEDE
      FF00C6C6F7008C8CC6008484BD0042428C00000000004A423900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003129
      2100CECEC600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CECE
      C600312921000000000000000000000000000000000000000000312921003129
      2100312921003129210031292100312921003129210031292100312921003129
      2100312921003129210000000000000000000000000000000000000000000000
      000000000000000000000000000042398C00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00E7E7F7009494C60042398C000000000031292100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000031292100000000000000000000000000000000009494
      9400312921003129210031292100312921003129210031292100312921003129
      2100949494000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004242840042398C0042398C004239
      8C0042398C0042398C0042428400000000000000000031292100312921000000
      00004A4242003129210031292100000000004A42420031292100312921000000
      00004A4242003129210031292100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000525A5A000000
      0000000000000000000000000000000000006363630063636300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008C9494007B8484007B8484007B8484008C949400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B73
      6B00635A52005A524A005A524A00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000636B6B00212121000000
      00000000000000000000000000005A5A5A0084848400848484005A5A5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009CA5A5006B4242009C9C9C009C9C9C009C9C9C006B4242009CA5A5000000
      000000000000000000000000000000000000000000000021210000212100216B
      84004A7384000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000635A5200A5AD
      AD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018212100000000000000
      000000000000000000005A5A5A00848484009C9C9C00C6C6C600A5A5A5005252
      5200000000000000000000000000000000000000000000000000000000000000
      00008C8C8C006B3131009C8484009C8484009C8484006B3131008C8C8C000000
      00000000000000000000000000000000000000000000002121004A9CC6005AA5
      CE0029739400216B8400526B8400000000000000000000000000000000000000
      000000000000000000000000000000000000000000006B6363009CA5A5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000008080800000000000000
      00000000000052525200848484009C9C9C00CECECE00BDBDBD00B5B5B500A5A5
      A5004A4A4A000000000000000000000000000000000000000000000000000000
      00009C6363006B3131006B4242006B4242006B4242006B3131009C6363000000
      00000000000000000000000000000000000000000000398CAD005AADD60084CE
      F7006BB5DE005AADCE0000398C00526B84000000000000000000000000000000
      000000000000000000000000000000000000000000005A524A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004A4A4A00848484009C9C9C00CECECE00B5B5B5009494940094949400B5B5
      B500949494004A4A4A0000000000000000000000000000000000000000000000
      00006B3131006B3131006B3131006B3131006B3131006B3131006B3131000000
      0000000000000000000000000000000000000000000073A5B500398CAD008CCE
      FF0084CEF700009CFF000063CE0000398C00526B840000000000000000000000
      000000000000000000000000000000000000000000006B635A00949494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004A42310039211000000000000000000000000000000000003939
      3900848484009C9C9C00CECECE00B5B5B500949494009C9C9C00848484008484
      84009C9C9C008484840042424200000000000000000000000000000000000000
      00000000000000000000BDBDBD006B6B6B00BDBDBD0000000000000000000000
      0000000000000000000000000000000000000000000000000000398CAD00FFFF
      FF0000CEFF0000B5FF00009CFF000063CE0000398C00526B8400000000000000
      000000000000000000000000000000000000000000009C9C9C0063635A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000031211000524A3900000000000000000000000000000000008484
      84009C9C9C00CECECE00B5B5B500949494009494940084848400737373007B7B
      7B00ADADAD00CECECE0039393900000000000000000000000000000000000000
      00007B84840000000000CECECE006B6B6B00CECECE00000000007B8484000000
      0000000000000000000000000000000000000000000000000000000000002152
      BD00FFFFFF0000CEFF0000B5FF00009CFF000063CE0000398C00526B84000000
      00000000000000000000000000000000000000000000000000006B635A008C8C
      8C00000000000000000000000000000000000000000000000000312110000000
      000031211000312110000000000000000000000000000000000000000000CECE
      CE00CECECE00B5B5B5004A4A4A00101010005A5A5A00737373006B6B6B00B5B5
      B500CECECE003131310000000000000000000000000000000000000000000000
      000000000000CECECE00E7E7E700CECECE00E7E7E700CECECE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002152BD00FFFFFF0000CEFF0000B5FF00009CFF000063CE0000398C00526B
      8400000000000000000000000000000000000000000000000000000000004A39
      3100A5ADAD000000000000000000000000000000000000000000291808002918
      1000291810003931290000000000000000000000000018181800000000002929
      2900B5B5B5009C9C9C0010101000C6C6C600101010006B6B6B00B5B5B500DEDE
      DE00292929000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00BDBDBD00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002152BD00FFFFFF0000CEFF0000B5FF00009CFF000063CE003131
      310063636B000000000000000000000000000000000000000000000000000000
      000042393100A5ADAD0000000000000000000000000000000000211808002118
      0800211808002118080021180800000000000000000000000000181818000000
      000029292900CECECE005A5A5A00181818004A4A4A00B5B5B500FFFFFF002929
      2900000000000000000000000000000000000000000000000000000000000000
      000000000000E7E7E700FFFFFF0084848400FFFFFF00E7E7E700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002152BD00FFFFFF0000CEFF0000BDFF00424242009C9C
      9C003131310063636B0000000000000000000000000000000000000000000000
      00000000000042393100A5ADAD00000000000000000000000000211008002110
      0800211008002110080000000000000000000000000000000000000000003939
      39000000000021212100CECECE0010101000B5B5B500FFFFFF00181818000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008C94940000000000FFFFFF006B6B6B00FFFFFF00000000008C9494000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002152BD00FFFFFF0063636300CECECE009C9C
      9C009C9C9C003131310063638C00000000000000000000000000000000000000
      0000000000000000000039312900A5ADAD000000000000000000181008001810
      0800181008000000000000000000000000000000000000000000000000000000
      000000000000000000001818180018181800FFFFFF0021212100080808000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BDBDBD006B6B6B00BDBDBD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000063636300D6EFCE00CECECE00CECE
      CE00424242005252E70031319C006B6B94000000000000000000000000000000
      0000000000000000000000000000393129008C94940000000000100808001008
      0800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002121210000000000080808000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000949C9C00000000006B6B6B0000000000949C9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000063636300FFFFFF006363
      63009C9CFF006363FF005252E70031319C0000000000000000004A4A42004A42
      4200848C8C00000000000000000000000000211884000808EF00100800000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000101010000000000000000000101010000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000063636300000000006363630000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000063636300FFFF
      FF009C9CFF009C9CFF006363FF005252BD0000000000524A4200000000000000
      0000312921008484840000000000000000001008EF006363FF001010F7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000313131005A5A63005A5A6300313131000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008C949C00000000008C949C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005252
      BD00FFFFFF009C9CFF005252BD00949CC6000000000000000000000000000000
      0000000000004A4A4200393931007B848400525ACE000808E7006B6BDE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003131310031313100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005252BD005252BD00949CC600000000000000000000000000000000000000
      0000000000000000000000000000312921004A4A420000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000018314A008CC6FF005AADFF005AADFF005A9CEF004A8CC6004A84
      B50018314A000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006B5A
      52004A5252000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000396B94008CC6FF005AADFF005AADFF005A9CEF004A8CC6004A84
      B50018314A00000000000000000000000000FF9C0000FF4A1800FF4A1800FF4A
      1800FF4A1800FF4A180000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000635A
      52004A4A52000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000018314A008CC6FF008CC6FF008CC6FF005AADFF005AADFF005A9CEF004A84
      B50018314A0000000000000000000000000000000000FF9C0000FFFFFF00FFFF
      FF00FFFF9C00FFA55200FF4A1800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000635A52006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004A5252004A4A52004A525200635A
      52004A4A4A004A4A4A00424A4A00424A4A00424A4A0042424A00424242004242
      4200525252000000000000000000000000000000000000000000000000001831
      52008CC6FF00ADD6FF008CC6FF008CC6FF007BBDFF0073B5FF005AADFF00528C
      CE00396B94002139520000000000000000000000000000000000FF9C0000FFFF
      FF00FFFFFF00FFFF9C00FFA55200FF4A18000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000949CA5006352
      4A006B6B6B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005A524A005A524A00525252005A52
      4A00525252005A524A005A524A005A524A005A524A005A524A005A524A005A52
      4A004A5252000000000000000000000000000000000000000000213952008CC6
      FF00ADD6FF00CEE7FF00ADD6FF00ADD6FF008CC6FF008CC6FF0073B5FF005AAD
      FF004A84B500213952000000000000000000000000000000000000000000FF9C
      0000FFFFFF00FFFFFF00FFFF9C00FFA55200FF4A180000000000000000000000
      000000000000000000000000000000000000000000000000000000000000949C
      A5005A524A006B6B6B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005A52
      42004A52520000000000000000000000000000000000000000004A5252005A52
      42004A52520000000000000000000000000000000000425A730021395A00ADD6
      FF00CEE7FF00CEE7FF00CEE7FF00CEE7FF00ADD6FF008CC6FF0084BDFF0073B5
      FF006BB5FF004A84B50021395A00000000000000000000000000000000000000
      0000FF9C0000FFFFFF00DECEBD00423121004231210000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000949CA500524A42006B6B6B00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000524A
      42004A52520000000000000000000000000000000000524A420000000000524A
      42004A5252000000000000000000000000000000000021425A00ADD6FF00CEE7
      FF009CCEFF00CEE7FF00CEE7FF00CEE7FF00CEE7FF00ADD6FF00ADD6FF008CC6
      FF007BBDFF005AADFF0021395A00000000000000000000000000000000000000
      000000000000FF9C0000A58C6300DECEBD00A58C630042312100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000949CA500524A39006B6B6B000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000524A
      39004A5252000000000000000000000000005242390000000000000000005242
      39004A525200000000000000000000000000214263008CC6FF00CEE7FF008CC6
      FF0021426300CEE7FF00CEE7FF00CEE7FF00CEE7FF00CEE7FF00CEE7FF008CC6
      FF00ADD6FF008CC6FF004A84B500000000000000000000000000000000000000
      00000000000000000000A58C6300FFFFFF00DECEBD00A58C6300423121000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000949CA5004A4239006B6B6B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004A42
      39004A4A520000000000000000004A4239000000000000000000000000004A42
      39004A52520000000000000000000000000029426300CEE7FF008CC6FF002142
      630029426300CEE7FF00CEE7FF008CC6FF00CEE7FF008CC6FF00CEE7FF008CC6
      FF004A84B500ADD6FF005AADFF00294263000000000000000000000000000000
      0000000000000000000000000000A58C6300FFFFFF00DECEBD00A58C63004231
      2100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000949CA500423931006B6B6B00000000000000
      0000000000000000000000000000000000000000000000000000000000004239
      3100424A4A00000000004A393100000000000000000000000000000000004239
      31004A52520000000000000000000000000000000000294A6B004A84B5000000
      0000294A6B00CEE7FF004A84B5008CC6FF00CEE7FF004A84B500CEE7FF00ADD6
      FF00294A6B00CEE7FF008CC6FF00294A6B000000000000000000000000000000
      000000000000000000000000000000000000A58C6300FFFFFF00DECEBD00A58C
      6300423121000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000949CA500423931006B6B6B000000
      0000000000000000000000000000000000000000000000000000000000004239
      3100424242004239310000000000000000000000000000000000000000004239
      31004A4A4A00000000000000000000000000000000000000000000000000294A
      6B008CC6FF00CEE7FF0029527300CEE7FF00CEE7FF0029527300CEE7FF008CC6
      FF00294A6B008CC6FF00BDDEFF00294A6B000000000000000000000000000000
      00000000000000000000000000000000000000000000A58C6300FFFFFF00DECE
      BD00A58C63004231210000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000949CA500393129006B6B
      6B00000000000000000000000000000000000000000000000000000000003931
      290042424200424A4A00424A4A00424A4A00424A4A004A4A4A004A4A4A003931
      2900424A4A004A4A4A004A525200000000000000000000000000425A7B004A84
      B500CEE7FF008CC6FF0029527300CEE7FF00ADD6FF0029527300CEE7FF008CC6
      FF00294A7300294A7300CEE7FF00294A73000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A58C6300FFFF
      FF00DECEBD00A58C630042312100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000949CA5003129
      29006B6B6B000000000000000000000000000000000000000000000000003931
      2900393129003931290039312900393129003931290039312900525252003931
      29004A4A52003931290039312900000000000000000000000000294A6B00CEE7
      FF00ADD6FF004A84B50029527300CEE7FF008CC6FF0029527300CEE7FF008CC6
      FF00295273003152730029527300000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A58C
      6300FFFFFF00DECEBD00A58C6300423121000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000949C
      A5003129210073737B0000000000000000000000000000000000312921000000
      0000000000000000000000000000000000000000000000000000000000003129
      21004A525200000000000000000000000000000000000000000029527300CEE7
      FF008CC6FF002952730029527300CEE7FF008CC6FF0029527300CEE7FF008CC6
      FF00295273000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A58C6300FFFFFF00DECEBD00423121000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009C9CA5003129210000000000000000000000000031292100000000000000
      0000000000000000000000000000000000000000000000000000000000003129
      2100525252000000000000000000000000000000000000000000000000002952
      7300315273000000000029527300CEE7FF008CC6FF0029527300295273003152
      7300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A58C6300A58C6300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003129210000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003129
      21005A6363000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000295273002952730000000000000000000000
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
      00007B7B73006B635A006B5A52006B5A52006B5A52006B5A52006B635A007B7B
      7300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003131310031313100313131003131
      3100424242000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B84006B6B63006B635A006B5A52006B5A52006B635A006B6B63007B7B
      8400000000000000000000000000000000000000000000000000000000006B6B
      63007B736B000000000000000000000000000000000000000000000000007B73
      6B006B6B63000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000392918004A393100000000000000
      0000000000000000000000000000000000000000000063636300CECECE009C9C
      9C00313131002121210000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007373
      6B007B736B00ADA59C00D6D6CE00F7F7F700F7F7F700D6D6CE00ADA59C007B73
      6B0073736B0000000000000000000000000000000000000000006B6B6300847B
      7300000000000000000000000000000000000000000000000000000000000000
      0000847B7B006B6B630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003921100039211000423121000000
      00000000000000000000000000000000000000000000737373009C9C9C009C9C
      9C009C9C9C005A5A5A0018181800000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000073737300847B
      7300CECECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CECE
      CE00847B7B007373730000000000000000000000000084848400736B63000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000736B630084848400000000000000000000000000000000000000
      0000000000000000000000000000392918003921100039211000000000000000
      000000000000000000000000000000000000000000000000000063636300CECE
      CE009C9C9C004242420039393900212121000000000000000000000000000000
      000000000000000000000000000000000000000000008C8C8C00736B6300CECE
      C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CECEC600736B63008C8C8C000000000000000000635A5A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000635A5A00000000000000000000000000000000000000
      000039211000524A420000000000392110003921100042312100000000000000
      0000000000000000000000000000000000000000000000000000636B6B00CECE
      CE007B7B7B005A5A5A0039393900181818000000000000000000000000000000
      00000000000000000000000000000000000000000000736B6B00A59C9400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00A59C9400736B6B000000000000000000524A4200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000392110003921100039211000392110003921100000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      39007B7B7B005A5A5A0039393900FF310000CE00000000000000000000000000
      000000000000000000000000000000000000000000005A525200CECECE00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00CECECE005A52520000000000000000004A423900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000039211000CEC6C6009C8C84005A4A390039211000524A4200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003939390039393900FF310000FF9C3100FFFFFF00CE000000000000000000
      000000000000000000000000000000000000000000004A424200EFEFEF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00EFEFEF004A424200000000000000000042393100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000031211000FFFFFF00FFFFFF00CECEC600A59C940031211000312110000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF633100FFFFFF00FFCE9C00FF310000CE0000000000
      000000000000000000000000000000000000000000004A423900E7E7DE00F7F7
      EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7EF00F7F7
      EF00F7F7EF00E7E7DE004A423900000000000000000042393100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000031211000FFFFFF00FFFFFF00FFFFFF00FFFFFF0031211000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF633100FF310000FFCE9C00FF9C3100CE00
      0000000000000000000000000000000000000000000052524A00BDBDAD00EFEF
      E700EFEFE700EFEFE700EFEFE700EFEFE700EFEFE700EFEFE700EFEFE700EFEF
      E700EFEFE700BDBDAD0052524A00000000000000000052524A00848473000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000029180800FFFFFF00FFFFFF00FFFFFF002918080000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF633100FFFFFF00FFCE9C00FF9C
      3100CE0000000000000000000000000000000000000073736B0084847300E7E7
      D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7D600E7E7
      D600E7E7D6008484730073736B0000000000000000008C8C8C004A4A39000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000021180800FFFFFF00FFFFFF00211808000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF633100FFFFFF00FFCE
      9C00FF9C3100CE000000000000000000000000000000000000005A524A00ADA5
      9400E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DECE00E7DE
      CE00ADA594005A524A0000000000000000000000000000000000635A5A005A52
      4A00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000018100800FFFFFF0018100800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF633100FFFF
      FF00FFCE9C00FF9C3100CE000000000000000000000000000000848484005A52
      4A00A59C8C00EFEFDE00F7F7EF00F7F7EF00F7F7EF00F7F7EF00EFEFDE00A59C
      8C005A524A00848484000000000000000000000000000000000000000000635A
      5A004A4239007B73630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000100800001008080000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF63
      3100FFFFFF00FFCE9C00FF9C3100CE0000000000000000000000000000008484
      8400524A4A007B736300A59C8C00FFFFFF00FFFFFF00A59C8C007B736300524A
      4A00848484000000000000000000000000000000000000000000000000000000
      00008C8C8C00524A4A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000080800000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF633100FFFFFF00FFCE9C00CE0000000000000000000000000000000000
      00000000000073737300524A4A003931290039312900524A4A00737373000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF633100FF633100E77352000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000B00000000100010000000000800500000000000000000000
      000000000000000000000000FFFFFF00FFF9FFF900000000FFF0FFF000000000
      FFE0FFE000000000FFC1FFC100000000C003C003000000008007800700000000
      000F000F00000000000F000F00000000000F000F00000000000F000F00000000
      000F000F00000000000F000F00000000000F000F00000000000F000F00000000
      801F801F00000000C03FC03F00000000FC00FFFFFFFFFFFFFC00F81FFFBFFDFF
      8000E007FF9FF9FF0000C003FFCFF3FF00008001FFC7E3FF00000000FFE7E7FF
      00010000C0E3C70300030000C1E3C78300070000C1E3C78300070000C0E3C703
      00070000D003C00B00078001D807E01B00078001FC0FF03F800FC003FF1FF8FF
      F8FFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC01C001FFFFF3FFFC01
      80018FE3E19FFC018001CFE7ED0FFC018001C7C7ED6F00018001C007E06F0001
      8001E00FF00F00018001E38FF81F00018001F39FFC7F00038001F11FFC3F0007
      8001F11FF83F000F8001F83FF91F007F8001F83FF99F00FF8001FC7FFB9F01FF
      8001FC7FFBDF03FFFFFFFFFFFFFFFFFFFFFFFC00C001FFFF9801FC00C001FFFF
      9801FC00C001FFFFF801FC00C0010007F801FC00C0010007F801FC00C0010003
      F801FC00C001000380010000C001000180010000C00100018001003FC0010000
      803F003FC0010000803F003FC0010007803F003FC00100078039003FC00301FF
      8039003FC00783FFFFFF003FC00FFFFFFFFFFFFFFFFFFF07FF01FFFFFFFFFE03
      FF01FC01FC01FE03FF01FC01FC01FE03E001FC01FC01FE03E00180018001FF07
      E00180018001FE03E00180018001FE03000F80018001FE03000F800180019C03
      000F800F800FC803000F800F800FE307000F800F800FE30701FF800F800FC98F
      01FF800F800F9C8FFFFFFFFFFFFFFFDFFFFFFFFFE1FFFFFFFFFFFFFFCFFFFF01
      FFFFFFFF9FFFFF010001F007BFFFFF010001F0079FF9E0010001F0079FF9E001
      0001F007CFD3E0010001F007E7C3E0010001F007F3C1000F0001F007F9C3000F
      0001E003FCC7000F0001E003FE4F000F0001F007C71F000F0001FE3FB3BF01FF
      FFFFFFFFF83F01FFFFFFFFFFFE7FFFFF9FFFFFFFFFF9FFFF07FFF9CFFFF0FFFF
      03FFF80FFFE0FFFF81FFFDDFFFC1C00180FFFDDFC003C001C07FFDDF8007C001
      E03FFDDF000FC001F017FDDF000F0001F803FDDF000F0001FC0399CC000FC001
      FE0381C0000FC001FF01DFFD000FC001FE00DFFD000FE03FFF008000000FF07F
      FFE09FFC801FFFFFFFF1FFFFC03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8889
      E007C00380FFBFFDE007C003007FFFFDC003C003003FBFFFC003C003801FBFFD
      8001C003800FBFFD8001C003C007FFFD8001C003C003BFFF8001C003E003BFFD
      8001C003F001BFFDC003C003F801FFFDC003C003FC00BFFFE007C003FE00BFFD
      E007FFFFFF019111FFFFFFFFFFFFFFFFDF3FF83FFFFFE1FF9E1FF01F87FFCFFF
      9C0FF01F81FF9FFF9807F01F80FFBFFF9003F01F807F9FF98001F83FC03F9FF9
      8001F01FE01FCFD38003F01FF00FE7C38007F01FF807F3C1C00FF01FFC03F9C3
      E01FF01FFE01FCC7FC1FF83FFF00FE4FFE5FF83FFF80C71FFEDFFC7FFFC0B31F
      FE1FFC7FFFE0F81FFF3FFEFFFFF1FE7FFFFFF807FFFFFFFFE7FFF80703FFFFFF
      E7FFF00781FFCFFF0007E003C0FFC7FF0007C003E07FE3FFE7C78001F07FF1FF
      E7A78001F83FF8FFE7670001FC1FFC7FE6E70000FE0FFE3FE5E79000FF07FF1F
      E3E7E000FF83FF8FE001C000FFC1FFC7E001C001FFE0FFE3DFE7C007FFF0FFF3
      BFE7E40FFFF9FFFF7FE7FE7FFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFF07FFF00F
      E7E7FF3F83FFE007CFF3FF1F81FFC0039FF9FE3FC0FF8001BFFDF23FC0FF8001
      BFFFF07FE07F8001BFFFF03FF03F8001BFFFF01FFC1F8001BFFFF03FFE0F8001
      9FFFF07FFF0780019FFFF0FFFF83C003CFFFF1FFFFC1C003E3FFF3FFFFE0E007
      F3FFF7FFFFF0F81FFFFFFFFFFFF8FFFF00000000000000000000000000000000
      000000000000}
  end
  object MainMenu1: TMainMenu
    Images = ImageList1
    Left = 75
    Top = 103
    object File1: TMenuItem
      Caption = 'File'
      object MenuNew: TMenuItem
        Action = FileNew
      end
      object MenuOpen: TMenuItem
        Action = FileOpen
      end
      object MenuSave: TMenuItem
        Action = FileSave
      end
      object MenuSaveAs: TMenuItem
        Action = FileSaveAs
      end
      object SaveasLibraryItem1: TMenuItem
        Action = FileSaveAsLibraryItem
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object ExporttoBitmap1: TMenuItem
        Caption = 'Export to Bitmap ...'
        OnClick = ExporttoBitmap1Click
      end
      object ExporttoWMF1: TMenuItem
        Caption = 'Export to WMF ...'
        OnClick = ExporttoWMF1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object MenuClose1: TMenuItem
        Action = FileClose
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object ZoomIn1: TMenuItem
        Action = ViewZoomIn
      end
      object ZoomOut1: TMenuItem
        Action = ViewZoomOut
      end
      object Zoom1: TMenuItem
        Caption = 'Zoom'
        object Zoom101: TMenuItem
          Action = ViewZoom10
        end
        object Zoom251: TMenuItem
          Action = ViewZoom25
        end
        object Zoom501: TMenuItem
          Action = ViewZoom50
        end
        object Zoom1001: TMenuItem
          Action = ViewZoom100
        end
        object Zoom2001: TMenuItem
          Action = ViewZoom200
        end
        object Zoom4001: TMenuItem
          Action = ViewZoom400
        end
        object Zoom8001: TMenuItem
          Action = ViewZoom800
        end
        object Zoom16001: TMenuItem
          Action = ViewZoom1600
        end
        object Zoom32001: TMenuItem
          Action = ViewZoom3200
        end
        object Zoom64001: TMenuItem
          Action = ViewZoom6400
        end
        object Zoom100001: TMenuItem
          Action = ViewZoom10000
        end
      end
    end
    object MenuEdit: TMenuItem
      Caption = 'Edit'
      object MenuUndo: TMenuItem
        Action = EditUndo
      end
      object MenuRedo: TMenuItem
        Action = EditRedo
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object MenuCut: TMenuItem
        Action = EditCut
      end
      object MenuCopy: TMenuItem
        Action = EditCopy
      end
      object MenuPaste: TMenuItem
        Action = EditPaste
      end
      object MenuDelete: TMenuItem
        Action = EditDelete
        GroupIndex = 1
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object MenuPen: TMenuItem
        Action = ToolPen
        GroupIndex = 1
      end
      object MenuRemovePoint: TMenuItem
        Action = ToolRemovePoint
        GroupIndex = 1
      end
      object N10: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object Clearallguides1: TMenuItem
        Caption = 'Clear all guides'
        Enabled = False
        GroupIndex = 1
        OnClick = Clearallguides1Click
      end
    end
    object MenuArrange1: TMenuItem
      Caption = 'Arrange'
      object Select1: TMenuItem
        Caption = 'Select'
        object MenuSelectAll: TMenuItem
          Action = ToolSelectAll
          GroupIndex = 1
        end
        object MenuDeselect: TMenuItem
          Action = ToolDeselect
          GroupIndex = 1
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuBringToFront: TMenuItem
        Action = ArrangeBringToFront
      end
      object MenuBringForward: TMenuItem
        Action = ArrangeBringForward
      end
      object MenuSendToBack: TMenuItem
        Action = ArrangeSendToBack
      end
      object MenuSendBackward: TMenuItem
        Action = ArrangeSendBackward
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MenuGroup: TMenuItem
        Action = ArrangeGroup
      end
      object MenuUngroup: TMenuItem
        Action = ArrangeUngroup
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object Pack1: TMenuItem
        Action = ArrangePack
      end
      object Unpack1: TMenuItem
        Action = ArrangeUnpack
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Lock1: TMenuItem
        Action = ArrangeLockSelection
      end
    end
    object MenuInsert: TMenuItem
      Caption = 'Insert'
      GroupIndex = 1
      object MenuSelect: TMenuItem
        Action = ToolSelect
        GroupIndex = 1
      end
      object N12: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object MenuPicture: TMenuItem
        Action = InsertPicture
        GroupIndex = 1
        RadioItem = True
      end
      object MenuRectangle: TMenuItem
        Action = InsertRect
        GroupIndex = 1
      end
      object MenuCircle: TMenuItem
        Action = InsertCircle
        GroupIndex = 1
      end
      object MenuEllipse: TMenuItem
        Action = InsertEllipse
        GroupIndex = 1
      end
      object MenuArc: TMenuItem
        Action = InsertArc
        GroupIndex = 1
      end
      object MenuPolygon: TMenuItem
        Action = InsertPolygon
        GroupIndex = 1
      end
      object MenuBezier: TMenuItem
        Action = InsertBezier
        GroupIndex = 1
      end
      object MenuFreehand: TMenuItem
        Action = InsertFreehand
        GroupIndex = 1
      end
      object MenuText: TMenuItem
        Action = InsertText
        GroupIndex = 1
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object LibraryItem1: TMenuItem
        Action = InsertLibraryItem
        GroupIndex = 1
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      GroupIndex = 1
      object SnaptoGrid1: TMenuItem
        Caption = 'Snap to Grid'
        OnClick = SnaptoGrid1Click
      end
      object CanSelect1: TMenuItem
        Caption = 'Can Select'
        OnClick = CanSelect1Click
      end
      object Multiselect1: TMenuItem
        Caption = 'Multi select'
        OnClick = Multiselect1Click
      end
      object Hottracking1: TMenuItem
        Caption = 'Hot tracking'
        OnClick = Hottracking1Click
      end
      object Mouseactions1: TMenuItem
        Caption = 'Mouse actions'
        OnClick = Mouseactions1Click
      end
      object Keyactions1: TMenuItem
        Caption = 'Key actions'
        OnClick = Keyactions1Click
      end
      object Showguides1: TMenuItem
        Caption = 'Show guides'
        OnClick = Showguides1Click
      end
      object Showcursorguide1: TMenuItem
        Caption = 'Show cursor guide'
        OnClick = Showcursorguide1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object GridType1: TMenuItem
        Caption = 'Grid Type'
        object GridLine: TMenuItem
          Caption = 'Line'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = GridLineClick
        end
        object GridDot: TMenuItem
          Caption = 'Dot'
          GroupIndex = 1
          RadioItem = True
          OnClick = GridDotClick
        end
        object GridNone: TMenuItem
          Caption = 'None'
          GroupIndex = 1
          RadioItem = True
          OnClick = GridNoneClick
        end
      end
    end
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 103
    Top = 103
    object InsertRect: TAction
      Tag = 100
      Category = 'Insert'
      Caption = 'Rectangle'
      Hint = 'Rectangle'
      ImageIndex = 13
      OnExecute = InsertRectExecute
    end
    object InsertPolygon: TAction
      Tag = 100
      Category = 'Insert'
      Caption = 'Polygon'
      Hint = 'Polygon'
      ImageIndex = 12
      OnExecute = InsertPolygonExecute
    end
    object InsertBezier: TAction
      Tag = 100
      Category = 'Insert'
      Caption = 'Bezier'
      Hint = 'Bezier'
      ImageIndex = 11
      OnExecute = InsertBezierExecute
    end
    object InsertFreehand: TAction
      Tag = 100
      Category = 'Insert'
      Caption = 'Freehand'
      Hint = 'Freehand'
      ImageIndex = 22
      OnExecute = InsertFreehandExecute
    end
    object InsertEllipse: TAction
      Tag = 100
      Category = 'Insert'
      Caption = 'Ellipse'
      Hint = 'Ellipse'
      ImageIndex = 37
      OnExecute = InsertEllipseExecute
    end
    object InsertCircle: TAction
      Tag = 100
      Category = 'Insert'
      Caption = 'Circle'
      Hint = 'Circle'
      ImageIndex = 3
      OnExecute = InsertCircleExecute
    end
    object InsertText: TAction
      Tag = 100
      Category = 'Insert'
      Caption = 'Text'
      Hint = 'Text'
      ImageIndex = 17
      OnExecute = InsertTextExecute
    end
    object InsertArc: TAction
      Tag = 100
      Category = 'Insert'
      Caption = 'Arc'
      Hint = 'Arc'
      ImageIndex = 0
      OnExecute = InsertArcExecute
    end
    object ToolSelect: TAction
      Tag = 100
      Category = 'Tool'
      Caption = 'Select'
      Checked = True
      Hint = 'Select'
      ImageIndex = 1
      OnExecute = ToolSelectExecute
    end
    object ToolSelectAll: TAction
      Category = 'Tool'
      Caption = 'Select All'
      Hint = 'Select All'
      ShortCut = 16449
      OnExecute = ToolSelectAllExecute
    end
    object ToolDeselect: TAction
      Category = 'Tool'
      Caption = 'Deselect'
      Enabled = False
      Hint = 'Deselect'
      ShortCut = 16452
      OnExecute = ToolDeselectExecute
    end
    object ToolPen: TAction
      Tag = 100
      Category = 'Tool'
      Caption = 'Pen'
      Enabled = False
      Hint = 'Pen'
      ImageIndex = 9
      OnExecute = ToolPenExecute
    end
    object EditDelete: TAction
      Category = 'Edit'
      Caption = 'Delete'
      Enabled = False
      Hint = 'Delete'
      ImageIndex = 21
      ShortCut = 46
      OnExecute = EditDeleteExecute
    end
    object EditNewLayer: TAction
      Category = 'Edit'
      Caption = 'New Layer'
      Hint = 'New Layer'
      ImageIndex = 19
      OnExecute = EditNewLayerExecute
    end
    object InsertPicture: TAction
      Tag = 100
      Category = 'Insert'
      Caption = 'Picture ...'
      Hint = 'Picture'
      ImageIndex = 20
      OnExecute = InsertPictureExecute
    end
    object ArrangeBringToFront: TAction
      Category = 'Arrange'
      Caption = 'Bring to front'
      Enabled = False
      Hint = 'Bring to front'
      ImageIndex = 23
      OnExecute = ArrangeBringToFrontExecute
    end
    object ArrangeSendToBack: TAction
      Category = 'Arrange'
      Caption = 'Send to back'
      Enabled = False
      Hint = 'Send to back'
      ImageIndex = 24
      OnExecute = ArrangeSendToBackExecute
    end
    object ArrangeBringForward: TAction
      Category = 'Arrange'
      Caption = 'Bring forward'
      Enabled = False
      Hint = 'Bring forward'
      ImageIndex = 25
      OnExecute = ArrangeBringForwardExecute
    end
    object ArrangeSendBackward: TAction
      Category = 'Arrange'
      Caption = 'Send backward'
      Enabled = False
      Hint = 'Send backward'
      ImageIndex = 26
      OnExecute = ArrangeSendBackwardExecute
    end
    object ToolRemovePoint: TAction
      Category = 'Tool'
      Caption = 'Remove point'
      Enabled = False
      Hint = 'Remove point'
      ImageIndex = 27
      OnExecute = ToolRemovePointExecute
    end
    object FileNew: TAction
      Category = 'File'
      Caption = 'New'
      Hint = 'New'
      ImageIndex = 30
      ShortCut = 16462
      OnExecute = FileNewExecute
    end
    object FileOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      Hint = 'Open'
      ImageIndex = 31
      ShortCut = 16463
      OnExecute = FileOpenExecute
    end
    object FileSave: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 32
      ShortCut = 16467
      OnExecute = FileSaveExecute
    end
    object FileSaveAs: TAction
      Category = 'File'
      Caption = 'Save as...'
      Hint = 'Save as...'
      ShortCut = 24659
      OnExecute = FileSaveAsExecute
    end
    object FileClose: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit'
      ShortCut = 16465
      OnExecute = FileCloseExecute
    end
    object ArrangeGroup: TAction
      Category = 'Arrange'
      Caption = 'Group'
      Enabled = False
      Hint = 'Group'
      ImageIndex = 28
      OnExecute = ArrangeGroupExecute
    end
    object ArrangeUngroup: TAction
      Category = 'Arrange'
      Caption = 'Ungroup'
      Enabled = False
      Hint = 'Ungroup'
      ImageIndex = 29
      OnExecute = ArrangeUngroupExecute
    end
    object InsertLabel: TAction
      Category = 'Insert'
      Caption = 'Label'
      Hint = 'Label'
      ImageIndex = 33
      OnExecute = InsertLabelExecute
    end
    object EditCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      Enabled = False
      Hint = 'Cut'
      ImageIndex = 34
      ShortCut = 16472
      OnExecute = EditCutExecute
    end
    object EditCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      Enabled = False
      Hint = 'Copy'
      ImageIndex = 35
      ShortCut = 16451
      OnExecute = EditCopyExecute
    end
    object EditPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      Enabled = False
      Hint = 'Paste'
      ImageIndex = 36
      ShortCut = 16470
      OnExecute = EditPasteExecute
    end
    object EditUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Enabled = False
      Hint = 'Undo'
      ImageIndex = 38
      ShortCut = 16474
      OnExecute = EditUndoExecute
    end
    object EditRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      Enabled = False
      Hint = 'Redo'
      ImageIndex = 39
      ShortCut = 24666
      OnExecute = EditRedoExecute
    end
    object ToolPan: TAction
      Category = 'Tool'
      Caption = 'Pan'
      Hint = 'Pan'
      ImageIndex = 18
      OnExecute = ToolPanExecute
    end
    object ViewZoomIn: TAction
      Category = 'View'
      Caption = 'Zoom In'
      Hint = 'Zoom In'
      ImageIndex = 40
      OnExecute = ViewZoomInExecute
    end
    object ViewZoomOut: TAction
      Category = 'View'
      Caption = 'Zoom Out'
      Hint = 'Zoom Out'
      ImageIndex = 41
      OnExecute = ViewZoomOutExecute
    end
    object ViewZoom10: TAction
      Category = 'View'
      Caption = 'Zoom 10%'
      Hint = 'Zoom 10%'
      OnExecute = ViewZoom10Execute
    end
    object ViewZoom25: TAction
      Category = 'View'
      Caption = 'Zoom 25%'
      Hint = 'Zoom 25%'
      OnExecute = ViewZoom25Execute
    end
    object ViewZoom50: TAction
      Category = 'View'
      Caption = 'Zoom 50%'
      Hint = 'Zoom 50%'
      OnExecute = ViewZoom50Execute
    end
    object ViewZoom100: TAction
      Category = 'View'
      Caption = 'Zoom 100%'
      Hint = 'Zoom 100%'
      OnExecute = ViewZoom100Execute
    end
    object ViewZoom200: TAction
      Category = 'View'
      Caption = 'Zoom 200%'
      Hint = 'Zoom 200%'
      OnExecute = ViewZoom200Execute
    end
    object ViewZoom400: TAction
      Category = 'View'
      Caption = 'Zoom 400%'
      Hint = 'Zoom 400%'
      OnExecute = ViewZoom400Execute
    end
    object ViewZoom800: TAction
      Category = 'View'
      Caption = 'Zoom 800%'
      Hint = 'Zoom 800%'
      OnExecute = ViewZoom800Execute
    end
    object ViewZoom1600: TAction
      Category = 'View'
      Caption = 'Zoom 1600%'
      Hint = 'Zoom 1600%'
      OnExecute = ViewZoom1600Execute
    end
    object ViewZoom3200: TAction
      Category = 'View'
      Caption = 'Zoom 3200%'
      Hint = 'Zoom 3200%'
      OnExecute = ViewZoom3200Execute
    end
    object ViewZoom6400: TAction
      Category = 'View'
      Caption = 'Zoom 6400%'
      Hint = 'Zoom 6400%'
      OnExecute = ViewZoom6400Execute
    end
    object ViewZoom10000: TAction
      Category = 'View'
      Caption = 'Zoom 10000%'
      Hint = 'Zoom 10000%'
      OnExecute = ViewZoom10000Execute
    end
    object FileSaveAsLibraryItem: TAction
      Category = 'File'
      Caption = 'Save as Library Item ...'
      Enabled = False
      OnExecute = FileSaveAsLibraryItemExecute
    end
    object InsertLibraryItem: TAction
      Category = 'Insert'
      Caption = 'Library Item ...'
      OnExecute = InsertLibraryItemExecute
    end
    object ArrangePack: TAction
      Category = 'Arrange'
      Caption = 'Pack'
      Enabled = False
      OnExecute = ArrangePackExecute
    end
    object ArrangeUnpack: TAction
      Category = 'Arrange'
      Caption = 'Unpack'
      Enabled = False
      OnExecute = ArrangeUnpackExecute
    end
    object ArrangeLockSelection: TAction
      Category = 'Arrange'
      Caption = 'Lock'
      Enabled = False
      OnExecute = ArrangeLockSelectionExecute
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 47
    Top = 131
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Layer files (*.sde)|*.sde'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 75
    Top = 131
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'sde'
    Filter = 'Layer files (*.sde)|*.sde'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 159
    Top = 131
  end
  object PopupMenu1: TPopupMenu
    Left = 131
    Top = 103
    object Zoom102: TMenuItem
      Action = ViewZoom10
    end
    object Zoom252: TMenuItem
      Action = ViewZoom25
    end
    object Zoom502: TMenuItem
      Action = ViewZoom50
    end
    object Zoom1002: TMenuItem
      Action = ViewZoom100
    end
    object Zoom2002: TMenuItem
      Action = ViewZoom200
    end
    object Zoom4002: TMenuItem
      Action = ViewZoom400
    end
    object Zoom8002: TMenuItem
      Action = ViewZoom800
    end
    object Zoom16002: TMenuItem
      Action = ViewZoom1600
    end
    object Zoom32002: TMenuItem
      Action = ViewZoom3200
    end
    object Zoom64002: TMenuItem
      Action = ViewZoom6400
    end
    object Zoom100002: TMenuItem
      Action = ViewZoom10000
    end
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmap files (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 131
    Top = 131
  end
  object SaveDialog3: TSaveDialog
    DefaultExt = 'sli'
    Filter = 'Library files (*.sli)|*.sli'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 103
    Top = 131
  end
  object OpenDialog2: TOpenDialog
    Filter = 'Library files (*.sli)|*.sli'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 75
    Top = 158
  end
  object ImageList2: TImageList
    Left = 47
    Top = 103
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFFF
      F700FFF7E700FFEFE700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6
      AD00EFCE9400F7D69C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFFF
      FF00FFFFF700FFF7E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6
      B500EFCE9C00EFCE9C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFFF
      FF00FFFFFF00FFFFF700FFEFE700F7E7D600F7E7D600F7E7CE00F7DEC600F7DE
      BD00EFCE9C00EFCE9C00B5848400000000000000000000000000000000000000
      00005A5A5A001818180018181800181818001818180018181800181818001818
      1800181818000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFFF
      FF00FFFFFF00FFFFFF00FFF7EF00FFEFE700F7E7D600F7E7CE00F7E7C600F7DE
      C600F7D6AD00EFCE9400B5848400000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE00181818000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CEB5AD00FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7E7
      C600F7D6B500EFCE9C00B5848400000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE00181818000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6B5AD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7EF00FFEFE700F7E7D600F7E7
      CE00F7DEBD00F7D6A500B5848400000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE00181818000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6BDB500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7F700FFEFE700FFEF
      E700F7E7D600F7D6AD00B5848400000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE00181818000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6BDB500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7F700F7F7
      EF00EFEFDE00E7DEC600B5848400000000000000000000000000000000000000
      00005A5A5A00FFFFFF009C633100FFEFCE009C633100FFCECE009C633100FFCE
      CE00181818000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F7F7EF00E7DEC600B5848400000000000000000000000000000000000000
      00005A5A5A00FFCECE009C633100FFCECE009C6331009C9C63009C6331009C9C
      6300181818000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700F7F7EF00EFEF
      DE00E7DEC600E7DEC600B5848400000000000000000000000000000000005A5A
      5A00633100006331000063310000633100006331000063310000633100006331
      0000633100001818180000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7E700C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000000000005A5A
      5A00FFCECE00FFEFCE00F7F7F700FFEFCE00FFCECE00FFEFCE00FFCECE00FFCE
      CE00FFCECE001818180000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7EF00EFE7D600BD8C
      7300F7E7C600F7E7C600C6846B00000000000000000000000000000000000000
      00005A5A5A005A5A5A005A5A5A009C633100FFCECE009C6331005A5A5A005A5A
      5A00848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7EFE700EFE7CE00C694
      7B00F7E7C600CE94730000000000000000000000000000000000000000000000
      0000000000000000000000000000181818001818180018181800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00EFE7D600E7DEC600C694
      7B00CE9C84000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00C001FFFF00000000C001FFFF00000000
      C001FFFF00000000C001F00700000000C001F00700000000C001F00700000000
      C001F00700000000C001F00700000000C001F00700000000C001F00700000000
      C001E00300000000C001E00300000000C001F00700000000C003FE3F00000000
      C007FFFF00000000C00FFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object OpenPictureDialog2: TOpenPictureDialog
    Left = 519
    Top = 144
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 519
    Top = 170
  end
end
