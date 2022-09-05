object DelphiXMapEditForm: TDelphiXMapEditForm
  Left = 320
  Top = 226
  Width = 618
  Height = 442
  Caption = 'Background Sprite Map Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlRight: TPanel
    Left = 467
    Top = 0
    Width = 143
    Height = 408
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object CancelButton: TButton
      Left = 5
      Top = 40
      Width = 73
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object DelphiXMapEditPropertiesPane: TPanel
      Left = 0
      Top = 72
      Width = 138
      Height = 267
      BevelOuter = bvNone
      BorderStyle = bsSingle
      TabOrder = 1
    end
    object OKButton: TButton
      Left = 5
      Top = 8
      Width = 73
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
      OnClick = OKButtonClick
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 467
    Height = 408
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 1
    object pblBase: TPanel
      Left = 8
      Top = 8
      Width = 451
      Height = 392
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 4
      BorderStyle = bsSingle
      TabOrder = 0
      object Panel1: TPanel
        Left = 4
        Top = 61
        Width = 439
        Height = 323
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 1
        Caption = 'Panel1'
        TabOrder = 1
        object Splitter1: TSplitter
          Left = 114
          Top = 1
          Width = 3
          Height = 321
          Cursor = crHSplit
        end
        object PicturesToChip: TListBox
          Left = 1
          Top = 1
          Width = 113
          Height = 321
          Align = alLeft
          DragMode = dmAutomatic
          ItemHeight = 16
          PopupMenu = PopupMenu1
          Style = lbOwnerDrawVariable
          TabOrder = 0
          Visible = False
          OnDrawItem = PicturesToChipDrawItem
          OnMeasureItem = PicturesToChipMeasureItem
        end
        object ScrollBox1: TScrollBox
          Left = 117
          Top = 1
          Width = 321
          Height = 321
          Align = alClient
          TabOrder = 1
          object MapArea: TDrawGrid
            Left = 1
            Top = 1
            Width = 165
            Height = 165
            TabStop = False
            BorderStyle = bsNone
            DefaultColWidth = 32
            DefaultRowHeight = 32
            FixedCols = 0
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
            PopupMenu = PopupMenu2
            ScrollBars = ssNone
            TabOrder = 0
            Visible = False
            OnDblClick = MapAreaDblClick
            OnDragDrop = MapAreaDragDrop
            OnDragOver = MapAreaDragOver
            OnDrawCell = MapAreaDrawCell
            OnMouseDown = MapAreaMouseDown
            OnMouseMove = MapAreaMouseMove
          end
        end
      end
      object pnlLabels: TPanel
        Left = 4
        Top = 4
        Width = 439
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object BtnSetSize: TSpeedButton
          Left = 413
          Top = 3
          Width = 21
          Height = 46
          Hint = 'Set up size.'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
            DDDDDDDDDDDDDDD0DDDDDDDDDDDDDD080DDDDDDDDDDDD080DDDDDDDDDDDD080D
            DDDDDDDDDDD080DDDDDDDDDDDD080DDDDDDDDDDDD080DDDDDDDDDDD0080DDDDD
            DDDDDD0FF0DDDDDDDDDDDD0FF0DDDDDDDDDDDD0F0DDDDDDDDDDDDD00DDDDDDDD
            DDDDDD0DDDDDDDDDDDDDDD0DDDDDDDDDDDDDDDDDDDDDDDDDDDDD}
          ParentShowHint = False
          ShowHint = True
          OnClick = BtnSetSizeClick
        end
        object LAreaOfChips: TLabel
          Left = 121
          Top = 44
          Width = 234
          Height = 13
          Caption = 'Area of Chips (doubleclick to properties change):'
          FocusControl = ScrollBox1
          Transparent = True
        end
        object LHeight: TLabel
          Left = 288
          Top = 31
          Width = 46
          Height = 13
          Caption = 'Chip size:'
          FocusControl = EHeight
        end
        object LImageToSet: TLabel
          Left = 5
          Top = 5
          Width = 109
          Height = 13
          Caption = 'Image by name to set:'
          FocusControl = ImageToSet
        end
        object LMapSizeX: TLabel
          Left = 242
          Top = 7
          Width = 45
          Height = 13
          Caption = 'Map size:'
          FocusControl = eMapSizeX
        end
        object LMapSizeY: TLabel
          Left = 347
          Top = 7
          Width = 6
          Height = 13
          Caption = 'x'
          FocusControl = eMapSizeY
        end
        object LPicturesToChip: TLabel
          Left = 5
          Top = 44
          Width = 42
          Height = 13
          Caption = 'Pictures:'
          FocusControl = PicturesToChip
        end
        object LWidth: TLabel
          Left = 370
          Top = 31
          Width = 6
          Height = 13
          Caption = 'x'
          FocusControl = EWidth
        end
        object EHeight: TEdit
          Left = 334
          Top = 28
          Width = 32
          Height = 21
          TabOrder = 0
          Text = '32'
        end
        object eMapSizeX: TSpinEdit
          Left = 293
          Top = 4
          Width = 50
          Height = 22
          MaxValue = 128
          MinValue = 1
          TabOrder = 1
          Value = 1
        end
        object eMapSizeY: TSpinEdit
          Left = 360
          Top = 4
          Width = 50
          Height = 22
          MaxValue = 128
          MinValue = 1
          TabOrder = 2
          Value = 1
        end
        object EWidth: TEdit
          Left = 378
          Top = 28
          Width = 32
          Height = 21
          TabOrder = 3
          Text = '32'
        end
        object ImageToSet: TComboBox
          Left = 5
          Top = 19
          Width = 153
          Height = 21
          Style = csDropDownList
          Enabled = False
          ItemHeight = 13
          TabOrder = 4
          OnChange = ImageToSetChange
        end
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 104
    Top = 320
    object Fillall1: TMenuItem
      Caption = 'Fill all'
      OnClick = Fillall1Click
    end
  end
  object PopupMenu2: TPopupMenu
    OnPopup = PopupMenu2Popup
    Left = 256
    Top = 200
    object ClearOneChip1: TMenuItem
      Caption = 'Clear One Chip'
      OnClick = ClearOneChip1Click
    end
    object Clear1: TMenuItem
      Caption = 'Clear All Chips'
      GroupIndex = 1
      OnClick = Clear1Click
    end
  end
end
