object Form1: TForm1
  Left = 312
  Top = 215
  Caption = 'Form1'
  ClientHeight = 387
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXTitleBar1: TSpTBXTitleBar
    Left = 0
    Top = 0
    Width = 531
    Height = 387
    Caption = 'Form1'
    DesignSize = (
      531
      387)
    object SpTBXSubmenuItem2: TSpTBXSubmenuItem
      Caption = 'Skins'
      Options = [tboDropdownArrow]
      OnDrawItem = SpTBXSubmenuItem2DrawItem
      LinkSubitems = SpTBXSubmenuItem3
    end
    object SpTBXStatusBar1: TSpTBXStatusBar
      Left = 4
      Top = 360
      Width = 523
      Height = 23
      ExplicitTop = 358
      object SpTBXLabelItem1: TSpTBXLabelItem
        Caption = 'Panel 1'
      end
      object SpTBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object SpTBXLabelItem2: TSpTBXLabelItem
        Caption = 'Panel 2'
      end
    end
    object SpTBXDock2: TSpTBXDock
      Left = 4
      Top = 26
      Width = 523
      Height = 23
      object SpTBXToolbar2: TSpTBXToolbar
        Left = 0
        Top = 0
        CloseButton = False
        DockPos = -8
        FullSize = True
        ProcessShortCuts = True
        ShrinkMode = tbsmWrap
        Stretch = True
        TabOrder = 0
        Caption = 'SpTBXToolbar2'
        Customizable = False
        MenuBar = True
        object SpTBXSubmenuItem1: TSpTBXSubmenuItem
          Tag = 100
          Caption = '&File'
          object SpTBXItem1: TSpTBXItem
            Caption = '&New'
          end
          object SpTBXItem2: TSpTBXItem
            Caption = '&Open'
          end
          object SpTBXSeparatorItem1: TSpTBXSeparatorItem
          end
          object SpTBXItem3: TSpTBXItem
            Caption = '&Exit'
          end
        end
        object SpTBXSubmenuItem3: TSpTBXSubmenuItem
          Caption = '&Skins'
          object SpTBXSkinGroupItem1: TSpTBXSkinGroupItem
          end
        end
      end
    end
    object SpTBXMultiDock1: TSpTBXMultiDock
      Left = 4
      Top = 49
      Width = 178
      Height = 311
      object SpTBXDockablePanel1: TSpTBXDockablePanel
        Left = 0
        Top = 0
        Width = 178
        Height = 311
        Caption = 'Form Popup Options'
        DockPos = 0
        TabOrder = 0
        Options.Close = False
        object SpTBXRadioGroup1: TSpTBXRadioGroup
          Left = 7
          Top = 56
          Width = 161
          Height = 97
          Caption = 'BorderStyle'
          TabOrder = 1
          OnClick = SpTBXRadioGroup1Click
          ItemIndex = 3
          Items.Strings = (
            'pbsFrame'
            'pbsSizeable'
            'pbsSizeableBottom'
            'pbsSizeableRightBottom')
        end
        object SpTBXCheckBox1: TSpTBXCheckBox
          Left = 11
          Top = 32
          Width = 69
          Height = 21
          Caption = 'Set focus'
          TabOrder = 2
          OnClick = SpTBXCheckBox1Click
        end
      end
    end
    object SpTBXGroupBox1: TSpTBXGroupBox
      Left = 192
      Top = 64
      Width = 329
      Height = 73
      Caption = 'Calendar Popup'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      object SpTBXButtonEdit1: TSpTBXButtonEdit
        Left = 16
        Top = 33
        Width = 121
        Height = 21
        TabOrder = 0
        EditButton.Left = 97
        EditButton.Top = 0
        EditButton.Width = 20
        EditButton.Height = 17
        EditButton.Align = alRight
        EditButton.DropDownMenu = SpTBXFormPopupMenu1
      end
      object SpTBXButton1: TSpTBXButton
        Left = 184
        Top = 32
        Width = 121
        Height = 25
        Caption = 'Date'
        TabOrder = 1
        DropDownMenu = SpTBXFormPopupMenu1
      end
    end
    object SpTBXGroupBox2: TSpTBXGroupBox
      Left = 192
      Top = 168
      Width = 329
      Height = 81
      Caption = 'Treeview Popup'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      object SpTBXButtonEdit2: TSpTBXButtonEdit
        Left = 16
        Top = 33
        Width = 121
        Height = 21
        TabOrder = 0
        EditButton.Left = 97
        EditButton.Top = 0
        EditButton.Width = 20
        EditButton.Height = 17
        EditButton.Align = alRight
        EditButton.DropDownMenu = SpTBXFormPopupMenu2
      end
      object SpTBXButton2: TSpTBXButton
        Left = 184
        Top = 32
        Width = 121
        Height = 25
        Caption = 'Colors'
        TabOrder = 1
        DropDownMenu = SpTBXFormPopupMenu2
      end
    end
  end
  object SpTBXFormPopupMenu1: TSpTBXFormPopupMenu
    BorderStyle = pbsSizeableRightBottom
    OnBeforeClosePopup = SpTBXFormPopupMenu1BeforeClosePopup
    Left = 200
    Top = 304
  end
  object SpTBXFormPopupMenu2: TSpTBXFormPopupMenu
    BorderStyle = pbsSizeableRightBottom
    OnBeforeClosePopup = SpTBXFormPopupMenu2BeforeClosePopup
    Left = 240
    Top = 304
  end
end
