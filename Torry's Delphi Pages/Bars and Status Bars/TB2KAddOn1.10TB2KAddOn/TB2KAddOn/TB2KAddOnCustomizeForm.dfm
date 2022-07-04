object TB2KCustomizeForm: TTB2KCustomizeForm
  Left = 451
  Top = 262
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Toolbar2000 Anpassen'
  ClientHeight = 311
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 297
    Height = 265
    ActivePage = tsToolbar
    TabOrder = 0
    object tsToolbar: TTabSheet
      Caption = 'Toolbar-Optionen'
      object Label1: TLabel
        Left = 8
        Top = 13
        Width = 39
        Height = 13
        Caption = 'Toolbar:'
      end
      object Label2: TLabel
        Left = 8
        Top = 40
        Width = 42
        Height = 13
        Caption = 'Sichtbar:'
      end
      object Label3: TLabel
        Left = 152
        Top = 40
        Width = 68
        Height = 13
        Caption = 'Ausgeblendet:'
      end
      object ddbToolbars: TComboBox
        Left = 56
        Top = 8
        Width = 233
        Height = 21
        AutoDropDown = True
        AutoCloseUp = True
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = ddbToolbarsChange
      end
      object tvSelectedItems: TTreeView
        Left = 0
        Top = 56
        Width = 145
        Height = 161
        Indent = 19
        ReadOnly = True
        ShowButtons = False
        ShowLines = False
        TabOrder = 1
        OnDblClick = tvSelectedItemsDblClick
      end
      object tvUnselectedItems: TTreeView
        Left = 144
        Top = 56
        Width = 145
        Height = 161
        Indent = 19
        ReadOnly = True
        ShowButtons = False
        ShowLines = False
        TabOrder = 2
        OnDblClick = tvUnselectedItemsDblClick
      end
      object cbToolbarSichtbar: TCheckBox
        Left = 0
        Top = 220
        Width = 137
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Toolbar sichtbar'
        TabOrder = 3
        OnClick = cbToolbarSichtbarClick
      end
      object cbIconOverCaption: TCheckBox
        Left = 144
        Top = 220
        Width = 137
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Text und Grafik anzeigen'
        TabOrder = 4
        OnClick = cbIconOverCaptionClick
      end
    end
    object tsDockbar: TTabSheet
      Caption = 'Dockbar-Optionen'
      ImageIndex = 1
      object Label4: TLabel
        Left = 8
        Top = 13
        Width = 44
        Height = 13
        Caption = 'Dockbar:'
      end
      object Label5: TLabel
        Left = 8
        Top = 101
        Width = 72
        Height = 13
        Caption = 'Grafik-Element:'
      end
      object imBitmap: TImage
        Left = 8
        Top = 128
        Width = 273
        Height = 105
        Stretch = True
      end
      object ddbDockbars: TComboBox
        Left = 56
        Top = 8
        Width = 225
        Height = 21
        AutoDropDown = True
        AutoCloseUp = True
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = ddbDockbarsChange
      end
      object cbDockLimitToOneRow: TCheckBox
        Left = 8
        Top = 40
        Width = 273
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Auf eine Reihe/Spalte beschr'#228'nken'
        TabOrder = 1
        OnClick = cbDockLimitToOneRowClick
      end
      object cbDockBmpOnToolbar: TCheckBox
        Left = 8
        Top = 72
        Width = 273
        Height = 18
        Alignment = taLeftJustify
        Caption = 'Grafik auch in Toolbar anzeigen'
        TabOrder = 3
        OnClick = cbDockBmpOnToolbarClick
      end
      object cbDockAllowDrag: TCheckBox
        Left = 8
        Top = 56
        Width = 273
        Height = 17
        Alignment = taLeftJustify
        Caption = 'An-/Abdocken von Elementen erlauben'
        TabOrder = 2
        OnClick = cbDockAllowDragClick
      end
      object ddbBackgrounds: TComboBox
        Left = 88
        Top = 96
        Width = 177
        Height = 21
        AutoDropDown = True
        AutoCloseUp = True
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 4
        OnChange = ddbBackgroundsChange
      end
      object btnLoadBMP: TButton
        Left = 264
        Top = 96
        Width = 17
        Height = 21
        Caption = '...'
        TabOrder = 5
        OnClick = btnLoadBMPClick
      end
    end
  end
  object btnClose: TButton
    Left = 232
    Top = 280
    Width = 73
    Height = 25
    Caption = 'Schliessen'
    Default = True
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object OpenPictureDialog1: TOpenPictureDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofDontAddToRecent]
    Title = 'Hintergrundgrafik ausw'#228'hlen'
    Left = 8
    Top = 280
  end
end
