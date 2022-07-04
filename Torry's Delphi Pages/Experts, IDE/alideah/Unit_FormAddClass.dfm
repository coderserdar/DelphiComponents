object FormAddClass: TFormAddClass
  Left = 396
  Top = 183
  BorderStyle = bsDialog
  Caption = 'Add Class'
  ClientHeight = 207
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 10
    Top = 7
    Width = 56
    Height = 13
    Caption = 'Class &Name'
    FocusControl = CBoxClassName
  end
  object Label3: TLabel
    Left = 10
    Top = 62
    Width = 57
    Height = 13
    Caption = 'Shrink Type'
  end
  object Label1: TLabel
    Left = 10
    Top = 38
    Width = 51
    Height = 13
    Caption = 'Lock Type'
  end
  object Label4: TLabel
    Left = 12
    Top = 140
    Width = 329
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'Note: Enter a classname or select a template from the pulldown m' +
      'enu.'
  end
  object Label5: TLabel
    Left = 41
    Top = 156
    Width = 324
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'Alternatively, drag this          icon to any unknown classname ' +
      'window'
  end
  object Label8: TLabel
    Left = 10
    Top = 113
    Width = 83
    Height = 13
    Caption = 'Caption OnShrink'
  end
  object Label10: TLabel
    Left = 41
    Top = 172
    Width = 98
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'to get it'#39's classname.'
  end
  object Label7: TLabel
    Left = 10
    Top = 87
    Width = 42
    Height = 13
    Caption = 'Direction'
  end
  object CBoxClassName: TComboBox
    Left = 83
    Top = 5
    Width = 312
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = CBoxClassNameChange
    OnDropDown = CBoxClassNameDropDown
  end
  object ButtonOK: TButton
    Left = 229
    Top = 178
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 7
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 315
    Top = 178
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object CBoxShrinkType: TComboBox
    Left = 100
    Top = 58
    Width = 108
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      'Caption'
      'Short Caption'
      'Ultra Thin')
  end
  object PanelCapture: TPanel
    Left = 149
    Top = 153
    Width = 21
    Height = 21
    Anchors = [akLeft, akBottom]
    BevelOuter = bvLowered
    Color = clWindow
    TabOrder = 5
    object ImageCapture: TImage
      Left = 1
      Top = 1
      Width = 19
      Height = 19
      Cursor = crHandPoint
      Align = alClient
      Center = True
      Transparent = True
      OnMouseDown = ImageCaptureMouseDown
      OnMouseMove = ImageCaptureMouseMove
      OnMouseUp = ImageCaptureMouseUp
    end
  end
  object CBoxLockType: TComboBox
    Left = 100
    Top = 33
    Width = 108
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    Items.Strings = (
      'Unlock'
      'Lock - Expand'
      'Lock - Shrink')
  end
  object GroupBox1: TGroupBox
    Left = 216
    Top = 28
    Width = 178
    Height = 73
    Anchors = [akTop, akRight]
    Caption = 'Ultra Thin Properties '
    TabOrder = 6
    object Label6: TLabel
      Left = 11
      Top = 21
      Width = 28
      Height = 13
      Caption = 'Width'
    end
    object Label9: TLabel
      Left = 11
      Top = 47
      Width = 44
      Height = 13
      Caption = 'OnShrink'
    end
    object CBoxUTOnShrink: TComboBox
      Left = 62
      Top = 44
      Width = 108
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'None'
        'BringToTop'
        'BringToTopMost')
    end
    object EditUTWidth: TEdit
      Left = 62
      Top = 19
      Width = 43
      Height = 21
      TabOrder = 0
      Text = '1'
      OnExit = EditUTWidthExit
      OnKeyPress = EditUTWidthKeyPress
    end
    object UpDownUTWidth: TUpDown
      Left = 105
      Top = 19
      Width = 13
      Height = 21
      Associate = EditUTWidth
      Min = 1
      Position = 1
      TabOrder = 2
      Thousands = False
      Wrap = False
    end
  end
  object CBoxCaptionOnShrink: TComboBox
    Left = 100
    Top = 108
    Width = 108
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'None'
      'BringToTop'
      'BringToTopMost')
  end
  object CBoxDirection: TComboBox
    Left = 100
    Top = 83
    Width = 108
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    Items.Strings = (
      'Top'
      'Left'
      'Bottom'
      'Right')
  end
end
