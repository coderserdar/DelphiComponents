object FormImportField: TFormImportField
  Left = 300
  Top = 188
  BorderStyle = bsDialog
  Caption = 'Import Field ...'
  ClientHeight = 416
  ClientWidth = 492
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
  DesignSize = (
    492
    416)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 22
    Height = 13
    Caption = 'Unit:'
  end
  object Label5: TLabel
    Left = 240
    Top = 12
    Width = 28
    Height = 13
    Caption = 'Class:'
  end
  object cmbUnits: TComboBox
    Left = 40
    Top = 8
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnClick = cmbUnitsClick
  end
  object cmbClasses: TComboBox
    Left = 280
    Top = 8
    Width = 209
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnClick = cmbClassesClick
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 301
    Width = 486
    Height = 76
    Caption = 'Selected property'
    TabOrder = 2
    object Label2: TLabel
      Left = 12
      Top = 19
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object Label3: TLabel
      Left = 232
      Top = 19
      Width = 27
      Height = 13
      Caption = 'Type:'
    end
    object Label4: TLabel
      Left = 10
      Top = 44
      Width = 49
      Height = 13
      Caption = 'FieldType:'
    end
    object ComponentNameLabel: TLabel
      Left = 208
      Top = 43
      Width = 57
      Height = 13
      Caption = 'C&omponent:'
      FocusControl = edComponent
    end
    object stType: TStaticText
      Left = 272
      Top = 17
      Width = 201
      Height = 17
      AutoSize = False
      BorderStyle = sbsSingle
      Caption = ' '
      TabOrder = 0
    end
    object stName: TStaticText
      Left = 64
      Top = 17
      Width = 137
      Height = 17
      AutoSize = False
      BorderStyle = sbsSingle
      Caption = ' '
      TabOrder = 1
    end
    object cmbFieldType: TComboBox
      Left = 64
      Top = 40
      Width = 137
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnClick = cmbFieldTypeClick
    end
    object edComponent: TEdit
      Left = 272
      Top = 40
      Width = 201
      Height = 21
      TabOrder = 3
      OnExit = edComponentExit
    end
  end
  object gbProperties: TGroupBox
    Left = 4
    Top = 32
    Width = 486
    Height = 265
    Caption = 'Avaible properties'
    TabOrder = 3
    object lvProperties: TListView
      Left = 4
      Top = 16
      Width = 477
      Height = 242
      Columns = <
        item
          Caption = 'Name'
          MinWidth = 50
          Width = 100
        end
        item
          Caption = 'Type'
          Width = 80
        end
        item
          Caption = 'FieldType'
          Width = 100
        end
        item
          AutoSize = True
          Caption = 'Component'
        end>
      GridLines = True
      HideSelection = False
      MultiSelect = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lvPropertiesSelectItem
    end
  end
  object btnCancel: TButton
    Left = 415
    Top = 386
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnOK: TButton
    Left = 335
    Top = 386
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 5
  end
  object btnAboutBox: TButton
    Left = 8
    Top = 384
    Width = 75
    Height = 25
    Caption = 'About Box'
    TabOrder = 6
    OnClick = btnAboutBoxClick
  end
end
