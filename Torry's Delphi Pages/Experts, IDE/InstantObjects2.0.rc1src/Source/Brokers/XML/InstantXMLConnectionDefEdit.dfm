object InstantXMLConnectionDefEditForm: TInstantXMLConnectionDefEditForm
  Left = 425
  Top = 292
  Width = 302
  Height = 190
  Caption = 'XML Connection'
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
  object BottomBevel: TBevel
    Left = 0
    Top = 133
    Width = 294
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 294
    Height = 133
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object XMLLabel: TLabel
      Left = 8
      Top = 11
      Width = 110
      Height = 13
      Caption = '&XML data root directory'
      FocusControl = RootDirEdit
    end
    object Label1: TLabel
      Left = 8
      Top = 83
      Width = 69
      Height = 13
      Caption = 'XML &encoding'
      FocusControl = RootDirEdit
    end
    object RootDirEdit: TEdit
      Left = 8
      Top = 26
      Width = 251
      Height = 21
      TabOrder = 0
    end
    object FolderButton: TButton
      Left = 260
      Top = 26
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = FolderButtonClick
    end
    object cbVersioning: TCheckBox
      Left = 8
      Top = 60
      Width = 149
      Height = 17
      Caption = 'Enable file versioning'
      TabOrder = 2
    end
    object cbEncoding: TComboBox
      Left = 8
      Top = 98
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 135
    Width = 294
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonsPanel: TPanel
      Left = 121
      Top = 0
      Width = 173
      Height = 28
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object OkButton: TButton
        Left = 5
        Top = 2
        Width = 80
        Height = 24
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object CancelButton: TButton
        Left = 88
        Top = 2
        Width = 80
        Height = 24
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
end
