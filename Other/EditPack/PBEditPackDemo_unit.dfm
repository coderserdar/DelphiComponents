object PBEditPackDemoForm: TPBEditPackDemoForm
  Left = 212
  Top = 105
  ActiveControl = PBBinHexEdit1
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PBEditpack demo'
  ClientHeight = 367
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object Label2: TLabel
    Left = 57
    Top = 169
    Width = 83
    Height = 16
    Caption = 'PBNumEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 146
    Top = 63
    Width = 101
    Height = 16
    Caption = 'PBBinHexEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 0
    Top = 158
    Width = 392
    Height = 2
    Shape = bsBottomLine
  end
  object Bevel3: TBevel
    Left = 0
    Top = 49
    Width = 392
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel1: TBevel
    Left = 0
    Top = 280
    Width = 392
    Height = 9
    Shape = bsBottomLine
  end
  object Label4: TLabel
    Left = 57
    Top = 198
    Width = 97
    Height = 16
    Caption = 'PBSuperSpin:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 192
    Top = 261
    Width = 137
    Height = 16
    Caption = 'Decimals /ImageIndex:'
  end
  object Label1: TLabel
    Left = 2
    Top = 128
    Width = 62
    Height = 16
    Caption = 'BinLength:'
  end
  object Label6: TLabel
    Left = 277
    Top = 128
    Width = 64
    Height = 16
    Caption = 'HexLength'
  end
  object PBNumEdit1: TPBNumEdit
    Left = 163
    Top = 165
    Width = 173
    Height = 24
    Alignment = taRightJustify
    Decimals = 1
    NumberFormat = Thousands
    TabOrder = 0
    Value = -123456789
  end
  object PBBinHexEdit1: TPBBinHexEdit
    Left = 2
    Top = 88
    Width = 388
    Height = 24
    Alignment = taCenter
    AsBin = '11100000111000001110000011100000'
    AsInteger = 3772834016
    AsHex = '$E0E0E0E0'
    BaseFormat = HexaDecimal
    TabOrder = 1
    Version = '8.10.00.00'
  end
  object Button1: TButton
    Left = 130
    Top = 3
    Width = 134
    Height = 22
    Caption = 'Toggle Alignment (all)'
    Default = True
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 127
    Top = 126
    Width = 138
    Height = 22
    Caption = 'Toggle BaseFormat'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 217
    Top = 228
    Width = 145
    Height = 22
    Caption = 'Toggle NumberFormat'
    TabOrder = 6
    OnClick = Button3Click
  end
  object CheckBox1: TCheckBox
    Left = 17
    Top = 5
    Width = 99
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Default button:'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = CheckBox1Click
  end
  object PBMaskEdit1: TPBMaskEdit
    Left = 151
    Top = 299
    Width = 90
    Height = 24
    TabOrder = 2
    Text = 'PBMaskEdit1'
    Alignment = taLeftJustify
  end
  object PBSpinEdit1: TPBSpinEdit
    Left = 285
    Top = 298
    Width = 90
    Height = 26
    Cursor = crDefault
    MaxLength = 2
    MaxValue = 57
    MinValue = 53
    TabOrder = 3
    Value = 55
    Alignment = taRightJustify
  end
  object CheckBox2: TCheckBox
    Left = 272
    Top = 5
    Width = 114
    Height = 17
    Caption = 'AutoSelect (all)'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = CheckBox2Click
  end
  object PBEdit1: TPBEdit
    Left = 16
    Top = 299
    Width = 90
    Height = 24
    Color = clBackground
    TabOrder = 9
    Text = 'PBEdit1'
    Alignment = taLeftJustify
    DisabledColor = clHighlight
  end
  object PBSuperSpin1: TPBSuperSpin
    Left = 163
    Top = 192
    Width = 173
    Height = 26
    Cursor = crDefault
    Alignment = taRightJustify
    Ctl3D = True
    Decimals = 1
    MaxValue = 10000
    MinValue = -1
    NumberFormat = Thousands
    ParentCtl3D = False
    TabOrder = 10
    Value = 0.5
    Increment = 1.5
    RoundValues = True
    Wrap = True
  end
  object DecimalEdit: TPBSuperSpin
    Left = 334
    Top = 256
    Width = 49
    Height = 26
    Cursor = crDefault
    Alignment = taRightJustify
    Decimals = 0
    MaxValue = 14
    MinValue = -1
    NumberFormat = Standard
    OnChange = SetDecimalsClick
    TabOrder = 12
    Value = 1
    Increment = 1
    RoundValues = False
    Wrap = True
  end
  object CancelButton: TButton
    Left = 158
    Top = 333
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 11
    OnClick = CancelButtonClick
  end
  object CheckBox3: TCheckBox
    Left = 272
    Top = 32
    Width = 97
    Height = 17
    Caption = 'Enabled (all)'
    Checked = True
    State = cbChecked
    TabOrder = 13
    OnClick = CheckBox3Click
  end
  object PBEditEx1: TPBEditEx
    Left = 287
    Top = 337
    Width = 100
    Height = 24
    TabOrder = 14
    Text = 'PBEditEx1'
    Alignment = taLeftJustify
    Images = ImageList1
    ImageIndex = 0
  end
  object PBSpinEdit2: TPBSpinEdit
    Left = 346
    Top = 123
    Width = 44
    Height = 26
    Cursor = crDefault
    MaxLength = 2
    MaxValue = 16
    MinValue = 1
    TabOrder = 15
    Value = 8
    OnChange = PBSpinEdit2Change
    Alignment = taCenter
  end
  object PBSpinEdit3: TPBSpinEdit
    Left = 66
    Top = 123
    Width = 44
    Height = 26
    Cursor = crDefault
    MaxLength = 2
    MaxValue = 64
    MinValue = 1
    TabOrder = 16
    Value = 32
    OnChange = PBSpinEdit3Change
    Alignment = taCenter
  end
  object Button4: TButton
    Left = 5
    Top = 228
    Width = 178
    Height = 22
    Caption = 'Change separators (local)'
    TabOrder = 17
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 5
    Top = 255
    Width = 178
    Height = 22
    Caption = 'Change separators (system)'
    TabOrder = 18
    OnClick = Button5Click
  end
  object ImageList1: TImageList
    Masked = False
    Left = 242
    Top = 332
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
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
      0000000000000000000000000000000000000000000000000000000000808000
      0000FFFF00BFFF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000808080BFFFFFFF00FFFFFFBFFFFF
      FF00FFFFFFBFFFFFFF00FFFFFFBFFFFFFF00FFFFFFBFFFFFFF00FFFFFF80C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000BFFFFF
      0000FF0000BFFF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000808080800000000080C0C0
      C00000000080C0C0C00000000080C0C0C00000000080C0C0C00000000080C0C0
      C000000000000000000000000000000000008080808080808000808080808080
      8000808080808080800080808080808080008080808080000000FFFF00BFFF00
      0000FF0000808080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080000000
      0000808080000000000080808000000000008080800000000000808080000000
      000080808080808080000000000000000000808080BFFFFFFF0000FFFF80C0C0
      C00000FFFF80C0C0C00000FFFF8080808000800000BFFFFF0000FF0000BFFF00
      0000008080808080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808080C0C0
      C000C0C0C080C0C0C000C0C0C080C0C0C000C0C0C080C0C0C000C0C0C0808080
      8000C0C0C080808080000000000000000000808080BFFFFFFF00C0C0C00000FF
      FF0080808000000000000000008080808000C0C0C0BFFF000000FF0000000080
      8000C0C0C0808080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000808080
      8000000000000000000000000000000000000000000000000000000000000000
      000080808080C0C0C0000000000000000000808080BFFFFFFF0000FFFF808080
      8000C0C0C0BFFFFF0000C0C0C00000000000808080000080800000808080C0C0
      C00000FFFF808080800000000000000000005F0100008342F7BFFB41F7BF041B
      02007C9EF8BF041B020074B16781CCDA6481F39EF8BF00000000FFFFF8BF68B3
      678174B1678164FE270100000000FFFFFFFF14FEFBBF6090F7BFFFFFFFFFECFF
      2701000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000808080
      8000FFFFFF80C0C0C000C0C0C080C0C0C000C0C0C080C0C0C000C0C0C0808080
      800000000080808080008080800000000000808080BFFFFFFF00808080BFFFFF
      FF00FFFF0080C0C0C000FFFF0080C0C0C0000000000000808000C0C0C00000FF
      FF00C0C0C080808080000000000000000000D0FE2701FB41F7BF9094FCBF25DD
      F7BF9094FCBFCCDA64810CDB6481B6DCF7BFBBDCF7BF20FF2701A0A969C10100
      0000FB41F7BF9094FCBF06DEF8BFE213F7BF5F01000036CFF7BFFB41F7BF9094
      FCBF513EF8BF0000000001000000010000000000000000000000000000000000
      000000000000000000009094FCBF000000000000000000000000000000808080
      8000FFFFFF000000000080808080808080008080808080808000C0C0C0808080
      800080808000000000000000000000000000808080BFFFFFFF00808080BFFFFF
      0000FFFFFFBFFFFF0000C0C0C0BFFFFF0000000000000080800000FFFF80C0C0
      C00000FFFF808080800000000000000000000000000000000000A4C26481F468
      450060916AC101000000FEA0F7BF17B3F7BF0000430000000000000000000000
      0000FB41F7BF75B1678100000000000000000CFE27016CB7F7BFFB41F7BF9094
      FCBF87B7F7BF9094FCBF0000000024FE270119B8F7BFCCDA6481FB41F7BF9094
      FCBF3FB8F7BF000000000100000060916AC10000000000000000000000808080
      8000FFFFFF000000000080800080808000008080008080808000C0C0C0808080
      800080808000000000000000000000000000808080BFFFFFFF00808080BFFFFF
      FF00FFFF00BFFFFFFF00FFFF0080C0C0C0000000008080808000C0C0C00000FF
      FF00C0C0C080808080000000000000000000FADA6481CCDA6481000000006091
      6AC18CD75D82CD6246005300450E68FD270192B2F7BFA4794500FEFFFFFF8CD7
      5D82FEFFFFFF000000008C1B5E82A01B5E82A47945000E0000433A5C57494E44
      4F57433A5C57494E444F57535C526563656E745C46696C652E69636F2E6C6E6B
      000044D9648158FD2701E46B4500000043000000000000000000000000808080
      8000FFFFFF0000000000FFFF0080808000008080008080808000C0C0C0808080
      800080808000000000000000000000000000808080BFFFFFFF00C0C0C0808080
      8000FFFFFFBFFFFF0000FFFFFF000000000080808080C0C0C00000FFFF80C0C0
      C00000FFFF808080800000000000000000000000F0000000DE8B0000CE8BC7C9
      901A0000971A000001000000483500000000570197000202EF00A057971A00F3
      EC8BC14C971A00000000CC35806A0000000000000000EF00E23A971A00000000
      0000A716168C6A2C3717971AD03600000000971A7E08020C328C65253717971A
      7F296701BCA10200A7167F29448C3E2037170000000000000000000000808080
      8000FFFFFF000000000000000000000000000000000000000000C0C0C0808080
      800080808000000000000000000000000000808080BFFFFFFF00FFFFFF80C0C0
      C00080808080808080000000008080808000FFFFFFBFFFFFFF00FFFFFFBFFFFF
      FF00FFFFFF808080800000000000000000007F210000971A000031E4FFFF3E0E
      9F17191C9717581EF37F0400000000000000181EF37FCE1CF37F0000F37F0300
      0000000000000100000003000000B8B1678100001E8B501901000000000031E4
      FFFF18011004781D0200000000000000000000000000FFFFFFFF00000100A716
      3E8B28039F1700000000FFFFFFFF000000000000000000000000000000808080
      8000FFFFFFBFFFFFFF00FFFFFFBFFFFFFF00FFFFFFBFFFFFFF00C0C0C0808080
      8000808080000000000000000000000000008080800000FFFF00C0C0C00000FF
      FF00C0C0C00000FFFF00C0C0C00000FFFF008080808080808000808080808080
      800080808080808080000000000000000000000000004D52554C69737400D8F9
      27016CB7F7BFFB41F7BF9094FCBF87B7F7BF9094FCBFFFFFFFFFE213F7BF5F01
      0000DF96F9BFFB41F7BF9094FCBF3FB8F7BF00000000A05768C160916AC128FA
      2701CD2BF9BFB70E448A67010000BDC8F7BF384C3A45FFFFFFFF2618F7BF0000
      00009094FCBF0241F9BFB819F7BF768A27010000000000000000000000000000
      000080808080C0C0C000C0C0C080C0C0C000C0C0C080C0C0C000C0C0C080C0C0
      C00080808000000000000000000000000000000000808080800000FFFF80C0C0
      C00000FFFF80C0C0C00000FFFF80808080000000000000000000000000000000
      000000000000000000000000000000000000187845000000000000000000D8F9
      2701A047866600000000D4F827010100000000000000433A5C57494E444F5753
      5C526563656E745C46696C652E692CF92701180000001C000000E46B450091A3
      F7BF00004300006C450018000000E01B5E82E40400000474B481000000000000
      000054F92701A1D0F7BF0474B481001C5E820000000000000000000000000000
      0000000000808080800080808080808080008080808080808000808080808080
      8000808080000000000000000000000000000000000000000000808080808080
      8000808080808080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000004B000000E86B45004B010000D4F7
      270114F4F7BF000043009C8245004B01000002000000ECF727015718F37F0000
      430000000000E86B45004B01000004F82701E4A781660CD0FD7FE86B45004B01
      00000CD0FD7F20F8270124CA8066E86B45004B01000060F827014B00000074FF
      2701087088660000000090678066087088660000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000500064811497678160916AC1E0F6
      2701E213F7BF5F01000075A1F7BF5904000001000000000000200CF727019882
      450000004300902302000EB3F7BF0000430017B3F7BF00004300010000000000
      00009C82450024000000ADA3F7BFE0250800FFFFFFFF400000000C905D820090
      5D8264F85D82400000000000000044D96481424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFF380808040000FFFE1FFFFFF40
      00078001FFFFFF4080030001FFFFFF40C0010001FFFFFF40C0010001FFFFFF40
      E0010001FFFFFF40E0010001FFFFFF40E0030001FFFFFF40E0030001FFFFFF40
      E003000100000000E003000100000000E003000300000000F00380FF00000000
      F807C1FF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end