object FormNewImage: TFormNewImage
  Left = 607
  Top = 108
  BorderStyle = bsDialog
  Caption = 
    'New Image Color Depth, Background Color, File Type and Dimension' +
    's'
  ClientHeight = 289
  ClientWidth = 474
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label2: TLabel
    Left = 10
    Top = 235
    Width = 228
    Height = 15
    Caption = '24-bit (16 million colors) White PNG 16 x 16'
  end
  object GroupBox1: TGroupBox
    Left = 247
    Top = 8
    Width = 219
    Height = 220
    Caption = 'Dimensions'
    TabOrder = 0
    object Label1: TLabel
      Left = 100
      Top = 172
      Width = 5
      Height = 15
      Caption = 'x'
    end
    object RadioButton1: TRadioButton
      Left = 18
      Top = 22
      Width = 113
      Height = 17
      Caption = '16 x 16'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 18
      Top = 42
      Width = 113
      Height = 17
      Caption = '32 x 32'
      TabOrder = 1
      OnClick = RadioButton2Click
    end
    object RadioButton3: TRadioButton
      Left = 18
      Top = 62
      Width = 113
      Height = 17
      Caption = '48 x 48'
      TabOrder = 2
      OnClick = RadioButton3Click
    end
    object RadioButton4: TRadioButton
      Left = 18
      Top = 82
      Width = 113
      Height = 17
      Caption = '64 x 64'
      TabOrder = 3
      OnClick = RadioButton4Click
    end
    object RadioButton5: TRadioButton
      Left = 18
      Top = 102
      Width = 113
      Height = 17
      Caption = '72 x 72'
      TabOrder = 4
      OnClick = RadioButton5Click
    end
    object RadioButton6: TRadioButton
      Left = 18
      Top = 122
      Width = 113
      Height = 17
      Caption = '128 x 128'
      TabOrder = 5
      OnClick = RadioButton6Click
    end
    object RadioButton7: TRadioButton
      Left = 18
      Top = 143
      Width = 113
      Height = 17
      Caption = 'Other'
      TabOrder = 6
      OnClick = RadioButton7Click
    end
    object RadioButton8: TRadioButton
      Left = 19
      Top = 193
      Width = 113
      Height = 17
      Caption = 'Screen Width'
      TabOrder = 7
      OnClick = RadioButton7Click
    end
    object Width1: TEdit
      Left = 20
      Top = 168
      Width = 47
      Height = 23
      TabOrder = 8
      Text = '1024'
    end
    object UpDown1: TUpDown
      Left = 67
      Top = 168
      Width = 17
      Height = 23
      Associate = Width1
      Max = 5000
      Position = 1024
      TabOrder = 9
      Thousands = False
    end
    object Height1: TEdit
      Left = 126
      Top = 169
      Width = 57
      Height = 23
      TabOrder = 10
      Text = '768'
    end
    object UpDown2: TUpDown
      Left = 183
      Top = 169
      Width = 17
      Height = 23
      Associate = Height1
      Max = 5000
      Position = 768
      TabOrder = 11
      Thousands = False
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 205
    Height = 73
    Caption = 'Color Depth'
    Columns = 2
    ItemIndex = 1
    Items.Strings = (
      'Win XP'
      'True Color'
      '256 Color'
      '16 Color'
      'Monochrome')
    TabOrder = 1
    OnClick = RadioGroup1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 257
    Width = 474
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    object OKBtn: TButton
      Left = 9
      Top = 4
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 90
      Top = 4
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 91
    Width = 129
    Height = 59
    Caption = 'Background Color'
    TabOrder = 3
    object Shape1: TShape
      Left = 9
      Top = 20
      Width = 71
      Height = 31
    end
    object Button1: TButton
      Left = 90
      Top = 27
      Width = 25
      Height = 25
      Caption = '...'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 159
    Width = 225
    Height = 56
    Caption = 'FileType'
    TabOrder = 4
    object FileType1: TComboBox
      Left = 9
      Top = 19
      Width = 204
      Height = 23
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 0
      Text = 'Windows BMP - BMP'
      OnChange = FileType1Change
      Items.Strings = (
        'Gif Image - GIF'
        'Jpeg bitmap - JPG'
        'Windows BMP - BMP'
        'Windows Icon - ICO'
        'Portable Network Graphics - PNG'
        'Windows Metafile - WMF'
        'Enhanced Windows Metafile - EMF')
    end
  end
  object ColorDialog1: TColorDialog
    Color = clTeal
    Left = 403
    Top = 247
  end
end
