object FormResizeResample: TFormResizeResample
  Left = 345
  Top = 295
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Resize'
  ClientHeight = 498
  ClientWidth = 640
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object OriginalWidth1: TcxLabel
    Left = 13
    Top = 239
    Caption = 'Original Width:'
  end
  object OriginalHeight1: TcxLabel
    Left = 13
    Top = 260
    Caption = 'Original Height:'
  end
  object NewHeight1: TcxLabel
    Left = 326
    Top = 260
    Caption = 'New Height:'
  end
  object NewWidth1: TcxLabel
    Left = 326
    Top = 239
    Caption = 'New Width:'
  end
  object Label4: TcxLabel
    Left = 326
    Top = 414
    Caption = 'Background'
  end
  object Label5: TcxLabel
    Left = 16
    Top = 12
    Caption = 'Original'
  end
  object Label6: TcxLabel
    Left = 326
    Top = 11
    Caption = 'Result'
  end
  object GroupBox2: TcxGroupBox
    Left = 13
    Top = 391
    Caption = 'Filter'
    TabOrder = 0
    Height = 63
    Width = 183
    object Label3: TLabel
      Left = 12
      Top = 27
      Width = 26
      Height = 15
      Caption = 'Filter'
    end
    object ComboBoxResampleFilter1: TcxComboBox
      Left = 45
      Top = 23
      Properties.DropDownListStyle = lsFixedList
      Properties.ImmediatePost = True
      Properties.Items.Strings = (
        'None'
        'Triangle'
        'Hermite'
        'Bell'
        'BSpline'
        'Lanczos3'
        'Mitchell')
      TabOrder = 0
      Text = 'None'
      Width = 124
    end
  end
  object GroupBox3: TcxGroupBox
    Left = 13
    Top = 281
    Caption = 'Size'
    TabOrder = 1
    Height = 104
    Width = 300
    object Bevel1: TBevel
      Left = 174
      Top = 26
      Width = 12
      Height = 50
      Shape = bsRightLine
    end
    object Bevel2: TBevel
      Left = 134
      Top = 26
      Width = 50
      Height = 21
      Shape = bsTopLine
    end
    object Bevel3: TBevel
      Left = 136
      Top = 58
      Width = 49
      Height = 19
      Shape = bsBottomLine
    end
    object Label1: TcxLabel
      Left = 12
      Top = 20
      Caption = 'Width (pixels)'
    end
    object Label2: TcxLabel
      Left = 12
      Top = 68
      Caption = 'Height (pixels)'
    end
    object AspectRatio1: TcxCheckBox
      Left = 174
      Top = 39
      Caption = 'Aspect ratio'
      State = cbsChecked
      TabOrder = 0
      Width = 92
    end
    object Width1: TcxSpinEdit
      Left = 94
      Top = 16
      Properties.ImmediatePost = True
      Properties.LargeIncrement = 4.000000000000000000
      Properties.MaxValue = 32767.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.SpinButtons.ShowFastButtons = True
      Properties.OnChange = cxSpinEdit1PropertiesChange
      TabOrder = 3
      Value = 1
      OnKeyUp = Width1KeyUp
      Width = 81
    end
    object Height1: TcxSpinEdit
      Left = 94
      Top = 64
      Properties.ImmediatePost = True
      Properties.LargeIncrement = 4.000000000000000000
      Properties.MaxValue = 32767.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.SpinButtons.ShowFastButtons = True
      Properties.OnChange = cxSpinEdit2PropertiesChange
      TabOrder = 4
      Value = 1
      OnKeyUp = Height1KeyUp
      Width = 81
    end
  end
  object ImageEnView1: TImageEnView
    Left = 13
    Top = 32
    Width = 300
    Height = 200
    Background = clWhite
    ParentCtl3D = False
    BackgroundStyle = iebsChessboard
    EnableInteractionHints = True
    TabOrder = 2
    OnMouseDown = ImageEnView1MouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 462
    Width = 640
    Height = 36
    Align = alBottom
    ParentBackground = False
    TabOrder = 3
    object Fit1: TcxButton
      Left = 171
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Fit'
      TabOrder = 4
      OnClick = Fit1Click
    end
    object Zoom1: TcxButton
      Left = 252
      Top = 6
      Width = 75
      Height = 25
      Caption = '100%'
      TabOrder = 5
      OnClick = Zoom1Click
    end
    object Button1: TcxButton
      Left = 474
      Top = 6
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TcxButton
      Left = 555
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Reset1: TcxButton
      Left = 9
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 2
      OnClick = Reset1Click
    end
    object Preview1: TcxButton
      Left = 90
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Preview'
      TabOrder = 3
      OnClick = Preview1Click
    end
  end
  object EnableAlphaChannel1: TcxCheckBox
    Left = 326
    Top = 391
    Caption = 'Enable Alpha Channel'
    Properties.ImmediatePost = True
    Properties.OnChange = EnableAlphaChannel1PropertiesChange
    State = cbsChecked
    TabOrder = 4
    Width = 140
  end
  object Background1: TcxComboBox
    Left = 402
    Top = 414
    Properties.DropDownListStyle = lsFixedList
    Properties.ImmediatePost = True
    Properties.Items.Strings = (
      'Solid'
      'Horizontal'
      'Vertical'
      'Foreward Diagonal'
      'Backward Diagonal'
      'Cross'
      'Diagonal Cross'
      'Chessboard'
      'Diagonals'
      'Cropped'
      'CropShadow '
      'Gradient'
      'Soft Shadow'
      'Photo Like ')
    TabOrder = 5
    Text = 'Chessboard'
    Width = 145
  end
  object RadioGroup1: TcxRadioGroup
    Left = 326
    Top = 281
    Caption = 'Other'
    Properties.Columns = 3
    Properties.ImmediatePost = True
    Properties.Items = <
      item
        Caption = '8 x 8'
      end
      item
        Caption = '12 x 12'
      end
      item
        Caption = '16 x 16'
      end
      item
        Caption = '24 x 24'
      end
      item
        Caption = '28 x 28'
      end
      item
        Caption = '32 x 32'
      end
      item
        Caption = '48 x 48'
      end
      item
        Caption = '64 x 64'
      end
      item
        Caption = '72 x 72'
      end
      item
        Caption = '128 x 128'
      end
      item
        Caption = '160 x 120'
      end
      item
        Caption = '320 x 240'
      end
      item
        Caption = '640 x 480'
      end
      item
        Caption = '1024 x 768'
      end
      item
        Caption = '2048 x 1536'
      end>
    Properties.OnChange = RadioGroup1PropertiesChange
    TabOrder = 6
    Height = 104
    Width = 300
  end
  object ImageEnView2: TImageEnView
    Left = 326
    Top = 32
    Width = 300
    Height = 200
    Background = clWhite
    ParentCtl3D = False
    BackgroundStyle = iebsChessboard
    EnableInteractionHints = True
    TabOrder = 7
    OnMouseDown = ImageEnView1MouseDown
  end
  object LockPreview1: TcxCheckBox
    Left = 531
    Top = 239
    Caption = 'Lock Preview'
    Properties.ImmediatePost = True
    Properties.OnChange = LockPreview1PropertiesChange
    State = cbsChecked
    TabOrder = 8
    OnKeyUp = LockPreview1KeyUp
    Width = 95
  end
  object ImageEnProc1: TImageEnProc
    AttachedImageEn = ImageEnView2
    Background = clWhite
    PreviewsParams = [prppShowResetButton, prppHardReset]
    PreviewFont.Charset = DEFAULT_CHARSET
    PreviewFont.Color = clWindowText
    PreviewFont.Height = -11
    PreviewFont.Name = 'MS Sans Serif'
    PreviewFont.Style = []
    Left = 129
    Top = 242
  end
  object ImageEnIO1: TImageEnIO
    AttachedImageEn = ImageEnView2
    Background = clWhite
    PreviewFont.Charset = DEFAULT_CHARSET
    PreviewFont.Color = clWindowText
    PreviewFont.Height = -11
    PreviewFont.Name = 'MS Sans Serif'
    PreviewFont.Style = []
    Left = 157
    Top = 242
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Kind = lfFlat
    NativeStyle = False
    Left = 190
    Top = 242
  end
end
