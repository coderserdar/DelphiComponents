object FormMain: TFormMain
  Left = 204
  Top = 126
  ActiveControl = btnLoad
  AutoScroll = False
  Caption = 'ATImageBox Demo'
  ClientHeight = 383
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 532
    Height = 165
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 8
      Top = 32
      Width = 241
      Height = 125
      Caption = ' Options '
      TabOrder = 1
      object Bevel1: TBevel
        Left = 8
        Top = 68
        Width = 225
        Height = 9
        Shape = bsTopLine
      end
      object chkFit: TCheckBox
        Left = 8
        Top = 16
        Width = 225
        Height = 17
        Caption = 'Fit image to window'
        TabOrder = 0
        OnClick = chkFitClick
      end
      object chkCenter: TCheckBox
        Left = 8
        Top = 48
        Width = 225
        Height = 17
        Caption = 'Center image in window'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = chkCenterClick
      end
      object chkFitOnlyBig: TCheckBox
        Left = 8
        Top = 32
        Width = 225
        Height = 17
        Caption = 'Fit only images larger than window'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = chkFitOnlyBigClick
      end
      object chkBorder: TCheckBox
        Left = 8
        Top = 72
        Width = 225
        Height = 17
        Caption = 'Show border'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = chkBorderClick
      end
      object chkLabel: TCheckBox
        Left = 8
        Top = 88
        Width = 225
        Height = 17
        Caption = 'Show info label'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = chkLabelClick
      end
      object chkDrag: TCheckBox
        Left = 8
        Top = 104
        Width = 225
        Height = 17
        Caption = 'Enable image dragging'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = chkDragClick
      end
    end
    object GroupBox2: TGroupBox
      Left = 256
      Top = 32
      Width = 268
      Height = 125
      Caption = ' Image scaling '
      TabOrder = 2
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 29
        Height = 13
        Caption = 'Scale:'
      end
      object LabelScale: TLabel
        Left = 8
        Top = 28
        Width = 29
        Height = 13
        Caption = '100%'
      end
      object labResampleDelay: TLabel
        Left = 26
        Top = 86
        Width = 51
        Height = 13
        Caption = 'Delay, ms:'
      end
      object TrackBar1: TTrackBar
        Left = 48
        Top = 16
        Width = 217
        Height = 25
        Max = 500
        Min = 1
        PageSize = 10
        Frequency = 50
        Position = 100
        TabOrder = 0
        ThumbLength = 16
        OnChange = TrackBar1Change
      end
      object chkKeepPos: TCheckBox
        Left = 8
        Top = 48
        Width = 250
        Height = 17
        Caption = 'Keep image position during scaling'
        TabOrder = 1
        OnClick = chkKeepPosClick
      end
      object chkResample: TCheckBox
        Left = 8
        Top = 68
        Width = 129
        Height = 17
        Caption = 'Resample on scaling'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = chkCenterClick
      end
      object edResampleDelay: TEdit
        Left = 88
        Top = 84
        Width = 33
        Height = 19
        AutoSize = False
        TabOrder = 3
        Text = '300'
        OnChange = chkCenterClick
      end
    end
    object btnLoad: TButton
      Left = 8
      Top = 6
      Width = 241
      Height = 23
      Caption = 'Load image...'
      TabOrder = 0
      OnClick = btnLoadClick
    end
  end
  object Box: TATImageBox
    Left = 0
    Top = 165
    Width = 532
    Height = 218
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Align = alClient
    Color = clGray
    ParentColor = False
    TabOrder = 1
    TabStop = True
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 256
  end
end
