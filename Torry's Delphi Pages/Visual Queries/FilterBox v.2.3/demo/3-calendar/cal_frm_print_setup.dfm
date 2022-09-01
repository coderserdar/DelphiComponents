object psc_frm_print_setup: Tpsc_frm_print_setup
  Left = 483
  Top = 165
  BorderStyle = bsDialog
  Caption = 'Print Setup'
  ClientHeight = 244
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel_PrintSetup: TPanel
    Left = 0
    Top = 0
    Width = 364
    Height = 244
    Align = alClient
    TabOrder = 0
    object GroupBox_PrintOptions: TGroupBox
      Left = 12
      Top = 8
      Width = 341
      Height = 193
      Caption = ' Print Options '
      TabOrder = 0
      object Label_Header: TLabel
        Left = 8
        Top = 27
        Width = 38
        Height = 13
        Caption = 'Header:'
      end
      object Label_Title: TLabel
        Left = 8
        Top = 60
        Width = 23
        Height = 13
        Caption = 'Title:'
      end
      object Label_Overlap: TLabel
        Left = 8
        Top = 87
        Width = 40
        Height = 13
        Caption = 'Overlap:'
      end
      object Edit_Header: TEdit
        Left = 56
        Top = 24
        Width = 97
        Height = 21
        TabOrder = 0
        OnChange = Edit_HeaderChange
      end
      object CheckBox_Header: TCheckBox
        Left = 176
        Top = 24
        Width = 145
        Height = 17
        Caption = 'Print Header'
        TabOrder = 2
        OnClick = CheckBox_HeaderClick
      end
      object CheckBox_ParentHeaderFont: TCheckBox
        Left = 176
        Top = 96
        Width = 145
        Height = 17
        Caption = 'Use Parent Header Font'
        TabOrder = 4
        OnClick = CheckBox_ParentHeaderFontClick
      end
      object Edit_Title: TEdit
        Left = 56
        Top = 56
        Width = 97
        Height = 21
        TabOrder = 1
        OnChange = Edit_TitleChange
      end
      object Button_HeaderFont: TButton
        Left = 8
        Top = 120
        Width = 75
        Height = 25
        Caption = 'Header Font'
        TabOrder = 5
        OnClick = Button_HeaderFontClick
      end
      object PSCEdit_Overlap: TPSCEdit
        Left = 56
        Top = 84
        Width = 57
        Height = 21
        BtnKind = bkUpDown
        ButtonsVisible = True
        TabOrder = 6
        OnButtonClick = PSCEdit_OverlapButtonClick
        OnChange = PSCEdit_OverlapChange
      end
      object ProgressBar_Printing: TProgressBar
        Left = 8
        Top = 152
        Width = 150
        Height = 19
        Step = 1
        TabOrder = 7
        Visible = False
      end
      object CheckBox_PageNumber: TCheckBox
        Left = 176
        Top = 72
        Width = 145
        Height = 17
        Caption = 'Print Page Number'
        TabOrder = 3
        OnClick = CheckBox_PageNumberClick
      end
      object CheckBox_ShowProgress: TCheckBox
        Left = 176
        Top = 48
        Width = 145
        Height = 17
        Caption = 'Show Progress'
        TabOrder = 8
        OnClick = CheckBox_ShowProgressClick
      end
    end
    object Panel_Buttons: TPanel
      Left = 1
      Top = 202
      Width = 362
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object BitBtn_Print: TBitBtn
        Left = 195
        Top = 6
        Width = 75
        Height = 28
        Caption = 'Print'
        Default = True
        TabOrder = 0
        OnClick = BitBtn_PrintClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000CE0E0000D80E00001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00222222222222
          22222200000000000222208888888880802200000000000008020888888BBB88
          0002088888877788080200000000000008800888888888808080200000000008
          0800220FFFFFFFF080802220F00000F000022220FFFFFFFF022222220F00000F
          022222220FFFFFFFF02222222000000000222222222222222222}
      end
      object Button_Cancel: TButton
        Left = 276
        Top = 6
        Width = 75
        Height = 28
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object PrintDialog_CalendarPrint: TPrintDialog
    Options = [poPrintToFile]
    Left = 328
    Top = 8
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 297
    Top = 9
  end
end
