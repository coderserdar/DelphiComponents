object fmSnapinWizard: TfmSnapinWizard
  Left = 341
  Top = 225
  ActiveControl = PageControl1
  BorderStyle = bsDialog
  Caption = 'fmSnapinWizard'
  ClientHeight = 338
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 287
    Width = 503
    Height = 51
    Align = alBottom
    TabOrder = 1
    object btnClose: TButton
      Left = 400
      Top = 8
      Width = 75
      Height = 25
      Action = actClose
      TabOrder = 2
    end
    object btnBack: TButton
      Left = 240
      Top = 8
      Width = 75
      Height = 25
      Action = actBack
      TabOrder = 0
    end
    object btnNext: TButton
      Left = 314
      Top = 8
      Width = 75
      Height = 25
      Action = actNext
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 129
    Height = 287
    Align = alLeft
    TabOrder = 2
    object Image1: TImage
      Left = 32
      Top = 32
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        055449636F6E0000010002001010100001000400280100002600000020201000
        01000400E80200004E0100002800000010000000200000000100040000000000
        C000000000000000000000001000000000000000000000000000800000800000
        00808000800000008000800080800000C0C0C000808080000000FF0000FF0000
        00FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000000000000003BB30
        000000000003BB30787877000003BB30F9F5F7000003BB30F9F5F7000003FB30
        F9F5F7000003FF70FFF5F7000000330FFFFFF70000000F044474740000000708
        88888880000000000000000000008880000000008700777800F000008F87F777
        8F0000008F88FFFFF00000008800888800000000F1FFFFFFE001FFFFE001FFFF
        E001FFFFE001FFFFE001FFFFE001FFFFF001FFFFF001FFFFF001FFFFF1FFFFFF
        10CFFFFF000FFFFF001FFFFF003FFFFF307FFFFF280000002000000040000000
        0100040000000000800200000000000000000000100000000000000000000000
        000080000080000000808000800000008000800080800000C0C0C00080808000
        0000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000
        0000000000000000000000000000000000000000000000000000000000000000
        003BF380000000000000000000000000003FB830000000000000000000000000
        003BF380000000000000000000000000003FB830000000000000000000000000
        003BF380000000000000000000000008883FB830888888888800000000000008
        7F3BF380F99FF55FF8000000000000087F3FB830F99FF55FF800000000000008
        7F3BF380F99FF55FF8000000000000087F3FB830F99FF55FF800000000000008
        7F3BF380F99FF55FF8000000000000087F3FB830F99FF55FF800000000000008
        7F3BF380FFFFF55FF8000000000000087F3FB830FFFFF55FF800000000000008
        7F3BF380FFFFFFFFF800000000000008783FBFB0888888888800000000000008
        74C33334C4C4C4C4C4000000000000087C48770C4C4C4CF0F000000000000008
        7778770777777777770000000000000888887708888888888880000000000000
        0008770000000000000000000000000000008800000000000000000000000000
        0000000000000000000000000000800008888880000000000000000000008880
        0877777800000000000000000000877887777777800070000000000000008F77
        8F777777787700000000000000008FF88FF77777777000000000000000008880
        08FFFFFFF88000000000000000000000008888888000000000000000FFFFFFFF
        FFE1FFFFFFC0FFFFFFC0FFFFFFC0FFFFFFC0FFFFFE00001FFE00001FFE00001F
        FE00001FFE00001FFE00001FFE00001FFE00001FFE00001FFE00001FFE00001F
        FE00001FFE00001FFE00001FFE00001FFE00001FFFE1FFFFFFE1FFFFFFC0FFFF
        F1807BFFF00073FFF00003FFF00007FFF0000FFFF1801FFFFFC07FFF}
    end
  end
  object PageControl1: TPageControl
    Left = 129
    Top = 0
    Width = 374
    Height = 287
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      TabVisible = False
      object StaticText3: TStaticText
        Left = 12
        Top = 24
        Width = 180
        Height = 17
        Caption = 'Type in a name for your MMC snapin.'
        TabOrder = 0
      end
      object edSnapinName: TEdit
        Left = 12
        Top = 56
        Width = 313
        Height = 21
        TabOrder = 1
      end
      object StaticText4: TStaticText
        Left = 12
        Top = 120
        Width = 309
        Height = 145
        AutoSize = False
        Caption = 
          'This name will appear in MMC'#39's list of available snapins.  You c' +
          'an change this name later by modifying the ProductName key in th' +
          'e snapin project'#39's Version Information'
        TabOrder = 2
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      TabVisible = False
      object StaticText1: TStaticText
        Left = 12
        Top = 24
        Width = 205
        Height = 17
        Caption = 'Type in a description for your MMC snapin.'
        TabOrder = 0
      end
      object edSnapinDescription: TEdit
        Left = 12
        Top = 56
        Width = 313
        Height = 21
        TabOrder = 1
      end
      object StaticText2: TStaticText
        Left = 12
        Top = 120
        Width = 309
        Height = 145
        AutoSize = False
        Caption = 
          'This description will appear in the Description box in MMC'#39's Add' +
          ' Snapin dialog.   You can change this entry later by modifying t' +
          'he FileDescription key in the snapin project'#39's Version Informati' +
          'on'
        TabOrder = 2
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
      TabVisible = False
      object StaticText5: TStaticText
        Left = 12
        Top = 24
        Width = 137
        Height = 17
        Caption = 'Type in your company name'
        TabOrder = 0
      end
      object edSnapinProvider: TEdit
        Left = 12
        Top = 56
        Width = 313
        Height = 21
        TabOrder = 1
      end
      object StaticText6: TStaticText
        Left = 12
        Top = 120
        Width = 309
        Height = 145
        AutoSize = False
        Caption = 
          'The company name will appear in the Vendor column of MMC'#39's list ' +
          'of available snapins.  You can change this entry later by modify' +
          'ing the CompanyName key in the snapin project'#39's Version Informat' +
          'ion'
        TabOrder = 2
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'TabSheet4'
      ImageIndex = 3
      TabVisible = False
      object StaticText7: TStaticText
        Left = 12
        Top = 24
        Width = 201
        Height = 17
        Caption = 'Type in the name of the static scope item.'
        TabOrder = 0
      end
      object edStaticScopeItem: TEdit
        Left = 12
        Top = 56
        Width = 313
        Height = 21
        TabOrder = 1
      end
      object StaticText8: TStaticText
        Left = 12
        Top = 120
        Width = 309
        Height = 145
        AutoSize = False
        Caption = 
          'This name will appear in MMC'#39's list of installed snapins, and as' +
          ' the scope pane'#39's root node for the snapin.  You can change this' +
          ' name later by modifying the '#39'Text'#39' property of the snapin data'#39 +
          's static scope item.'
        TabOrder = 2
      end
    end
  end
  object ActionList1: TActionList
    Left = 16
    Top = 295
    object actBack: TAction
      Caption = '<< &Back'
      OnExecute = actBackExecute
    end
    object actNext: TAction
      Caption = '&Next >>'
      OnExecute = actNextExecute
    end
    object actClose: TAction
      Caption = '&Close'
      OnExecute = actCloseExecute
    end
  end
end
