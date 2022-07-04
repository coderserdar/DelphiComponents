object Form1: TForm1
  Left = 283
  Top = 170
  Width = 689
  Height = 482
  Caption = 'NCOCI8 - Filters'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  DesignSize = (
    681
    455)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 28
    Width = 681
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 242
    Width = 681
    Height = 213
    ActivePage = TabSheet3
    Align = alBottom
    TabIndex = 2
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Tree'
      object mm: TMemo
        Left = 32
        Top = 0
        Width = 641
        Height = 185
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'System'
        Font.Style = [fsBold]
        Lines.Strings = (
          'Enter filter expression, when:'
          '1) On page "Tree":'
          
            '    Uncomment: {*$DEFINE DEBUG} in NCOCIFilter.pas and recompile' +
            '.'
          
            '    Press left button and explore "Expression tree" in this memo' +
            '.'
          '2) On page "Find":'
          '    Press buttons <<, <, >, >> for locating valid records.'
          '3) On page "Locate"'
          '    Enter field name, field value and press "Locate" button'
          '    for measuring performance - 1000 locate'#39's.'
          ' ')
        ParentFont = False
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 32
        Height = 185
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        object SpeedButton1: TSpeedButton
          Left = 3
          Top = 4
          Width = 23
          Height = 22
          Glyph.Data = {
            66010000424D6601000000000000760000002800000014000000140000000100
            040000000000F000000000000000000000001000000010000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888800008888888888888888888800008888888888888888888800008888
            88888888777777780000888888888880000000780000888888888840FBFBF078
            0000888888888480000000880000888888884888888888880000887777748888
            77777778000080000007777000000078000080FFFF044440FBFBF07800008000
            0008788000000088000088888884878888888888000088888888487877777778
            0000888888888480000000780000888888888840FBFBF0780000888888888880
            0000008800008888888888888888888800008888888888888888888800008888
            88888888888888880000}
          OnClick = SpeedButton1Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Find'
      ImageIndex = 1
      object Button1: TButton
        Left = 8
        Top = 8
        Width = 33
        Height = 25
        Caption = '<<'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 48
        Top = 8
        Width = 33
        Height = 25
        Caption = '<'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 88
        Top = 8
        Width = 33
        Height = 25
        Caption = '>'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 128
        Top = 8
        Width = 33
        Height = 25
        Caption = '>>'
        TabOrder = 3
        OnClick = Button4Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Locate'
      ImageIndex = 2
      object Label1: TLabel
        Left = 16
        Top = 24
        Width = 37
        Height = 13
        Caption = 'Field 1 :'
      end
      object Label2: TLabel
        Left = 184
        Top = 24
        Width = 33
        Height = 13
        Caption = 'Value :'
      end
      object Label5: TLabel
        Left = 16
        Top = 48
        Width = 37
        Height = 13
        Caption = 'Field 2 :'
      end
      object Label6: TLabel
        Left = 184
        Top = 48
        Width = 33
        Height = 13
        Caption = 'Value :'
      end
      object Edit2: TEdit
        Left = 56
        Top = 21
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'SALESPERSON_ID'
      end
      object Edit3: TEdit
        Left = 221
        Top = 21
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '7521'
      end
      object Button5: TButton
        Left = 352
        Top = 20
        Width = 75
        Height = 25
        Caption = 'Locate'
        TabOrder = 2
        OnClick = Button5Click
      end
      object Edit4: TEdit
        Left = 56
        Top = 45
        Width = 121
        Height = 21
        TabOrder = 3
        Text = 'CUSTOMER_ID'
      end
      object Edit5: TEdit
        Left = 221
        Top = 45
        Width = 121
        Height = 21
        TabOrder = 4
        Text = '1*'
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Evalute'
      ImageIndex = 3
      object Label4: TLabel
        Left = 48
        Top = 16
        Width = 3
        Height = 13
      end
      object Button6: TButton
        Left = 8
        Top = 8
        Width = 33
        Height = 25
        Caption = '='
        TabOrder = 0
        OnClick = Button6Click
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 213
    Width = 681
    Height = 29
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      681
      29)
    object Label3: TLabel
      Left = 8
      Top = 8
      Width = 28
      Height = 13
      Caption = 'Filter :'
    end
    object Edit1: TEdit
      Left = 40
      Top = 4
      Width = 633
      Height = 21
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
    end
  end
  object DBNavigator1: TDBNavigator
    Left = 1
    Top = 1
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 3
  end
  object OCIDatabase1: TOCIDatabase
    InitModes = [dmThreaded, dmObject, dmEvents, dmShared]
    UserName = 'DEMO'
    Password = 'DEMO'
    Connected = True
    WaitCursor = crDefault
    Left = 8
    Top = 8
  end
  object OCIQuery1: TOCIQuery
    Active = True
    Prepared = True
    Filters = <
      item
        Name = 'NCSysDefault'
      end>
    SQL.Strings = (
      'select s.*, sysdate as dt from sales s')
    Left = 40
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = OCIQuery1
    Left = 80
    Top = 8
  end
end
