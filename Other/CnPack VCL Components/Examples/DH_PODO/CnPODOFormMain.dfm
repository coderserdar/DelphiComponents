object frmPodoMain: TfrmPodoMain
  Left = 339
  Top = 171
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'CnPack - PODO ������'
  ClientHeight = 495
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblDB: TLabel
    Left = 8
    Top = 24
    Width = 57
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '���ݿ�'
  end
  object lblTblName: TLabel
    Left = 8
    Top = 51
    Width = 57
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '����'
  end
  object lblTblInfo: TLabel
    Left = 8
    Top = 79
    Width = 57
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '����Ϣ'
  end
  object lblPODO: TLabel
    Left = 8
    Top = 199
    Width = 57
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'PODO Ԥ��'
  end
  object edtDB: TEdit
    Left = 71
    Top = 21
    Width = 226
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object btnConn: TButton
    Left = 303
    Top = 21
    Width = 50
    Height = 21
    Caption = '����'
    TabOrder = 0
    OnClick = btnConnClick
  end
  object cbTableName: TComboBox
    Left = 71
    Top = 48
    Width = 282
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cbTableNameSelect
  end
  object lvInfo: TListView
    Left = 71
    Top = 75
    Width = 282
    Height = 110
    Columns = <
      item
        Caption = '�ֶ���'
        Width = 130
      end
      item
        Caption = '��������'
        Width = 130
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
  end
  object btnGenerate: TButton
    Left = 71
    Top = 455
    Width = 140
    Height = 21
    Caption = '���� PODO �ļ�'
    TabOrder = 5
    OnClick = btnGenerateClick
  end
  object btnClose: TButton
    Left = 288
    Top = 455
    Width = 65
    Height = 21
    Caption = '�ر�'
    TabOrder = 7
    OnClick = btnCloseClick
  end
  object btnHelp: TButton
    Left = 217
    Top = 455
    Width = 65
    Height = 21
    Caption = '����'
    TabOrder = 6
    OnClick = btnHelpClick
  end
  object mmPodo: TMemo
    Left = 72
    Top = 200
    Width = 281
    Height = 241
    TabOrder = 4
  end
  object conn: TADOConnection
    LoginPrompt = False
    Left = 144
    Top = 104
  end
  object table: TADOTable
    Connection = conn
    Left = 176
    Top = 104
  end
  object sdPODO: TSaveDialog
    DefaultExt = 'pas'
    Filter = 'delphi ��Ԫ�ļ�(*.pas)|*.pas'
    Title = '�����ļ�'
    Left = 240
    Top = 104
  end
end
