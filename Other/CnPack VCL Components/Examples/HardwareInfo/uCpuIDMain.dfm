object FrmCPUIDs: TFrmCPUIDs
  Left = 177
  Top = 201
  Width = 544
  Height = 375
  Caption = 'TCnCpuId Demo ��ȡ���CPUָ�������к��Լ�Ӳ�����к�'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '����'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object BtnGetCpuIDs: TButton
    Left = 16
    Top = 16
    Width = 121
    Height = 25
    Caption = '���CPU���к�'
    TabOrder = 1
    OnClick = BtnGetCpuIDsClick
  end
  object Memo1: TMemo
    Left = 150
    Top = 0
    Width = 386
    Height = 348
    Align = alRight
    ImeName = '���� (����) - ΢��ƴ��'
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnGetUsage: TButton
    Left = 16
    Top = 112
    Width = 121
    Height = 25
    Caption = '��� CPU ռ����'
    TabOrder = 4
    OnClick = btnGetUsageClick
  end
  object btnGetCpuOems: TButton
    Left = 16
    Top = 80
    Width = 121
    Height = 25
    Caption = '���CPU��������'
    TabOrder = 3
    OnClick = btnGetCpuOemsClick
  end
  object btnInfoStr: TButton
    Left = 16
    Top = 48
    Width = 121
    Height = 25
    Caption = '���CPU��Ϣ��'
    TabOrder = 2
    OnClick = btnInfoStrClick
  end
  object btnGetBios: TButton
    Left = 16
    Top = 176
    Width = 121
    Height = 21
    Caption = '��� BIOS ID'
    TabOrder = 6
    OnClick = btnGetBiosClick
  end
  object btnHardDiskSn: TButton
    Left = 16
    Top = 216
    Width = 121
    Height = 21
    Caption = '���Ӳ�����к�'
    TabOrder = 7
    OnClick = btnHardDiskSnClick
  end
  object btnVolumnInfos: TButton
    Left = 16
    Top = 246
    Width = 121
    Height = 21
    Caption = '��÷������'
    TabOrder = 8
    OnClick = btnVolumnInfosClick
  end
  object btnCPULogical: TButton
    Left = 16
    Top = 144
    Width = 121
    Height = 25
    Caption = '��� CPU �����Ϣ'
    TabOrder = 5
    OnClick = btnCPULogicalClick
  end
end
