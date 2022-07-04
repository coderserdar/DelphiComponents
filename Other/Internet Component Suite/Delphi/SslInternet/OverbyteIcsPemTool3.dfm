object frmPemTool3: TfrmPemTool3
  Left = 331
  Top = 226
  BorderStyle = bsDialog
  Caption = 'Create self-signed certificate'
  ClientHeight = 397
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  DesignSize = (
    374
    397)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxCertCreate: TGroupBox
    Left = 6
    Top = 4
    Width = 361
    Height = 355
    Caption = ' Certificate Properties '
    TabOrder = 0
    object lbCountry: TLabel
      Left = 28
      Top = 38
      Width = 71
      Height = 13
      Caption = 'Country Code:'
    end
    object lbState: TLabel
      Left = 28
      Top = 65
      Width = 30
      Height = 13
      Caption = 'State:'
    end
    object lbLocality: TLabel
      Left = 28
      Top = 92
      Width = 40
      Height = 13
      Caption = 'Locality:'
    end
    object lbOrganization: TLabel
      Left = 28
      Top = 120
      Width = 65
      Height = 13
      Caption = 'Organization:'
    end
    object lbOrganizationalUnit: TLabel
      Left = 28
      Top = 147
      Width = 95
      Height = 13
      Caption = 'Organizational Unit:'
    end
    object lbCommonName: TLabel
      Left = 28
      Top = 175
      Width = 75
      Height = 13
      Caption = 'Common Name:'
    end
    object lbEMail: TLabel
      Left = 28
      Top = 202
      Width = 73
      Height = 13
      Caption = 'E-Mail address:'
    end
    object lbBits: TLabel
      Left = 28
      Top = 230
      Width = 21
      Height = 13
      Caption = 'Bits:'
    end
    object lbInfo: TLabel
      Left = 68
      Top = 316
      Width = 265
      Height = 26
      Caption = 
        'Certificate and private key are written to the same file.'#13#10'Priva' +
        'te key will not be password protected. '
    end
    object lbDays: TLabel
      Left = 28
      Top = 272
      Width = 28
      Height = 13
      Caption = 'Days:'
    end
    object EditCountry: TEdit
      Left = 126
      Top = 36
      Width = 31
      Height = 21
      TabOrder = 0
      Text = 'DE'
    end
    object EditState: TEdit
      Left = 126
      Top = 63
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'Germany'
    end
    object EditLocality: TEdit
      Left = 126
      Top = 90
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'Berlin'
    end
    object EditOrganization: TEdit
      Left = 126
      Top = 117
      Width = 121
      Height = 21
      TabOrder = 3
      Text = 'CryptoMania Inc.'
    end
    object EditOrganizationalUnit: TEdit
      Left = 126
      Top = 144
      Width = 121
      Height = 21
      TabOrder = 4
      Text = 'CryptoMania Inc.'
    end
    object EditCommonName: TEdit
      Left = 126
      Top = 171
      Width = 121
      Height = 21
      TabOrder = 5
      Text = 'www.domain.de'
    end
    object EditEMail: TEdit
      Left = 126
      Top = 198
      Width = 121
      Height = 21
      TabOrder = 6
      Text = 'certs@cryptomania.de'
    end
    object EditBits: TEdit
      Left = 126
      Top = 225
      Width = 35
      Height = 21
      MaxLength = 4
      TabOrder = 7
      Text = '1024'
    end
    object CheckBoxCA: TCheckBox
      Left = 26
      Top = 250
      Width = 113
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Is CA:'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object Editdays: TEdit
      Left = 126
      Top = 270
      Width = 121
      Height = 21
      TabOrder = 9
      Text = '365'
    end
  end
  object btnCreate: TButton
    Left = 109
    Top = 368
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Create'
    TabOrder = 1
    OnClick = btnCreateClick
  end
  object btnClose: TButton
    Left = 193
    Top = 368
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 2
  end
end
