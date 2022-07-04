unit fmConfiguration;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, unTranslation;

type
  TConfiguration = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    edMaxSize: TEdit;
    cbSplit: TCheckBox;
    cbCrypt: TCheckBox;
    cbCompress: TCheckBox;
    cbSolid: TCheckBox;
    TabSheet3: TTabSheet;
    cbShowEmpty: TCheckBox;
    rgMaxSize: TRadioGroup;
    cbReadOnly: TCheckBox;
    GroupBox1: TGroupBox;
    edBlockSize: TEdit;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    edReserveSpace: TEdit;
    Label2: TLabel;
    cbWriteSFXCode: TCheckBox;
    Label3: TLabel;
    cbComprLevel: TComboBox;
    Label5: TLabel;
    cbLanguage: TComboBox;
    cbShowTreeView: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rgMaxSizeClick(Sender: TObject);
    procedure edMaxSizeChange(Sender: TObject);
    procedure cbSolidClick(Sender: TObject);
  private
    { Déclarations privées }
    FSplit : Boolean;
    FMaxSize : Integer;
    FMaxSizeIdx : Integer;

    procedure WMTranslate( var Message : TMessage ); message WM_TRANSLATE;
  public
    { Déclarations publiques }
  end;

var
  Configuration: TConfiguration;

implementation

{$R *.DFM}

procedure TConfiguration.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  FSplit := False;
  FMaxSize := 0;
  FMaxSizeIdx := rgMaxSize.ItemIndex;
  edMaxSize.Text := IntToStr( FMaxSize );
  rgMaxSize.ItemIndex := FMaxSizeIdx;
  cbSplit.Checked := FSplit;
end;

procedure TConfiguration.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOk then
    begin
      FSplit := cbSplit.Checked;
      FMaxSize := StrToIntDef(edMaxSize.Text, 0);
      FMaxSizeIdx := rgMaxSize.ItemIndex;
    end
  else
    begin
      cbSplit.Checked := FSplit;
      edMaxSize.Text := IntToStr( FMaxSize );
      rgMaxSize.ItemIndex := FMaxSizeIdx;
    end;
end;

procedure TConfiguration.rgMaxSizeClick(Sender: TObject);
begin
  cbSplit.Checked := True;
end;

procedure TConfiguration.edMaxSizeChange(Sender: TObject);
begin
  cbSplit.Checked := True;
  rgMaxSize.ItemIndex := 2;
end;

procedure TConfiguration.cbSolidClick(Sender: TObject);
begin
  cbComprLevel.ItemIndex := 0;
end;

procedure TConfiguration.WMTranslate( var Message : TMessage );
begin
  rgMaxSize.Items.Text := GetStr(1006);
  cbComprLevel.Items.Text := GetStr(911);
  cbLanguage.Items.Text := GetStr(1018);
end;

end.
