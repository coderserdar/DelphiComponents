unit CardFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, ShellApi;

type
  TCardFormData = record
    Name: string;
    Nickname: string;
    Emails: string;
    HandTel: string;
    BP: string;
    Oicq: string;
    Icq: string;
    Sex: Integer;
    Birthday: TDate;
    UseBirth: Boolean;
    Constellation: Integer;
    BloodType: Integer;
    Taste: string;
    Homepage: string;
    HomeZip: string;
    HomeAddr: string;
    HomeTel1: string;
    HomeTel2: string;
    HomeFax: string;
    CorpZip: string;
    CorpAddr: string;
    CorpJob: string;
    CorpDept: string;
    CorpTel1: string;
    CorpTel2: string;
    CorpFax: string;
    CorpHomepage: string;
    ExtMemo: string;
  end;

  TCardForm = class(TForm)
    PageControl: TPageControl;
    NormalTabSheet: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    NameEdit: TEdit;
    GroupBox1: TGroupBox;
    EmailEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
  
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Label9: TLabel;
    ExtInfoMemo: TMemo;
    EmailListBox: TListBox;
    AddEmailButton: TButton;
    DeleteEmailButton: TButton;
    EditEmailButton: TButton;
    Label8: TLabel;
    CorpHomepageEdit: TEdit;
    Label10: TLabel;
    HandTelEdit: TEdit;
    Label11: TLabel;
    BPEdit: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    OicqEdit: TEdit;
    Label14: TLabel;
    IcqEdit: TEdit;
    Label15: TLabel;
    SexComboBox: TComboBox;
    Label16: TLabel;
    BirthdayDateTimePicker: TDateTimePicker;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    HomeZipEdit: TEdit;
    HomeAddrEdit: TEdit;
    Label5: TLabel;
    HomeFaxEdit: TEdit;
    Label7: TLabel;
    Label20: TLabel;
    CorpZipEdit: TEdit;
    CorpAddrEdit: TEdit;
    Label21: TLabel;
    CorpFaxEdit: TEdit;
    Label22: TLabel;
    HomepageEdit: TEdit;
    Label2: TLabel;
    Label6: TLabel;
    CorpTel1Edit: TEdit;
    CorpTel2Edit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    HomeTel1Edit: TEdit;
    HomeTel2Edit: TEdit;
    Label23: TLabel;
    CorpJobEdit: TEdit;
    Label24: TLabel;
    CorpDeptEdit: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    NicknameEdit: TEdit;
    ConComboBox: TComboBox;
    Label31: TLabel;
    BloodComboBox: TComboBox;
    TasteEdit: TEdit;
    MoveUpEmailButton: TButton;
    MoveDownEmailButton: TButton;
    HomepageSpeedButton: TSpeedButton;
    CorpHomepageSpeedButton: TSpeedButton;  procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure AddEmailButtonClick(Sender: TObject);
    procedure DeleteEmailButtonClick(Sender: TObject);
    procedure EditEmailButtonClick(Sender: TObject);
    procedure MoveUpEmailButtonClick(Sender: TObject);
    procedure MoveDownEmailButtonClick(Sender: TObject);
    procedure EmailEditKeyPress(Sender: TObject; var Key: Char);
    procedure HomepageSpeedButtonClick(Sender: TObject);
    procedure CorpHomepageSpeedButtonClick(Sender: TObject);
    procedure EmailListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
    function CheckValid: Boolean;
  public
    { Public declarations }
    procedure SetData(Value: TCardFormData);
    procedure GetData(var Value: TCardFormData);
  end;

var
  CardForm: TCardForm;

function ShowCardForm(var Value: TCardFormData): Boolean;

implementation

uses StrRes, InputFrm;

{$R *.DFM}

function ShowCardForm(var Value: TCardFormData): Boolean;
var
  Frm: TCardForm;
begin
  Frm := TCardForm.Create(Application);
  Frm.SetData(Value);
  Result := (Frm.ShowModal = mrOk);
  if Result then Frm.GetData(Value);
  Frm.Free;
end;

procedure TCardForm.GetData(var Value: TCardFormData);
begin
  with Value do
  begin
    Name := NameEdit.Text;
    Nickname := NicknameEdit.Text;
    Emails := EmailListBox.Items.CommaText;
    HandTel := HandTelEdit.Text;
    BP := BPEdit.Text;
    Oicq := OicqEdit.Text;
    Icq := IcqEdit.Text;
    Sex := SexComboBox.ItemIndex;
    Birthday := BirthdayDateTimePicker.Date;
    UseBirth := BirthdayDateTimePicker.Checked;
    Constellation := ConComboBox.ItemIndex;
    BloodType := BloodComboBox.ItemIndex;
    Taste := TasteEdit.Text;
    Homepage := HomepageEdit.Text;
    HomeZip := HomeZipEdit.Text;
    HomeAddr := HomeAddrEdit.Text;
    HomeTel1 := HomeTel1Edit.Text;
    HomeTel2 := HomeTel2Edit.Text;
    HomeFax := HomeFaxEdit.Text;
    CorpZip := CorpZipEdit.Text;
    CorpAddr := CorpAddrEdit.Text;
    CorpJob := CorpJobEdit.Text;
    CorpDept := CorpDeptEdit.Text;
    CorpTel1 := CorpTel1Edit.Text;
    CorpTel2 := CorpTel2Edit.Text;
    CorpFax := CorpFaxEdit.Text;
    CorpHomepage := CorpHomepageEdit.Text;
    ExtMemo := ExtInfoMemo.Lines.Text;
  end;
end;

procedure TCardForm.SetData(Value: TCardFormData);
begin
  with Value do
  begin
    NameEdit.Text := Name;
    NicknameEdit.Text := Nickname;
    EmailListBox.Items.CommaText := Emails;
    HandTelEdit.Text := HandTel;
    BPEdit.Text := BP;
    OicqEdit.Text := Oicq;
    IcqEdit.Text := Icq;
    SexComboBox.ItemIndex := Sex;
    BirthdayDateTimePicker.Date := Birthday;
    BirthdayDateTimePicker.Checked := UseBirth;
    ConComboBox.ItemIndex := Constellation;
    BloodComboBox.ItemIndex := BloodType;
    TasteEdit.Text := Taste;
    HomepageEdit.Text := Homepage;
    HomeZipEdit.Text := HomeZip;
    HomeAddrEdit.Text := HomeAddr;
    HomeTel1Edit.Text := HomeTel1;
    HomeTel2Edit.Text := HomeTel2;
    HomeFaxEdit.Text := HomeFax;
    CorpZipEdit.Text := CorpZip;
    CorpAddrEdit.Text := CorpAddr;
    CorpJobEdit.Text := CorpJob;
    CorpDeptEdit.Text := CorpDept;
    CorpTel1Edit.Text := CorpTel1;
    CorpTel2Edit.Text := CorpTel2;
    CorpFaxEdit.Text := CorpFax;
    CorpHomepageEdit.Text := CorpHomepage;
    ExtInfoMemo.Lines.Text := ExtMemo;
  end;
end;

function TCardForm.CheckValid: Boolean;
begin
  Result := True;
  if NameEdit.Text = '' then
  begin
    MessageBox(Handle, PChar(SNameIsEmpty), PChar(Application.Title), 48);
    PageControl.ActivePage := NormalTabSheet;
    NameEdit.SetFocus;
    Result := False;
  end;
end;

procedure TCardForm.OkButtonClick(Sender: TObject);
begin
  if CheckValid then
    ModalResult := mrOk;
end;

procedure TCardForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TCardForm.AddEmailButtonClick(Sender: TObject);
begin
  if EmailEdit.Text = '' then Exit;
  EmailListBox.Items.Add(EmailEdit.Text);
  EmailEdit.Text := '';
  EmailEdit.SetFocus;
end;

procedure TCardForm.DeleteEmailButtonClick(Sender: TObject);
begin
  if EmailListBox.ItemIndex = -1 then Exit;
  EmailListBox.Items.Delete(EmailListBox.ItemIndex);
end;

procedure TCardForm.EditEmailButtonClick(Sender: TObject);
var
  Item: string;
begin
  if EmailListBox.ItemIndex = -1 then Exit;
  Item := EmailListBox.Items[EmailListBox.ItemIndex];
  if ShowInputForm(Item, 'E-mail', 'Email:') then
  begin
    EmailListBox.Items[EmailListBox.ItemIndex] := Item;
  end;
end;

procedure TCardForm.MoveUpEmailButtonClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  I := EmailListBox.ItemIndex;
  if I <= 0 then Exit;
  S := EmailListBox.Items[I];
  EmailListBox.Items.Delete(I);
  Dec(I);
  EmailListBox.Items.Insert(I, S);
  EmailListBox.ItemIndex := I;
end;

procedure TCardForm.MoveDownEmailButtonClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  I := EmailListBox.ItemIndex;
  if I < 0 then Exit;
  if I >= EmailListBox.Items.Count - 1 then Exit;
  S := EmailListBox.Items[I];
  EmailListBox.Items.Delete(I);
  Inc(I);
  EmailListBox.Items.Insert(I, S);
  EmailListBox.ItemIndex := I;
end;

procedure TCardForm.EmailEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    AddEmailButtonClick(nil);
  end;
end;

procedure TCardForm.HomepageSpeedButtonClick(Sender: TObject);
begin
  if HomepageEdit.Text <> '' then
    ShellExecute(Self.Handle, 'Open', PChar(HomepageEdit.Text), '', '', 1);
end;

procedure TCardForm.CorpHomepageSpeedButtonClick(Sender: TObject);
begin
  if CorpHomepageEdit.Text <> '' then
    ShellExecute(Self.Handle, 'Open', PChar(CorpHomepageEdit.Text), '', '', 1);
end;

procedure TCardForm.EmailListBoxDblClick(Sender: TObject);
var
  Email: string;
begin
  if EmailListBox.ItemIndex = -1 then Exit;
  Email := EmailListBox.Items[EmailListBox.ItemIndex];
  ShellExecute(Handle, 'Open', PChar('mailto:' + Email), '', '', 1);
end;

end.
