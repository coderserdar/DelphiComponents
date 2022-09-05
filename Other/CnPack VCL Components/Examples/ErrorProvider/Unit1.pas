unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  CnErrorProvider, ExtDlgs;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    er_test: TCnErrorProvider;
    OpenPictureDialog1: TOpenPictureDialog;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ed_name: TEdit;
    ed_pw: TEdit;
    ed_rpw: TEdit;
    ed_email: TEdit;
    Memo1: TMemo;
    ed_age: TEdit;
    Button1: TButton;
    ed_img: TEdit;
    Button2: TButton;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    ComboBox1: TComboBox;
    Button3: TButton;
    ed_tip: TEdit;
    ComboBox2: TComboBox;
    ed_test: TMemo;
    er_demo:  TCnErrorProvider;
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  Private
    procedure OnInputCheck(Sender: TObject);
    procedure OnEnterDispose(Sender: TObject);
  Public
    procedure ErrorClick(Sender: TObject;eObject: TControl);
    procedure OnFilterError(Sender: TObject;Control: TControl;
      var ES: TErrorStyle;var result: Boolean);
  end;

var
  Form1: TForm1;
implementation

{$R *.dfm}

procedure TForm1.ErrorClick(Sender: TObject;eObject: TControl);
begin
  ShowMessage(TCnErrorProviderItem(Sender).Title + #13#10 + '��������ǣ�' + eObject.Name);
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  if (edit1.Text <> '') then
  begin
    try
      (StrToInt64(edit1.Text));
      er_test.Dispose(edit1);
    except
      er_test.SetError(edit1, 'Ӧ������������.'#13#10'����������.',iaMiddleRight,bsBlinkIfDifferentError);
      //edit1.Text := '';//���ж����룬����ʹ�ÿɸ���ʵ����;
      MessageBeep(0);
    end;
  end;
end;

procedure TForm1.Edit2Change(Sender: TObject);
var
  i: Integer;
begin
  if Edit2.Text <> '' then
  begin
    for i := 1 to Length(Edit2.Text) do
      if (Ord(Edit2.Text[i]) in [48..57]) then
      begin
        er_test.SetError(Edit2, 'Ӧ�����ַ�.'#13#10'����������.',iaMiddleRight,bsBlinkIfDifferentError);
        // Edit2.Text := '';
        MessageBeep(0);
        Exit;
      end;
    er_test.Dispose(Edit2);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ComboBox1.ItemIndex := 0;
  ComboBox2.ItemIndex := 0;
  
  er_demo.DoubleBuffer := True;
  er_demo.OnClick := ErrorClick;
  er_test.OnClick := ErrorClick;
  er_demo.OnSetError := OnFilterError;
  for i := 0 to ComponentCount - 1 do
    if (Components[i].ClassName = 'TEdit') and (Components[i].GetParentComponent.Name = 'GroupBox2') then
    begin
      if (Components[i].Name = 'ed_email') then
        (Components[i] as TEdit).OnChange := OnInputCheck
      else
      begin
        (Components[i] as TEdit).OnExit := OnInputCheck;
        (Components[i] as TEdit).OnEnter := OnEnterDispose;
      end;
    end;
  Memo1.OnExit := OnInputCheck;
  Memo1.OnEnter := OnEnterDispose;
end;

procedure TForm1.OnInputCheck(Sender: TObject);
begin
  er_demo.SetError(Sender as TControl);
end;

procedure TForm1.OnFilterError(Sender: TObject;Control: TControl;
  var ES: TErrorStyle;var result: Boolean);
var
  age: Integer;
begin
  if (Control is TEdit) then
  begin
    with Control as TEdit do
    begin
      result := not (Text = '');
      if (not result) then Exit;
      if (Control.Name = ed_name.Name) then
      begin
        result := CompareText('User',Text) <> 0;
        ES.Title := '�û��Ѵ���(����ȡ"User")';
      end
      else
        if (Control.Name = ed_pw.Name) then
        begin
          result := Length(TEdit(Control).Text) < 6;
          ES.Title := '����ǿ�Ȳ���';
          ES.Icon := EP_WARNING2;
          if (ed_pw.Text <> '') then ed_rpw.OnExit(ed_rpw);
        end
        else
          if (Control.Name = ed_rpw.Name) then
          begin
            result := Text <> ed_pw.Text;
            ES.Title := '�����������벻һ��';
            ES.Icon := EP_WARNING;
          end
          else
            if (Control.Name = ed_email.Name) then
            begin
              result := Pos('@',Text) * Pos('.',Text) = 0;
              if (result) then
              begin
                ES.Title := '���������Email��ַ����ȷ';
                ES.Hint := 'ȱ��@��.����';
                ES.Icon := EP_ERROR;
              end
              else
              begin
                ES.Hint := '��Ч';
                ES.Title := '';
                ES.Icon := EP_OK;
              end;
              ES.BlinkStyle := bsNeverBlink;

              result := True;
            end
            else
              if (Control.Name = ed_age.Name) then
              begin
                age := StrToIntDef(Text,0);
                result := not (age in [18..99]);
                if (age = 0) then
                  ES.Title := '�����������Ҳ�Ϊ0'
                else
                  ES.Title := '�㻹δ�����ұ���̳���ʺ�������';
              end
              else
                if (Control.Name = ed_img.Name) then
                begin
                  result := not FileExists(Text);
                  ES.Title := '�ļ�������';
                  ES.Icon := EP_ERROR;
                  ES.IconAlignment := iaMiddleRight;
                  ES.Padding := Button2.Width + 8;
                end;

    end;
  end
  else
    if (Control.Name = Memo1.Name) then
    begin
      result := (Length(TMemo(Control).Text) < 20) and ((Length(TMemo(Control).Text) > 0));
      ES.Title := '�����ˮ';
      ES.Hint := '�ַ����賬��20��';
      ES.Icon := EP_INFO;
      ES.BlinkStyle := bsAlwaysBlink;
      ES.IconAlignment := iaBottomCenter;
    end;
end;

procedure TForm1.OnEnterDispose(Sender: TObject);
begin
  er_demo.Dispose(Sender as TControl);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  if (ed_name.Text = '') or (ed_pw.Text = '') or (ed_rpw.Text = '') then
  begin
    ShowMessage('�û���������Ϊ������');
    Exit;
  end;
  if er_demo.ErrorItmeCount <> 0 then
  begin
    for i := 0 to er_demo.ErrorItmeCount - 1 do
      if (er_demo[i].IconType <> EP_OK) then
      begin
        ShowMessage('���д��������ֵ��Ҫ����: ' + er_demo[i].Title);
        er_demo[i].SetBlinkStyle(bsBlinkIfDifferentError);
        Exit;
      end;
  end;
  ShowMessage('Good,ͨ����');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if (OpenPictureDialog1.Execute) then
    ed_img.Text := OpenPictureDialog1.FileName;
  ed_img.OnEnter(ed_img);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  er_test.Dispose(ed_test);
  with er_test.SetError(ed_test,ed_tip.Text,TErrorIconAlignment(ComboBox2.ItemIndex),tblinkstyle(RadioGroup1.ItemIndex)) do
  begin
    Title := ed_test.Text;
    IconType := TIconType(ComboBox1.ItemIndex);
  end;
end;

end.

