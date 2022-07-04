unit ODBCDialogUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TODBCDialog = class(TForm)
    RadioGroup1: TRadioGroup;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    { Private declarations }
     NewDSN : String;
  public
    { Public declarations }
    Function Execute(SysDsn, UsrDsn:TStrings; Var DsnString:String; UseODBCDialog:Boolean):Boolean;
  end;

var
  ODBCDialog  : TODBCDialog;

implementation
Uses
  KDaoDatabase;
{$R *.DFM}

Var
 Resultat : String;

procedure TODBCDialog.RadioGroup1Click(Sender: TObject);
begin
  NewDSN := '';
  if RadioGroup1.ItemIndex=0 Then
     Begin
       ComboBox1.Enabled:=True;
       ComboBox2.Enabled:=False;
     End
  Else
     Begin
       ComboBox1.Enabled:=False;
       ComboBox2.Enabled:=True;
     End;
end;

procedure TODBCDialog.Button2Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TODBCDialog.Button1Click(Sender: TObject);
begin
  ModalResult:=mrOk;
  if RadioGroup1.ItemIndex=0 Then
     Begin
       Resultat:=ComboBox1.Items.Strings[ComboBox1.ItemIndex];
     End
  Else
     Begin
       Resultat:=ComboBox2.Items.Strings[ComboBox2.ItemIndex];
     End;
end;

Function TODBCDialog.Execute(SysDsn, UsrDsn:TStrings; Var DsnString:String; UseODBCDialog:Boolean):Boolean;
begin
  NewDSN := '';
  ComboBox1.Clear;
  ComboBox2.Clear;
  ComboBox1.Items.SetText(SysDsn.GetText);
  ComboBox2.Items.SetText(UsrDsn.GetText);
  ComboBox1.ItemIndex:=0;
  ComboBox2.ItemIndex:=0;
  if ComboBox1.Items.IndexOf(DsnString) > -1 Then
     Begin
       RadioGroup1.ItemIndex:=0;
       ComboBox1.Enabled:=True;
       ComboBox2.Enabled:=False;
       ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(DsnString);
     End;
  if ComboBox2.Items.IndexOf(DsnString) > -1 Then
     Begin
       RadioGroup1.ItemIndex:=1;
       ComboBox1.Enabled:=False;
       ComboBox2.Enabled:=True;
       ComboBox2.ItemIndex:=ComboBox2.Items.IndexOf(DsnString);
     End;
  if UseODBCDialog Then
     Begin
       Button3.Click;
       Result := False;
       if NewDSN <> '' Then
          Begin
           DsnString := NewDSN;
           Result    := True;
          End;
     End
  Else
     Begin
      if ShowModal=mrOK Then
         Begin
          Result := True;
          if NewDSN = '' Then DsnString := Resultat Else DsnString := NewDSN;
         End
      Else
         Begin
          Result := False;
         End;
     End;
end;

procedure TODBCDialog.Button3Click(Sender: TObject);
Var
 DB     : TKADaoDatabase;
 Error  : TDaoErrRec;
Begin
 NewDSN := '';
 //************************************************************** ODBC Based
 DB := TKADaoDatabase.Create(Nil);
 DB.GoOffline;
 DB.PrivateEngine        := True;
 DB.DatabaseType         := 'ODBC';
 DB.Database             := 'DATABASE=;DSN=';
 DB.GoOnline;
 if DB.Database='' Then
    Begin
     DB.Free;
     Exit;
    End;
 Try
   DB.Open;
 Except
   Error := DB.GetLastDaoError;
   DB.Free;
   ShowMessage(Error.Description);
   Exit;
 End;
 if NOT DB.Connected Then
    Begin
      DB.Free;
      Exit;
    End;
 NewDSN := DB.CoreDatabase.Connect;
 DB.Free;
 Button1.Click;
End;

procedure TODBCDialog.ComboBox1Change(Sender: TObject);
begin
 NewDSN := '';
end;

procedure TODBCDialog.ComboBox2Change(Sender: TObject);
begin
 NewDSN := '';
end;
                              
end.
