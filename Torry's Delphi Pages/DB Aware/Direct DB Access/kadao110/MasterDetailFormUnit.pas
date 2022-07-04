unit MasterDetailFormUnit;
//******************************************************************************
//                    Delphi Dao Project Version 1.6
//                 Copyright (c) 2000 by Kiril Antonov
//******************************************************************************

interface
                          
uses                                          
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMasterDetailForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ListBox1: TListBox;
    Button1: TButton;
    ListBox2: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    ListBox3: TListBox;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure ListBox3Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    Procedure ProcessJoinedFields;
  private
    { Private declarations }
  public
    { Public declarations }
    Function Execute(DetailFields,MasterFields:TStrings;Var JoinedFields:TStrings):Boolean;
  end;

var
  MasterDetailForm: TMasterDetailForm;

implementation
{$R *.DFM}
 Var
   JoinedFieldsTemp:TStrings;

procedure TMasterDetailForm.Button5Click(Sender: TObject);
begin
 ModalResult:=mrCancel;
end;

procedure TMasterDetailForm.Button4Click(Sender: TObject);
begin
 JoinedFieldsTemp.Clear;
 JoinedFieldsTemp.AddStrings(ListBox3.Items);
 ModalResult:=mrOk;
end;

Procedure TMasterDetailForm.ProcessJoinedFields;
 Var
  X,I:Integer;
  S,MasterField,DetailField:String;
Begin
  For X:=0 to ListBox3.Items.Count-1 do                     
      Begin
        S := ListBox3.Items[X];
        I := Pos(' -> ',S);
        if I > 0 Then
        Begin
         DetailField:=Copy(S,1,I-1);
         Delete(S,1,I+Length(' -> ')-1);
         MasterField:=S;
         ListBox1.Items.Delete(ListBox1.Items.IndexOf(DetailField));
         ListBox2.Items.Delete(ListBox2.Items.IndexOf(MasterField));
        End;
      End;
End;

procedure TMasterDetailForm.ListBox1Click(Sender: TObject);
begin
 if (ListBox1.ItemIndex <> -1) And (ListBox2.ItemIndex <> -1) Then Button1.Enabled:=True Else Button1.Enabled:=False;
end;

procedure TMasterDetailForm.ListBox2Click(Sender: TObject);
begin
 if (ListBox1.ItemIndex <> -1) And (ListBox2.ItemIndex <> -1) Then Button1.Enabled:=True Else Button1.Enabled:=False;
end;

procedure TMasterDetailForm.ListBox3Click(Sender: TObject);
begin
 if (ListBox3.ItemIndex <> -1) Then Button2.Enabled:=True Else Button2.Enabled:=False;
end;


procedure TMasterDetailForm.Button1Click(Sender: TObject);
Var
  MI,DI:Integer;
  MasterField,DetailField:String;
begin
  DI:=ListBox1.ItemIndex;
  MI:=ListBox2.ItemIndex;
  DetailField:=ListBox1.Items[DI];
  MasterField:=ListBox2.Items[MI];
  ListBox3.Items.Insert(0,DetailField+' -> '+MasterField);
  ListBox1.Items.Delete(DI);
  ListBox2.Items.Delete(MI);
  Button1.Enabled:=False;
  Button3.Enabled:=True;
  Button4.Enabled:=True;
end;

procedure TMasterDetailForm.Button2Click(Sender: TObject);
Var
    I:Integer;
    S,MasterField,DetailField:String;
begin
    I:=ListBox3.ItemIndex;
    if I = -1 Then Exit;
    S:=ListBox3.Items[I];
    ListBox3.Items.Delete(I);
    I:=Pos(' -> ',S);
    if I > 0 Then
       Begin
         DetailField:=Copy(S,1,I-1);
         Delete(S,1,I+Length(' -> ')-1);
         MasterField:=S;
         ListBox1.Items.Add(DetailField);
         ListBox2.Items.Add(MasterField);
       End;
    Button2.Enabled:=False;
    Button4.Enabled:=True;
end;
                                                    
procedure TMasterDetailForm.Button3Click(Sender: TObject);
begin
  Repeat
    ListBox3.ItemIndex:=0;
    Button2.Click;
  Until ListBox3.Items.Count=0;
  Button3.Enabled:=False;
  Button4.Enabled:=True;
end;

Function TMasterDetailForm.Execute(DetailFields,MasterFields:TStrings;Var JoinedFields:TStrings):Boolean;
Begin
 ListBox1.Clear;
 ListBox1.Items.AddStrings(DetailFields);

 ListBox2.Clear;
 ListBox2.Items.AddStrings(MasterFields);

 ListBox3.Clear;
 ListBox3.Items.AddStrings(JoinedFields);
 ProcessJoinedFields;
 if JoinedFields.Count > 0 Then Button3.Enabled:=True Else Button3.Enabled:=False;
 JoinedFieldsTemp:=TStringList.Create;
 JoinedFieldsTemp.SetText(JoinedFields.GetText);
 if ShowModal = mrOK Then Result:=True Else Result:=False;
 JoinedFields.SetText(JoinedFieldsTemp.GetText);
End;


end.
