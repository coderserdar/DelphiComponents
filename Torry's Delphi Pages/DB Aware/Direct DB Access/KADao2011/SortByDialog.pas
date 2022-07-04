unit SortByDialog;
//******************************************************************************
//                    Delphi Dao Project Version 1.6
//                 Copyright (c) 2000 by Kiril Antonov
//******************************************************************************

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TSortByDialog = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ListBox2: TListBox;
    AddBtn: TBitBtn;
    RemoveBtn: TBitBtn;
    UpBtn: TBitBtn;
    DownBtn: TBitBtn;
    OKButton: TButton;
    CancelButton: TButton;
    ListBox3: TListBox;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Label4: TLabel;
    ListBox1: TListBox;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure ListBox3Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    Function Execute(FieldNames:TStrings; OrderList:TStrings;UseQuotes:Boolean):Boolean;
    { Public declarations }
  end;

var
  SortDialog: TSortByDialog;


implementation
{$R *.DFM}
var
 OrderListTemp  : TStrings;
 F_UseQuotes    : Boolean;

Procedure RemoveExtraSpaces(var S:String);
Var
  SS : String;
  P  : Integer;
Begin
  SS:='  ';
  Repeat
    P:=Pos(SS,S);
    if P > 0 Then Delete(S,P,1);
  Until P=0;

  SS:='[';
  Repeat
    P:=Pos(SS,S);
    if P > 0 Then Delete(S,P,1);
  Until P=0;

  SS:=']';
  Repeat
    P:=Pos(SS,S);
    if P > 0 Then Delete(S,P,1);
  Until P=0;
End;

procedure TSortByDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSortByDialog.OKButtonClick(Sender: TObject);
Var
 X:Integer;
begin
 if ListBox1.Items.Count > 0 Then
    Begin
     OrderListTemp.Clear;
     For X:=0 To ListBox1.Items.Count-1 do
         Begin
          if ListBox3.Items[X]='Ascending' Then
             Begin
               if F_UseQuotes Then
                  OrderListTemp.Add('['+ListBox1.Items[X]+'] ASC')
               Else
                  OrderListTemp.Add(ListBox1.Items[X]+' ASC');
             End
          Else
             Begin
               if F_UseQuotes Then
                  OrderListTemp.Add('['+ListBox1.Items[X]+'] DESC')
               Else
                  OrderListTemp.Add(ListBox1.Items[X]+' DESC')
             End;
         End;
    End;
 ModalResult := mrOK;
end;

procedure TSortByDialog.AddBtnClick(Sender: TObject);
begin
 if ListBox2.ItemIndex <> -1 Then
    Begin
      ListBox1.Items.Add(ListBox2.Items.Strings[ListBox2.ItemIndex]);
      ListBox3.Items.Add(ComboBox1.Items[ComboBox1.ItemIndex]);
      ListBox2.Items.Delete(ListBox2.ItemIndex);
      ListBox1.ItemIndex:=ListBox1.Items.Count-1;
      ListBox1Click(ListBox1);
    End;
end;

procedure TSortByDialog.RemoveBtnClick(Sender: TObject);
begin
 if ListBox1.ItemIndex <> -1 Then
    Begin
      ListBox2.Items.Add(ListBox1.Items.Strings[ListBox1.ItemIndex]);
      ListBox3.Items.Delete(ListBox1.ItemIndex);
      ListBox1.Items.Delete(ListBox1.ItemIndex);
    End;
end;

procedure TSortByDialog.UpBtnClick(Sender: TObject);
Var
  TmpField:String;
  TmpOrder:String;
  Poz     : Integer;
begin
  if ListBox1.ItemIndex > 0 Then
     Begin
       Poz:=ListBox1.ItemIndex;
       TmpField:=ListBox1.Items[Poz-1];
       TmpOrder:=ListBox3.Items[Poz-1];
       ListBox1.Items[Poz-1]:=ListBox1.Items[Poz];
       ListBox3.Items[Poz-1]:=ListBox3.Items[Poz];
       ListBox1.Items[Poz]:=TmpField;
       ListBox3.Items[Poz]:=TmpOrder;
       ListBox1.ItemIndex:=Poz-1;
       ListBox3.ItemIndex:=Poz-1;
     End;
end;

procedure TSortByDialog.DownBtnClick(Sender: TObject);
Var
  TmpField:String;
  TmpOrder:String;
  Poz     : Integer;
begin
  if ListBox1.ItemIndex < ListBox1.Items.Count-1 Then
     Begin
       Poz:=ListBox1.ItemIndex;
       TmpField:=ListBox1.Items[Poz+1];
       TmpOrder:=ListBox3.Items[Poz+1];
       ListBox1.Items[Poz+1]:=ListBox1.Items[Poz];
       ListBox3.Items[Poz+1]:=ListBox3.Items[Poz];
       ListBox1.Items[Poz]:=TmpField;
       ListBox3.Items[Poz]:=TmpOrder;
       ListBox1.ItemIndex:=Poz+1;
       ListBox3.ItemIndex:=Poz+1;
     End;
end;

Function TSortByDialog.Execute(FieldNames:TStrings; OrderList:TStrings;UseQuotes:Boolean):Boolean;
Var
 X,P           : Integer;
 S             : String;
 FieldNamesTmp : TStringList;
 Tmp           : TStringList;
Begin
 Result:=False;
 F_UseQuotes:=UseQuotes;
 ComboBox1.Clear;
 ComboBox1.Items.Add('Ascending');
 ComboBox1.Items.Add('Descending');
 ComboBox1.ItemIndex:=0;
 Tmp := TStringList.Create;
 Tmp.SetText(OrderList.GetText);
 if OrderList.Count > 0 Then
    begin
      ListBox1.Clear;
      For X:=0 To OrderList.Count-1 Do
          Begin
            S:=OrderList.Strings[X];
            RemoveExtraSpaces(S);
            P:=Pos(' DESC',S);
            if P > 0 Then
               Begin
                 ListBox3.Items.Add('Descending');
                 S:=Copy(S,1,P-1);
               End
            Else
               Begin
                 P:=Pos(' ASC',S);
                 if P > 0 Then
                    Begin
                      S:=Copy(S,1,P-1);
                    End;
                 ListBox3.Items.Add('Ascending');
                End;
            ListBox1.Items.Add(S);
            OrderList.Strings[X]:=S;
          End;
    End;
 FieldNamesTmp:=TStringList.Create;
 FieldNamesTmp.SetText(FieldNames.GetText);
 For X:=0 To OrderList.Count-1 do
     Begin
      P:=FieldNamesTmp.IndexOF(OrderList.Strings[X]);
      if P > -1 Then FieldNamesTmp.Delete(P);
     End;
 if FieldNamesTmp.Count > 0 Then
    Begin
       ListBox2.Clear;
       For X:=0 To FieldNamesTmp.Count-1 Do ListBox2.Items.Add(FieldNamesTmp.Strings[X]);
    End;
 FieldNamesTmp.Free;
 OrderListTemp:=TStringList.Create;
 OrderListTemp.Clear;
 if ShowModal = mrOK Then
    Begin
      if OrderListTemp.Count > 0 Then
         Begin
           OrderList.SetText(OrderListTemp.GetText);
         End
      Else
         OrderList.Clear;
      Result:=True;
    End                                         
 Else
    Begin
      OrderList.SetText(Tmp.GetText);
    End;
 OrderListTemp.Free;
 Tmp.Free;
End;

procedure TSortByDialog.ListBox3Click(Sender: TObject);
begin
  ListBox1.ItemIndex:=ListBox3.ItemIndex;
  ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(ListBox3.Items.Strings[ListBox3.ItemIndex]);
end;

procedure TSortByDialog.ListBox1Click(Sender: TObject);
begin
 ListBox3.ItemIndex:=ListBox1.ItemIndex;
 ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(ListBox3.Items.Strings[ListBox3.ItemIndex]);
end;

procedure TSortByDialog.ComboBox1Change(Sender: TObject);
Var
  P: Integer;
begin
  P:=ListBox3.ItemIndex;
  if P <> -1 Then
     ListBox3.Items[P]:=ComboBox1.Items[ComboBox1.ItemIndex];
end;

procedure TSortByDialog.ListBox2DblClick(Sender: TObject);
begin
 AddBtn.Click;
end;

end.
