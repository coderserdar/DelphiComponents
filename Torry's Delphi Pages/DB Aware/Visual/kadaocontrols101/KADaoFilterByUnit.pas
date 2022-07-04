unit KADaoFilterByUnit;
{$I KADaoControlsCommonDirectives.pas}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Db, KDaoDatabase, KDaoTable, DaoApi, StdCtrls;

type
  TKADaoFilterBy = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    Edit2: TEdit;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    Edit3: TEdit;
    ComboBox9: TComboBox;
    ComboBox10: TComboBox;
    ComboBox11: TComboBox;
    Edit4: TEdit;
    ComboBox12: TComboBox;
    ComboBox13: TComboBox;
    ComboBox14: TComboBox;
    Edit5: TEdit;
    ComboBox15: TComboBox;
    ComboBox16: TComboBox;
    ComboBox17: TComboBox;
    Edit6: TEdit;
    ComboBox18: TComboBox;
    ComboBox19: TComboBox;
    ComboBox20: TComboBox;
    Edit7: TEdit;
    ComboBox21: TComboBox;
    ComboBox22: TComboBox;
    ComboBox23: TComboBox;
    Edit8: TEdit;
    ComboBox24: TComboBox;
    ComboBox25: TComboBox;
    ComboBox26: TComboBox;
    Edit9: TEdit;
    ComboBox27: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label6: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure Edit_1Change(Sender: TObject);
  private
    { Private declarations }
    Combo_1 : Array[1..9] of TComboBox;
    Combo_2 : Array[1..9] of TComboBox;
    Edit_1  : Array[1..9] of TEdit;
    Combo_3 : Array[1..9] of TComboBox;
    Table   : TKAdaoTable;
    Procedure ClearALL(DaoTable : TKAdaoTable);
    Function  FindCombo(CB:TComboBox):Integer;
    Function  FindEdit(ED:TEdit):Integer;
  public
    { Public declarations }
    Function Execute(DaoTable : TKAdaoTable; Var FilterStr : String; Var LastFilter : TstringList):Boolean;
  end;

var
  KADaoFilterBy : TKADaoFilterBy;

implementation
{$R *.dfm}

Procedure TKADaoFilterBy.ClearALL(DaoTable : TKAdaoTable);
Var
  X : Integer;
Begin
 //*****************************************************************************
 Combo_1[1].Items.Clear;
 Combo_1[1].Items.Add('');
 For X := 0 To DaoTable.FieldCount-1 do
     Begin
      if (DaoTable.Fields[X].FieldKind=fkData) And (DaoTable.Fields[X].Visible) Then
         Combo_1[1].Items.Add(DaoTable.Fields[X].FieldName);
     End;
  Combo_1[1].ItemIndex := 0;
  For X := 2 To 9 do
     Begin
       Combo_1[X].Items.Assign(Combo_1[1].Items);
       Combo_1[X].ItemIndex := 0;
     End;


 Combo_2[1].Items.Clear;
 Combo_2[1].Items.Add(' = ');
 Combo_2[1].Items.Add(' > ');
 Combo_2[1].Items.Add(' < ');
 Combo_2[1].Items.Add(' >= ');
 Combo_2[1].Items.Add(' <= ');
 Combo_2[1].Items.Add(' <> ');
 Combo_2[1].Items.Add(' LIKE ');
 Combo_2[1].Items.Add(' IS NULL ');
 Combo_2[1].Items.Add(' IS NOT NULL ');
 Combo_2[1].ItemIndex := 0;
 For X := 2 To 9 do
     Begin
       Combo_2[X].Items.Assign(Combo_2[1].Items);
       Combo_2[X].ItemIndex := 0;
     End;
 For X := 1 To 9 do
     Begin
       Combo_3[X].ItemIndex := 0;
     End;
 For X := 1 To 9 do Edit_1[X].Text := '';
 //*****************************************************************************
End;

Function TKADaoFilterBy.Execute(DaoTable : TKAdaoTable; Var FilterStr : String; Var LastFilter : TstringList):Boolean;
Var
 X  : Integer;
 P  : Integer;
 S  : String;
 SD : String;
 FF : TField;
 Sep: String;
 Dat: String;
Begin
 Result := False;
 //*****************************************************************************
 Combo_1[1]:= ComboBox1;
 Combo_1[2]:= ComboBox4;
 Combo_1[3]:= ComboBox7;
 Combo_1[4]:= ComboBox10;
 Combo_1[5]:= ComboBox13;
 Combo_1[6]:= ComboBox16;
 Combo_1[7]:= ComboBox19;
 Combo_1[8]:= ComboBox22;
 Combo_1[9]:= ComboBox25;

 Combo_2[1]:= ComboBox2;
 Combo_2[2]:= ComboBox5;
 Combo_2[3]:= ComboBox8;
 Combo_2[4]:= ComboBox11;
 Combo_2[5]:= ComboBox14;
 Combo_2[6]:= ComboBox17;
 Combo_2[7]:= ComboBox20;
 Combo_2[8]:= ComboBox23;
 Combo_2[9]:= ComboBox26;

 Combo_3[1]:= ComboBox3;
 Combo_3[2]:= ComboBox6;
 Combo_3[3]:= ComboBox9;
 Combo_3[4]:= ComboBox12;
 Combo_3[5]:= ComboBox15;
 Combo_3[6]:= ComboBox18;
 Combo_3[7]:= ComboBox21;
 Combo_3[8]:= ComboBox24;
 Combo_3[9]:= ComboBox27;
 Combo_3[9].Visible := False;

 Edit_1[1] := Edit1;
 Edit_1[2] := Edit2;
 Edit_1[3] := Edit3;
 Edit_1[4] := Edit4;
 Edit_1[5] := Edit5;
 Edit_1[6] := Edit6;
 Edit_1[7] := Edit7;
 Edit_1[8] := Edit8;
 Edit_1[9] := Edit9;

 //*****************************************************************************
 Table := DaoTable;
 ClearALL(Table);
 if LastFilter.Count > 0 Then
    Begin
     For X := 0 To LastFilter.Count-1 do
         Begin
           S := LastFilter.Strings[X];
           P := Pos('***',S);
           if P > 0 Then
              Begin
                Combo_1[X+1].ItemIndex := Combo_1[X+1].Items.IndexOf(System.Copy(S,1,P-1));
                System.Delete(S,1,P+2);
              End;
           P := Pos('***',S);
           if P > 0 Then
              Begin
                SD:=System.Copy(S,1,P-1);
                Combo_2[X+1].ItemIndex :=  Combo_2[X+1].Items.IndexOf(System.Copy(S,1,P-1));
                System.Delete(S,1,P+2);
                if (SD=' IS NULL ') or (SD=' IS NOT NULL ') Then
                   Begin
                     Edit_1[X+1].Text    := '';
                     Edit_1[X+1].Enabled := False;
                     Edit_1[X+1].Color   := clInactiveCaption;
                   End;
              End;
           P := Pos('***',S);
           if P > 0 Then
              Begin
                Edit_1[X+1].Text := System.Copy(S,1,P-1);
                System.Delete(S,1,P+2);
              End;
           Combo_3[X+1].ItemIndex := Combo_3[X+1].Items.IndexOf(S);
           if Combo_3[X+1].ItemIndex = -1 Then Combo_3[X+1].ItemIndex := 0;
         End;
    End;

 ShowModal;
 if ModalResult=mrOK Then
    Begin
      Result    := True;
      FilterStr := '';
      LastFilter.Clear;
      For X := 1 to 9 Do
          Begin
           if  (Combo_1[X].Text <> '')
           And (Combo_2[X].Text <> '') Then
             Begin
              if (Edit_1[X].Enabled=False) or (Edit_1[X].Text <> '') Then
                 Begin
                   FF  := Table.FindField(Combo_1[X].Text);
                   Dat := Edit_1[X].Text;
                   Case FF.DataType of
                        ftDateTime   ,
                        ftDate       ,
                        ftTime       : Begin
                                         Sep := '#';
                                         P := Pos('.',Dat);
                                         While P > 0 Do
                                            Begin
                                              Dat[P] :='/';
                                              P := Pos('.',Dat);
                                            End;
                                       End;
                        ftUnknown    : Begin
                                         Sep := '';
                                       End;
                        ftString     : Begin
                                         Sep := '"';
                                       End;
                        ftMemo       : Begin
                                         Sep := '"';
                                       End;
                   else
                       Begin
                        Sep := '';
                       End;
                   End;
                   if X=1 Then
                      FilterStr := FilterStr+'(['+Combo_1[X].Text+']'+Combo_2[X].Text+Sep+Dat+Sep+') '
                   Else
                      FilterStr := FilterStr+Combo_3[X-1].Text+' (['+Combo_1[X].Text+']'+Combo_2[X].Text+Sep+Dat+Sep+') ';
                   LastFilter.Add(Combo_1[X].Text+'***'+Combo_2[X].Text+'***'+Edit_1[X].Text+'***'+Combo_3[X].Text);
                 End;
             End;
          End;
    End;
End;

procedure TKADaoFilterBy.Button2Click(Sender: TObject);
begin
 ModalResult := mrCancel;
end;

procedure TKADaoFilterBy.Button1Click(Sender: TObject);
begin
 ModalResult := mrOK;
end;

procedure TKADaoFilterBy.Button3Click(Sender: TObject);
begin
 ClearALL(Table);
end;

Function  TKADaoFilterBy.FindCombo(CB:TComboBox):Integer;
Var
 X : Integer;
Begin
 Result :=0;
 For X := 1 to 9 do
     Begin
      if Combo_2[X]=CB Then
         Begin
           Result := X;
           Exit;
         End;
     End;
End;

Function  TKADaoFilterBy.FindEdit(ED:TEdit):Integer;
Var
 X : Integer;
Begin
 Result :=0;
 For X := 1 to 9 do
     Begin
      if Edit_1[X]=ED Then
         Begin
           Result := X;
           Exit;
         End;
     End;
End;

procedure TKADaoFilterBy.ComboBoxChange(Sender: TObject);
Var
 CBN : Integer;
 CB  : TComboBox;
begin
 CB  := (Sender AS TComboBox);
 CBN := FindCombo(CB);
 if CBN=0 Then Exit;
 if (CB.Text=' IS NULL ') Or (CB.Text=' IS NOT NULL ') Then
    Begin
     Edit_1[CBN].Text    := '';
     Edit_1[CBN].Enabled := False;
     Edit_1[CBN].Color   := clInactiveCaption;
    End
 Else
    Begin
     Edit_1[CBN].Enabled := True;
     Edit_1[CBN].Color   := clWindow;
    End;
end;

procedure TKADaoFilterBy.Edit_1Change(Sender: TObject);
Var
 EDN : Integer;
 ED  : TEdit;
begin
 ED  := (Sender AS TEdit);
 EDN := FindEdit(ED);
 if EDN=0 Then Exit;
 if ED.Text='' Then Combo_1[EDN].ItemIndex:=-1;
end;

end.
