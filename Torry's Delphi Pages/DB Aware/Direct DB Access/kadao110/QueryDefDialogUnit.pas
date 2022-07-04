unit QueryDefDialogUnit;
//******************************************************************************
//                    Delphi Dao Project Version 1.6
//                 Copyright (c) 2000 by Kiril Antonov
//******************************************************************************

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Grids;

type
  TQueryDefDialog = class(TForm)                                   
    StringGrid1: TStringGrid;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Function Execute(ParmNames,ParamDaoTypes,ParamBDETypes:TStrings;Var ParmValues:TStrings):Boolean;
  end;

var
  QueryDefDialog : TQueryDefDialog;
  ParmValuesTemp :TStrings;

implementation
{$R *.DFM}
Var
 RC:Integer;

procedure TQueryDefDialog.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TQueryDefDialog.Button1Click(Sender: TObject);
Var
 X:Integer;
begin
  ParmValuesTemp.Clear;
  For X:=1 to RC do ParmValuesTemp.Add(StringGrid1.Cells[4,X]);
  ModalResult := mrOK;
end;

Function TQueryDefDialog.Execute(ParmNames,ParamDaoTypes,ParamBDETypes:TStrings;Var ParmValues:TStrings):Boolean;
Var
  X:Integer;
Begin
  Result:=False;
  StringGrid1.RowHeights[0]:=36;
  StringGrid1.Cols[0].Add(' No:');
  StringGrid1.Cols[1].Add(' Parameter name');
  StringGrid1.Cols[2].Add(' Parameter DAO type');
  StringGrid1.Cols[3].Add(' Parameter BDE type');
  StringGrid1.Cols[4].Add(' Parameter Value');

  For X:=0 to ParmNames.Count-1 do
      Begin
        ParmNames.Strings[X]     := ' '+ParmNames.Strings[X];
        ParamDaoTypes.Strings[X] := ' '+ParamDaoTypes.Strings[X];
        ParamBDETypes.Strings[X] := ' '+ParamBDETypes.Strings[X];
      End;

  StringGrid1.Cols[1].AddStrings(ParmNames);
  StringGrid1.Cols[2].AddStrings(ParamDaoTypes);
  StringGrid1.Cols[3].AddStrings(ParamBDETypes);
  StringGrid1.Cols[4].AddStrings(ParmValues);

  RC:=ParmNames.Count;

  For X:=1 to ParmNames.Count do  StringGrid1.Cols[0].Add(' '+InTToStr(X));
  ParmValuesTemp := TStringList.Create;
  ParmValuesTemp.Clear;
  if ShowModal=mrOK Then
     Begin
       ParmValues.Clear;
       For X:=0 to ParmValuesTemp.Count-1 do
           Begin
             if ParmValuesTemp.Strings[X]='' Then ParmValuesTemp.Strings[X]:='NULL';
             ParmValues.Add(ParmValuesTemp.Strings[X]);
           End;
       Result:=True;
     End;
  ParmValuesTemp.Free;
End;

procedure TQueryDefDialog.StringGrid1Click(Sender: TObject);
begin
  if StringGrid1.Row > RC Then StringGrid1.Row:=RC;
end;

end.
