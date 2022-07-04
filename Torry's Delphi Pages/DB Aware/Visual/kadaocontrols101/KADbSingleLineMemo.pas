unit KADbSingleLineMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, DBCtrls, Graphics;

type
  TKADbSingleLineMemo = class(TdbMemo)
  private
    { Private declarations }
    F_NumLines : Integer;
    Function     GetLineHeight:Integer;
    Procedure    F_Set_NumLines(Value : Integer);
    Procedure    F_Set_Font(Value : TFont);
    Function     F_Get_Font : TFont;
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    Constructor Create(AOwner: TComponent); Override;
    Property    LineCount : Integer Read F_NumLines Write F_Set_NumLines;
    Property    Font      : TFont   Read F_Get_Font Write F_Set_Font;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADbSingleLineMemo]);
end;

{ TDbSingleLineMemo }


constructor TKADbSingleLineMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ScrollBars := ssVertical;
  Height     := GetLineHeight+2;
  WordWrap   := False;
  F_NumLines := 1;
end;

procedure TKADbSingleLineMemo.F_Set_NumLines(Value: Integer);
begin
 F_NumLines  := Value;
 Height      := (GetLineHeight*F_NumLines)+2;
end;

Function TKADbSingleLineMemo.GetLineHeight:Integer;
Var
  Edit : TEdit;                                                                 
Begin
  Edit      := TEdit.Create(Nil);
  Edit.Font := Font;
  Result    := Edit.Height;
  Edit.Free;
End;

Procedure TKADbSingleLineMemo.F_Set_Font(Value : TFont);
Begin
  Inherited Font := Value;
  LineCount      := F_NumLines;
End;

Function  TKADbSingleLineMemo.F_Get_Font : TFont;
Begin
 Result := Inherited Font;
End;

end.
