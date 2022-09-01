unit bvUtils;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
{$else}
  QControls,
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
  Qt,
{$endif}
  Classes,
  SysUtils;

type TMoverMouse=class
       OldX:integer;
       OldY:integer;
       procedure GetMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
       procedure GetMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
     end;


function GetForm(ID:{$ifndef LINUX}HWND{$else}QWidgetH{$endif};LOCKMDI:boolean=false):TForm;

function FindMdiChildForm(frm:pointer):boolean;

procedure UpdateScreen;

var MoverMouse:TMoverMouse;


implementation

function GetForm(ID:{$ifndef LINUX}HWND{$else}QWidgetH{$endif};LOCKMDI:boolean=false):TForm;
var i:integer;
begin
  Result:=nil;
  if LockMDI then
  begin
    if assigned(Application) and assigned(Application.mainform) then
    for i:=0 to TForm(Application.MainForm).MDIChildCount-1 do
    begin
       if TForm(application.MainForm).MDIChildren[i].handle=ID
       then begin
          REsult:=TForm(Application.mainform).mdichildren[i];
          break;
       end
    end
  end
  else for i:=0 to Screen.FormCount-1 do
  begin
     if Screen.Forms[i].handle=ID
     then begin
        REsult:=Screen.Forms[i];
        break;
     end
  end;
end;

procedure TMoverMouse.GetMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssleft in Shift then begin
    OldX:=X;
    OldY:=Y;
    if Sender is TControl then (Sender as Tcontrol).Repaint;
  end;
end;

procedure TMoverMouse.GetMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (ssLeft in Shift) and (Sender is TControl)  then with Sender as TControl do begin
    Left:=Left+x-OldX;
    Top:=Top+y-OldY;
  end
//  else if Sender is TControl then (Sender as TControl).Repaint;
end;

function FindMdiChildForm(frm:pointer):boolean;
var i:integer;
begin
   REsult:=false;
   if not assigned(Application.MainForm) then exit;

   for i:=0 to TForm(Application.mainform).MDIChildCount-1
   do
      if TForm(Application.mainform).MDIChildren[i]=frm
      then begin
        Result:=true;
        break
      end
end;


//TCompareItemFunction=function(Item1:variant;Item2:variant):integer;
{
procedure bvQuickSort(SortArray:array of Variant;L, R: Integer;SCompare: TCompareItemFunction);
var
  I, J: Integer;
  P, T: Variant;
begin
  repeat
    I := L;
    J := R;
    P := sortArray[(L+R) shr 1];
    repeat
      while SCompare(SortArray[I], P) < 0 do Inc(I);
      while SCompare(SortArray[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        T := SortArray[I];
        SortArray[I] := SortArray[J];
        SortArray[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then bvQuickSort(SortArray, L, J, SCompare);
    L := I;
  until I >= R;
end;
}


procedure UpdateScreen;
var i:integer;
begin
  if Assigned(Screen) then
  for i:=0 to Screen.FormCount-1 do begin
     if (Screen.forms[i].FormStyle<>fsMDICHILD)
        and screen.forms[i].visible
     then Screen.forms[i].update;
  end;
end;


//type  TDateTimeParam=(Year,Month,Day,Hour, Min, Sec, MSec);

initialization

  Movermouse:=TMoverMouse.Create;

finalization

  MoverMouse.Free;


end.

