{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmGlobalComponentHook
Purpose  : Provides a standard stack type interface to the rmControls that
           descend from TComponent and that hook their owners WndProc.  This
           enables the component to correctly backout of the hook with out
           hanging the form or application.
Date     : 10-26-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmGlobalComponentHook;

interface

{$I CompilerDefines.INC}

uses Controls, Windows;

Procedure PushOldProc(aWinControl:TWinControl; OldHook:TFarProc);
Function PopOldProc(aWinControl:TWinControl):TFarProc;

implementation

uses classes;

type
   TrmWinControlHookList = class(TObject)
   private
      fWinControl:TWinControl;
      fHooks:TList;
   public
      constructor Create(aWinControl:TWinControl);
      destructor destroy; override;
      property WinControl:TWinControl read fWinControl;
      procedure AddHook(oldHook:TFarProc);
      function GetNextHook:TFarProc;
      function Count:integer;
   end;

var
   FormList : TList;

Procedure PushOldProc(aWinControl:TWinControl; OldHook:TFarProc);
var
   loop : integer;
   wHook : TrmWinControlHookList;
   found : boolean;
begin
   found := false;
   wHook := nil;
   for loop := 0 to formlist.count-1 do
   begin
      wHook := TrmWinControlHookList(formlist[loop]);
      found := wHook.WinControl = aWinControl;
      if found then break;
   end;
   if found then
      wHook.AddHook(oldHook)
   else
   begin
      if assigned(aWinControl) then
      begin
         wHook := TrmWinControlHookList.create(aWinControl);
         formlist.add(wHook);
         wHook.addhook(oldhook);
      end;
   end;
end;

Function PopOldProc(aWinControl:TWinControl):TFarProc;
var
   loop : integer;
   wHook : TrmWinControlHookList;
   found : boolean;
begin
   found := false;
   wHook := nil;
   for loop := 0 to formlist.count-1 do
   begin
      wHook := TrmWinControlHookList(formlist[loop]);
      found := wHook.WinControl = aWinControl;
      if found then break;
   end;
   if found then
   begin
      result := wHook.GetNextHook;
      if wHook.Count = 0 then
      begin
         Formlist.Delete(loop);
         wHook.free;
      end;
   end
   else
      result := nil;
end;

{ TrmWinControlHookList }

procedure TrmWinControlHookList.AddHook(oldHook: TFarProc);
begin
   fhooks.add(oldhook);  
end;

function TrmWinControlHookList.Count: integer;
begin
   result := fhooks.count;
end;

constructor TrmWinControlHookList.Create(aWinControl:TWinControl);
begin
   fWinControl := aWinControl;
   fhooks := TList.create;
end;

destructor TrmWinControlHookList.destroy;
begin
   fhooks.free;
   inherited;  
end;

function TrmWinControlHookList.GetNextHook: TFarProc;
begin
   result := fhooks[fhooks.count-1];
   fhooks.delete(fhooks.count-1);
end;

initialization
   FormList := TList.create;

finalization
   FormList.free;         

end.
