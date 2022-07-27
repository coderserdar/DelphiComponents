{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmKeyBindingPropEdit
Purpose  : Component editor for the rmKeyBinding component.
Date     : 05-03-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmKeyBindingPropEdit;

interface

{$I CompilerDefines.INC}

{$ifdef D6_or_higher}
uses
  DesignIntf, DesignEditors, TypInfo;    
{$else}
uses
  DsgnIntf, TypInfo;
{$endif}


type
  TrmKeyBindingEditor = class(TComponentEditor)
  private
    procedure SaveToFile(Binary:boolean);
    procedure LoadFromFile(Binary:Boolean);
    procedure ClearData;
    procedure EditBindings;
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(index:integer); override;
  end;


implementation

uses
  rmKeyBindings, dialogs;

{ TrmKeyBindingEditor }

procedure TrmKeyBindingEditor.ClearData;
begin
   if assigned(Component) then
   begin
      TrmKeyBindings(Component).ClearBindings;
      designer.modified;
   end;
end;

procedure TrmKeyBindingEditor.EditBindings;
begin
   if assigned(Component) then
   begin
      if TrmKeyBindings(Component).EditBindings then
      begin
         TrmKeyBindings(Component).ApplyBindings;
         designer.modified;
      end;
   end;
end;

procedure TrmKeyBindingEditor.ExecuteVerb(index: integer);
begin
   case index of
        0:EditBindings;
        1:LoadFromFile(True);
        2:LoadFromFile(False);
        3:SaveToFile(True);
        4:SaveToFile(False);
        5:ClearData;
   end;
end;

function TrmKeyBindingEditor.GetVerb(index: integer): string;
begin
   case index of
        0:result := 'Edit bindings...';
        1:result := 'Load bindings from file (Binary)...';
        2:result := 'Load bindings from file (Text)...';
        3:result := 'Save bindings to file (Binary)...';
        4:result := 'Save bindings to file (Text)...';
        5:result := 'Clear bindings';
   end;
end;

function TrmKeyBindingEditor.GetVerbCount: integer;
begin
   result := 6;
end;

procedure TrmKeyBindingEditor.LoadFromFile(Binary:Boolean);
begin
   if assigned(Component) then
   begin
      with TOpenDialog.create(nil) do
      try
         Title := 'Load file...';
         Filter := 'All Files|*.*';
         FilterIndex := 1;
         if execute then
         begin
            TrmKeyBindings(Component).LoadBindingsFromFile(filename, Binary);
            designer.modified;
         end;
      finally
         free;
      end
   end;
end;

procedure TrmKeyBindingEditor.SaveToFile(Binary:Boolean);
begin
   if assigned(Component) then
   begin
      if not assigned(TrmKeyBindings(Component).Actions) then
      begin
         ShowMessage('No actions are assigned');
         exit;
      end;
      with TSaveDialog.create(nil) do
      try
         Title := 'Save to...';
         Filter := 'All Files|*.*';
         FilterIndex := 1;
         if execute then
            TrmKeyBindings(Component).SaveBindingsToFile(filename, Binary);
      finally
         free;
      end
   end;
end;

end.
