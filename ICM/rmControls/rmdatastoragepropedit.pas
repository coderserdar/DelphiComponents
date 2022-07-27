{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmDataStoragePropEdit
Purpose  : The property and component editors for the rmDataStorage components
Date     : 12-29-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmDataStoragePropEdit;

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
  TrmDataLongintProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TrmDataStorageEditor = class(TComponentEditor)
  private
    procedure SaveToFile;
    procedure LoadFromFile;
    procedure ClearData;
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(index:integer); override;
  end;


implementation

uses
  rmDataStorage, dialogs;

{ TrmDataLongintProperty }

function TrmDataLongintProperty.GetAttributes: TPropertyAttributes;
begin
   Result := [paReadOnly];
end;

{ TrmDataStorageEditor }

procedure TrmDataStorageEditor.ClearData;
begin
   if assigned(Component) then
   begin
      TrmCustomDataStorage(Component).ClearData;
      designer.modified;
   end;
end;

procedure TrmDataStorageEditor.ExecuteVerb(index: integer);
begin
   case index of
        0:LoadFromFile;
        1:SaveToFile;
        2:ClearData;
   end;
end;

function TrmDataStorageEditor.GetVerb(index: integer): string;
begin
   case index of
        0:result := 'Load data from file...';
        1:result := 'Save data to file...';
        2:result := 'Clear data';
   end;
end;

function TrmDataStorageEditor.GetVerbCount: integer;
begin
   result := 3;
end;

procedure TrmDataStorageEditor.LoadFromFile;
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
            TrmCustomDataStorage(Component).LoadFromFile(filename);
            designer.modified;
         end;
      finally
         free;
      end
   end;
end;

procedure TrmDataStorageEditor.SaveToFile;
begin
   if assigned(Component) then
   begin
      if TrmCustomDataStorage(Component).DataSize = 0 then
      begin
         ShowMessage('Component contains no data.');
         exit;  
      end;
      with TSaveDialog.create(nil) do
      try
         Title := 'Save to...';
         Filter := 'All Files|*.*';
         FilterIndex := 1;
         if execute then
            TrmCustomDataStorage(Component).WriteToFile(filename);
      finally
         free;
      end
   end;
end;

end.
