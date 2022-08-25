{  Unit Name: AppInit
  Purpose: The purpose of this unit is to indicate whether a previous
           instance of an application is running.

  Author: Marc Batchelor
  Date: 06/19/97

  Usage: Simply include this unit in your Delphi 16/32 project. From the
         Project's Initialization, check the variable
         AppInit.IsFirstInstance. If IsFirstInstance is false, then
         the application can bring the previous instance in focus and
         terminate.

15th July 1999 - Angus - removed application path, only check program name

}

unit AppInit;

interface

uses
  SysUtils, WinProcs, WinTypes, Classes, Forms,  dialogs;

var
{$IFDEF WIN32}
  InstanceMutexHandle: THandle = 0;
  UniqueApplicationString: String;
{$ENDIF}
  IsFirstInstance: Boolean;

implementation


{$IFDEF WIN32}
procedure SetUniqueAppString;
var
  times: Integer;
begin
  { Setup Unique String for the Mutex }
  UniqueApplicationString := 'APPLICATION-' +
  									ExtractFileName (Application.ExeName) ;
  for times := 1 to Length(UniqueApplicationString) do
  begin
    { Mutex names can't have '\' in them, so perform replacement }
    if UniqueApplicationString[times] = '\' then
      UniqueApplicationString[times] := '_';
  end;
  { Uppercase the string to prevent case sensitivity problems }
  UniqueApplicationString := AnsiUppercase(UniqueApplicationString);
end;

procedure InitInstance;
begin
  { Check to see if the mutex is already there }
  InstanceMutexHandle := OpenMutex(MUTEX_ALL_ACCESS, false,
    								pchar(UniqueApplicationString));
  if InstanceMutexHandle = 0 then
  begin
    { This is the first instance }
    InstanceMutexHandle := CreateMutex(nil, false,
								      pchar(UniqueApplicationString));
    { Error checking to see if anyone beat us... }
    if InstanceMutexHandle = 0 then
      IsFirstInstance := false
    else
      IsFirstInstance := true;
  end
  else
    IsFirstInstance := false;
end;
{$ENDIF}

initialization
{$IFDEF WIN32}
  IsFirstInstance := false;
  SetUniqueAppString;
  InitInstance;
finalization
  if IsFirstInstance then
  begin
    CloseHandle(InstanceMutexHandle);
    InstanceMutexHandle := 0;
  end;
  UniqueApplicationString := '';
{$ELSE}
  IsFirstInstance = (hPrevInst = 0);
{$ENDIF}
end.
