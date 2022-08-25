{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 11, 2009
Description:  This source is part of WebAppServer demo application.
              The datamodule is a container for data handling routines.
              Normally you would find here database stuff. In this simple
              demo, persistent data is not stored in a database but into a
              simple INI file.
Version:      1.01
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Jul 30, 2010 V1.01 F.Piette - added SaveConfig


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebAppServerDataModule;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.Unistd,
  {$ENDIF}
    SysUtils, Classes, OverbyteIcsIniFiles;

type
    TWebAppSrvDisplayEvent = procedure (Sender : TObject;
                                        const Msg : String) of object;
    TWebAppSrvDataModule = class(TDataModule)
    private
        FIniFileName     : String;
        FDataDir         : String;
        FImagesDir       : String;
        FCounterFileName : String;
        FPort            : String;
        FOnDisplay       : TWebAppSrvDisplayEvent;
        procedure SetDataDir(const Value: String);
    public
        function  CounterValue(const CounterName : String;
                               DefaultValue      : Integer) : Integer;
        function  CounterIncrement(const CounterRef: String) : Integer;
        procedure LoadConfig;
        procedure SaveConfig;
        procedure Display(const Msg : String);
        procedure DisplayHandler(Sender : TObject; const Msg : String);
        property IniFileName : String read  FIniFileName
                                      write FIniFileName;
        property DataDir     : String read  FDataDir
                                      write SetDataDir;
        property ImagesDir   : String read  FImagesDir
                                      write FImagesDir;
        property CounterFileName : String             read  FCounterFileName;
        property Port            : String             read  FPort
                                                      write FPort;
        property OnDisplay   : TWebAppSrvDisplayEvent read  FOnDisplay
                                                      write FOnDisplay;
    end;

procedure ForceRemoveDir(const Dir : String);
procedure CleanupTimeStampedDir(const Dir : String);

var
    WebAppSrvDataModule: TWebAppSrvDataModule;

const
    CounterSection = 'Counter';
    SectionConfig  = 'Config';
    KeyPort        = 'Port';
    DftPort        = '20105';

implementation

{$R *.dfm}

{ TWebAppSrvDataModule }
function TWebAppSrvDataModule.CounterValue(
    const CounterName : String;
    DefaultValue      : Integer) : Integer;
var
    IniFile     : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(WebAppSrvDataModule.CounterFileName);
    try
        Result := IniFile.ReadInteger(CounterSection,
                                      CounterName,
                                      DefaultValue);
    finally
        FreeAndNil(IniFile);
    end;
end;

function TWebAppSrvDataModule.CounterIncrement(
    const CounterRef: String): Integer;
var
    IniFile     : TIcsIniFile;
begin
    // Open the ini file (will be created if doesn't exists)
    IniFile := TIcsIniFile.Create(FCounterFileName);
    try
        // Read the counter value and increment it
        Result := IniFile.ReadInteger(CounterSection, CounterRef, 0) + 1;
        // Write the new value back to the inifile
        IniFile.WriteInteger(CounterSection, CounterRef, Result);
        IniFile.UpdateFile;
    finally
        IniFile.Destroy;
    end;
end;

procedure TWebAppSrvDataModule.Display(const Msg: String);
begin
    DisplayHandler(Self, Msg);
end;

procedure TWebAppSrvDataModule.DisplayHandler(
    Sender : TObject; const Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Sender, Msg);
end;

procedure TWebAppSrvDataModule.LoadConfig;
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        FPort := IniFile.ReadString(SectionConfig, KeyPort, DftPort);
    finally
        FreeAndNil(IniFile);
    end;
end;

procedure TWebAppSrvDataModule.SaveConfig;
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteString(SectionConfig, KeyPort, FPort);
        IniFile.UpdateFile;
    finally
        FreeAndNil(IniFile);
    end;
end;

procedure TWebAppSrvDataModule.SetDataDir(const Value: String);
begin
    FDataDir         := Value;
    FCounterFileName := FDataDir + PathDelim + 'Counters.ini';
end;

// Delete a directory, all files it contains as well as all subdirectories
// recursively
procedure ForceRemoveDir(const Dir : String);
var
    F      : TSearchRec;
    Status : Integer;
begin
    Status := FindFirst(IncludeTrailingPathDelimiter(Dir) + '*.*',
                        faAnyFile, F);
    try
        while Status = 0 do begin
            if (F.Attr and faDirectory) <> 0 then begin
                // We have a subdirectory
                if (F.Name <> '.') and (F.Name <> '..') then
                    ForceRemoveDir(Dir + PathDelim + F.Name)
            end
            else
                DeleteFile(Dir + PathDelim + F.Name);
            Status := FindNext(F);
        end;
    finally
        FindClose(F);
    end;
    RemoveDir(Dir);
end;

// Check is a string begins by at least L digits
function IsNumeric(const S : String; L : Integer) : Boolean;
var
    I : Integer;
begin
    Result := TRUE;
    for I := 1 to L do begin
        if I > Length(S) then
            break;
        if not ((S[I] >= '0') and (S[I] <= '9')) then begin
            Result := FALSE;
            break;
        end;
    end;
end;

// Cleanup a directory of his subdirectories having a name which starts by
// a timestamp YYYYMMDDHHNNSS. The cleanup occurs as soon as the timestamp
// if in the past.
procedure CleanupTimeStampedDir(const Dir : String);
var
    TimeStamp : String;
    F      : TSearchRec;
    Status : Integer;
begin
    TimeStamp := FormatDateTime('YYYYMMDDHHNNSS', Now);
    Status := FindFirst(IncludeTrailingPathDelimiter(Dir) + '*.*',
                        faAnyFile, F);
    try
        while Status = 0 do begin
            if (F.Attr and faDirectory) <> 0 then begin
                // We have a subdirectory
                if (F.Name <> '.') and (F.Name <> '..') and
                   (Length(F.Name) >= 14) and
                   (IsNumeric(F.Name, 14)) and
                   (Copy(F.Name, 1, 14) <= TimeStamp) then begin
                    ForceRemoveDir(Dir + PathDelim + F.Name)
                end;
            end;
            Status := FindNext(F);
        end;
    finally
        FindClose(F);
    end;
end;

end.
