  {*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Utility Classes for Native Libraries           }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainLoader;

interface

{$I ZPlain.inc}

uses Types,
{$IFDEF FPC}
  dynlibs,
{$ENDIF}
  ZCompatibility;

type
  {$IFDEF FPC}
  THandle = TLibHandle;
  {$ENDIF}
  {** Implements a loader for native library. }

  { TZNativeLibraryLoader }

  TZNativeLibraryLoader = class (TObject)
  private
    FLocations: TStringDynArray;
    FHandle: THandle;  //M.A. LongWord;
    FLoaded: Boolean;
    FCurrentLocation: String;
    function ZLoadLibrary(const Location: String): Boolean;
  protected
    procedure FreeNativeLibrary; virtual;
  public
    constructor Create(const Locations: array of string);
    destructor Destroy; override;

    procedure ClearLocations;
    procedure AddLocation(const Location: String);
    function Load: Boolean; virtual;
    function LoadNativeLibrary: Boolean; virtual;
    function LoadNativeLibraryStrict(const Location: String): Boolean;
    procedure LoadIfNeeded; virtual;

    property Loaded: Boolean read FLoaded write FLoaded;
    property Handle: THandle { M.A. LongWord} read FHandle write FHandle;
    property CurrentLocation: String read FCurrentLocation write FCurrentLocation;
    function GetAddress(ProcName: PAnsiChar): Pointer;
  end;

implementation

uses SysUtils, 
{$IFNDEF UNIX} 
  Windows, 
{$ELSE} 
  {$IFNDEF FPC} 
    libc, 
  {$ENDIF} 
{$ENDIF}
  ZMessages;

{ TZNativeLibraryLoader }

{**
  Creates this loader class and assignes main properties.
  @param Locations locations of native library on windows platform.
}
constructor TZNativeLibraryLoader.Create(const Locations: array of string);
var
  I: Integer;
begin
  SetLength(FLocations, Length(Locations));
  for I := 0 to High(Locations) do
    FLocations[I] := Locations[I]; 
  FHandle := INVALID_HANDLE_VALUE;
  FCurrentLocation := '';
  FLoaded := False;
end;

{**
  Destroys the library and cleanups the memory.
}
destructor TZNativeLibraryLoader.Destroy;
begin
  if Loaded then
    FreeNativeLibrary;
  inherited Destroy;
end;

procedure TZNativeLibraryLoader.ClearLocations;
begin
  SetLength(FLocations,0);
end;

procedure TZNativeLibraryLoader.AddLocation(const Location: String);
var
   i: integer;
begin
  if Location <> '' then
  begin
    SetLength(FLocations, Length(FLocations) + 1);
    for i := High(FLocations) downto 1 do
      FLocations[i] := FLocations[i - 1];
    FLocations[0] := Location;
  end;
end;

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZNativeLibraryLoader.Load: Boolean;
begin
  Result := LoadNativeLibrary;
end;

{**
  Loads a library if it was not previously loaded.
}
procedure TZNativeLibraryLoader.LoadIfNeeded;
begin
  if not Loaded then
    Load;
end;

function TZNativeLibraryLoader.ZLoadLibrary(const Location: String): Boolean;
var newpath, temp: String; // AB modif
begin
  if FLoaded then
    Self.FreeNativeLibrary;
  temp := ''; //init for FPC
  FLoaded := False;
  Result := False;
  newpath := ExtractFilePath(Location);
  // AB modif BEGIN
  try
   if newpath <> '' then begin
     temp := GetCurrentDir;
     SetCurrentDir(newpath);
   end;
  // AB modif END

{$IFDEF UNIX}
  {$IFDEF FPC}
    FHandle := LoadLibrary(PAnsiChar(Location));
  {$ELSE}
    FHandle := HMODULE(dlopen(PAnsiChar(Location), RTLD_GLOBAL));
  {$ENDIF}
{$ELSE}
  FHandle := LoadLibrary(PChar(Location));
{$ENDIF}

  // AB modif BEGIN
  finally
   if temp<>'' then
     SetCurrentDir(temp);
  end;
  // AB modif END
  if (FHandle <> INVALID_HANDLE_VALUE) and (FHandle <> 0) then
  begin
    FLoaded := True;
    FCurrentLocation := Location;
    Result := True;
  end;
end;
{**
  Loads a library module and initializes the handle.
  @return <code>True</code> is library was successfully loaded.
}
function TZNativeLibraryLoader.LoadNativeLibrary: Boolean;
var
  I: Integer;
  TriedLocations: string;
begin
  TriedLocations := '';
  for I := 0 to High(FLocations) do
    begin
      if ZLoadLibrary(FLocations[I]) then
        Break
      else
        if TriedLocations <> '' then
          TriedLocations := TriedLocations + ', ' + FLocations[I]
        else
          TriedLocations := FLocations[I];
    end;

  if not Loaded then
    if (Length(FLocations) > 0) and FileExists(FLocations[High(FLocations)]) then
      raise Exception.Create(Format(SLibraryNotCompatible, [TriedLocations]))
    else
      raise Exception.Create(Format(SLibraryNotFound, [TriedLocations]));
  Result := True;
end;

function TZNativeLibraryLoader.LoadNativeLibraryStrict(const Location: String): Boolean;
begin
  If not ZLoadLibrary(Location) then
    if FileExists(Location) then
      raise Exception.Create(Format(SLibraryNotCompatible, [Location]))
    else
      raise Exception.Create(Format(SLibraryNotFound, [Location]));
  Result := True;
end;

{**
  Frees a previously loaded library.
}
procedure TZNativeLibraryLoader.FreeNativeLibrary;
begin
  if (FHandle <> INVALID_HANDLE_VALUE) and (FHandle <> 0) and Loaded then
    FreeLibrary(Handle);
  FHandle := INVALID_HANDLE_VALUE;
  FLoaded := False;
  FCurrentLocation := '';
end;

{**
  Gets a procedure address from the loaded library by its name.
  @param ProcName a name of the procedure.
  @return a procedure address.
}
function TZNativeLibraryLoader.GetAddress(ProcName: PAnsiChar): Pointer;
begin
  Result := GetProcAddress(Handle, ProcName);
end;

end.



