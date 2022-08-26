unit FindFile;

//   FindFile version 1.0.1
//
//   Copyright (C) September 1997 Walter Dorawa
//
//   Everyone is free to use this code as they wish, but
//   if you use it commercially then I wouldn't mind a
//   little something.
//
//   Please submit suggestions, bugs, or any improvements to
//   walterd@gte.net
//
//   Improvements:  10-21-97
//         Attributes property               TotalFile
//         Abort property                    TotalSpace
//         OnNewPath event                   TotalDir
//
//         thanks to: Howard Harvey, Jim Keatley and Dale Derix
//         for suggestions and code improvements
//
//++++ Note: Walter Dorawa no longer appears to have an email address.
//           If you manage to find him, please let me know (hh)
//
//   Version 1.10 (hh) 23/MAY/1999 modification by Howard Harvey
//   Improvements
//         Defualt values specifically initialised in Create
//         Sorted property   (default true)
//         StrictRO property   (default false)
//         Attributes set changed
//         Code appearance cleanup
//
//   Version 1.20 (hh) 17/APRIL/2000 (suggestion from Marcus Luk)
//   Improvements:
//         Added trailing path delimiter in SearchCurrentDirectory
//
//   Version 1.30 (hh) 15/FEBRUARY/2001 (suggestion from rattus)
//   Improvements:
//         Allows multiple filters separated by semicolons:
//         Duplicate entries automatically excluded if Sorted := true
//

interface

uses
  Classes, SysUtils, Dialogs;

type
  TAttrOption = (ffReadOnly, ffHidden, ffSysFile, ffVolumeID,
                 ffDirectory);
  TAttrOptions = set of TAttrOption;
  TNewPathEvent = procedure(Sender: TObject; NewPath: string;
                             var Abort: boolean) of object;

  ThhFindFile = class(TComponent)
  private
    { Private declarations }
    FAbort:boolean;
    FTotalSpace:longint;
    FTotalDir:longint;
    FTotalFile:longint;
    FAttribs:TAttrOptions;
    FDirectory:string;
    FRecurse: boolean;
    FFilter : string;
    FFiles  : TStrings;
    FSorted : boolean;
    FStrictRO : boolean;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    FOnNewPath: TNewPathEvent;
    procedure SearchCurrentDirectory(Directory:string);
    procedure SearchRecursive(Directory:string);
    function FindSubDirectory(strDirs:TStringList; Directory:string):Boolean;
  protected
    { Protected declarations }
    procedure SetFiles(Value: TStrings);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; dynamic;
    
    property Abort: boolean read FAbort write FAbort default False;
    property Files: TStrings read FFiles write SetFiles;
    property TotalDir: longint read FTotalDir write FTotalDir;
    property TotalFile: longint read FTotalFile write FTotalFile;
    property TotalSpace: longint read FTotalSpace write FTotalSpace;
  published
    { Published declarations }
    property Attributes: TAttrOptions read FAttribs write FAttribs
             default [ffReadOnly, ffHidden, ffSysFile];
    property Directory: string read FDirectory write FDirectory;
    property Filter: string read FFilter write FFilter;
    property Recurse: boolean read FRecurse write FRecurse default False;
    property Sorted: boolean read FSorted write FSorted default True;
    property StrictRO: boolean read FStrictRO write FStrictRO default False;

    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
    property OnNewPath: TNewPathEvent read FOnNewPath write FOnNewPath;
  end;

procedure Register;

{ -------------------------------------------------------------------- }

implementation

const
  DefaultFilter = '*.*';

var
  Attribs : integer;

{ -------------------------------------------------------------------- }

constructor ThhFindFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAbort   := False;
  FRecurse := False;
  FSorted  := True;
  FStrictRO  := False;
  FFilter  := DefaultFilter;
  FAttribs := [ffReadOnly, ffHidden, ffSysFile];
  FFiles   := TStringList.Create;
end;

{ -------------------------------------------------------------------- }

destructor ThhFindFile.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

{ -------------------------------------------------------------------- }

procedure ThhFindFile.SetFiles(Value: TStrings);
begin
  FFiles.Assign(Value);
end;

{ -------------------------------------------------------------------- }

procedure ThhFindFile.Execute;
begin
  FAbort  := false ;
  Attribs := 0;
  if ffReadOnly  in Attributes then Attribs:=Attribs OR faReadOnly;
  if ffHidden    in Attributes then Attribs:=Attribs OR faHidden;
  if ffSysFile   in Attributes then Attribs:=Attribs OR faSysFile;
  if ffVolumeID  in Attributes then Attribs:=Attribs OR faVolumeID;
  if ffDirectory in Attributes then Attribs:=Attribs OR faDirectory;

  FFiles.Clear;
  FTotalSpace :=0;
  FTotalDir   :=0;
  FTotalFile  :=0;
  if Assigned(FBeforeExecute) then FBeforeExecute(Self);
  if Length(FDirectory)<>0
  then begin
    if FRecurse
    then SearchRecursive(FDirectory)
    else SearchCurrentDirectory(FDirectory);
  end;
  if Assigned(FAfterExecute) then FAfterExecute(Self);
end;

{ -------------------------------------------------------------------- }

procedure ExtractItem( var sInput : string ; var sOutput : string ) ;
begin
  if POS(';',sInput) > 0
    then sOutput := COPY( sInput , 1 , POS(';',sInput)-1 )
    else sOutput := sInput ;

{ Remove any leading spaces }

  while (length(sOutput) > 0) AND (sOutput[1] = ' ') do
    DELETE( sOutput , 1 , 1 ) ;

{ Remove any trailing spaces }

  while (length(sOutput) > 0) AND (sOutput[length(sOutput)] = ' ') do
    DELETE( sOutput , length(sOutput) , 1 ) ;

{ Clear the input field }
  if POS(';',sInput) > 0
    then DELETE( sInput , 1 , POS(';',sInput) )
    else sInput := '' ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhFindFile.SearchCurrentDirectory(Directory:string);
var
  ix       : word;
  cnt      : word;
  Fattr    : integer;
  srchRec  : TSearchRec;
  sfName   : string;
  sFullFilter : string ;
  sThisFilter : string ;
  strFiles : TStringList;

function StrictROCheck( ThisAttr : integer ) : boolean ;
begin
  Result := true ;
  if FStrictRO
  then begin
{ ReadOnly is part of "normal files" so must be
  validated under "StrictRO" rule }
    if (Attribs AND faReadOnly) <> 0
    then exit
    else if (ThisAttr AND faReadOnly) <> 0
    then begin
      Result := false ;
      exit ;
    end ;
  end ;
end;

begin
  if Directory[Length(Directory)]<>'\' then AppendStr(Directory,'\');
  if Assigned(FOnNewPath) then FOnNewPath(Self,Directory,FAbort);
  if FAbort then Exit;
  if FSorted
  then begin
    strFiles:=TStringList.Create;
    strFiles.Sorted := true;
    strFiles.Duplicates := dupIgnore;
  end;
  sFullFilter := FFilter ;
  while length(sFullFilter) > 0 do
  begin
    ExtractItem( sFullFilter,sThisFilter ) ;
    if length(sThisFilter) = 0 then continue;
    ix:=FindFirst(Directory+sThisFilter,Attribs,srchRec);
    while ix=0 do
    begin
      sfName := srchRec.Name;
      if (sfName<>'.') AND (sfName<>'..')
      then begin
        Fattr := srchRec.Attr ;
        if StrictROCheck( Fattr )
        then begin
{ V1.20: If sfName is a Directory then add a '\' }
          if (Fattr AND faDirectory) = faDirectory then
             if sfName[Length(sfName)]<>'\' then AppendStr(sfName,'\');
          if FSorted
          then strFiles.Add(Directory+sfName)
          else FFiles.Add(Directory+sfName);
          case (Fattr AND faDirectory) of
            faDirectory: Inc(FTotalDir);
            else         Inc(FTotalFile);
          end;
          FTotalSpace:=FTotalSpace+srchRec.Size;
        end ;
      end;
      ix:=FindNext(srchRec);
    end;
    FindClose(srchRec);
  end ;
  if FSorted
  then begin
    cnt:=strFiles.Count;
    for ix := 1 to cnt do
    begin
      FFiles.Add(strFiles.Strings[0]);
      strFiles.Delete(0);
    end;
    strFiles.Free;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhFindFile.SearchRecursive(Directory:string);
var
  strDirs       : TStringList;
  ThisDirectory : string;
begin
  strDirs:=TStringList.Create;
  try
    if Directory[Length(Directory)]<>'\' then AppendStr(Directory,'\');
    strDirs.Clear;
    strDirs.Add(Directory);
    while strDirs.Count<>0 do
    begin
      ThisDirectory:=strDirs.Strings[0];
      strDirs.Delete(0);
      FindSubDirectory(strDirs,ThisDirectory);
      SearchCurrentDirectory(ThisDirectory);
      if FSorted then strDirs.Sort;
      if FAbort then Exit;
    end;
  finally
    strDirs.Free;
  end;
end;

{ -------------------------------------------------------------------- }

function ThhFindFile.FindSubDirectory(strDirs:TStringList; Directory:string):Boolean;
var
  ix      : word;
  srchRec : TSearchRec;
  sdName  : string ;
  InsAt   : word;
begin
  Result:=True;
  InsAt:= 0 ;
  if Directory[Length(Directory)]<>'\' then AppendStr(Directory,'\');
  ix:=FindFirst(Directory+'*.*',faAnyFile,srchRec);
  while ix=0 do
  begin
    sdName := srchRec.Name ;
    if ((srchRec.Attr AND faDirectory)>0) AND (sdName<>'.') AND (sdName<>'..')
    then begin
      if FSorted
      then strDirs.Insert(InsAt,Directory+sdName)
      else strDirs.Add(Directory+sdName);
      INC(InsAt);
    end;
    ix:=FindNext(srchRec);
  end;
  FindClose(srchRec);
end;

{ ==================================================================== }

procedure Register;
begin
  RegisterComponents('Howie', [ThhFindFile]);
end;

end.
