{$DEFINE USE_SEARCH}
unit CakFinder;

interface

uses
  {$IFDEF USE_SEARCH} Searches, {$ENDIF}
  Windows, Messages, SysUtils, Classes, CakDir2, CakDefs2;

type
  TCakFinder = class(TComponent)
  private
    { Private declarations }
    FinderOptions: FinderOptionsType;
    FOnFound: TCFoundEvent;
    FOnFinished : TNotifyEvent;
  protected
    { Protected declarations }
  public
    { Public declarations }
    Targetname : Tstrings;
    CakDir: TCakDir2;
    procedure Find;
    procedure FindStop;
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;


  published
    { Published declarations }
    property CakDir2 : TCakDir2 read CakDir write CakDir;

    property Targetcontaintext : string read FinderOptions.Af_Text2Look
                                        write FinderOptions.Af_Text2Look;
    property SourceDir  : string read FinderOptions.Af_SourceDir
                                 write FinderOptions.Af_SourceDir;
    property SubDir  : boolean read FinderOptions.Af_SubDir
                               write FinderOptions.Af_SubDir;
    property Archive2Search : string read FinderOptions.Af_ArcFilter
                                     write FinderOptions.Af_ArcFilter;
    property OnCArchiveFound: TCFoundEvent read FOnFound write FOnFound;
    property OnCSearchCompleted : TNotifyEvent read FOnFinished write FOnFinished;
  end;

TFinder = class(TThread)
  private
    FOnFound: TCFoundEvent;
    FOption: FinderOptionsType;
    FOnFinish : TNotifyEvent;
    CakDir: TCakDir2;
    procedure Search(Dir: string);
  protected

  public
    constructor Create(CreateSuspended: Boolean);
    procedure Execute; override;
    destructor Destroy; override;
  published
    property FinderOption: FinderOptionsType read FOption write FOption;
    property OnCArchiveFound: TCFoundEvent read FOnFound write FOnFound;
    property OnCSearchCompleted : TNotifyEvent read FOnFinish write FOnFinish;
  end;

procedure Register;

implementation
uses Cakutils2, Forms, Dialogs;
var aFinder : TFinder;

constructor TFinder.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

destructor TFinder.Destroy;
begin
  inherited Destroy;
end;

procedure TFinder.Search(Dir: string);
var
  sr:       TSearchRec;
  k:        String;
  FileAttrs, i, j, l: Integer;
  aStrings: TStrings;
  aList:    TStrings;
  {$IFDEF USE_SEARCH} TextSearch: TFileSearch; {$ENDIF}
  {$IFDEF USE_SEARCH}
  procedure usesearch;
   begin
    if FOption.Af_Text2Look = '' then
      FOnFound(NIL, Dir + sr.Name, sr.Size)
    else
     begin
      CakDir.UnSelectAll;
      CakDir.Select('*' + FOption.Af_TargetName.Strings[i],
        '');
      CakDir.ExtractOptions.Extr_DirNames   := False;
      CakDir.ExtractOptions.Extr_Overwrite  := True;
      CakDir.ExtractOptions.Extr_ArcInArc   := False;
      CakDir.ExtractOptions.Extr_ExtractAll := False;
      CakDir.ExtractOptions.Extr_to         := CakDir.NewTempPath;
      CakDir.Extract;

      TextSearch := TFileSearch.Create(NIL);
      TextSearch.pattern := FOption.Af_Text2Look;
      TextSearch.FileName := CakDir.ExtractOptions.Extr_to +
        FOption.Af_TargetName.Strings[i];
      l := TextSearch.FindFirst;
      TextSearch.FileName := '';
      TextSearch.Free;
      TextSearch := NIL;

      if l <> POSITION_EOF then
       begin
        FOnFound(NIL, Dir + sr.Name, sr.Size);
       end;      
     end;
   end;
  {$ENDIF}
begin
  if terminated then Exit;

  aList           := TStringList.Create;
  aList.CommaText := FOption.Af_ArcFilter;
  for j := 0 to aList.Count - 1 do
   begin
    k         := AppendSlash(Dir) + aList.Strings[j];
    FileAttrs := 0;
    FileAttrs := FileAttrs and faAnyFile;

    if FindFirst(k, FileAttrs, sr) = 0 then
     begin
      if FileExists(AppendSlash(Dir) + sr.Name) then
       begin
        //CakDir.Load(AppendSlash(Dir) + sr.Name);
        //CakDir.Total_Contents := 0;
        if CakDir.CanDo(sr.Name,
          wtLoadContents) then
          CakDir.Load(AppendSlash(Dir) + sr.Name);

        if CakDir.Total_Contents > 0 then
          for i := 0 to FOption.Af_TargetName.Count - 1 do
            if CakDir.Locate(FOption.Af_TargetName.Strings[i]) <> -1  then
        {$IFDEF USE_SEARCH}
              usesearch;
        {$ELSE}
        FOnFound(NIL, Dir + sr.Name, sr.Size);
        {$ENDIF}
       end;
      while (FindNext(sr) = 0) and not terminated do
          if FileExists(AppendSlash(Dir) + sr.Name) then
         begin
          if CakDir.CanDo(sr.Name,
          wtLoadContents) then
          CakDir.Load(AppendSlash(Dir) + sr.Name);

          for i := 0 to FOption.Af_TargetName.Count - 1 do
            if CakDir.Locate(FOption.Af_TargetName.Strings[i]) <> -1 then
            {$IFDEF USE_SEARCH}
              usesearch;
          {$ELSE}
          FOnFound(NIL, Dir + sr.Name, sr.Size);
          {$ENDIF}
         end;
      FindClose(sr);
     end;
   end;
  aList.Free;

  Application.ProcessMessages;
  if FOption.Af_SubDir then
   begin
    aStrings := SubDirList(Dir);
    if aStrings.Count > 0 then
      for i := 0 to aStrings.Count - 1 do
        if not terminated then
         begin
          Search(aStrings.Strings[i]);
          Application.ProcessMessages;
          FOnFound(NIL, AppendSlash(aStrings.Strings[i]), 0);
         end;
    aStrings.Free;
   end;
   if Assigned(FonFinish) then
        FOnFinish(nil);
end;

procedure TFinder.Execute;
begin
  if Assigned(FOnFound) then
   begin
    Search(FOption.Af_SourceDir);
    FOnFound(NIL, '*COMPLETED*', - 1);
   end
  else
    ShowMessage('Error : Unassigned found event');
end;

procedure TCakFinder.Find;
begin
  if not Assigned(Cakdir) then exit;
  aFinder         := TFinder.Create(True);

  aFinder.CakDir := Cakdir;
  aFinder.OnCArchiveFound := FOnFound;
  aFinder.OnCSearchCompleted := FonFinished;
  aFinder.FOption := FinderOptions;
  FinderOptions.Af_SourceDir := AppendSlash(FinderOptions.Af_SourceDir);
  aFinder.FOption.Af_TargetName := TStringList.Create;
  aFinder.FOption.Af_TargetName.AddStrings(TargetName);
  aFinder.FreeOnTerminate := True;
  aFinder.Execute;
end;

constructor TCakFinder.Create( AOwner: TComponent );
begin
        inherited Create(aowner);
        TargetName := TStringList.create;
end;

destructor TCakFinder.Destroy;
begin
        TargetName.free;
        inherited Destroy;
end;



procedure TCakFinder.FindStop;
begin
  aFinder.Terminate;
end;


procedure Register;
begin
  RegisterComponents('CAKE', [TCakFinder]);
end;

end.
