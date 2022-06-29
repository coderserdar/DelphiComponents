unit CakDescentHog;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver;

const CanExtract = TRUE;
      CanAdd = FALSE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = 'dummydll.dll';
type
  TCakDescentHog = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    Cakdir : TComponent;
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils, Cakdir2;
procedure TCakDescentHog.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakDescentHog.DoStop(Stopp: Boolean);
begin
end;

procedure TCakDescentHog.Process(dowhat : WorkType);
var
  Buf1:       array[1..3] of Char;
  //Buf2:         array[1..4] of Byte;
  Buf3:       array[1..13] of Char;
  Buf6:       array[1..120] of Byte;
  F, Ff:      file;
  numread:    Longint;
  i, j, loc, fsize: Integer;
  currentpos: Longint;
  function Buf3tostr: string;
  var
    I:      Integer;
    Output: String;
   begin
    Output := '';
    I      := 1;
    while (Buf3[I] <> #0) and (I <= 56) do //----> GC
     begin
      Output := Output + Char(Buf3[I]);
      Inc(I);
     end;
    Result := Output;
   end;
begin
  with (Cakdir as TCakdir2) do
  case doWhat of
    wtLoadContents:
     begin
        Total_Contents := 0;
        FileMode       := fmOpenRead;                                //----> GC
        AssignFile(F, ArchiveName);
        Reset(F, 1);
        currentpos := 0;
        BlockRead(F, Buf1, 3, NumRead);
        Inc(currentpos, numread);
        if Buf1 = 'DHF' then
          while not Eof(f) do
           begin
            Inc(Total_Contents);
            SetLength(Archive_Contents, Total_Contents);
            with Archive_Contents[Total_Contents - 1] do
             begin
              BlockRead(F, Buf3, 13, Numread);
              Inc(currentpos, numread);
              _FileName    := Buf3tostr;
              _FileDefPath := '';
              BlockRead(F, j, SizeOf(j), Numread);
              Inc(currentpos, numread);
              _FileSize    := j;
              _FileTime    := Now;
              _FileCRC     := 'FFFFFFFF';
              Loc          := ReturnIconType(_FileName);
              _FileIcon    := Loc;
              _Filetype    := FileType.Strings[Loc];
              _Tag         := currentpos;
              _FileArchive := ArchiveName;
              Seek(F, currentpos+_FileSize);
              currentpos := currentpos+_FileSize;
             end;
           end;
        CloseFile(f);
       end;
    wtExtract:
     begin
        FileMode := fmOpenRead;
        AssignFile(F, ArchiveName);
        Reset(F, 1);

        for J := 0 to Total_Contents - 1 do
          if Archive_Contents[J]._FileArchive =
            ArchiveName then
            if Archive_Contents[J]._Selected then
             begin
              with Archive_Contents[J] do
               begin                                                 //----> GC
                FileMode := fmOpenWrite;                             //----> GC
                AssignFile(Ff, ExtractOptions.Extr_to+_FileName);    //----> GC
               end;                                                  //----> GC

              Rewrite(Ff, 1);

              Seek(F, Archive_Contents[J]._Tag);
              Fsize := Archive_Contents[J]._FileSize;
              while Fsize >= SizeOf(Buf6) do
               begin
                BlockRead(F, Buf6, SizeOf(Buf6), NumRead);
                Fsize := Fsize - NumRead;
                BlockWrite(FF, Buf6, Numread);
               end;
              if Fsize > 0 then
               begin
                BlockRead(F, Buf6, Fsize, NumRead);
                BlockWrite(FF, Buf6, Numread);
               end;
              CloseFile(Ff);
             end;
        CloseFile(F);
     end;
   end;
end;

function TCakDescentHog.DllExists : boolean;
begin
        Result := True;
end;

function TCakDescentHog.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
        wtNone : Result := true;
        wtTest, wtExtract : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        else Result := false;
        end;
        if Result then
          if not DLLExists then
            Result := false;

end;

function TCakDescentHog.Def_TreatAs : string;
begin
end;

procedure TCakDescentHog.Load_DLL;
begin
end;

procedure TCakDescentHog.UnLoad_DLL;
begin
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakDescentHog]);
end;

end.
 