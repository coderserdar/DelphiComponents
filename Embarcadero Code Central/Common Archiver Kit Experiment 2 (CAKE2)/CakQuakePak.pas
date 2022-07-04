unit CakQuakePak;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver;

const CanExtract = TRUE;
      CanAdd = FALSE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = 'dummydll.dll';
type
  TCakQuakePak = class(TCakArchiver)
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
uses CakUtils2, Cakdir2;

procedure TCakQuakePak.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakQuakePak.DoStop(Stopp: Boolean);
begin
end;

procedure TCakQuakePak.Process(dowhat : WorkType);
var
  Buf2:         array[1..4] of Byte;
  Buf3:         array[1..56] of Char;
  Buf4:         array[1..120] of Char;
  Buf5:         array[1..16] of Char;
  Buf6:         array[1..120] of Byte;
  Sign:         Longint;
  F, Ff:        file;
  Fsize:        Longint;
  NumRead, Offset, Contents: Longint;
  {I,} J,{ K,} Loc: Integer;
  PAKAge:       Integer;                                             //----> GC
  PAKDateTime:  TDateTime;                                           //----> GC
  PAKHandle:    Integer;                                             //----> GC
  function HexToInt(HexStr: string): Longint;
  var
    S: String;
   begin
    S      := '$' + HexStr;
    Result := StrToInt(S);
   end;
  function Buf5tostr: string;
  var
    I:      Integer;
    Output: String;
   begin
    Output := '';
    I      := 1;
    while (Buf5[I] <> #0) and (I <= 16) do //----> GC
     begin
      Output := Output + Char(Buf5[I]);
      Inc(I);
     end;
    Result := Output;
   end;
  function Buf4tostr: string;
  var
    I:      Integer;
    Output: String;
   begin
    Output := '';
    I      := 1;
    while (Buf4[I] <> #0) and (I <= 120) do //----> GC
     begin
      Output := Output + Char(Buf4[I]);
      Inc(I);
     end;
    Result := Output;
   end;
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
  function Buf2toint: Integer;
  var
    X:      Byte;
    S:      String;
    I:      Integer;
    HexStr: String;
   begin
    HexStr := '';
    for I := 4 downto 1 do
     begin
      X      := Buf2[I];
      S      := IntToHex(X, 2);                                      //----> GC
      HexStr := HexStr + S;
     end;
    Result := HexToInt(HexStr);
   end;
  procedure LoadPAK;
  var
    I: Integer;
  begin
   with (Cakdir as TCakdir2) do
   begin
    BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
    Offset := Buf2ToInt;
    BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
    Contents := Buf2ToInt div 64;
    if Fsize >= Offset + Contents then
     begin
      Seek(F, Offset);
      Inc(Total_Contents, Contents);
      SetLength(Archive_Contents, Total_Contents);
      for I := 0 to Contents - 1 do
       begin
        Archive_Contents[I] := InitContentType;
        with Archive_Contents[I] do
         begin
          if (Sign = $4b415053) then
           begin
            BlockRead(F, Buf4, SizeOf(Buf4), NumRead);
            _FileName    := ExtractFileName(ModifySlash(Buf4tostr));
            _FileDefPath := ExtractFilePath(ModifySlash(Buf4tostr));
           end
          else
           begin
            BlockRead(F, Buf3, SizeOf(Buf3), NumRead);
            _FileName    := ExtractFileName(ModifySlash(Buf3tostr));
            _FileDefPath := ExtractFilePath(ModifySlash(Buf3tostr));
           end;
          Loc       := ReturnIconType(_FileName);
          _FileIcon := Loc;
          _Filetype := FileType.Strings[Loc];
          if DirectoryList.IndexOf(_FileDefPath) = -1 then
            if (_FileDefPath) <> '' then
              DirectoryList.Add(_FileDefPath);
          BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
          _Tag := Buf2toint;
          BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
          _FileSize    := Buf2toInt;
          _FileTime    := PAKDateTime;                               //----> GC
          _FileArchive := Archivename;
         end;
       end;
     end;
   end;
  end;
  procedure LoadWAD;
  var
    I,J:     Integer;
    Dummy: String[8];
  begin
  with (Cakdir as TCakdir2) do
   begin
    BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
    Contents := Buf2ToInt;
    BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
    Offset := Buf2ToInt;
    if Fsize >= Offset + Contents * $20 then
     begin
      Seek(F, Offset);
      Inc(Total_Contents, Contents);
      SetLength(Archive_Contents, Total_Contents);
      for I := 0 to Contents - 1 do
       begin
        Archive_Contents[I] := InitContentType;
        with Archive_Contents[I] do
         begin
          BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
          _Tag := Buf2toint;
          BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
          _FileSize := Buf2toInt;
          BlockRead(F, Dummy, 8, NumRead);
          BlockRead(F, Buf5, SizeOf(Buf5), NumRead);
          _FileName    := ExtractFileName(ModifySlash(Buf5tostr));
          _FileDefPath := ExtractFilePath(ModifySlash(Buf5tostr));
          _FileTime    := PAKDateTime;          // ----> GC
          _FileArchive := Archivename;
          Loc          := ReturnIconType(_FileName);
          _FileIcon    := Loc;
          _Filetype    := FileType.Strings[Loc];
          if (_FileDefPath) <> '' then
          if DirectoryList.IndexOf(_FileDefPath) = -1 then
              DirectoryList.AddObject(_FileDefPath,TObject(_FileSize) ) else
              begin
              j := DirectoryList.IndexOf(_FileDefPath);
              DirectoryList.Objects[j] := TObject(Longint(DirectoryList.Objects[j]) + _FileSize);
              end;
         end;
       end;
     end;
   end;
  end;
  procedure LoadIWAD;
  var
    I,J: Integer;
   begin
   with (Cakdir as TCakdir2) do
   begin
    BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
    Contents := Buf2ToInt;
    BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
    Offset := Buf2ToInt;
    if Fsize >= Offset + Contents * $10 then
     begin
      Seek(F, Offset);
      Inc(Total_Contents, Contents);
      SetLength(Archive_Contents, Total_Contents);
      for I := 0 to Contents - 1 do
       begin
        Archive_Contents[I] := InitContentType;
        with Archive_Contents[I] do
         begin
          BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
          _Tag := Buf2toint;
          BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
          _FileSize := Buf2toInt;
          FillChar(Buf5, SizeOf(Buf5), 0);                           //----> GC
          BlockRead(F, Buf5, 8, NumRead);
          _FileName    := ExtractFileName(ModifySlash(Buf5tostr));
          _FileDefPath := ExtractFilePath(ModifySlash(Buf5tostr));
          _FileTime    := PAKDateTime;                               //----> GC
          _FileArchive := ArchiveName;
          Loc          := ReturnIconType(_FileName);
          _FileIcon    := Loc;
          _Filetype    := FileType.Strings[Loc];
          if (_FileDefPath) <> '' then
          if DirectoryList.IndexOf(_FileDefPath) = -1 then
              DirectoryList.AddObject(_FileDefPath,TObject(_FileSize) ) else
              begin
              j := DirectoryList.IndexOf(_FileDefPath);
              DirectoryList.Objects[j] := TObject(Longint(DirectoryList.Objects[j]) + _FileSize);
              end;
         end;
       end;
     end;
   end;
  end;
  procedure LoadUNKNOWN;
  var
    I,J:       Integer;
    Test1:    Integer;
    RecSize: Longint;
    Dummy:   String[4];

   begin
   with (Cakdir as TCakdir2) do
   begin
    BlockRead(F, Test1, 4, NumRead);
    if (Test1 and $ffffff) <> $464650 then
      Exit;
    BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
    Contents := Buf2ToInt div 64;;
    BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
    RecSize := Buf2ToInt;
    BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
    Offset := Buf2ToInt;
    if Fsize >= Offset + Contents * RecSize then
     begin
      Seek(F, Offset);
      Inc(Total_Contents, Contents);
      SetLength(Archive_Contents, Total_Contents);
      for I := 0 to Contents - 1 do
       begin
        Archive_Contents[I] := InitContentType;
        with Archive_Contents[I] do
         begin
          BlockRead(F, Dummy, 4, NumRead);
          BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
          _Tag := Buf2toint;
          BlockRead(F, Buf2, SizeOf(Buf2), NumRead);
          _FileSize := Buf2toInt;
          BlockRead(F, Dummy, 4, NumRead);
          BlockRead(F, Buf5, SizeOf(Buf5), NumRead);
          _FileName    := ExtractFileName(ModifySlash(Buf5tostr));
          _FileDefPath := ExtractFilePath(ModifySlash(Buf5tostr));
          _FileTime    := PAKDateTime;                               //----> GC
          _FileArchive := ArchiveName;
          Loc          := ReturnIconType(_FileName);
          _FileIcon    := Loc;
          _Filetype    := FileType.Strings[Loc];
          if (_FileDefPath) <> '' then
          if DirectoryList.IndexOf(_FileDefPath) = -1 then
              DirectoryList.AddObject(_FileDefPath,TObject(_FileSize) ) else
              begin
              j := DirectoryList.IndexOf(_FileDefPath);
              DirectoryList.Objects[j] := TObject(Longint(DirectoryList.Objects[j]) + _FileSize);
              end;
         end;
       end;
     end;
   end;
   end;
begin
  with (Cakdir as TCakdir2) do
  case doWhat of
    wtLoadContents:
     begin
      DirectoryList.Clear;
      Total_Contents := 0;
      FileMode       := fmOpenRead;                                //----> GC
      AssignFile(F, ArchiveName);
      Reset(F, 1);
      Fsize       := FileSize(F);
      PAKAge      := FileAge(ArchiveName);            //----> GC
      PAKDateTime := FileDateToDateTime(PAKAge);                   //----> GC
      BlockRead(F, Sign, 4, NumRead);
      case Sign of
          $4b434150, $4b415053:
            LOADPAK;
          $32444157, $33444157:
            LOADWAD;
          $44415749, $44415750:
            LOADIWAD;
          else
            LOADUNKNOWN;
         end;
        CloseFile(F);
       end;
    wtExtract:
     begin
        FileMode := fmOpenRead;                                      //----> GC
        AssignFile(F, ArchiveName);
        Reset(F, 1);
        Fsize  := FileSize(F);
        PAKAge := FileAge(ArchiveName);                 //----> GC
        for J  := 0 to Total_Contents - 1 do
          if Archive_Contents[J]._FileArchive =
            ArchiveName then
            if Archive_Contents[J]._Selected then
             begin
              with Archive_Contents[J] do
                if ExtractOptions.Extr_DirNames then
                 begin
                  ForceDirectories(ExtractOptions.Extr_to+_Filedefpath);//--> GC
                  FileMode := fmOpenWrite;                           //----> GC
                  AssignFile(Ff, ExtractOptions.Extr_to+_Filedefpath+_FileName)
                 end
              else
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
              // Set for extracted files the same date as the archive's date
              with Archive_Contents[J] do // ----> GC
                if ExtractOptions.Extr_DirNames then  // ----> GC
                  PAKHandle := FileOpen(ExtractOptions.Extr_to+_Filedefpath+_FileName,
                    fmOpenWrite or fmShareDenyNone)                  //----> GC
              else                                                 //----> GC
                PAKHandle := FileOpen(ExtractOptions.Extr_to+_FileName,
                  fmOpenWrite or fmShareDenyNone);                 //----> GC
              if PAKHandle > 0 then                                  //----> GC
                FileSetDate(PAKHandle, PAKAge);                      //----> GC
              FileClose(PAKHandle);                                  //----> GC
             end;
        CloseFile(F);
       end;
     end;
end;

function TCakQuakePak.DllExists : boolean;
begin
        Result := True;
end;

function TCakQuakePak.Cando(aWork: WorkType): Boolean;
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

function TCakQuakePak.Def_TreatAs : string;
begin
end;

procedure TCakQuakePak.Load_DLL;
begin
end;

procedure TCakQuakePak.UnLoad_DLL;
begin
end;

procedure Register;
begin
  //RegisterComponents('QZip', [TCakQuakePak]);
end;

end.
 