unit CakIndy8;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver,
  IdBaseComponent, IdCoder, IDGlobal, IdCoder3To4;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = FALSE;
type
  TCakIndy = class(TCakArchiver)
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
    
    function ProcessUUE(ProcessWhat: WorkType): Boolean;
    function ProcessB64(ProcessWhat: WorkType): Boolean;
    function ProcessXXE(ProcessWhat: WorkType): Boolean;
  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2;
procedure TCakIndy.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);
end;
procedure TCakIndy.DoStop(Stopp: Boolean);
begin
end;

procedure TCakIndy.Process(dowhat : WorkType);
begin
    if (Uppercase(Extractfileext(TCakdir2(Cakdir).Archivename)) = '.UUE') or
       (Uppercase(Extractfileext(TCakdir2(Cakdir).Archivename)) = '.UU')
    then
       ProcessUUE(doWhat) else
     if (Uppercase(Extractfileext(TCakdir2(Cakdir).Archivename)) = '.XXE') or
       (Uppercase(Extractfileext(TCakdir2(Cakdir).Archivename)) = '.XX')
     then
       ProcessXXE(doWhat) else
       ProcessB64(doWhat);
end;

function TCakIndy.DllExists : boolean;
begin
        Result := TRUE;
end;

function TCakIndy.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
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

function TCakIndy.Def_TreatAs : string;
begin
  result := '';
end;

procedure TCakIndy.Load_DLL;
begin
end;

procedure TCakIndy.UnLoad_DLL;
begin
end;
function TCakIndy.ProcessUUE(ProcessWhat: WorkType): Boolean;
var 
  IDUUDecoder1: TIDUUDecoder;
  IDUUEncoder1: TIDUUEncoder;
  s, k, x: String;
  t:  array[0..44] of Char;
  temp : char;
  tf: textfile;
  fn: String;
  loc, i, fz, Count: Integer;
  bf: file;
  Fs: TFileStream;
begin
  Result := True;
  with (Cakdir as TCakdir2) do
  case ProcessWhat of
    wtLoadContents: 
     begin
      Total_Contents := 0;
       begin
        AssignFile(tf, ArchiveName);
        Reset(tf);
        fz           := FileSize(tf);
        fn           := '';
        IDUUDecoder1 := TIDUUDecoder.Create(NIL);

        with IDUUDecoder1 do
         begin
          AutocompleteInput := False;
          Reset;
          while not Eof(tf) and (fn = '') do
             begin
              Readln(tf, k);
            s := CodeString(k + #13);
            s := CompletedInput;
            s := CompletedInput;
            if FileName <> '' then fn := FileName;
           end;
         end;
        CloseFile(tf);
        IDUUDecoder1.Free;
        TCakdir2(Cakdir).AddContents(ArchiveName,'',fn,fz,fz,false,Now,'FFFFFFFF');
       end;
     end;
    wtExtract: 
     begin
        if ExtractOptions.Extr_ExtractAll or Archive_Contents[0]._Selected then
         begin
          AssignFile(tf, ArchiveName);
          Reset(tf);
          fn           := '';
          IDUUDecoder1 := TIDUUDecoder.Create(NIL);
          with IDUUDecoder1 do
           begin
            while not Eof(tf) and (fn = '') do
               begin
                AutocompleteInput := False;
              Reset;
              Readln(tf, k);

              if UpperCase(k) = 'TABLE' then
               begin
                x := '';
                s := '';
                while not Eof(tf) and not (UpperCase(Copy(s, 0, 7)) = 'BEGIN 6') do
                   begin
                    x := x + s;
                    Readln(tf, s);
                   end;
                Read(tf,Temp);
                x := x + temp;
                s := s + temp;
                Read(tf,Temp);
                x := x + temp;
                s := s + temp;
                SetCodingtable(x);
                k := s;
               end;

              if UpperCase(Copy(k, 0, 7)) = 'BEGIN 6' then
               begin
                s := CodeString(k + #13);
                s := CompletedInput;
                s := CompletedInput;
                if FileName <> '' then fn := FileName;
               end;
             end;

            s := AppendSlash(ExtractOptions.Extr_to) + fn;
            AssignFile(bf, s);
            Rewrite(bf, 1);

            while not Eof(tf) do
             begin
              Readln(tf, k);
              k := CodeString(k + #13#10);
              Fetch(k, ';');
              BlockWrite(bf, k[1], Length(k));
             end;

            repeat
              k := CompletedInput;
              Fetch(k, ';');
              BlockWrite(bf, k[1], Length(k));
            until k = '';
           end;

          CloseFile(tf);
          CloseFile(bf);
          IDUUDecoder1.Free;
         end;
     end;
    wtAdd: 
     begin
      IDUUEncoder1 := TIDUUEncoder.Create(NIL);
      Fs           := TFileStream.Create(AddOptions.Add_Files.Strings[0], fmOPENREAD);
      with IDUUEncoder1 do
       begin
        AutocompleteInput := False;
        Reset;
        FileName := ExtractFileName(AddOptions.Add_Files.Strings[0]);
        AssignFile(tf, ArchiveName);
        Rewrite(tf);
        Writeln(tf, 'table');
        i := Length(IDUUEncoder1.CodingTable) div 2;
        Writeln(tf, Copy(IDUUEncoder1.CodingTable, 0, i));
        Writeln(tf, Copy(IDUUEncoder1.CodingTable, i + 1,
          Length(IDUUEncoder1.CodingTable) - i));
        repeat
          Count := fs.Read(t, 45);
          SetBufferSize(Count);
          s := CodeString(t);
          Fetch(s, ';');
          Write(tf, s);
        until Count < 45;
        s := CompletedInput;
        Fetch(s, ';');
        if s <> '' then Write(tf, s);
        Free;

        CloseFile(tf);
        Fs.Free;
       end;
     end;
{
wtAdd : begin
       IDUUEncoder1 := TIDUUEncoder.Create(nil);
       with IDUUEncoder1 do
       begin
        AutocompleteInput := False;
        filter := DEFAULTFILTER;
        Reset;
        SetCodingtable(filter);
        AssignFile(bf, Addoptions.add_files.Strings[0]);
        System.Reset(bf, 1);
        Filename := Extractfilename(Addoptions.add_files.strings[0]);
        AssignFile(tf, ArchiveName);
        Rewrite(tf);
        SetLength(t, 45);
        BlockRead(bf, t[1], 45, count);
        SetLength(t, count);
        while count > 0 do
        begin
        // set coding buffer size to the number of bytes read (up to 45)
        SetBufferSize(Length(t));
        s := CodeString(t);
        Fetch(s, ';');
        if s <> '' then
              write(tf, s);
        BlockRead(bf, t[1], 45, count);
        SetLength(t, count);
        end;

        // to end coding and get an "end" line
        s := CompletedInput;
        Fetch(s, ';');
        if s <> ''
          then write(tf, s);
        Free;
        end;
        CloseFile(bf);
        CloseFile(tf);
       end;
}
   end;
end;

function TCakIndy.ProcessXXE(ProcessWhat: WorkType): Boolean;
var 
  IDXXDecoder1: TIDXXDecoder;
  //    IDXXEncoder1 : TIDXXEncoder;
  s, k, x:      String;
  //    t : array[0..44] of Char;
  tf:           textfile;
  fn:           String;
  loc, fz{,count}: Integer;
  bf:           file;
  //Fs : TFileStream;
begin
  Result := True;
  with (Cakdir as TCakdir2) do
  case ProcessWhat of
    wtLoadContents: 
     begin
      Total_Contents := 0;
       begin
        AssignFile(tf, ArchiveName);
        Reset(tf);
        fz           := FileSize(tf);
        fn           := '';
        IDXXDecoder1 := TIDXXDecoder.Create(NIL);

        with IDXXDecoder1 do
         begin
          AutocompleteInput := False;
          Reset;
          while not Eof(tf) and (fn = '') do
             begin
              Readln(tf, k);
            s := CodeString(k + #13);
            s := CompletedInput;
            s := CompletedInput;
            if FileName <> '' then fn := FileName;
           end;
         end;
        CloseFile(tf);
        IDXXDecoder1.Free;

        Inc(Total_Contents);
        SetLength(Archive_Contents, Total_Contents + 5);
        Archive_Contents[Total_Contents - 1]._FileName := fn;
        loc := ReturnIconType(fn);
        Archive_Contents[Total_Contents - 1]._FileIcon := loc;
        Archive_Contents[Total_Contents - 1]._FileType := FileType.Strings[loc];

        Archive_Contents[Total_Contents - 1]._FileRatio   := 100;
        Archive_Contents[Total_Contents - 1]._Encrypted   := False;
        Archive_Contents[Total_Contents - 1]._FileSize    := fz;
        Archive_Contents[Total_Contents - 1]._FilePackedSize := fz;
        Archive_Contents[Total_Contents - 1]._FileCRC     := '';
        Archive_Contents[Total_Contents - 1]._FileDefPath := '';
        Archive_Contents[Total_Contents - 1]._FileArchive :=
          ArchiveName;
       end;
     end;
    wtExtract: 
     begin
        if ExtractOptions.Extr_ExtractAll or Archive_Contents[0]._Selected then
         begin
          AssignFile(tf, ArchiveName);
          Reset(tf);
          fn           := '';
          IDXXDecoder1 := TIDXXDecoder.Create(NIL);
          with IDXXDecoder1 do
           begin
            while not Eof(tf) and (fn = '') do
               begin
                AutocompleteInput := False;
              Reset;
              Readln(tf, k);

              if UpperCase(k) = 'TABLE' then
               begin
                x := '';
                s := '';
                while not Eof(tf) and not (UpperCase(Copy(s, 0, 9)) = 'BEGIN 644') do
                   begin
                    x := x + s;
                  Readln(tf, s);
                 end;
                SetCodingtable(x);
                k := s;
               end;

              if UpperCase(Copy(k, 0, 9)) = 'BEGIN 644' then
               begin
                s := CodeString(k + #13);
                s := CompletedInput;
                s := CompletedInput;
                if FileName <> '' then fn := FileName;
               end;
             end;

            s := AppendSlash(ExtractOptions.Extr_to) + fn;
            AssignFile(bf, s);
            Rewrite(bf, 1);

            while not Eof(tf) do
             begin
              Readln(tf, k);
              k := CodeString(k + #13#10);
              Fetch(k, ';');
              BlockWrite(bf, k[1], Length(k));
             end;

            repeat
              k := CompletedInput;
              Fetch(k, ';');
              BlockWrite(bf, k[1], Length(k));
            until k = '';
           end;

          CloseFile(tf);
          CloseFile(bf);
          IDXXDecoder1.Free;
         end;
     end;
   end;
end;
function TCakIndy.ProcessB64(ProcessWhat: WorkType): Boolean;
var 
  IDBase64Decoder1: TIDBase64Decoder;
  //    IDXXEncoder1 : TIDXXEncoder;
  s, k: String;
  //    t : array[0..44] of Char;
  tf:   textfile;
  fn:   String;
  loc, fz{,count}: Integer;
  bf:   file;
  //Fs : TFileStream;
begin
  Result := True;
  with (Cakdir as TCakdir2) do
  case ProcessWhat of
    wtLoadContents:
     begin
      Total_Contents := 0;
       begin
        AssignFile(tf, ArchiveName);
        Reset(tf);
        fz := FileSize(tf);
        fn := '';
        IDBase64Decoder1 := TIDBase64Decoder.Create(NIL);

        with IDBase64Decoder1 do
         begin
          AutocompleteInput := False;
          Reset;
          while not Eof(tf) and (fn = '') do
             begin
              Readln(tf, k);
            s := CodeString(k + #13);
            s := CompletedInput;
            s := CompletedInput;
            if FileName <> '' then fn := FileName;
           end;
         end;
        CloseFile(tf);
        IDBase64Decoder1.Free;

        Inc(Total_Contents);
        SetLength(Archive_Contents, Total_Contents + 5);
        Archive_Contents[Total_Contents - 1]._FileName := fn;
        loc := ReturnIconType(fn);
        Archive_Contents[Total_Contents - 1]._FileIcon := loc;
        Archive_Contents[Total_Contents - 1]._FileType := FileType.Strings[loc];

        Archive_Contents[Total_Contents - 1]._FileRatio   := 100;
        Archive_Contents[Total_Contents - 1]._Encrypted   := False;
        Archive_Contents[Total_Contents - 1]._FileSize    := fz;
        Archive_Contents[Total_Contents - 1]._FilePackedSize := fz;
        Archive_Contents[Total_Contents - 1]._FileCRC     := '';
        Archive_Contents[Total_Contents - 1]._FileDefPath := '';
        Archive_Contents[Total_Contents - 1]._FileArchive :=
          ArchiveName;
       end;
     end;
    wtExtract: 
     begin
        if ExtractOptions.Extr_ExtractAll or Archive_Contents[0]._Selected then
         begin
          AssignFile(tf, ArchiveName);
          Reset(tf);
          fn := '';
          IDBase64Decoder1 := TIDBase64Decoder.Create(NIL);
          with IDBase64Decoder1 do
           begin
            Readln(tf, k);
            s := CodeString(k + #13);
            s := CompletedInput;
            s := CompletedInput;
            if FileName <> '' then fn := FileName;

            s := AppendSlash(ExtractOptions.Extr_to) + fn;
            AssignFile(bf, s);
            Rewrite(bf, 1);

            while not Eof(tf) do
             begin
              Readln(tf, k);
              k := CodeString(k + #13#10);
              Fetch(k, ';');
              BlockWrite(bf, k[1], Length(k));
             end;

            repeat
              k := CompletedInput;
              Fetch(k, ';');
              BlockWrite(bf, k[1], Length(k));
            until k = '';
           end;

          CloseFile(tf);
          CloseFile(bf);
          IDBase64Decoder1.Free;
         end;
     end;
   end;
end;


procedure Register;
begin
  //RegisterComponents('QZip', [TCakIndy]);
end;

end.
 