{
   by Vadim V.Lopushansky. Kiev 2002.
   e-mail: pult@ukr.net
   icq:    44335679

   Clearing of text files of superfluous characters in string, at the end of the file and inside
   the file from characters with the code of the end of string "0".

   It is possible to fill on the concrete file or on a mask. It is possible to specify assemblage
   of the masks divided by the character "/". It is possible to fill on the directory recursively.

   Are handled and ReadOnly files.
}

program trim_file;
{$APPTYPE CONSOLE}
{$B-}
//{$O-,D+}
{$O+,D-,R-}

uses
  SysUtils,
  Windows,
  Classes,
  Masks;

const
  cVersion = '2007.06.14';

{ Relocation info stripped from file. }
const
  IMAGE_FILE_RELOCS_STRIPPED = $0001;
{$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED}

 var
     StartDir :String='.\';
     FilesMask:String='*.*';
     Recurse:Boolean = False;
     Compact:Boolean = False;
     // ---
     sTmp: string;

 const
     cHelpPrint          = 1;
     cDirectoryIsUnknown = 2;
     cMaskSyntaxError    = 3;

function IntToStrLeft(V: Integer; iLeft: Integer = -1): string;
begin
  Result := IntTostr(V);
  if iLeft <= 0 then
    exit;
  V := Length(Result);
  if iLeft > V then
    for V := 1 to iLeft - V do
      Result := '0'+Result;
end;

procedure trim_file_right(f:String; HaltOnError:Boolean=True);
 var S:TStringList;
     buf:String;
     i:Integer;
     c,l,z:Integer;
     fs:TFileStream;
begin
    try
      S:=nil;
      fs:=TFileStream.Create(f, fmOpenReadWrite);
      try

        if fs.Size=0 then exit;

        SetLength(buf, fs.Size);
        fs.ReadBuffer( buf[1], fs.Size);
        l:=fs.Size;
        c:=0; // trim changec count
        z:=0; // zero changes count

        if buf[Length(buf)] in [#10,#13] then dec(c);
        if buf[Length(buf)-1] in [#10,#13] then dec(c);

        i:=1;
        while i<=Length(buf) do begin
            if buf[i]=#0 then
            begin
              system.Delete(buf, i, 1);
              inc(z);
            end
            else
              inc(i);
        end;
        buf := TrimRight(buf);
        c:= c+l-Length(buf);

        S:=TStringList.Create;
        S.Text := buf;
        for i:=0 to S.Count-1 do begin
            l:=Length(S[i]);
            S[i] := TrimRight(S[i]);
            c:=c+(l-Length(S[i]));
        end;

        if (c>0)or(z>0) then
        begin
          fs.Size := 0;
          S.SaveToStream(fs);
        end
        else
        if Compact and (fs.Size=1) and (S.Count=0) then
        begin
          fs.Size := 0;
          c := 1;
        end;

        if (c>0)or(z>0)or(HaltOnError) then
        begin
          //WriteLn('  - truncated/zero "'+IntToStrLeft(c, 3)+'/'+IntToStr(z)+'" symbols in file: "'+f+'"');
          sTmp := '  - truncated/zero "'+IntToStrLeft(c, 3)+'/'+IntToStr(z)+'" symbols in file: "'+f+'"';
          CharToOem( PChar(sTmp), PChar(sTmp) );
          WriteLn(sTmp);
        end;

      finally
        fs.Free;
        s.free;
      end;

    except
        on e:Exception do begin
            //writeLn('ERROR: '+ e.Message);
            sTmp := 'ERROR: '+ e.Message;
            CharToOem( PChar(sTmp), PChar(sTmp) );
            WriteLn(sTmp);

            if HaltOnError
              then Halt(cHelpPrint);
        end;
    end;
end;

Type
 TLoopFunc = function (Const FileName:String; Const SearchRecInfo:SysUtils.TSearchRec):Boolean; // return True for break work

function LoopFiles(StartDir: String; Recurse:Boolean; LoopFunc: TLoopFunc; FilesMask:String='*.*') :Boolean;
  function LoopFilesCustom(StartDir: String; Recurse:Boolean; LoopFunc: TLoopFunc; Const MaskList:TList ) :Boolean;
   var
     Search        :SysUtils.TSearchRec;
     SearchRecInfo :SysUtils.TSearchRec;
   function MatchesByMask(Const Value:String):Boolean;
    var i:Integer;
   begin
       for i:=0 to MaskList.Count-1 do begin
           if TMask(MaskList[i]).Matches(Value)
           then begin
               Result := True;
               exit;
           end;
       end;
       Result := False;
   end;
  begin

    Result := True;

    if Startdir[Length(Startdir)] <> '\'
    then startdir := startdir + '\';

    if SysUtils.FindFirst(startdir + '*.*', faAnyFile, Search) = 0 then
    repeat
      if (Search.Name[1] <> '.' ) then
        if ((Search.Attr and faDirectory) > 0) then
        begin
          if Recurse then begin
            if LoopFilesCustom(StartDir + Search.Name, Recurse, LoopFunc, MaskList)
            then exit;
          end;
        end else
        begin

          if MatchesByMask(StartDir + Search.Name) then begin

            SearchRecInfo :=Search;
            SearchRecInfo.FindHandle := 0;

            if LoopFunc(StartDir + Search.Name, SearchRecInfo)
            then exit;

            sleep(0);

          end;

        end;
    until FindNext(Search) <> 0;

    SysUtils.FindClose(Search);

    Result := False;
  end;
  var
      MaskList:TList;
      SL:TStringList;
      i:Integer;
begin
    Result := False;

    if (not Assigned(LoopFunc))
    then exit;

    if Length(FilesMask)=0
    then FilesMask:='*.*';

    MaskList:=nil;
    SL:=TStringList.Create;
    try
        SL.Text := StringReplace( FilesMask, '/', #13, [rfReplaceAll] );
        MaskList:=TList.Create;
        for i:=0 to SL.Count-1 do begin
            MaskList.Add(TMask.Create(SL[i]));
            try
              // check syntax mask
              TMask(MaskList[i]).Matches('test');
            except
              on e:exception do begin
                  //WriteLn('ERROR: Mask "'+FilesMask+'" is syntactically invalid.');
                  sTmp := 'ERROR: Mask "'+FilesMask+'" is syntactically invalid.';
                  CharToOem( PChar(sTmp), PChar(sTmp) );
                  WriteLn(sTmp);

                  exit;
              end;
            end;
        end;

        Result := not LoopFilesCustom(StartDir, Recurse, LoopFunc, MaskList );

    finally
      SL.Free;
      if Assigned(MaskList) then begin
          for i:=0 to MaskList.Count-1 do begin
              TMask(MaskList[i]).Free;
          end;
          MaskList.Free;
      end;
    end;

end;

function DirectoryExists(const Directory: string): Boolean;
 var Code: Integer;
begin
  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function TrimFileLoopFunc(Const FileName:String; Const SearchRecInfo:SysUtils.TSearchRec):Boolean;
 var isReadOnly :Boolean;
begin
    Result := False;
    if FileExists(FileName)
      then begin
           isReadOnly := (SearchRecInfo.Attr and faReadOnly)>0;
           if isReadOnly then begin
               FileSetAttr(FileName, SearchRecInfo.Attr - faReadOnly);
           end;
           try

             trim_file_right(FileName, False);

           finally;
             if isReadOnly then begin
                 FileSetAttr(FileName, SearchRecInfo.Attr);
             end;
           end;
      end;
end;

procedure PrintHelp;
begin
    writeLn('Text File Trim Right. Version:' + cVersion);
    writeLn('  Usage as:');
    writeLn('    trim_file.exe filename');
    writeLn('       or');
    writeLn('    trim_file.exe [/r] [/c] [StartDir=dirctory_name] mask=file_mask ');
    writeLn('       or');
    writeLn('    trim_file.exe [/?] ');
    writeLn;
    writeLn('    switches:');
    writeLn('       /r - recurse subdirectories');
    writeLn('       /c - clear file if is contain only one trimed symbol ("\n", "\r" ...) ');
    writeLn('       StartDir - base directory (defaul ".").');
    writeLn('       mask - file masks (*.txt or ?readme_*.txt) multiple mask: *.txt/*.doc');
    writeLn('       /? - printing this help');
    writeLn('       ');
    writeLn('       examples:');
    writeLn('       ');
    writeLn('       >trim_file.exe  readme.txt');
    writeLn('       >trim_file.exe  mask=*.txt');
    writeLn('       >trim_file.exe  StartDir=C: mask=*.txt');
    writeLn('       >trim_file.exe  /r StartDir=C: mask=*.txt');
    writeLn('       >trim_file.exe  /r StartDir=C: mask=*.txt/*.text/*readme*.');
end;

 var Params:TStringList;
     i:integer;

begin
  if (ParamCount=1)and(ParamStr(1)='/?')
  then PrintHelp
  else
  if (ParamCount=1)and(FileExists(ParamStr(1)))
  then trim_file_right(ParamStr(1))
  else begin

      if (ParamCount>0) then begin
          Params:=TStringList.Create;
          try

            for i:=1 to ParamCount do begin
                Params.Add(ParamStr(i));
            end;

            if pos('/r'#13,LowerCase(Params.Text)+#13)>0
            then Recurse := True;

            if pos('/c'#13,LowerCase(Params.Text)+#13)>0
            then Compact := True;

            if Length(Params.Values['StartDir'])>0
            then begin
                 StartDir := Params.Values['StartDir'];
                 if not DirectoryExists(StartDir)
                 then begin
                     //writeLn('ERROR: Directory: "'+StartDir+'" not found.');
                     sTmp := 'ERROR: Directory: "'+StartDir+'" not found.';
                     CharToOem( PChar(sTmp), PChar(sTmp) );
                     WriteLn(sTmp);

                     PrintHelp;
                     Halt(cDirectoryIsUnknown);
                 end;
            end;

            if Length(Params.Values['mask'])>0
            then begin
                FilesMask := Params.Values['mask'];

                if pos('/',FilesMask)<=0 then
                try
                  MatchesMask('test', FilesMask);
                except
                    on e:exception do begin
                        //WriteLn('ERROR: Mask "'+FilesMask+'" is syntactically invalid.');
                        sTmp := 'ERROR: Mask "'+FilesMask+'" is syntactically invalid.';
                        CharToOem( PChar(sTmp), PChar(sTmp) );
                        WriteLn(sTmp);

                        PrintHelp;
                        Halt(cMaskSyntaxError);
                    end;
                end;

                if not LoopFiles(StartDir, Recurse, TrimFileLoopFunc, FilesMask)
                then begin
                     PrintHelp;
                     Halt(cMaskSyntaxError);
                end else begin
                  writeLn('PROCESS FINISHED');
                  exit;
                end;

            end;

          finally
            Params.Free;
          end;
      end;

        PrintHelp;
        Halt(cHelpPrint);

  end;
end.
