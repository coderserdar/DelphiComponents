unit OraUtils;


{$INCLUDE dOCI.inc}

interface

uses Windows, SysUtils
     {$ifdef ADEBUG} ,Dialogs {$endif};

function MakeStr(c:char;l:integer):string;
function RightStr(s:string;l:integer):string;
function StrToOem(const AnsiStr: string): string;
function CorrectPath(const Path: string): string;
function VerStr2Int(const AVersion: string): Integer;
procedure replaceDA(t:PChar);
procedure OraNumToInt64(pin,pout:pointer;opor:byte); stdcall;


{$ifdef ADEBUG}
procedure LogMessage(Msg:string);
{$endif}

implementation

function MakeStr(c:char;l:integer):string;
var i:integer;
begin
 Result:='';
 for i:=0 to l-1 do Result:=Result+c;
end;

function RightStr(s:string;l:integer):string;
var i:integer;
begin
 Result:=s;
 for i:=length(s) to l-1 do Result:=Result+' ';
end;

function StrToOem(const AnsiStr: string): string;
begin
  SetLength(Result, Length(AnsiStr));
  if Length(Result) > 0 then
   CharToOemBuff(PChar(AnsiStr), PChar(Result), Length(Result));
end;

function VerStr2Int(const AVersion: string): Integer;
var
    i, i1: Integer;
    s: string;
begin
  Result := 0;
  s := AVersion;
  for i := 1 to 5 do begin
    if s = ''
     then Result := Result * 100
     else begin
       i1 := Pos('.', s);
       if i1 = 0 then i1 := Length(s) + 1;
       Result := Result * 100 + StrToInt(Trim(Copy(s, 1, i1 - 1)));
       s := Copy(s, i1 + 1, Length(s));
     end;
  end;
end;

function CorrectPath(const Path: string): string;
begin
  if (Path<>'')and(Path[Length(Path)]='\')
    then Result := Copy(Path, 1, Length(Path) - 1)
    else Result := Path;
end;

procedure replaceDA(t:PChar);
var i:integer;
begin
 i:=0;
 while ord(t[i])<>0 do begin
  if (t[i]=#13) then t[i]:=' ';
  inc(i);
 end;
end;

procedure OraNumToInt64(pin,pout:pointer;opor:byte); stdcall;
label m1,m2,m3,m4,m5,m6,me;
     asm
      pushad
      pushfd
//               vCurrency:=0;
      xor ESI,ESI // LO word
      xor EDI,EDI // HI word
//               exponent:=pbyte(pin)^;
      mov ebx,pin
      mov ch,[ebx] // exponent
      inc bx

      xor ah,ah
//               por:=(exponent and $7F)-1;
      mov cl,ch
      and cl,$7f
      dec cl
//               if (exponent and $80)>0
      sub cl,64
      test ch,$80
      jnz m1
//                else por:=64-por;
      neg cl
      jmp m2
//                then por:=por-64+3
m1:   add cl,3
      add cl,opor //offset for currency and int64
m2:
      cmp cl,19
      jo  me

//               for i:=0 to por do begin
//                pc:=pbyte(cardinal(pin)+i+1);
//                nb:=pc^-1;
m5:   mov al,[ebx]
      inc ebx
      cmp al,0
      jz m3

      dec al
//                if (exponent and $80)>0
//                 then nb:=nb
//                 else nb:=100-nb;
      test ch,$80
      jnz m3
      sub al,100
      neg al
//                if nb=255 then nb:=0 else if nb>99 then Exception.Create('Bad internal NUMBER !');
m3:   cmp al,$FF
      jne m4
      not al
//                vCurrency:=vCurrency*100+nb;
m4:   push ax
      mov eax,100
      mul edi
      mov edi,eax
      mov eax,100
      mul esi
      mov esi,eax
      add edi,edx
      xor eax,eax
      pop ax
      add esi,eax
      adc edi,0
      dec cl
      jnz m5

      test ch,$80
      jnz m6
      not edi
      not esi
      add esi,1
      adc edi,0
m6:
me:
      mov ebx,pout
      mov [ebx],esi
      add ebx,4
      mov [ebx],edi

      popfd
      popad
     end;


{$ifdef ADEBUG}
procedure LogMessage(Msg:string);
var FFile:THandle;
    AlreadyExists:boolean;

  procedure WriteTo(const Msg: string);
  var tmpI: DWORD;
  begin
    if FFile <> INVALID_HANDLE_VALUE then
     try
      WriteFile(FFile, PChar(Msg)^, Length(Msg), tmpI, nil);
      except
       on E:Exception do
         MessageDlg(Format('Error writing to log file : %s', [E.Message]), mtError, [mbOk], 0);
      end;
  end;

  const LogFileName='ADEBUG_dOCI.log';
begin
  FFile := CreateFile(PChar(LogFileName), GENERIC_WRITE or GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  AlreadyExists:= GetLastError = ERROR_ALREADY_EXISTS;
  if FFile = INVALID_HANDLE_VALUE then raise Exception.CreateFmt('Cannot open/create file : %s', [LogFileName]);
  try
   SetFilePointer(FFile, 0, nil, FILE_END);
   WriteTo(Msg+#13#10);

  finally
   CloseHandle(FFile);
  end;
end;
{$endif}

end.
