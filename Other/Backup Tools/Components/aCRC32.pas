{***********************************************}
{                                               }
{ CRC32 calculation V1.01                       }
{ Copyright (c) 1997 by S.Kurinny & S.Kostinsky }
{                                               }
{***********************************************}

unit aCRC32;

interface

type
  int = Integer;
{----------------------------------------------------------------------}

{ Tests CRC algorythm. Returns true if Ok. This function is for testing
  purposes only and you don't need to call it.}
Function  TestCRC:Boolean;

{ Finishes calculation of CRC. If you don't need compartability with PKZIP's
  CRC algorythm you may not call this function.}
function  CRC32Finish(CRC:Integer):Integer;

{ Returns initialized CRC variable ($FFFFFFFF). CRC variable should be
  initialized before calculating CRC.}
function  CRC32Start:Integer;

{ Calculates CRC for a given memory buffer. Buf  - memory buffer.
  CRC - initial CRC value (result of previuos call to CRC32Calc or $FFFFFFFF).
  BufSize - buffer size. Returns new CRC value.}
function  CRC32Calc(Var Buf;CRC:integer;BufSize:Integer):Integer;

{ Calculates CRC value for a given buffer. This function calls CRC32Start,
  CRC32Calc,CRC32 finish internally and returns calculated CRC value.}
Function  CalculateCRC32(VAR Buf;Count:Integer):Integer;

{----------------------------------------------------------------------}
implementation
{----------------------------------------------------------------------}

Function CalculateCRC32(VAR Buf;Count:Integer):Integer;
begin
  Result:=not CRC32Calc(Buf,int($FFFFFFFF),Count);
end;

{----------------------------------------------------------------------}

const
  TestString: String[31] = 'Testing CRC32 routine ... ';
  PKZIP_CRC = int($6759C95B);

{----------------------------------------------------------------------}
Const
aTableCRC32:Array[0..255] of integer=(
{;           $}
            int($00000000), int($77073096), int($ee0e612c), int($990951ba),
            int($076dc419), int($706af48f), int($e963a535), int($9e6495a3),
            int($0edb8832), int($79dcb8a4), int($e0d5e91e), int($97d2d988),
            int($09b64c2b), int($7eb17cbd), int($e7b82d07), int($90bf1d91),
{;            1}
            int($1db71064), int($6ab020f2), int($f3b97148), int($84be41de),
            int($1adad47d), int($6ddde4eb), int($f4d4b551), int($83d385c7),
            int($136c9856), int($646ba8c0), int($fd62f97a), int($8a65c9ec),
            int($14015c4f), int($63066cd9), int($fa0f3d63), int($8d080df5),
{;            2}
            int($3b6e20c8), int($4c69105e), int($d56041e4), int($a2677172),
            int($3c03e4d1), int($4b04d447), int($d20d85fd), int($a50ab56b),
            int($35b5a8fa), int($42b2986c), int($dbbbc9d6), int($acbcf940),
            int($32d86ce3), int($45df5c75), int($dcd60dcf), int($abd13d59),
{;            3}
            int($26d930ac), int($51de003a), int($c8d75180), int($bfd06116),
            int($21b4f4b5), int($56b3c423), int($cfba9599), int($b8bda50f),
            int($2802b89e), int($5f058808), int($c60cd9b2), int($b10be924),
            int($2f6f7c87), int($58684c11), int($c1611dab), int($b6662d3d),
{;            4}
            int($76dc4190), int($01db7106), int($98d220bc), int($efd5102a),
            int($71b18589), int($06b6b51f), int($9fbfe4a5), int($e8b8d433),
            int($7807c9a2), int($0f00f934), int($9609a88e), int($e10e9818),
            int($7f6a0dbb), int($086d3d2d), int($91646c97), int($e6635c01),
{;            5}
            int($6b6b51f4), int($1c6c6162), int($856530d8), int($f262004e),
            int($6c0695ed), int($1b01a57b), int($8208f4c1), int($f50fc457),
            int($65b0d9c6), int($12b7e950), int($8bbeb8ea), int($fcb9887c),
            int($62dd1ddf), int($15da2d49), int($8cd37cf3), int($fbd44c65),
{;            6}
            int($4db26158), int($3ab551ce), int($a3bc0074), int($d4bb30e2),
            int($4adfa541), int($3dd895d7), int($a4d1c46d), int($d3d6f4fb),
            int($4369e96a), int($346ed9fc), int($ad678846), int($da60b8d0),
            int($44042d73), int($33031de5), int($aa0a4c5f), int($dd0d7cc9),
{;            7}
            int($5005713c), int($270241aa), int($be0b1010), int($c90c2086),
            int($5768b525), int($206f85b3), int($b966d409), int($ce61e49f),
            int($5edef90e), int($29d9c998), int($b0d09822), int($c7d7a8b4),
            int($59b33d17), int($2eb40d81), int($b7bd5c3b), int($c0ba6cad),
{;            8}
            int($edb88320), int($9abfb3b6), int($03b6e20c), int($74b1d29a),
            int($ead54739), int($9dd277af), int($04db2615), int($73dc1683),
            int($e3630b12), int($94643b84), int($0d6d6a3e), int($7a6a5aa8),
            int($e40ecf0b), int($9309ff9d), int($0a00ae27), int($7d079eb1),
{;            9}
            int($f00f9344), int($8708a3d2), int($1e01f268), int($6906c2fe),
            int($f762575d), int($806567cb), int($196c3671), int($6e6b06e7),
            int($fed41b76), int($89d32be0), int($10da7a5a), int($67dd4acc),
            int($f9b9df6f), int($8ebeeff9), int($17b7be43), int($60b08ed5),
{;            A}
            int($d6d6a3e8), int($a1d1937e), int($38d8c2c4), int($4fdff252),
            int($d1bb67f1), int($a6bc5767), int($3fb506dd), int($48b2364b),
            int($d80d2bda), int($af0a1b4c), int($36034af6), int($41047a60),
            int($df60efc3), int($a867df55), int($316e8eef), int($4669be79),
{;            B}
            int($cb61b38c), int($bc66831a), int($256fd2a0), int($5268e236),
            int($cc0c7795), int($bb0b4703), int($220216b9), int($5505262f),
            int($c5ba3bbe), int($b2bd0b28), int($2bb45a92), int($5cb36a04),
            int($c2d7ffa7), int($b5d0cf31), int($2cd99e8b), int($5bdeae1d),
{;            C}
            int($9b64c2b0), int($ec63f226), int($756aa39c), int($026d930a),
            int($9c0906a9), int($eb0e363f), int($72076785), int($05005713),
            int($95bf4a82), int($e2b87a14), int($7bb12bae), int($0cb61b38),
            int($92d28e9b), int($e5d5be0d), int($7cdcefb7), int($0bdbdf21),
{;            D}
            int($86d3d2d4), int($f1d4e242), int($68ddb3f8), int($1fda836e),
            int($81be16cd), int($f6b9265b), int($6fb077e1), int($18b74777),
            int($88085ae6), int($ff0f6a70), int($66063bca), int($11010b5c),
            int($8f659eff), int($f862ae69), int($616bffd3), int($166ccf45),
{;            E}
            int($a00ae278), int($d70dd2ee), int($4e048354), int($3903b3c2),
            int($a7672661), int($d06016f7), int($4969474d), int($3e6e77db),
            int($aed16a4a), int($d9d65adc), int($40df0b66), int($37d83bf0),
            int($a9bcae53), int($debb9ec5), int($47b2cf7f), int($30b5ffe9),
{;            F}
            int($bdbdf21c), int($cabac28a), int($53b39330), int($24b4a3a6),
            int($bad03605), int($cdd70693), int($54de5729), int($23d967bf),
            int($b3667a2e), int($c4614ab8), int($5d681b02), int($2a6f2b94),
            int($b40bbe37), int($c30c8ea1), int($5a05df1b), int($2d02ef8d)
);

{----------------------------------------------------------------------}
function CRC32Calc(Var Buf;CRC:integer;BufSize:Integer):Integer;assembler;
asm
  push esi
  mov esi,Buf
@1:
  movzx eax,byte ptr [esi]
  inc ESI
  xor al,dl
  shr edx,8
  xor edx,dword ptr [atablecrc32+eax*4]
  Loop @1
  mov eax,edx
  pop esi
end;

{----------------------------------------------------------------------}

Function TestCRC:Boolean;
Var
  CRC:Integer;
begin
  CRC:=not CRC32Calc(TestString[1],int($ffffffff),Length(TestString));
  Result:=CRC=PKZIP_CRC;
end;

{----------------------------------------------------------------------}

function CRC32Finish(CRC:Integer):Integer;
begin
  Result := not CRC;
end;

{----------------------------------------------------------------------}

function CRC32Start:Integer;
begin
  Result := int($FFFFFFFF);
end;

{----------------------------------------------------------------------}
end.


