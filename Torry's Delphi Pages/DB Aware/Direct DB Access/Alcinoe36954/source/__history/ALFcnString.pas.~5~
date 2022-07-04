{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
              John O'Harrow (john@elmcrest.demon.co.uk)
              Charalabos Michael (chmichael@creationpower.com)
              Aleksandr Sharahov
              Dennis Kjaer Christensen
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe String functions
Version:      3.52

Description:  Powerfull stringreplace, Pos, Move, comparetext,
              uppercase, lowercase function. Also a powerfull
              FastTagReplace function To replace in string tag
              like <#tagname params1="value1" params2="value2">
              by custom value

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History:      11/05/2005: Remove the bug in alFastTagReplace that raise
                          an exception when char % is found in the params
                          of a tag
							20/10/2005: Move AlANSICodePage1252UppercaseNoDiacritic to
							            ALWideUpperCaseNoDiacritic in alFcnUnicode...
              16/11/2005: minor update in ALFastTagReplace to better
                          handle the "Handled" property of TALHandleTagfunct
              02/12/2005: 1/ Correct AlCopyStr;
                          2/ Move some copy call to AlCopyStr call;
                          3/ Update AlFastTagReplace to better performance and
                             low memory usage;
              08/12/2005: Update AlFastTagReplace to correct a bug that make
                          rfignorecase wrong in some case
              16/12/2005: remove ALStringMatches that seam to not work propertly
                          use MatchesMask insteed !
              01/04/2007: Update the FastCode Function
              22/02/2008: Use AlHttpEncode instead that HttpEncode
              26/12/2008: replace ALGetStringFromFileWithoutUT8BOM by
                          ALGetStringFromFileWithoutUTF8BOM

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALfcnString;

{$IFDEF VER170}
  {$DEFINE ALSSE2Basm}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 18.0}
    {$DEFINE ALSSE2Basm}
    {$DEFINE ALINLINE}
  {$IFEND}
{$ENDIF}

interface

uses Windows,
     SysUtils,
     Classes;

type

  {type declaration}
  TALHandleTagfunct = function(const TagString: string; TagParams: TStrings; ExtData: pointer; Var Handled: Boolean): string;

{from Charalabos Michael <chmichael@creationpower.com>, John O'Harrow (john@elmcrest.demon.co.uk), Aleksandr Sharahov
 original name: PosEx_JOH_IA32_8, PosEx_JOH_IA32_7, PosEx_Sha_IA32_3, PosEx_Sha_Pas_2}
Var ALPosEx: function(const SubStr, S: string; Offset: Integer = 1): Integer;
{from John O'Harrow (john@elmcrest.demon.co.uk)
 original name: StringReplace_JOH_IA32_12}
function ALStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
{from FastCode John O'Harrow (john@elmcrest.demon.co.uk)
 Version: 3.03 : 06-MAR-2006}
var ALMove: procedure(const Source; var Dest; Count : Integer); {Fastest Move}
{from FastCode John O'Harrow (john@elmcrest.demon.co.uk) and Charalabos Michael <chmichael@creationpower.com>
 original name: Pos_JOH_IA32_6, Pos_JOH_SSE2_3, Pos_JOH_PAS_6}
var ALPos: function(const SubStr: AnsiString; const Str: AnsiString): Integer;
{from FastCode John O'Harrow (john@elmcrest.demon.co.uk)
 original name: CharPosJOH_IA32, CharPosJOH_MMX, CharPosJOH_SSE, CharPosJOH_SSE2}
Var ALCharPos: function(Ch: Char; const Str : AnsiString): Integer;
{from FastCode John O'Harrow (john@elmcrest.demon.co.uk)
 original name: CharPosEY_JOH_IA32_4}
function ALCharPosEX(const SearchCharacter: Char; const SourceString: AnsiString; Occurrence: Integer; StartPos: Integer): Integer; overload;
function ALCharPosEX(const SearchCharacter: Char; const SourceString: AnsiString; StartPos: Integer = 1): Integer; overload;
{from FastCode Charalabos Michael <chmichael@creationpower.com> and John O'Harrow <john@elmcrest.demon.co.uk> and Aleksandr Sharahov
 original name: CompareText_Sha_IA32_3, CompareText_JOH_IA32_5, CompareText_JOH_IA32_6, CompareText_Sha_IA32_4, CompareText_Sha_Pas_5}
Var ALCompareText: function(const S1, S2: string): Integer;
{from FastCode Charalabos Michael <chmichael@creationpower.com> and John O'Harrow <john@elmcrest.demon.co.uk> and Aleksandr Sharahov
 original name: LowerCase_JOH_MMX_2, LowerCase_JOH_IA32_5, LowerCase_Sha_Pas_2}
Var ALLowerCase: function(const s: string): string;
{from FastCode Charalabos Michael <chmichael@creationpower.com> and John O'Harrow <john@elmcrest.demon.co.uk> and Dennis Kjaer Christensen
 original name: UpperCase_JOH_SSE2_2, UpperCase_JOH_SSE_2, UpperCase_JOH_MMX_3, UpperCase_JOH_IA32_5, UpperCase_DKC_Pas_32}
Var ALUpperCase: function(const s: string): string;

{Alcinoe}
function  ALFastTagReplace(Const SourceString, TagStart, TagEnd: string; FastTagReplaceProc: TALHandleTagFunct; ReplaceStrParamName, ReplaceWith: String; AStripParamQuotes: Boolean; Flags: TReplaceFlags; ExtData: Pointer): string; overload;
function  ALFastTagReplace(const SourceString, TagStart, TagEnd: string; ReplaceStrParamName: String; AStripParamQuotes: Boolean; const Flags: TReplaceFlags=[rfreplaceall]): string; overload;
function  ALFastTagReplace(const SourceString, TagStart, TagEnd: string; FastTagReplaceProc: TALHandleTagFunct; AStripParamQuotes: Boolean; ExtData: Pointer; Const flags: TReplaceFlags = [rfreplaceall]): string; overload;
function  ALFastTagReplace(const SourceString, TagStart, TagEnd: string; ReplaceWith: string; const Flags: TReplaceFlags=[rfreplaceall] ): string; overload;
function  ALExtractTagParams(const SourceString, TagStart, TagEnd: string; AStripParamQuotes: Boolean; TagParams: TStrings; IgnoreCase: Boolean): Boolean;
function  ALCopyStr(const aSourceString: string; aStart, aLength: Integer): string;
function  ALRandomStr(aLength: Longint): string;
function  ALNEVExtractName(const S: string): string;
function  ALNEVExtractValue(const s: string): string;
procedure ALExtractHeaderFields(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings; Decode: Boolean; StripQuotes: Boolean = False);
function  ALGetStringFromFile(filename: string): string;
function  ALGetStringFromFileWithoutUTF8BOM(filename: string): string;
procedure ALSaveStringtoFile(Str,filename: string);
Function  ALAnsiUpperCaseNoDiacritic(S: string): string;

implementation

uses AlHTTPCommon,
     ALFcnUnicode, 
     ALCPUID;

////////////////////////////
//////////ALPosEx //////////
////////////////////////////

{*********************************************************************************}
function ALPosEx_JOH_IA32_8(const SubStr, S: string; Offset: Integer = 1): Integer;
asm {299 Bytes}
  sub     esp, 20
  mov     [esp], ebx
  cmp     eax, 1
  sbb     ebx, ebx         {-1 if SubStr = '' else 0}
  sub     edx, 1           {-1 if S = ''}
  sbb     ebx, 0           {Negative if S = '' or SubStr = '' else 0}
  sub     ecx, 1           {Offset - 1}
  or      ebx, ecx         {Negative if S = '' or SubStr = '' or Offset < 1}
  jl      @@InvalidInput
  mov     [esp+4], edi
  mov     [esp+8], esi
  mov     [esp+12], ebp
  mov     [esp+16], edx
  mov     edi, [eax-4]     {Length(SubStr)}
  mov     esi, [edx-3]     {Length(S)}
  add     ecx, edi
  cmp     ecx, esi
  jg      @@NotFound       {Offset to High for a Match}
  test    edi, edi
  jz      @@NotFound       {Length(SubStr = 0)}
  lea     ebp, [eax+edi]   {Last Character Position in SubStr + 1}
  add     esi, edx         {Last Character Position in S}
  movzx   eax, [ebp-1]     {Last Character of SubStr}
  add     edx, ecx         {Search Start Position in S for Last Character}
  mov     ah, al
  neg     edi              {-Length(SubStr)}
  mov     ecx, eax
  shl     eax, 16
  or      ecx, eax         {All 4 Bytes = Last Character of SubStr}
@@MainLoop:
  add     edx, 4
  cmp     edx, esi
  ja      @@Remainder      {1 to 4 Positions Remaining}
  mov     eax, [edx-4]     {Check Next 4 Bytes of S}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@MainLoop       {Loop Until any Match on Last Character Found}
  bsf     eax, eax         {Find First Match Bit}
  shr     eax, 3           {Byte Offset of First Match (0..3)}
  lea     edx, [eax+edx-3] {Address of First Match on Last Character + 1}
@@Compare:
  cmp     edi, -4
  jle     @@Large          {Lenght(SubStr) >= 4}
  cmp     edi, -1
  je      @@SetResult      {Exit with Match if Lenght(SubStr) = 1}
  mov     ax, [ebp+edi]    {Last Char Matches - Compare First 2 Chars}
  cmp     ax, [edx+edi]
  jne     @@MainLoop       {No Match on First 2 Characters}
@@SetResult:               {Full Match}
  lea     eax, [edx+edi]   {Calculate and Return Result}
  mov     ebx, [esp]
  mov     edi, [esp+4]
  mov     esi, [esp+8]
  mov     ebp, [esp+12]
  sub     eax, [esp+16]
  add     esp, 20
  ret
@@NotFound:
  mov     edi, [esp+4]
  mov     esi, [esp+8]
  mov     ebp, [esp+12]
@@InvalidInput:
  mov     ebx, [esp]
  add     esp, 20
  xor     eax, eax         {Return 0}
  ret
@@Remainder:               {Check Last 1 to 4 Characters}
  mov     eax, [esi-3]     {Last 4 Characters of S - May include Length 
Bytes}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@NotFound       {No Match Possible}
  lea     eax, [edx-4]     {Check Valid Match Positions}
  cmp     cl, [eax]
  lea     edx, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+2]
  cmp     cl, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+3]
  cmp     cl, [eax+2]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+4]
  jmp     @@Compare
@@Large:
  mov     eax, [ebp-4]     {Compare Last 4 Characters of S and SubStr}
  cmp     eax, [edx-4]
  jne     @@MainLoop       {No Match on Last 4 Characters}
  mov     ebx, edi
@@CompareLoop:             {Compare Remaining Characters}
  add     ebx, 4           {Compare 4 Characters per Loop}
  jge     @@SetResult      {All Characters Matched}
  mov     eax, [ebp+ebx-4]
  cmp     eax, [edx+ebx-4]
  je      @@CompareLoop    {Match on Next 4 Characters}
  jmp     @@MainLoop       {No Match}
end; {PosEx}

{*********************************************************************************}
function ALPosEx_JOH_IA32_7(const SubStr, S: string; Offset: Integer = 1): Integer;
asm {180 Bytes}
  push    ebx
  cmp     eax, 1
  sbb     ebx, ebx         {-1 if SubStr = '' else 0}
  sub     edx, 1           {-1 if S = ''}
  sbb     ebx, 0           {Negative if S = '' or SubStr = '' else 0}
  dec     ecx              {Offset - 1}
  or      ebx, ecx         {Negative if S = '' or SubStr = '' or Offset < 1}
  jl      @@InvalidInput
  push    edi
  push    esi
  push    ebp
  push    edx
  mov     edi, [eax-4]     {Length(SubStr)}
  mov     esi, [edx-3]     {Length(S)}
  add     ecx, edi
  cmp     ecx, esi
  jg      @@NotFound       {Offset to High for a Match}
  test    edi, edi
  jz      @@NotFound       {Length(SubStr = 0)}
  lea     ebp, [eax+edi]   {Last Character Position in SubStr + 1}
  add     esi, edx         {Last Character Position in S}
  movzx   eax, [ebp-1]     {Last Character of SubStr}
  add     edx, ecx         {Search Start Position in S for Last Character}
  mov     ah, al
  neg     edi              {-Length(SubStr)}
  mov     ecx, eax
  shl     eax, 16
  or      ecx, eax         {All 4 Bytes = Last Character of SubStr}
@@MainLoop:
  add     edx, 4
  cmp     edx, esi
  ja      @@Remainder      {1 to 4 Positions Remaining}
  mov     eax, [edx-4]     {Check Next 4 Bytes of S}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@MainLoop       {Loop Until any Match on Last Character Found}
  bsf     eax, eax         {Find First Match Bit}
  shr     eax, 3           {Byte Offset of First Match (0..3)}
  lea     edx, [eax+edx-4] {Address of First Match on Last Character}
@@Compare:
  inc     edx
  cmp     edi, -4
  jle     @@Large          {Lenght(SubStr) >= 4}
  cmp     edi, -1
  je      @@SetResult      {Exit with Match if Lenght(SubStr) = 1}
  mov     ax, [ebp+edi]    {Last Char Matches - Compare First 2 Chars}
  cmp     ax, [edx+edi]
  jne     @@MainLoop       {No Match on First 2 Characters}
@@SetResult:               {Full Match}
  lea     eax, [edx+edi]   {Calculate and Return Result}
  pop     edx
  pop     ebp
  pop     esi
  pop     edi
  pop     ebx
  sub     eax, edx         {Subtract Start Position}
  ret
@@NotFound:
  pop     edx              {Dump Start Position}
  pop     ebp
  pop     esi
  pop     edi
@@InvalidInput:
  pop     ebx
  xor     eax, eax         {No Match Found - Return 0}
  ret
@@Remainder:               {Check Last 1 to 4 Characters}
  sub     edx, 4
@@RemainderLoop:
  cmp     cl, [edx]
  je      @@Compare
  cmp     edx, esi
  jae     @@NotFound
  inc     edx
  jmp     @@RemainderLoop
@@Large:
  mov     eax, [ebp-4]     {Compare Last 4 Characters}
  cmp     eax, [edx-4]
  jne     @@MainLoop       {No Match on Last 4 Characters}
  mov     ebx, edi
@@CompareLoop:             {Compare Remaining Characters}
  add     ebx, 4           {Compare 4 Characters per Loop}
  jge     @@SetResult      {All Characters Matched}
  mov     eax, [ebp+ebx-4]
  cmp     eax, [edx+ebx-4]
  je      @@CompareLoop    {Match on Next 4 Characters}
  jmp     @@MainLoop       {No Match}
end; {PosEx}

{*********************************************************************************}
function ALPosEx_Sha_IA32_3(const SubStr, Str: string; Offset: integer=1): integer;
asm
       test  eax, eax
       jz    @Nil
       test  edx, edx
       jz    @Nil
       dec   ecx
       jl    @Nil

       push  esi
       push  ebx

       mov   esi, [edx-4]  //Length(Str)
       mov   ebx, [eax-4]  //Length(Substr)
       sub   esi, ecx      //effective length of Str
       add   edx, ecx      //addr of the first char at starting position
       cmp   esi, ebx
       jl    @Past         //jump if EffectiveLength(Str)<Length(Substr)
       test  ebx, ebx
       jle   @Past         //jump if Length(Substr)<=0

       add   esp, -12
       add   ebx, -1       //Length(Substr)-1
       add   esi, edx      //addr of the terminator
       add   edx, ebx      //addr of the last char at starting position
       mov   [esp+8], esi  //save addr of the terminator
       add   eax, ebx      //addr of the last char of Substr
       sub   ecx, edx      //-@Str[Length(Substr)]
       neg   ebx           //-(Length(Substr)-1)
       mov   [esp+4], ecx  //save -@Str[Length(Substr)]
       mov   [esp], ebx    //save -(Length(Substr)-1)
       movzx ecx, byte ptr [eax] //the last char of Substr

@SmallLoop:
       cmp   cl, [edx]
       jz    @Test0
@AfterTest0:
       cmp   cl, [edx+1]
       jz    @TestT
@AfterTest1:
       add   edx, 8
       cmp   edx, [esp+8]
       jae   @EndSmall
@MainLoop:
       cmp   cl, [edx-6]
       jz    @Test6
       cmp   cl, [edx-5]
       jz    @Test5
       cmp   cl, [edx-4]
       jz    @Test4
       cmp   cl, [edx-3]
       jz    @Test3
       cmp   cl, [edx-2]
       jz    @Test2
       cmp   cl, [edx-1]
       jz    @Test1
       cmp   cl, [edx]
       jz    @Test0
       cmp   cl, [edx+1]
       jz    @TestT
       add   edx, 8
       cmp   edx, [esp+8]
       jb    @MainLoop
@EndSmall:
       add   edx, -6
       cmp   edx, [esp+8]
       jb    @SmallLoop
@Exit:
       add   esp, 12
@Past:
       pop   ebx
       pop   esi
@Nil:
       xor   eax, eax
       ret

@Test6:
       add   edx, -2
@Test4:
       add   edx, -2
@Test2:
       add   edx, -2
@Test0:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found0
@Loop0:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx]
       jnz   @AfterTest0
       cmp   esi, -2
       jge   @Found0
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+2]
       jnz   @AfterTest0
       add   esi, 4
       jl    @Loop0
@Found0:
       mov   eax, [esp+4]
       add   edx, 1
       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
       ret

@Test5:
       add   edx, -2
@Test3:
       add   edx, -2
@Test1:
       add   edx, -2
@TestT:
       mov   esi, [esp]
       test  esi, esi
       jz    @Found1
@Loop1:
       movzx ebx, word ptr [esi+eax]
       cmp   bx, word ptr [esi+edx+1]
       jnz   @AfterTest1
       cmp   esi, -2
       jge   @Found1
       movzx ebx, word ptr [esi+eax+2]
       cmp   bx, word ptr [esi+edx+3]
       jnz   @AfterTest1
       add   esi, 4
       jl    @Loop1
@Found1:
       mov   eax, [esp+4]
       add   edx, 2

       cmp   edx, [esp+8]
       ja    @Exit

       add   esp, 12
       add   eax, edx
       pop   ebx
       pop   esi
end;

{********************************************************************************}
function ALPosEx_Sha_Pas_2(const SubStr, S: string; Offset: Integer = 1): Integer;
var
  len, lenSub: integer;
  ch: char;
  p, pSub, pStart, pStop: pchar;
label
  Loop0, Loop4,
  TestT, Test0, Test1, Test2, Test3, Test4,
  AfterTestT, AfterTest0,
  Ret, Exit;
begin;
  pSub:=pointer(SubStr);
  p:=pointer(S);

  if (p=nil) or (pSub=nil) or (Offset<1) then begin;
    Result:=0;
    goto Exit;
    end;

  lenSub:=pinteger(pSub-4)^-1;
  len:=pinteger(p-4)^;
  if (len<lenSub+Offset) or (lenSub<0) then begin;
    Result:=0;
    goto Exit;
    end;

  pStop:=p+len;
  p:=p+lenSub;
  pSub:=pSub+lenSub;
  pStart:=p;
  p:=p+Offset+3;

  ch:=pSub[0];
  lenSub:=-lenSub;
  if p<pStop then goto Loop4;
  p:=p-4;
  goto Loop0;

Loop4:
  if ch=p[-4] then goto Test4;
  if ch=p[-3] then goto Test3;
  if ch=p[-2] then goto Test2;
  if ch=p[-1] then goto Test1;
Loop0:
  if ch=p[0] then goto Test0;
AfterTest0:
  if ch=p[1] then goto TestT;
AfterTestT:
  p:=p+6;
  if p<pStop then goto Loop4;
  p:=p-4;
  if p<pStop then goto Loop0;
  Result:=0;
  goto Exit;

Test3: p:=p-2;
Test1: p:=p-2;
TestT: len:=lenSub;
  if lenSub<>0 then repeat;
    if (psub[len]<>p[len+1])
    or (psub[len+1]<>p[len+2]) then goto AfterTestT;
    len:=len+2;
    until len>=0;
  p:=p+2;
  if p<=pStop then goto Ret;
  Result:=0;
  goto Exit;

Test4: p:=p-2;
Test2: p:=p-2;
Test0: len:=lenSub;
  if lenSub<>0 then repeat;
    if (psub[len]<>p[len])
    or (psub[len+1]<>p[len+1]) then goto AfterTest0;
    len:=len+2;
    until len>=0;
  inc(p);
Ret:
  Result:=p-pStart;
Exit:
end;

{**********************************}
{Called Once by Unit Initialisation}
procedure ALInitPosExFunct;
begin
  case AlGetCpuTarget of
    fctP4R: ALPosEx := ALPosEx_JOH_IA32_8;
    fctP4N: ALPosEx := ALPosEx_JOH_IA32_8;
    fctPMY: ALPosEx := ALPosEx_Sha_IA32_3;
    fctPMD: ALPosEx := ALPosEx_Sha_IA32_3;
    fctAMD64: ALPosEx := ALPosEx_JOH_IA32_8;
    fctAMD64_SSE3: ALPosEx := ALPosEx_JOH_IA32_8;
    fctIA32SizePenalty: ALPosEx := ALPosEx_JOH_IA32_7;
    fctIA32: ALPosEx := ALPosEx_JOH_IA32_8;
    fctMMX: ALPosEx := ALPosEx_JOH_IA32_8;
    fctSSESizePenalty: ALPosEx := ALPosEx_JOH_IA32_7;
    fctSSE: ALPosEx := ALPosEx_JOH_IA32_8;
    fctSSE2: ALPosEx := ALPosEx_JOH_IA32_8;
    fctPascalSizePenalty: ALPosEx := ALPosEx_Sha_Pas_2;
    fctPascal: ALPosEx := ALPosEx_Sha_Pas_2;
  end;
end;



///////////////////////////////////////
//////////AlFastStringReplace//////////
///////////////////////////////////////

var
  vALStringReplaceAnsiUpcase: packed array[Char] of Char; {Upcase Lookup Table}
  vALStringReplacesrCodePage: UINT; {Active String Replace Windows CodePage}

{Setup Lookup Table for Ansi Uppercase}
procedure ALStringReplaceInitialiseAnsiUpcase;
var
  Ch: Char;
begin
  vALStringReplacesrCodePage := GetACP;
  for Ch := #0 to #255 do
    vALStringReplaceAnsiUpcase[Ch] := Ch;
  CharUpperBuffA(@vALStringReplaceAnsiUpcase, 256);
end;

{*********************************************************************************************}
function ALStringReplaceAnsiPosExIC(const SubStr, S: Ansistring; Offset: Integer = 1): Integer;
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx, [edx-4]     {Length(Str)}
  mov     ebx, [eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx         {Max Start Pos for Full Match}
  lea     edx, [edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi, [ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  push    edi              {Save Remainder to Check = Length(SubStr) - 2}
  push    ecx              {Save Max Start Position}
  lea     edi, vALStringReplaceAnsiUpcase  {Uppercase Lookup Table}
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [edi+ebx]   {Convert to Uppercase}
@@Loop:                    {Loop Comparing 2 Characters per Loop}
  movzx   eax, [edx]       {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  jne     @@NotChar1
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char1Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@SetResult
@@NotChar1:
  movzx   eax, [edx+1]     {Get Next Character}
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  cmp     bl, al
  jne     @@NotChar2
  mov     ebp, [esp+4]     {Remainder to Check}
@@Char2Loop:
  movzx   eax, [esi+ebp]
  movzx   ecx, [edx+ebp+1]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  movzx   eax, [esi+ebp+1]
  movzx   ecx, [edx+ebp+2]
  movzx   eax, [edi+eax]   {Convert to Uppercase}
  movzx   ecx, [edi+ecx]   {Convert to Uppercase}
  cmp     eax, ecx
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ecx
  pop     edi
  pop     ebp
  pop     edi
  jmp     @@CheckResult    {Check Match is within String Data}
@@NotChar2:
  add     edx, 2
  cmp     edx, [esp]       {Compate to Max Start Position}
  jle     @@Loop           {Loop until Start Position > Max Start Position}
  pop     ecx              {Dump Start Position}
  pop     edi              {Dump Remainder to Check}
  pop     ebp
  pop     edi
  jmp     @@NotFound
@@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  lea     esi, vALStringReplaceAnsiUpcase
  movzx   ebx, [eax]       {Search Character = 1st Char of SubStr}
  movzx   ebx, [esi+ebx]   {Convert to Uppercase}
@@CharLoop:
  movzx   eax, [edx]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@SetResult
  movzx   eax, [edx+1]
  movzx   eax, [esi+eax]   {Convert to Uppercase}
  cmp     eax, ebx
  je      @@CheckResult
  add     edx, 2
  cmp     edx, ecx
  jle     @@CharLoop
@@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
@@CheckResult:             {Check Match is within String Data}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1           {OK - Adjust Result}
@@SetResult:               {Set Result Position}
  pop     ecx              {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax, [edx+ecx+1]
end; {AnsiPosExIC}

{******************************************************************************************************}
function ALStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
type
  TPosEx = function(const SubStr, S: Ansistring; Offset: Integer): Integer;
const
  StaticBufferSize = 16;
var
  SrcLen, OldLen, NewLen, Found, Count, Start, Match, Matches, BufSize,
  Remainder    : Integer;
  PosExFunction: TPosEx;
  StaticBuffer : array[0..StaticBufferSize-1] of Integer;
  Buffer       : PIntegerArray;
  P, PSrc, PRes: PChar;
  Ch           : Char;
begin
  SrcLen := Length(S);
  OldLen := Length(OldPattern);
  NewLen := Length(NewPattern);
  if (OldLen = 0) or (SrcLen < OldLen) then
    begin
      if SrcLen = 0 then
        Result := '' {Needed for Non-Nil Zero Length Strings}
      else
        Result := S
    end
  else
    begin
      if rfIgnoreCase in Flags then
        begin
          PosExFunction := ALStringReplaceAnsiPosExIC;
          if GetACP <> vALStringReplacesrCodePage then {Check CodePage}
            ALStringReplaceInitialiseAnsiUpcase; {CodePage Changed - Update Lookup Table}
        end
      else
        PosExFunction := ALPosEx;
      if rfReplaceAll in Flags then
        begin
          if (OldLen = 1) and (NewLen = 1) then
            begin {Single Character Replacement}
              Remainder := SrcLen;
              SetLength(Result, Remainder);
              P := Pointer(Result);
              Move(Pointer(S)^, P^, Remainder);
              if rfIgnoreCase in Flags then
                begin
                  Ch := vALStringReplaceAnsiUpcase[OldPattern[1]];
                  repeat
                    Dec(Remainder);
                    if vALStringReplaceAnsiUpcase[P[Remainder]] = Ch then
                      P[Remainder] := NewPattern[1];
                  until Remainder = 0;
                end
              else
                begin
                  repeat
                    Dec(Remainder);
                    if P[Remainder] = OldPattern[1] then
                      P[Remainder] := NewPattern[1];
                  until Remainder = 0;
                end;
              Exit;
            end;
          Found := PosExFunction(OldPattern, S, 1);
          if Found <> 0 then
            begin
              Buffer    := @StaticBuffer;
              BufSize   := StaticBufferSize;
              Matches   := 1;
              Buffer[0] := Found;
              repeat
                Inc(Found, OldLen);
                Found := PosExFunction(OldPattern, S, Found);
                if Found > 0 then
                  begin
                    if Matches = BufSize then
                      begin {Create or Expand Dynamic Buffer}
                        BufSize := BufSize + (BufSize shr 1); {Grow by 50%}
                        if Buffer = @StaticBuffer then
                          begin {Create Dynamic Buffer}
                            GetMem(Buffer, BufSize * SizeOf(Integer));
                            Move(StaticBuffer, Buffer^, SizeOf(StaticBuffer));
                          end
                        else {Expand Dynamic Buffer}
                          ReallocMem(Buffer, BufSize * SizeOf(Integer));
                      end;
                    Buffer[Matches] := Found;
                    Inc(Matches);
                  end
              until Found = 0;
              SetLength(Result, SrcLen + (Matches * (NewLen - OldLen)));
              PSrc := Pointer(S);
              PRes := Pointer(Result);
              Start := 1;
              Match := 0;
              repeat
                Found := Buffer[Match];
                Count := Found - Start;
                Start := Found + OldLen;
                if Count > 0 then
                  begin
                    Move(PSrc^, PRes^, Count);
                    Inc(PRes, Count);
                  end;
                Inc(PSrc, Count + OldLen);
                Move(Pointer(NewPattern)^, PRes^, NewLen);
                Inc(PRes, NewLen);
                Inc(Match);
              until Match = Matches;
              Remainder := SrcLen - Start;
              if Remainder >= 0 then
                Move(PSrc^, PRes^, Remainder + 1);
              if BufSize <> StaticBufferSize then
                FreeMem(Buffer); {Free Dynamic Buffer if Created}
            end
          else {No Matches Found}
            Result := S
        end {ReplaceAll}
      else
        begin {Replace First Occurance Only}
          Found := PosExFunction(OldPattern, S, 1);
          if Found <> 0 then
            begin {Match Found}
              SetLength(Result, SrcLen - OldLen + NewLen);
              Dec(Found);
              PSrc := Pointer(S);
              PRes := Pointer(Result);
              if NewLen = OldLen then
                begin
                  Move(PSrc^, PRes^, SrcLen);
                  Inc(PRes, Found);
                  Move(Pointer(NewPattern)^, PRes^, NewLen);
                end
              else
                begin
                  Move(PSrc^, PRes^, Found);
                  Inc(PRes, Found);
                  Inc(PSrc, Found + OldLen);
                  Move(Pointer(NewPattern)^, PRes^, NewLen);
                  Inc(PRes, NewLen);
                  Move(PSrc^, PRes^, SrcLen - Found - OldLen);
                end;
            end
          else {No Matches Found}
            Result := S
        end;
    end;
end;

{**********************************}
{Called Once by Unit Initialisation}
procedure ALInitStringReplaceFunct;
begin
  vALStringReplacesrCodePage := 0; {Invalidate AnsiUpcase Lookup Table}
  ALStringReplaceInitialiseAnsiUpcase;
end;




//////////////////////////
//////////AlMove//////////
//////////////////////////

var
  vALMoveCacheLimit : Integer; {Used within SSE Moves}

const
  cALMoveTINYSIZE = 36;

{***********************************}
{Perform Forward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source+Count, EDX = Dest+Count.  Destroys ECX}
procedure ALSmallForwardMove_10;
asm
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Fwd01, @@Fwd02, @@Fwd03, @@Fwd04, @@Fwd05, @@Fwd06, @@Fwd07, @@Fwd08
  dd      @@Fwd09, @@Fwd10, @@Fwd11, @@Fwd12, @@Fwd13, @@Fwd14, @@Fwd15, @@Fwd16
  dd      @@Fwd17, @@Fwd18, @@Fwd19, @@Fwd20, @@Fwd21, @@Fwd22, @@Fwd23, @@Fwd24
  dd      @@Fwd25, @@Fwd26, @@Fwd27, @@Fwd28, @@Fwd29, @@Fwd30, @@Fwd31, @@Fwd32
  dd      @@Fwd33, @@Fwd34, @@Fwd35, @@Fwd36
@@Fwd36:
  mov     ecx, [eax-36]
  mov     [edx-36], ecx
@@Fwd32:
  mov     ecx, [eax-32]
  mov     [edx-32], ecx
@@Fwd28:
  mov     ecx, [eax-28]
  mov     [edx-28], ecx
@@Fwd24:
  mov     ecx, [eax-24]
  mov     [edx-24], ecx
@@Fwd20:
  mov     ecx, [eax-20]
  mov     [edx-20], ecx
@@Fwd16:
  mov     ecx, [eax-16]
  mov     [edx-16], ecx
@@Fwd12:
  mov     ecx, [eax-12]
  mov     [edx-12], ecx
@@Fwd08:
  mov     ecx, [eax-8]
  mov     [edx-8], ecx
@@Fwd04:
  mov     ecx, [eax-4]
  mov     [edx-4], ecx
  ret
  nop
@@Fwd35:
  mov     ecx, [eax-35]
  mov     [edx-35], ecx
@@Fwd31:
  mov     ecx, [eax-31]
  mov     [edx-31], ecx
@@Fwd27:
  mov     ecx, [eax-27]
  mov     [edx-27], ecx
@@Fwd23:
  mov     ecx, [eax-23]
  mov     [edx-23], ecx
@@Fwd19:
  mov     ecx, [eax-19]
  mov     [edx-19], ecx
@@Fwd15:
  mov     ecx, [eax-15]
  mov     [edx-15], ecx
@@Fwd11:
  mov     ecx, [eax-11]
  mov     [edx-11], ecx
@@Fwd07:
  mov     ecx, [eax-7]
  mov     [edx-7], ecx
  mov     ecx, [eax-4]
  mov     [edx-4], ecx
  ret
  nop
@@Fwd03:
  movzx   ecx, word ptr [eax-3]
  mov     [edx-3], cx
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1], cl
  ret
@@Fwd34:
  mov     ecx, [eax-34]
  mov     [edx-34], ecx
@@Fwd30:
  mov     ecx, [eax-30]
  mov     [edx-30], ecx
@@Fwd26:
  mov     ecx, [eax-26]
  mov     [edx-26], ecx
@@Fwd22:
  mov     ecx, [eax-22]
  mov     [edx-22], ecx
@@Fwd18:
  mov     ecx, [eax-18]
  mov     [edx-18], ecx
@@Fwd14:
  mov     ecx, [eax-14]
  mov     [edx-14], ecx
@@Fwd10:
  mov     ecx, [eax-10]
  mov     [edx-10], ecx
@@Fwd06:
  mov     ecx, [eax-6]
  mov     [edx-6], ecx
@@Fwd02:
  movzx   ecx, word ptr [eax-2]
  mov     [edx-2], cx
  ret
  nop
  nop
  nop
@@Fwd33:
  mov     ecx, [eax-33]
  mov     [edx-33], ecx
@@Fwd29:
  mov     ecx, [eax-29]
  mov     [edx-29], ecx
@@Fwd25:
  mov     ecx, [eax-25]
  mov     [edx-25], ecx
@@Fwd21:
  mov     ecx, [eax-21]
  mov     [edx-21], ecx
@@Fwd17:
  mov     ecx, [eax-17]
  mov     [edx-17], ecx
@@Fwd13:
  mov     ecx, [eax-13]
  mov     [edx-13], ecx
@@Fwd09:
  mov     ecx, [eax-9]
  mov     [edx-9], ecx
@@Fwd05:
  mov     ecx, [eax-5]
  mov     [edx-5], ecx
@@Fwd01:
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1], cl
  ret
@@Done:
end; {SmallForwardMove}

{************************************}
{Perform Backward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure ALSmallBackwardMove_10;
asm
  jmp     dword ptr [@@BwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@BwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Bwd01, @@Bwd02, @@Bwd03, @@Bwd04, @@Bwd05, @@Bwd06, @@Bwd07, @@Bwd08
  dd      @@Bwd09, @@Bwd10, @@Bwd11, @@Bwd12, @@Bwd13, @@Bwd14, @@Bwd15, @@Bwd16
  dd      @@Bwd17, @@Bwd18, @@Bwd19, @@Bwd20, @@Bwd21, @@Bwd22, @@Bwd23, @@Bwd24
  dd      @@Bwd25, @@Bwd26, @@Bwd27, @@Bwd28, @@Bwd29, @@Bwd30, @@Bwd31, @@Bwd32
  dd      @@Bwd33, @@Bwd34, @@Bwd35, @@Bwd36
@@Bwd36:
  mov     ecx, [eax+32]
  mov     [edx+32], ecx
@@Bwd32:
  mov     ecx, [eax+28]
  mov     [edx+28], ecx
@@Bwd28:
  mov     ecx, [eax+24]
  mov     [edx+24], ecx
@@Bwd24:
  mov     ecx, [eax+20]
  mov     [edx+20], ecx
@@Bwd20:
  mov     ecx, [eax+16]
  mov     [edx+16], ecx
@@Bwd16:
  mov     ecx, [eax+12]
  mov     [edx+12], ecx
@@Bwd12:
  mov     ecx, [eax+8]
  mov     [edx+8], ecx
@@Bwd08:
  mov     ecx, [eax+4]
  mov     [edx+4], ecx
@@Bwd04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
  nop
  nop
  nop
@@Bwd35:
  mov     ecx, [eax+31]
  mov     [edx+31], ecx
@@Bwd31:
  mov     ecx, [eax+27]
  mov     [edx+27], ecx
@@Bwd27:
  mov     ecx, [eax+23]
  mov     [edx+23], ecx
@@Bwd23:
  mov     ecx, [eax+19]
  mov     [edx+19], ecx
@@Bwd19:
  mov     ecx, [eax+15]
  mov     [edx+15], ecx
@@Bwd15:
  mov     ecx, [eax+11]
  mov     [edx+11], ecx
@@Bwd11:
  mov     ecx, [eax+7]
  mov     [edx+7], ecx
@@Bwd07:
  mov     ecx, [eax+3]
  mov     [edx+3], ecx
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
  nop
  nop
  nop
@@Bwd03:
  movzx   ecx, word ptr [eax+1]
  mov     [edx+1], cx
  movzx   ecx, byte ptr [eax]
  mov     [edx], cl
  ret
  nop
  nop
@@Bwd34:
  mov     ecx, [eax+30]
  mov     [edx+30], ecx
@@Bwd30:
  mov     ecx, [eax+26]
  mov     [edx+26], ecx
@@Bwd26:
  mov     ecx, [eax+22]
  mov     [edx+22], ecx
@@Bwd22:
  mov     ecx, [eax+18]
  mov     [edx+18], ecx
@@Bwd18:
  mov     ecx, [eax+14]
  mov     [edx+14], ecx
@@Bwd14:
  mov     ecx, [eax+10]
  mov     [edx+10], ecx
@@Bwd10:
  mov     ecx, [eax+6]
  mov     [edx+6], ecx
@@Bwd06:
  mov     ecx, [eax+2]
  mov     [edx+2], ecx
@@Bwd02:
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
  nop
@@Bwd33:
  mov     ecx, [eax+29]
  mov     [edx+29], ecx
@@Bwd29:
  mov     ecx, [eax+25]
  mov     [edx+25], ecx
@@Bwd25:
  mov     ecx, [eax+21]
  mov     [edx+21], ecx
@@Bwd21:
  mov     ecx, [eax+17]
  mov     [edx+17], ecx
@@Bwd17:
  mov     ecx, [eax+13]
  mov     [edx+13], ecx
@@Bwd13:
  mov     ecx, [eax+9]
  mov     [edx+9], ecx
@@Bwd09:
  mov     ecx, [eax+5]
  mov     [edx+5], ecx
@@Bwd05:
  mov     ecx, [eax+1]
  mov     [edx+1], ecx
@@Bwd01:
  movzx   ecx, byte ptr[eax]
  mov     [edx], cl
  ret
@@Done:
end; {SmallBackwardMove}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure ALForwards_IA32_10;
asm
  fild    qword ptr [eax] {First 8}
  lea     eax, [eax+ecx-8]
  lea     ecx, [edx+ecx-8]
  push    edx
  push    ecx
  fild    qword ptr [eax] {Last 8}
  neg     ecx {QWORD Align Writes}
  and     edx, -8
  lea     ecx, [ecx+edx+8]
  pop     edx
@@Loop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @@Loop
  pop     eax
  fistp   qword ptr [edx] {Last 8}
  fistp   qword ptr [eax] {First 8}
end; {Forwards_IA32}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure ALBackwards_IA32_10;
asm
  sub     ecx, 8
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx {QWORD Align Writes}
  push    ecx
  and     ecx, -8
  sub     ecx, edx
@@Loop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @@Loop
  pop     eax
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [eax] {Last 8}
end; {Backwards_IA32}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure ALForwards_MMX_10;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE {Size at which using MMX becomes worthwhile}
  jl      ALForwards_IA32_10
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  push    ebx
  mov     ebx, edx
  movq    mm0, [eax] {First 8 Bytes}
  add     eax, ecx {QWORD Align Writes}
  add     ecx, edx
  and     edx, -8
  add     edx, 40
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
  nop {Align Loop}
@@FwdLoopMMX:
  movq    mm1, [eax+ecx-32]
  movq    mm2, [eax+ecx-24]
  movq    mm3, [eax+ecx-16]
  movq    mm4, [eax+ecx- 8]
  movq    [edx+ecx-32], mm1
  movq    [edx+ecx-24], mm2
  movq    [edx+ecx-16], mm3
  movq    [edx+ecx- 8], mm4
  add     ecx, 32
  jle     @@FwdLoopMMX
  movq    [ebx], mm0 {First 8 Bytes}
  emms
  pop     ebx
  neg     ecx
  add     ecx, 32
  jmp     ALSmallForwardMove_10
  nop {Align Loop}
  nop
@@FwdLargeMove:
  push    ebx
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    ALSmallForwardMove_10
@@FwdAligned:
  mov     ecx, ebx
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    esi
  push    edi
  mov     esi, eax          {ESI = Source}
  mov     edi, edx          {EDI = Dest}
  mov     eax, ecx          {EAX = Count}
  and     eax, -64          {EAX = No of Bytes to Blocks Moves}
  and     ecx, $3F          {ECX = Remaining Bytes to Move (0..63)}
  add     esi, eax
  add     edi, eax
  neg     eax
@@MMXcopyloop:
  movq    mm0, [esi+eax   ]
  movq    mm1, [esi+eax+ 8]
  movq    mm2, [esi+eax+16]
  movq    mm3, [esi+eax+24]
  movq    mm4, [esi+eax+32]
  movq    mm5, [esi+eax+40]
  movq    mm6, [esi+eax+48]
  movq    mm7, [esi+eax+56]
  movq    [edi+eax   ], mm0
  movq    [edi+eax+ 8], mm1
  movq    [edi+eax+16], mm2
  movq    [edi+eax+24], mm3
  movq    [edi+eax+32], mm4
  movq    [edi+eax+40], mm5
  movq    [edi+eax+48], mm6
  movq    [edi+eax+56], mm7
  add     eax, 64
  jnz     @@MMXcopyloop
  emms                   {Empty MMX State}
  add     ecx, ebx
  shr     ecx, 2
  rep     movsd
  mov     ecx, ebx
  and     ecx, 3
  rep     movsb
  pop     edi
  pop     esi
  pop     ebx
end; {Forwards_MMX}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure ALBackwards_MMX_10;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE {Size at which using MMX becomes worthwhile}
  jl      ALBackwards_IA32_10
  push    ebx
  movq    mm0, [eax+ecx-8] {Get Last QWORD}
  lea     ebx, [edx+ecx] {QWORD Align Writes}
  and     ebx, 7
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
@@BwdLoopMMX:
  movq    mm1, [eax+ecx   ]
  movq    mm2, [eax+ecx+ 8]
  movq    mm3, [eax+ecx+16]
  movq    mm4, [eax+ecx+24]
  movq    [edx+ecx+24], mm4
  movq    [edx+ecx+16], mm3
  movq    [edx+ecx+ 8], mm2
  movq    [edx+ecx   ], mm1
  sub     ecx, 32
  jge     @@BwdLoopMMX
  movq    [edx+ebx-8], mm0 {Last QWORD}
  emms
  add     ecx, 32
  pop     ebx
  jmp     ALSmallBackwardMove_10
end; {Backwards_MMX}

{******************************}
procedure ALLargeAlignedSSEMove;
asm
@@Loop:
  movaps  xmm0, [eax+ecx]
  movaps  xmm1, [eax+ecx+16]
  movaps  xmm2, [eax+ecx+32]
  movaps  xmm3, [eax+ecx+48]
  movaps  [edx+ecx], xmm0
  movaps  [edx+ecx+16], xmm1
  movaps  [edx+ecx+32], xmm2
  movaps  [edx+ecx+48], xmm3
  movaps  xmm4, [eax+ecx+64]
  movaps  xmm5, [eax+ecx+80]
  movaps  xmm6, [eax+ecx+96]
  movaps  xmm7, [eax+ecx+112]
  movaps  [edx+ecx+64], xmm4
  movaps  [edx+ecx+80], xmm5
  movaps  [edx+ecx+96], xmm6
  movaps  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeAlignedSSEMove}

{********************************}
procedure ALLargeUnalignedSSEMove;
asm
@@Loop:
  movups  xmm0, [eax+ecx]
  movups  xmm1, [eax+ecx+16]
  movups  xmm2, [eax+ecx+32]
  movups  xmm3, [eax+ecx+48]
  movaps  [edx+ecx], xmm0
  movaps  [edx+ecx+16], xmm1
  movaps  [edx+ecx+32], xmm2
  movaps  [edx+ecx+48], xmm3
  movups  xmm4, [eax+ecx+64]
  movups  xmm5, [eax+ecx+80]
  movups  xmm6, [eax+ecx+96]
  movups  xmm7, [eax+ecx+112]
  movaps  [edx+ecx+64], xmm4
  movaps  [edx+ecx+80], xmm5
  movaps  [edx+ecx+96], xmm6
  movaps  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeUnalignedSSEMove}

{*****************************}
procedure ALHugeAlignedSSEMove;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movaps  xmm0, [eax+ecx]
  movaps  xmm1, [eax+ecx+16]
  movaps  xmm2, [eax+ecx+32]
  movaps  xmm3, [eax+ecx+48]
  movntps [edx+ecx], xmm0
  movntps [edx+ecx+16], xmm1
  movntps [edx+ecx+32], xmm2
  movntps [edx+ecx+48], xmm3
  movaps  xmm4, [eax+ecx+64]
  movaps  xmm5, [eax+ecx+80]
  movaps  xmm6, [eax+ecx+96]
  movaps  xmm7, [eax+ecx+112]
  movntps [edx+ecx+64], xmm4
  movntps [edx+ecx+80], xmm5
  movntps [edx+ecx+96], xmm6
  movntps [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeAlignedSSEMove}

{*******************************}
procedure ALHugeUnalignedSSEMove;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movups  xmm0, [eax+ecx]
  movups  xmm1, [eax+ecx+16]
  movups  xmm2, [eax+ecx+32]
  movups  xmm3, [eax+ecx+48]
  movntps [edx+ecx], xmm0
  movntps [edx+ecx+16], xmm1
  movntps [edx+ecx+32], xmm2
  movntps [edx+ecx+48], xmm3
  movups  xmm4, [eax+ecx+64]
  movups  xmm5, [eax+ecx+80]
  movups  xmm6, [eax+ecx+96]
  movups  xmm7, [eax+ecx+112]
  movntps [edx+ecx+64], xmm4
  movntps [edx+ecx+80], xmm5
  movntps [edx+ecx+96], xmm6
  movntps [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeUnalignedSSEMove}

{***********************************************************}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure ALLargeSSEMove(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  neg     ecx
  cmp     ecx, vALMoveCacheLimit       {Count > Limit - Use Prefetch}
  jl      @@Huge
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@LargeUnaligned
  call    ALLargeAlignedSSEMove   {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    ALLargeUnalignedSSEMove
  jmp     @@Remainder
@@Huge:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@HugeUnaligned
  call    ALHugeAlignedSSEMove    {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@HugeUnaligned:                {Source Not 16-Byte Aligned}
  call    ALHugeUnalignedSSEMove
@@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
  movups  xmm0, [eax+ebx]
  movaps  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end; {LargeSSEMove}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure ALForwards_SSE_10;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE
  jle     ALForwards_IA32_10
  push    ebx
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  movups  xmm0, [eax] {First 16 Bytes}
  mov     ebx, edx
  add     eax, ecx {Align Writes}
  add     ecx, edx
  and     edx, -16
  add     edx, 48
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
  nop {Align Loop}
@@FwdLoopSSE:
  movups  xmm1, [eax+ecx-32]
  movups  xmm2, [eax+ecx-16]
  movaps  [edx+ecx-32], xmm1
  movaps  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @@FwdLoopSSE
  movups  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     ALSmallForwardMove_10
@@FwdLargeMove:
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdLargeAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    ALSmallForwardMove_10
  mov     ecx, ebx
@@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    ALLargeSSEMove
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     ALSmallForwardMove_10
end; {Forwards_SSE}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure ALBackwards_SSE_10;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE
  jle     ALBackwards_IA32_10
  push    ebx
  movups  xmm0, [eax+ecx-16] {Last 16 Bytes}
  lea     ebx, [edx+ecx] {Align Writes}
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
@@BwdLoop:
  movups  xmm1, [eax+ecx]
  movups  xmm2, [eax+ecx+16]
  movaps  [edx+ecx], xmm1
  movaps  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @@BwdLoop
  movups  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     ALSmallBackwardMove_10
end; {Backwards_SSE}

{*******************************}
procedure ALLargeAlignedSSE2Move; {Also used in SSE3 Move}
asm
@@Loop:
  movdqa  xmm0, [eax+ecx]
  movdqa  xmm1, [eax+ecx+16]
  movdqa  xmm2, [eax+ecx+32]
  movdqa  xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], xmm0
  movdqa  [edx+ecx+16], xmm1
  movdqa  [edx+ecx+32], xmm2
  movdqa  [edx+ecx+48], xmm3
  movdqa  xmm4, [eax+ecx+64]
  movdqa  xmm5, [eax+ecx+80]
  movdqa  xmm6, [eax+ecx+96]
  movdqa  xmm7, [eax+ecx+112]
  movdqa  [edx+ecx+64], xmm4
  movdqa  [edx+ecx+80], xmm5
  movdqa  [edx+ecx+96], xmm6
  movdqa  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeAlignedSSE2Move}

{*********************************}
procedure ALLargeUnalignedSSE2Move;
asm
@@Loop:
  movdqu  xmm0, [eax+ecx]
  movdqu  xmm1, [eax+ecx+16]
  movdqu  xmm2, [eax+ecx+32]
  movdqu  xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], xmm0
  movdqa  [edx+ecx+16], xmm1
  movdqa  [edx+ecx+32], xmm2
  movdqa  [edx+ecx+48], xmm3
  movdqu  xmm4, [eax+ecx+64]
  movdqu  xmm5, [eax+ecx+80]
  movdqu  xmm6, [eax+ecx+96]
  movdqu  xmm7, [eax+ecx+112]
  movdqa  [edx+ecx+64], xmm4
  movdqa  [edx+ecx+80], xmm5
  movdqa  [edx+ecx+96], xmm6
  movdqa  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeUnalignedSSE2Move}

{******************************}
procedure ALHugeAlignedSSE2Move; {Also used in SSE3 Move}
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movdqa  xmm0, [eax+ecx]
  movdqa  xmm1, [eax+ecx+16]
  movdqa  xmm2, [eax+ecx+32]
  movdqa  xmm3, [eax+ecx+48]
  movntdq [edx+ecx], xmm0
  movntdq [edx+ecx+16], xmm1
  movntdq [edx+ecx+32], xmm2
  movntdq [edx+ecx+48], xmm3
  movdqa  xmm4, [eax+ecx+64]
  movdqa  xmm5, [eax+ecx+80]
  movdqa  xmm6, [eax+ecx+96]
  movdqa  xmm7, [eax+ecx+112]
  movntdq [edx+ecx+64], xmm4
  movntdq [edx+ecx+80], xmm5
  movntdq [edx+ecx+96], xmm6
  movntdq [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeAlignedSSE2Move}

{********************************}
procedure ALHugeUnalignedSSE2Move;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movdqu  xmm0, [eax+ecx]
  movdqu  xmm1, [eax+ecx+16]
  movdqu  xmm2, [eax+ecx+32]
  movdqu  xmm3, [eax+ecx+48]
  movntdq [edx+ecx], xmm0
  movntdq [edx+ecx+16], xmm1
  movntdq [edx+ecx+32], xmm2
  movntdq [edx+ecx+48], xmm3
  movdqu  xmm4, [eax+ecx+64]
  movdqu  xmm5, [eax+ecx+80]
  movdqu  xmm6, [eax+ecx+96]
  movdqu  xmm7, [eax+ecx+112]
  movntdq [edx+ecx+64], xmm4
  movntdq [edx+ecx+80], xmm5
  movntdq [edx+ecx+96], xmm6
  movntdq [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeUnalignedSSE2Move}

{***********************************************************}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure ALLargeSSE2Move(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  neg     ecx
  cmp     ecx, vALMoveCacheLimit       {Count > Limit - Use Prefetch}
  jl      @@Huge
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@LargeUnaligned
  call    ALLargeAlignedSSE2Move  {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    ALLargeUnalignedSSE2Move
  jmp     @@Remainder
@@Huge:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@HugeUnaligned
  call    ALHugeAlignedSSE2Move   {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@HugeUnaligned:                {Source Not 16-Byte Aligned}
  call    ALHugeUnalignedSSE2Move
@@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
  movdqu  xmm0, [eax+ebx]
  movdqa  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end; {LargeSSE2Move}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure ALForwards_SSE2_10;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE
  jle     ALForwards_IA32_10
  push    ebx
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  movdqu  xmm0, [eax] {First 16 Bytes}
  mov     ebx, edx
  add     eax, ecx {Align Writes}
  add     ecx, edx
  and     edx, -16
  add     edx, 48
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
@@FwdLoopSSE2:
  movdqu  xmm1, [eax+ecx-32]
  movdqu  xmm2, [eax+ecx-16]
  movdqa  [edx+ecx-32], xmm1
  movdqa  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @@FwdLoopSSE2
  movdqu  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     ALSmallForwardMove_10
@@FwdLargeMove:
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdLargeAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    ALSmallForwardMove_10
  mov     ecx, ebx
@@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    ALLargeSSE2Move
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     ALSmallForwardMove_10
end; {Forwards_SSE2}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure ALBackwards_SSE2_10;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE
  jle     ALBackwards_IA32_10
  push    ebx
  movdqu  xmm0, [eax+ecx-16] {Last 16 Bytes}
  lea     ebx, [edx+ecx] {Align Writes}
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
  add     edi, 0 {3-Byte NOP Equivalent to Align Loop}
@@BwdLoop:
  movdqu  xmm1, [eax+ecx]
  movdqu  xmm2, [eax+ecx+16]
  movdqa  [edx+ecx], xmm1
  movdqa  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @@BwdLoop
  movdqu  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     ALSmallBackwardMove_10
end; {Backwards_SSE2}

{*********************************}
procedure ALLargeUnalignedSSE3Move;
asm
@@Loop:
{$IFDEF ALSSE2Basm}
  lddqu   xmm0, [eax+ecx]
  lddqu   xmm1, [eax+ecx+16]
  lddqu   xmm2, [eax+ecx+32]
  lddqu   xmm3, [eax+ecx+48]
{$ELSE}
  DB      $F2,$0F,$F0,$04,$01
  DB      $F2,$0F,$F0,$4C,$01,$10
  DB      $F2,$0F,$F0,$54,$01,$20
  DB      $F2,$0F,$F0,$5C,$01,$30
{$ENDIF}
  movdqa  [edx+ecx], xmm0
  movdqa  [edx+ecx+16], xmm1
  movdqa  [edx+ecx+32], xmm2
  movdqa  [edx+ecx+48], xmm3
{$IFDEF ALSSE2Basm}
  lddqu   xmm4, [eax+ecx+64]
  lddqu   xmm5, [eax+ecx+80]
  lddqu   xmm6, [eax+ecx+96]
  lddqu   xmm7, [eax+ecx+112]
{$ELSE}
  DB      $F2,$0F,$F0,$64,$01,$40
  DB      $F2,$0F,$F0,$6C,$01,$50
  DB      $F2,$0F,$F0,$74,$01,$60
  DB      $F2,$0F,$F0,$7C,$01,$70
{$ENDIF}
  movdqa  [edx+ecx+64], xmm4
  movdqa  [edx+ecx+80], xmm5
  movdqa  [edx+ecx+96], xmm6
  movdqa  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeUnalignedSSE3Move}

{********************************}
procedure ALHugeUnalignedSSE3Move;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
{$IFDEF ALSSE2Basm}
  lddqu   xmm0, [eax+ecx]
  lddqu   xmm1, [eax+ecx+16]
  lddqu   xmm2, [eax+ecx+32]
  lddqu   xmm3, [eax+ecx+48]
{$ELSE}
  DB      $F2,$0F,$F0,$04,$01
  DB      $F2,$0F,$F0,$4C,$01,$10
  DB      $F2,$0F,$F0,$54,$01,$20
  DB      $F2,$0F,$F0,$5C,$01,$30
{$ENDIF}
  movntdq [edx+ecx], xmm0
  movntdq [edx+ecx+16], xmm1
  movntdq [edx+ecx+32], xmm2
  movntdq [edx+ecx+48], xmm3
{$IFDEF ALSSE2Basm}
  lddqu   xmm4, [eax+ecx+64]
  lddqu   xmm5, [eax+ecx+80]
  lddqu   xmm6, [eax+ecx+96]
  lddqu   xmm7, [eax+ecx+112]
{$ELSE}
  DB      $F2,$0F,$F0,$64,$01,$40
  DB      $F2,$0F,$F0,$6C,$01,$50
  DB      $F2,$0F,$F0,$74,$01,$60
  DB      $F2,$0F,$F0,$7C,$01,$70
{$ENDIF}
  movntdq [edx+ecx+64], xmm4
  movntdq [edx+ecx+80], xmm5
  movntdq [edx+ecx+96], xmm6
  movntdq [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeUnalignedSSE3Move}

{***********************************************************}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure ALLargeSSE3Move(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  neg     ecx
  cmp     ecx, vALMoveCacheLimit       {Count > Limit - Use Prefetch}
  jl      @@Huge
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@LargeUnaligned
  call    ALLargeAlignedSSE2Move  {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    ALLargeUnalignedSSE3Move
  jmp     @@Remainder
@@Huge:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@HugeUnaligned
  call    ALHugeAlignedSSE2Move   {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@HugeUnaligned:                {Source Not 16-Byte Aligned}
  call    ALHugeUnalignedSSE3Move
@@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
{$IFDEF ALSSE2Basm}
  lddqu   xmm0, [eax+ebx]
{$ELSE}
  DB      $F2,$0F,$F0,$04,$03
{$ENDIF}
  movdqa  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end; {LargeSSE3Move}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure ALForwards_SSE3_10;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE
  jle     ALForwards_IA32_10
  push    ebx
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
{$IFDEF ALSSE2Basm}
  lddqu   xmm0, [eax] {First 16 Bytes}
{$ELSE}
  DB      $F2,$0F,$F0,$00
{$ENDIF}
  mov     ebx, edx
  add     eax, ecx {Align Writes}
  add     ecx, edx
  and     edx, -16
  add     edx, 48
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
@@FwdLoopSSE3:
{$IFDEF ALSSE2Basm}
  lddqu   xmm1, [eax+ecx-32]
  lddqu   xmm2, [eax+ecx-16]
{$ELSE}
  DB      $F2,$0F,$F0,$4C,$01,$E0
  DB      $F2,$0F,$F0,$54,$01,$F0
{$ENDIF}
  movdqa  [edx+ecx-32], xmm1
  movdqa  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @@FwdLoopSSE3
  movdqu  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     ALSmallForwardMove_10
@@FwdLargeMove:
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdLargeAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    ALSmallForwardMove_10
  mov     ecx, ebx
@@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    ALLargeSSE3Move
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     ALSmallForwardMove_10
end; {Forwards_SSE3}

{***********************************************************************}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure ALBackwards_SSE3_10;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE
  jle     ALBackwards_IA32_10
  push    ebx
{$IFDEF ALSSE2Basm}
  lddqu   xmm0, [eax+ecx-16] {Last 16 Bytes}
{$ELSE}
  DB      $F2,$0F,$F0,$44,$01,$F0
{$ENDIF}
  lea     ebx, [edx+ecx] {Align Writes}
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
  add     edi, 0 {3-Byte NOP Equivalent to Align Loop}
@@BwdLoop:
{$IFDEF ALSSE2Basm}
  lddqu   xmm1, [eax+ecx]
  lddqu   xmm2, [eax+ecx+16]
{$ELSE}
  DB      $F2,$0F,$F0,$0C,$01
  DB      $F2,$0F,$F0,$54,$01,$10
{$ENDIF}
  movdqa  [edx+ecx], xmm1
  movdqa  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @@BwdLoop
  movdqu  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     ALSmallBackwardMove_10
end; {Backwards_SSE3}

{************************************}
{Move using IA32 Instruction Set Only}
procedure ALMoveJOH_IA32_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, cALMoveTINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     ALSmallForwardMove_10
@@SmallCheck:
  jne     ALSmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      ALForwards_IA32_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     ALForwards_IA32_10
  jmp     ALBackwards_IA32_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_IA32}

{******************************}
{Move using MMX Instruction Set}
procedure ALMoveJOH_MMX_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, cALMoveTINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     ALSmallForwardMove_10
@@SmallCheck:
  jne     ALSmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      ALForwards_MMX_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     ALForwards_MMX_10
  jmp     ALBackwards_MMX_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_MMX}

{******************************}
{Move using SSE Instruction Set}
procedure ALMoveJOH_SSE_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, cALMoveTINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     ALSmallForwardMove_10
@@SmallCheck:
  jne     ALSmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      ALForwards_SSE_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     ALForwards_SSE_10
  jmp     ALBackwards_SSE_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_SSE}

{*******************************}
{Move using SSE2 Instruction Set}
procedure ALMoveJOH_SSE2_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, cALMOVETINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     ALSmallForwardMove_10
@@SmallCheck:
  jne     ALSmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      ALForwards_SSE2_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     ALForwards_SSE2_10
  jmp     ALBackwards_SSE2_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_SSE2}

{*******************************}
{Move using SSE3 Instruction Set}
procedure ALMoveJOH_SSE3_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, cALMoveTINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     ALSmallForwardMove_10
@@SmallCheck:
  jne     ALSmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      ALForwards_SSE3_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     ALForwards_SSE3_10
  jmp     ALBackwards_SSE3_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_SSE3}


{**********************************}
{Called Once by Unit Initialisation}
procedure ALInitMovProc;
Var aCpuInfo: TALCpuinfo;
begin
  aCpuInfo := AlGetCpuInfo;
  if isSSE3 in aCpuInfo.InstructionSupport then ALMove := ALMoveJOH_SSE3_10 {Processor Supports SSE3}
  else if isSSE2 in aCpuInfo.InstructionSupport then ALMove := ALMoveJOH_SSE2_10 {Processor Supports SSE2}
  else if isSSE in aCpuInfo.InstructionSupport then ALMove := ALMoveJOH_SSE_10 {Processor Supports SSE}
  else if isMMX in aCpuInfo.InstructionSupport then ALMove := ALMoveJOH_MMX_10 {Processor Supports MMX}
  else ALMove := ALMoveJOH_IA32_10; {Processor does not Support MMX or SSE}
  vALMoveCacheLimit := aCpuInfo.L2CacheSize * -512; {Used within SSE Based Moves}
end;



/////////////////////////
//////////ALPos//////////
/////////////////////////

{*************************************************************************************}
function ALPos_JOH_IA32_6(const SubStr : AnsiString; const Str : AnsiString) : Integer;
asm {Slightly Cut-Down version of PosEx_JOH_6}
  push    ebx
  cmp     eax, 1
  sbb     ebx, ebx         {-1 if SubStr = '' else 0}
  sub     edx, 1           {-1 if S = ''}
  sbb     ebx, 0           {Negative if S = '' or SubStr = '' else 0}
  jl      @@InvalidInput
  push    edi
  push    esi
  push    ebp
  push    edx
  mov     edi, [eax-4]     {Length(SubStr)}
  mov     esi, [edx-3]     {Length(S)}
  cmp     edi, esi
  jg      @@NotFound       {Offset to High for a Match}
  test    edi, edi
  jz      @@NotFound       {Length(SubStr = 0)}
  lea     ebp, [eax+edi]   {Last Character Position in SubStr + 1}
  add     esi, edx         {Last Character Position in S}
  movzx   eax, [ebp-1]     {Last Character of SubStr}
  add     edx, edi         {Search Start Position in S for Last Character}
  mov     ah, al
  neg     edi              {-Length(SubStr)}
  mov     ecx, eax
  shl     eax, 16
  or      ecx, eax         {All 4 Bytes = Last Character of SubStr}
@@MainLoop:
  add     edx, 4
  cmp     edx, esi
  ja      @@Remainder      {1 to 4 Positions Remaining}
  mov     eax, [edx-4]     {Check Next 4 Bytes of S}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@MainLoop       {Loop Until any Match on Last Character Found}
  bsf     eax, eax         {Find First Match Bit}
  shr     eax, 3           {Byte Offset of First Match (0..3)}
  lea     edx, [eax+edx-3] {Address of First Match on Last Character + 1}
@@Compare:
  cmp     edi, -4
  jle     @@Large          {Lenght(SubStr) >= 4}
  cmp     edi, -1
  je      @@SetResult      {Exit with Match if Lenght(SubStr) = 1}
  movzx   eax, word ptr [ebp+edi] {Last Char Matches - Compare First 2 Chars}
  cmp     ax, [edx+edi]
  jne     @@MainLoop       {No Match on First 2 Characters}
@@SetResult:               {Full Match}
  lea     eax, [edx+edi]   {Calculate and Return Result}
  pop     edx
  pop     ebp
  pop     esi
  pop     edi
  pop     ebx
  sub     eax, edx         {Subtract Start Position}
  ret
@@NotFound:
  pop     edx              {Dump Start Position}
  pop     ebp
  pop     esi
  pop     edi
@@InvalidInput:
  pop     ebx
  xor     eax, eax         {No Match Found - Return 0}
  ret
@@Remainder:               {Check Last 1 to 4 Characters}
  mov     eax, [esi-3]     {Last 4 Characters of S - May include Length Bytes}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@NotFound       {No Match Possible}
  lea     eax, [edx-4]     {Check Valid Match Positions}
  cmp     cl, [eax]
  lea     edx, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+2]
  cmp     cl, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+3]
  cmp     cl, [eax+2]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+4]
  jmp     @@Compare
@@Large:
  mov     eax, [ebp-4]     {Compare Last 4 Characters of S and SubStr}
  cmp     eax, [edx-4]
  jne     @@MainLoop       {No Match on Last 4 Characters}
  mov     ebx, edi
@@CompareLoop:             {Compare Remaining Characters}
  add     ebx, 4           {Compare 4 Characters per Loop}
  jge     @@SetResult      {All Characters Matched}
  mov     eax, [ebp+ebx-4]
  cmp     eax, [edx+ebx-4]
  je      @@CompareLoop    {Match on Next 4 Characters}
  jmp     @@MainLoop       {No Match}
end;

{*************************************************************************************}
function ALPos_JOH_SSE2_3(const SubStr : AnsiString; const Str : AnsiString) : Integer;
asm
  test      eax, eax
  jz        @NotFoundExit    {Exit if SurStr = ''}
  test      edx, edx
  jz        @NotFound        {Exit if Str = ''}
  mov       ecx, [edx-4]     {Length(Str)}
  cmp       [eax-4], 1       {Length SubStr = 1?}
  je        @SingleChar      {Yes - Exit via CharPos}
  jl        @NotFound        {Exit if Length(SubStr) < 1}
  sub       ecx, [eax-4]     {Subtract Length(SubStr)}
  jl        @NotFound        {Exit if Length(SubStr) > Length(Str)}
  push      esi              {Save Registers}
  push      edi
  push      ebx
  push      ebp
  mov       esi, eax         {Start Address of SubStr}
  lea       edi, [ecx+1]     {Initial Remainder Count}
  mov       eax, [eax]       {AL = 1st Char of SubStr}
  mov       ebp, edx         {Start Address of Str}
  mov       ebx, eax         {Maintain 1st Search Char in BL}
@StrLoop:
  mov       eax, ebx         {AL  = 1st char of SubStr}
  mov       ecx, edi         {Remaining Length}
  push      edx              {Save Start Position}
  call      @CharPos         {Search for 1st Character}
  pop       edx              {Restore Start Position}
  test      eax, eax         {Result = 0?}
  jz        @StrExit         {Exit if 1st Character Not Found}
  mov       ecx, [esi-4]     {Length SubStr}
  add       edx, eax         {Update Start Position for Next Loop}
  sub       edi, eax         {Update Remaining Length for Next Loop}
  sub       ecx, 1           {Remaining Characters to Compare}
@StrCheck:
  mov       ax, [edx+ecx-2]  {Compare Next Char of SubStr and Str}
  cmp       ax, [esi+ecx-1]
  jne       @StrLoop         {Different - Return to First Character Search}
  sub       ecx, 2
  jg        @StrCheck        {Check each Remaining Character}
  mov       eax, edx         {All Characters Matched - Calculate Result}
  sub       eax, ebp
@StrExit:
  pop       ebp              {Restore Registers}
  pop       ebx
  pop       edi
  pop       esi
  ret
@NotFound:
  xor       eax, eax         {Return 0}
@NotFoundExit:
  ret
@SingleChar:
  mov       al, [eax]        {Search Character}
@CharPos:
  PUSH      EBX
  MOV       EBX, EAX
  CMP       ECX, 16
  JL        @@Small
@@NotSmall:
  MOV       AH, AL           {Fill each Byte of XMM1 with AL}
  MOVD      XMM1, EAX
  PSHUFLW   XMM1, XMM1, 0
  PSHUFD    XMM1, XMM1, 0
@@First16:
  MOVUPS    XMM0, [EDX]      {Unaligned}
  PCMPEQB   XMM0, XMM1       {Compare First 16 Characters}
  PMOVMSKB  EAX, XMM0
  TEST      EAX, EAX
  JNZ       @@FoundStart     {Exit on any Match}
  CMP       ECX, 32
  JL        @@Medium         {If Length(Str) < 32, Check Remainder}
@@Align:
  SUB       ECX, 16          {Align Block Reads}
  PUSH      ECX
  MOV       EAX, EDX
  NEG       EAX
  AND       EAX, 15
  ADD       EDX, ECX
  NEG       ECX
  ADD       ECX, EAX
@@Loop:
  MOVAPS    XMM0, [EDX+ECX]  {Aligned}
  PCMPEQB   XMM0, XMM1       {Compare Next 16 Characters}
  PMOVMSKB  EAX, XMM0
  TEST      EAX, EAX
  JNZ       @@Found          {Exit on any Match}
  ADD       ECX, 16
  JLE       @@Loop
@Remainder:
  POP       EAX              {Check Remaining Characters}
  ADD       EDX, 16
  ADD       EAX, ECX         {Count from Last Loop End Position}
  JMP       DWORD PTR [@@JumpTable2-ECX*4]

@@NullString:
  XOR       EAX, EAX         {Result = 0}
  RET

@@FoundStart:
  BSF       EAX, EAX         {Get Set Bit}
  POP       EBX
  ADD       EAX, 1           {Set Result}
  RET

@@Found:
  POP       EDX
  BSF       EAX, EAX         {Get Set Bit}
  ADD       EDX, ECX
  POP       EBX
  LEA       EAX, [EAX+EDX+1] {Set Result}
  RET

@@Medium:
  ADD       EDX, ECX         {End of String}
  MOV       EAX, 16          {Count from 16}
  JMP       DWORD PTR [@@JumpTable1-64-ECX*4]

@@Small:
  ADD       EDX, ECX         {End of String}
  XOR       EAX, EAX         {Count from 0}
  JMP       DWORD PTR [@@JumpTable1-ECX*4]

  nop; nop                   {Aligb Jump Tables}

@@JumpTable1:
  DD        @@NotFound, @@01, @@02, @@03, @@04, @@05, @@06, @@07
  DD        @@08, @@09, @@10, @@11, @@12, @@13, @@14, @@15, @@16

@@JumpTable2:
  DD        @@16, @@15, @@14, @@13, @@12, @@11, @@10, @@09, @@08
  DD        @@07, @@06, @@05, @@04, @@03, @@02, @@01, @@NotFound

@@16:
  ADD       EAX, 1
  CMP       BL, [EDX-16]
  JE        @@Done
@@15:
  ADD       EAX, 1
  CMP       BL, [EDX-15]
  JE        @@Done
@@14:
  ADD       EAX, 1
  CMP       BL, [EDX-14]
  JE        @@Done
@@13:
  ADD       EAX, 1
  CMP       BL, [EDX-13]
  JE        @@Done
@@12:
  ADD       EAX, 1
  CMP       BL, [EDX-12]
  JE        @@Done
@@11:
  ADD       EAX, 1
  CMP       BL, [EDX-11]
  JE        @@Done
@@10:
  ADD       EAX, 1
  CMP       BL, [EDX-10]
  JE        @@Done
@@09:
  ADD       EAX, 1
  CMP       BL, [EDX-9]
  JE        @@Done
@@08:
  ADD       EAX, 1
  CMP       BL, [EDX-8]
  JE        @@Done
@@07:
  ADD       EAX, 1
  CMP       BL, [EDX-7]
  JE        @@Done
@@06:
  ADD       EAX, 1
  CMP       BL, [EDX-6]
  JE        @@Done
@@05:
  ADD       EAX, 1
  CMP       BL, [EDX-5]
  JE        @@Done
@@04:
  ADD       EAX, 1
  CMP       BL, [EDX-4]
  JE        @@Done
@@03:
  ADD       EAX, 1
  CMP       BL, [EDX-3]
  JE        @@Done
@@02:
  ADD       EAX, 1
  CMP       BL, [EDX-2]
  JE        @@Done
@@01:
  ADD       EAX, 1
  CMP       BL, [EDX-1]
  JE        @@Done
@@NotFound:
  XOR       EAX, EAX
@@Done:
  POP       EBX
end;

{*********************************************************************************************************************}
function ALPos_JOH_PAS_6(const SubStr : AnsiString; const Str : AnsiString) : Integer; {$IFDEF ALInline} inline; {$ENDIF}
begin
  Result := Pos(SubStr, Str);
end;

{**********************************}
{Called Once by Unit Initialisation}
procedure ALInitPosFunct;
begin
  case AlGetCpuTarget of
    fctP4R: ALPos := ALPos_JOH_IA32_6;
    fctP4N: ALPos := ALPos_JOH_IA32_6;
    fctPMY: ALPos := ALPos_JOH_SSE2_3;
    fctPMD: ALPos := ALPos_JOH_SSE2_3;
    fctAMD64: ALPos := ALPos_JOH_IA32_6;
    fctAMD64_SSE3: ALPos := ALPos_JOH_IA32_6;
    fctIA32SizePenalty: ALPos := ALPos_JOH_PAS_6;
    fctIA32: ALPos := ALPos_JOH_IA32_6;
    fctMMX: ALPos := ALPos_JOH_IA32_6;
    fctSSESizePenalty: ALPos := ALPos_JOH_PAS_6;
    fctSSE: ALPos := ALPos_JOH_IA32_6;
    fctSSE2: ALPos := ALPos_JOH_IA32_6;
    fctPascalSizePenalty: ALPos := ALPos_JOH_PAS_6;
    fctPascal: ALPos := ALPos_JOH_PAS_6;
  end;
end;





/////////////////////////////
//////////ALCharPos//////////
/////////////////////////////

{*************************************************************************}
function ALCharPos_JOH_SSE2_1(Ch : Char; const Str : AnsiString) : Integer;
asm
  test      edx, edx
  jz        @@NullString
  mov       ecx, [edx-4]
  push      ebx
  mov       ebx, eax
  cmp       ecx, 16
  jl        @@Small
@@NotSmall:
  mov       ah, al           {Fill each Byte of XMM1 with AL}
  movd      xmm1, eax
  pshuflw   xmm1, xmm1, 0
  pshufd    xmm1, xmm1, 0
@@First16:
  movups    xmm0, [edx]      {Unaligned}
  pcmpeqb   xmm0, xmm1       {Compare First 16 Characters}
  pmovmskb  eax, xmm0
  test      eax, eax
  jnz       @@FoundStart     {Exit on any Match}
  cmp       ecx, 32
  jl        @@Medium         {If Length(Str) < 32, Check Remainder}
@@Align:
  sub       ecx, 16          {Align Block Reads}
  push      ecx
  mov       eax, edx
  neg       eax
  and       eax, 15
  add       edx, ecx
  neg       ecx
  add       ecx, eax
@@Loop:
  movaps    xmm0, [edx+ecx]  {Aligned}
  pcmpeqb   xmm0, xmm1       {Compare Next 16 Characters}
  pmovmskb  eax, xmm0
  test      eax, eax
  jnz       @@Found          {Exit on any Match}
  add       ecx, 16
  jle       @@Loop
  pop       eax              {Check Remaining Characters}
  add       edx, 16
  add       eax, ecx         {Count from Last Loop End Position}
  jmp       dword ptr [@@JumpTable2-ecx*4]
  nop
  nop
@@NullString:
  xor       eax, eax         {Result = 0}
  ret
  nop
@@FoundStart:
  bsf       eax, eax         {Get Set Bit}
  pop       ebx
  inc       eax              {Set Result}
  ret
  nop
  nop
@@Found:
  pop       edx
  bsf       eax, eax         {Get Set Bit}
  add       edx, ecx
  pop       ebx
  lea       eax, [eax+edx+1] {Set Result}
  ret
@@Medium:
  add       edx, ecx         {End of String}
  mov       eax, 16          {Count from 16}
  jmp       dword ptr [@@JumpTable1-64-ecx*4]
  nop
  nop
@@Small:
  add       edx, ecx         {End of String}
  xor       eax, eax         {Count from 0}
  jmp       dword ptr [@@JumpTable1-ecx*4]
  nop
@@JumpTable1:
  dd        @@NotFound, @@01, @@02, @@03, @@04, @@05, @@06, @@07
  dd        @@08, @@09, @@10, @@11, @@12, @@13, @@14, @@15, @@16
@@JumpTable2:
  dd        @@16, @@15, @@14, @@13, @@12, @@11, @@10, @@09, @@08
  dd        @@07, @@06, @@05, @@04, @@03, @@02, @@01, @@NotFound
@@16:
  add       eax, 1
  cmp       bl, [edx-16]
  je        @@Done
@@15:
  add       eax, 1
  cmp       bl, [edx-15]
  je        @@Done
@@14:
  add       eax, 1
  cmp       bl, [edx-14]
  je        @@Done
@@13:
  add       eax, 1
  cmp       bl, [edx-13]
  je        @@Done
@@12:
  add       eax, 1
  cmp       bl, [edx-12]
  je        @@Done
@@11:
  add       eax, 1
  cmp       bl, [edx-11]
  je        @@Done
@@10:
  add       eax, 1
  cmp       bl, [edx-10]
  je        @@Done
@@09:
  add       eax, 1
  cmp       bl, [edx-9]
  je        @@Done
@@08:
  add       eax, 1
  cmp       bl, [edx-8]
  je        @@Done
@@07:
  add       eax, 1
  cmp       bl, [edx-7]
  je        @@Done
@@06:
  add       eax, 1
  cmp       bl, [edx-6]
  je        @@Done
@@05:
  add       eax, 1
  cmp       bl, [edx-5]
  je        @@Done
@@04:
  add       eax, 1
  cmp       bl, [edx-4]
  je        @@Done
@@03:
  add       eax, 1
  cmp       bl, [edx-3]
  je        @@Done
@@02:
  add       eax, 1
  cmp       bl, [edx-2]
  je        @@Done
@@01:
  add       eax, 1
  cmp       bl, [edx-1]
  je        @@Done
@@NotFound:
  xor       eax, eax
  pop       ebx
  ret
@@Done:
  pop       ebx
end;

{************************************************************************}
function ALCharPos_JOH_MMX_1(Ch : Char; const Str : AnsiString) : Integer;
asm
  TEST      EDX, EDX         {Str = NIL?}
  JZ        @@NotFound       {Yes - Jump}
  MOV       ECX, [EDX-4]     {ECX = Length(Str)}
  CMP       ECX, 8
  JG        @@NotSmall
  TEST      ECX, ECX
  JZ        @@NotFound       {Exit if Length = 0}
@@Small:
  CMP       AL, [EDX]
  JZ        @Found1
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+1]
  JZ        @Found2
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+2]
  JZ        @Found3
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+3]
  JZ        @Found4
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+4]
  JZ        @Found5
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+5]
  JZ        @Found6
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+6]
  JZ        @Found7
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+7]
  JZ        @Found8
@@NotFound:
  XOR       EAX, EAX
  RET
@Found1:
  MOV       EAX, 1
  RET
@Found2:
  MOV       EAX, 2
  RET
@Found3:
  MOV       EAX, 3
  RET
@Found4:
  MOV       EAX, 4
  RET
@Found5:
  MOV       EAX, 5
  RET
@Found6:
  MOV       EAX, 6
  RET
@Found7:
  MOV       EAX, 7
  RET
@Found8:
  MOV       EAX, 8
  RET

@@NotSmall:                  {Length(Str) > 8}
  MOV       AH, AL
  ADD       EDX, ECX
  MOVD      MM0, EAX
  PUNPCKLWD MM0, MM0
  PUNPCKLDQ MM0, MM0
  PUSH      ECX              {Save Length}
  NEG       ECX
@@First8:
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare All 8 Bytes}
  PACKSSWB  MM1, MM1         {Pack Result into 4 Bytes}
  MOVD      EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
  JGE       @@Last8
@@Align:                     {Align to Previous 8 Byte Boundary}
  LEA       EAX, [EDX+ECX]
  AND       EAX, 7           {EAX -> 0 or 4}
  SUB       ECX, EAX
@@Loop:
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare All 8 Bytes}
  PACKSSWB  MM1, MM1         {Pack Result into 4 Bytes}
  MOVD      EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
{$IFNDEF NoUnroll}
  JGE       @@Last8
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare All 8 Bytes}
  PACKSSWB  MM1, MM1         {Pack Result into 4 Bytes}
  MOVD      EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
{$ENDIF}
  JL        @@Loop
@@Last8:
  MOVQ      MM1, [EDX-8]     {Position for Last 8 Used Characters}
  POP       EDX              {Original Length}
  PCMPEQB   MM1, MM0         {Compare All 8 Bytes}
  PACKSSWB  MM1, MM1         {Pack Result into 4 Bytes}
  MOVD      EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched2       {Exit on Match at any Position}
  EMMS
  RET                        {Finished - Not Found}
@@Matched:                   {Set Result from 1st Match in EDX}
  POP       EDX              {Original Length}
  ADD       EDX, ECX
@@Matched2:
  EMMS
  SUB       EDX, 8           {Adjust for Extra ADD ECX,8 in Loop}
  TEST      AL, AL
  JNZ       @@MatchDone      {Match at Position 1 or 2}
  TEST      AH, AH
  JNZ       @@Match1         {Match at Position 3 or 4}
  SHR       EAX, 16
  TEST      AL, AL
  JNZ       @@Match2         {Match at Position 5 or 6}
  SHR       EAX, 8
  ADD       EDX, 6
  JMP       @@MatchDone
@@Match2:
  ADD       EDX, 4
  JMP       @@MatchDone
@@Match1:
  SHR       EAX, 8           {AL <- AH}
  ADD       EDX, 2
@@MatchDone:
  XOR       EAX, 2
  AND       EAX, 3           {EAX <- 1 or 2}
  ADD       EAX, EDX
end;

{************************************************************************}
function ALCharPos_JOH_SSE_1(Ch : Char; const Str : AnsiString) : Integer;
asm
  TEST      EDX, EDX         {Str = NIL?}
  JZ        @@NotFound       {Yes - Jump}
  MOV       ECX, [EDX-4]     {ECX = Length(Str)}
  CMP       ECX, 8
  JG        @@NotSmall
  TEST      ECX, ECX
  JZ        @@NotFound       {Exit if Length = 0}
@@Small:
  CMP       AL, [EDX]
  JZ        @Found1
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+1]
  JZ        @Found2
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+2]
  JZ        @Found3
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+3]
  JZ        @Found4
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+4]
  JZ        @Found5
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+5]
  JZ        @Found6
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+6]
  JZ        @Found7
  DEC       ECX
  JZ        @@NotFound
  CMP       AL, [EDX+7]
  JZ        @Found8
@@NotFound:
  XOR       EAX, EAX
  RET
@Found1:
  MOV       EAX, 1
  RET
@Found2:
  MOV       EAX, 2
  RET
@Found3:
  MOV       EAX, 3
  RET
@Found4:
  MOV       EAX, 4
  RET
@Found5:
  MOV       EAX, 5
  RET
@Found6:
  MOV       EAX, 6
  RET
@Found7:
  MOV       EAX, 7
  RET
@Found8:
  MOV       EAX, 8
  RET
@@NotSmall:
  MOV       AH, AL
  ADD       EDX, ECX
  MOVD      MM0, EAX
  PSHUFW    MM0, MM0, 0
  PUSH      ECX
  NEG       ECX
@@First8:
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare Next 8 Bytes}
  PMOVMSKB  EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
  JGE       @@Last8
@@Align:
  LEA       EAX, [EDX+ECX]
  AND       EAX, 7
  SUB       ECX, EAX
@@Loop:                      {Loop Unrolled 2X}
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare Next 8 Bytes}
  PMOVMSKB  EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
{$IFNDEF NoUnroll}
  JGE       @@Last8
  MOVQ      MM1, [EDX+ECX]
  ADD       ECX, 8
  PCMPEQB   MM1, MM0         {Compare Next 8 Bytes}
  PMOVMSKB  EAX, MM1
  TEST      EAX, EAX
  JNZ       @@Matched        {Exit on Match at any Position}
  CMP       ECX, -8          {Check if Next Loop would pass String End}
{$ENDIF}
  JL        @@loop
@@Last8:
  PCMPEQB   MM0, [EDX-8]
  POP       ECX              {Original Length}
  PMOVMSKB  EAX, MM0
  TEST      EAX, EAX
  JNZ       @@Matched2
  EMMS
  RET                        {Finished}
@@Matched:                   {Set Result from 1st Match in EcX}
  POP       EDX              {Original Length}
  ADD       ECX, EDX
@@Matched2:
  EMMS
  BSF       EDX, EAX
  LEA       EAX, [EDX+ECX-7]
end;

{********************************************************************}
function ALCharPos_Sha_IA32_1(ch: char; const s: AnsiString): integer;
asm
     test edx,edx
     jz @@ret0

     push ebp
     push ebx
     push edx
     push esi
     push edi
     mov ecx,[edx-4]

     xor ecx,-1
     jz @@pop0

     mov ah,al;     add ecx,1
     movzx edi,ax;  and ecx,-4
     shl eax,16;    sub edx,ecx
     or edi,eax;    mov ebp,$80808080

     mov eax,edi
     xor edi,[ecx+edx]
     mov esi,eax
     lea ebx,[edi-$01010101]
     xor edi,-1
     and ebx,edi
     add ecx,4
     jge @@last1
     and ebx,ebp
     jnz @@found4;
     xor esi,[ecx+edx]
     mov ebp,ebp //nop
@@find:
     lea ebx,[esi-$01010101]
     xor esi,-1
     and ebx,esi
     mov edi,[ecx+edx+4]
     add ecx,8
     jge @@last2
     xor edi,eax
     and ebx,ebp
     mov esi,[ecx+edx]
     jnz @@found0
     lea ebx,[edi-$01010101]
     xor edi,-1
     and ebx,edi
     xor esi,eax
     and ebx,ebp
     jz @@find;
@@found4:
     add ecx,4
@@found0:
     shr ebx,8;     jc @@inc0
     shr ebx,8;     jc @@inc1
     shr ebx,8;     jc @@inc2
@@inc3: inc ecx
@@inc2: inc ecx
@@inc1: inc ecx
@@inc0:
     pop edi;
     pop esi;       lea eax,[ecx+edx-7]
     pop edx;
     pop ebx;       sub eax,edx
     pop ebp;       cmp eax,[edx-4]
     jg @@ret0
     ret
@@last2:
     and ebx,ebp
     jnz @@found0
     xor edi,eax
     lea ebx,[edi-$01010101]
     xor edi,-1
     and ebx,edi
@@last1:
     and ebx,ebp
     jnz @@found4;
@@pop0:
     pop edi
     pop esi
     pop edx
     pop ebx
     pop ebp
@@ret0:
     xor eax,eax
     //ret
end;

{*******************************************************************}
function ALCharPos_Sha_Pas_1(ch: char; const s: AnsiString): integer;
var
  c, d, Mask, Sign, Lim, SaveLenAddr: integer;
label
  Next, Last4, Last3, Found1, Found2, NotFound;
begin;
    Result:=integer(s)-4;              // length address
    Mask:=byte(ch);                    // start creation mask
    Lim:=Result;
    c:=-4;
    if Result=-4 then goto NotFound;   // if empty string
    c:=c and pIntegerArray(Lim)[0];    // c:=length - length mod 4
    if c=0 then goto Last3;            // if length<4
    d:=Mask;
    Mask:=(Mask shl 8);
    Lim:=Lim+c;                        // main loop limit
    Mask:=Mask or d;
    cardinal(Sign):=$80808080;         // sign bit in each byte
    c:=Mask;
    SaveLenAddr:=Result;               // save address of length
    Mask:=Mask shl 16;
    d:=pIntegerArray(Result)[1];       // first dword of string
    Mask:=Mask or c;                   // mask created
    inc(Result,4);
    d:=d xor Mask;                     // zero in matched byte
    if cardinal(Result)>=cardinal(Lim) // if last full dword
      then goto Last4;
Next:
    c:=integer(@pchar(d)[-$01010101]); // minus 1 from each byte
    d:=d xor (-1);
    c:=c and d;                        // set sign on in matched byte
    d:=Mask;
    if c and Sign<>0 then goto Found1; // if matched in any byte
    d:=d xor pIntegerArray(Result)[1]; // zero in matched byte
    inc(Result,4);
    if cardinal(Result)<cardinal(Lim)  // if not last full dword
      then goto Next;
Last4:                                 // last dword
    c:=integer(@pchar(d)[-$01010101]); // minus 1 from each byte
    d:=d xor (-1);
    c:=c and d;                        // set sign on in matched byte
    Lim:=SaveLenAddr;                  // get address of length
    if c and Sign<>0 then goto Found2; // if matched in any byte
Last3:                                 // last (length mod 4) bytes
    c:=3;
    c:=c and pIntegerArray(Lim)[0];
    if c=0 then goto NotFound;
    if byte(Mask)=byte(pchar(Result)[4]) then begin; Result:=Result-Lim+1;
exit; end;
    if c=1 then goto NotFound;
    if byte(Mask)=byte(pchar(Result)[5]) then begin; Result:=Result-Lim+2;
exit; end;
    if c=2 then goto NotFound;
    if byte(Mask)=byte(pchar(Result)[6]) then begin; Result:=Result-Lim+3;
exit; end;
NotFound:
    Result:=0; exit;
    goto NotFound;                     // supress compiler warnings
Found1:                                // not last dword ...
    Lim:=SaveLenAddr;                  // ... need address of length
Found2:                                // finally find matched byte
    c:=c and Sign;                     // get sign of each byte
    dec(Result,Lim);                   // index of highest byte in Result
    if word(c)<>0 then dec(Result,2) else c:=c shr 16;
    if byte(c)<>0 then dec(Result);
end;

{*******************************************************************}
function ALCharPos_Sha_Pas_2(ch: char; const s: AnsiString): integer;
const
  cMinusOnes = -$01010101;
  cSignums   =  $80808080;
var
  Ndx, Len, c, d, Mask, Sign, Save, SaveEnd: integer;
label
  Small, Middle, Large,
  Found0, Found1, Found2, Found3, NotFound,
  Matched, MatchedPlus1, MatchedMinus1, NotMatched,
  Return;
begin
  c:=integer(@pchar(integer(s))[-4]);
  if c=-4 then goto NotFound;
  Len:=pinteger(c)^;
  if Len>24 then goto Large;
  Ndx:=4;
  if Ndx>Len then goto Small;

Middle:
  if pchar(c)[Ndx+0]=ch then goto Found0;
  if pchar(c)[Ndx+1]=ch then goto Found1;
  if pchar(c)[Ndx+2]=ch then goto Found2;
  if pchar(c)[Ndx+3]=ch then goto Found3;
  inc(Ndx,4);
  if Ndx<=Len then goto Middle;

  Ndx:=Len+1;
  if pchar(c)[Len+1]=ch then goto Found0;
  if pchar(c)[Len+2]=ch then goto Found1;
  if pchar(c)[Len+3]<>ch then goto NotFound;
  Result:=integer(@pchar(Ndx)[-1]); exit;
  goto Return; //drop Ndx

Small:
  if Len=0 then goto NotFound; if pchar(c)[Ndx+0]=ch then goto Found0;
  if Len=1 then goto NotFound; if pchar(c)[Ndx+1]=ch then goto Found1;
  if Len=2 then goto NotFound; if pchar(c)[Ndx+2]<>ch then goto NotFound;

Found2: Result:=integer(@pchar(Ndx)[-1]); exit;
Found1: Result:=integer(@pchar(Ndx)[-2]); exit;
Found0: Result:=integer(@pchar(Ndx)[-3]); exit;
NotFound: Result:=0; exit;
  goto NotFound; //kill warning 'Ndx might not have been initialized'
Found3: Result:=integer(@pchar(Ndx)[0]); exit;
  goto Return; //drop Ndx

Large:
  Save:=c;
    Mask:=ord(ch);
  Ndx:=integer(@pchar(c)[+4]);

    d:=Mask;
  inc(Len,c);
  SaveEnd:=Len;
    Mask:=(Mask shl 8);
  inc(Len,+4-16+3);

    Mask:=Mask or d;
  Len:=Len and (-4);
    d:=Mask;
  cardinal(Sign):=cSignums;

    Mask:=Mask shl 16;
  c:=pintegerArray(Ndx)[0];
    Mask:=Mask or d;
  inc(Ndx,4);

    c:=c xor Mask;
    d:=integer(@pchar(c)[cMinusOnes]);
    c:=c xor (-1);
    c:=c and d;
    d:=Mask;

    if c and Sign<>0 then goto MatchedMinus1;
    Ndx:=Ndx and (-4);
    d:=d xor pintegerArray(Ndx)[0];

    if cardinal(Ndx)<cardinal(Len) then repeat;
      c:=integer(@pchar(d)[cMinusOnes]);
      d:=d xor (-1);
      c:=c and d;
      d:=Mask;

      d:=d xor pintegerArray(Ndx)[1];
      if c and Sign<>0 then goto Matched;
      c:=integer(@pchar(d)[cMinusOnes]);
      d:=d xor (-1);
      c:=c and d;
      d:=pintegerArray(Ndx)[2];
      if c and Sign<>0 then goto MatchedPlus1;
      d:=d xor Mask;

      c:=integer(@pchar(d)[cMinusOnes]);
      d:=d xor (-1);
      inc(Ndx,12);
      c:=c and d;

      //if c and Sign<>0 then goto MatchedMinus1;
      d:=Mask;
      if c and Sign<>0 then goto MatchedMinus1;
      d:=d xor pintegerArray(Ndx)[0];
      until cardinal(Ndx)>=cardinal(Len);

    Len:=SaveEnd;
    while true do begin;
      c:=integer(@pchar(d)[cMinusOnes]);
      d:=d xor (-1);
      c:=c and d;
      inc(Ndx,4);
      if c and Sign<>0 then goto MatchedMinus1;
      d:=Mask;
      if cardinal(Ndx)<=cardinal(Len)
      then d:=d xor pintegerArray(Ndx)[0]
      else begin;
        if Len=0 then goto NotMatched;
        d:=d xor pintegerArray(Len)[0];
        Ndx:=Len;
        Len:=0;
        end
      end;

NotMatched:
  Result:=0; exit;

MatchedPlus1:   inc(Ndx,8);
MatchedMinus1:  dec(Ndx,4);
Matched:
    c:=c and Sign;
    dec(Ndx,integer(Save)+2);
    if word(c)=0 then begin;
      c:=c shr 16; inc(Ndx,2);
      end;
    if byte(c)<>0 then dec(Ndx);
    Result:=Ndx;
Return:
end;

{**********************************}
{Called Once by Unit Initialisation}
procedure ALInitCharPosFunct;
begin
  case AlGetCpuTarget of
    fctP4R: ALCharPos := ALCharPos_JOH_SSE2_1;
    fctP4N: ALCharPos := ALCharPos_JOH_SSE2_1;
    fctPMY: ALCharPos := ALCharPos_JOH_SSE2_1;
    fctPMD: ALCharPos := ALCharPos_JOH_SSE2_1;
    fctAMD64: ALCharPos := ALCharPos_JOH_SSE2_1;
    fctAMD64_SSE3: ALCharPos := ALCharPos_JOH_SSE2_1;
    fctIA32SizePenalty: ALCharPos := ALCharPos_Sha_IA32_1;
    fctIA32: ALCharPos := ALCharPos_Sha_Pas_2;
    fctMMX: ALCharPos := ALCharPos_JOH_MMX_1;
    fctSSESizePenalty: ALCharPos := ALCharPos_JOH_SSE_1;
    fctSSE: ALCharPos := ALCharPos_JOH_SSE_1;
    fctSSE2: ALCharPos := ALCharPos_JOH_SSE2_1;
    fctPascalSizePenalty: ALCharPos := ALCharPos_Sha_Pas_1;
    fctPascal: ALCharPos := ALCharPos_Sha_Pas_2;
  end;
end;


///////////////////////////////
//////////ALCharPosEX//////////
///////////////////////////////

{****************************************}
{Can Read DWORD containing NULL Charatcer}
function ALCharPosEX(const SearchCharacter: Char;
                     const SourceString: AnsiString;
                     Occurrence: Integer;
                     StartPos: Integer): Integer;
asm
  test   edx, edx
  jz     @@NotFoundExit        {Exit if SourceString = ''}
  cmp    ecx, 1
  jl     @@NotFoundExit        {Exit if Occurence < 1}
  mov    ebp, StartPos         {Safe since EBP automatically saved}
  sub    ebp, 1
  jl     @@NotFoundExit        {Exit if StartPos < 1}
  push   ebx
  add    ebp, edx
  mov    ebx, [edx-4]
  add    ebx, edx
  sub    ebp, ebx
  jge    @@NotFound            {Traps Zero Length Non-Nil String}
@@Loop:
  cmp    al, [ebx+ebp]
  je     @@Check1
@@Next:
  cmp    al, [ebx+ebp+1]
  je     @@Check2
@@Next2:
  cmp    al, [ebx+ebp+2]
  je     @@Check3
@@Next3:
  cmp    al, [ebx+ebp+3]
  je     @@Check4
@@Next4:
  add    ebp, 4
  jl     @@Loop
@@NotFound:
  pop    ebx
@@NotFoundExit:
  xor    eax, eax
  jmp    @@Exit
@@Check4:
  sub    ecx, 1
  jnz    @@Next4
  add    ebp, 3
  jge    @@NotFound
  jmp    @@SetResult
@@Check3:
  sub    ecx, 1
  jnz    @@Next3
  add    ebp, 2
  jge    @@NotFound
  jmp    @@SetResult
@@Check2:
  sub    ecx, 1
  jnz    @@Next2
  add    ebp, 1
  jge    @@NotFound
  jmp    @@SetResult
@@Check1:
  sub    ecx, 1
  jnz    @@Next
@@SetResult:
  lea    eax, [ebx+ebp+1]
  sub    eax, edx
  pop    ebx
@@Exit:
end;

{****************************************}
{Can Read DWORD containing NULL Charatcer}
function ALCharPosEX(const SearchCharacter: Char;
                     const SourceString: AnsiString;
                     StartPos: Integer = 1): Integer;
begin
  result := ALCharPosEX(SearchCharacter, SourceString, 1, StartPos);
end;




/////////////////////////////////
//////////ALCompareText//////////
/////////////////////////////////

{***************************************************************}
function ALCompareText_JOH_IA32_6(const S1, S2: string): Integer;
asm
  cmp     eax, edx
  je      @@Same             {S1 = S2}
  test    eax, edx
  jnz     @@Compare
  test    eax, eax
  jz      @FirstNil          {S1 = NIL}
  test    edx, edx
  jnz     @@Compare          {S1 <> NIL and S2 <> NIL}
  mov     eax, [eax-4]       {S2 = NIL, Result = Length(S1)}
  ret
@@Same:
  xor     eax, eax
  ret
@FirstNil:
  sub     eax, [edx-4]       {S1 = NIL, Result = -Length(S2)}
  ret
@@Compare:
  push    ebx
  push    ebp
  push    edi
  push    esi
  mov     ebx, [eax-4]       {Length(S1)}
  sub     ebx, [edx-4]       {Default Result if All Compared Characters Match}
  push    ebx                {Save Default Result}
  sbb     ebp, ebp
  and     ebp, ebx
  sub     ebp, [eax-4]       {-Min(Length(S1),Length(S2))}
  sub     eax, ebp           {End of S1}
  sub     edx, ebp           {End of S2}
@@MainLoop:                  {Compare 4 Characters per Loop}
  mov     ebx, [eax+ebp]
  mov     ecx, [edx+ebp]
  cmp     ebx, ecx
  je      @@Next
  mov     esi, ebx           {Convert 4 Chars in EBX into Uppercase}
  or      ebx, $80808080
  mov     edi, ebx
  sub     ebx, $7B7B7B7B
  xor     edi, esi
  or      ebx, $80808080
  sub     ebx, $66666666
  and     ebx, edi
  shr     ebx, 2
  xor     ebx, esi
  mov     esi, ecx           {Convert 4 Chars in ECX into Uppercase}
  or      ecx, $80808080
  mov     edi, ecx
  sub     ecx, $7B7B7B7B
  xor     edi, esi
  or      ecx, $80808080
  sub     ecx, $66666666
  and     ecx, edi
  shr     ecx, 2
  xor     ecx, esi
  cmp     ebx, ecx
  jne     @@CheckDiff
@@Next:
  add     ebp, 4
  jl      @@MainLoop         {Loop until all required Characters Compared}
  pop     eax                {Default Result}
  jmp     @@Done
@@CheckDiff:
  pop     eax                {Default Result}
@@DiffLoop:
  cmp     bl, cl
  jne     @@SetResult
  add     ebp, 1
  jz      @@Done             {Difference after Compare Length}
  shr     ecx, 8
  shr     ebx, 8
  jmp     @@DiffLoop
@@SetResult:
  movzx   eax, bl            {Set Result from Character Difference}
  and     ecx, $ff
  sub     eax, ecx
@@Done:
  pop     esi
  pop     edi
  pop     ebp
  pop     ebx
end;

{***************************************************************}
function ALCompareText_JOH_IA32_5(const S1, S2: string): Integer;
asm
  cmp     eax, edx
  je      @@Same             {S1 = S2}
  test    eax, edx
  jnz     @@Compare
  test    eax, eax
  jz      @FirstNil          {S1 = NIL}
  test    edx, edx
  jnz     @@Compare          {S1 <> NIL and S2 <> NIL}
  mov     eax, [eax-4]       {S2 = NIL, Result = Length(S1)}
  ret
@@Same:
  xor     eax, eax
  ret
@FirstNil:
  sub     eax, [edx-4]       {S1 = NIL, Result = -Length(S2)}
  ret
@@Compare:
  push    ebx
  push    ebp
  push    edi
  push    esi
  mov     ebx, [eax-4]       {Length(S1)}
  sub     ebx, [edx-4]       {Default Result if All Compared Characters Match}
  push    ebx                {Save Default Result}
  sbb     ebp, ebp
  and     ebp, ebx
  add     ebp, [edx-4]       {Compare Length = Min(Length(S1),Length(S2))}
  add     eax, ebp           {End of S1}
  add     edx, ebp           {End of S2}
  neg     ebp                {Negate Compare Length}
@@MainLoop:                  {Compare 4 Characters per Loop}
  mov     ebx, [eax+ebp]
  mov     ecx, [edx+ebp]
  cmp     ebx, ecx
  je      @@Next
  mov     esi, ebx           {Convert 4 Chars in EBX into Uppercase}
  or      ebx, $80808080
  mov     edi, ebx
  sub     ebx, $7B7B7B7B
  xor     edi, esi
  or      ebx, $80808080
  sub     ebx, $66666666
  and     ebx, edi
  shr     ebx, 2
  xor     ebx, esi
  mov     esi, ecx           {Convert 4 Chars in ECX into Uppercase}
  or      ecx, $80808080
  mov     edi, ecx
  sub     ecx, $7B7B7B7B
  xor     edi, esi
  or      ecx, $80808080
  sub     ecx, $66666666
  and     ecx, edi
  shr     ecx, 2
  xor     ecx, esi
  cmp     ebx, ecx
  jne     @@CheckDiff
@@Next:
  add     ebp, 4
  jl      @@MainLoop         {Loop until all required Characters Compared}
  pop     eax                {Default Result}
  jmp     @@Done
@@CheckDiff:
  pop     eax                {Default Result}
@@DiffLoop:
  cmp     cl, bl
  jne     @@SetResult
  add     ebp, 1
  jz      @@Done             {Difference after Compare Length}
  shr     ecx, 8
  shr     ebx, 8
  jmp     @@DiffLoop
@@SetResult:
  movzx   eax, bl            {Set Result from Character Difference}
  and     ecx, $ff
  sub     eax, ecx
@@Done:
  pop     esi
  pop     edi
  pop     ebp
  pop     ebx
end;

{***************************************************************}
function ALCompareText_Sha_IA32_3(const S1, S2: string): Integer;
asm
         test  eax, eax
         jz    @nil1
         test  edx, edx
         jnz   @ptrok

@nil2:   mov   eax, [eax-4]
         ret
@nil1:   test  edx, edx
         jz    @nil0
         sub   eax, [edx-4]
@nil0:   ret

@ptrok:  push  edi
         push  ebx
         xor   edi, edi
         mov   ebx, [eax-4]
         mov   ecx, ebx
         sub   ebx, [edx-4]
         adc   edi, -1
         push  ebx
         and   ebx, edi
         mov   edi, eax
         sub   ebx, ecx
         jge   @len

@lenok:  sub   edi, ebx
         sub   edx, ebx

@loop:   mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         cmp   eax, ecx
         jne   @byte0
@same:   add   ebx, 4
         jl    @loop

@len:    pop   eax
         pop   ebx
         pop   edi
         ret

@loop2:  mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         cmp   eax, ecx
         je    @same

@byte0:  cmp   al, cl
         je    @byte1

         and   eax, $FF
         and   ecx, $FF
         sub   eax, 'a'
         sub   ecx, 'a'
         cmp   al, 'z'-'a'
         ja    @up0a
         sub   eax, 'a'-'A'
@up0a:   cmp   cl, 'z'-'a'
         ja    @up0c
         sub   ecx, 'a'-'A'
@up0c:   sub   eax, ecx
         jnz   @done

         mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]

@byte1:  cmp   ah, ch
         je    @byte2

         and   eax, $FF00
         and   ecx, $FF00
         sub   eax, 'a'*256
         sub   ecx, 'a'*256
         cmp   ah, 'z'-'a'
         ja    @up1a
         sub   eax, ('a'-'A')*256
@up1a:   cmp   ch, 'z'-'a'
         ja    @up1c
         sub   ecx, ('a'-'A')*256
@up1c:   sub   eax, ecx
         jnz   @done

         mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]

@byte2:  add   ebx, 2
         jnl   @len2
         shr   eax, 16
         shr   ecx, 16
         cmp   al, cl
         je    @byte3

         and   eax, $FF
         and   ecx, $FF
         sub   eax, 'a'
         sub   ecx, 'a'
         cmp   al, 'z'-'a'
         ja    @up2a
         sub   eax, 'a'-'A'
@up2a:   cmp   cl, 'z'-'a'
         ja    @up2c
         sub   ecx, 'a'-'A'
@up2c:   sub   eax, ecx
         jnz   @done

         movzx eax, word ptr [ebx+edi]
         movzx ecx, word ptr [ebx+edx]

@byte3:  cmp   ah, ch
         je    @byte4

         and   eax, $FF00
         and   ecx, $FF00
         sub   eax, 'a'*256
         sub   ecx, 'a'*256
         cmp   ah, 'z'-'a'
         ja    @up3a
         sub   eax, ('a'-'A')*256
@up3a:   cmp   ch, 'z'-'a'
         ja    @up3c
         sub   ecx, ('a'-'A')*256
@up3c:   sub   eax, ecx
         jnz   @done

@byte4:  add   ebx, 2
         jl    @loop2
@len2:   pop   eax
         pop   ebx
         pop   edi
         ret

@done:   pop   ecx
         pop   ebx
         pop   edi
end;

{***************************************************************}
function ALCompareText_Sha_IA32_4(const S1, S2: string): integer;
asm
         test  eax, eax
         jz    @nil1
         test  edx, edx
         jnz   @ptrok

@nil2:   mov   eax, [eax-4]
         ret
@nil1:   test  edx, edx
         jz    @nil0
         sub   eax, [edx-4]
@nil0:   ret

@ptrok:  push  edi
         push  ebx
         xor   edi, edi
         mov   ebx, [eax-4]
         mov   ecx, ebx
         sub   ebx, [edx-4]
         adc   edi, -1
         push  ebx
         and   ebx, edi
         mov   edi, eax
         sub   ebx, ecx        //ebx := -min(Length(s1),Length(s2))
         jge   @len

@lenok:  sub   edi, ebx
         sub   edx, ebx

@loop:   mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         xor   eax, ecx
         jne   @differ
@same:   add   ebx, 4
         jl    @loop

@len:    pop   eax
         pop   ebx
         pop   edi
         ret

@loop2:  mov   eax, [ebx+edi]
         mov   ecx, [ebx+edx]
         xor   eax, ecx
         je    @same
@differ: test  eax, $DFDFDFDF  //$00 or $20
         jnz   @find
         add   eax, eax        //$00 or $40
         add   eax, eax        //$00 or $80
         test  eax, ecx
         jnz   @find
         and   ecx, $5F5F5F5F  //$41..$5A
         add   ecx, $3F3F3F3F  //$80..$99
         and   ecx, $7F7F7F7F  //$00..$19
         add   ecx, $66666666  //$66..$7F
         test  ecx, eax
         jnz   @find
         add   ebx, 4
         jl    @loop2

@len2:   pop   eax
         pop   ebx
         pop   edi
         ret

@loop3:  add   ebx, 1
         jge   @len2
@find:   movzx eax, [ebx+edi]
         movzx ecx, [ebx+edx]
         sub   eax, 'a'
         sub   ecx, 'a'
         cmp   al, 'z'-'a'
         ja    @upa
         sub   eax, 'a'-'A'
@upa:    cmp   cl, 'z'-'a'
         ja    @upc
         sub   ecx, 'a'-'A'
@upc:    sub   eax, ecx
         jz    @loop3

@found:  pop   ecx
         pop   ebx
         pop   edi
end;

{**************************************************************}
function ALCompareText_Sha_Pas_5(const S1, S2: string): integer;
var
  c1, c2, d, q, save: integer;
  p: pIntegerArray;
label
  past, find;
begin;
  d:=integer(@pchar(pointer(s1))[-4]);
  c1:=0;
  c2:=0;
  p:=@pchar(pointer(s2))[-4];
  if d<>-4 then c1:=pinteger(d)^;          //c1 = length of s1
  if p<>pointer(-4) then c2:=pinteger(p)^; //c2 = length of s2
  d:=(d-integer(p)) shr 2;                 //d = distance(s1-s2) div 4
  q:=c1;
  c1:=c1-c2;
  if c1>0 then q:=c2;                      //q = min length
  save:=c1;                    //save result for equal data
  if q<=0 then begin;
    Result:=c1;
    exit;
    end;
  q:=q+integer(p);

  repeat;
    c1:=p[d+1];                            //dword from s1
    c2:=p[1];                              //dword from s2
    inc(integer(p),4);
    c1:=c1 xor c2;
    if c1<>0 then begin;                   //test the difference
      //all bits of each byte must be 0, except bit5 (weight $20)
      if (c1 and integer($DFDFDFDF))<>0 then goto find;

      //bit5 can be 1 for letters only
      c1:=c1 + c1;                         //$00 or $40
      c1:=c1 + c1;                         //$00 or $80
      if (c1 and c2)<>0 then goto find;    //if not letter
      c2:=c2 and $5F5F5F5F;                //$41..$5A
      c2:=c2   + $3F3F3F3F;                //$80..$99
      c2:=c2 and $7F7F7F7F;                //$00..$19
      c2:=c2   + $66666666;                //$66..$7F
      if (c1 and c2)<>0 then goto find;    //if not letter
      end;
    until cardinal(p)>=cardinal(q);
past:
  Result:=save;
  exit;

  repeat; //find mismatched characters
    if cardinal(p)>=cardinal(q+4) then goto past;
find:
    c1:=byte(p[d]);
    c2:=byte(p[0]);
    inc(integer(p));
    c1:=c1-ord('a');
    c2:=c2-ord('a');
    if cardinal(c1)<=ord('z')-ord('a') then c1:=c1-(ord('a')-ord('A'));
    if cardinal(c2)<=ord('z')-ord('a') then c2:=c2-(ord('a')-ord('A'));
    until c1<>c2;
  Result:=c1-c2;
end;

{**********************************}
{Called Once by Unit Initialisation}
procedure ALInitCompareTextFunct;
begin
  case AlGetCpuTarget of
    fctP4R: ALCompareText := ALCompareText_JOH_IA32_6;
    fctP4N: ALCompareText := ALCompareText_JOH_IA32_6;
    fctPMY: ALCompareText := ALCompareText_Sha_IA32_3;
    fctPMD: ALCompareText := ALCompareText_Sha_IA32_3;
    fctAMD64: ALCompareText := ALCompareText_JOH_IA32_5;
    fctAMD64_SSE3: ALCompareText := ALCompareText_JOH_IA32_5;
    fctIA32SizePenalty: ALCompareText := ALCompareText_Sha_IA32_4;
    fctIA32: ALCompareText := ALCompareText_JOH_IA32_5;
    fctMMX: ALCompareText := ALCompareText_JOH_IA32_5;
    fctSSESizePenalty: ALCompareText := ALCompareText_Sha_IA32_4;
    fctSSE: ALCompareText := ALCompareText_JOH_IA32_5;
    fctSSE2: ALCompareText := ALCompareText_JOH_IA32_5;
    fctPascalSizePenalty: ALCompareText := ALCompareText_Sha_Pas_5;
    fctPascal: ALCompareText := ALCompareText_Sha_Pas_5;
  end;
end;




////////////////////////////////////////////////////////////////
////////////////////////ALLowerCase/////////////////////////////
////////////////////////////////////////////////////////////////

var
  vALAsciiLowerCase : array[Char] of Char = (
    #$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$0F,
    #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
    #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
    #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
    #$40,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
    #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$5B,#$5C,#$5D,#$5E,#$5F,
    #$60,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6A,#$6B,#$6C,#$6D,#$6E,#$6F,
    #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7A,#$7B,#$7C,#$7D,#$7E,#$7F,
    #$80,#$81,#$82,#$83,#$84,#$85,#$86,#$87,#$88,#$89,#$8A,#$8B,#$8C,#$8D,#$8E,#$8F,
    #$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$9A,#$9B,#$9C,#$9D,#$9E,#$9F,
    #$A0,#$A1,#$A2,#$A3,#$A4,#$A5,#$A6,#$A7,#$A8,#$A9,#$AA,#$AB,#$AC,#$AD,#$AE,#$AF,
    #$B0,#$B1,#$B2,#$B3,#$B4,#$B5,#$B6,#$B7,#$B8,#$B9,#$BA,#$BB,#$BC,#$BD,#$BE,#$BF,
    #$C0,#$C1,#$C2,#$C3,#$C4,#$C5,#$C6,#$C7,#$C8,#$C9,#$CA,#$CB,#$CC,#$CD,#$CE,#$CF,
    #$D0,#$D1,#$D2,#$D3,#$D4,#$D5,#$D6,#$D7,#$D8,#$D9,#$DA,#$DB,#$DC,#$DD,#$DE,#$DF,
    #$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF,
    #$F0,#$F1,#$F2,#$F3,#$F4,#$F5,#$F6,#$F7,#$F8,#$F9,#$FA,#$FB,#$FC,#$FD,#$FE,#$FF);

{******************************************************}
function ALLowerCase_JOH_MMX_2(const S: string): string;
const
  B25 : Int64 = $2525252525252525;
  B65 : Int64 = $6565656565656565;
  B20 : Int64 = $2020202020202020;
asm
  xchg    eax, edx
  test    edx, edx              {Test for S = ''}
  jz      system.@LStrSetLength {Return Empty String}
  mov     ecx, edx              {Addr(S)}
  mov     edx, [edx-4]
  push    ebx
  push    edi
  push    esi
  mov     edi, ecx              {Addr(S)}
  mov     esi, edx              {Length}
  mov     ebx, eax              {Addr(Result)}
  call    system.@LStrSetLength {Create Result String}
  mov     ecx, esi              {Length}
  mov     eax, edi              {Addr(S)}
  sub     ecx, 16
  mov     edx, [ebx]            {Result}
  jl      @@Small
  movq    mm4, B25
  movq    mm5, B65
  movq    mm6, B20
  add     eax, ecx
  add     edx, ecx
  neg     ecx
@@LargeLoop:
  movq    mm0, [eax+ecx  ]
  movq    mm1, [eax+ecx+8]
  movq    mm2, mm0
  movq    mm3, mm1
  paddb   mm2, mm4
  paddb   mm3, mm4
  pcmpgtb mm2, mm5
  pcmpgtb mm3, mm5
  pand    mm2, mm6
  pand    mm3, mm6
  paddb   mm0, mm2
  paddb   mm1, mm3
  movq    [edx+ecx  ], mm0
  movq    [edx+ecx+8], mm1
  add     ecx, 16
  jle     @@LargeLoop
  emms
  neg     ecx
  sub     eax, ecx
  sub     edx, ecx
@@Small:
  add     ecx, 16
  lea     edi, vALAsciiLowerCase
  jz      @@Done
@@SmallLoop:
  sub     ecx, 1
  movzx   esi, [eax+ecx]
  movzx   ebx, [edi+esi]
  mov     [edx+ecx], bl
  jg      @@SmallLoop
@@Done:
  pop     esi
  pop     edi
  pop     ebx
end;

{*******************************************************}
function ALLowerCase_JOH_IA32_5(const S: string): string;
asm {Size = 134 Bytes}
  push    ebx
  push    edi
  push    esi
  test    eax, eax               {Test for S = NIL}
  mov     esi, eax               {@S}
  mov     edi, edx               {@Result}
  mov     eax, edx               {@Result}
  jz      @@Null                 {S = NIL}
  mov     edx, [esi-4]           {Length(S)}
  test    edx, edx
  je      @@Null                 {Length(S) = 0}
  mov     ebx, edx
  call    system.@LStrSetLength  {Create Result String}
  mov     edi, [edi]             {@Result}
  mov     eax, [esi+ebx-4]       {Convert the Last 4 Characters of String}
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $5B5B5B5B         {Set High Bit if Original <= Ord('Z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('A')}
  and     eax, edx               {80h if Orig in 'A'..'Z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  add     ecx, eax               {Set Bit 5 if Original in 'A'..'Z'}
  mov     [edi+ebx-4], ecx
  sub     ebx, 1
  and     ebx, -4
  jmp     @@CheckDone
@@Null:
  pop     esi
  pop     edi
  pop     ebx
  jmp     System.@LStrClr
@@Loop:                          {Loop converting 4 Character per Loop}
  mov     eax, [esi+ebx]
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $5B5B5B5B         {Set High Bit if Original <= Ord('Z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('A')}
  and     eax, edx               {80h if Orig in 'A'..'Z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  add     ecx, eax               {Set Bit 5 if Original in 'A'..'Z'}
  mov     [edi+ebx], ecx
@@CheckDone:
  sub     ebx, 4
  jnc     @@Loop
  pop     esi
  pop     edi
  pop     ebx
end;

{******************************************************}
function ALLowerCase_Sha_Pas_2(const s: string): string;
var
  ch1, ch2, ch3, dist, term: integer;
  p: pchar;
label
  loop, last;
begin
  if s='' then begin;
    Result:=''; exit;
    end;

  p:=pointer(s);

  //If need pure Pascal change the next line to term:=Length(s);
  term:=pinteger(@p[-4])^;

  SetLength(Result,term);

  if term<>0 then begin;
    dist:=integer(Result);
    term:=integer(p+term);
    dist:=dist-integer(p)-4;

loop:
    ch1:=pinteger(p)^;
    ch3:=$7F7F7F7F;

    ch2:=ch1;
    ch3:=ch3 and ch1;

    ch2:=ch2 xor (-1);
    ch3:=ch3 + $25252525;

    ch2:=ch2 and $80808080;
    ch3:=ch3 and $7F7F7F7F;

    ch3:=ch3 + $1A1A1A1A;
    inc(p,4);

    ch3:=ch3 and ch2;
    if cardinal(p)>=cardinal(term) then goto last;

    ch3:=ch3 shr 2;
    ch1:=ch1 + ch3;
    pinteger(p+dist)^:=ch1;
    goto loop;

last:
    ch3:=ch3 shr 2;
    term:=term-integer(p);
    p:=p+dist;
    ch1:=ch1 + ch3;
    if term<-1
      then pword(p)^:=word(ch1)
      else pinteger(p)^:=ch1;
    end;
end;

{**********************************}
{Called Once by Unit Initialisation}
procedure ALInitLowerCaseFunct;
begin
  case AlGetCpuTarget of
    fctP4R: ALLowerCase := ALLowerCase_JOH_MMX_2;
    fctP4N: ALLowerCase := ALLowerCase_JOH_MMX_2;
    fctPMY: ALLowerCase := ALLowerCase_JOH_MMX_2;
    fctPMD: ALLowerCase := ALLowerCase_JOH_MMX_2;
    fctAMD64: ALLowerCase := ALLowerCase_JOH_IA32_5;
    fctAMD64_SSE3: ALLowerCase := ALLowerCase_JOH_IA32_5;
    fctIA32SizePenalty: ALLowerCase := ALLowerCase_JOH_IA32_5;
    fctIA32: ALLowerCase := ALLowerCase_JOH_IA32_5;
    fctMMX: ALLowerCase := ALLowerCase_JOH_MMX_2;
    fctSSESizePenalty: ALLowerCase := ALLowerCase_JOH_IA32_5;
    fctSSE: ALLowerCase := ALLowerCase_JOH_MMX_2;
    fctSSE2: ALLowerCase := ALLowerCase_JOH_MMX_2;
    fctPascalSizePenalty: ALLowerCase := ALLowerCase_Sha_Pas_2;
    fctPascal: ALLowerCase := ALLowerCase_Sha_Pas_2;
  end;
end;




////////////////////////////////////////////////////////////////
//////////////////////////ALUpperCase///////////////////////////
////////////////////////////////////////////////////////////////

var
  vALUpperCaseLookUpTable : array of Char;

{*******************************************************}
function ALUpperCase_JOH_SSE2_2(const S: string): string;
const
  B05 : array[1..2] of Int64 = ($0505050505050505, $0505050505050505);
  B65 : array[1..2] of Int64 = ($6565656565656565, $6565656565656565);
  B20 : array[1..2] of Int64 = ($2020202020202020, $2020202020202020);
asm
  push    ebx
  push    edi
  push    esi
  test    eax, eax               {Test for S = NIL}
  mov     esi, eax               {@S}
  mov     edi, edx               {@Result}
  mov     eax, edx               {@Result}
  jz      @@Null                 {S = NIL}
  mov     edx, [esi-4]           {Length(S)}
  test    edx, edx
  je      @@Null                 {Length(S) = 0}
  mov     ebx, edx
  call    system.@LStrSetLength  {Create Result String}
  mov     edi, [edi]             {@Result}
  cmp     ebx, 32
  jg      @@Large
@@Small:
  mov     eax, [esi+ebx-4]       {Convert the Last 4 Characters of String}
  mov     ecx, $7f7f7f7f         {Note - For Strings of Length 1, 2 or 3,
this}
  mov     edx, eax               {will Read/Write the last 1, 2 or 3 bytes
of }
  not     edx                    {the Length Preaamble, but since these 
bytes }
  and     ecx, eax               {will always contain 0 in these cases, 
hey  }
  and     edx, $80808080         {will never be modified}
  add     ecx, $05050505
  and     ecx, $7f7f7f7f
  add     ecx, $1a1a1a1a
  and     ecx, edx
  shr     ecx, 2
  xor     eax, ecx
  mov     [edi+ebx-4], eax
  sub     ebx, 1
  and     ebx, -4
  jz      @@Done                 {Exit if Length <= 4}
  add     esi, ebx
  add     edi, ebx
  neg     ebx
@@SmallLoop:                     {Loop converting 4 Character per Loop}
  mov     eax, [esi+ebx]
  mov     ecx, $7f7f7f7f
  mov     edx, eax
  not     edx
  and     ecx, eax
  and     edx, $80808080
  add     ecx, $05050505
  and     ecx, $7f7f7f7f
  add     ecx, $1a1a1a1a
  and     ecx, edx
  shr     ecx, 2
  xor     eax, ecx
  mov     [edi+ebx], eax
  add     ebx, 4
  jnz     @@SmallLoop
@@Done:
  pop     esi
  pop     edi
  pop     ebx
  ret
@@Null:
  pop     esi
  pop     edi
  pop     ebx
  jmp     System.@LStrClr
@@Large:
  movdqu  xmm2, B05
  movdqu  xmm3, B65
  movdqu  xmm4, B20
  movdqu  xmm0, [esi]            {Translate First 16 Chars}
  movdqu  xmm1, xmm0
  paddb   xmm1, xmm2
  pcmpgtb xmm1, xmm3
  pand    xmm1, xmm4
  psubb   xmm0, xmm1
  movdqu  [edi], xmm0
  sub     ebx, 16
  movdqu  xmm0, [esi+ebx]        {Translate Last 16 Chars}
  movdqu  xmm1, xmm0
  paddb   xmm1, xmm2
  pcmpgtb xmm1, xmm3
  pand    xmm1, xmm4
  psubb   xmm0, xmm1
  movdqu  [edi+ebx], xmm0
  mov     ecx, edi               {Align Writes}
  add     esi, ebx
  add     ebx, edi
  and     edi, -16
  sub     ebx, edi
  add     edi, ebx
  sub     ebx, 16
  neg     ebx
@@LargeLoop:
  movdqu  xmm0, [esi+ebx]        {Translate Next 16 Chars}
  movdqa  xmm1, xmm0
  paddb   xmm1, xmm2
  pcmpgtb xmm1, xmm3
  pand    xmm1, xmm4
  psubb   xmm0, xmm1
  movdqa  [edi+ebx], xmm0
  add     ebx, 16
  jl      @@LargeLoop
  pop     esi
  pop     edi
  pop     ebx
end;

{******************************************************}
function ALUpperCase_JOH_SSE_2(const S: string): string;
const
  B05 : Int64 = $0505050505050505;
  B65 : Int64 = $6565656565656565;
  B20 : Int64 = $2020202020202020;
asm
  push    ebx
  push    edi
  push    esi
  test    eax, eax               {Test for S = NIL}
  mov     esi, eax               {@S}
  mov     edi, edx               {@Result}
  mov     eax, edx               {@Result}
  jz      @@Null                 {S = NIL}
  mov     edx, [esi-4]           {Length(S)}
  test    edx, edx
  je      @@Null                 {Length(S) = 0}
  mov     ebx, edx
  call    system.@LStrSetLength  {Create Result String}
  mov     edi, [edi]             {@Result}
  cmp     ebx, 32
  jg      @@Large
@@Small:
  mov     eax, [esi+ebx-4]       {Convert the Last 4 Characters of String}
  mov     ecx, $7f7f7f7f         {Note - For Strings of Length 1, 2 or 3,
this}
  mov     edx, eax               {will Read/Write the last 1, 2 or 3 bytes
of }
  not     edx                    {the Length Preaamble, but since these 
bytes }
  and     ecx, eax               {will always contain 0 in these cases,
hey  }
  and     edx, $80808080         {will never be modified}
  add     ecx, $05050505
  and     ecx, $7f7f7f7f
  add     ecx, $1a1a1a1a
  and     ecx, edx
  shr     ecx, 2
  xor     eax, ecx
  mov     [edi+ebx-4], eax
  sub     ebx, 1
  and     ebx, -4
  jz      @@Done                 {Exit if Length <= 4}
  add     esi, ebx
  add     edi, ebx
  neg     ebx
@@SmallLoop:                     {Loop converting 4 Character per Loop}
  mov     eax, [esi+ebx]
  mov     ecx, $7f7f7f7f
  mov     edx, eax
  not     edx
  and     ecx, eax
  and     edx, $80808080
  add     ecx, $05050505
  and     ecx, $7f7f7f7f
  add     ecx, $1a1a1a1a
  and     ecx, edx
  shr     ecx, 2
  xor     eax, ecx
  mov     [edi+ebx], eax
  add     ebx, 4
  jnz     @@SmallLoop
@@Done:
  pop     esi
  pop     edi
  pop     ebx
  ret
@@Null:
  pop     esi
  pop     edi
  pop     ebx
  jmp     System.@LStrClr
@@Large:
  movq    mm4, B05
  movq    mm5, B65
  movq    mm6, B20
  movq    mm0, [esi  ]           {Translate First 16 Chars}
  movq    mm1, [esi+8]
  pshufw  mm2, mm0, $E4          {Faster Version of movq mm2, mm0}
  pshufw  mm3, mm1, $E4          {Faster Version of movq mm3, mm1}
  paddb   mm2, mm4
  paddb   mm3, mm4
  pcmpgtb mm2, mm5
  pcmpgtb mm3, mm5
  pand    mm2, mm6
  pand    mm3, mm6
  psubb   mm0, mm2
  psubb   mm1, mm3
  movq    [edi  ], mm0
  movq    [edi+8], mm1
  sub     ebx, 16
  movq    mm0, [esi+ebx ]        {Translate Last 16 Chars}
  movq    mm1, [esi+ebx+8]
  pshufw  mm2, mm0, $E4          {Faster Version of movq mm2, mm0}
  pshufw  mm3, mm1, $E4          {Faster Version of movq mm3, mm1}
  paddb   mm2, mm4
  paddb   mm3, mm4
  pcmpgtb mm2, mm5
  pcmpgtb mm3, mm5
  pand    mm2, mm6
  pand    mm3, mm6
  psubb   mm0, mm2
  psubb   mm1, mm3
  movq    [edi+ebx  ], mm0
  movq    [edi+ebx+8], mm1
  mov     ecx, edi               {Align Writes}
  add     esi, ebx
  add     ebx, edi
  and     edi, -16
  sub     ebx, edi
  add     edi, ebx
  sub     ebx, 16
  neg     ebx
@@LargeLoop:
  movq    mm0, [esi+ebx  ]       {Translate Next 16 Chars}
  movq    mm1, [esi+ebx+8]
  pshufw  mm2, mm0, $E4          {Faster Version of movq mm2, mm0}
  pshufw  mm3, mm1, $E4          {Faster Version of movq mm3, mm1}
  paddb   mm2, mm4
  paddb   mm3, mm4
  pcmpgtb mm2, mm5
  pcmpgtb mm3, mm5
  pand    mm2, mm6
  pand    mm3, mm6
  psubb   mm0, mm2
  psubb   mm1, mm3
  movq    [edi+ebx  ], mm0
  movq    [edi+ebx+8], mm1
  add     ebx, 16
  jl      @@LargeLoop
  emms
  pop     esi
  pop     edi
  pop     ebx
end;

{******************************************************}
function ALUpperCase_JOH_MMX_3(const S: string): string;
const
  B05 : Int64 = $0505050505050505;
  B65 : Int64 = $6565656565656565;
  B20 : Int64 = $2020202020202020;
asm
  push    ebx
  push    edi
  push    esi
  test    eax, eax               {Test for S = NIL}
  mov     esi, eax               {@S}
  mov     edi, edx               {@Result}
  mov     eax, edx               {@Result}
  jz      @@Null                 {S = NIL}
  mov     edx, [esi-4]           {Length(S)}
  test    edx, edx
  je      @@Null                 {Length(S) = 0}
  mov     ebx, edx
  call    system.@LStrSetLength  {Create Result String}
  mov     edi, [edi]             {@Result}
  cmp     ebx, 32
  jg      @@Large
@@Small:
  mov     eax, [esi+ebx-4]       {Convert the Last 4 Characters of String}
  mov     ecx, $7f7f7f7f         {Note - For Strings of Length 1, 2 or 3, 
this}
  mov     edx, eax               {will Read/Write the last 1, 2 or 3 bytes
of }
  not     edx                    {the Length Preaamble, but since these 
bytes }
  and     ecx, eax               {will always contain 0 in these cases, 
hey  }
  and     edx, $80808080         {will never be modified}
  add     ecx, $05050505
  and     ecx, $7f7f7f7f
  add     ecx, $1a1a1a1a
  and     ecx, edx
  shr     ecx, 2
  xor     eax, ecx
  mov     [edi+ebx-4], eax
  sub     ebx, 1
  and     ebx, -4
  jz      @@Done                 {Exit if Length <= 4}
  add     esi, ebx
  add     edi, ebx
  neg     ebx
@@SmallLoop:                     {Loop converting 4 Character per Loop}
  mov     eax, [esi+ebx]
  mov     ecx, $7f7f7f7f
  mov     edx, eax
  not     edx
  and     ecx, eax
  and     edx, $80808080
  add     ecx, $05050505
  and     ecx, $7f7f7f7f
  add     ecx, $1a1a1a1a
  and     ecx, edx
  shr     ecx, 2
  xor     eax, ecx
  mov     [edi+ebx], eax
  add     ebx, 4
  jnz     @@SmallLoop
@@Done:
  pop     esi
  pop     edi
  pop     ebx
  ret
@@Null:
  pop     esi
  pop     edi
  pop     ebx
  jmp     System.@LStrClr
@@Large:
  movq    mm4, B05
  movq    mm5, B65
  movq    mm6, B20
  movq    mm0, [esi  ]           {Translate First 16 Chars}
  movq    mm1, [esi+8]
  movq    mm2, mm0
  movq    mm3, mm1
  paddb   mm2, mm4
  paddb   mm3, mm4
  pcmpgtb mm2, mm5
  pcmpgtb mm3, mm5
  pand    mm2, mm6
  pand    mm3, mm6
  psubb   mm0, mm2
  psubb   mm1, mm3
  movq    [edi  ], mm0
  movq    [edi+8], mm1
  sub     ebx, 16
  movq    mm0, [esi+ebx ]        {Translate Last 16 Chars}
  movq    mm1, [esi+ebx+8]
  movq    mm2, mm0
  movq    mm3, mm1
  paddb   mm2, mm4
  paddb   mm3, mm4
  pcmpgtb mm2, mm5
  pcmpgtb mm3, mm5
  pand    mm2, mm6
  pand    mm3, mm6
  psubb   mm0, mm2
  psubb   mm1, mm3
  movq    [edi+ebx  ], mm0
  movq    [edi+ebx+8], mm1
  mov     ecx, edi               {Align Writes}
  add     esi, ebx
  add     ebx, edi
  and     edi, -16
  sub     ebx, edi
  add     edi, ebx
  sub     ebx, 16
  neg     ebx
@@LargeLoop:
  movq    mm0, [esi+ebx  ]       {Translate Next 16 Chars}
  movq    mm1, [esi+ebx+8]
  movq    mm2, mm0
  movq    mm3, mm1
  paddb   mm2, mm4
  paddb   mm3, mm4
  pcmpgtb mm2, mm5
  pcmpgtb mm3, mm5
  pand    mm2, mm6
  pand    mm3, mm6
  psubb   mm0, mm2
  psubb   mm1, mm3
  movq    [edi+ebx  ], mm0
  movq    [edi+ebx+8], mm1
  add     ebx, 16
  jl      @@LargeLoop
  emms
  pop     esi
  pop     edi
  pop     ebx
end;

{*******************************************************}
function ALUpperCase_JOH_IA32_5(const S: string): string;
asm {Size = 134 Bytes}
  push    ebx
  push    edi
  push    esi
  test    eax, eax               {Test for S = NIL}
  mov     esi, eax               {@S}
  mov     edi, edx               {@Result}
  mov     eax, edx               {@Result}
  jz      @@Null                 {S = NIL}
  mov     edx, [esi-4]           {Length(S)}
  test    edx, edx
  je      @@Null                 {Length(S) = 0}
  mov     ebx, edx
  call    system.@LStrSetLength  {Create Result String}
  mov     edi, [edi]             {@Result}
  mov     eax, [esi+ebx-4]       {Convert the Last 4 Characters of String}
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $7B7B7B7B         {Set High Bit if Original <= Ord('z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('a')}
  and     eax, edx               {80h if Orig in 'a'..'z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  sub     ecx, eax               {Clear Bit 5 if Original in 'a'..'z'}
  mov     [edi+ebx-4], ecx
  sub     ebx, 1
  and     ebx, -4
  jmp     @@CheckDone
@@Null:
  pop     esi
  pop     edi
  pop     ebx
  jmp     System.@LStrClr
@@Loop:                          {Loop converting 4 Character per Loop}
  mov     eax, [esi+ebx]
  mov     ecx, eax               {4 Original Bytes}
  or      eax, $80808080         {Set High Bit of each Byte}
  mov     edx, eax               {Comments Below apply to each Byte...}
  sub     eax, $7B7B7B7B         {Set High Bit if Original <= Ord('z')}
  xor     edx, ecx               {80h if Original < 128 else 00h}
  or      eax, $80808080         {Set High Bit}
  sub     eax, $66666666         {Set High Bit if Original >= Ord('a')}
  and     eax, edx               {80h if Orig in 'a'..'z' else 00h}
  shr     eax, 2                 {80h > 20h ('a'-'A')}
  sub     ecx, eax               {Clear Bit 5 if Original in 'a'..'z'}
  mov     [edi+ebx], ecx
@@CheckDone:
  sub     ebx, 4
  jnc     @@Loop
  pop     esi
  pop     edi
  pop     ebx
end;

{*****************************************}
procedure ALUpperCaseInitializeLookUpTable;
var
 I : Byte;
 S1, S2 : AnsiString;

begin
 SetLength(vALUpperCaseLookUpTable, 256);
 for I := 0 to 255 do
  begin
   S1 := Char(I);
   S2 := UpperCase(S1);
   vALUpperCaseLookUpTable[I] := S2[1];
  end;
end;

{*******************************************************}
function ALUpperCase_DKC_Pas_32(const S: string): string;
var
 Max, CharNo : Cardinal;
 pResult : PChar;

begin
 Max := Length(S);
 SetLength(Result, Max);
 if Max > 0 then
  begin
   pResult := PChar(Result);
   CharNo := 0;
   repeat
    pResult[CharNo] := vALUpperCaseLookUpTable[Ord(S[CharNo+1])];
    Inc(CharNo);
    if CharNo >= Max then
     Break;
    pResult[CharNo] := vALUpperCaseLookUpTable[Ord(S[CharNo+1])];
    Inc(CharNo);
    if CharNo >= Max then
     Break;
    pResult[CharNo] := vALUpperCaseLookUpTable[Ord(S[CharNo+1])];
    Inc(CharNo);
    if CharNo >= Max then
     Break;
    pResult[CharNo] := vALUpperCaseLookUpTable[Ord(S[CharNo+1])];
    Inc(CharNo);
   until(CharNo >= Max);
  end;
end;

{**********************************}
{Called Once by Unit Initialisation}
procedure ALInitUpperCaseFunct;
begin
  ALUpperCaseInitializeLookUpTable;
  case AlGetCpuTarget of
    fctP4R: ALUpperCase := ALUpperCase_JOH_SSE2_2;
    fctP4N: ALUpperCase := ALUpperCase_JOH_SSE_2;
    fctPMY: ALUpperCase := ALUpperCase_JOH_MMX_3;
    fctPMD: ALUpperCase := ALUpperCase_JOH_MMX_3;
    fctAMD64: ALUpperCase := ALUpperCase_JOH_MMX_3;
    fctAMD64_SSE3: ALUpperCase := ALUpperCase_JOH_SSE_2;
    fctIA32SizePenalty: ALUpperCase := ALUpperCase_JOH_IA32_5;
    fctIA32: ALUpperCase := SysUtils.UpperCase;
    fctMMX: ALUpperCase := ALUpperCase_JOH_MMX_3;
    fctSSESizePenalty: ALUpperCase := ALUpperCase_JOH_IA32_5;
    fctSSE: ALUpperCase := ALUpperCase_JOH_SSE_2;
    fctSSE2: ALUpperCase := ALUpperCase_JOH_SSE_2;
    fctPascalSizePenalty: ALUpperCase := ALUpperCase_DKC_Pas_32;
    fctPascal: ALUpperCase := ALUpperCase_DKC_Pas_32;
  end;
end;



///////////////////////////
//////////Alcinoe//////////
///////////////////////////

{********************************************************************************}
function ALCopyStr(const aSourceString: string; aStart, aLength: Integer): string;
var SourceStringLength: Integer;
begin
  SourceStringLength := Length(aSourceString);
  If (aStart < 1) then aStart := 1;

  if (SourceStringLength=0) or
     (aLength < 1) or
     (aStart > SourceStringLength) then Begin
    Result := '';
    Exit;
  end;

  if aLength > SourceStringLength - (aStart - 1) then aLength := SourceStringLength - (aStart-1);

  SetLength(Result,aLength);
  ALMove(aSourceString[aStart], Result[1], aLength);
end;

{*********************************************}
function ALRandomStr(aLength: Longint): string;
var X: Longint;
begin
  if aLength <= 0 then exit;
  SetLength(Result, aLength);
  for X:=1 to aLength do Result[X] := Chr(Random(26) + 65);
end;

{*************************************************}
function ALNEVExtractName(const S: string): string;
var P: Integer;
begin
  Result := S;
  P := alCharPos('=', Result);
  if P <> 0 then SetLength(Result, P-1)
  else SetLength(Result, 0);
end;

{**************************************************}
function ALNEVExtractValue(const s: string): string;
begin
  Result := AlCopyStr(s, Length(ALNEVExtractName(s)) + 2, MaxInt)
end;

{*********************************************************************}
function ALFastTagReplace(Const SourceString, TagStart, TagEnd: string;
                          FastTagReplaceProc: TALHandleTagFunct;
                          ReplaceStrParamName,
                          ReplaceWith: String;
                          AStripParamQuotes: Boolean;
                          Flags: TReplaceFlags;
                          ExtData: Pointer): string;
var  i: integer;
     ReplaceString: String;
     Token, FirstTagEndChar: Char;
     TokenStr, ParamStr: string;
     ParamList: TStringList;
     TagStartLength: integer;
     TagEndLength: integer;
     SourceStringLength: Integer;
     T1,T2: Integer;
     InDoubleQuote: Boolean;
     InsingleQuote: Boolean;
     Work_SourceString: String;
     Work_TagStart: String;
     Work_TagEnd: String;
     TagHandled: Boolean;
     ResultCurrentPos: integer;
     ResultCurrentLength: integer;

Const ResultBuffSize: integer = 16384;

     {-------------------------------}
     Function ExtractTokenStr: String;
     var x: Integer;
     Begin
       x := AlCharPos(' ',ReplaceString);
       if x > 0 then Result := trim( AlcopyStr(ReplaceString,1,x) )
       else Result := trim(ReplaceString);
     end;

     {--------------------------------}
     Function ExtractParamsStr: String;
     Begin
       Result := trim( AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt) );
     end;

     {-----------------------------------}
     Procedure MoveStr2Result(Src:String);
     Var l: integer;
     Begin
       If Src <> '' then begin
         L := Length(Src);
         If L+ResultCurrentPos-1>ResultCurrentLength Then begin
           ResultCurrentLength := ResultCurrentLength + L + ResultBuffSize;
           SetLength(Result,ResultCurrentLength);
         end;
         AlMove(Src[1],Result[ResultCurrentPos],L);
         ResultCurrentPos := ResultCurrentPos + L;
       end;
     end;


begin
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then begin
    Result := SourceString;
    Exit;
  end;

  If rfIgnoreCase in flags then begin
    Work_SourceString := ALUppercase(SourceString);
    Work_TagStart := ALuppercase(TagStart);
    Work_TagEnd := ALUppercase(TagEnd);
  end
  Else begin
    Work_SourceString := SourceString;
    Work_TagStart := TagStart;
    Work_TagEnd := TagEnd;
  end;

  SourceStringLength := length(Work_SourceString);
  ResultCurrentLength := SourceStringLength;
  SetLength(Result,ResultCurrentLength);
  ResultCurrentPos := 1;
  TagStartLength := Length(Work_TagStart);
  TagEndLength := Length(Work_TagEnd);
  FirstTagEndChar := Work_TagEnd[1];
  i := 1;

  T1 := ALPosEx(Work_TagStart,Work_SourceString,i);
  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin
    InDoubleQuote := False;
    InsingleQuote := False;
    Token := Work_SourceString[T2];
    if token = '"' then InDoubleQuote := True
    else if token = '''' then InSingleQuote := True;
    While (T2 < SourceStringLength) and (InDoubleQuote or InSingleQuote or (Token <> FirstTagEndChar) or (ALPosEx(Work_TagEnd,Work_SourceString,T2) <> T2)) do begin
      inc(T2);
      Token := Work_SourceString[T2];
      If Token = '"' then InDoubleQuote := not InDoubleQuote and not InSingleQuote
      else If Token = '''' then InSingleQuote := not InSingleQuote and not InDoubleQuote
    end;
  end;


  While (T1 > 0) and (T2 > T1) do begin
    ReplaceString := AlCopyStr(SourceString,T1 + TagStartLength,T2 - T1 - TagStartLength);

    TagHandled := True;
    If assigned(FastTagReplaceProc) or (ReplaceStrParamName <> '') then begin
      TokenStr := ExtractTokenStr;
      ParamStr := ExtractParamsStr;
      ParamList := TStringList.Create;
      try
        ALExtractHeaderFields([' ', #9], [' ', #9], PChar(ParamStr), ParamList, False, AStripParamQuotes);
        If assigned(FastTagReplaceProc) then ReplaceString := FastTagReplaceProc(TokenStr, ParamList, ExtData, TagHandled)
        else ReplaceString := ParamList.Values[ReplaceStrParamName];
      finally
        ParamList.Free;
      end;
    end
    else ReplaceString := ReplaceWith;


    If tagHandled then MoveStr2Result(AlcopyStr(SourceString,i,T1 - i) + ReplaceString)
    else MoveStr2Result(AlcopyStr(SourceString,i,T2 + TagEndLength - i));
    i := T2 + TagEndLength;

    If TagHandled and (not (rfreplaceAll in flags)) then Break;

    T1 := ALPosEx(Work_TagStart,Work_SourceString,i);
    T2 := T1 + TagStartLength;
    If (T1 > 0) and (T2 <= SourceStringLength) then begin
      InDoubleQuote := False;
      InsingleQuote := False;
      Token := Work_SourceString[T2];
      if token = '"' then InDoubleQuote := True
      else if token = '''' then InSingleQuote := True;
      While (T2 < SourceStringLength) and (InDoubleQuote or InSingleQuote or (Token <> FirstTagEndChar) or (ALPosEx(Work_TagEnd,Work_SourceString,T2) <> T2)) do begin
        inc(T2);
        Token := Work_SourceString[T2];
        If Token = '"' then InDoubleQuote := not InDoubleQuote and not InSingleQuote
        else If Token = '''' then InSingleQuote := not InSingleQuote and not InDoubleQuote
      end;
    end;
  end;

  MoveStr2Result(AlcopyStr(SourceString,i,maxint));
  SetLength(Result,ResultCurrentPos-1);
end;

{*********************************************************************}
function ALFastTagReplace(const SourceString, TagStart, TagEnd: string;
                          ReplaceWith: string;
                          const Flags: TReplaceFlags=[rfreplaceall] ): string;
Begin
  Result := ALFastTagReplace(SourceString, TagStart, TagEnd, nil, '', ReplaceWith, True, flags, nil);
end;

{*********************************************************************}
function ALFastTagReplace(const SourceString, TagStart, TagEnd: string;
                          ReplaceStrParamName: string;
                          AStripParamQuotes: Boolean;
                          const Flags: TReplaceFlags=[rfreplaceall] ): string;
Begin
  Result := ALFastTagReplace(SourceString, TagStart, TagEnd, nil, ReplaceStrParamName, '', AStripParamQuotes, flags, nil);
end;

{*********************************************************************}
function ALFastTagReplace(const SourceString, TagStart, TagEnd: string;
                          FastTagReplaceProc: TALHandleTagFunct;
                          AStripParamQuotes: Boolean;
                          ExtData: Pointer;
                          Const flags: TReplaceFlags=[rfreplaceall]): string;
Begin
  result := ALFastTagReplace(SourceString, TagStart, TagEnd, FastTagReplaceProc, '', '', AStripParamQuotes, flags, extdata);
end;

{***********************************************************************}
function ALExtractTagParams(Const SourceString, TagStart, TagEnd: string;
                            AStripParamQuotes: Boolean;
                            TagParams: TStrings;
                            IgnoreCase: Boolean): Boolean;
var  ReplaceString: String;
     Token, FirstTagEndChar: Char;
     TokenStr, ParamStr: string;
     TagStartLength: integer;
     SourceStringLength: Integer;
     T1,T2: Integer;
     InDoubleQuote: Boolean;
     InsingleQuote: Boolean;
     Work_SourceString: String;
     Work_TagStart: String;
     Work_TagEnd: String;

     {-------------------------------}
     Function ExtractTokenStr: String;
     var x: Integer;
     Begin
       x := AlCharPos(' ',ReplaceString);
       if x > 0 then Result := trim( AlcopyStr(ReplaceString,1,x) )
       else Result := trim(ReplaceString);
     end;

     {--------------------------------}
     Function ExtractParamsStr: String;
     Begin
       Result := trim( AlcopyStr(ReplaceString,length(TokenStr) + 1, MaxInt) );
     end;

begin
  Result := False;
  if (SourceString = '') or (TagStart = '') or (TagEnd = '') then Exit;

  If IgnoreCase then begin
    Work_SourceString := ALUppercase(SourceString);
    Work_TagStart := ALuppercase(TagStart);
    Work_TagEnd := ALUppercase(TagEnd);
  end
  Else begin
    Work_SourceString := SourceString;
    Work_TagStart := TagStart;
    Work_TagEnd := TagEnd;
  end;

  TagStartLength := Length(Work_TagStart);
  SourceStringLength := length(SourceString);
  FirstTagEndChar := tagEnd[1];

  T1 := ALPosEx(Work_TagStart,Work_SourceString,1);
  T2 := T1 + TagStartLength;
  If (T1 > 0) and (T2 <= SourceStringLength) then begin
    InDoubleQuote := False;
    InsingleQuote := False;
    Token := Work_SourceString[T2];
    if token = '"' then InDoubleQuote := True
    else if token = '''' then InSingleQuote := True;
    While (T2 < SourceStringLength) and (InDoubleQuote or InSingleQuote or (Token <> FirstTagEndChar) or (ALPosEx(Work_TagEnd,Work_SourceString,T2) <> T2)) do begin
      inc(T2);
      Token := Work_SourceString[T2];
      If Token = '"' then InDoubleQuote := not InDoubleQuote and not InSingleQuote
      else If Token = '''' then InSingleQuote := not InSingleQuote and not InDoubleQuote
    end;
  end;

  If (T1 > 0) and (T2 > T1) Then begin
    ReplaceString := AlCopyStr(SourceString,T1 + TagStartLength,T2 - T1 - TagStartLength);

    TokenStr := ExtractTokenStr;
    ParamStr := ExtractParamsStr;
    ALExtractHeaderFields([' ', #9], [' ', #9], PChar(ParamStr), TagParams, False, AStripParamQuotes);
    Result := True
  end;
end;

{********************************************************}
{Parses a multi-valued string into its constituent fields.
 ExtractHeaderFields is a general utility to parse multi-valued HTTP header strings into separate substrings.
 * Separators is a set of characters that are used to separate individual values within the multi-valued string.
 * WhiteSpace is a set of characters that are to be ignored when parsing the string.
 * Content is the multi-valued string to be parsed.
 * Strings is the TStrings object that receives the individual values that are parsed from Content.
 * StripQuotes determines whether the surrounding quotes are removed from the resulting items. When StripQuotes is true, surrounding quotes are removed
   before substrings are added to Strings.
 Note:	Characters contained in Separators or WhiteSpace are treated as part of a value substring if the substring is surrounded by single or double quote
 marks. HTTP escape characters are converted using the ALHTTPDecode function.}
procedure ALExtractHeaderFields(Separators, WhiteSpace: TSysCharSet; Content: PChar; Strings: TStrings; Decode: Boolean; StripQuotes: Boolean = False);
var
  Head, Tail: PChar;
  EOS, InQuote, LeadQuote: Boolean;
  QuoteChar: Char;
  ExtractedField: string;
  WhiteSpaceWithCRLF: TSysCharSet;
  SeparatorsWithCRLF: TSysCharSet;

  function DoStripQuotes(const S: string): string;
  var I: Integer;
      InStripQuote: Boolean;
      StripQuoteChar: Char;
  begin
    Result := S;
    InStripQuote := False;
    StripQuoteChar := #0;
    if StripQuotes then
      for I := Length(Result) downto 1 do
        if Result[I] in ['''', '"'] then
          if InStripQuote and (StripQuoteChar = Result[I]) then begin
            Delete(Result, I, 1);
            InStripQuote := False;
          end
          else if not InStripQuote then begin
            StripQuoteChar := Result[I];
            InStripQuote := True;
            Delete(Result, I, 1);
          end
  end;

Begin
  if (Content = nil) or (Content^ = #0) then Exit;
  WhiteSpaceWithCRLF := WhiteSpace + [#13, #10];
  SeparatorsWithCRLF := Separators + [#0, #13, #10, '"', ''''];
  Tail := Content;
  QuoteChar := #0;
  repeat
    while Tail^ in WhiteSpaceWithCRLF do Inc(Tail);
    Head := Tail;
    InQuote := False;
    LeadQuote := False;
    while True do begin
      while (InQuote and not (Tail^ in [#0, '"', ''''])) or not (Tail^ in SeparatorsWithCRLF) do Inc(Tail);
      if Tail^ in ['"',''''] then begin
        if (QuoteChar <> #0) and (QuoteChar = Tail^) then QuoteChar := #0
        else If QuoteChar = #0 then begin
          LeadQuote := Head = Tail;
          QuoteChar := Tail^;
          if LeadQuote then Inc(Head);
        end;
        InQuote := QuoteChar <> #0;
        if InQuote then Inc(Tail)
        else Break;
      end else Break;
    end;
    if not LeadQuote and (Tail^ <> #0) and (Tail^ in ['"','''']) then Inc(Tail);
    EOS := Tail^ = #0;
    if Head^ <> #0 then begin
      SetString(ExtractedField, Head, Tail-Head);
      if Decode then Strings.Add(ALHTTPDecode(DoStripQuotes(ExtractedField)))
      else Strings.Add(DoStripQuotes(ExtractedField));
    end;
    Inc(Tail);
  until EOS;
end;

{*****************************************************}
Function ALAnsiUpperCaseNoDiacritic(S: string): string;
var Len1, Len2: Integer;
    i,J: integer;
    TmpStr1,
    TmpStr2: String;
begin
  result := '';
  If s = '' then exit;

  {upper the result}
  TmpStr1 := AnsiUppercase(s);
  Len1 := length(TmpStr1);

  {remove diacritic}
  Len2 := FoldString(MAP_COMPOSITE, PChar(TmpStr1), Len1, nil, 0);
  setlength(TmpStr2,len2);
  FoldString(MAP_COMPOSITE, PChar(TmpStr1), Len1, PChar(TmpStr2), len2);
  i := 1;
  J := 1;
  SetLength(result,len1);
  while J <= len1 do begin
    Result[j] := TmpStr2[i];
    if TmpStr1[j] <> TmpStr2[i] then inc(i,2)
    else inc(i);
    inc(j);
  end;
end;

{*****************************************************}
function ALGetStringFromFile(filename: string): string;
Var AFileStream: TfileStream;
begin
  AFileStream := TFileStream.Create(filename,fmOpenRead or fmShareDenyWrite);
  try

    If AFileStream.size > 0 then begin
      SetLength(Result, AFileStream.size);
      AfileStream.Read(Result[1],AfileStream.Size)
    end
    else Result := '';

  finally
    AfileStream.Free;
  end;
end;

{******************************************************************}
function ALGetStringFromFileWithoutUTF8BOM(filename: string): string;
Var AFileStream: TfileStream;
    aBOMStr: String;
    aSize: Integer;
begin
  AFileStream := TFileStream.Create(filename,fmOpenRead or fmShareDenyWrite);
  try

    aSize := AFileStream.size;
    If ASize > 0 then begin

      If Asize >= 3 then begin
        SetLength(aBOMStr,3);
        AfileStream.Read(aBOMStr[1],3);
        If AlUTF8DetectBOM(Pchar(aBOMStr), 3) then aSize := aSize - 3
        else AfileStream.Position := 0;
      end;

      If aSize > 0 then begin
        SetLength(Result, aSize);
        AfileStream.Read(Result[1],ASize)
      end
      else Result := '';

    end
    else Result := '';

  finally
    AfileStream.Free;
  end;
end;

{*************************************************}
procedure ALSaveStringtoFile(Str,filename: string);
Var AStringStream: TStringStream;
    AMemoryStream: TMemoryStream;
begin
  AMemoryStream := TMemoryStream.Create;
  try

    AStringStream := TStringStream.Create(str);
    try
      AmemoryStream.LoadFromStream(AstringStream);
      AmemoryStream.SaveToFile(filename);
    finally
      AStringStream.Free;
    end;

  finally
    AMemoryStream.Free;
  end;
end;

{************}
initialization
  ALInitStringReplaceFunct;
  ALInitPosExFunct;
  ALInitMovProc;
  ALInitPosFunct;
  ALInitCharPosFunct;
  ALInitCompareTextFunct;
  ALInitLowerCaseFunct;
  ALInitUpperCaseFunct;
end.
