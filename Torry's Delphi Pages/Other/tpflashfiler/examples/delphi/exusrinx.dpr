library EXUsrInx;

{General notes:
  The DLL is quite large compared with the number of lines of actual
  code because it links in a lot of code and resources from SysUtils
  and Classes (these units are used by FFLLBASE).
  Removing FFLLBASE from the uses list and copying over the string
  comparison and uppercase routines from the source to FFLLBASE would
  help a great deal in making the final DLL smaller.}

uses
  FFLLBase,
  FFSrIntf;

function CompareUppercase(const Key1, Key2; aData : PffCompareData) : integer;
  {$IFDEF Win32} stdcall; {$ENDIF}
  {$IFDEF Windows} export; {$ENDIF}
var
  SS1 : TffShStr absolute Key1;
  SS2 : TffShStr absolute Key2;
begin
  {Note: the build key routine returns an uppercased string, hence we
   don't need to do an uppercase type comparison (which is slower), we
   can stick to the normal string comparison. Note the use of the FF
   routine FFCheckDescend: this flips the sign of the second parameter
   if the first is false}

  with aData^ do begin
    {if a full field comparison is needed...}
    if (cdFldCnt = 0) and (cdPartLen = 0) then
      {compare the two keys as short strings, up to cdKeyLen characters}
      Result := FFCheckDescend(aData^.cdAscend,
                               FFCmpShStr(SS1, SS2, aData^.cdKeyLen))
    {otherwise a partial field comparison is required...}
    else
      {compare the two keys as short strings, up to cdPartLen characters}
      Result := FFCheckDescend(aData^.cdAscend,
                               FFCmpShStr(SS1, SS2, aData^.cdPartLen));
  end;
end;

function BuildUppercase(Index     : integer;
                        DataRec   : PffByteArray;
                    var Key;
                        KeyLen    : integer) : boolean;
  {$IFDEF Win32} stdcall; {$ENDIF}
  {$IFDEF Windows} export; {$ENDIF}
var
  Field1 : PffShStr absolute DataRec;
  KeyStr : TffShStr absolute Key;
begin
  {we're building a key using the first field. It is a string[31]
   field, and the key is just the string uppercased. We assume
   therefore that the offset of the field is 0. We ignore KeyLen in
   this routine, it will be equal to the field size (unless we've
   hopelessly got it wrong).}
  KeyStr := FFShStrUpper(Field1^);
  Result := true;
end;

exports
  CompareUppercase,
  BuildUppercase;

begin
end.
