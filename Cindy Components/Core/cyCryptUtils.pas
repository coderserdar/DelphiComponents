{   Unit cyCryptUtils

    Description:
    Unit with crypt functions.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyCryptUtils;

interface

uses
  Classes, SysUtils, Math;

     // *** VERNAM METHODS *** //
     function VERNAM_CRYPT(aString: String; var GeneratedKey: String): String;
     function VERNAM_CRYPT_WITH_KEY(aString: String; withKey: String): String;
     function VERNAM_DECRYPT(Criptado, withKey: String): String;

     procedure VERNAM_CRYPT_FILE(Src, Dest, GeneratedKey: TFileName);
     procedure VERNAM_CRYPT_FILE_WITH_KEY(Src, Dest, withKey: TFileName);
     procedure VERNAM_DECRYPT_FILE(Src, Dest, withKey: TFileName);


implementation

function VERNAM_CRYPT(aString: String; var GeneratedKey: String): String;
var
  i, curOrd: Integer;
  RandomVal: byte;
begin
  Result := '';
  GeneratedKey  := '';
  Randomize;

  for i := 1 to length(aString) do
  begin
    repeat
      RandomVal := RandomRange(1, 127);
      curOrd    := Ord(aString[i]) XOR RandomVal;
    until curOrd <> 0;   // Ça foire si curOrd = 0 (Quand Ord(aString[i]) = RandomVal)  parce que chr(0) n' existe pas !!!

    Result := Result + Chr(curOrd);
    GeneratedKey := GeneratedKey + chr(RandomVal);
  end;
end;

function VERNAM_CRYPT_WITH_KEY(aString: String; withKey: String): String;
var
  i, curOrd, lengthwithKey, lengthaString, repetir: Integer;
begin
  Result := '';

  if withKey = '' then withKey := 'a';    // Mettre qque chose sinon ça va foiré ...

  lengthwithKey := length(withKey);
  lengthaString := length(aString);

  if lengthwithKey < lengthaString        // La taille de la clé doit être au moins aussi grande que le message crypté ...
  then begin
    Repetir := 1;
    for i := lengthwithKey + 1 to lengthaString do
    begin
      withKey   := withKey + withKey[Repetir];

      if Repetir < lengthwithKey
      then Repetir := Repetir + 1
      else Repetir := 1;
    end;
  end;

  for i := 1 to length(aString) do
  begin
    curOrd    := Ord(aString[i]) XOR Ord(withKey[i]);

    if curOrd = 0                  // Ça arrive quand aString[i] = withKey[i] ...
    then curOrd := Ord(aString[i]);  // Chr(0) n' existe pas, donc on peut pas coder ce caractere ...

    Result    := Result + Chr(curOrd);
  end;
end;

function VERNAM_DECRYPT(Criptado, withKey: String): String;
var
  i, repetir, curOrd, lengthwithKey, lengthaString: Integer;
begin
  Result := '';

  if withKey = '' then withKey := 'a';

  lengthwithKey := length(withKey);
  lengthaString := length(Criptado);

  if lengthwithKey < lengthaString
  then begin
    Repetir := 1;
    for i := lengthwithKey + 1 to lengthaString do
    begin
      withKey  := withKey + withKey[Repetir];

      if Repetir < lengthwithKey
      then Repetir := Repetir + 1
      else Repetir := 1;
    end;
  end;

  for i := 1 to lengthaString do
  begin
    curOrd := Ord(Criptado[i]) XOR Ord(withKey[i]);

    if curOrd = 0                      // On est dans le cas où aString[i] était égal à withKey[i] ...
    then curOrd := Ord(Criptado[i]);   // Donc, la lettre ne fut pas cryptée ...

    Result := Result + Chr(curOrd);
  end;
end;

procedure VERNAM_CRYPT_FILE(Src, Dest, GeneratedKey: TFileName);
var
  fs_Src, fs_Dest, fs_GeneratedKey: TFileStream;
  fs_Src_Buffer, fs_Dest_Buffer, fs_GeneratedKey_Buffer: Array[0..1023] of byte; // Buffers de 1Ko ...
  fs_Buffer_size: Int64;
  i: Integer;
begin
  Randomize;
  fs_Src   := TFileStream.Create(Src,   fmOpenRead or fmShareDenyWrite);
  fs_Dest  := TFileStream.Create(Dest,  fmCreate or fmShareExclusive);
  fs_GeneratedKey := TFileStream.Create(GeneratedKey, fmCreate or fmShareExclusive);

  try
    while fs_Src.Position < fs_Src.Size do
    begin
      fs_Buffer_size := fs_Src.Size - fs_Src.Position;

      If fs_Buffer_size > 1024
      Then fs_Buffer_size := 1024;

      fs_Src.Read(fs_Src_Buffer, fs_Buffer_size);

      for i := 0 to fs_Buffer_size - 1 do
      begin
        fs_GeneratedKey_Buffer[i] := RandomRange(0, 247);  // 1 byte = 8bits (1 octet en français) donc valeur max. est de 11111111 en binaire = 247 en décimal ...
        fs_Dest_Buffer[i]  := fs_GeneratedKey_Buffer[i] XOR fs_Src_Buffer[i];
      end;

      fs_GeneratedKey.Write(fs_GeneratedKey_Buffer, fs_Buffer_size);
      fs_Dest.Write(fs_Dest_Buffer, fs_Buffer_size);
    end;
  finally
    fs_Src.Free;
    fs_Dest.Free;
    fs_GeneratedKey.Free;
  end;
end;

procedure VERNAM_CRYPT_FILE_WITH_KEY(Src, Dest, withKey: TFileName);
var
  fs_Src, fs_Dest, fs_withKey: TFileStream;
  fs_Src_Buffer, fs_Dest_Buffer, fs_withKey_Buffer, fs_withKey_Buffer2: Array[0..1023] of byte; // Buffers de 1Ko ...
  fs_Src_Buffer_size, fs_withKey_Buffer_size, TransfBytes: Int64;
  i: Integer;
begin
  Randomize;
  fs_Src   := TFileStream.Create(Src,   fmOpenRead or fmShareDenyWrite);
  fs_Dest  := TFileStream.Create(Dest,  fmCreate or fmShareExclusive);
  fs_withKey := TFileStream.Create(withKey, fmOpenRead or fmShareDenyWrite);

  Try
    while fs_Src.Position < fs_Src.Size do
    begin
      fs_Src_Buffer_size := fs_Src.Size - fs_Src.Position;
      If fs_Src_Buffer_size > 1024
      Then fs_Src_Buffer_size := 1024;
      fs_Src.Read(fs_Src_Buffer, fs_Src_Buffer_size);

      // On doit contrôler la lecture du fichier clé parce que celui-ci peut être + petit que le fichier crypté !
      fs_withKey_Buffer_size := 0;
      while fs_withKey_Buffer_size < fs_Src_Buffer_size do   // On doit avoir le meme nombre de bytes ...
      begin
        TransfBytes := fs_withKey.Size - fs_withKey.Position;  // Bytes dispo à lire dans le fichier clé ...

        if TransfBytes = 0 then   // On est à la fin du fichier clé ...
        begin
          fs_withKey.Seek(0, soFromBeginning);
          TransfBytes := fs_withKey.Size;
        end;

        If TransfBytes + fs_withKey_Buffer_size > fs_Src_Buffer_size
        Then TransfBytes := fs_Src_Buffer_size - fs_withKey_Buffer_size;

        fs_withKey.Read(fs_withKey_Buffer2, TransfBytes);      // Mémoire temporaire pour transférer vers fs_withKey_Buffer ...

        for i := 0 to TransfBytes - 1 do                   // Compléter le buffer de la clé ...
          fs_withKey_Buffer[fs_withKey_Buffer_size + i] := fs_withKey_Buffer2[i];

        fs_withKey_Buffer_size := fs_withKey_Buffer_size + TransfBytes;
      end;

      for i := 0 to fs_Src_Buffer_size - 1 do
        fs_Dest_Buffer[i]  := fs_withKey_Buffer[i] XOR fs_Src_Buffer[i];

      fs_Dest.Write(fs_Dest_Buffer, fs_Src_Buffer_size);
    end;
  finally
    fs_Src.Free;
    fs_Dest.Free;
    fs_withKey.Free;
  end;
end;

procedure VERNAM_DECRYPT_FILE(Src, Dest, withKey: TFileName);
var
  fs_Src, fs_Dest, fs_withKey: TFileStream;
  fs_Src_Buffer, fs_Dest_Buffer, fs_withKey_Buffer, fs_withKey_Buffer2: Array[0..1023] of byte; // Buffers de 1Ko ...
  fs_Src_Buffer_size, fs_withKey_Buffer_size, TransfBytes: Int64;
  i: Integer;
begin
  fs_Src   := TFileStream.Create(Src,   fmOpenRead or fmShareDenyWrite);
  fs_Dest  := TFileStream.Create(Dest,  fmCreate or fmShareExclusive);
  fs_withKey := TFileStream.Create(withKey, fmOpenRead or fmShareDenyWrite);

  try
    while fs_Src.Position < fs_Src.Size do
    begin
      fs_Src_Buffer_size := fs_Src.Size - fs_Src.Position;
      If fs_Src_Buffer_size > 1024
      Then fs_Src_Buffer_size := 1024;
      fs_Src.Read(fs_Src_Buffer, fs_Src_Buffer_size);

      // On doit contrôler la lecture du fichier clé parce que celui-ci peut être + petit que le fichier crypté !
      fs_withKey_Buffer_size := 0;
      while fs_withKey_Buffer_size < fs_Src_Buffer_size do
      begin
        TransfBytes := fs_withKey.Size - fs_withKey.Position;  // Bytes dispo à lire dans le fichier clé ...

        If TransfBytes = 0    // On est à la fin du fichier clé ...
        Then Begin
          fs_withKey.Seek(0, soFromBeginning);
          TransfBytes := fs_withKey.Size;
        End;

        If TransfBytes + fs_withKey_Buffer_size > fs_Src_Buffer_size
        Then TransfBytes := fs_Src_Buffer_size - fs_withKey_Buffer_size;

        fs_withKey.Read(fs_withKey_Buffer2, TransfBytes);      // Mémoire temporaire pour transférer vers fs_withKey_Buffer ...

        for i := 0 to TransfBytes - 1 do                   // Compléter le buffer de la clé ...
          fs_withKey_Buffer[fs_withKey_Buffer_size + i] := fs_withKey_Buffer2[i];

        fs_withKey_Buffer_size := fs_withKey_Buffer_size + TransfBytes;
      end;

      for i := 0 to fs_Src_Buffer_size - 1 do
        fs_Dest_Buffer[i] := fs_withKey_Buffer[i] XOR fs_Src_Buffer[i];

      fs_Dest.Write(fs_Dest_Buffer, fs_Src_Buffer_size);
    end;
  finally
    fs_Src.Free;
    fs_Dest.Free;
    fs_withKey.Free;
  end;
end;

end.
