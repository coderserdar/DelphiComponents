{   Unit cyImageEn3

    Description:
    Unit with functions to use with Indy components.

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

unit cyIndyMail;

interface

uses Classes, SysUtils, idGlobal, idHttp, idComponent, idCoderHeader, IdCoderMIME, IdHashMessageDigest, IdAttachment, IdURI,
       cyStrUtils, Dialogs;

type
  // Set ContentType according to mail content
  // RelatedAttach are embedded content (like images) on html page
  TContentTypeMessage = (cmPlainText, cmPlainText_Attach,
                         cmHtml_Attach, cmHtml_RelatedAttach,
                         cmAlterText_Html,
                         cmAlterText_Html_Attach,
                         cmAlterText_Html_RelatedAttach,
                         cmAlterText_Html_Attach_RelatedAttach,
                         cmReadNotification);

const
  MessagePlainText                           = 'text/plain';
  MessagePlainText_Attach                    = 'multipart/mixed';
  MessageAlterText_Html                      = 'multipart/alternative';  // Hide attachments and only html is viewed
  MessageHtml_Attach                         = 'multipart/mixed';
  MessageHtml_RelatedAttach                  = 'multipart/related; type="text/html"'; // No simple text
  MessageAlterText_Html_Attach               = 'multipart/mixed';
  MessageAlterText_Html_RelatedAttach        = 'multipart/related; type="multipart/alternative"';
  MessageAlterText_Html_Attach_RelatedAttach = 'multipart/mixed';
  MessageReadNotification                    = 'multipart/report; report-type="disposition-notification"';

function ForceDecodeHeader(aHeader: String): String;

function GetAttachmentFilename(aAttachment: TIdAttachment; const EmbeddedEmlFileName: string): String;

implementation

{$R Resources\cyIndyMail_MIME_types.rec}

// Decode idMsg.Subject, IdMsg.From.Address etc ...
function ForceDecodeHeader(aHeader: String): String;
var
  VEndPos: Integer;

    procedure Terminate_aHeader(AStartPos: Integer; var VEndPos: Integer);
    var
      LCharSet, LEncoding, LData, LDataEnd: Integer;
    begin
      LCharSet := PosIdx('=?', AHeader, AStartPos);  {Do not Localize}
      if (LCharSet = 0) or (LCharSet > VEndPos) then begin
        Exit;
      end;
      Inc(LCharSet, 2);

      LEncoding := PosIdx('?', AHeader, LCharSet);  {Do not Localize}
      if (LEncoding = 0) or (LEncoding > VEndPos) then begin
        Exit;
      end;
      Inc(LEncoding);

      LData := PosIdx('?', AHeader, LEncoding);  {Do not Localize}
      if (LData = 0) or (LData > VEndPos) then begin
        Exit;
      end;
      Inc(LData);

      LDataEnd := PosIdx('?=', AHeader, LData);  {Do not Localize}
      if (LDataEnd = 0) or (LDataEnd > VEndPos) then begin
        // My code :
        aHeader := aHeader + '?=';
      end;
    end;

begin
  (*
    Why?: "Subject" not correctly returned on DecodeHeader function:
    Subject Exemple:
    =?iso-8859-1?Q?Est=E1 na hora de avan=E7ar para o RAD Studio XE Enterprise!?=

    idMessage.pas:
    Subject := DecodeHeader(Headers.Values['Subject']);

     idCoderHeader.pas linha 228:
        LDataEnd := PosIdx('?=', AHeader, LData);
        if (LDataEnd = 0) // RHR or (LDataEnd > VEndPos)
        then begin
          Exit;
        end;

  // Depois de ter falado com Remy Lebeau, pela norma iso, não deveria ter espaços no assunto pelo que é por isso que não descodifica ... *)


  // 2015-02-12 UPDATE!  Seems that as above explained, line 228 is waiting for '?=' that is not found in order to decode IdMsg.From.Address
  VEndPos := Length(aHeader);
  Terminate_aHeader(1, VEndPos);


  aHeader := StringReplace(aHeader, ' ', '&nbsp;', [rfReplaceAll]);
  aHeader := idCoderHeader.DecodeHeader(aHeader);
  Result := StringReplace(aHeader, '&nbsp;', ' ', [rfReplaceAll]);
end;

function GetAttachmentFilename(aAttachment: TIdAttachment; const EmbeddedEmlFileName: string): String;
var
  LowerContentType: String;
  StringStream: TStringStream;

      // Get file information by ContentType :
      function GetFileNameFromMIMETYPE: String;
      var
        Stream: TResourceStream;
        ListaMIMES: TStringList;
        Linha, ContentType, ExtensionFile: String;
        i, x: Integer;
      begin
        Result := '';

        Stream := TResourceStream.Create(hInstance, 'MIME_TYPES', 'TEXT');

        try
          ListaMIMES := TStringList.Create;

          try
            ListaMIMES.LoadFromStream(Stream);
            //Stream.SaveToFile('d:\_output\ficheiro em stream.txt');

            for i := 0 to ListaMIMES.Count-1 do
            begin
              Linha := ListaMIMES[i];  // Exemple: application/msword;extension file=doc

              x := pos(';extension file=', Linha);  // Não gerir outras linhas de texto de comentàrio etc ...

              if x <> 0 then
              begin
                ContentType := copy(Linha, 1, x - 1);

                if AnsiLowerCase(ContentType) = LowerContentType then
                begin
                  ExtensionFile := copy( Linha, x + 16, Length(Linha) );
                  Result := 'Embedded.' + ExtensionFile;
                  Break;
                end;
              end;
            end;
          finally
            ListaMIMES.Free;
          end;


        finally
          Stream.Free;
        end;

      end;

      function SanitizeFilename(const aFilename: String): String;
      var
        i: Integer;
      begin
        Result := '';

        for i := 1 to length(aFilename) do
          if not (aFilename[i] in ['<', '>', ':', '"', '/', '\', '|', '?', '*', '''']) then
            Result := Result + aFilename[i];
      end;

begin
  Result := aAttachment.FileName;

  LowerContentType := TIdURI.URLDecode(aAttachment.ContentType);
  LowerContentType := AnsiLowerCase(LowerContentType);


  //  Not working :
  // if aAttachment is TIdAttachmentFile then
  //    ShowMessage(TIdAttachmentFile(aAttachment).StoredPathName);

  // Embedded MIME (html) images :
  if (Result = '') and (Pos('image', LowerContentType) = 1) then
  begin
    // Check if it is an embedded image "cid" but without name
    // Ex. ContentType = image/jpeg
    Result := 'embedded';
    if Pos('jpeg', LowerContentType) > 0 then Result := Result + '.jpg';
    if Pos('jpg', LowerContentType) > 0  then Result := Result + '.jpg';
    if Pos('gif', LowerContentType) > 0  then Result := Result + '.gif';
    if Pos('png', LowerContentType) > 0  then Result := Result + '.png';
  end;

  // Another Embedded MIME extension file :
  if Result = '' then
    Result := GetFileNameFromMIMETYPE;

  // Extract name from ContentType :
  if Result = '' then
  begin
    // LowerContentType = 'application/vnd.ms-excel; name*=UTF-8''REFER êNCIES.xls'
    Result := String_copy(LowerContentType, srFromRight, '=', false);

    // Bypass encoding problems :
    if pos('utf-8', Result) = 1 then
    begin
      //Result := StringReplace(Result, 'utf-8', '=?utf-8?Q?', []);
      Result := StringReplace(Result, 'utf-8', '', []);
      Result := cyIndyMail.ForceDecodeHeader(Result);
    end;
  end;


  if Result = '' then  // Nothing works until here ...
    // MessageParts like 'message/*' (exemple: 'message/rfc822') are embedded messages, so must be extract as it !
    if ( pos('message/', AnsiLowercase(aAttachment.ContentType)) = 1 ) and ( AnsiLowercase(aAttachment.ContentDisposition) = 'attachment' ) then
      Result := EmbeddedEmlFileName
    else
      Result := 'ATT00001' + ExtractFileExt(LowerContentType);

  Result := SanitizeFilename(Result);
end;

end.
