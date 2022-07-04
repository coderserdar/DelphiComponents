{   Unit cyzlib

    Description:
    Unit with functions using zlib library.

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

unit cyzlib;

interface

uses classes, sysUtils, zlib;

function ZCompressString(const aText: string; const aCompressionLevel: TZCompressionLevel = zcMax): string;
function ZDecompressString(const aText: string): string;

procedure ZCompressStream(StreamInput, StreamOutput: TStream; const aCompressionLevel: TZCompressionLevel = zcMax);
procedure ZDecompressStream(StreamInput, StreamOutput: TStream);

procedure ZCompressFile(const InputFile, OutputFile: string; const aCompressionLevel: TZCompressionLevel = zcMax);
procedure ZDecompressFile(const InputFile, OutputFile: string);

implementation

function ZCompressString(const aText: string; const aCompressionLevel: TZCompressionLevel = zcMax): string;
var
  strInput,
  strOutput: TStringStream;
  Zipper: TZCompressionStream;
begin
  Result:= '';
  strInput:= TStringStream.Create(aText);
  strOutput:= TStringStream.Create('');
  try
    Zipper:= TZCompressionStream.Create(strOutput, aCompressionLevel);
    try
      Zipper.CopyFrom(strInput, strInput.Size);
    finally
      Zipper.Free;
    end;
    Result:= strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

function ZDecompressString(const aText: string): string;
var
  strInput,
  strOutput: TStringStream;
  Unzipper: TZDecompressionStream;
begin
  Result:= '';
  strInput:= TStringStream.Create(aText);
  strOutput:= TStringStream.Create;
  try
    Unzipper:= TZDecompressionStream.Create(strInput);
    try
      strOutput.CopyFrom(Unzipper, Unzipper.Size);
    finally
      Unzipper.Free;
    end;
    Result:= strOutput.DataString;
  finally
    strInput.Free;
    strOutput.Free;
  end;
end;

procedure ZCompressStream(StreamInput, StreamOutput: TStream; const aCompressionLevel: TZCompressionLevel = zcMax);
var
  Zipper: TZCompressionStream;
begin
  Zipper := TZCompressionStream.Create(StreamOutput, aCompressionLevel);

  try
    Zipper.CopyFrom(StreamInput, StreamInput.Size);
  finally
    Zipper.Free;
  end;
end;

procedure ZDecompressStream(StreamInput, StreamOutput: TStream);
var
  Unzipper: TZDecompressionStream;
begin
  Unzipper := TZDecompressionStream.Create(StreamInput);

  try
    StreamOutput.CopyFrom(Unzipper, Unzipper.Size);
  finally
    Unzipper.Free;
  end;
end;

procedure ZCompressFile(const InputFile, OutputFile: string; const aCompressionLevel: TZCompressionLevel = zcMax);
var
  StreamInput, StreamOutput: TFileStream;
begin
  StreamInput := TFileStream.Create(InputFile, fmOpenRead);
  StreamOutput := TFileStream.Create(OutputFile, fmCreate);

  try
    ZCompressStream(StreamInput, StreamOutput);
  finally
    StreamInput.Free;
    StreamOutput.Free;
  end;
end;

procedure ZDecompressFile(const InputFile, OutputFile: string);
var
  StreamInput, StreamOutput: TFileStream;
begin
  StreamInput := TFileStream.Create(InputFile, fmOpenRead);
  StreamOutput := TFileStream.Create(OutputFile, fmCreate);

  try
    ZDecompressStream(StreamInput, StreamOutput);
  finally
    StreamInput.Free;
    StreamOutput.Free;
  end;
end;

end.
