unit DoInstal;

{ TCompress 3.0 routine to install a self-extracting archive from a
  compressed resource -- no change from V2.5
 See SELFEXTF.PAS or SELFXSML.DPR for detailed information on this file }

interface

procedure DoInstall(TargetDir, ExeFilename, ReadmeFilename: string);

implementation
uses WinTypes, WinProcs, Classes, SysUtils  ,Compress;

const  RESOURCE_NAME = 'MyArchiv';

procedure DoInstall(TargetDir, ExeFilename, ReadmeFilename: string);
var TempStream: TStream;
    NameBuff: Array[0..255] of char;
    Compress: TCompress;
begin
 Compress := TCompress.Create(nil);
 with Compress do
 begin
   TempStream := LoadCompressedResource(RESOURCE_NAME,'');
   { Right -- at this point TempStream has our compressed file in
     a stream we can usefully access. Start at the beginning! }
   try
      RegNumber := 0; { your TCompress registration ID }
      RegName := 'Your name';
      MakeDirectories := True;
      Targetpath := TargetDir;
      if TempStream<>nil then
        ExpandFilesFromStream(TempStream,nil); { get the lot }
   finally
      TempStream.free;
   end;
 end;
 if ReadMeFileName<>'' then { if we get here, we should have expanded ok }
    WinExec(strPCopy(NameBuff,'NOTEPAD '+TargetDir+ReadMeFileName),sw_Show);
 if EXEFileName<>'' then { if we get here, we should have expanded ok }
    WinExec(strPCopy(NameBuff,TargetDir+EXEFileName),sw_Show);
end;

end.
 