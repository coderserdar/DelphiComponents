program Sfx;
(* Self-Extracting EXE example application for TCompLHA
   -- this is a quick n'easy proof of concept, but you can
      extend with options like a "where to install" form, or
      code which causes it to clean itself up afterwards etc.

      This version has no user interaction, to keep it small.
      The resulting EXE can be made smaller still if you obtain
      the TCompLHA source and use the $DEFINES therein to
      selectively disable everything except (say) LZH5 expansion.

      It expands the contents of the appended archive (including paths
      if they are in the archive) into either the current directory, or
      the directory specified by the command line parameter. It then runs
      the EXE program specified in the ProgramToRun constant. Note that
      it expands the WHOLE archive, not just the EXE, so data can be
      in the archive as well. It does not "clean up" afterwards.

   -- How to use it:
   1. Modify the ProgramToRun constant, and build this application
   2. VITAL!!!!! Check the size of the EXE file it makes
   3. If the size is different from the SizeOfThisEXE constant,
      modify the constant to the correct size and build the project again
   4. Exit this project, and put all the files you want to compress
      (not this EXE!) into their own directory or directory tree
   5. Load, build and run the MakeEXE project
   6. Test-run the resulting EXE
*)
uses
  WinTypes, WinProcs, Classes, SysUtils, CompLHA;

{$R *.RES}

const
 SizeOfThisEXE = 95232; { VITAL!!!! check after build and CHANGE if necessary! }
 ProgramToRun='LHADEMO.exe';{ for example -- change at will }

var fs: TFileStream;
    namebuff: array[0..255] of char; { for WinExe }
    Compress: TCompLHA; { compression component }
begin
  fs := TFilestream.create(paramstr(0),fmOpenRead or fmShareDenyNone); { read our own EXE file }
  Compress := TCompLHA.Create(nil);
  try
   if fs.Size > SizeOfThisEXE then { has an archive appended }
   begin
     fs.seek(SizeOfThisEXE,soFromBeginning);
     with Compress do
     begin
       TargetPath := paramstr(1);     { optional "where to install" parameter }
       MakeDirectories := True;       { in case they specified a path which doesn't exist }
       ExpandFilesFromStream(fs,nil); { do the lot, into TargetPath (current dir if empty) }
       WinExec(strPCopy(NameBuff,TargetPath+ProgramToRun),sw_Show); { run the program }
     end; { with }
   end; { if we have an archive }
  finally
   Compress.free;
   fs.free;
  end;
end.
