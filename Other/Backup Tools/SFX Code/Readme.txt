If you want to build the Components\Archiver\SFXCode.res that contains
the SFX code, do the following :

- Compile SFX project
- Note the .EXE size and write it in the file StartOfFile.inc
- Edit the sfx1.res file and update the version tag for the SFXCodeSize
  Note that you can edit sfx1.rc and use brcc32.exe to get sfx1.res
- Compile SFX project again
- Edit the generate.bat to define the right path to the Dephi\bin folder
- Execute the generate.bat batch. It will do the following:
    . Rename the SFX.EXE to SFXCODE.EXE.
    . Open a DOS-Box and start the Program:
      <PF>Borland\Delphi\Bin\Brcc32.exe SFXCode.rc
      Note that SFXCode.exe and SFXCode.rc have to be in the same directory. The
      SFXCode.res will be built in the same folder.
    . Copy the SFXCode.res to the Components\Archiver folder

That's all !
