{
  TArchiverSFX (C) Copyright by Oliver Buschjost (autor_oliver@iname.com), 1998

  COPYRIGHT
  ---------

  This code is email-free-ware. You may use it, distribute it and modify it, but
  you may not charge for it. Please send me a mail if you use it, I'll be happy
  to see in which country it is used, and I'll be able to mail you the updates.

  In case of modifications you must mail me a copy of the modifications.
  The reasons are simple: Any changes that improve this free-ware component should
  be to benefit for everybody, not only you. That way you can be pretty sure,
  that this component has few errors and much functionality.
  In case of modifications, you will be on the credits list beneath.

  This Program contains code that is copyrightd by others.
  Some of the Comments of the original authors were removed if they were no
  longer relevant (.ZIP Archive specific comments for example)  
  All the code was released in Freeware programs including SourceCode.

- This SFX is based on Freewarecode that is copyrighted by

   Morgan Martinet        (mmm@imaginet.fr)                //The extraction code
   Arjen Broeze           (Arjen@nedap.nl)                 //The Code for the Progress Dialog
   Carl Bunton            (Twojags@cris.com)               //he made an SFX for .ZIP Files. The basic style of the Main Form, etc were made by him
   Eric W. Engler         (englere@swcp.com)               //Made modifications on the above mentioned SFX
   Markus Stephany        (mirbir.st@t-online.de)          //Made modifications on the above mentioned SFX

  DESCRIPTION
  -----------

  This code lets you create an Selfextracting archive.
}

//--Start of the Copyright statement that was included in Carl Bunton's Freeware SFX
//--when it was already modified by markus Stephany and Eric W. Engler
(******************************************************************)
(* ZipSFX                                                         *)
(* Copyright 1997, Carl Bunton                                    *)
(* Home of ZipTV compression components for Delphi                *)
(* Email: Twojags@cris.com                                        *)
(* Web-page: http://www.concentric.net/~twojags                   *)
(*                                                                *)
(* This program was written in Delphi 2 because version 2         *)
(* compiles a much smaller executable using the windows api.  It  *)
(* should be fully compatible with Delphi 3, but will produce a   *)
(* noticable increase of size in the final compiled program.      *)
(*                                                                *)
(*MODIFIED by M. Stephany mirbir.st@t-online.de  12/28/97-07/05/98*)
(* for some special purposes; modified lines are marked (##)      *)
(******************************************************************)

{ notes:

the initial release of zipsfx comes from Carl Bunton (see above).
he has created a powerful shareware vcl for delphi that can handle
almost all of the popular archive-formats like zip,lzh,arj...., called ziptv.

the first modifications came from Eric W. Engler, the author of the great freeware
delphi-vcl delzip that can handle zip-archives and -sfx's. (englere@swcp.com)

now i am trying to make the code a bit more sfx-creator- and sfx-user- friendly,
and i must say, eric is a very hard beta-tester :).

original zip-code comes from the infozip-group, they developped a free implementation
of the zip/unzip-code for unix and later for other platforms.
  Info-Zip home page:
  http://www.cdrom.com/pub/infozip/

regards, Markus Stephany, mirbir.st@t-online.de
Merkes' Pages at http://home.t-online.de/home/mirbir.st/

losheim am see, saarland, germany, jan 03 1998 ; mar 15, 1998 ; july 05 , 1998

NOTE : !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                                                                       !
TO GET ZIPSFX WORKING CORRECTLY, YOU SHOULD FIRST COMPILE IT ONCE AND THEN SET THE CORRECT VALUE FOR   !
StartOfFile IN StartOfFile.inc. THIS MUST BE SET TO THE SIZE OF THE COMPILED EXE.             !
THIS VALUE CAN DIFFER DEPENDING ON THE DELPHI-VERSION (AND OTHER THINGS I DO NOT UNDERSTAND).          !
PLEASE USE THE INCLUDED SFX.DOF AS YOUR PROJECT'S OPTIONS FILE.                                        !
                                                                                                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}
//--End of the Copyright statement that was included in Carl Bunton's Freeware SFX
//--when it was already modified by markus Stephany and Eric W. Engler

{
!!!!And also change the Values in the Ressourcefile!! (The Filesize and the TagInfo Size!!)!!!
}

{The Header is declared as record of the type TTaginfo and its structure is as follows:

TTagInfo = packed record
                ExecuteFileAfterExtract:boolean;
                UserChooseFilesToExtract:boolean;
                UserChooseOverwriteMode:boolean;
                UserAllowedToDontRunTheFile:boolean;
                DefaultOwerwriteMode:TOverwritemode;
                SFXFileSize:DWord; //Dateigröße des fertigen SFX, also: SFX.EXE-Code, sizeof(TTagefile) and Archivesize
                CommandLine:string[80];
                Caption:string[60];
                DefaultExtractPath:string[80];
                CopyrightLine:string[80];
                Language:TLanguage;
                Comment:TCommentMode;
                end;

This uses the following new types:

TOverwritemode=(confirm,overwrite,skip,update,existing,updateexisting);
TCommentMode=(none,Before,After,Both);


Informations on several parts of the TTAgInfo:

commandline:  the command line (to execute after successfull extraction) format :

              possible parameters that are parsed:

              <AD>  -->  will be replaced with the directory where the files were extracted to
              <TD>  -->  will be replaced with the Tempdirectory         (often 'c:\windows\temp' or 'c:\win95\temp' or 'c:\temp')
              <WD>  -->  will be replaced with the Windowsdirectory      (often 'c:\windows' or 'c:\win95')
              <SD>  -->  will be replaced with the Systemdirectory       (often 'c:\windows\system' or 'c:\win95\system')
              <PF>  -->  will be replaced with the ProgramFilesdirectory (often 'c:\Program Files' or 'c:\programme' [depending on the language of the installed Windows])

              if the string "<AD>" (greater than+AD+less than) is somewhere in the command line,
              it will be replaced with the path where the archive has been extracted to.
              (e.g. "<AD>readme\test.txt" after an extraction to the path "C:\Program files\unpacked" means :
              "c:\progra~1\unpacked\readme\test.txt") <- the short path will be created by zipsfx.
              if the pipe "|" is in the command-line, the part to the left will get the application to run
              and the part to the right will be it's argument;
              if the archive is extracted to e.g. "d:\unpack", then we will get the following :
              "<AD>setup\setup.exe|<AD>install.inf" will parse to :
              run "d:\unpack\setup\setup.exe" with parameters "d:\unpack\install.inf".
              "c:\windows\notepad.exe|<AD>readme.txt" will parse to :
              run "c:\windows\notepad.exe" with parameters "d:\unpack\readme.txt".
              "<AD>readme.txt" will parse to :
              open "d:\unpack\readme.txt" with its associated program, if there is any.
              "<AD>setup.exe" will run "d:\unpack\setup.exe" without special parameters.
              ...


DefaultExtractPath:   the default directory (to extract the files to) format :

                      possible parameters that are parsed:

                      <TD>  -->  will be replaced with the Tempdirectory         (often 'c:\windows\temp' or 'c:\win95\temp' or 'c:\temp')
                      <WD>  -->  will be replaced with the Windowsdirectory      (often 'c:\windows' or 'c:\win95')
                      <SD>  -->  will be replaced with the Systemdirectory       (often 'c:\windows\system' or 'c:\win95\system')
                      <PF>  -->  will be replaced with the ProgramFilesdirectory (often 'c:\Program Files' or 'c:\programme' [depending on the language of the installed Windows])

                      !!!!YOU SHOULD NOT USE <WD> or <SD> UNLESS IT IS REALLY NEEDED!!!!
                      !!!!Please tell the users of your programs, too, or simply disable it !!!!
}

PROGRAM Sfx;

{$A-}

USES  Dialog,
  SFXgbls,
  SFXmisc,
  Windows,
  dialogsel;

//{$R *.RES} //## renamed to sfx1.res (because of the delphi-created res-file) {Decreases the filesize}
{$R SFX1.res}

var
   MyClass : TMyClass;

BEGIN
  (* Open the archive *)
  InFile := CreateFile( PChar( ParamStr( 0 ){'c:\sfx140a\MMM - SFX\sfx1.exe'}),
                       GENERIC_READ,
                       FILE_SHARE_READ,
                       NIL,
                       OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL,
                       0 );

  (* If error, notify and abort *)
  IF InFile = INVALID_HANDLE_VALUE THEN
  BEGIN
     MessageBox( 0, PChar( ParamStr( 0 ) ), STR_EARCHIVE, mb_OK );
     EXIT;
  END;

  TRY
     (* Display the dialog *)
    getSfxFileInfo; //read the special setup-header from the file, if any {in dialogsel.pas}
  FINALLY
     (* Close the archive *)
     IF NOT CloseHandle( InFile ) THEN
        MessageBox( 0, STR_CANNOTCLOSE, STR_E , mb_OK );
    END;
    MyClass:=TMyClass.Create;
     DialogBox( hInstance, STR_MAINDLG, 0, @MainDialogProc ); //Create the Dialogbox
    MyClass.free;
  FreeTheMemoryOfTheStrings;
  END.
