AUTHOR:
~~~~~~~

 Morgan Martinet
    23 rue du 14 juillet
    94270 le Kremlin-Bicetre
    France
    Phone (Work): 01 47 25 70 77
    e-mail: morgan.martinet@altavista.net
            marat@multimania.com
            mmm@mcom.fr
            


GREETINGS:
~~~~~~~~~~
  I'd like to thank Oliver Buschjost for his enthusiasm and for his smart suggestions.


CONTRIBUTORS:
~~~~~~~~~~~~~
  Oliver Buschjost for the SFX Code
  e-mail: buschjost@geocities.com

  Angus Johnson,   ajohnson@rpi.net.au
  Anders Melander, anders@melander.dk, http://www.melander.dk
  Graham Wideman,  graham@sdsu.edu,  http://www.wideman-one.com
  for the Drag&Drop components used by WinArchiver (WinArchiver\Components)

  
  S.Kurinny & S.Kostinsky
  E-mail:  Sergey Kurinny kurinn@gu.kiev.ua
	   Sergey Kostinsky kostin@kostin.kiev.ua
  WWW:     Dream Company
           http://www.brama.com/dream-co
  for the Difference Maker (V1.34) that lets TArchiver make patches


TRANSLATORS:
~~~~~~~~~~~~
  English & french :   Morgan Martinet
  German :             Oliver Buschjost (buschjost@geocities.com)
  Italian :            Gabriele Bigliardi (gbigliardi@manord.com)
  Portuguese :         Hugo Souza (hsouza@spcb.com.br)
  Chinese :            Huanlin Tsai (easyman@ms2.seeder.net)
  Spanish :            Erick I. Jimenez Alvarado (erickj@usa.net)
  Russian :            Leonid Belousov (upsuntec@cityline.ru)
  Dutch :              Franck De Cock (fdecock@mail.dma.be)

COPYRIGHT:
~~~~~~~~~~
  This components are postcard-ware or at least email-ware.
  If you're really happy of this components, don't hesitate to send us money
  in order to contribute to this work.
  You may use them, distribute them and modify them, but
  you may not charge for them. Please send me a mail if you use one of them, I'll be happy
  to see in which country it is used, and I'll be able to mail you the updates.

  This source code is distributed with no WARRANTY, of any kind.
  Everyone is allowed to use and change this code free for his own tasks
  and projects, as long as this header and its copyright text is intact.
  For changed versions of this code, which are publicly distributed the
  following additional conditions have to be fullfilled:
  1) The header has to contain a comment on the change and the author of it.
  2) A copy of the changed source has to be sent to one of the above E-Mail
     addresses or our then valid address, if this is possible to the
     author.
  The second condition has the target to maintain an up to date central
  version of the components. If this condition is not acceptable for
  confidential or legal reasons, everyone is free to derive components
  or to generate a diff file to our or other original sources.


DESCRIPTION:
~~~~~~~~~~~~
  TCopyFile           lets you easily copy a file or a whole directory, and much more !

  TCustomExtractor    lets you extract archive of files made by TCustomArchiver.
                      (Open API)

  TExtractor          lets you extract archive of files made by TArchiver.
                      Useful for a Self extracting archive. It uses the minimum
                      of the VCL to be the as light as possible.
                      (Default API)

  TCustomArchiver     lets you build an archive of files. It supports adding, extracting,
                      enumerating and deleting files, and segmentation.
                      (Open API)

  TArchiver           Same as TCustomArchiver, but the archiving is done with the pasZLib compression
                      library, and the default encryption uses the Blowfish algorithm.
                      (Default API)

  *** The following components are quite obsolete, because TArchiver can do what they did,
      and even more !
      They remain usefull if you use another Compression component which can't segment
      (like DelZip).

  TMyBackup       lets you backup/restore files to/from a removable floppy.

  TExtBackup      Same as TMyBackup, but closes/reopens all tables used by your session.

  TMyArchBackup   Same as TMyBackuo, but uses TArchiver for compressing the data.


FILES:
~~~~~~
  Readme.txt                    This file.
  Components\Backup tools.dpk   The package containing all components
  Components\CopyFile.pas       The TCopyFile component
  Components\MyBackup.pas       The TMyBackup component
  Components\ExtBackup.pas      The TExtBackup component
  Components\MyArchBackup.pas   The TMyBackup component
  Components\prgform.*          A form for displaying status information
                                in TCopyFile and TMyBackup.
  Components\aDiff.pas          Difference Maker unit
  Components\Archiver           Units implementing the TArchiver component
  Components\ZLib               Units implementing the ZLib (GNU gzip)
				  by NOMSSI NZALI (see the readme.txt).
  Components\Crypto             Components for crypting
                                  by CRYPTOCard Corporation, Greg Carter
                                  (see the readme.txt). It is not the full package,
                                  but only the units of the components with
                                  the 32bit DCR files. If you want it complete,
                                  just go at http://www.crpytocard.com
  Examples\Delphi X\MyBackup        Folder containing a demonstration of how to
                                    use TMyBackup. (X is the version of Delphi)
  Examples\Delphi X\WinArchiver     Folder containing a complete tool demonstrating how to
                                    use TArchiver (it's a clone of Winzip)
                                    (X is the version of Delphi).
                                    You can freely distribute this tool with your applications,
                                    because it will let the enduser to easily handle your archives.
                                    Look at the readme.txt file for informations about this tool.
  Examples\Delphi X\ArchBackup      Folder containing a simple application showing you how to
                                    do a Backup/Restore with TArchiver.
  Examples\Delphi X\ExternalStream  Folder containing a demo of the use of an external stream
                                    like a TBlobStream for archiving.
  SFX Code                          Folder containing the code of the SFX
                                    by Oliver Buschjost.


Note that Delphi4 examples works without any problem with Delphi5.


INSTALLATION:
~~~~~~~~~~~~~
  Delphi2:
    install components: CopyFile.pas, MyBackup.pas, ExtBackup.pas, MyArchBackup.pas,
                        Archiver\RegisterArchiver.pas

  Delphi3:
    open package "BackupTools_d3.dpk", compile it and install it.

  Delphi4:
    open package "BackupTools_d4.dpk", compile it and install it.

  Delphi5:
    open package "BackupTools_d5.dpk", compile it and install it.

  Be sure that your Delphi Path contains a reference to the "Components" folder, to
  the "Components\ZLib" folder, to the "Components\Crypto" folder (if you haven't
  installed the CryptoCard components before), and to the "Components\Archiver" folder.


NOTES:
~~~~~
  The documentation is inside the units.
  But for TArchiver, the documentation is in the file
  Components\Archiver\help.txt
  You can look at the "history.txt" and the "to do.txt" list too.
  Look at the "tutorial.txt" for a first draft.
  Look at the "Readme.txt" file in the WinArchiver folder.


UPDATES:
~~~~~~~~
  You can get updates at http://www.multimania.com/marat/
