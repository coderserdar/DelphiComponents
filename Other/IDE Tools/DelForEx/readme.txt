Program:
        DelforExp, Delphi Formatter

Version:
        2.4.1 for Delphi 2-7
        Note DelForEx7.dll might not work, please notify me if you 
        have problems

Category:
        Programmers tool

Description:
        DelforExp is a customizable source code formatter.
        It can improve the indentation, spacing, capitalization and
        the use of blank lines of Delphi 5.0 source code.
        In the default settings, the style of the Borland source code
        is followed closely.
        It is an expert that is integrated in the Delphi IDE.

Status:
        The program is released as FREEWARE to improve the productivity
        of Delphi. You may distribute the files freely as long as you don't
        make money by it. The use of the program is at own risk. (see also 
        license.txt)
        The source code is partly included, to make it possible to customize
        the user interface and upgrade to future Delphi versions. Only the 
        engine of the formatter is included as compiled dll file. 

Files:
        DelForEx5.dll   Delphi 5 version
        DelForDll.dll   The engine of the parser/formatter
        DelFor.hlp      On-line help
        SetupEx.dpr     Install/deinstall
        Readme.txt      this file
        License.txt     license notes 
        Preview.pas     Example pascal file for preview window
        Problems.pas    Nonsense Pascal with solved (and some known) problems in DelFor
	Source.zip	Zipped file with source of the interface parts of
                           DelForEx2.dpr (Delphi 2 version)
                           DelForEx3.dpr (Delphi 3 version)
                           DelForEx4.dpr (Delphi 4 version)
                           DelForEx5.dpr (Delphi 5 version)
                           DelForEx6.dpr (Delphi 6 version)
                           DelForEx7.dpr (Delphi 7 version)
                           DelFor.dpr    (Stand-alone version (needs the freeware opensource 
                                          component mwEdit)
                           SetupEx.dpr   (the setup utility)

Install:
        Copy all files to any directory.
          [If necessary unzip source.zip and build the required version 
          in Delphi.]
        Close Delphi
        Run the program SetupEx. 
        Restart Delphi.
        The second item of the "Tools" menu should be "Source Formatter..."
        To install/remove manually: set or delete the following registry keys
        D7: HKEY_CURRENT_USER\Software\Borland\Delphi\7.0\Experts\DelForEx6=[Path]\DelForEx7.dll
        D6: HKEY_CURRENT_USER\Software\Borland\Delphi\6.0\Experts\DelForEx6=[Path]\DelForEx6.dll
        D5: HKEY_CURRENT_USER\Software\Borland\Delphi\5.0\Experts\DelForEx5=[Path]\DelForEx5.dll
        D4: HKEY_CURRENT_USER\Software\Borland\Delphi\4.0\Experts\DelForEx4=[Path]\DelForEx4.dll
        D3: HKEY_CURRENT_USER\Software\Borland\Delphi\3.0\Experts\DelForExp=[Path]\DelForEx3.dll
        D2: HKEY_CURRENT_USER\Software\Borland\Delphi\2.0\Experts\DelForEx=[Path]\DelForEx2.dll

Compile instructions
        The version of the DFM files is Delphi 5. Therefore you will get errormessages in earlier 
        versions. Please open all dialog boxes and ignore all errors before compiling
        DelForEx2.dpr and DelForEx3.dpr.

DeInstall:
        Rerun the program SetupEx.
        Delete all files.

Contact:
        Egbert van Nes
        http://www.dow.wau.nl/aew/DelForExp.html (NOTE: HAS CHANGED)
        egbert.vannes@wur.nl
	Wageningen University
	The Netherlands

FAQ
        Q: Could you please add xxx option in your formatter? 
        A: There are many wishes like yours. Currently, I only add those that 
           - I like a LOT 
           - are not too difficult to add 
           My time is limited, I don't make money with this tool and my priority is to keep the formatter stable and to fix bugs. 

        Q: Please, give me the source code! 
        A: Pardon? 

        Q: Could I please buy the code? 
        A: No, I am sorry it is not for sale. 

        Q: Where can I find your free formatter?
        A: The latest version is at the official DelForExp Homepage.
           http://www.slm.wau.nl/wkao/DelForExp.html
           There are also copies at: Torry pages,The Delphi Super Page, 
           DelphiSource.COM and The Delphi Pages. 
           On these pages you can find many other useful components too. 

        Q: Have you also created a similar formatter for C++, Java 
           or Visual Basic?
        A: No, I am sorry, I have only created one for Delphi. 
           Being an addicted Delphi user, I have no plans to create one 
           for any other computer language.

        Q: I want to develop an expert. I'm completely new to the subject. Where can I find good resources about writing experts?
        A: I have used the following resources:
           (1) Templest software has gathered some information at 
               http://www.tempest-sw.com/opentools/.
	   (2) At the Torry pages there are many freeware components 
               (with source) of Martin Waldenburg that are very usefull 
               (e.g., TOTA, MPasLex, MWLexGen, IDEStrea (used for DelForExp)
               , MIDETre4)
           (3) There is a newsgroup about the Open Tools API: borland.public.delphi.opentoolsapi 

Known problems:
        (1)
        Compiler {$IFDEF} + {$ELSE} directives may be nested to 3 levels and break
        into blocks of code. After the third nested level the right indentation
        is not guaranteed.

        (2)
        After some options (align, adding line breaks) the indentation might
        be not correct. Rerunning DelFor can fix the problems.

        (3)
        It is tried to indent function directives after function declarations.
        In some cases this does not happen.

        (4)
        After formatting the positions of breakpoints and bookmarks are not changed 
        (all bookmarks are moved and stacked at the end of the file) breakpoints 
        are removed) If someone knows how to get and set the locations of these points
        I would be happy to hear from you.

        

