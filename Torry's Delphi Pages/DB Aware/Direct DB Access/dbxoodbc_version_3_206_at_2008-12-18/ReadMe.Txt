{
  Kylix / Delphi open source DbExpress driver for ODBC

  Copyright (c) 2001-2008 Edward Benson

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}

The Open-dbExpress project is a home for Open Source dbExpress drivers.

dbExpress as cross-platform data access technology developed by Borland for its Kylix, Delphi and
C++ Builder products. It takes the best ideas from the BDE (Borland Database Engine), Borland
ADOExpress, and other technologies such as Microsoft ADO.

Note -- dbExpress drivers can only be used with Borland products, specifically: Borland Kylix (for
Linux); Borland Delphi version 6 and above (for Microsoft Windows); Borland C++ Builder version 6
and above (for Microsoft Windows).

The latest version is only available from the project CVS repository.

Home page:
  http://open-dbexpress.sourceforge.net/

Project Home Page:
  https://sourceforge.net/projects/open-dbexpress/

CVS repository in web:
  http://open-dbexpress.cvs.sourceforge.net/open-dbexpress/dbxoodbc/

What to do at occurrence of problems:
*************************************

English:
========

If you have problems with the driver it is necessary for you to make the following steps:

1) If it is possible to switch on in your project DbxOpenOdbc.pas unit.

2) If debugging with included DbxOpenOdbc.pas does not decide your problem you need to
   switch on the mode of journalizing.

3) For inclusion on of the mode of journalizing, you need to switch on in DbxOpenOdbc.pas unit
   of an option {$DEFINE _DEBUG_} and {$DEFINE _TRACE_CALLS_}; and to switch off an
   option {.$DEFINE _RELEASE_}. After that you need to rebuild your project or
   OpenODBC the driver.

4) Debug messages can be seen in Delphi IDE, in the window "Event Log". But it is accessible
   only in an event of start of the application from the environment of development.
   Otherwise it is necessary to take advantage of the special program "Debug View":
   http://www.sysinternals.com/Utilities/DebugView.html
   More in detail about it is indicated in "DbxOpenOdbcTrace.pas unit.

5) In the debug window versions of the Database, ODBC the driver, OpenODBC, parameters of
   connection are indicated. If you want to ask connected with your problem necessarily indicate
   this information. It will allow to find faster a key to your problem.
   In the journal the history of calls of methods ...

6) Some files (*.7z) are packed by archiver "7-Zip":
     http://www.7-zip.org
     http://sourceforge.net/projects/sevenzip/

Project Sources (Browse CVS):
http://open-dbexpress.cvs.sourceforge.net/open-dbexpress/dbxoodbc/

I recommend Tortoise CVS as the easiest to use Windows CVS client, from http://www.tortoisecvs.org
You will need to enter the CVS path (all on one line, including the leading colon :
:pserver:anonymous@cvs.open-dbexpress.sourceforge.net:/cvsroot/open-dbexpress

or

Example of loading/updating of the project from CVS by means of a batch file.
For work it is necessary to edit a path to cvs.exe.

load_cvs_dbxoodbc.bat
=================================================================
@echo off
set path=C:\Program Files\WinCVS\CVSNT;%path%
rem set path=D:\CygWin\bin;%path%
echo **********************************************
echo  - When prompted for a password for anonymous,
echo    simply press the Enter key.
echo **********************************************
rem set cvsproj=cvs-pserver.sourceforge.net:80/cvsroot/open-dbexpress
set cvsproj=open-dbexpress.cvs.sourceforge.net:/cvsroot/open-dbexpress
echo on
cvs.exe -d:pserver:anonymous@%cvsproj% login
cvs.exe -z3 -d:pserver:anonymous@%cvsproj% co dbxoodbc
=================================================================

cvs.exe for NT you can receive on http://www.cvsnt.org or having installed:
   WinCVS [www.wincvs.org]
 or
   TortoiseCVS [http://www.tortoisecvs.org/download.shtml]

*** WARNINGS ***

1) Following remark has grown old :(
[http://sourceforge.net/docman/display_doc.php?docid=768&group_id=1#firewall]
--
Access to the project CVS servers is provided via SSH (for developers) and the CVS pserver
protocols (for non-developers). SSH uses TCP port 22; CVS pserver uses TCP port 2401. Many
firewalls block outbound traffic on these ports, thus preventing SourceForge.net developers and
users from accessing the project CVS servers. To work around this limitation, we now offer the
ability to connect to the project CVS servers using alternate ports (using different hostnames).
...
To access the project CVS server using anonymous pserver-based access from behind a firewall, you
may connect to port 443 or port 80 of cvs-pserver.sourceforge.net. Traffic to port 443 and to port
80 of cvs-pserver.sourceforge.net is redirected to cvs.sourceforge.net.
--

RUSSIAN CP:Win1251:
===================

Если вы имеете проблемы с драйвером, то вам необходимо сделать следующие шаги:

1) Если это возможно, то включить в ваш проект модуль DbxOpenOdbc.pas.

2) Если отладка с подключенным DbxOpenOdbc.pas не решает вашу проблему, то вам нужно
   включить режим журналирования.

3) Для включения режима журналирования, вам нужно включить в модуле DbxOpenOdbc.pas опции
   {$DEFINE _DEBUG_} и {$DEFINE _TRACE_CALLS_}; и выключить опцию {.$DEFINE _RELEASE_}.
   После этого вам нужно пересобрать ваш проект или OpenODBC драйвер.

4) Отладочные сообщения можно увидеть в Delphi IDE, в окне "Event Log", для случая запуска
   приложения из среды разработки. Иначе нужно воспользоваться специальной программой "Debug View":
   http://www.sysinternals.com/Utilities/DebugView.html
   Подробнее про неё указано в модуле "DbxOpenOdbcTrace.pas".

5) В отладочном окне указаны версии Базы Данных, ODBC драйвера, OpenODBC, параметры соединения.
   Если вы захотите задать вопросы связанные с вашей проблемой, то обязательно укажите эту информацию.
   Это позволит быстрее найти ключ к вашей проблеме.
   Кроме этого в журнале ведётся история вызовов с указанием потока...

Пример загрузки/оюновления проекта из CVS посредством пакетного файла.
Для работы нужно откорректировать путь к cvs.exe.

load_cvs_dbxoodbc.bat
=================================================================
@echo off
set path=C:\Program Files\WinCVS\CVSNT;%path%
rem set path=D:\CygWin\bin;%path%
echo **********************************************
echo  - When prompted for a password for anonymous,
echo    simply press the Enter key.
echo    (Просто нажмите Enter).
echo **********************************************
rem set cvsproj=cvs.sourceforge.net/cvsroot/open-dbexpress
set cvsproj=cvs-pserver.sourceforge.net:80/cvsroot/open-dbexpress
echo on
cvs -d:pserver:anonymous@%cvsproj% login
cvs -z3 -d:pserver:anonymous@%cvsproj% co dbxoodbc
=================================================================

----
.EOF
