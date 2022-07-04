Hi everybody
KADaoInfo is a small FREE component for using Microsoft's msldbusr.dll to get information about an Access Database without opening it
Also it uses a DOCUMENTED techiques to get usernames of all logged to database users
I thing it will be very usefull in some cases.

Instalation:
-------------
First go to Microsoft Site and download jetutils.exe Package from the following URL:
   http://download.microsoft.com/download/access97/utility1/1/WIN98/EN-US/JETUTILS.EXE
You also may review the folowing page for more information about Jet Utils:
   http://support.microsoft.com/support/kb/articles/Q176/6/70.ASP
Then install all components from the package JETUTILS.EXE
Copy msldbusr.dll in your Windows folder

Then: 
1. Copy KADaoInfo.pas and KDaoDatabase.dcr in directory where KADao is Installed
2. Open KADao.dpk in IDE
3. Press Add button and select KADaoInfo.pas
4. Press Compile button
5. Now KADaoInfo is in your "KA Dao" palette

Properties:
-------------
Property Database           : String - Set to full pathname of the Database you want to get info
Property DaoInfoDll         : String - By default it points to 'msldbusr.dll'
Property DatabaseVersion    : Integer - Returns the version of the selected Database
Property NumberOfUsers      : Integer - Returns the total number of users logged to database
Property LoggedUsers        : TStringList  - Returns the name of machines for all users connected to Database since it is open in share mode
Property LoggedUsersEx      : TStringList  - Returns the name of machines/usernames for all users connected to Database since it is open in share mode
Property LoggedNowUsers     : TStringList  - Returns the name of machines for currently logged users
Property ErrorUsers         : TStringList  - Returns the name of machines for users caused crashing of Database
Property LastError          : Integer - Returns the last error when all properties are retrieved
Property LastErrorText      : String  - Returns the last error text representation when all properties are retrieved
Property Active             : Boolean - Set to true to obtain info

Methods: (Doing the same without setting Active to true and retrieving only one property)
----------------
Function KAGetDatabaseVersion(LibraryName,DatabasePath:String):Integer; {Fills DatabaseVersion property}
Function KAGetNumberOfUsers(LibraryName,DatabasePath:String):Integer;   {Fills NumberOfUsers property}
Function KAGetLoggedUsers(LibraryName,DatabasePath:String):Integer;     {Fills LoggedUsers property}
Function KAGetLoggedInfo(DatabasePath:String):Boolean;                  {Fills LoggedUsersEx property} 
Function KAGetLoggedNowUsers(LibraryName,DatabasePath:String):Integer;  {Fills LoggedNowUsers property}
Function KAGetErrorUsers(LibraryName,DatabasePath:String):Integer;      {Fills ErrorUsers property}

Events: (All events are triggered after a call to one of six available functions so you can get LastError and LastErrorText for each)
-------
AfterGetDatabaseVersion
AfterGetNumberOfUsers
AfterGetLoggedUsers
AfterGetLoggedUsersEx
AfterGetLoggedNowUsers
AfterGetErrorUsers


IMPORTANT:
--------------
Since getting all information is based on LDB file created when an mdb is open in shared mode 
You must call frequently this component to obtain realtime information about a database
I am sorry but i cannot support this component by e-mail
Only bugfixes/patches e-mails will be processed
Kiril Antonov
