{*********************************************************}
{*                                                       *}
{*  Common Archiver Kit Experinment 2 Beta Source.       *}
{*     Copyright (c) 2003 Joseph Leung Yat Chun          *}
{*                 All rights reserved.                  *}
{*                                                       *}
{*********************************************************}
The source is distribute under MPL/LGPL license, the license
is included in License.txt.

You will have to install a number of components in order to 
install CAKE2, most of them is included in a package which 
can be found in torry.net, including

Microsoft CAB SDK Header Files For Delphi v.1.1 and Cabinet Components v.1.1 beta 
Indy 8.0 (indy8_00_23.zip)
Resource (rs26src.zip for Delphi 5, rs261src.zip for Delphi 6)
Zip Repair (ZipRepair.pas)

Other component please download in these web sites
Delphi Zip 1.70Beta (http://www.users.bigpond.com/russellpeters/delphizip.html)
Cmarc (http://www.ceres.dti.ne.jp/~norg1964/cmarc/cmarc.html)
 *Modification is required for 7z support.
RtdunAce, RtdunRar3, RtdunRar (http://members.home.nl/rtimmermans/)
ActiveAce (http://www.winace.com/ftp/dev/activeace.exe)
SQX Archiver (http://www.sqx-archiver.org/)

This is not required yet, will be used in future.
Crypto Zip (zg14_compliance_kit.exe http://www.zipgenius.it)
 
Please note that if you dont need all listed archvie support you can remove them 
from uses list and AddArchiver in line 190, then you dont have to install the component.

Some archive type support require DLL, if you cant find it you can use
the one included in Quick Zip.

After you installed all those component, Add Cakdir2.pas, 
CakListview2.pas, Caktreeview2.pas, CakTreeList2.pas and
CakTreeCombobox2.pas to install.

How to use?
1.Drop Cakdir2, CakTreeview2 on a form. (or other Cak components.)
2.Set CakTreeview2.Cakdir2 to Cakdir2
3.To Open, use Cakdir2.Archivename := 'c:\yourname.zip';
4.To Extract update Cakdir2.ExtractOptions. then Cakdir2.Extract

All question please ask in CAKE forum
http://www.quickzip.org/forums/viewforum.php?f=7

