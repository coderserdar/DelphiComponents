(*

eICQ: the free ICQ for Microsoft(tm) Windows(tm)

Copyright 2003-2004 eICQ ICQ project,
all portions of this codebase are copyrighted to the people
listed in contributors.txt.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)

program ICQ;

{%ToDo 'ICQ.todo'}

uses
  Forms,
  ICQClient in '..\Component\ICQClient.pas',
  ICQWorks in '..\Component\ICQWorks.pas',
  ICQSock in '..\Component\ICQSock.pas',
  ICQLang in '..\Component\ICQLang.pas',
  ICQDirect2 in '..\Component\ICQDirect2.pas',
  Main in 'Main.pas' {MainForm},
  RecvMsg in 'RecvMsg.pas' {RecvMsgForm},
  SendMsg in 'SendMsg.pas' {SendMsgForm},
  UserInfo in 'UserInfo.pas' {UserInfoForm},
  UserSearch in 'UserSearch.pas' {UserSearchForm},
  PktDump in 'PktDump.pas' {PktDumpForm},
  UserSearchWP in 'UserSearchWP.pas' {UserSearchWPForm},
  AutoAway in 'AutoAway.pas' {AutoAwayForm},
  UserReg in 'UserReg.pas' {UserRegForm},
  UserRegNew in 'UserRegNew.pas' {UserRegNewForm},
  InfoMsgs in 'InfoMsgs.pas' {InfoMsgsForm},
  ReasonFrm in 'ReasonFrm.pas' {ReasonForm},
  UserUnReg in 'UserUnReg.pas' {UserUnRegForm},
  ChangePasswd in 'ChangePasswd.pas' {PasswdForm},
  SendURL in 'SendURL.pas' {SendURLForm},
  SendSMS in 'SendSMS.pas' {SendSMSForm},
  RecvURL in 'RecvURL.pas' {RecvURLForm},
  RecvSMS in 'RecvSMS.pas' {RecvSMSForm},
  AddUser in 'AddUser.pas' {AddUserForm},
  Options in 'Options.pas' {OptionsForm},
  About in 'About.pas' {AboutForm},
  ExeStamp in 'ExeStamp.pas';

{$R *.res}
{$R manifest.res}

begin
  Application.Initialize;
  Application.Title := 'eICQ';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPktDumpForm, PktDumpForm);
  Application.CreateForm(TUserSearchWPForm, UserSearchWPForm);
  Application.Run;
end.
