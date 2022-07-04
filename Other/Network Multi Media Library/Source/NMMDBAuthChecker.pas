(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMDBAuthChecker;

interface
uses Classes, SyncObjs, NMMUserData, CustomSecChecker;

type
 TNMMDBAuthChecker = class(TNMMCustomAuthChecker)
 protected
   FUsersInfoFileName: String;
   FFileUpdateTime: TDateTime;
   FUsersInfo: TThreadList;
   FCS: TCriticalSection;
 public
   constructor Create(AUsersInfoFileName: String);
   destructor Destroy; override;
   function AcceptClient(AUserData: TNMMUserData;
                         var ARefuseReason: String): Boolean; override;
   property UsersInfoFileName: String read FUsersInfoFileName
                                     write FUsersInfoFileName;
 end;


implementation
uses DBISAMTb, Forms, NMMCommon, NMMServerGlobals, SysUtils;



{TNMMDBAuthChecker}
constructor TNMMDBAuthChecker.Create(AUsersInfoFileName: String);
begin
 inherited Create;
 FUsersInfo:= TThreadList.Create;
 FUsersInfoFileName:= AUsersInfoFileName;
 FFileUpdateTime:= 0;
 FCS:= TCriticalSection.Create;
end;

destructor TNMMDBAuthChecker.Destroy;
var LLockedUsersInfo: TList;
    i: Integer;
begin
 LLockedUsersInfo:= FUsersInfo.LockList;
 try
   for i:=0 to LLockedUsersInfo.Count-1 do
     TNMMUserInfo(LLockedUsersInfo[i]).Free;
   LLockedUsersInfo.Clear;
 finally
   FUsersInfo.UnlockList;
 end;
 FreeAndNil(FCS);
 inherited;
end;

function TNMMDBAuthChecker.AcceptClient(AUserData: TNMMUserData;
                                       var ARefuseReason: String): Boolean;
  procedure LoadUsersInfo;
  var LTmpUsersInfo, LLockedUsersInfo: TList;
      i: Integer;
      LTable: TDBISAMTable;
      LSession: TDBISAMSession;
  begin // LoadUsersInfo
    LTmpUsersInfo:= TList.Create;
    LTable:= TDBISAMTable.Create(Application);
    LSession:= TDBISAMSession.Create(Application);
    LSession.Name:= 'LSession';
    
    if GetCurrentSessionName<>'' then
    begin
      LTable.SessionName:= GetCurrentSessionName;
    end
    else
    begin
      LSession.SessionName:= GetNewSessionName;
      LSession.Active:= true;
      LTable.SessionName:= LSession.SessionName;
    end;

    try
      LTable.TableName:= FUsersInfoFileName;
      LSession.Active:= true;
      LTable.Open;
      while not LTable.Eof do
      begin
        LTmpUsersInfo.Add(
           TNMMUserInfo.Create( LTable.FieldByName('UserName').AsString,
                             LTable.FieldByName('Password').AsString,
                             LTable.FieldByName('ExpiresAt').AsDateTime )
           );
        LTable.Next;
      end;
      LTable.Close;
      LSession.Active:= false;
      
      LLockedUsersInfo:= FUsersInfo.LockList;
      try
        for i:=0 to LLockedUsersInfo.Count-1 do
          TNMMUserInfo(LLockedUsersInfo[i]).Free;
        LLockedUsersInfo.Clear;

        for i:=0 to LTmpUsersInfo.Count-1 do
          LLockedUsersInfo.Add(LTmpUsersInfo[i]);
      finally
        FUsersInfo.UnlockList;
      end;
    finally
      LTmpUsersInfo.Free;
      LTable.Free;
      LSession.Active:= false;
      LSession.Free;
    end;
  end;

var LFileUpdateTime: TDateTime;
    LLockedUsersInfo: TList;
    I: Integer;
// AcceptClient
begin
 result:= false;
 ARefuseReason:= cmdInvalidUser;
 FCS.Enter;
 try
   LFileUpdateTime:= FileDateToDateTime(FileAge(FUsersInfoFileName));
   if FFileUpdateTime < LFileUpdateTime then
   begin
     LoadUsersInfo;
     FFileUpdateTime:= LFileUpdateTime;
   end;

   LLockedUsersInfo:= FUsersInfo.LockList;
   try
     for i:=0 to LLockedUsersInfo.Count-1 do
       if UpperCase(TNMMUserInfo(LLockedUsersInfo[i]).User)=
          UpperCase(AUserData.User) then
       begin
         if TNMMUserInfo(LLockedUsersInfo[i]).Password<>AUserData.Password then
         begin
           result:= false;
           ARefuseReason:= cmdInvalidPassword;
           break;
         end;
         if TNMMUserInfo(LLockedUsersInfo[i]).ExpiresAt<=Now then
         begin
           result:= false;
           ARefuseReason:= cmdUserLoginExpired;
           break;
         end;
         result:= true;
         ARefuseReason:= '';
         break;
       end;
   finally
     FUsersInfo.UnlockList;
   end;
 finally
   FCS.Leave;
 end;
end;

end.
