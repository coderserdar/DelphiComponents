(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMReceiverList;

interface

uses Classes;


type
 TNMMReceiverList = class(TObject)
 protected
   FSendToAll: Boolean;
   FUserList: TStringList;
 public
   constructor Create;
   destructor Destroy; override;
   function CheckUser(AUserName: String): Boolean;
 published
   property SendToAll: Boolean read FSendToAll write FSendToAll default true;
   property UserList: TStringList read FUserList write FUserList;
 end;


implementation


constructor TNMMReceiverList.Create;
begin
 inherited;
 FUserList:= TStringList.Create;
 FSendToAll:= true;
end;

destructor TNMMReceiverList.Destroy;
begin
 FUserList.Free;
 inherited;
end;

function TNMMReceiverList.CheckUser(AUserName: String): Boolean;
begin
 if FSendToAll then
 begin
   result:= true;
   exit;
 end
 else
 begin
   result:= FUserList.IndexOf(AUserName) <> -1;
 end;
end;


end.

