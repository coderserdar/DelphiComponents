(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMBasicAuthChecker;

interface
uses Classes, SyncObjs, NMMCustomAuthChecker, NMMUserData;

type
 TNMMBasicAuthChecker = class(TNMMCustomAuthChecker)
 public
   constructor Create;
   destructor Destroy; override;
   function AcceptClient(AUserData: TNMMUserData; var ARefuseReason: String): Boolean; override;
 end;


implementation
uses Forms, NMMCommon, NMMServerGlobals, SysUtils;

{TNMMBasicAuthChecker}
constructor TNMMBasicAuthChecker.Create;
begin
 inherited Create;
end;

destructor TNMMBasicAuthChecker.Destroy;
begin
 inherited;
end;

function TNMMBasicAuthChecker.AcceptClient(AUserData: TNMMUserData;
                                       var ARefuseReason: String): Boolean;
begin
 result:= true;
 ARefuseReason:= '';

 if Assigned(FOnAuthentication) then
    FOnAuthentication(self,AUserData.User,AUserData.Password,result)
 else
    result:= true;
end;

end.
