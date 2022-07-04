(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomAuthChecker;

interface
uses Classes, SyncObjs, NMMUserData;

type
 TNMMUserInfo = class(TNMMUserData)
 protected
   FExpiresAt: TDateTime;
 public
   constructor Create(AUser,APassword: String; AExpiresAt: TDateTime);
   property ExpiresAt: TDateTime read FExpiresAt write FExpiresAt;
 end;

 TAuthenticationEvent= procedure(Sender: TObject; User, Password: string;
                       var Accept: Boolean) of object;

 TNMMCustomAuthChecker = class(TObject)
 protected
   FOnAuthentication: TAuthenticationEvent;
 public
   function AcceptClient(AUserData: TNMMUserData;
                         var ARefuseReason: String): Boolean; virtual; abstract;
   property OnAuthentication: TAuthenticationEvent
            read FOnAuthentication write FOnAuthentication;
 end;


implementation
uses NMMCommon, NMMServerGlobals, SysUtils;

{TNMMUserInfo}
constructor TNMMUserInfo.Create(AUser,APassword: String; AExpiresAt: TDateTime);
begin
 inherited Create;
 FUser:= AUser;
 FPassword:= APassword;
 FExpiresAt:= AExpiresAt;
end;


end.
