(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMUserData;

interface

type
 TNMMUserData = class(TObject)
 protected
   FUser: String;
   FPassword: String;
   FPeriod: Integer;
   FConnectionTime: TDateTime;
 public
   function GetUserAndPassword: String;
   procedure SetUserAndPassword(AUserAndPassword: String);
   function WriteToString: String;
   procedure ReadFromString(AUserAndPassword: String);

   property User: String read FUser write FUser;
   property Password: String read FPassword write FPassword;
   property Period: Integer read FPeriod write FPeriod;
   property UserAndPassword: String read GetUserAndPassword
                                    write SetUserAndPassword;
   property UserData: String read WriteToString write ReadFromString;
   property ConnectionTime: TDateTime read FConnectionTime write FConnectionTime;
 end;

implementation

uses Classes, SysUtils, NMMCommon;

function TNMMUserData.GetUserAndPassword: String;
begin
 result:= FUser + '/' + FPassword;
end;

procedure TNMMUserData.SetUserAndPassword(AUserAndPassword: String);
var SepPos: Integer;
begin
 FUser:= '';
 FPassword:= '';
 SepPos:= Pos('/',AUserAndPassword);
 if SepPos>0 then
 begin
   FUser:= Copy( AUserAndPassword, 1, SepPos-1 );
   FPassword:= Copy( AUserAndPassword, SepPos+1,
                     Length(AUserAndPassword)-SepPos );
 end;
end;

function TNMMUserData.WriteToString: String;
begin
 result:= FUser + ';' + FPassword + ';' + IntToStr(FPeriod) + ';';
end;

procedure TNMMUserData.ReadFromString(AUserAndPassword: String);
var LParser: TStringList;
begin
 LParser:= TStringList.Create;
 try
   LParser.Delimiter:= ';';
   LParser.DelimitedText:= AUserAndPassword;
   if LParser.Count>=3 then
   begin
     FUser:= LParser[0];
     FPassword:= LParser[1];
     FPeriod:= DefaultPeriod;
     if Trim(LParser[2])<>'' then
     begin
       try
         FPeriod:= StrToInt(LParser[2]);
       except
       end;
     end;
   end;
 finally
   FreeAndNil(LParser);
 end;
end;

end.
