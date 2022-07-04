(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMRCServer;

interface
uses  Classes, NMMCustomAuthChecker, NMMCustomConnectionProcessor, NMMRCConnectionProcessor,
      NMMUserData, NMMP2PServer;

type
 TNMMRCServer= class(TNMMP2PServer)
 protected
   function CreateAuthChecker: TNMMCustomAuthChecker; override;
   function CreateConnectionProcessor(AUserData: TNMMUserData): TNMMCustomConnectionProcessor; override;
 end;

 
implementation

uses NMMBasicAuthChecker, NMMConnectionHandle, IdGlobal;


function TNMMRCServer.CreateAuthChecker: TNMMCustomAuthChecker;
begin
 result:= TNMMBasicAuthChecker.Create;
end;

function TNMMRCServer.CreateConnectionProcessor(AUserData: TNMMUserData): TNMMCustomConnectionProcessor; 
begin
  result:= TRCConnectionProcessor.Create(AUserData.Period);
end;

end.
