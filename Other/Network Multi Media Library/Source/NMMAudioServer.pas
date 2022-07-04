(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMAudioServer;

interface
uses  Classes, NMMCustomConnectionProcessor, NMMCustomAuthChecker, NMMAudioConnectionProcessor,
      NMMUserData, NMMP2PServer;

type
 TNMMAudioServer= class(TNMMP2PServer)
 protected
   function CreateAuthChecker: TNMMCustomAuthChecker; override;
   function CreateConnectionProcessor(AUserData: TNMMUserData): TNMMCustomConnectionProcessor; override;
 end;

 
implementation

uses NMMBasicAuthChecker, NMMConnectionHandle, IdGlobal;


function TNMMAudioServer.CreateAuthChecker: TNMMCustomAuthChecker;
begin
 result:= TNMMBasicAuthChecker.Create(FUser,FPassword);
end;

function TNMMAudioServer.CreateConnectionProcessor(AUserData: TNMMUserData): TNMMCustomConnectionProcessor; 
begin
  result:= TNMMAudioConnectionProcessor.Create(AUserData.Period);
end;

end.
