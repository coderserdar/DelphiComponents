{*********************************************************}
{*                VPEXCEPTION.PAS 1.03                   *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpException;
  {Vp exceptions}

interface

uses
  Windows, Classes, SysUtils, VpConst, VpSR;

type
  {*** Base Vp exeption class ***}
  EVpException = class(Exception);

  EVpCodedException = class(Exception)
  public
    ErrorCode : LongInt;
  end;

  {*** Data Classes ***}
  EExclusiveEventConflict = class(EVpException)
  public
    constructor Create;
  end;

  EBackwardTimesError = class(EVpException)
  public
    constructor Create;
  end;

  EFailToCreateTask = class(EVpException)
  public
    constructor Create;
  end;

  EFailToCreateContact = class(EVpException)
  public
    constructor Create;
  end;

  EFailToCreateEvent = class(EVpException)
  public
    constructor Create;
  end;

  EFailToCreateResource = class(EVpException)
  public
    constructor Create;
  end;

  EDuplicateResource = class (EVpException)
    public
      constructor Create;
  end;

  EInvalidTable = class (EVpException)
    public
      constructor Create;
  end;

  {*** Timer Pool ***}
  ENoTimersAvailable = class(EVpException)
  public
    constructor Create;
  end;

  {*** DB Errors ***}
  EDBPostError = class(EVpException)
  public
    constructor Create;
  end;

  ETimerPoolError = class(EVpException);

  EInvalidTriggerHandle = class(ETimerPoolError)
  public
    constructor Create;
  end;

  EVpCanvasError = class (EVpException);

  EVpPrintFormatError = class (EVpException);

  EVpPrintPreviewError = class (EVpException);

  EVpDateException = class (EVpException);

  EVpContactEditError = class (EVpException);

  EVpDateEditError = class (EVpException);

  EVpCalendarError = class (EVpException);

  EVpPrintFormatEditorError = class (EVpException);

  EVpNoLocalizationFile = class (EVpException);                          

implementation

constructor ENoTimersAvailable.Create;
begin
  inherited Create(RSNoTimersAvail);
end;

constructor EDBPostError.Create;
begin
  inherited Create(RSDBPostError);
end;

constructor EInvalidTriggerHandle.Create;
begin
  inherited Create(RSBadTriggerHandle);
end;

constructor EExclusiveEventConflict.Create;
begin
  inherited Create(RSExclusiveEventConflict);
end;

constructor EBackwardTimesError.Create;
begin
  inherited Create(RSBackwardTimesError);
end;

constructor EFailToCreateTask.Create;
begin
  inherited Create(RSFailToCreateTask);
end;

constructor EFailToCreateContact.Create;
begin
  inherited Create(RSFailToCreateContact);
end;

constructor EFailToCreateEvent.Create;
begin
  inherited Create(RSFailToCreateEvent);
end;

constructor EFailToCreateResource.Create;
begin
  inherited Create(RSFailToCreateResource);
end;

constructor EDuplicateResource.Create;
begin
  inherited Create (RSDuplicateResource);
end;

constructor EInvalidTable.Create;
begin
  inherited Create (RSInvalidTableSpecified);
end;

end.
