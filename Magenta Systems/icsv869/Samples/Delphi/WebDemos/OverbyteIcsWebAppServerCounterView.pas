{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 11, 2009
Description:  This source is part of WebAppServer demo application.
              The purpose is to show counter values recorded by the
              counter (See WebAppServerCounter.pas). Actually beside showing
              counter values, it shows how to use AJAX to update a web page
              without reloading the entire page. It also shows how to build
              a dynamic "array of const" at runtime.
Version:      1.00
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWebAppServerCounterView;

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    Classes, SysUtils, OverbyteIcsIniFiles, Variants,
    OverbyteIcsHttpSrv,
    OverbyteIcsHttpAppServer,
    OverbyteIcsWebAppServerDataModule,
    OverbyteIcsWebAppServerHttpHandlerBase,
    OverbyteIcsWebAppServerUrlDefs,
    OverbyteIcsWebAppServerSessionData;

type
    TUrlHandlerCounterViewHtml = class(TUrlHandlerBase)
    private
        FNames            : TStringList;
        FCounters         : TStringList;
        FCountersSelected : TStringList;
        FTags             : TArrayOfConstBuilder;
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure Execute; override;
        procedure GetRowData(Sender: TObject; const TableName: String;
                             Row: Integer; TagData: TStringIndex;
                             var More: Boolean; UserData: TObject);
    end;

    TUrlHandlerAjaxFetchCounter = class(TUrlHandlerBase)
    public
        procedure Execute; override;
    end;

implementation

const
    PleaseSelect = 'Please select';

constructor TUrlHandlerCounterViewHtml.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FCounters         := TStringList.Create;
    FNames            := TStringList.Create;
    FCountersSelected := TStringList.Create;
    FTags             := TArrayOfConstBuilder.Create;
end;

destructor TUrlHandlerCounterViewHtml.Destroy;
begin
    FreeAndNil(FCounters);
    FreeAndNil(FNames);
    FreeAndNil(FCountersSelected);
    FreeAndNil(FTags);
    inherited;
end;

procedure TUrlHandlerCounterViewHtml.Execute;
var
    I           : Integer;
    CounterName : String;
begin
    if NotLogged then
        Exit;

    ExtractURLEncodedParamList(Params, FNames);

    FTags.Add('LOGIN',     UrlLogin);
    FTags.Add('COUNTER',   UrlCounter);
    FTags.Add('USERCODE',  SessionData.UserCode);
    FTags.Add('LOGINTIME', DateToStr(SessionData.LogonTime));
    for I := 0 to FNames.Count - 1 do begin
        ExtractURLEncodedValue(Params, FNames[I], CounterName);
        FCountersSelected.Add(CounterName);
        FTags.Add('CounterValue' + IntToStr(I + 1),
                  WebAppSrvDataModule.CounterValue(CounterName, 0));
    end;

    OnGetRowData := GetRowData;
    AnswerPage('', NO_CACHE, '/CounterView.html', nil, FTags.Value);
    OnGetRowData := nil;
    Finish;
end;

procedure TUrlHandlerCounterViewHtml.GetRowData(
    Sender          : TObject;
    const TableName : String;
    Row             : Integer;
    TagData         : TStringIndex;
    var More        : Boolean;
    UserData        : TObject);
var
    IniFile : TIcsIniFile;
    NoTable : Integer;
begin
    NoTable := StrToIntDef(TableName, 0);
    if Row = 1 then begin
        IniFile := TIcsIniFile.Create(WebAppSrvDataModule.CounterFileName);
        try
            FCounters.Clear;
            IniFile.ReadSection(CounterSection, FCounters);
            FCounters.Sort;
        finally
            FreeAndNil(IniFile);
        end;
        TagData.Add('CounterItem', PleaseSelect);
        if FCountersSelected.Count = 0 then
            TagData.Add('CounterSelected', 'SELECTED');
        More := TRUE;
        Exit;
    end;

    More := Row <= FCounters.Count;
    if More then begin
        TagData.Add('CounterItem',     FCounters[Row - 2]);
        if (NoTable <= FCountersSelected.Count) and
           SameText(FCountersSelected[NoTable - 1], FCounters[Row - 2])  then
            TagData.Add('CounterSelected', 'SELECTED');
    end;
end;

procedure TUrlHandlerAjaxFetchCounter.Execute;
var
    CounterName  : String;
    CounterValue : Integer;
begin
    if not ValidateSession then begin
        AnswerString('500', 'text/plain', NO_CACHE, 'Invalid login');
        Finish;
    end;

    ExtractURLEncodedValue(Params, 'counter', CounterName);
    CounterName := Trim(CounterName);

    if (CounterName = PleaseSelect) or (CounterName = '') then
        CounterValue := 0
    else
        CounterValue := WebAppSrvDataModule.CounterValue(CounterName, 0);

    AnswerString('', 'text/plain', NO_CACHE, IntToStr(CounterValue));
    Finish;
end;

end.
