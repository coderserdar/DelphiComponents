{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jan 21, 2010
Description:  Demo program showing how to use REST API from Google Search
              and JSON. REST stands for "Representational State Transfer".
              For an introduction to REST, see wikipedia article:
              http://en.wikipedia.org/wiki/Representational_State_Transfer
              JSON stands for "JavaScript Object Notation". For details,
              see http://www.json.org/.
Version:      1.00
EMail:        francois.piette@overbyte.be    francois.piette@rtfm.be
              http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2010 by François PIETTE
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
unit OverbyteIcsRestJsonClientDemo1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids,
  // ICS components for HTTP access
  OverbyteIcsWndControl,
  OverbyteIcsHttpProt,
  OverbyteIcsUrl,
  // OpenSource JASON parser by Henri Gourvest.
  // Download from http://www.progdigy.com/?page_id=6
  SuperObject;

type
  TGoogleSearchJsonClientForm = class(TForm)
    HttpCli1: THttpCli;
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    GoogleGetButton: TButton;
    GSearch: TEdit;
    Splitter1: TSplitter;
    ResultStringGrid: TStringGrid;
    procedure GoogleGetButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Display(Msg: String);
  end;

var
  GoogleSearchJsonClientForm: TGoogleSearchJsonClientForm;

implementation

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TGoogleSearchJsonClientForm.FormCreate(Sender: TObject);
begin
    ResultStringGrid.ColCount    := 2;
    ResultStringGrid.RowCount    := 2;
    ResultStringGrid.Cells[0, 0] := 'visibleUrl';
    ResultStringGrid.Cells[1, 0] := 'unescapedUrl';
    ResultStringGrid.ColWidths[0] := 150;
    ResultStringGrid.ColWidths[1] := 600;
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TGoogleSearchJsonClientForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 2000 then begin
            while DisplayMemo.Lines.Count > 2000 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TGoogleSearchJsonClientForm.GoogleGetButtonClick(Sender: TObject);
const
    Zero : byte = 0;
var
    DataStream   : TMemoryStream;
    O            : ISuperObject;
    Item         : ISuperObject;
    Results      : ISuperObject;
    N            : Integer;
begin
    DisplayMemo.Clear;
    ResultStringGrid.RowCount := 2;
    GoogleGetButton.Enabled   := FALSE;
    DataStream                := TMemoryStream.Create;
    try
        DataStream.Clear;
        Display('Searching');
        // Google documentation is here: http://code.google.com/intl/en/apis/ajaxsearch/documentation/reference.html#_intro_fonje
        HttpCli1.URL := 'http://ajax.googleapis.com/ajax/services/search/web' +
                        '?rsz=large' +
                        '&v=1.0' +
                        '&q='+ UrlEncode(GSearch.Text);
        HttpCli1.RcvdStream := DataStream;
        try
            HttpCli1.Get;
        except
            on E:Exception do
                Display('Failed. ' + E.ClassName + ': ' + E.Message);
        end;
        if HttpCli1.StatusCode <> 200 then begin
            Display('  => Failed ' + IntToStr(HttpCli1.StatusCode) + ' ' +
                    HttpCli1.ReasonPhrase);
        end
        else begin
            O := TSuperObject.ParseStream(DataStream, TRUE);
            if O.I['responseStatus'] <> 200 then
                Display('  => Failed ' +
                        O.S['responseStatus'] + ' ' +
                        O.S['responseDetails'])
            else begin
                Results := O['responseData.results'];
                if Results = nil then
                    Display('No result available')
                else begin
                    N := 1;
                    for Item in Results do begin
                        if ResultStringGrid.RowCount < N then
                            ResultStringGrid.RowCount := N + 1;
                        ResultStringGrid.Cells[0, N] := Item.S['visibleUrl'];
                        ResultStringGrid.Cells[1, N] := Item.S['unescapedUrl'];
                        Inc(N);
                    end;
                end;
            end;
        end;
    finally
        FreeAndNil(DataStream);
        GoogleGetButton.Enabled := TRUE;
        Display('Done');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
