unit Unit2;

//------------------------------------------------------------------------------
//
// 20102009, ari pikivirta
//  * changed edit to read only to avoid problems on enabling monitoring while
//    typing in new folder to monitor
//
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_files, API_base, API_dirmonitor, StdCtrls, API_memo, ExtCtrls,
  API_checkbox;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    API_memo1: TAPI_memo;
    API_DirMonitor1: TAPI_DirMonitor;
    API_files1: TAPI_files;
    API_checkbox1: TAPI_checkbox;
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure API_DirMonitor1Change(sender: TObject;
      var ContinueMonitoring: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    AFilesList: TStringlist;
    procedure Log(Const AText: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  api_strings;

//------------------------------------------------------------------------------
procedure TForm1.Log(Const AText: String);
begin
  api_memo1.lines.add(datetimetostr(now)+'> '+AText);
  api_memo1.scrolltobottom;
end;

//------------------------------------------------------------------------------
procedure TForm1.API_DirMonitor1Change(sender: TObject; var ContinueMonitoring: Boolean);
var
  i, j, k: integer;
  ATempList: Tstringlist;
  AFilename1, AFilename2: String;
  AFiletime1, AFiletime2: TDatetime;
  AFileSize1, AFileSize2: int64;
  AFileAttr1, AFileAttr2: Integer;
begin
  ContinueMonitoring:= TRUE;
  //
  ATemplist:= Tstringlist.create;
  try
    ATemplist.Clear;
    api_files.FindFiles(ATemplist, api_dirmonitor1.Folder, '*.*', '', api_dirmonitor1.Subs, TRUE, TRUE, '|');
    //
    // look for changed files
    i:= 0;
    while i<AFilesList.count do
    begin
      api_files.ParseListDetails(AFilesList[i], AFilename1, AFiletime1, AFilesize1, AFileAttr1, '|');
      k:= -1;
      for j:=0 to ATempList.count-1 do
      begin
        api_files.ParseListDetails(ATempList[j], AFilename2, AFiletime2, AFilesize2, AFileAttr2, '|');
        if (AFilename2=AFilename1) then // filename was found!
        begin
          if (AFiletime1<>AFiletime2) then Log('File '+AFilename1+' time was changed to '+datetimetostr(AFiletime2)+' from '+datetimetostr(Afiletime1)+'.');
          if (AFilesize1<>AFilesize2) then Log('File '+AFilename1+' size was changed to '+bytestostr(AFileSize2)+' from '+bytestostr(AFilesize1)+'.');
          k:= j;
          break;
        end;
      end;
      if (k<0) then
      begin
        Log('File '+AFilename1+' was removed.');
        AFilesList.delete(i);
      end else
        i:= i + 1;
    end;
    //
    // look for added files
    if ATempList.count>AFilesList.count then
      for j:=0 to ATempList.count-1 do
      begin
        api_files.ParseListDetails(ATempList[j], AFilename2, AFiletime2, AFilesize2, AFileAttr2, '|');
        k:= -1;
        for i:=0 to AFilesList.count-1 do
        begin
          api_files.ParseListDetails(AFilesList[i], AFilename1, AFiletime1, AFilesize1, AFileAttr1, '|');
          if (AFilename2=AFilename1) then // filename was found!
          begin
            k:= i;
            break;
          end;
        end;
        if (k<0) then
        begin
          AFilesList.add(ATempList[j]);
          Log('New file '+ATempList[j]+' created.');
        end;        
      end;
  finally
    ATemplist.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
var
  s: string;
begin
  if edit1.text<>'' then s:= edit1.text else s:= extractfiledir(application.exename);
  s:= api_files.BrowseFolderDialog('Select Folder', s, FALSE);
  if s<>'' then edit1.text:= s;
end;

//------------------------------------------------------------------------------
procedure TForm1.Edit1Change(Sender: TObject);
begin
  if directoryexists(edit1.text) then
  begin
    api_dirmonitor1.Active:= FALSE; // disable temporarily if was enabled
    api_dirmonitor1.Folder:= edit1.text;
    api_dirmonitor1.Subs:= api_checkbox1.checked;
    AFilesList.Clear;
    api_files.FindFiles(AFilesList, api_dirmonitor1.Folder, '*.*', '', api_dirmonitor1.Subs, TRUE, TRUE, '|');
    api_dirmonitor1.Active:= TRUE;
  end else
    api_dirmonitor1.Active:= FALSE;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  AFilesList:= Tstringlist.create;
  AFilesList.Clear;
end;

//------------------------------------------------------------------------------
procedure TForm1.FormDestroy(Sender: TObject);
begin
  AFilesList.Free;
end;

end.
