{*******************************************************}
{                                                       }
{       Delphi Visual Component Library  (WebPack)      }
{       Pluggable Namespace Handler Component           }
{                                                       }
{       Copyright (c) 2000, Semyon A. Chertkov          }
{                                                       }
{     Written by:                                       }
{       Semyon A. Chertkov                              }
{       e-mail:  chertkov@chat.ru                       }
{       WWW: www.chat.ru/~chertkov                      }
{                                                       }
{*******************************************************}

unit pkgdsgn;

interface
uses Classes;

procedure Register;

implementation
uses DsgnIntf, typinfo, webprotocol, ie5;

type
  TWPCompEditor = class(TComponentEditor)
  private
    procedure EnumProc(Prop: TPropertyEditor);
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
  end;

  TMimeTypeProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: string); override;
  end;

procedure TWPCompEditor.EnumProc(Prop: TPropertyEditor);
begin
  if Prop.GetName = 'Items' then
    Prop.Edit;
end;

procedure TWPCompEditor.Edit;
var
  SelList: TDesignerSelectionList;
begin
  SelList := TDesignerSelectionList.Create;
  SelList.Add(Component);
  GetComponentProperties(SelList, [tkClass], Designer, EnumProc);
  SelList.Free;
end;

procedure TWPCompEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TWPCompEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Item Editor';
end;

function TWPCompEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

// TMimeTypeProperty

function TMimeTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paAutoUpdate];
end;

procedure TMimeTypeProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('text/html');
  Proc('text/xml');
  Proc('text/css');
  Proc('text/plain');
  Proc('text/richtext');
  Proc('audio/x-aiff');
  Proc('audio/basic');
  Proc('audio/wav');
  Proc('image/gif');
  Proc('image/jpeg');
  Proc('image/pjpeg');
  Proc('image/tiff');
  Proc('image/x-png');
  Proc('image/x-xbitmap');
  Proc('image/bmp');
  Proc('image/x-jg');
  Proc('image/x-emf');
  Proc('image/x-wmf');
  Proc('video/avi');
  Proc('video/mpeg');
  Proc('application/postscript');
  Proc('application/base64');
  Proc('application/macbinhex40');
  Proc('application/pdf');
  Proc('application/x-compressed');
  Proc('application/x-zip-compressed');
  Proc('application/x-gzip-compressed');
  Proc('application/java');
  Proc('application/x-msdownload');
end;

function TMimeTypeProperty.GetValue: String;
begin
  Result := GetStrValue;
end;

procedure TMimeTypeProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
  Designer.Modified;
end;

procedure Register;
begin
  RegisterComponents('WebPack', [TWebProvider, TWebBrowserControl]);
  RegisterComponentEditor(TWebProvider, TWPCompEditor);
  RegisterPropertyEditor(TypeInfo(String), TWebDispatcher,
    'MimeType', TMimeTypeProperty);
end;

end.
