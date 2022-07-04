{                                                                              }
{                             XML Components v3.00                             }
{                                                                              }
{               This unit is copyright © 2004 by David J Butler                }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                Its original file name is cXMLComponents.pas                  }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   01/04/2004  3.00  Initial version.                                         }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cXMLComponents;

interface

uses
  { Delphi }
  Classes,

  { XML }
  cXMLDocument,
  cXMLParser;



{                                                                              }
{ TXMLParserComponent                                                          }
{                                                                              }
type
  TXMLParserComponent = class;
  TXMLParserComponentObjectEvent = procedure (Sender: TXMLParserComponent;
      Obj: AxmlType) of object;
  TXMLParserComponent = class(TComponent)
  private
    FText       : String;
    FFileName   : String;
    FEncoding   : String;
    FOnTag      : TXMLParserComponentObjectEvent;
    FOnElement  : TXMLParserComponentObjectEvent;
    FOnPI       : TXMLParserComponentObjectEvent;
    FOnComment  : TXMLParserComponentObjectEvent;
    FOnDocument : TXMLParserComponentObjectEvent;
    FParser     : TxmlParser;
    FDocument   : TxmlDocument;

    procedure OnParserTag(Sender: TxmlParser; Obj: AxmlType);
    procedure OnParserElement(Sender: TxmlParser; Obj: AxmlType);
    procedure OnParserPI(Sender: TxmlParser; Obj: AxmlType);
    procedure OnParserComment(Sender: TxmlParser; Obj: AxmlType);

  public
    destructor Destroy; override;

    property  Text: String read FText write FText;
    property  FileName: String read FFileName write FFileName;
    property  Encoding: String read FEncoding write FEncoding;

    property  OnTag: TXMLParserComponentObjectEvent read FOnTag write FOnTag;
    property  OnElement: TXMLParserComponentObjectEvent read FOnElement write FOnElement;
    property  OnPI: TXMLParserComponentObjectEvent read FOnPI write FOnPI;
    property  OnComment: TXMLParserComponentObjectEvent read FOnComment write FOnComment;
    property  OnDocument: TXMLParserComponentObjectEvent read FOnDocument write FOnDocument;

    function  Parse: TxmlDocument;
    property  Document: TxmlDocument read FDocument;
    function  ReleaseDocument: TxmlDocument;
  end;

  TfndXMLParser = class(TXMLParserComponent)
  published
    property  Text;
    property  FileName;
    property  Encoding;
    property  OnTag;
    property  OnElement;
    property  OnPI;
    property  OnComment;
    property  OnDocument;
  end;



{                                                                              }
{ Component Register                                                           }
{                                                                              }
procedure Register;



implementation

uses
  { Delphi }
  SysUtils;



{                                                                              }
{ TXMLParserComponent                                                          }
{                                                                              }
destructor TXMLParserComponent.Destroy;
begin
  FreeAndNil(FDocument);
  FreeAndNil(FParser);
  inherited Destroy;
end;

procedure TXMLParserComponent.OnParserTag(Sender: TxmlParser; Obj: AxmlType);
begin
  if Assigned(FOnTag) then
    FOnTag(self, Obj);
end;

procedure TXMLParserComponent.OnParserElement(Sender: TxmlParser; Obj: AxmlType);
begin
  if Assigned(FOnElement) then
    FOnElement(self, Obj);
end;

procedure TXMLParserComponent.OnParserPI(Sender: TxmlParser; Obj: AxmlType);
begin
  if Assigned(FOnPI) then
    FOnPI(self, Obj);
end;

procedure TXMLParserComponent.OnParserComment(Sender: TxmlParser; Obj: AxmlType);
begin
  if Assigned(FOnComment) then
    FOnComment(self, Obj);
end;

function TXMLParserComponent.Parse: TxmlDocument;
begin
  if not Assigned(FParser) then
    begin
      FParser := TxmlParser.Create;
      FParser.Options := [];
      FParser.OnTag := OnParserTag;
      FParser.OnElement := OnParserElement;
      FParser.OnPI := OnParserPI;
      FParser.OnComment := OnParserComment;
    end;
  FParser.Encoding := FEncoding;
  if FFileName <> '' then
    FParser.SetFileName(FFileName) else
  if FText <> '' then
    FParser.SetString(FText)
  else
    raise ExmlParser.Create('No XML text');
  FreeAndNil(FDocument);
  FDocument := FParser.ExtractDocument;
  if Assigned(FOnDocument) then
    FOnDocument(self, FDocument);
  Result := FDocument;
end;

function TXMLParserComponent.ReleaseDocument: TxmlDocument;
begin
  Result := FDocument;
  FDocument := nil;
end;



{                                                                              }
{ Component Register                                                           }
{                                                                              }
procedure Register;
begin
  RegisterComponents('Fundamentals', [TfndXMLParser]);
end;



end.

