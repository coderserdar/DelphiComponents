(**
===============================================================================================
Name    : LibXmlComps
===============================================================================================
Project : All Projects processing XML documents
===============================================================================================
Subject : XML parser for Delphi's VCL toolbar
===============================================================================================
Dipl.-Ing. (FH) Stefan Heymann, Softwaresysteme, Tübingen, Germany
===============================================================================================
Date        Author Changes
-----------------------------------------------------------------------------------------------
2000-03-31  HeySt  1.0.0  Start
2000-07-27  HeySt  1.0.1  Added "TAttr" declaration
                          Moved GetNormalize/SetNormalize to PROTECTED section
2001-02-03  HeySt         Changed prototype for the TExternalEvent callback function type
                          so that C++Builder users should get it compiled better.

2001-02-28  HeySt  1.0.2  Introduced the "StopParser" property. When you set this property to
                          TRUE in one of the Parser Events, parsing is stopped and the Execute
                          method returns.
                          Introduced Version numbers
2001-07-10  HeySt  1.0.3  Fixed a bug in TScannerXmlParser.DtdElementFound so that the
                          OnAttList event is correctly fired
2001-07-11  HeySt  1.1.0  Derived from the new TCustomXmlScanner class from LibXmlParser
2005-07-07  HeySt  1.1.1  Published new TranslateCharacter event property
*)

UNIT LibXmlComps;

INTERFACE

USES
  Classes,
  LibXmlParser;

TYPE
  TXmlScanner = CLASS (TCustomXmlScanner)
                PUBLIC
                  PROPERTY XmlParser;
                  PROPERTY StopParser;
                PUBLISHED
                  PROPERTY Filename;
                  PROPERTY Normalize;
                  PROPERTY OnXmlProlog;
                  PROPERTY OnComment;
                  PROPERTY OnPI;
                  PROPERTY OnDtdRead;
                  PROPERTY OnStartTag;
                  PROPERTY OnEmptyTag;
                  PROPERTY OnEndTag;
                  PROPERTY OnContent;
                  PROPERTY OnCData;
                  PROPERTY OnElement;
                  PROPERTY OnAttList;
                  PROPERTY OnEntity;
                  PROPERTY OnNotation;
                  PROPERTY OnDtdError;
                  PROPERTY OnLoadExternal;
                  PROPERTY OnTranslateEncoding;
                  PROPERTY OnTranslateCharacter;
                END;

  // The "Easy" XML Scanner leaves out events and properties which you are unlikely to use
  // for "normal" XML files.
  // CDATA sections trigger "OnContent" events
  TEasyXmlScanner = CLASS (TCustomXmlScanner)
                    PROTECTED
                      PROCEDURE WhenCData (Content : string); OVERRIDE;
                    PUBLIC
                      PROPERTY XmlParser;
                      PROPERTY StopParser;
                    PUBLISHED
                      PROPERTY Filename;
                      PROPERTY Normalize;
                      PROPERTY OnComment;
                      PROPERTY OnPI;
                      PROPERTY OnStartTag;
                      PROPERTY OnEmptyTag;
                      PROPERTY OnEndTag;
                      PROPERTY OnContent;
                      PROPERTY OnLoadExternal;
                      PROPERTY OnTranslateEncoding;
                    END;

PROCEDURE Register;

(*
===============================================================================================
IMPLEMENTATION
===============================================================================================
*)

IMPLEMENTATION


PROCEDURE Register;
BEGIN
  RegisterComponents ('XML', [TXmlScanner, TEasyXmlScanner]);
END;


(*
===============================================================================================
TEasyXmlScanner
===============================================================================================
*)

PROCEDURE TEasyXmlScanner.WhenCData (Content : string);
BEGIN
  INHERITED WhenContent (Content);
END;


(*
===============================================================================================
INITIALIZATION
===============================================================================================
*)

END.

