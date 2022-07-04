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

unit pkgmsg;

interface
uses Messages;

const
  CM_INVOKE_URL = WM_APP + 101;
  SZ_BUFFER = 1024;

resourcestring
   SDispatchItemName = 'Name';
   SDispatchItemURI = 'URI';
   SDispatchItemEnabled = 'Enabled';
   SDispatchItemDefault = 'Default';
   SDispatchItemXSLT = 'XSLT';
   SDispatchItemTemplate = 'Template';
   SInvalidMask = 'Invalid mask';
   SDuplicateDispatchName = 'The dispatchers names are duplicated';
   SErrorMsg = 'URL: <A HRef="%s">%s</A><P>%s Col %d, Row %d<P><TEXTAREA COLS="80">%s</TEXTAREA>';
   SMimeTypeNotDetected = 'The Mime type can not be determined';
   SErrorPage = '<HTML><HEAD><TITLE>Error in TWebProvider</TITLE></HEAD><PRE>Project %s raised exception<BR>%s</PRE></BODY></HTML>';


implementation

end.
