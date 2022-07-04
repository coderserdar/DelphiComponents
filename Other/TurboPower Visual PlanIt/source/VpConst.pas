{*********************************************************}
{*                   VPCONST.PAS 1.03                    *}
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

unit VpConst;
  {-Versioning defines and methods}

interface

uses
  Windows, Forms, StdCtrls;

const
  BuildTime = '09/13/2002 09:25 AM';
  VpVersionStr = 'v1.03';    {Visual PlanIt library version}
  VpProductName = 'Visual PlanIt';

  BorderStyles    : array[TBorderStyle] of LongInt =
                    (0, WS_BORDER);
  ScrollBarStyles : array [TScrollStyle] of LongInt =
                    (0, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);

  SecondsInDay     = 86400;   { Number of seconds in a day               }
  SecondsInHour    = 3600;    { Number of seconds in an hour             }
  SecondsInMinute  = 60;      { Number of seconds in a minute            }
  HoursInDay       = 24;      { Number of hours in a day                 }
  MinutesInHour    = 60;      { Number of minutes in an hour             }
  MinutesInDay     = 1440;    { Number of minutes in a day               }
  MaxDateLen       = 40;      { maximum length of date picture strings   }
  MaxMonthName     = 15;      { maximum length for month names           }
  MaxDayName       = 15;      { maximum length for day names             }
  TextMargin       = 5;       { amount of space around text              }
  MaxVisibleEvents = 1024;    { maximum number of events that can be     }
                              { visible at any one time                  }
  MaxEventDepth    = 50;      { the maximum number of side by side       }
                              { events, which can be displayed in the    }
                              { DayView component.                       }
  ClickDelay : Integer = 500; { the number of milliseconds of delay for  } 
                              { each event click in the TimeGrid         }
  calDefHeight     = 140;     { popup calendar default height            }
  calDefWidth      = 200;     { popup calendar default width             }
  ExtraBarWidth    = 2;       { The extra, draggable area on either side }
                              { of the Contact Grid's horizontal bars.   }

  ResourceTableName = 'Resources';
  TasksTableName    = 'Tasks';
  EventsTableName   = 'Events';
  ContactsTableName = 'Contacts';
  RecordIDTableName = 'RecordIDS';

  {virtual key constants not already defined}
  VK_NONE = 0;
  VK_ALT  = VK_MENU;
  VK_A = Ord('A');  VK_B = Ord('B');  VK_C = Ord('C');  VK_D = Ord('D');
  VK_E = Ord('E');  VK_F = Ord('F');  VK_G = Ord('G');  VK_H = Ord('H');
  VK_I = Ord('I');  VK_J = Ord('J');  VK_K = Ord('K');  VK_L = Ord('L');
  VK_M = Ord('M');  VK_N = Ord('N');  VK_O = Ord('O');  VK_P = Ord('P');
  VK_Q = Ord('Q');  VK_R = Ord('R');  VK_S = Ord('S');  VK_T = Ord('T');
  VK_U = Ord('U');  VK_V = Ord('V');  VK_W = Ord('W');  VK_X = Ord('X');
  VK_Y = Ord('Y');  VK_Z = Ord('Z');  VK_0 = Ord('0');  VK_1 = Ord('1');
  VK_2 = Ord('2');  VK_3 = Ord('3');  VK_4 = Ord('4');  VK_5 = Ord('5');
  VK_6 = Ord('6');  VK_7 = Ord('7');  VK_8 = Ord('8');  VK_9 = Ord('9');

{------------------- Windows messages -----------------------}
  {Not a message code. Value of the first of the message codes used}
  Vp_FIRST = $7F00; {***}
  {sent to force a call to RecreateWnd}
  Vp_RECREATEWND         = Vp_FIRST + 1;
  {sent to perform after-enter notification}
  Vp_AFTERENTER          = Vp_FIRST + 2;
  {sent to perform after-exit notification}
  Vp_AFTEREXIT           = Vp_FIRST + 3;
  {sent by a collection to its property editor when a property is changed}
  Vp_PROPCHANGE          = Vp_FIRST + 4;


{*** Error message codes ***}
  oeFirst             = 256;

{ XML support }

  {The following constants are the tokens needed to parse an XML
   document. The tokens are stored in UCS-4 format to reduce the
   number of conversions needed by the filter.}
  Xpc_BracketAngleLeft : array[0..0] of Longint = (60); {<}
  Xpc_BracketAngleRight : array[0..0] of Longint = (62); {>}
  Xpc_BracketSquareLeft : array[0..0] of Longint = (91); {[}
  Xpc_BracketSquareRight : array[0..0] of Longint = (93); {]}
  Xpc_CDATAStart :
    array[0..5] of Longint = (67, 68, 65, 84, 65, 91); {CDATA[}
  Xpc_CharacterRef : array[0..0] of Longint = (35); {#}
  Xpc_CharacterRefHex : array[0..0] of Longint = (120); {x}
  Xpc_CommentEnd : array[0..2] of Longint = (45, 45, 62); {-->}
  Xpc_CommentStart : array[0..3] of Longint = (60, 33, 45, 45); {<!--}
  Xpc_ConditionalEnd : array[0..2] of Longint = (93, 93, 62); {]]>}
  Xpc_ConditionalIgnore :
    array[0..5] of Longint = (73, 71, 78, 79, 82, 69); {IGNORE}
  Xpc_ConditionalInclude :
    array[0..6] of Longint = (73, 78, 67, 76, 85, 68, 69); {INCLUDE}
  Xpc_ConditionalStart :
    array[0..2] of Longint = (60, 33, 91); {<![}
  Xpc_Dash : array[0..0] of Longint = (45); {-}
  Xpc_DTDAttFixed :
    array[0..4] of Longint = (70, 73, 88, 69, 68); {FIXED}
  Xpc_DTDAttImplied :
    array[0..6] of Longint = (73, 77, 80, 76, 73, 69, 68); {IMPLIED}
  Xpc_DTDAttlist :
    array[0..8] of Longint =
      (60, 33, 65, 84, 84, 76, 73, 83, 84); {<!ATTLIST}
  Xpc_DTDAttRequired :
    array[0..7] of Longint =
      (82, 69, 81, 85, 73, 82, 69, 68); {REQUIRED}
  Xpc_DTDDocType :
    array[0..8] of Longint =
      (60, 33, 68, 79, 67, 84, 89, 80, 69); {<!DOCTYPE}
  Xpc_DTDElement :
    array[0..8] of Longint =
      (60, 33, 69, 76, 69, 77, 69, 78, 84); {<!ELEMENT}
  Xpc_DTDElementAny : array[0..2] of Longint = (65, 78, 89); {ANY}
  Xpc_DTDElementCharData :
    array[0..6] of Longint = (35, 80, 67, 68, 65, 84, 65); {#PCDATA}
  Xpc_DTDElementEmpty :
    array[0..4] of Longint = (69, 77, 80, 84, 89); {EMPTY}
  Xpc_DTDEntity :
    array[0..7] of Longint =
      (60, 33, 69, 78, 84, 73, 84, 89); {<!ENTITY}
  Xpc_DTDNotation :
    array[0..9] of Longint =
      (60, 33, 78, 79, 84, 65, 84, 73, 79, 78); {<!NOTATION}
  Xpc_Encoding : array[0..7] of Longint =
    (101, 110, 99, 111, 100, 105, 110, 103); {encoding}
  Xpc_Equation : array[0..0] of Longint = (61); {=}
  Xpc_ExternalPublic :
    array[0..5] of Longint = (80, 85, 66, 76, 73, 67); {PUBLIC}
  Xpc_ExternalSystem :
    array[0..5] of Longint = (83, 89, 83, 84, 69, 77); {SYSTEM}
  Xpc_GenParsedEntityEnd : array[0..0] of Longint = (59); {;}
  Xpc_ListOperator : array[0..0] of Longint = (124); {|}
  Xpc_MixedEnd : array[0..1] of Longint = (41, 42); {)*}
  Xpc_OneOrMoreOpr : array[0..0] of Longint = (42); {*}
  Xpc_ParamEntity : array[0..0] of Longint = (37); {%}
  Xpc_ParenLeft : array[0..0] of Longint = (40); {(}
  Xpc_ParenRight : array[0..0] of Longint = (41); {)}
  Xpc_ProcessInstrEnd : array[0..1] of Longint = (63, 62); {?>}
  Xpc_ProcessInstrStart : array[0..1] of Longint = (60, 63); {<?}
  Xpc_QuoteDouble : array[0..0] of Longint = (34); {"}
  Xpc_QuoteSingle : array[0..0] of Longint = (39); {'}
  Xpc_Standalone :
    array[0..9] of Longint =
      (115, 116, 97, 110, 100, 97, 108, 111, 110, 101); {standalone}
  Xpc_UnparsedEntity :
    array[0..4] of Longint = (78, 68, 65, 84, 65); {NDATA}
  Xpc_Version :
    array[0..6] of Longint =
      (118, 101, 114, 115, 105, 111, 110); {version}

  LIT_CHAR_REF = 1;
  LIT_ENTITY_REF = 2;
  LIT_PE_REF = 4;
  LIT_NORMALIZE = 8;

  CONTEXT_NONE = 0;
  CONTEXT_DTD = 1;
  CONTEXT_ENTITYVALUE = 2;
  CONTEXT_ATTRIBUTEVALUE = 3;

  CONTENT_UNDECLARED = 0;
  CONTENT_ANY = 1;
  CONTENT_EMPTY = 2;
  CONTENT_MIXED = 3;
  CONTENT_ELEMENTS = 4;

  OCCURS_REQ_NOREPEAT = 0;
  OCCURS_OPT_NOREPEAT = 1;
  OCCURS_OPT_REPEAT = 2;
  OCCURS_REQ_REPEAT = 3;

  REL_OR = 0;
  REL_AND = 1;
  REL_NONE = 2;

  ATTRIBUTE_UNDECLARED = 0;
  ATTRIBUTE_CDATA = 1;
  ATTRIBUTE_ID = 2;
  ATTRIBUTE_IDREF = 3;
  ATTRIBUTE_IDREFS = 4;
  ATTRIBUTE_ENTITY = 5;
  ATTRIBUTE_ENTITIES = 6;
  ATTRIBUTE_NMTOKEN = 7;
  ATTRIBUTE_NMTOKENS = 8;
  ATTRIBUTE_ENUMERATED = 9;
  ATTRIBUTE_NOTATION = 10;

  ATTRIBUTE_DEFAULT_UNDECLARED = 0;
  ATTRIBUTE_DEFAULT_SPECIFIED = 1;
  ATTRIBUTE_DEFAULT_IMPLIED = 2;
  ATTRIBUTE_DEFAULT_REQUIRED = 3;
  ATTRIBUTE_DEFAULT_FIXED = 4;

  ENTITY_UNDECLARED = 0;
  ENTITY_INTERNAL = 1;
  ENTITY_NDATA = 2;
  ENTITY_TEXT = 3;

  CONDITIONAL_INCLUDE = 0;
  CONDITIONAL_IGNORE = 1;

  { Version numbers }
  VpXSLImplementation = 0.0;
  VpXMLSpecification = '1.0';

{ Defaults }                                                             

  { MonthView }                                                          

  vpDefWVRClickChangeDate = True;                                        

implementation

initialization                                                           

  ClickDelay :=  GetDoubleClickTime;                                     

end.
