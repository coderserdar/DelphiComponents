{*********************************************************}
{*                    VPSR.PAS 1.03                      *}
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

{Visual PlanIt String Resources - To create language specific versions of
 Visual PlanIt controls, translate the strings in VpSR.INC to the desired
 language and re-compile your application package.}

unit VpSR;
  {- Visual PlanIt String Resources}

interface

{$I VpSR.INC}

type
  { For acquiring the labels that go with each telephone field }
  { ie. Home, work, fax etc...                                 }
  TVpPhoneType = (ptAssistant, ptCallback, ptCar, ptCompany, ptHome, ptHomeFax,
                  ptISDN, ptMobile, ptOther, ptOtherFax, ptPager, ptPrimary,
                  ptRadio, ptTelex, ptTTYTDD, ptWork, ptWorkFax);

  TVpCategoryType = (ctBusiness, ctClients, ctFamily, ctOther, ctPersonal);

function PhoneLabel (PhoneType : TVpPhoneType) : string;

function CategoryLabel (CategoryType : TVpCategoryType) : string;

implementation

function PhoneLabel(PhoneType: TVpPhoneType): string;
begin
  Result := '';
  case PhoneType of
    ptAssistant : Result := RSPhoneTypeLabel1;
    ptCallback  : Result := RSPhoneTypeLabel2;
    ptCar       : Result := RSPhoneTypeLabel3;
    ptCompany   : Result := RSPhoneTypeLabel4;
    ptHome      : Result := RSPhoneTypeLabel5;
    ptHomeFax   : Result := RSPhoneTypeLabel6;
    ptISDn      : Result := RSPhoneTypeLabel7;
    ptMobile    : Result := RSPhoneTypeLabel8;
    ptOther     : Result := RSPhoneTypeLabel9;
    ptOtherFax  : Result := RSPhoneTypeLabel10;
    ptPager     : Result := RSPhoneTypeLabel11;
    ptPrimary   : Result := RSPhoneTypeLabel12;
    ptRadio     : Result := RSPhoneTypeLabel13;
    ptTelex     : Result := RSPhoneTypeLabel14;
    ptTTYTDD    : Result := RSPhoneTypeLabel15;
    ptWork      : Result := RSPhoneTypeLabel16;
    ptWorkFax   : Result := RSPhoneTypeLabel17;
  end;
end;

function CategoryLabel (CategoryType : TVpCategoryType) : string;
begin
  Result := '';
  case CategoryType of
    ctBusiness : Result := RSCategoryLabel1;
    ctClients  : Result := RSCategoryLabel2;
    ctFamily   : Result := RSCategoryLabel3;
    ctOther    : Result := RSCategoryLabel5;
    ctPersonal : Result := RSCategoryLabel4;
  end;
end;

end.

