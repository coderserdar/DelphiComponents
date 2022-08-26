// ------------------------------------------------------------------------------
// DPF.iOS.ABPeoplePickerNavigationController Wrapped Classes & Interfaces
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.iOS.ABPeoplePickerNavigationController;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Math,
{$IFDEF IOS}
  iOSapi.UIKit,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
  iOSapi.CoreLocation,
  Macapi.CoreFoundation,
{$ENDIF}
  DPF.iOS.Common,
  FMX.Dialogs;

{$IFDEF IOS}
// ===== External functions =====

const
  libAddressBookUI = '/System/Library/Frameworks/AddressBookUI.framework/AddressBookUI';
  libAddressBook   = '/System/Library/Frameworks/AddressBook.framework/AddressBook';

  // Record Types
  kABPersonType = 0;
  kABGroupType  = 1;
  kABSourceType = 2;

  kABPersonImageFormatThumbnail    = 0;
  kABPersonImageFormatOriginalSize = 2;

  (*
    kABPersonFirstNameProperty          = 0;
    kABPersonLastNameProperty           = 1;
    kABPersonMiddleNameProperty         = 2;
    kABPersonPrefixProperty             = 3;
    kABPersonSuffixProperty             = 4;
    kABPersonNicknameProperty           = 5;
    kABPersonFirstNamePhoneticProperty  = 6;
    kABPersonLastNamePhoneticProperty   = 7;
    kABPersonMiddleNamePhoneticProperty = 8;
    kABPersonOrganizationProperty       = 9;
    kABPersonJobTitleProperty           = 10;
    kABPersonDepartmentProperty         = 11;
    kABPersonEmailProperty              = 12;
    kABPersonBirthdayProperty           = 13;
    kABPersonNoteProperty               = 14;
    kABPersonCreationDateProperty       = 15;
    kABPersonModificationDateProperty   = 16;
    kABPersonAddressProperty            = 17;
  *)

  kABErrorInProperty         = $0;
  kABStringProperty          = $1;
  kABIntegerProperty         = $2;
  kABRealProperty            = $3;
  kABDateProperty            = $4;
  kABArrayProperty           = $5;
  kABDictionaryProperty      = $6;
  kABDataProperty            = $7;
  kABMultiStringProperty     = $100 or kABStringProperty;
  kABMultiIntegerProperty    = $100 or kABIntegerProperty;
  kABMultiRealProperty       = $100 or kABRealProperty;
  kABMultiDateProperty       = $100 or kABDateProperty;
  kABMultiArrayProperty      = $100 or kABArrayProperty;
  kABMultiDictionaryProperty = $100 or kABDictionaryProperty;
  kABMultiDataProperty       = $100 or kABDataProperty;

  kABMultiValueMask = ( 1 shl 8 );

  // ABPropertyType : Record Property Types
  kABInvalidPropertyType         = 0;
  kABStringPropertyType          = 1;
  kABIntegerPropertyType         = 2;
  kABRealPropertyType            = 3;
  kABDateTimePropertyType        = 4;
  kABDictionaryPropertyType      = 5;
  kABMultiStringPropertyType     = kABMultiValueMask or kABStringPropertyType;
  kABMultiIntegerPropertyType    = kABMultiValueMask or kABIntegerPropertyType;
  kABMultiRealPropertyType       = kABMultiValueMask or kABRealPropertyType;
  kABMultiDateTimePropertyType   = kABMultiValueMask or kABDateTimePropertyType;
  kABMultiDictionaryPropertyType = kABMultiValueMask or kABDictionaryPropertyType;

  kABPersonCompositeNameFormatFirstNameFirst = 0;
  kABPersonCompositeNameFormatLastNameFirst  = 1;

  // ABAuthorizationStatus iOS 6 and later
{$IFDEF IOS6}
  kABAuthorizationStatusNotDetermined = 0; // iOS 6 and later
  kABAuthorizationStatusRestricted    = 1; // iOS 6 and later
  kABAuthorizationStatusDenied        = 2; // iOS 6 and later
  kABAuthorizationStatusAuthorized    = 3; // iOS 6 and later
{$ENDIF}

type
{$M+}
  ABAddressBookRef            = CFTypeRef;
  ABRecordRef                 = CFTypeRef;
  ABMultiValueRef             = CFTypeRef;
  ABMutableMultiValueRef      = CFTypeRef;
  ABPropertyID                = UINT32;
  ABRecordID                  = UINT32;
  ABMultiValueIdentifier      = UINT32;
  ABPersonSortOrdering        = UINT32;
  ABRecordType                = UINT32;
  ABPropertyType              = NSInteger;
  ABPersonImageFormat         = NSInteger;
  ABPersonCompositeNameFormat = NSInteger;
  ABAuthorizationStatus       = NSInteger;

  ABPeoplePickerNavigationControllerDelegate = interface;
  ABPersonViewControllerDelegate             = interface;

  // ----------------------------------------------------------------------------
  // ABPeoplePickerNavigationController
  // ----------------------------------------------------------------------------
  ABPersonViewControllerClass = interface( UIViewControllerClass )
    ['{3E4616A3-F589-48DB-9C6E-AA5ED298758F}']
  end;

  ABPersonViewController = interface( UIViewController )
    ['{57E05C68-2A6C-4344-AC07-E544DD74CE64}']

    function displayedProperties: NSArray; cdecl;
    procedure setDisplayedProperties( displayedProperties: NSArray ); cdecl;

    function shouldShowLinkedPeople: Boolean; cdecl;
    function allowsActions: Boolean; cdecl;

    function allowsEditing: Boolean; cdecl;
    procedure setAllowsEditing( allowsEditing: Boolean ); cdecl;

    procedure setHighlightedItemForProperty( &property: ABPropertyID; identifier: ABMultiValueIdentifier ); cdecl;

    function personViewDelegate: ABPersonViewControllerDelegate; cdecl;
    procedure setPersonViewDelegate( personViewDelegate: ABPersonViewControllerDelegate ); cdecl;

    function displayedPerson: ABRecordRef; cdecl;
    procedure setDisplayedPerson( displayedPerson: ABRecordRef ); cdecl;

    function addressBook: ABAddressBookRef; cdecl;
    procedure setAddressBook( addressBook: ABAddressBookRef ); cdecl;
  end;

  TABPersonViewController = class( TOCGenericImport<ABPersonViewControllerClass, ABPersonViewController> )
  end;

  // ----------------------------------------------------------------------------
  // ABPersonViewControllerDelegate
  // ----------------------------------------------------------------------------
  ABPersonViewControllerDelegate = interface( IObjectiveC )
    ['{CE68E474-4506-4DEF-AB2C-A580ECEDFFA7}']
    function personViewController( personViewController: ABPersonViewController; shouldPerformDefaultActionForPerson: ABRecordRef; &property: ABPropertyID; identifier: ABMultiValueIdentifier ): Boolean; cdecl;
  end;

  // ----------------------------------------------------------------------------
  // ABPeoplePickerNavigationController
  // ----------------------------------------------------------------------------
  ABPeoplePickerNavigationControllerClass = interface( UINavigationControllerClass )
    ['{5234C8DD-C52B-41BE-AA10-52EE0C4CCBFF}']
  end;

  ABPeoplePickerNavigationController = interface( UINavigationController )
    ['{4BEE4788-DAE4-4FA4-BD92-0AA7D94C2CAA}']
    function displayedProperties: NSArray; cdecl;

    function addressBook: ABAddressBookRef; cdecl;

    function peoplePickerDelegate: ABPeoplePickerNavigationControllerDelegate; cdecl;
    procedure setPeoplePickerDelegate( peoplePickerDelegate: ABPeoplePickerNavigationControllerDelegate ); cdecl;
  end;

  TABPeoplePickerNavigationController = class( TOCGenericImport<ABPeoplePickerNavigationControllerClass, ABPeoplePickerNavigationController> )
  end;

  // ----------------------------------------------------------------------------
  // ABPeoplePickerNavigationControllerDelegate
  // ----------------------------------------------------------------------------
  ABPeoplePickerNavigationControllerDelegate = interface( IObjectiveC )
    ['{BF4EDA17-29D6-4C0D-BBFE-6AEBACDBFF64}']
    function peoplePickerNavigationController( peoplePicker: ABPeoplePickerNavigationController; shouldContinueAfterSelectingPerson: ABRecordRef ): Boolean; cdecl; overload;

    function peoplePickerNavigationController( peoplePicker: ABPeoplePickerNavigationController; shouldContinueAfterSelectingPerson: ABRecordRef; &property: ABPropertyID; identifier: ABMultiValueIdentifier ): Boolean; cdecl; overload;
    procedure peoplePickerNavigationControllerDidCancel( peoplePickerend: ABPeoplePickerNavigationController ); cdecl;
  end;

  TABAddressBookRequestAccessCompletionHandler = procedure( grant: Pointer; error: PCFErrorRef );
  TABExternalChangeCallback                    = procedure( addressBook: ABAddressBookRef; info: CFDictionaryRef; context: Pointer ) of object;

  // ----------------------------------------------------------------------------
function ABRecordGetRecordID( &record: ABRecordRef ): ABRecordID; cdecl; external libAddressBook name _PU + 'ABRecordGetRecordID';
function ABRecordGetRecordType( &record: ABRecordRef ): ABRecordType; cdecl; external libAddressBook name _PU + 'ABRecordGetRecordType';
function ABRecordSetValue( &record: ABRecordRef; &property: ABPropertyID; value: CFTypeRef; error: CFErrorRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABRecordSetValue';
function ABRecordRemoveValue( &record: ABRecordRef; &property: ABPropertyID; error: CFErrorRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABRecordRemoveValue';
function ABRecordCopyCompositeName( &record: ABRecordRef ): CFStringRef; cdecl; external libAddressBook name _PU + 'ABRecordCopyCompositeName';

// ----------------------------------------------------------------------------
// Apple sayed: ABAddressBookCreate deprecated use ABAddressBookCreateWithOptions
function ABAddressBookCreateWithOptions( options: CFDictionaryRef; error: CFErrorRef ): ABAddressBookRef; cdecl; external libAddressBook name _PU + 'ABAddressBookCreateWithOptions';
function ABRecordCopyValue( &record: ABRecordRef; &property: ABPropertyID ): CFTypeRef; cdecl; external libAddressBook name _PU + 'ABRecordCopyValue';
function ABMultiValueCopyValueAtIndex( multiValue: ABMultiValueRef; index: CFIndex ): CFTypeRef; cdecl; external libAddressBook name _PU + 'ABMultiValueCopyValueAtIndex';
function ABMultiValueCopyArrayOfAllValues( multiValue: ABMultiValueRef ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABMultiValueCopyArrayOfAllValues';
function ABMultiValueGetCount( multiValue: ABMultiValueRef ): CFIndex; cdecl; external libAddressBook name _PU + 'ABMultiValueGetCount';
function ABMultiValueGetFirstIndexOfValue( multiValue: ABMultiValueRef; value: CFTypeRef ): CFIndex; cdecl; external libAddressBook name _PU + 'ABMultiValueGetFirstIndexOfValue';
function ABMultiValueCopyLabelAtIndex( multiValue: ABMultiValueRef; index: CFIndex ): CFStringRef; cdecl; external libAddressBook name _PU + 'ABMultiValueCopyLabelAtIndex';
function ABMultiValueGetIdentifierAtIndex( multiValue: ABMultiValueRef; index: CFIndex ): ABMultiValueIdentifier; cdecl; external libAddressBook name _PU + 'ABMultiValueGetIdentifierAtIndex';
function ABMultiValueGetIndexForIdentifier( multiValue: ABMultiValueRef; identifier: ABMultiValueIdentifier ): CFIndex; cdecl; external libAddressBook name _PU + 'ABMultiValueGetIndexForIdentifier';
function ABMultiValueGetPropertyType( multiValue: ABMultiValueRef ): ABPropertyType; cdecl; external libAddressBook name _PU + 'ABMultiValueGetPropertyType';

function ABMultiValueCreateMutable( &type: ABPropertyType ): ABMutableMultiValueRef; cdecl; external libAddressBook name _PU + 'ABMultiValueCreateMutable';
function ABMultiValueAddValueAndLabel( multiValue: ABMutableMultiValueRef; value: CFTypeRef; &label: CFStringRef; outIdentifier: ABMultiValueIdentifier ): Boolean; cdecl; external libAddressBook name _PU + 'ABMultiValueAddValueAndLabel';

// ----------------------------------------------------------------------------
function ABPersonCreate: ABRecordRef; cdecl; external libAddressBook name _PU + 'ABPersonCreate';
function ABPersonCreateInSource( source: ABRecordRef ): ABRecordRef; cdecl; external libAddressBook name _PU + 'ABPersonCreateInSource';
function ABPersonComparePeopleByName( person1: ABRecordRef; person2: ABRecordRef; ordering: ABPersonSortOrdering ): CFComparisonResult; cdecl; external libAddressBook name _PU + 'ABPersonComparePeopleByName';

function ABPersonGetTypeOfProperty( &property: ABPropertyID ): ABPropertyType; cdecl; external libAddressBook name _PU + 'ABPersonGetTypeOfProperty';
function ABPersonCopyLocalizedPropertyName( &property: ABPropertyID ): CFStringRef; cdecl; external libAddressBook name _PU + 'ABPersonCopyLocalizedPropertyName';

function ABPersonSetImageData( person: ABRecordRef; imageData: CFDataRef; error: CFErrorRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABPersonSetImageData';
function ABPersonCopyImageData( person: ABRecordRef ): CFDataRef; cdecl; external libAddressBook name _PU + 'ABPersonCopyImageData';
function ABPersonCopyImageDataWithFormat( person: ABRecordRef; format: ABPersonImageFormat ): CFDataRef; cdecl; external libAddressBook name _PU + 'ABPersonCopyImageDataWithFormat';
function ABPersonHasImageData( person: ABRecordRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABPersonHasImageData';
function ABPersonRemoveImageData( person: ABRecordRef; error: CFErrorRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABPersonRemoveImageData';
function ABAddressBookGetPersonCount( addressBook: ABAddressBookRef ): CFIndex; cdecl; external libAddressBook name _PU + 'ABAddressBookGetPersonCount';
function ABAddressBookGetPersonWithRecordID( addressBook: ABAddressBookRef; recordID: ABRecordID ): ABRecordRef; cdecl; external libAddressBook name _PU + 'ABAddressBookGetPersonWithRecordID';
function ABAddressBookCopyArrayOfAllPeople( addressBook: ABAddressBookRef ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABAddressBookCopyArrayOfAllPeople';

// ----------------------------------------------------------------------------
function ABAddressBookCopyArrayOfAllPeopleInSource( addressBook: ABAddressBookRef; source: ABRecordRef ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABAddressBookCopyArrayOfAllPeopleInSource';
function ABAddressBookCopyArrayOfAllPeopleInSourceWithSortOrdering( addressBook: ABAddressBookRef; source: ABRecordRef; sortOrdering: ABPersonSortOrdering ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABAddressBookCopyArrayOfAllPeopleInSourceWithSortOrdering';
function ABAddressBookCopyPeopleWithName( addressBook: ABAddressBookRef; name: CFStringRef ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABAddressBookCopyPeopleWithName';
function ABPersonCopyArrayOfAllLinkedPeople( person: ABRecordRef ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABPersonCopyArrayOfAllLinkedPeople';
function ABPersonCopySource( person: ABRecordRef ): ABRecordRef; cdecl; external libAddressBook name _PU + 'ABPersonCopySource';
function ABPersonGetSortOrdering( ): ABPersonSortOrdering; cdecl; external libAddressBook name _PU + 'ABPersonGetSortOrdering';
function ABPersonGetCompositeNameFormat( ): ABPersonCompositeNameFormat; cdecl; external libAddressBook name _PU + 'ABPersonGetCompositeNameFormat';
function ABPersonCreatePeopleInSourceWithVCardRepresentation( source: ABRecordRef; vCardData: CFDataRef ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABPersonCreatePeopleInSourceWithVCardRepresentation';
function ABPersonCreateVCardRepresentationWithPeople( people: CFArrayRef ): CFDataRef; cdecl; external libAddressBook name _PU + 'ABPersonCreateVCardRepresentationWithPeople';

// ----------------------------------------------------------------------------
function ABGroupCreate( ): ABRecordRef; cdecl; external libAddressBook name _PU + 'ABGroupCreate';
function ABGroupCreateInSource( source: ABRecordRef ): ABRecordRef; cdecl; external libAddressBook name _PU + 'ABGroupCreateInSource';
function ABGroupCopyArrayOfAllMembers( group: ABRecordRef ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABGroupCopyArrayOfAllMembers';
function ABGroupCopyArrayOfAllMembersWithSortOrdering( group: ABRecordRef; sortOrdering: ABPersonSortOrdering ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABGroupCopyArrayOfAllMembersWithSortOrdering';
function ABGroupAddMember( group: ABRecordRef; person: ABRecordRef; error: CFErrorRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABGroupAddMember';
function ABGroupRemoveMember( group: ABRecordRef; member: ABRecordRef; error: CFErrorRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABGroupRemoveMember';
function ABAddressBookGetGroupWithRecordID( addressBook: ABAddressBookRef; recordID: ABRecordID ): ABRecordRef; cdecl; external libAddressBook name _PU + 'ABAddressBookGetGroupWithRecordID';
function ABAddressBookGetGroupCount( addressBook: ABAddressBookRef ): CFIndex; cdecl; external libAddressBook name _PU + 'ABAddressBookGetGroupCount';
function ABAddressBookCopyArrayOfAllGroups( addressBook: ABAddressBookRef ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABAddressBookCopyArrayOfAllGroups';
function ABAddressBookCopyArrayOfAllGroupsInSource( addressBook: ABAddressBookRef; source: ABRecordRef ): CFArrayRef; cdecl; external libAddressBook name _PU + 'ABAddressBookCopyArrayOfAllGroupsInSource';
function ABGroupCopySource( group: ABRecordRef ): ABRecordRef; cdecl; external libAddressBook name _PU + 'ABGroupCopySource';
function ABAddressBookCreate( ): ABAddressBookRef; cdecl; external libAddressBook name _PU + 'ABAddressBookCreate';

// ----------------------------------------------------------------------------
// iOS 6 and later only
{$IFDEF IOS6}
function ABAddressBookGetAuthorizationStatus: ABAuthorizationStatus; cdecl; external libAddressBook name _PU + 'ABAddressBookGetAuthorizationStatus'; // iOS 6 and later
procedure ABAddressBookRequestAccessWithCompletion( addressBook: ABAddressBookRef; completion: TABAddressBookRequestAccessCompletionHandler ); cdecl; external libAddressBook name _PU + 'ABAddressBookRequestAccessWithCompletion'; // iOS 6 and later
{$ENDIF}
// ----------------------------------------------------------------------------
function ABAddressBookHasUnsavedChanges( addressBook: ABAddressBookRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABAddressBookHasUnsavedChanges';
function ABAddressBookSave( addressBook: ABAddressBookRef; error: CFErrorRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABAddressBookSave';
procedure ABAddressBookRevert( addressBook: ABAddressBookRef ); cdecl; external libAddressBook name _PU + 'ABAddressBookRevert';
function ABAddressBookAddRecord( addressBook: ABAddressBookRef; &record: ABRecordRef; error: CFErrorRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABAddressBookAddRecord';
function ABAddressBookRemoveRecord( addressBook: ABAddressBookRef; &record: ABRecordRef; error: CFErrorRef ): Boolean; cdecl; external libAddressBook name _PU + 'ABAddressBookRemoveRecord';
procedure ABAddressBookRegisterExternalChangeCallback( addressBook: ABAddressBookRef; callback: TABExternalChangeCallback; context: Pointer ); cdecl; external libAddressBook name _PU + 'ABAddressBookRegisterExternalChangeCallback';
procedure ABAddressBookUnregisterExternalChangeCallback( addressBook: ABAddressBookRef; callback: TABExternalChangeCallback; context: Pointer ); cdecl; external libAddressBook name _PU + 'ABAddressBookUnregisterExternalChangeCallback';
function ABAddressBookCopyLocalizedLabel( &label: CFStringRef ): CFStringRef; cdecl; external libAddressBook name _PU + 'ABAddressBookCopyLocalizedLabel';

// ----------------------------------------------------------------------------
// Consts
function kABPersonPhoneMainLabel: string;
function kABPersonPhoneMobileLabel: string;
function kABPersonPhoneIPhoneLabel: string;

{$ENDIF}

// ----------------------------------------------------------------------------
implementation

{$IFDEF IOS}
// ----------------------------------------------------------------------------

{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  iAddressBookUIModule: THandle;
  iAddressBookModule  : THandle;
{$ENDIF}
{$IFDEF IOS}

  // ----------------------------------------------------------------------------
function kABPersonPhoneMainLabel: string;
var
  ns: NSSTring;
begin
  ns     := CocoaNSStringConst( libAddressBook, 'kABPersonPhoneMainLabel' );
  result := ns.UTF8String;
end;

// ----------------------------------------------------------------------------
function kABPersonPhoneMobileLabel: string;
var
  ns: NSSTring;
begin
  ns     := CocoaNSStringConst( libAddressBook, 'kABPersonPhoneMobileLabel' );
  result := ns.UTF8String;
end;

// ----------------------------------------------------------------------------
function kABPersonPhoneIPhoneLabel: string;
var
  ns: NSSTring;
begin
  ns     := CocoaNSStringConst( libAddressBook, 'kABPersonPhoneIPhoneLabel' );
  result := ns.UTF8String;
end;

// ----------------------------------------------------------------------------
{$IF defined(CPUARM)}
procedure LibAddressBookUILoader; cdecl; external libAddressBookUI;
procedure LibAddressBookLoader; cdecl; external libAddressBook;
{$ELSE}
// ----------------------------------------------------------------------------

initialization

iAddressBookModule   := dlopen( MarshaledAString( libAddressBook ), RTLD_LAZY );
iAddressBookUIModule := dlopen( MarshaledAString( libAddressBookUI ), RTLD_LAZY );

finalization

dlclose( iAddressBookUIModule );
dlclose( iAddressBookModule );
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.
