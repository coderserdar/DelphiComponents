// ------------------------------------------------------------------------------
// DPF.iOS.ABPeoplePicker Component
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
unit DPF.iOS.ABPeoplePicker;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  Macapi.CoreFoundation,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
  DPF.iOS.ABPeoplePickerNavigationController,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

  TAddress = record
    Country: string;
    City: string;
    Street: string;
    Zip: string;
    CountryCode: string;
    State: string;
  end;

  TPersonRecord = record
    FirstName: string;
    LastName: string;
    MiddleName: string;
    Prefix: string;
    Suffix: string;
    URL: array of string;
    Nickname: string;
    Organization: string;
    Company: string;
    JobTitle: string;
    Department: string;
    Birthday: string;
    Notes: array of string;
    Emails: array of string;
    CreationDate: string;
    Phones: array of string;
    Addresses: array of TAddress;
{$IFDEF IOS}
    Image: UIImage;
{$ENDIF}
  end;

  TPersonRecordArray = array of TPersonRecord;

  TDPFAddressBook = class;

  TDPFOnPersonSelected       = procedure( Sender: TObject; Person: TPersonRecord; var ShowUserDetail: Boolean ) of object;
  TDPFOnPersonSelectedDetail = procedure( Sender: TObject; Person: TPersonRecord; SelectedPropertyName: string; var CloseAddressBook: Boolean ) of object;
  TDPFOnPersonSelectedCancel = procedure( Sender: TObject; var CloseAddressBook: Boolean ) of object;
{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  TPeoplePickerViewControllerDelegate = class( TOCLocal, ABPeoplePickerNavigationControllerDelegate )
  private
    FDPFAddressBook: TDPFAddressBook;
  public
    constructor Create( DPFAddressBook: TDPFAddressBook );
    function ScanPersonProperties( Person: ABRecordRef; var PersonRec: TPersonRecord; OnlyThisIndex: Integer = -1 ): string;
    procedure SetPropertyValue( PropName, ProbValue: string; var PersonRecord: TPersonRecord );
    procedure SetAddressFields( NSD: NSDictionary; var Address: TAddress );

    function peoplePickerNavigationController( peoplePicker: ABPeoplePickerNavigationController; shouldContinueAfterSelectingPerson: ABRecordRef ): Boolean; overload; cdecl;
    function peoplePickerNavigationController( peoplePicker: ABPeoplePickerNavigationController; shouldContinueAfterSelectingPerson: ABRecordRef; &property: ABPropertyID; identifier: ABMultiValueIdentifier ): Boolean; overload; cdecl;
    procedure peoplePickerNavigationControllerDidCancel( peoplePickerend: ABPeoplePickerNavigationController ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TABPersonViewControllerDelegate = class( TOCLocal, ABPersonViewControllerDelegate )
  private
    // FDPFAddressBook: TDPFAddressBook;
  public
    constructor Create( DPFMailCompose: TDPFAddressBook );
    function personViewController( personViewController: ABPersonViewController; shouldPerformDefaultActionForPerson: ABRecordRef; &property: ABPropertyID; identifier: ABMultiValueIdentifier ): Boolean; cdecl;
  end;
{$ENDIF}

  TOnRequestAccess = procedure( Sender: TObject; const Granted: Boolean ) of object;

  TPhoneNumbers = record
    PhoneNumber: string;
    PhoneLabel: string;
  end;
  // TPhoneNumbersArray= array of TPhoneNumbers;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFAddressBook = class( TComponent )
  private
    FOnPersonSelected      : TDPFOnPersonSelected;
    FOnPersonSelectedCancel: TDPFOnPersonSelectedCancel;
    FOnPersonSelectedDetail: TDPFOnPersonSelectedDetail;
    FOnRequestAccess       : TOnRequestAccess;
    FGranted               : Boolean;
{$IFDEF IOS}
    FPeoplePicker                      : ABPeoplePickerNavigationController;
    FPeoplePickerViewControllerDelegate: TPeoplePickerViewControllerDelegate;
    { FPersonViewController          : ABPersonViewController;
      FABPersonViewControllerDelegate: TABPersonViewControllerDelegate; }
{$ENDIF}
  protected
    procedure ViewControllerCompletion;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
{$IFDEF IOS}
    function ShowAddressBook: Boolean;
    procedure RequestAccess;
    function GetAddressBookList: TPersonRecordArray;
    function AddAddressBook( FirstName: string; LatName: string; PhoneNumbers: array of TPhoneNumbers ): Boolean;
    // procedure RequestAccessCompletionHandler( grant: pointer; error: PCFErrorRef );cdecl;
{$ENDIF}
  published
    property OnPersonSelected      : TDPFOnPersonSelected read FOnPersonSelected write FOnPersonSelected;
    property OnPersonSelectedDetail: TDPFOnPersonSelectedDetail read FOnPersonSelectedDetail write FOnPersonSelectedDetail;
    property OnPersonSelectedCancel: TDPFOnPersonSelectedCancel read FOnPersonSelectedCancel write FOnPersonSelectedCancel;
    property OnRequestAccess       : TOnRequestAccess read FOnRequestAccess write FOnRequestAccess;
    property Granted               : Boolean read FGranted default false;
  end;

  // ------------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
{ TDPFAddressBook }
// ------------------------------------------------------------------------------
constructor TDPFAddressBook.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
{$IFDEF IOS}
  FPeoplePicker                       := nil;
  FPeoplePickerViewControllerDelegate := TPeoplePickerViewControllerDelegate.Create( self );

  // FPersonViewController               := nil;
  // FABPersonViewControllerDelegate     := TABPersonViewControllerDelegate.Create( Self );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFAddressBook.Destroy;
begin
{$IFDEF IOS}
  FPeoplePickerViewControllerDelegate.DisposeOf;
  if Assigned( FPeoplePicker ) then
    FPeoplePicker.release;
  // FABPersonViewControllerDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFAddressBook.ViewControllerCompletion;
begin

end;

{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure RequestAccessCompletionHandler( grant: pointer; error: PCFErrorRef );
begin
  { if Assigned( FOnRequestAccess ) then
    begin
    FGranted := True;
    if TOSVersion.Major >= 6.0 then
    begin
    FGranted := ABAddressBookGetAuthorizationStatus( ) = kABAuthorizationStatusAuthorized;
    end;
    OnRequestAccess( self, Granted );
    end; }
end;

// ------------------------------------------------------------------------------
function TDPFAddressBook.AddAddressBook( FirstName: string; LatName: string; PhoneNumbers: array of TPhoneNumbers ): Boolean;
var
  Person               : ABRecordRef;
  error                : CFErrorRef;
  addressBook          : ABAddressBookRef;
  phoneNumberMultiValue: ABMutableMultiValueRef;
  venuePhoneNumbers    : NSMutableArray;
  I                    : Integer;
  s                    : string;
begin
  result      := false;
  addressBook := ABAddressBookCreateWithOptions( nil, &error );
  Person      := ABPersonCreate;
  if not Assigned( Person ) then
    exit;

  // FirsName & LastName
  result := ABRecordSetValue( Person, 0, PNSSTR( FirstName ), &error );
  if not result then
    exit;
  result := ABRecordSetValue( Person, 1, PNSSTR( LatName ), &error );
  if not result then
    exit;
  result := ABAddressBookAddRecord( addressBook, Person, &error );
  if not result then
    exit;

  // Phone
  phoneNumberMultiValue := ABMultiValueCreateMutable( kABMultiStringPropertyType );
  for I                 := low( PhoneNumbers ) to high( PhoneNumbers ) do
  begin
    if PhoneNumbers[I].PhoneLabel = '' then
      PhoneNumbers[I].PhoneLabel := kABPersonPhoneMainLabel;
    ABMultiValueAddValueAndLabel( phoneNumberMultiValue, PNSSTR( PhoneNumbers[I].PhoneNumber ), CFSTR( PhoneNumbers[I].PhoneLabel ), 0 );
  end;
  result := ABRecordSetValue( person, 3, phoneNumberMultiValue, nil );
  CFRelease( phoneNumberMultiValue );

  if not result then
    exit;

  // Save to AddressBook
  if ABAddressBookHasUnsavedChanges( addressBook ) then
    result := ABAddressBookSave( addressBook, &error );

  exit;
end;

// ------------------------------------------------------------------------------
function TDPFAddressBook.GetAddressBookList: TPersonRecordArray;
var
  error         : CFErrorRef;
  addressBook   : ABAddressBookRef;
  allPeople     : CFArrayRef;
  numberOfPeople: CFIndex;
  person        : ABRecordRef;
  firstName     : NSString;
  phoneNumber   : NSString;
  phoneNumbers  : ABMultiValueRef;
  lastName      : NSString;
  nn, i, j      : Integer;
begin
  error          := nil;
  addressBook    := ABAddressBookCreateWithOptions( nil, error );
  allPeople      := ABAddressBookCopyArrayOfAllPeople( addressBook );
  numberOfPeople := ABAddressBookGetPersonCount( addressBook );

  SetLength( Result, numberOfPeople );
  for i := 0 to numberOfPeople - 1 do
  begin

    person := CFArrayGetValueAtIndex( allPeople, i );

    firstName    := TNSString.Wrap( ABRecordCopyValue( person, { kABPersonFirstNameProperty } 0 ) );
    lastName     := TNSString.Wrap( ABRecordCopyValue( person, { kABPersonLastNameProperty } 1 ) );
    phoneNumbers := ABRecordCopyValue( person, { kABPersonPhoneProperty } 3 );
    nn           := ABMultiValueGetCount( phoneNumbers );

    Result[i].FirstName := UTF8ToString( firstName.UTF8String );
    Result[i].LastName  := UTF8ToString( LastName.UTF8String );
    SetLength( Result[i].Phones, nn );

    for j := 0 to nn - 1 do
    begin
      phoneNumber         := TNSString.Wrap( ABMultiValueCopyValueAtIndex( phoneNumbers, j ) );
      Result[i].Phones[j] := UTF8ToString( phoneNumber.UTF8String );
    end;

  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFAddressBook.RequestAccess;

var
  AddressBook: ABAddressBookRef;
  AStatus    : ABAuthorizationStatus;
  // grant                : Boolean;
  isClicked            : Boolean;
  FNSDefaultRunLoopMode: NSString;
begin
{$IFDEF IOS6}
  if TOSVersion.Major >= 6.0 then
  begin
    AStatus := ABAddressBookGetAuthorizationStatus( );
    if AStatus = kABAuthorizationStatusNotDetermined then
    begin
      AddressBook := ABAddressBookCreateWithOptions( nil, nil );
      ABAddressBookRequestAccessWithCompletion( AddressBook, nil );

      FNSDefaultRunLoopMode := NSDefaultRunLoopMode;
      isClicked             := false;
      while not isClicked do
      begin
        TNSRunLoop.Wrap( TNSRunLoop.OCClass.currentRunLoop ).runMode( FNSDefaultRunLoopMode, TNSDate.Wrap( TNSDate.OCClass.dateWithTimeIntervalSinceNow( 0.1 ) ) );
        AStatus   := ABAddressBookGetAuthorizationStatus( );
        isClicked := AStatus <> kABAuthorizationStatusNotDetermined;
      end;

      if AddressBook <> nil then
        CFRelease( AddressBook );
      RequestAccess;
    end
    else
    begin
      if Assigned( FOnRequestAccess ) then
        FOnRequestAccess( self, true )
    end;
  end
  else
  begin
    if Assigned( FOnRequestAccess ) then
      FOnRequestAccess( self, true )
  end;
{$ELSE}
  if Assigned( FOnRequestAccess ) then
    FOnRequestAccess( self, true )
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFAddressBook.ShowAddressBook: Boolean;
var
  AStatus    : ABAuthorizationStatus;
  FMainWindow: UIWindow;
begin
  result := false;
{$IFDEF IOS6}
  if TOSVersion.Major >= 6.0 then
  begin
    AStatus := ABAddressBookGetAuthorizationStatus( );
    if AStatus <> kABAuthorizationStatusAuthorized then
      exit;
  end;
{$ENDIF}
  FPeoplePicker := TABPeoplePickerNavigationController.Create;
  if not Assigned( FPeoplePicker ) then
    exit;

  // FPeoplePickerViewControllerDelegate := TPeoplePickerViewControllerDelegate.Create( self );
  FPeoplePicker.setPeoplePickerDelegate( FPeoplePickerViewControllerDelegate );

  (*
    // Display only a person's phone, email, and birthdate
    NSArray *displayedItems = [NSArray arrayWithObjects:
    [NSNumber numberWithInt:kABPersonPhoneProperty],
    [NSNumber numberWithInt:kABPersonEmailProperty],
    [NSNumber numberWithInt:kABPersonBirthdayProperty], nil];
    picker.displayedProperties = displayedItems;
  *)

  FMainWindow := GetSharedApplication.keyWindow;
  if Assigned( FMainWindow ) and Assigned( FMainWindow.rootViewController ) then
    FMainWindow.rootViewController.presentViewController( FPeoplePicker, true, ViewControllerCompletion );
  result := true;
end;

// ------------------------------------------------------------------------------
{ TPeoplePickerViewControllerDelegate }
constructor TPeoplePickerViewControllerDelegate.Create( DPFAddressBook: TDPFAddressBook );
begin
  inherited Create;
  FDPFAddressBook := DPFAddressBook;
end;

// ------------------------------------------------------------------------------
procedure TPeoplePickerViewControllerDelegate.SetPropertyValue( PropName, ProbValue: string; var PersonRecord: TPersonRecord );
begin
  if SameText( PropName, 'First' ) then
    PersonRecord.FirstName := ProbValue
  else if SameText( PropName, 'Last' ) then
    PersonRecord.LastName := ProbValue
  else if SameText( PropName, 'Middle' ) then
    PersonRecord.MiddleName := ProbValue
  else if SameText( PropName, 'Prefix' ) then
    PersonRecord.Prefix := ProbValue
  else if SameText( PropName, 'Suffix' ) then
    PersonRecord.Suffix := ProbValue
  else if SameText( PropName, 'Nickname' ) then
    PersonRecord.Nickname := ProbValue
  else if SameText( PropName, 'Organization' ) then
    PersonRecord.Organization := ProbValue
  else if SameText( PropName, 'Company' ) then
    PersonRecord.Company := ProbValue
  else if SameText( PropName, 'Job Title' ) then
    PersonRecord.JobTitle := ProbValue
  else if SameText( PropName, 'Department' ) then
    PersonRecord.Department := ProbValue
  else if SameText( PropName, 'Birthday' ) then
    PersonRecord.Birthday := ProbValue
  else if SameText( PropName, 'CreationDate' ) then
    PersonRecord.CreationDate := ProbValue
  else if SameText( PropName, 'URL' ) then
  begin
    SetLength( PersonRecord.URL, Length( PersonRecord.URL ) + 1 );
    PersonRecord.URL[high( PersonRecord.URL )] := ProbValue;
  end
  else if SameText( PropName, 'Notes' ) then
  begin
    SetLength( PersonRecord.Notes, Length( PersonRecord.Notes ) + 1 );
    PersonRecord.Notes[high( PersonRecord.Notes )] := ProbValue;
  end
  else if SameText( PropName, 'Email' ) then
  begin
    SetLength( PersonRecord.Emails, Length( PersonRecord.Emails ) + 1 );
    PersonRecord.Emails[high( PersonRecord.Emails )] := ProbValue;
  end
  else if SameText( PropName, 'Phone' ) then
  begin
    SetLength( PersonRecord.Phones, Length( PersonRecord.Phones ) + 1 );
    PersonRecord.Phones[high( PersonRecord.Phones )] := ProbValue;
  end
  else
    PersonRecord.CreationDate := ProbValue;
end;

// ------------------------------------------------------------------------------
procedure TPeoplePickerViewControllerDelegate.SetAddressFields( NSD: NSDictionary; var Address: TAddress );
var
  IdxDic: Integer;
  Key   : NSString;
begin
  for IdxDic := 0 to NSD.count - 1 do
  begin
    Key := TNSString.Wrap( NSD.allKeys.objectAtIndex( IdxDic ) );
    if SameText( UTF8ToString( Key.UTF8String ), 'Country' ) then
      Address.Country := UTF8ToString( TNSString.Wrap( NSD.valueForKey( Key ) ).UTF8String )
    else if SameText( UTF8ToString( Key.UTF8String ), 'City' ) then
      Address.City := UTF8ToString( TNSString.Wrap( NSD.valueForKey( Key ) ).UTF8String )
    else if SameText( UTF8ToString( Key.UTF8String ), 'Street' ) then
      Address.Street := UTF8ToString( TNSString.Wrap( NSD.valueForKey( Key ) ).UTF8String )
    else if SameText( UTF8ToString( Key.UTF8String ), 'Zip' ) then
      Address.Zip := UTF8ToString( TNSString.Wrap( NSD.valueForKey( Key ) ).UTF8String )
    else if SameText( UTF8ToString( Key.UTF8String ), 'CountryCode' ) then
      Address.CountryCode := UTF8ToString( TNSString.Wrap( NSD.valueForKey( Key ) ).UTF8String )
    else if SameText( UTF8ToString( Key.UTF8String ), 'State' ) then
      Address.State := UTF8ToString( TNSString.Wrap( NSD.valueForKey( Key ) ).UTF8String )
  end;
end;

// ------------------------------------------------------------------------------
function TPeoplePickerViewControllerDelegate.ScanPersonProperties( Person: ABRecordRef; var PersonRec: TPersonRecord; OnlyThisIndex: Integer = -1 ): string;
var
  propertyIndex: Integer;
  P1, P2       : ABRecordRef;
  IdxVal, Cnt  : Integer;
  NSD          : NSDictionary;
begin
  P1 := nil;
  if OnlyThisIndex <> -1 then
    propertyIndex := OnlyThisIndex
  else
    propertyIndex := 0;
  while true do
  begin
    Result := UTF8ToString( TNSString.Wrap( ABPersonCopyLocalizedPropertyName( propertyIndex ) ).UTF8String );
    if SameText( Result, 'UNKNOWN_PROPERTY' ) then
      break;

    case ABPersonGetTypeOfProperty( propertyIndex ) of
      kABMultiDictionaryPropertyType: // Addresses
        begin
          P1  := ABRecordCopyValue( Person, propertyIndex );
          Cnt := ABMultiValueGetCount( P1 );
          if Cnt > 0 then
          begin
            for IdxVal := 0 to Cnt do
            begin
              P2 := ABMultiValueCopyValueAtIndex( P1, IdxVal );
              if Assigned( P2 ) then
              begin
                SetLength( PersonRec.Addresses, Length( PersonRec.Addresses ) + 1 );
                NSD := TNSDictionary.Wrap( P2 );
                if SameText( Result, 'Address' ) then
                  SetAddressFields( NSD, PersonRec.Addresses[high( PersonRec.Addresses )] );
                CFRelease( P2 );
              end;
            end;
          end;
        end;
      kABDictionaryPropertyType: // Address
        begin
          P1  := ABRecordCopyValue( Person, propertyIndex );
          NSD := TNSDictionary.Wrap( P1 );
          if SameText( Result, 'Address' ) then
            SetAddressFields( NSD, PersonRec.Addresses[high( PersonRec.Addresses )] );
        end;
      kABStringPropertyType:
        begin
          P1 := ABRecordCopyValue( Person, propertyIndex );
          if Assigned( P1 ) then
            SetPropertyValue( Result, UTF8ToString( TNSString.Wrap( P1 ).UTF8String ), PersonRec );
        end;
      kABDateTimePropertyType:
        begin
          P1 := ABRecordCopyValue( Person, propertyIndex );
          if Assigned( P1 ) then
            SetPropertyValue( Result, DateTimeToStr( NSDateToDateTime( TNSDate.Wrap( P1 ) ) ), PersonRec );
        end;
      kABMultiStringProperty:
        // Email, Phone, Ringtone, URL, Related People, ...
        begin
          P1  := ABRecordCopyValue( Person, propertyIndex );
          Cnt := ABMultiValueGetCount( P1 );
          if Cnt > 0 then
          begin
            for IdxVal := 0 to Cnt do
            begin
              P2 := ABMultiValueCopyValueAtIndex( P1, IdxVal );
              if Assigned( P2 ) then
              begin
                SetPropertyValue( Result, UTF8ToString( TNSString.Wrap( P2 ).UTF8String ), PersonRec );
                CFRelease( P2 );
              end;
            end;
          end;
        end;
      kABMultiDateProperty: // date,...
        begin
        end;
      kABDataProperty: // Image, ...
        begin
          P1 := ABRecordCopyValue( Person, propertyIndex );

          PersonRec.Image := TUIImage.Wrap( TUIImage.OCClass.imageWithData( TNSData.Wrap( P1 ) ) );
        end;

    end;
    if Assigned( P1 ) then
    begin
      CFRelease( P1 );
      P1 := nil;
    end;
    Inc( propertyIndex );
    if OnlyThisIndex <> -1 then
      break;
  end;

end;

// ------------------------------------------------------------------------------
function TPeoplePickerViewControllerDelegate.peoplePickerNavigationController( peoplePicker: ABPeoplePickerNavigationController; shouldContinueAfterSelectingPerson: ABRecordRef ): Boolean;
var
  ShowUserDetail: Boolean;
  Prsn          : TPersonRecord;
begin
  ScanPersonProperties( shouldContinueAfterSelectingPerson, Prsn );
  ShowUserDetail := false;
  if Assigned( FDPFAddressBook.FOnPersonSelected ) then
    FDPFAddressBook.FOnPersonSelected( FDPFAddressBook, Prsn, ShowUserDetail );
  Result := ShowUserDetail;
end;

// ------------------------------------------------------------------------------
function TPeoplePickerViewControllerDelegate.peoplePickerNavigationController( peoplePicker: ABPeoplePickerNavigationController; shouldContinueAfterSelectingPerson: ABRecordRef; &property: ABPropertyID; identifier: ABMultiValueIdentifier ): Boolean;
var
  CloseAddressBook     : Boolean;
  Prsn                 : TPersonRecord;
  ThisIndexPropertyName: string;
begin

  ThisIndexPropertyName := ScanPersonProperties( shouldContinueAfterSelectingPerson, Prsn, &property );

  CloseAddressBook := true;
  if Assigned( FDPFAddressBook.FOnPersonSelectedDetail ) then
    FDPFAddressBook.FOnPersonSelectedDetail( FDPFAddressBook, Prsn, ThisIndexPropertyName, CloseAddressBook );

  if CloseAddressBook then
    FDPFAddressBook.FPeoplePicker.dismissModalViewControllerAnimated( true );
  Result := CloseAddressBook;
end;

// ------------------------------------------------------------------------------
procedure TPeoplePickerViewControllerDelegate.peoplePickerNavigationControllerDidCancel( peoplePickerend: ABPeoplePickerNavigationController ); cdecl;
var
  CloseAddressBook: Boolean;
begin
  CloseAddressBook := true;
  if Assigned( FDPFAddressBook.FOnPersonSelectedCancel ) then
    FDPFAddressBook.FOnPersonSelectedCancel( FDPFAddressBook, CloseAddressBook );
  if CloseAddressBook then
    FDPFAddressBook.FPeoplePicker.dismissModalViewControllerAnimated( true );
end;

// ------------------------------------------------------------------------------
{ TABPersonViewControllerDelegate }

constructor TABPersonViewControllerDelegate.Create( DPFMailCompose: TDPFAddressBook );
begin
  inherited Create;
end;

// ------------------------------------------------------------------------------
function TABPersonViewControllerDelegate.personViewController( personViewController: ABPersonViewController; shouldPerformDefaultActionForPerson: ABRecordRef; &property: ABPropertyID; identifier: ABMultiValueIdentifier ): Boolean;
begin
  result := true;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
