unit LocationApiLib_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 8291 $
// File generated on 03/03/2014 15:55:30 from Type Library described below.
// 7 March 2013 - ANGUS - added SENSOR_DATA and SENSOR_TYPE literals
//                       cleaned up ILocationReport.GetValue

// ************************************************************************  //
// Type Lib: C:\Windows\System32\LocationApi.dll (1)
// LIBID: {4486DF98-22A5-4F6B-BD5C-8CADCEC0A6DE}
// LCID: 0
// Helpfile: 
// HelpString: LocationApi 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// Errors:
//   Hint: Symbol 'type' renamed to 'type_'
//   Error creating palette bitmap of (TLocation) : Server C:\Windows\System32\LocationApi.dll contains no icons
//   Error creating palette bitmap of (TDefaultLocation) : Server C:\Windows\System32\LocationApi.dll contains no icons
//   Error creating palette bitmap of (TLatLongReportFactory) : Server C:\Windows\System32\LocationApi.dll contains no icons
//   Error creating palette bitmap of (TCivicAddressReportFactory) : Server C:\Windows\System32\LocationApi.dll contains no icons
//   Error creating palette bitmap of (TDispLatLongReport) : Server C:\Windows\System32\LocationApi.dll contains no icons
//   Error creating palette bitmap of (TDispCivicAddressReport) : Server C:\Windows\System32\LocationApi.dll contains no icons
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, OleServer, StdVCL, Variants;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  LocationApiLibMajorVersion = 1;
  LocationApiLibMinorVersion = 0;

  LIBID_LocationApiLib: TGUID = '{4486DF98-22A5-4F6B-BD5C-8CADCEC0A6DE}';

  IID_ILocation: TGUID = '{AB2ECE69-56D9-4F28-B525-DE1B0EE44237}';
  CLASS_Location: TGUID = '{E5B8E079-EE6D-4E33-A438-C87F2E959254}';
  IID_ILocationEvents: TGUID = '{CAE02BBF-798B-4508-A207-35A7906DC73D}';
  IID_ILocationReport: TGUID = '{C8B7F7EE-75D0-4DB9-B62D-7A0F369CA456}';
  IID_ISequentialStream: TGUID = '{0C733A30-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IStream: TGUID = '{0000000C-0000-0000-C000-000000000046}';
  IID_IStorage: TGUID = '{0000000B-0000-0000-C000-000000000046}';
  IID_IEnumSTATSTG: TGUID = '{0000000D-0000-0000-C000-000000000046}';
  IID_IRecordInfo: TGUID = '{0000002F-0000-0000-C000-000000000046}';
  IID_ITypeInfo: TGUID = '{00020401-0000-0000-C000-000000000046}';
  IID_ITypeComp: TGUID = '{00020403-0000-0000-C000-000000000046}';
  IID_ITypeLib: TGUID = '{00020402-0000-0000-C000-000000000046}';
  IID_IDefaultLocation: TGUID = '{A65AF77E-969A-4A2E-8ACA-33BB7CBB1235}';
  CLASS_DefaultLocation: TGUID = '{8B7FBFE0-5CD7-494A-AF8C-283A65707506}';
  IID_ILatLongReport: TGUID = '{7FED806D-0EF8-4F07-80AC-36A0BEAE3134}';
  CLASS_LatLongReport: TGUID = '{ED81C073-1F84-4CA8-A161-183C776BC651}';
  IID_ICivicAddressReport: TGUID = '{C0B19F70-4ADF-445D-87F2-CAD8FD711792}';
  CLASS_CivicAddressReport: TGUID = '{D39E7BDD-7D05-46B8-8721-80CF035F57D7}';
  DIID__ILatLongReportFactoryEvents: TGUID = '{16EE6CB7-AB3C-424B-849F-269BE551FCBC}';
  IID_IDispLatLongReport: TGUID = '{8AE32723-389B-4A11-9957-5BDD48FC9617}';
  DIID__ICivicAddressReportFactoryEvents: TGUID = '{C96039FF-72EC-4617-89BD-84D88BEDC722}';
  IID_IDispCivicAddressReport: TGUID = '{16FF1A34-9E30-42C3-B44D-E22513B5767A}';
  IID_ILocationReportFactory: TGUID = '{2DAEC322-90B2-47E4-BB08-0DA841935A6B}';
  IID_ILatLongReportFactory: TGUID = '{3F0804CB-B114-447D-83DD-390174EBB082}';
  CLASS_LatLongReportFactory: TGUID = '{9DCC3CC8-8609-4863-BAD4-03601F4C65E8}';
  IID_ICivicAddressReportFactory: TGUID = '{BF773B93-C64F-4BEE-BEB2-67C0B8DF66E0}';
  CLASS_CivicAddressReportFactory: TGUID = '{2A11F42C-3E81-4AD4-9CBE-45579D89671A}';
  CLASS_DispLatLongReport: TGUID = '{7A7C3277-8F84-4636-95B2-EBB5507FF77E}';
  CLASS_DispCivicAddressReport: TGUID = '{4C596AEC-8544-4082-BA9F-EB0A7D8E65C6}';

{ Location Sensor Types }
  SENSOR_TYPE_LOCATION_GPS: TGUID = '{ED4CA589-327A-4FF9-A560-91DA4B48275E}';
  SENSOR_TYPE_LOCATION_STATIC: TGUID = '{095F8184-0FA9-4445-8E6E-B70F320B6B4C}';
  SENSOR_TYPE_LOCATION_LOOKUP: TGUID = '{3B2EAE4A-72CE-436D-96D2-3C5B8570E987}';
  SENSOR_TYPE_LOCATION_TRIANGULATION: TGUID = '{691C341A-5406-4FE1-942F-2246CBEB39E0}';
  SENSOR_TYPE_LOCATION_OTHER: TGUID = '{9B2D0566-0368-4F71-B88D-533F132031DE}';
  SENSOR_TYPE_LOCATION_BROADCAST: TGUID = '{D26988CF-5162-4039-BB17-4C58B698E44A}';
  SENSOR_TYPE_LOCATION_DEAD_RECKONING: TGUID = '{1A37D538-F28B-42DA-9FCE-A9D0A2A6D829}';

const
{ Location Sensor Data Types }

  SENSOR_DATA_TYPE_LOCATION_GUID: TGUID = '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}';
  {$EXTERNALSYM SENSOR_DATA_TYPE_LOCATION_GUID}
{ SENSOR_DATA_TYPE_LATITUDE: Degrees latitude where North is positive }
  SENSOR_DATA_TYPE_LATITUDE_DEGREES: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 2);
  {$EXTERNALSYM SENSOR_DATA_TYPE_LATITUDE_DEGREES}
{ SENSOR_DATA_TYPE_LONGITUDE: Degrees longitude where East is positive }
  SENSOR_DATA_TYPE_LONGITUDE_DEGREES: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 3);
  {$EXTERNALSYM SENSOR_DATA_TYPE_LONGITUDE_DEGREES}
{ SENSOR_DATA_TYPE_ALTITUDE_SEALEVEL_METERS: Altitude with regards to sea level, in meters }
  SENSOR_DATA_TYPE_ALTITUDE_SEALEVEL_METERS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 4);
  {$EXTERNALSYM SENSOR_DATA_TYPE_ALTITUDE_SEALEVEL_METERS}
{ SENSOR_DATA_TYPE_ALTITUDE_ELLIPSOID_METERS: Altitude with regards to ellipsoid, in meters }
  SENSOR_DATA_TYPE_ALTITUDE_ELLIPSOID_METERS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 5);
  {$EXTERNALSYM SENSOR_DATA_TYPE_ALTITUDE_ELLIPSOID_METERS}
{ SENSOR_DATA_TYPE_SPEED_KNOTS: Speed measured in knots }
  SENSOR_DATA_TYPE_SPEED_KNOTS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 6);
  {$EXTERNALSYM SENSOR_DATA_TYPE_SPEED_KNOTS}
{ SENSOR_DATA_TYPE_TRUE_HEADING_DEGREES: Heading relative to true North in degrees }
  SENSOR_DATA_TYPE_TRUE_HEADING_DEGREES: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 7);
  {$EXTERNALSYM SENSOR_DATA_TYPE_TRUE_HEADING_DEGREES}
{ SENSOR_DATA_TYPE_MAGNETIC_HEADING_DEGREES: Heading relative to magnetic North in degrees }
  SENSOR_DATA_TYPE_MAGNETIC_HEADING_DEGREES: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 8);
  {$EXTERNALSYM SENSOR_DATA_TYPE_MAGNETIC_HEADING_DEGREES}
{ SENSOR_DATA_TYPE_MAGNETIC_VARIATION: Magnetic variation. East is positive }
  SENSOR_DATA_TYPE_MAGNETIC_VARIATION: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 9);
  {$EXTERNALSYM SENSOR_DATA_TYPE_MAGNETIC_VARIATION}
{ SENSOR_DATA_TYPE_FIX_QUALITY: Quality of fix }
  SENSOR_DATA_TYPE_FIX_QUALITY: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 10);
  {$EXTERNALSYM SENSOR_DATA_TYPE_FIX_QUALITY}
{ SENSOR_DATA_TYPE_FIX_TYPE: Fix Type }
  SENSOR_DATA_TYPE_FIX_TYPE: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 11);
  {$EXTERNALSYM SENSOR_DATA_TYPE_FIX_TYPE}
{ SENSOR_DATA_TYPE_POSITION_DILUTION_OF_PRECISION: Position Dilution of Precision }
  SENSOR_DATA_TYPE_POSITION_DILUTION_OF_PRECISION: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 12);
  {$EXTERNALSYM SENSOR_DATA_TYPE_POSITION_DILUTION_OF_PRECISION}
{ SENSOR_DATA_TYPE_HORIZONAL_DILUTION_OF_PRECISION: HORIZONTAL Dilution of Precision }
  SENSOR_DATA_TYPE_HORIZONAL_DILUTION_OF_PRECISION: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 13);
  {$EXTERNALSYM SENSOR_DATA_TYPE_HORIZONAL_DILUTION_OF_PRECISION}
{ SENSOR_DATA_TYPE_VERTICAL_DILUTION_OF_PRECISION: VERTICAL Dilution of Precision }
  SENSOR_DATA_TYPE_VERTICAL_DILUTION_OF_PRECISION: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 14);
  {$EXTERNALSYM SENSOR_DATA_TYPE_VERTICAL_DILUTION_OF_PRECISION}
{ SENSOR_DATA_TYPE_SATELLITES_USED_COUNT: Number of satellites used in solution }
  SENSOR_DATA_TYPE_SATELLITES_USED_COUNT: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 15);
  {$EXTERNALSYM SENSOR_DATA_TYPE_SATELLITES_USED_COUNT}
{ SENSOR_DATA_TYPE_SATELLITES_USED_PRNS: PRN numbers of satellites used in the solution }
  SENSOR_DATA_TYPE_SATELLITES_USED_PRNS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 16);
  {$EXTERNALSYM SENSOR_DATA_TYPE_SATELLITES_USED_PRNS}
{ SENSOR_DATA_TYPE_SATELLITES_IN_VIEW: Number of satellites in view.  From 0-GPS_MAX_SATELLITES }
  SENSOR_DATA_TYPE_SATELLITES_IN_VIEW: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 17);
  {$EXTERNALSYM SENSOR_DATA_TYPE_SATELLITES_IN_VIEW}
{ SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_PRNS: PRN numbers of satellites in view }
  SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_PRNS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 18);
  {$EXTERNALSYM SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_PRNS}
{ SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_ELEVATION: Elevation of each sattellite in view }
  SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_ELEVATION: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 19);
  {$EXTERNALSYM SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_ELEVATION}
{ SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_AZIMUTH: Azimuth of each satellite in view }
  SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_AZIMUTH: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 20);
  {$EXTERNALSYM SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_AZIMUTH}
{ SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_STN_RATIO: Signal to noise ratio for each satellite in view }
  SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_STN_RATIO: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 21);
  {$EXTERNALSYM SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_STN_RATIO}
{ SENSOR_DATA_TYPE_ERROR_RADIUS_METERS: Accuracy of Latitude and Longitude values }
  SENSOR_DATA_TYPE_ERROR_RADIUS_METERS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 22);
  {$EXTERNALSYM SENSOR_DATA_TYPE_ERROR_RADIUS_METERS}
{ SENSOR_DATA_TYPE_ADDRESS1: AddressLine1 }
  SENSOR_DATA_TYPE_ADDRESS1: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 23);
  {$EXTERNALSYM SENSOR_DATA_TYPE_ADDRESS1}
{ SENSOR_DATA_TYPE_ADDRESS2: AddressLine2 }
  SENSOR_DATA_TYPE_ADDRESS2: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 24);
  {$EXTERNALSYM SENSOR_DATA_TYPE_ADDRESS2}
{ SENSOR_DATA_TYPE_CITY: City }
  SENSOR_DATA_TYPE_CITY: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 25);
  {$EXTERNALSYM SENSOR_DATA_TYPE_CITY}
{ SENSOR_DATA_TYPE_STATE_PROVINCE: State/Province }
  SENSOR_DATA_TYPE_STATE_PROVINCE: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 26);
  {$EXTERNALSYM SENSOR_DATA_TYPE_STATE_PROVINCE}
{ SENSOR_DATA_TYPE_POSTALCODE: Postal Code (e.g. ZIP) }
  SENSOR_DATA_TYPE_POSTALCODE: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 27);
  {$EXTERNALSYM SENSOR_DATA_TYPE_POSTALCODE}
{ SENSOR_DATA_TYPE_COUNTRY_REGION: Country/Region }
  SENSOR_DATA_TYPE_COUNTRY_REGION: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 28);
  {$EXTERNALSYM SENSOR_DATA_TYPE_COUNTRY_REGION}
{ SENSOR_DATA_TYPE_ALTITUDE_ELLIPSOID_ERROR_METERS: Altitude Error with regards to ellipsoid, in meters }
  SENSOR_DATA_TYPE_ALTITUDE_ELLIPSOID_ERROR_METERS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 29);
  {$EXTERNALSYM SENSOR_DATA_TYPE_ALTITUDE_ELLIPSOID_ERROR_METERS}
{ SENSOR_DATA_TYPE_ALTITUDE_SEALEVEL_ERROR_METERS: Altitude Error with regards to sea level, in meters }
  SENSOR_DATA_TYPE_ALTITUDE_SEALEVEL_ERROR_METERS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 30);
  {$EXTERNALSYM SENSOR_DATA_TYPE_ALTITUDE_SEALEVEL_ERROR_METERS}
{ SENSOR_DATA_TYPE_GPS_SELECTION_MODE: }
  SENSOR_DATA_TYPE_GPS_SELECTION_MODE: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 31);
  {$EXTERNALSYM SENSOR_DATA_TYPE_GPS_SELECTION_MODE}
{ SENSOR_DATA_TYPE_GPS_OPERATION_MODE: }
  SENSOR_DATA_TYPE_GPS_OPERATION_MODE: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 32);
  {$EXTERNALSYM SENSOR_DATA_TYPE_GPS_OPERATION_MODE}
{ SENSOR_DATA_TYPE_GPS_STATUS: }
  SENSOR_DATA_TYPE_GPS_STATUS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 33);
  {$EXTERNALSYM SENSOR_DATA_TYPE_GPS_STATUS}
{ SENSOR_DATA_TYPE_GEOIDAL_SEPARATION: }
  SENSOR_DATA_TYPE_GEOIDAL_SEPARATION: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 34);
  {$EXTERNALSYM SENSOR_DATA_TYPE_GEOIDAL_SEPARATION}
{ SENSOR_DATA_TYPE_DGPS_DATA_AGE: }
  SENSOR_DATA_TYPE_DGPS_DATA_AGE: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 35);
  {$EXTERNALSYM SENSOR_DATA_TYPE_DGPS_DATA_AGE}
{ SENSOR_DATA_TYPE_ALTITUDE_ANTENNA_SEALEVEL_METERS: }
  SENSOR_DATA_TYPE_ALTITUDE_ANTENNA_SEALEVEL_METERS: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 36);
  {$EXTERNALSYM SENSOR_DATA_TYPE_ALTITUDE_ANTENNA_SEALEVEL_METERS}
{ SENSOR_DATA_TYPE_DIFFERENTIAL_REFERENCE_STATION_ID: }
  SENSOR_DATA_TYPE_DIFFERENTIAL_REFERENCE_STATION_ID: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 37);
  {$EXTERNALSYM SENSOR_DATA_TYPE_DIFFERENTIAL_REFERENCE_STATION_ID}
{ SENSOR_DATA_TYPE_NMEA_SENTENCE: }
  SENSOR_DATA_TYPE_NMEA_SENTENCE: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 38);
  {$EXTERNALSYM SENSOR_DATA_TYPE_NMEA_SENTENCE}
{ SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_ID: }
  SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_ID: PROPERTYKEY = (
    fmtid: '{055C74D8-CA6F-47D6-95C6-1ED3637A0FF4}'; pid: 39);
  {$EXTERNALSYM SENSOR_DATA_TYPE_SATELLITES_IN_VIEW_ID}

{ Sensor Properties }


  SENSOR_PROPERTY_COMMON_GUID: TGUID = '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}';
  {$EXTERNALSYM SENSOR_PROPERTY_COMMON_GUID}
  SENSOR_PROPERTY_TYPE: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 2);
  {$EXTERNALSYM SENSOR_PROPERTY_TYPE}
  SENSOR_PROPERTY_STATE: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 3);
  {$EXTERNALSYM SENSOR_PROPERTY_STATE}
  SENSOR_PROPERTY_PERSISTENT_UNIQUE_ID: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 5);
  {$EXTERNALSYM SENSOR_PROPERTY_PERSISTENT_UNIQUE_ID}
  SENSOR_PROPERTY_MANUFACTURER: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 6);
  {$EXTERNALSYM SENSOR_PROPERTY_MANUFACTURER}
  SENSOR_PROPERTY_MODEL: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 7);
  {$EXTERNALSYM SENSOR_PROPERTY_MODEL}
  SENSOR_PROPERTY_SERIAL_NUMBER: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 8);
  {$EXTERNALSYM SENSOR_PROPERTY_SERIAL_NUMBER}
  SENSOR_PROPERTY_FRIENDLY_NAME: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 9);
  {$EXTERNALSYM SENSOR_PROPERTY_FRIENDLY_NAME}
  SENSOR_PROPERTY_DESCRIPTION: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 10);
  {$EXTERNALSYM SENSOR_PROPERTY_DESCRIPTION}
  SENSOR_PROPERTY_CONNECTION_TYPE: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 11);
  {$EXTERNALSYM SENSOR_PROPERTY_CONNECTION_TYPE}
  SENSOR_PROPERTY_MIN_REPORT_INTERVAL: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 12);
  {$EXTERNALSYM SENSOR_PROPERTY_MIN_REPORT_INTERVAL}
  SENSOR_PROPERTY_CURRENT_REPORT_INTERVAL: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 13);
  {$EXTERNALSYM SENSOR_PROPERTY_CURRENT_REPORT_INTERVAL}
  SENSOR_PROPERTY_CHANGE_SENSITIVITY: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 14);
  {$EXTERNALSYM SENSOR_PROPERTY_CHANGE_SENSITIVITY}
  SENSOR_PROPERTY_DEVICE_PATH: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 15);
  {$EXTERNALSYM SENSOR_PROPERTY_DEVICE_PATH}
  SENSOR_PROPERTY_LIGHT_RESPONSE_CURVE: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 16);
  {$EXTERNALSYM SENSOR_PROPERTY_LIGHT_RESPONSE_CURVE}
  SENSOR_PROPERTY_ACCURACY: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 17);
  {$EXTERNALSYM SENSOR_PROPERTY_ACCURACY}
  SENSOR_PROPERTY_RESOLUTION: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 18);
  {$EXTERNALSYM SENSOR_PROPERTY_RESOLUTION}
  SENSOR_PROPERTY_LOCATION_DESIRED_ACCURACY: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 19);
  {$EXTERNALSYM SENSOR_PROPERTY_LOCATION_DESIRED_ACCURACY}
  SENSOR_PROPERTY_RANGE_MINIMUM: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 20);
  {$EXTERNALSYM SENSOR_PROPERTY_RANGE_MINIMUM}
  SENSOR_PROPERTY_RANGE_MAXIMUM: PROPERTYKEY = (
    fmtid: '{7F8383EC-D3EC-495C-A8CF-B8BBE85C2920}'; pid: 21);
  {$EXTERNALSYM SENSOR_PROPERTY_RANGE_MAXIMUM}

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum tagTYPEKIND
type
  tagTYPEKIND = TOleEnum;
const
  TKIND_ENUM = $00000000;
  TKIND_RECORD = $00000001;
  TKIND_MODULE = $00000002;
  TKIND_INTERFACE = $00000003;
  TKIND_DISPATCH = $00000004;
  TKIND_COCLASS = $00000005;
  TKIND_ALIAS = $00000006;
  TKIND_UNION = $00000007;
  TKIND_MAX = $00000008;

// Constants for enum tagDESCKIND
type
  tagDESCKIND = TOleEnum;
const
  DESCKIND_NONE = $00000000;
  DESCKIND_FUNCDESC = $00000001;
  DESCKIND_VARDESC = $00000002;
  DESCKIND_TYPECOMP = $00000003;
  DESCKIND_IMPLICITAPPOBJ = $00000004;
  DESCKIND_MAX = $00000005;

// Constants for enum tagFUNCKIND
type
  tagFUNCKIND = TOleEnum;
const
  FUNC_VIRTUAL = $00000000;
  FUNC_PUREVIRTUAL = $00000001;
  FUNC_NONVIRTUAL = $00000002;
  FUNC_STATIC = $00000003;
  FUNC_DISPATCH = $00000004;

// Constants for enum tagINVOKEKIND
type
  tagINVOKEKIND = TOleEnum;
const
  INVOKE_FUNC = $00000001;
  INVOKE_PROPERTYGET = $00000002;
  INVOKE_PROPERTYPUT = $00000004;
  INVOKE_PROPERTYPUTREF = $00000008;

// Constants for enum tagCALLCONV
type
  tagCALLCONV = TOleEnum;
const
  CC_FASTCALL = $00000000;
  CC_CDECL = $00000001;
  CC_MSCPASCAL = $00000002;
  CC_PASCAL = $00000002;
  CC_MACPASCAL = $00000003;
  CC_STDCALL = $00000004;
  CC_FPFASTCALL = $00000005;
  CC_SYSCALL = $00000006;
  CC_MPWCDECL = $00000007;
  CC_MPWPASCAL = $00000008;
  CC_MAX = $00000009;

// Constants for enum tagVARKIND
type
  tagVARKIND = TOleEnum;
const
  VAR_PERINSTANCE = $00000000;
  VAR_STATIC = $00000001;
  VAR_CONST = $00000002;
  VAR_DISPATCH = $00000003;

// Constants for enum tagSYSKIND
type
  tagSYSKIND = TOleEnum;
const
  SYS_WIN16 = $00000000;
  SYS_WIN32 = $00000001;
  SYS_MAC = $00000002;
  SYS_WIN64 = $00000003;

// Constants for enum LOCATION_REPORT_STATUS
type
  LOCATION_REPORT_STATUS = TOleEnum;
const
  REPORT_NOT_SUPPORTED = $00000000;
  REPORT_ERROR = $00000001;
  REPORT_ACCESS_DENIED = $00000002;
  REPORT_INITIALIZING = $00000003;
  REPORT_RUNNING = $00000004;

// Constants for enum LOCATION_DESIRED_ACCURACY
type
  LOCATION_DESIRED_ACCURACY = TOleEnum;
const
  LOCATION_DESIRED_ACCURACY_DEFAULT = $00000000;
  LOCATION_DESIRED_ACCURACY_HIGH = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ILocation = interface;
  ILocationEvents = interface;
  ILocationReport = interface;
  ISequentialStream = interface;
  IStream = interface;
  IStorage = interface;
  IEnumSTATSTG = interface;
  IRecordInfo = interface;
  ITypeInfo = interface;
  ITypeComp = interface;
  ITypeLib = interface;
  IDefaultLocation = interface;
  ILatLongReport = interface;
  ICivicAddressReport = interface;
  _ILatLongReportFactoryEvents = dispinterface;
  IDispLatLongReport = interface;
  IDispLatLongReportDisp = dispinterface;
  _ICivicAddressReportFactoryEvents = dispinterface;
  IDispCivicAddressReport = interface;
  IDispCivicAddressReportDisp = dispinterface;
  ILocationReportFactory = interface;
  ILocationReportFactoryDisp = dispinterface;
  ILatLongReportFactory = interface;
  ILatLongReportFactoryDisp = dispinterface;
  ICivicAddressReportFactory = interface;
  ICivicAddressReportFactoryDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Location = ILocation;
  DefaultLocation = IDefaultLocation;
  LatLongReport = ILatLongReport;
  CivicAddressReport = ICivicAddressReport;
  LatLongReportFactory = ILatLongReportFactory;
  CivicAddressReportFactory = ICivicAddressReportFactory;
  DispLatLongReport = IDispLatLongReport;
  DispCivicAddressReport = IDispCivicAddressReport;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  wirePSAFEARRAY = ^PUserType5; 
  wireSNB = ^tagRemSNB; 
  PUserType6 = ^_FLAGGED_WORD_BLOB; {*}
  PUserType7 = ^_wireVARIANT; {*}
  PUserType14 = ^_wireBRECORD; {*}
  PUserType5 = ^_wireSAFEARRAY; {*}
  PPUserType1 = ^PUserType5; {*}
  PUserType11 = ^tagTYPEDESC; {*}
  PUserType12 = ^tagARRAYDESC; {*}
  PUserType3 = ^tag_inner_PROPVARIANT; {*}
  wireHWND = ^_RemotableHandle; 
  PUserType1 = ^TGUID; {*}
  PUserType2 = ^_tagpropertykey; {*}
  PByte1 = ^Byte; {*}
  PUserType4 = ^_FILETIME; {*}
  POleVariant1 = ^OleVariant; {*}
  PUserType8 = ^tagTYPEATTR; {*}
  PUserType9 = ^tagFUNCDESC; {*}
  PUserType10 = ^tagVARDESC; {*}
  PUserType13 = ^tagTLIBATTR; {*}
  PUINT1 = ^LongWord; {*}

  _SYSTEMTIME = packed record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;

  _tagpropertykey = packed record
    fmtid: TGUID;
    pid: LongWord;
  end;

  _LARGE_INTEGER = packed record
    QuadPart: Int64;
  end;

  _ULARGE_INTEGER = packed record
    QuadPart: Largeuint;
  end;

  _FILETIME = packed record
    dwLowDateTime: LongWord;
    dwHighDateTime: LongWord;
  end;

  tagCLIPDATA = packed record
    cbSize: LongWord;
    ulClipFmt: Integer;
    pClipData: ^Byte;
  end;

  tagBSTRBLOB = packed record
    cbSize: LongWord;
    pData: ^Byte;
  end;

  tagBLOB = packed record
    cbSize: LongWord;
    pBlobData: ^Byte;
  end;

  tagVersionedStream = packed record
    guidVersion: TGUID;
    pStream: IStream;
  end;


  tagSTATSTG = packed record
    pwcsName: PWideChar;
    type_: LongWord;
    cbSize: _ULARGE_INTEGER;
    mtime: _FILETIME;
    ctime: _FILETIME;
    atime: _FILETIME;
    grfMode: LongWord;
    grfLocksSupported: LongWord;
    clsid: TGUID;
    grfStateBits: LongWord;
    reserved: LongWord;
  end;


  tagRemSNB = packed record
    ulCntStr: LongWord;
    ulCntChar: LongWord;
    rgString: ^Word;
  end;

  tagCAC = packed record
    cElems: LongWord;
    pElems: ^Byte;
  end;

  tagCAUB = packed record
    cElems: LongWord;
    pElems: ^Byte;
  end;


  _wireSAFEARR_BSTR = packed record
    Size: LongWord;
    aBstr: ^PUserType6;
  end;

  _wireSAFEARR_UNKNOWN = packed record
    Size: LongWord;
    apUnknown: ^IUnknown;
  end;

  _wireSAFEARR_DISPATCH = packed record
    Size: LongWord;
    apDispatch: ^IDispatch;
  end;

  _FLAGGED_WORD_BLOB = packed record
    fFlags: LongWord;
    clSize: LongWord;
    asData: ^Word;
  end;


  _wireSAFEARR_VARIANT = packed record
    Size: LongWord;
    aVariant: ^PUserType7;
  end;


  _wireBRECORD = packed record
    fFlags: LongWord;
    clSize: LongWord;
    pRecInfo: IRecordInfo;
    pRecord: ^Byte;
  end;


  __MIDL_IOleAutomationTypes_0005 = record
    case Integer of
      0: (lptdesc: PUserType11);
      1: (lpadesc: PUserType12);
      2: (hreftype: LongWord);
  end;

  tagTYPEDESC = packed record
    DUMMYUNIONNAME: __MIDL_IOleAutomationTypes_0005;
    vt: Word;
  end;

  tagSAFEARRAYBOUND = packed record
    cElements: LongWord;
    lLbound: Integer;
  end;

  ULONG_PTR = LongWord; 

  tagIDLDESC = packed record
    dwReserved: ULONG_PTR;
    wIDLFlags: Word;
  end;

  DWORD = LongWord; 

  tagPARAMDESCEX = packed record
    cBytes: LongWord;
    varDefaultValue: OleVariant;
  end;

  tagPARAMDESC = packed record
    pparamdescex: ^tagPARAMDESCEX;
    wParamFlags: Word;
  end;

  tagELEMDESC = packed record
    tdesc: tagTYPEDESC;
    paramdesc: tagPARAMDESC;
  end;

  tagFUNCDESC = packed record
    memid: Integer;
    lprgscode: ^SCODE;
    lprgelemdescParam: ^tagELEMDESC;
    funckind: tagFUNCKIND;
    invkind: tagINVOKEKIND;
    callconv: tagCALLCONV;
    cParams: Smallint;
    cParamsOpt: Smallint;
    oVft: Smallint;
    cScodes: Smallint;
    elemdescFunc: tagELEMDESC;
    wFuncFlags: Word;
  end;

  __MIDL_IOleAutomationTypes_0006 = record
    case Integer of
      0: (oInst: LongWord);
      1: (lpvarValue: ^OleVariant);
  end;

  tagVARDESC = packed record
    memid: Integer;
    lpstrSchema: PWideChar;
    DUMMYUNIONNAME: __MIDL_IOleAutomationTypes_0006;
    elemdescVar: tagELEMDESC;
    wVarFlags: Word;
    varkind: tagVARKIND;
  end;

  tagTLIBATTR = packed record
    guid: TGUID;
    lcid: LongWord;
    syskind: tagSYSKIND;
    wMajorVerNum: Word;
    wMinorVerNum: Word;
    wLibFlags: Word;
  end;

  _wireSAFEARR_BRECORD = packed record
    Size: LongWord;
    aRecord: ^PUserType14;
  end;

  _wireSAFEARR_HAVEIID = packed record
    Size: LongWord;
    apUnknown: ^IUnknown;
    iid: TGUID;
  end;

  _BYTE_SIZEDARR = packed record
    clSize: LongWord;
    pData: ^Byte;
  end;

  _SHORT_SIZEDARR = packed record
    clSize: LongWord;
    pData: ^Word;
  end;

  _LONG_SIZEDARR = packed record
    clSize: LongWord;
    pData: ^LongWord;
  end;

  _HYPER_SIZEDARR = packed record
    clSize: LongWord;
    pData: ^Int64;
  end;

  tagCAI = packed record
    cElems: LongWord;
    pElems: ^Smallint;
  end;

  tagCAUI = packed record
    cElems: LongWord;
    pElems: ^Word;
  end;

  tagCAL = packed record
    cElems: LongWord;
    pElems: ^Integer;
  end;

  tagCAUL = packed record
    cElems: LongWord;
    pElems: ^LongWord;
  end;

  tagCAH = packed record
    cElems: LongWord;
    pElems: ^_LARGE_INTEGER;
  end;

  tagCAUH = packed record
    cElems: LongWord;
    pElems: ^_ULARGE_INTEGER;
  end;

  tagCAFLT = packed record
    cElems: LongWord;
    pElems: ^Single;
  end;

  tagCADBL = packed record
    cElems: LongWord;
    pElems: ^Double;
  end;

  tagCABOOL = packed record
    cElems: LongWord;
    pElems: ^WordBool;
  end;

  tagCASCODE = packed record
    cElems: LongWord;
    pElems: ^SCODE;
  end;

  tagCACY = packed record
    cElems: LongWord;
    pElems: ^Currency;
  end;

  tagCADATE = packed record
    cElems: LongWord;
    pElems: ^TDateTime;
  end;

  tagCAFILETIME = packed record
    cElems: LongWord;
    pElems: ^_FILETIME;
  end;

  tagCACLSID = packed record
    cElems: LongWord;
    pElems: ^TGUID;
  end;

  tagCACLIPDATA = packed record
    cElems: LongWord;
    pElems: ^tagCLIPDATA;
  end;

  tagCABSTR = packed record
    cElems: LongWord;
    pElems: ^WideString;
  end;

  tagCABSTRBLOB = packed record
    cElems: LongWord;
    pElems: ^tagBSTRBLOB;
  end;

  tagCALPSTR = packed record
    cElems: LongWord;
    pElems: ^PChar;
  end;

  tagCALPWSTR = packed record
    cElems: LongWord;
    pElems: ^PWideChar;
  end;


  tagCAPROPVARIANT = packed record
    cElems: LongWord;
    pElems: PUserType3;
  end;

  __MIDL___MIDL_itf_locationapi_0004_0039_0001 = record
    case Integer of
      0: (cVal: Byte);
      1: (bVal: Byte);
      2: (iVal: Smallint);
      3: (uiVal: Word);
      4: (lVal: Integer);
      5: (ulVal: LongWord);
      6: (intVal: SYSINT);
      7: (uintVal: SYSUINT);
      8: (hVal: _LARGE_INTEGER);
      9: (uhVal: _ULARGE_INTEGER);
      10: (fltVal: Single);
      11: (dblVal: Double);
      12: (boolVal: WordBool);
      13: (bool: WordBool);
      14: (scode: SCODE);
      15: (cyVal: Currency);
      16: (date: TDateTime);
      17: (filetime: _FILETIME);
      18: (puuid: ^TGUID);
      19: (pClipData: ^tagCLIPDATA);
      20: (bstrVal: {!!WideString}Pointer);
      21: (bstrblobVal: tagBSTRBLOB);
      22: (blob: tagBLOB);
      23: (pszVal: PChar);
      24: (pwszVal: PWideChar);
      25: (punkVal: {!!IUnknown}Pointer);
      26: (pdispVal: {!!IDispatch}Pointer);
      27: (pStream: {!!IStream}Pointer);
      28: (pStorage: {!!IStorage}Pointer);
      29: (pVersionedStream: ^tagVersionedStream);
      30: (parray: wirePSAFEARRAY);
      31: (cac: tagCAC);
      32: (caub: tagCAUB);
      33: (cai: tagCAI);
      34: (caui: tagCAUI);
      35: (cal: tagCAL);
      36: (caul: tagCAUL);
      37: (cah: tagCAH);
      38: (cauh: tagCAUH);
      39: (caflt: tagCAFLT);
      40: (cadbl: tagCADBL);
      41: (cabool: tagCABOOL);
      42: (cascode: tagCASCODE);
      43: (cacy: tagCACY);
      44: (cadate: tagCADATE);
      45: (cafiletime: tagCAFILETIME);
      46: (cauuid: tagCACLSID);
      47: (caclipdata: tagCACLIPDATA);
      48: (cabstr: tagCABSTR);
      49: (cabstrblob: tagCABSTRBLOB);
      50: (calpstr: tagCALPSTR);
      51: (calpwstr: tagCALPWSTR);
      52: (capropvar: tagCAPROPVARIANT);
      53: (pcVal: ^Byte);
      54: (pbVal: ^Byte);
      55: (piVal: ^Smallint);
      56: (puiVal: ^Word);
      57: (plVal: ^Integer);
      58: (pulVal: ^LongWord);
      59: (pintVal: ^SYSINT);
      60: (puintVal: ^SYSUINT);
      61: (pfltVal: ^Single);
      62: (pdblVal: ^Double);
      63: (pboolVal: ^WordBool);
      64: (pdecVal: ^TDecimal);
      65: (pscode: ^SCODE);
      66: (pcyVal: ^Currency);
      67: (pdate: ^TDateTime);
      68: (pbstrVal: ^WideString);
      69: (ppunkVal: {!!^IUnknown}Pointer);
      70: (ppdispVal: {!!^IDispatch}Pointer);
      71: (pparray: ^wirePSAFEARRAY);
      72: (pvarVal: PUserType3);
  end;


  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = packed record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;


  tag_inner_PROPVARIANT = packed record
    vt: Word;
    wReserved1: Byte;
    wReserved2: Byte;
    wReserved3: LongWord;
    __MIDL____MIDL_itf_locationapi_0004_00390001: __MIDL___MIDL_itf_locationapi_0004_0039_0001;
  end;


  __MIDL_IOleAutomationTypes_0004 = record
    case Integer of
      0: (llVal: Int64);
      1: (lVal: Integer);
      2: (bVal: Byte);
      3: (iVal: Smallint);
      4: (fltVal: Single);
      5: (dblVal: Double);
      6: (boolVal: WordBool);
      7: (scode: SCODE);
      8: (cyVal: Currency);
      9: (date: TDateTime);
      10: (bstrVal: ^_FLAGGED_WORD_BLOB);
      11: (punkVal: {!!IUnknown}Pointer);
      12: (pdispVal: {!!IDispatch}Pointer);
      13: (parray: ^PUserType5);
      14: (brecVal: ^_wireBRECORD);
      15: (pbVal: ^Byte);
      16: (piVal: ^Smallint);
      17: (plVal: ^Integer);
      18: (pllVal: ^Int64);
      19: (pfltVal: ^Single);
      20: (pdblVal: ^Double);
      21: (pboolVal: ^WordBool);
      22: (pscode: ^SCODE);
      23: (pcyVal: ^Currency);
      24: (pdate: ^TDateTime);
      25: (pbstrVal: ^PUserType6);
      26: (ppunkVal: {!!^IUnknown}Pointer);
      27: (ppdispVal: {!!^IDispatch}Pointer);
      28: (pparray: ^PPUserType1);
      29: (pvarVal: ^PUserType7);
      30: (cVal: Byte);
      31: (uiVal: Word);
      32: (ulVal: LongWord);
      33: (ullVal: Largeuint);
      34: (intVal: SYSINT);
      35: (uintVal: SYSUINT);
      36: (decVal: TDecimal);
      37: (pdecVal: ^TDecimal);
      38: (pcVal: ^Byte);
      39: (puiVal: ^Word);
      40: (pulVal: ^LongWord);
      41: (pullVal: ^Largeuint);
      42: (pintVal: ^SYSINT);
      43: (puintVal: ^SYSUINT);
  end;

  __MIDL_IOleAutomationTypes_0001 = record
    case Integer of
      0: (BstrStr: _wireSAFEARR_BSTR);
      1: (UnknownStr: _wireSAFEARR_UNKNOWN);
      2: (DispatchStr: _wireSAFEARR_DISPATCH);
      3: (VariantStr: _wireSAFEARR_VARIANT);
      4: (RecordStr: _wireSAFEARR_BRECORD);
      5: (HaveIidStr: _wireSAFEARR_HAVEIID);
      6: (ByteStr: _BYTE_SIZEDARR);
      7: (WordStr: _SHORT_SIZEDARR);
      8: (LongStr: _LONG_SIZEDARR);
      9: (HyperStr: _HYPER_SIZEDARR);
  end;

  _wireSAFEARRAY_UNION = packed record
    sfType: LongWord;
    u: __MIDL_IOleAutomationTypes_0001;
  end;

  _wireVARIANT = packed record
    clSize: LongWord;
    rpcReserved: LongWord;
    vt: Word;
    wReserved1: Word;
    wReserved2: Word;
    wReserved3: Word;
    DUMMYUNIONNAME: __MIDL_IOleAutomationTypes_0004;
  end;


  tagTYPEATTR = packed record
    guid: TGUID;
    lcid: LongWord;
    dwReserved: LongWord;
    memidConstructor: Integer;
    memidDestructor: Integer;
    lpstrSchema: PWideChar;
    cbSizeInstance: LongWord;
    typekind: tagTYPEKIND;
    cFuncs: Word;
    cVars: Word;
    cImplTypes: Word;
    cbSizeVft: Word;
    cbAlignment: Word;
    wTypeFlags: Word;
    wMajorVerNum: Word;
    wMinorVerNum: Word;
    tdescAlias: tagTYPEDESC;
    idldescType: tagIDLDESC;
  end;

  tagARRAYDESC = packed record
    tdescElem: tagTYPEDESC;
    cDims: Word;
    rgbounds: ^tagSAFEARRAYBOUND;
  end;


  _wireSAFEARRAY = packed record
    cDims: Word;
    fFeatures: Word;
    cbElements: LongWord;
    cLocks: LongWord;
    uArrayStructs: _wireSAFEARRAY_UNION;
    rgsabound: ^tagSAFEARRAYBOUND;
  end;


// *********************************************************************//
// Interface: ILocation
// Flags:     (0)
// GUID:      {AB2ECE69-56D9-4F28-B525-DE1B0EE44237}
// *********************************************************************//
  ILocation = interface(IUnknown)
    ['{AB2ECE69-56D9-4F28-B525-DE1B0EE44237}']
    function RegisterForReport(const pEvents: ILocationEvents; var reportType: TGUID; 
                               dwRequestedReportInterval: LongWord): HResult; stdcall;
    function UnregisterForReport(var reportType: TGUID): HResult; stdcall;
    function GetReport(var reportType: TGUID; out ppLocationReport: ILocationReport): HResult; stdcall;
    function GetReportStatus(var reportType: TGUID; out pStatus: LOCATION_REPORT_STATUS): HResult; stdcall;
    function GetReportInterval(var reportType: TGUID; out pMilliseconds: LongWord): HResult; stdcall;
    function SetReportInterval(var reportType: TGUID; millisecondsRequested: LongWord): HResult; stdcall;
    function GetDesiredAccuracy(var reportType: TGUID; 
                                out pDesiredAccuracy: LOCATION_DESIRED_ACCURACY): HResult; stdcall;
    function SetDesiredAccuracy(var reportType: TGUID; desiredAccuracy: LOCATION_DESIRED_ACCURACY): HResult; stdcall;
    function RequestPermissions(var hParent: _RemotableHandle; var pReportTypes: TGUID; 
                                count: LongWord; fModal: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ILocationEvents
// Flags:     (0)
// GUID:      {CAE02BBF-798B-4508-A207-35A7906DC73D}
// *********************************************************************//
  ILocationEvents = interface(IUnknown)
    ['{CAE02BBF-798B-4508-A207-35A7906DC73D}']
    function OnLocationChanged(var reportType: TGUID; const pLocationReport: ILocationReport): HResult; stdcall;
    function OnStatusChanged(var reportType: TGUID; newStatus: LOCATION_REPORT_STATUS): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ILocationReport
// Flags:     (0)
// GUID:      {C8B7F7EE-75D0-4DB9-B62D-7A0F369CA456}
// *********************************************************************//
  ILocationReport = interface(IUnknown)
    ['{C8B7F7EE-75D0-4DB9-B62D-7A0F369CA456}']
    function GetSensorID(var pSensorID: TGUID): HResult; stdcall;                            // ANGUS
    function GetTimestamp(var pCreationTime: TSYSTEMTIME): HResult; stdcall;                 // ANGUS
    function GetValue(const pKey: TPropertyKey; var pValue: TPropVariant): HResult; stdcall; // ANGUS
  end;

// *********************************************************************//
// Interface: ISequentialStream
// Flags:     (0)
// GUID:      {0C733A30-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ISequentialStream = interface(IUnknown)
    ['{0C733A30-2A1C-11CE-ADE5-00AA0044773D}']
    function RemoteRead(out pv: Byte; cb: LongWord; out pcbRead: LongWord): HResult; stdcall;
    function RemoteWrite(var pv: Byte; cb: LongWord; out pcbWritten: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IStream
// Flags:     (0)
// GUID:      {0000000C-0000-0000-C000-000000000046}
// *********************************************************************//
  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function RemoteSeek(dlibMove: _LARGE_INTEGER; dwOrigin: LongWord; 
                        out plibNewPosition: _ULARGE_INTEGER): HResult; stdcall;
    function SetSize(libNewSize: _ULARGE_INTEGER): HResult; stdcall;
    function RemoteCopyTo(const pstm: IStream; cb: _ULARGE_INTEGER; out pcbRead: _ULARGE_INTEGER; 
                          out pcbWritten: _ULARGE_INTEGER): HResult; stdcall;
    function Commit(grfCommitFlags: LongWord): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord): HResult; stdcall;
    function UnlockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord): HResult; stdcall;
    function Stat(out pstatstg: tagSTATSTG; grfStatFlag: LongWord): HResult; stdcall;
    function Clone(out ppstm: IStream): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IStorage
// Flags:     (0)
// GUID:      {0000000B-0000-0000-C000-000000000046}
// *********************************************************************//
  IStorage = interface(IUnknown)
    ['{0000000B-0000-0000-C000-000000000046}']
    function CreateStream(pwcsName: PWideChar; grfMode: LongWord; reserved1: LongWord; 
                          reserved2: LongWord; out ppstm: IStream): HResult; stdcall;
    function RemoteOpenStream(pwcsName: PWideChar; cbReserved1: LongWord; var reserved1: Byte; 
                              grfMode: LongWord; reserved2: LongWord; out ppstm: IStream): HResult; stdcall;
    function CreateStorage(pwcsName: PWideChar; grfMode: LongWord; reserved1: LongWord; 
                           reserved2: LongWord; out ppstg: IStorage): HResult; stdcall;
    function OpenStorage(pwcsName: PWideChar; const pstgPriority: IStorage; grfMode: LongWord; 
                         var snbExclude: tagRemSNB; reserved: LongWord; out ppstg: IStorage): HResult; stdcall;
    function RemoteCopyTo(ciidExclude: LongWord; var rgiidExclude: TGUID; 
                          var snbExclude: tagRemSNB; const pstgDest: IStorage): HResult; stdcall;
    function MoveElementTo(pwcsName: PWideChar; const pstgDest: IStorage; pwcsNewName: PWideChar; 
                           grfFlags: LongWord): HResult; stdcall;
    function Commit(grfCommitFlags: LongWord): HResult; stdcall;
    function Revert: HResult; stdcall;
    function RemoteEnumElements(reserved1: LongWord; cbReserved2: LongWord; var reserved2: Byte; 
                                reserved3: LongWord; out ppenum: IEnumSTATSTG): HResult; stdcall;
    function DestroyElement(pwcsName: PWideChar): HResult; stdcall;
    function RenameElement(pwcsOldName: PWideChar; pwcsNewName: PWideChar): HResult; stdcall;
    function SetElementTimes(pwcsName: PWideChar; var pctime: _FILETIME; var patime: _FILETIME; 
                             var pmtime: _FILETIME): HResult; stdcall;
    function SetClass(var clsid: TGUID): HResult; stdcall;
    function SetStateBits(grfStateBits: LongWord; grfMask: LongWord): HResult; stdcall;
    function Stat(out pstatstg: tagSTATSTG; grfStatFlag: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumSTATSTG
// Flags:     (0)
// GUID:      {0000000D-0000-0000-C000-000000000046}
// *********************************************************************//
  IEnumSTATSTG = interface(IUnknown)
    ['{0000000D-0000-0000-C000-000000000046}']
    function RemoteNext(celt: LongWord; out rgelt: tagSTATSTG; out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumSTATSTG): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRecordInfo
// Flags:     (0)
// GUID:      {0000002F-0000-0000-C000-000000000046}
// *********************************************************************//
  IRecordInfo = interface(IUnknown)
    ['{0000002F-0000-0000-C000-000000000046}']
    function RecordInit(out pvNew: Pointer): HResult; stdcall;
    function RecordClear(var pvExisting: Pointer): HResult; stdcall;
    function RecordCopy(var pvExisting: Pointer; out pvNew: Pointer): HResult; stdcall;
    function GetGuid(out pguid: TGUID): HResult; stdcall;
    function GetName(out pbstrName: WideString): HResult; stdcall;
    function GetSize(out pcbSize: LongWord): HResult; stdcall;
    function GetTypeInfo(out ppTypeInfo: ITypeInfo): HResult; stdcall;
    function GetField(var pvData: Pointer; szFieldName: PWideChar; out pvarField: OleVariant): HResult; stdcall;
    function GetFieldNoCopy(var pvData: Pointer; szFieldName: PWideChar; out pvarField: OleVariant; 
                            out ppvDataCArray: Pointer): HResult; stdcall;
    function PutField(wFlags: LongWord; var pvData: Pointer; szFieldName: PWideChar; 
                      var pvarField: OleVariant): HResult; stdcall;
    function PutFieldNoCopy(wFlags: LongWord; var pvData: Pointer; szFieldName: PWideChar; 
                            var pvarField: OleVariant): HResult; stdcall;
    function GetFieldNames(var pcNames: LongWord; out rgBstrNames: WideString): HResult; stdcall;
    function IsMatchingType(const pRecordInfo: IRecordInfo): Integer; stdcall;
    function RecordCreate: Pointer; stdcall;
    function RecordCreateCopy(var pvSource: Pointer; out ppvDest: Pointer): HResult; stdcall;
    function RecordDestroy(var pvRecord: Pointer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITypeInfo
// Flags:     (0)
// GUID:      {00020401-0000-0000-C000-000000000046}
// *********************************************************************//
  ITypeInfo = interface(IUnknown)
    ['{00020401-0000-0000-C000-000000000046}']
    function RemoteGetTypeAttr(out ppTypeAttr: PUserType8; out pDummy: DWORD): HResult; stdcall;
    function GetTypeComp(out ppTComp: ITypeComp): HResult; stdcall;
    function RemoteGetFuncDesc(index: SYSUINT; out ppFuncDesc: PUserType9; out pDummy: DWORD): HResult; stdcall;
    function RemoteGetVarDesc(index: SYSUINT; out ppVarDesc: PUserType10; out pDummy: DWORD): HResult; stdcall;
    function RemoteGetNames(memid: Integer; out rgBstrNames: WideString; cMaxNames: SYSUINT; 
                            out pcNames: SYSUINT): HResult; stdcall;
    function GetRefTypeOfImplType(index: SYSUINT; out pRefType: LongWord): HResult; stdcall;
    function GetImplTypeFlags(index: SYSUINT; out pImplTypeFlags: SYSINT): HResult; stdcall;
    function LocalGetIDsOfNames: HResult; stdcall;
    function LocalInvoke: HResult; stdcall;
    function RemoteGetDocumentation(memid: Integer; refPtrFlags: LongWord; 
                                    out pbstrName: WideString; out pBstrDocString: WideString; 
                                    out pdwHelpContext: LongWord; out pBstrHelpFile: WideString): HResult; stdcall;
    function RemoteGetDllEntry(memid: Integer; invkind: tagINVOKEKIND; refPtrFlags: LongWord; 
                               out pBstrDllName: WideString; out pbstrName: WideString; 
                               out pwOrdinal: Word): HResult; stdcall;
    function GetRefTypeInfo(hreftype: LongWord; out ppTInfo: ITypeInfo): HResult; stdcall;
    function LocalAddressOfMember: HResult; stdcall;
    function RemoteCreateInstance(var riid: TGUID; out ppvObj: IUnknown): HResult; stdcall;
    function GetMops(memid: Integer; out pBstrMops: WideString): HResult; stdcall;
    function RemoteGetContainingTypeLib(out ppTLib: ITypeLib; out pIndex: SYSUINT): HResult; stdcall;
    function LocalReleaseTypeAttr: HResult; stdcall;
    function LocalReleaseFuncDesc: HResult; stdcall;
    function LocalReleaseVarDesc: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITypeComp
// Flags:     (0)
// GUID:      {00020403-0000-0000-C000-000000000046}
// *********************************************************************//
  ITypeComp = interface(IUnknown)
    ['{00020403-0000-0000-C000-000000000046}']
    function RemoteBind(szName: PWideChar; lHashVal: LongWord; wFlags: Word; 
                        out ppTInfo: ITypeInfo; out pDescKind: tagDESCKIND; 
                        out ppFuncDesc: PUserType9; out ppVarDesc: PUserType10; 
                        out ppTypeComp: ITypeComp; out pDummy: DWORD): HResult; stdcall;
    function RemoteBindType(szName: PWideChar; lHashVal: LongWord; out ppTInfo: ITypeInfo): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITypeLib
// Flags:     (0)
// GUID:      {00020402-0000-0000-C000-000000000046}
// *********************************************************************//
  ITypeLib = interface(IUnknown)
    ['{00020402-0000-0000-C000-000000000046}']
    function RemoteGetTypeInfoCount(out pcTInfo: SYSUINT): HResult; stdcall;
    function GetTypeInfo(index: SYSUINT; out ppTInfo: ITypeInfo): HResult; stdcall;
    function GetTypeInfoType(index: SYSUINT; out pTKind: tagTYPEKIND): HResult; stdcall;
    function GetTypeInfoOfGuid(var guid: TGUID; out ppTInfo: ITypeInfo): HResult; stdcall;
    function RemoteGetLibAttr(out ppTLibAttr: PUserType13; out pDummy: DWORD): HResult; stdcall;
    function GetTypeComp(out ppTComp: ITypeComp): HResult; stdcall;
    function RemoteGetDocumentation(index: SYSINT; refPtrFlags: LongWord; 
                                    out pbstrName: WideString; out pBstrDocString: WideString; 
                                    out pdwHelpContext: LongWord; out pBstrHelpFile: WideString): HResult; stdcall;
    function RemoteIsName(szNameBuf: PWideChar; lHashVal: LongWord; out pfName: Integer; 
                          out pBstrLibName: WideString): HResult; stdcall;
    function RemoteFindName(szNameBuf: PWideChar; lHashVal: LongWord; out ppTInfo: ITypeInfo; 
                            out rgMemId: Integer; var pcFound: Word; out pBstrLibName: WideString): HResult; stdcall;
    function LocalReleaseTLibAttr: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDefaultLocation
// Flags:     (0)
// GUID:      {A65AF77E-969A-4A2E-8ACA-33BB7CBB1235}
// *********************************************************************//
  IDefaultLocation = interface(IUnknown)
    ['{A65AF77E-969A-4A2E-8ACA-33BB7CBB1235}']
    function SetReport(var reportType: TGUID; const pLocationReport: ILocationReport): HResult; stdcall;
    function GetReport(var reportType: TGUID; out ppLocationReport: ILocationReport): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ILatLongReport
// Flags:     (0)
// GUID:      {7FED806D-0EF8-4F07-80AC-36A0BEAE3134}
// *********************************************************************//
  ILatLongReport = interface(ILocationReport)
    ['{7FED806D-0EF8-4F07-80AC-36A0BEAE3134}']
    function GetLatitude(out pLatitude: Double): HResult; stdcall;
    function GetLongitude(out pLongitude: Double): HResult; stdcall;
    function GetErrorRadius(out pErrorRadius: Double): HResult; stdcall;
    function GetAltitude(out pAltitude: Double): HResult; stdcall;
    function GetAltitudeError(out pAltitudeError: Double): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICivicAddressReport
// Flags:     (0)
// GUID:      {C0B19F70-4ADF-445D-87F2-CAD8FD711792}
// *********************************************************************//
  ICivicAddressReport = interface(ILocationReport)
    ['{C0B19F70-4ADF-445D-87F2-CAD8FD711792}']
    function GetAddressLine1(out pbstrAddress1: WideString): HResult; stdcall;
    function GetAddressLine2(out pbstrAddress2: WideString): HResult; stdcall;
    function GetCity(out pbstrCity: WideString): HResult; stdcall;
    function GetStateProvince(out pbstrStateProvince: WideString): HResult; stdcall;
    function GetPostalCode(out pbstrPostalCode: WideString): HResult; stdcall;
    function GetCountryRegion(out pbstrCountryRegion: WideString): HResult; stdcall;
    function GetDetailLevel(out pDetailLevel: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  _ILatLongReportFactoryEvents
// Flags:     (4096) Dispatchable
// GUID:      {16EE6CB7-AB3C-424B-849F-269BE551FCBC}
// *********************************************************************//
  _ILatLongReportFactoryEvents = dispinterface
    ['{16EE6CB7-AB3C-424B-849F-269BE551FCBC}']
    procedure NewLatLongReport(const newReport: IDispLatLongReport); dispid 1;
    procedure StatusChanged(status: LongWord); dispid 2;
  end;

// *********************************************************************//
// Interface: IDispLatLongReport
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8AE32723-389B-4A11-9957-5BDD48FC9617}
// *********************************************************************//
  IDispLatLongReport = interface(IDispatch)
    ['{8AE32723-389B-4A11-9957-5BDD48FC9617}']
    function Get_Latitude: Double; safecall;
    function Get_Longitude: Double; safecall;
    function Get_ErrorRadius: Double; safecall;
    function Get_Altitude: Double; safecall;
    function Get_AltitudeError: Double; safecall;
    function Get_Timestamp: TDateTime; safecall;
    property Latitude: Double read Get_Latitude;
    property Longitude: Double read Get_Longitude;
    property ErrorRadius: Double read Get_ErrorRadius;
    property Altitude: Double read Get_Altitude;
    property AltitudeError: Double read Get_AltitudeError;
    property Timestamp: TDateTime read Get_Timestamp;
  end;

// *********************************************************************//
// DispIntf:  IDispLatLongReportDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8AE32723-389B-4A11-9957-5BDD48FC9617}
// *********************************************************************//
  IDispLatLongReportDisp = dispinterface
    ['{8AE32723-389B-4A11-9957-5BDD48FC9617}']
    property Latitude: Double readonly dispid 1;
    property Longitude: Double readonly dispid 2;
    property ErrorRadius: Double readonly dispid 3;
    property Altitude: Double readonly dispid 4;
    property AltitudeError: Double readonly dispid 5;
    property Timestamp: TDateTime readonly dispid 6;
  end;

// *********************************************************************//
// DispIntf:  _ICivicAddressReportFactoryEvents
// Flags:     (4096) Dispatchable
// GUID:      {C96039FF-72EC-4617-89BD-84D88BEDC722}
// *********************************************************************//
  _ICivicAddressReportFactoryEvents = dispinterface
    ['{C96039FF-72EC-4617-89BD-84D88BEDC722}']
    procedure NewCivicAddressReport(const newReport: IDispCivicAddressReport); dispid 1;
    procedure StatusChanged(status: LongWord); dispid 2;
  end;

// *********************************************************************//
// Interface: IDispCivicAddressReport
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {16FF1A34-9E30-42C3-B44D-E22513B5767A}
// *********************************************************************//
  IDispCivicAddressReport = interface(IDispatch)
    ['{16FF1A34-9E30-42C3-B44D-E22513B5767A}']
    function Get_AddressLine1: WideString; safecall;
    function Get_AddressLine2: WideString; safecall;
    function Get_City: WideString; safecall;
    function Get_StateProvince: WideString; safecall;
    function Get_PostalCode: WideString; safecall;
    function Get_CountryRegion: WideString; safecall;
    function Get_DetailLevel: LongWord; safecall;
    function Get_Timestamp: TDateTime; safecall;
    property AddressLine1: WideString read Get_AddressLine1;
    property AddressLine2: WideString read Get_AddressLine2;
    property City: WideString read Get_City;
    property StateProvince: WideString read Get_StateProvince;
    property PostalCode: WideString read Get_PostalCode;
    property CountryRegion: WideString read Get_CountryRegion;
    property DetailLevel: LongWord read Get_DetailLevel;
    property Timestamp: TDateTime read Get_Timestamp;
  end;

// *********************************************************************//
// DispIntf:  IDispCivicAddressReportDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {16FF1A34-9E30-42C3-B44D-E22513B5767A}
// *********************************************************************//
  IDispCivicAddressReportDisp = dispinterface
    ['{16FF1A34-9E30-42C3-B44D-E22513B5767A}']
    property AddressLine1: WideString readonly dispid 1;
    property AddressLine2: WideString readonly dispid 2;
    property City: WideString readonly dispid 3;
    property StateProvince: WideString readonly dispid 4;
    property PostalCode: WideString readonly dispid 5;
    property CountryRegion: WideString readonly dispid 6;
    property DetailLevel: LongWord readonly dispid 7;
    property Timestamp: TDateTime readonly dispid 8;
  end;

// *********************************************************************//
// Interface: ILocationReportFactory
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2DAEC322-90B2-47E4-BB08-0DA841935A6B}
// *********************************************************************//
  ILocationReportFactory = interface(IDispatch)
    ['{2DAEC322-90B2-47E4-BB08-0DA841935A6B}']
    procedure ListenForReports(requestedReportInterval: LongWord); safecall;
    procedure StopListeningForReports; safecall;
    function Get_status: LongWord; safecall;
    function Get_ReportInterval: LongWord; safecall;
    procedure Set_ReportInterval(pMilliseconds: LongWord); safecall;
    function Get_desiredAccuracy: LongWord; safecall;
    procedure Set_desiredAccuracy(pDesiredAccuracy: LongWord); safecall;
    procedure RequestPermissions(var hWnd: LongWord); safecall;
    property status: LongWord read Get_status;
    property ReportInterval: LongWord read Get_ReportInterval write Set_ReportInterval;
    property desiredAccuracy: LongWord read Get_desiredAccuracy write Set_desiredAccuracy;
  end;

// *********************************************************************//
// DispIntf:  ILocationReportFactoryDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2DAEC322-90B2-47E4-BB08-0DA841935A6B}
// *********************************************************************//
  ILocationReportFactoryDisp = dispinterface
    ['{2DAEC322-90B2-47E4-BB08-0DA841935A6B}']
    procedure ListenForReports(requestedReportInterval: LongWord); dispid 1;
    procedure StopListeningForReports; dispid 2;
    property status: LongWord readonly dispid 3;
    property ReportInterval: LongWord dispid 4;
    property desiredAccuracy: LongWord dispid 5;
    procedure RequestPermissions(var hWnd: LongWord); dispid 6;
  end;

// *********************************************************************//
// Interface: ILatLongReportFactory
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3F0804CB-B114-447D-83DD-390174EBB082}
// *********************************************************************//
  ILatLongReportFactory = interface(ILocationReportFactory)
    ['{3F0804CB-B114-447D-83DD-390174EBB082}']
    function Get_LatLongReport: IDispLatLongReport; safecall;
    property LatLongReport: IDispLatLongReport read Get_LatLongReport;
  end;

// *********************************************************************//
// DispIntf:  ILatLongReportFactoryDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3F0804CB-B114-447D-83DD-390174EBB082}
// *********************************************************************//
  ILatLongReportFactoryDisp = dispinterface
    ['{3F0804CB-B114-447D-83DD-390174EBB082}']
    property LatLongReport: IDispLatLongReport readonly dispid 7;
    procedure ListenForReports(requestedReportInterval: LongWord); dispid 1;
    procedure StopListeningForReports; dispid 2;
    property status: LongWord readonly dispid 3;
    property ReportInterval: LongWord dispid 4;
    property desiredAccuracy: LongWord dispid 5;
    procedure RequestPermissions(var hWnd: LongWord); dispid 6;
  end;

// *********************************************************************//
// Interface: ICivicAddressReportFactory
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BF773B93-C64F-4BEE-BEB2-67C0B8DF66E0}
// *********************************************************************//
  ICivicAddressReportFactory = interface(ILocationReportFactory)
    ['{BF773B93-C64F-4BEE-BEB2-67C0B8DF66E0}']
    function Get_CivicAddressReport: IDispCivicAddressReport; safecall;
    property CivicAddressReport: IDispCivicAddressReport read Get_CivicAddressReport;
  end;

// *********************************************************************//
// DispIntf:  ICivicAddressReportFactoryDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BF773B93-C64F-4BEE-BEB2-67C0B8DF66E0}
// *********************************************************************//
  ICivicAddressReportFactoryDisp = dispinterface
    ['{BF773B93-C64F-4BEE-BEB2-67C0B8DF66E0}']
    property CivicAddressReport: IDispCivicAddressReport readonly dispid 7;
    procedure ListenForReports(requestedReportInterval: LongWord); dispid 1;
    procedure StopListeningForReports; dispid 2;
    property status: LongWord readonly dispid 3;
    property ReportInterval: LongWord dispid 4;
    property desiredAccuracy: LongWord dispid 5;
    procedure RequestPermissions(var hWnd: LongWord); dispid 6;
  end;

// *********************************************************************//
// The Class CoLocation provides a Create and CreateRemote method to          
// create instances of the default interface ILocation exposed by              
// the CoClass Location. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLocation = class
    class function Create: ILocation;
    class function CreateRemote(const MachineName: string): ILocation;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TLocation
// Help String      : Location Class
// Default Interface: ILocation
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TLocationProperties= class;
{$ENDIF}
  TLocation = class(TOleServer)
  private
    FIntf: ILocation;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TLocationProperties;
    function GetServerProperties: TLocationProperties;
{$ENDIF}
    function GetDefaultInterface: ILocation;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ILocation);
    procedure Disconnect; override;
    function RegisterForReport(const pEvents: ILocationEvents; var reportType: TGUID; 
                               dwRequestedReportInterval: LongWord): HResult;
    function UnregisterForReport(var reportType: TGUID): HResult;
    function GetReport(var reportType: TGUID; out ppLocationReport: ILocationReport): HResult;
    function GetReportStatus(var reportType: TGUID; out pStatus: LOCATION_REPORT_STATUS): HResult;
    function GetReportInterval(var reportType: TGUID; out pMilliseconds: LongWord): HResult;
    function SetReportInterval(var reportType: TGUID; millisecondsRequested: LongWord): HResult;
    function GetDesiredAccuracy(var reportType: TGUID; 
                                out pDesiredAccuracy: LOCATION_DESIRED_ACCURACY): HResult;
    function SetDesiredAccuracy(var reportType: TGUID; desiredAccuracy: LOCATION_DESIRED_ACCURACY): HResult;
    function RequestPermissions(var hParent: _RemotableHandle; var pReportTypes: TGUID; 
                                count: LongWord; fModal: Integer): HResult;
    property DefaultInterface: ILocation read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TLocationProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TLocation
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TLocationProperties = class(TPersistent)
  private
    FServer:    TLocation;
    function    GetDefaultInterface: ILocation;
    constructor Create(AServer: TLocation);
  protected
  public
    property DefaultInterface: ILocation read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoDefaultLocation provides a Create and CreateRemote method to          
// create instances of the default interface IDefaultLocation exposed by              
// the CoClass DefaultLocation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDefaultLocation = class
    class function Create: IDefaultLocation;
    class function CreateRemote(const MachineName: string): IDefaultLocation;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TDefaultLocation
// Help String      : DefaultLocation Class
// Default Interface: IDefaultLocation
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TDefaultLocationProperties= class;
{$ENDIF}
  TDefaultLocation = class(TOleServer)
  private
    FIntf: IDefaultLocation;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TDefaultLocationProperties;
    function GetServerProperties: TDefaultLocationProperties;
{$ENDIF}
    function GetDefaultInterface: IDefaultLocation;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IDefaultLocation);
    procedure Disconnect; override;
    function SetReport(var reportType: TGUID; const pLocationReport: ILocationReport): HResult;
    function GetReport(var reportType: TGUID; out ppLocationReport: ILocationReport): HResult;
    property DefaultInterface: IDefaultLocation read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TDefaultLocationProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TDefaultLocation
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TDefaultLocationProperties = class(TPersistent)
  private
    FServer:    TDefaultLocation;
    function    GetDefaultInterface: IDefaultLocation;
    constructor Create(AServer: TDefaultLocation);
  protected
  public
    property DefaultInterface: IDefaultLocation read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoLatLongReport provides a Create and CreateRemote method to          
// create instances of the default interface ILatLongReport exposed by              
// the CoClass LatLongReport. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLatLongReport = class
    class function Create: ILatLongReport;
    class function CreateRemote(const MachineName: string): ILatLongReport;
  end;

// *********************************************************************//
// The Class CoCivicAddressReport provides a Create and CreateRemote method to          
// create instances of the default interface ICivicAddressReport exposed by              
// the CoClass CivicAddressReport. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCivicAddressReport = class
    class function Create: ICivicAddressReport;
    class function CreateRemote(const MachineName: string): ICivicAddressReport;
  end;

// *********************************************************************//
// The Class CoLatLongReportFactory provides a Create and CreateRemote method to          
// create instances of the default interface ILatLongReportFactory exposed by              
// the CoClass LatLongReportFactory. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoLatLongReportFactory = class
    class function Create: ILatLongReportFactory;
    class function CreateRemote(const MachineName: string): ILatLongReportFactory;
  end;

  TLatLongReportFactoryNewLatLongReport = procedure(ASender: TObject; const newReport: IDispLatLongReport) of object;
  TLatLongReportFactoryStatusChanged = procedure(ASender: TObject; status: LongWord) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TLatLongReportFactory
// Help String      : LatLongReportFactory Class
// Default Interface: ILatLongReportFactory
// Def. Intf. DISP? : No
// Event   Interface: _ILatLongReportFactoryEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TLatLongReportFactoryProperties= class;
{$ENDIF}
  TLatLongReportFactory = class(TOleServer)
  private
    FOnNewLatLongReport: TLatLongReportFactoryNewLatLongReport;
    FOnStatusChanged: TLatLongReportFactoryStatusChanged;
    FIntf: ILatLongReportFactory;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TLatLongReportFactoryProperties;
    function GetServerProperties: TLatLongReportFactoryProperties;
{$ENDIF}
    function GetDefaultInterface: ILatLongReportFactory;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_LatLongReport: IDispLatLongReport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ILatLongReportFactory);
    procedure Disconnect; override;
    property DefaultInterface: ILatLongReportFactory read GetDefaultInterface;
    property LatLongReport: IDispLatLongReport read Get_LatLongReport;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TLatLongReportFactoryProperties read GetServerProperties;
{$ENDIF}
    property OnNewLatLongReport: TLatLongReportFactoryNewLatLongReport read FOnNewLatLongReport write FOnNewLatLongReport;
    property OnStatusChanged: TLatLongReportFactoryStatusChanged read FOnStatusChanged write FOnStatusChanged;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TLatLongReportFactory
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TLatLongReportFactoryProperties = class(TPersistent)
  private
    FServer:    TLatLongReportFactory;
    function    GetDefaultInterface: ILatLongReportFactory;
    constructor Create(AServer: TLatLongReportFactory);
  protected
    function Get_LatLongReport: IDispLatLongReport;
  public
    property DefaultInterface: ILatLongReportFactory read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCivicAddressReportFactory provides a Create and CreateRemote method to          
// create instances of the default interface ICivicAddressReportFactory exposed by              
// the CoClass CivicAddressReportFactory. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCivicAddressReportFactory = class
    class function Create: ICivicAddressReportFactory;
    class function CreateRemote(const MachineName: string): ICivicAddressReportFactory;
  end;

  TCivicAddressReportFactoryNewCivicAddressReport = procedure(ASender: TObject; const newReport: IDispCivicAddressReport) of object;
  TCivicAddressReportFactoryStatusChanged = procedure(ASender: TObject; status: LongWord) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCivicAddressReportFactory
// Help String      : CivicAddressReportFactory Class
// Default Interface: ICivicAddressReportFactory
// Def. Intf. DISP? : No
// Event   Interface: _ICivicAddressReportFactoryEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCivicAddressReportFactoryProperties= class;
{$ENDIF}
  TCivicAddressReportFactory = class(TOleServer)
  private
    FOnNewCivicAddressReport: TCivicAddressReportFactoryNewCivicAddressReport;
    FOnStatusChanged: TCivicAddressReportFactoryStatusChanged;
    FIntf: ICivicAddressReportFactory;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCivicAddressReportFactoryProperties;
    function GetServerProperties: TCivicAddressReportFactoryProperties;
{$ENDIF}
    function GetDefaultInterface: ICivicAddressReportFactory;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_CivicAddressReport: IDispCivicAddressReport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICivicAddressReportFactory);
    procedure Disconnect; override;
    property DefaultInterface: ICivicAddressReportFactory read GetDefaultInterface;
    property CivicAddressReport: IDispCivicAddressReport read Get_CivicAddressReport;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCivicAddressReportFactoryProperties read GetServerProperties;
{$ENDIF}
    property OnNewCivicAddressReport: TCivicAddressReportFactoryNewCivicAddressReport read FOnNewCivicAddressReport write FOnNewCivicAddressReport;
    property OnStatusChanged: TCivicAddressReportFactoryStatusChanged read FOnStatusChanged write FOnStatusChanged;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCivicAddressReportFactory
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCivicAddressReportFactoryProperties = class(TPersistent)
  private
    FServer:    TCivicAddressReportFactory;
    function    GetDefaultInterface: ICivicAddressReportFactory;
    constructor Create(AServer: TCivicAddressReportFactory);
  protected
    function Get_CivicAddressReport: IDispCivicAddressReport;
  public
    property DefaultInterface: ICivicAddressReportFactory read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoDispLatLongReport provides a Create and CreateRemote method to          
// create instances of the default interface IDispLatLongReport exposed by              
// the CoClass DispLatLongReport. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDispLatLongReport = class
    class function Create: IDispLatLongReport;
    class function CreateRemote(const MachineName: string): IDispLatLongReport;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TDispLatLongReport
// Help String      : LatLongReport Class
// Default Interface: IDispLatLongReport
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TDispLatLongReportProperties= class;
{$ENDIF}
  TDispLatLongReport = class(TOleServer)
  private
    FIntf: IDispLatLongReport;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TDispLatLongReportProperties;
    function GetServerProperties: TDispLatLongReportProperties;
{$ENDIF}
    function GetDefaultInterface: IDispLatLongReport;
  protected
    procedure InitServerData; override;
    function Get_Latitude: Double;
    function Get_Longitude: Double;
    function Get_ErrorRadius: Double;
    function Get_Altitude: Double;
    function Get_AltitudeError: Double;
    function Get_Timestamp: TDateTime;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IDispLatLongReport);
    procedure Disconnect; override;
    property DefaultInterface: IDispLatLongReport read GetDefaultInterface;
    property Latitude: Double read Get_Latitude;
    property Longitude: Double read Get_Longitude;
    property ErrorRadius: Double read Get_ErrorRadius;
    property Altitude: Double read Get_Altitude;
    property AltitudeError: Double read Get_AltitudeError;
    property Timestamp: TDateTime read Get_Timestamp;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TDispLatLongReportProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TDispLatLongReport
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TDispLatLongReportProperties = class(TPersistent)
  private
    FServer:    TDispLatLongReport;
    function    GetDefaultInterface: IDispLatLongReport;
    constructor Create(AServer: TDispLatLongReport);
  protected
    function Get_Latitude: Double;
    function Get_Longitude: Double;
    function Get_ErrorRadius: Double;
    function Get_Altitude: Double;
    function Get_AltitudeError: Double;
    function Get_Timestamp: TDateTime;
  public
    property DefaultInterface: IDispLatLongReport read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoDispCivicAddressReport provides a Create and CreateRemote method to          
// create instances of the default interface IDispCivicAddressReport exposed by              
// the CoClass DispCivicAddressReport. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDispCivicAddressReport = class
    class function Create: IDispCivicAddressReport;
    class function CreateRemote(const MachineName: string): IDispCivicAddressReport;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TDispCivicAddressReport
// Help String      : CivicAddressReport Class
// Default Interface: IDispCivicAddressReport
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TDispCivicAddressReportProperties= class;
{$ENDIF}
  TDispCivicAddressReport = class(TOleServer)
  private
    FIntf: IDispCivicAddressReport;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TDispCivicAddressReportProperties;
    function GetServerProperties: TDispCivicAddressReportProperties;
{$ENDIF}
    function GetDefaultInterface: IDispCivicAddressReport;
  protected
    procedure InitServerData; override;
    function Get_AddressLine1: WideString;
    function Get_AddressLine2: WideString;
    function Get_City: WideString;
    function Get_StateProvince: WideString;
    function Get_PostalCode: WideString;
    function Get_CountryRegion: WideString;
    function Get_DetailLevel: LongWord;
    function Get_Timestamp: TDateTime;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IDispCivicAddressReport);
    procedure Disconnect; override;
    property DefaultInterface: IDispCivicAddressReport read GetDefaultInterface;
    property AddressLine1: WideString read Get_AddressLine1;
    property AddressLine2: WideString read Get_AddressLine2;
    property City: WideString read Get_City;
    property StateProvince: WideString read Get_StateProvince;
    property PostalCode: WideString read Get_PostalCode;
    property CountryRegion: WideString read Get_CountryRegion;
    property DetailLevel: LongWord read Get_DetailLevel;
    property Timestamp: TDateTime read Get_Timestamp;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TDispCivicAddressReportProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TDispCivicAddressReport
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TDispCivicAddressReportProperties = class(TPersistent)
  private
    FServer:    TDispCivicAddressReport;
    function    GetDefaultInterface: IDispCivicAddressReport;
    constructor Create(AServer: TDispCivicAddressReport);
  protected
    function Get_AddressLine1: WideString;
    function Get_AddressLine2: WideString;
    function Get_City: WideString;
    function Get_StateProvince: WideString;
    function Get_PostalCode: WideString;
    function Get_CountryRegion: WideString;
    function Get_DetailLevel: LongWord;
    function Get_Timestamp: TDateTime;
  public
    property DefaultInterface: IDispCivicAddressReport read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoLocation.Create: ILocation;
begin
  Result := CreateComObject(CLASS_Location) as ILocation;
end;

class function CoLocation.CreateRemote(const MachineName: string): ILocation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Location) as ILocation;
end;

procedure TLocation.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{E5B8E079-EE6D-4E33-A438-C87F2E959254}';
    IntfIID:   '{AB2ECE69-56D9-4F28-B525-DE1B0EE44237}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TLocation.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ILocation;
  end;
end;

procedure TLocation.ConnectTo(svrIntf: ILocation);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TLocation.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TLocation.GetDefaultInterface: ILocation;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TLocationProperties.Create(Self);
{$ENDIF}
end;

destructor TLocation.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TLocation.GetServerProperties: TLocationProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TLocation.RegisterForReport(const pEvents: ILocationEvents; var reportType: TGUID; 
                                     dwRequestedReportInterval: LongWord): HResult;
begin
  Result := DefaultInterface.RegisterForReport(pEvents, reportType, dwRequestedReportInterval);
end;

function TLocation.UnregisterForReport(var reportType: TGUID): HResult;
begin
  Result := DefaultInterface.UnregisterForReport(reportType);
end;

function TLocation.GetReport(var reportType: TGUID; out ppLocationReport: ILocationReport): HResult;
begin
  Result := DefaultInterface.GetReport(reportType, ppLocationReport);
end;

function TLocation.GetReportStatus(var reportType: TGUID; out pStatus: LOCATION_REPORT_STATUS): HResult;
begin
  Result := DefaultInterface.GetReportStatus(reportType, pStatus);
end;

function TLocation.GetReportInterval(var reportType: TGUID; out pMilliseconds: LongWord): HResult;
begin
  Result := DefaultInterface.GetReportInterval(reportType, pMilliseconds);
end;

function TLocation.SetReportInterval(var reportType: TGUID; millisecondsRequested: LongWord): HResult;
begin
  Result := DefaultInterface.SetReportInterval(reportType, millisecondsRequested);
end;

function TLocation.GetDesiredAccuracy(var reportType: TGUID; 
                                      out pDesiredAccuracy: LOCATION_DESIRED_ACCURACY): HResult;
begin
  Result := DefaultInterface.GetDesiredAccuracy(reportType, pDesiredAccuracy);
end;

function TLocation.SetDesiredAccuracy(var reportType: TGUID; 
                                      desiredAccuracy: LOCATION_DESIRED_ACCURACY): HResult;
begin
  Result := DefaultInterface.SetDesiredAccuracy(reportType, desiredAccuracy);
end;

function TLocation.RequestPermissions(var hParent: _RemotableHandle; var pReportTypes: TGUID; 
                                      count: LongWord; fModal: Integer): HResult;
begin
  Result := DefaultInterface.RequestPermissions(hParent, pReportTypes, count, fModal);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TLocationProperties.Create(AServer: TLocation);
begin
  inherited Create;
  FServer := AServer;
end;

function TLocationProperties.GetDefaultInterface: ILocation;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoDefaultLocation.Create: IDefaultLocation;
begin
  Result := CreateComObject(CLASS_DefaultLocation) as IDefaultLocation;
end;

class function CoDefaultLocation.CreateRemote(const MachineName: string): IDefaultLocation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DefaultLocation) as IDefaultLocation;
end;

procedure TDefaultLocation.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{8B7FBFE0-5CD7-494A-AF8C-283A65707506}';
    IntfIID:   '{A65AF77E-969A-4A2E-8ACA-33BB7CBB1235}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TDefaultLocation.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IDefaultLocation;
  end;
end;

procedure TDefaultLocation.ConnectTo(svrIntf: IDefaultLocation);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TDefaultLocation.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TDefaultLocation.GetDefaultInterface: IDefaultLocation;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TDefaultLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TDefaultLocationProperties.Create(Self);
{$ENDIF}
end;

destructor TDefaultLocation.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TDefaultLocation.GetServerProperties: TDefaultLocationProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TDefaultLocation.SetReport(var reportType: TGUID; const pLocationReport: ILocationReport): HResult;
begin
  Result := DefaultInterface.SetReport(reportType, pLocationReport);
end;

function TDefaultLocation.GetReport(var reportType: TGUID; out ppLocationReport: ILocationReport): HResult;
begin
  Result := DefaultInterface.GetReport(reportType, ppLocationReport);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TDefaultLocationProperties.Create(AServer: TDefaultLocation);
begin
  inherited Create;
  FServer := AServer;
end;

function TDefaultLocationProperties.GetDefaultInterface: IDefaultLocation;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoLatLongReport.Create: ILatLongReport;
begin
  Result := CreateComObject(CLASS_LatLongReport) as ILatLongReport;
end;

class function CoLatLongReport.CreateRemote(const MachineName: string): ILatLongReport;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LatLongReport) as ILatLongReport;
end;

class function CoCivicAddressReport.Create: ICivicAddressReport;
begin
  Result := CreateComObject(CLASS_CivicAddressReport) as ICivicAddressReport;
end;

class function CoCivicAddressReport.CreateRemote(const MachineName: string): ICivicAddressReport;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CivicAddressReport) as ICivicAddressReport;
end;

class function CoLatLongReportFactory.Create: ILatLongReportFactory;
begin
  Result := CreateComObject(CLASS_LatLongReportFactory) as ILatLongReportFactory;
end;

class function CoLatLongReportFactory.CreateRemote(const MachineName: string): ILatLongReportFactory;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_LatLongReportFactory) as ILatLongReportFactory;
end;

procedure TLatLongReportFactory.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{9DCC3CC8-8609-4863-BAD4-03601F4C65E8}';
    IntfIID:   '{3F0804CB-B114-447D-83DD-390174EBB082}';
    EventIID:  '{16EE6CB7-AB3C-424B-849F-269BE551FCBC}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TLatLongReportFactory.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as ILatLongReportFactory;
  end;
end;

procedure TLatLongReportFactory.ConnectTo(svrIntf: ILatLongReportFactory);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TLatLongReportFactory.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TLatLongReportFactory.GetDefaultInterface: ILatLongReportFactory;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TLatLongReportFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TLatLongReportFactoryProperties.Create(Self);
{$ENDIF}
end;

destructor TLatLongReportFactory.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TLatLongReportFactory.GetServerProperties: TLatLongReportFactoryProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TLatLongReportFactory.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnNewLatLongReport) then
         FOnNewLatLongReport(Self, IUnknown(TVarData(Params[0]).VPointer) as IDispLatLongReport {const IDispLatLongReport});
    2: if Assigned(FOnStatusChanged) then
         FOnStatusChanged(Self, Params[0] {LongWord});
  end; {case DispID}
end;

function TLatLongReportFactory.Get_LatLongReport: IDispLatLongReport;
begin
    Result := DefaultInterface.LatLongReport;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TLatLongReportFactoryProperties.Create(AServer: TLatLongReportFactory);
begin
  inherited Create;
  FServer := AServer;
end;

function TLatLongReportFactoryProperties.GetDefaultInterface: ILatLongReportFactory;
begin
  Result := FServer.DefaultInterface;
end;

function TLatLongReportFactoryProperties.Get_LatLongReport: IDispLatLongReport;
begin
    Result := DefaultInterface.LatLongReport;
end;

{$ENDIF}

class function CoCivicAddressReportFactory.Create: ICivicAddressReportFactory;
begin
  Result := CreateComObject(CLASS_CivicAddressReportFactory) as ICivicAddressReportFactory;
end;

class function CoCivicAddressReportFactory.CreateRemote(const MachineName: string): ICivicAddressReportFactory;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CivicAddressReportFactory) as ICivicAddressReportFactory;
end;

procedure TCivicAddressReportFactory.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{2A11F42C-3E81-4AD4-9CBE-45579D89671A}';
    IntfIID:   '{BF773B93-C64F-4BEE-BEB2-67C0B8DF66E0}';
    EventIID:  '{C96039FF-72EC-4617-89BD-84D88BEDC722}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCivicAddressReportFactory.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as ICivicAddressReportFactory;
  end;
end;

procedure TCivicAddressReportFactory.ConnectTo(svrIntf: ICivicAddressReportFactory);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TCivicAddressReportFactory.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TCivicAddressReportFactory.GetDefaultInterface: ICivicAddressReportFactory;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCivicAddressReportFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCivicAddressReportFactoryProperties.Create(Self);
{$ENDIF}
end;

destructor TCivicAddressReportFactory.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCivicAddressReportFactory.GetServerProperties: TCivicAddressReportFactoryProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TCivicAddressReportFactory.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnNewCivicAddressReport) then
         FOnNewCivicAddressReport(Self, IUnknown(TVarData(Params[0]).VPointer) as IDispCivicAddressReport {const IDispCivicAddressReport});
    2: if Assigned(FOnStatusChanged) then
         FOnStatusChanged(Self, Params[0] {LongWord});
  end; {case DispID}
end;

function TCivicAddressReportFactory.Get_CivicAddressReport: IDispCivicAddressReport;
begin
    Result := DefaultInterface.CivicAddressReport;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCivicAddressReportFactoryProperties.Create(AServer: TCivicAddressReportFactory);
begin
  inherited Create;
  FServer := AServer;
end;

function TCivicAddressReportFactoryProperties.GetDefaultInterface: ICivicAddressReportFactory;
begin
  Result := FServer.DefaultInterface;
end;

function TCivicAddressReportFactoryProperties.Get_CivicAddressReport: IDispCivicAddressReport;
begin
    Result := DefaultInterface.CivicAddressReport;
end;

{$ENDIF}

class function CoDispLatLongReport.Create: IDispLatLongReport;
begin
  Result := CreateComObject(CLASS_DispLatLongReport) as IDispLatLongReport;
end;

class function CoDispLatLongReport.CreateRemote(const MachineName: string): IDispLatLongReport;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DispLatLongReport) as IDispLatLongReport;
end;

procedure TDispLatLongReport.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{7A7C3277-8F84-4636-95B2-EBB5507FF77E}';
    IntfIID:   '{8AE32723-389B-4A11-9957-5BDD48FC9617}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TDispLatLongReport.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IDispLatLongReport;
  end;
end;

procedure TDispLatLongReport.ConnectTo(svrIntf: IDispLatLongReport);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TDispLatLongReport.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TDispLatLongReport.GetDefaultInterface: IDispLatLongReport;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TDispLatLongReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TDispLatLongReportProperties.Create(Self);
{$ENDIF}
end;

destructor TDispLatLongReport.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TDispLatLongReport.GetServerProperties: TDispLatLongReportProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TDispLatLongReport.Get_Latitude: Double;
begin
    Result := DefaultInterface.Latitude;
end;

function TDispLatLongReport.Get_Longitude: Double;
begin
    Result := DefaultInterface.Longitude;
end;

function TDispLatLongReport.Get_ErrorRadius: Double;
begin
    Result := DefaultInterface.ErrorRadius;
end;

function TDispLatLongReport.Get_Altitude: Double;
begin
    Result := DefaultInterface.Altitude;
end;

function TDispLatLongReport.Get_AltitudeError: Double;
begin
    Result := DefaultInterface.AltitudeError;
end;

function TDispLatLongReport.Get_Timestamp: TDateTime;
begin
    Result := DefaultInterface.Timestamp;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TDispLatLongReportProperties.Create(AServer: TDispLatLongReport);
begin
  inherited Create;
  FServer := AServer;
end;

function TDispLatLongReportProperties.GetDefaultInterface: IDispLatLongReport;
begin
  Result := FServer.DefaultInterface;
end;

function TDispLatLongReportProperties.Get_Latitude: Double;
begin
    Result := DefaultInterface.Latitude;
end;

function TDispLatLongReportProperties.Get_Longitude: Double;
begin
    Result := DefaultInterface.Longitude;
end;

function TDispLatLongReportProperties.Get_ErrorRadius: Double;
begin
    Result := DefaultInterface.ErrorRadius;
end;

function TDispLatLongReportProperties.Get_Altitude: Double;
begin
    Result := DefaultInterface.Altitude;
end;

function TDispLatLongReportProperties.Get_AltitudeError: Double;
begin
    Result := DefaultInterface.AltitudeError;
end;

function TDispLatLongReportProperties.Get_Timestamp: TDateTime;
begin
    Result := DefaultInterface.Timestamp;
end;

{$ENDIF}

class function CoDispCivicAddressReport.Create: IDispCivicAddressReport;
begin
  Result := CreateComObject(CLASS_DispCivicAddressReport) as IDispCivicAddressReport;
end;

class function CoDispCivicAddressReport.CreateRemote(const MachineName: string): IDispCivicAddressReport;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DispCivicAddressReport) as IDispCivicAddressReport;
end;

procedure TDispCivicAddressReport.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{4C596AEC-8544-4082-BA9F-EB0A7D8E65C6}';
    IntfIID:   '{16FF1A34-9E30-42C3-B44D-E22513B5767A}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TDispCivicAddressReport.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IDispCivicAddressReport;
  end;
end;

procedure TDispCivicAddressReport.ConnectTo(svrIntf: IDispCivicAddressReport);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TDispCivicAddressReport.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TDispCivicAddressReport.GetDefaultInterface: IDispCivicAddressReport;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TDispCivicAddressReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TDispCivicAddressReportProperties.Create(Self);
{$ENDIF}
end;

destructor TDispCivicAddressReport.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TDispCivicAddressReport.GetServerProperties: TDispCivicAddressReportProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TDispCivicAddressReport.Get_AddressLine1: WideString;
begin
    Result := DefaultInterface.AddressLine1;
end;

function TDispCivicAddressReport.Get_AddressLine2: WideString;
begin
    Result := DefaultInterface.AddressLine2;
end;

function TDispCivicAddressReport.Get_City: WideString;
begin
    Result := DefaultInterface.City;
end;

function TDispCivicAddressReport.Get_StateProvince: WideString;
begin
    Result := DefaultInterface.StateProvince;
end;

function TDispCivicAddressReport.Get_PostalCode: WideString;
begin
    Result := DefaultInterface.PostalCode;
end;

function TDispCivicAddressReport.Get_CountryRegion: WideString;
begin
    Result := DefaultInterface.CountryRegion;
end;

function TDispCivicAddressReport.Get_DetailLevel: LongWord;
begin
    Result := DefaultInterface.DetailLevel;
end;

function TDispCivicAddressReport.Get_Timestamp: TDateTime;
begin
    Result := DefaultInterface.Timestamp;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TDispCivicAddressReportProperties.Create(AServer: TDispCivicAddressReport);
begin
  inherited Create;
  FServer := AServer;
end;

function TDispCivicAddressReportProperties.GetDefaultInterface: IDispCivicAddressReport;
begin
  Result := FServer.DefaultInterface;
end;

function TDispCivicAddressReportProperties.Get_AddressLine1: WideString;
begin
    Result := DefaultInterface.AddressLine1;
end;

function TDispCivicAddressReportProperties.Get_AddressLine2: WideString;
begin
    Result := DefaultInterface.AddressLine2;
end;

function TDispCivicAddressReportProperties.Get_City: WideString;
begin
    Result := DefaultInterface.City;
end;

function TDispCivicAddressReportProperties.Get_StateProvince: WideString;
begin
    Result := DefaultInterface.StateProvince;
end;

function TDispCivicAddressReportProperties.Get_PostalCode: WideString;
begin
    Result := DefaultInterface.PostalCode;
end;

function TDispCivicAddressReportProperties.Get_CountryRegion: WideString;
begin
    Result := DefaultInterface.CountryRegion;
end;

function TDispCivicAddressReportProperties.Get_DetailLevel: LongWord;
begin
    Result := DefaultInterface.DetailLevel;
end;

function TDispCivicAddressReportProperties.Get_Timestamp: TDateTime;
begin
    Result := DefaultInterface.Timestamp;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TLocation, TDefaultLocation, TLatLongReportFactory, TCivicAddressReportFactory, 
    TDispLatLongReport, TDispCivicAddressReport]);
end;

end.
