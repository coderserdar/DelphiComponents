D.P.F Android Native Components
-----------------------------------------------

How to install and use:

1) Go Tools->Options->Delphi Options->Library and :
1-1) change Selected Pltaform to 32-bit windows and add X:\DPF.Android.Native.Components\ in library path
1-2) Then change Selected Platform to Android and add X:\DPF.Android.Native.Components\ in library path


-------------------------------------------------------------------------------------------
XE5: (Not support since v2.8.6 )
2) DPFAndroidPackagesXE5.dpk
 2-1) Select 32-bit windows and build then install. 

-------------------------------------------------------------------------------------------
XE6:
3) DPFAndroidPackagesXE6.dpk
 3-1) Select 32-bit windows and build then install. 

-------------------------------------------------------------------------------------------
XE7:
3) DPFAndroidPackagesXE7.dpk
 3-1) Select 32-bit windows and build then install. 

-------------------------------------------------------------------------------------------

4) Merging classes.dex
  4-1) Open build.bat file in editor and set ANDROID and EMBO_DEX variables
  4-2 Or use my changed Classes\classes.dex ( Only Delphi XE7 )

Note:

If you cant make this file successfully, all demos or you new project on device will be shown as black screen...

When the small Java source files get compiled, the Android dx tool expects them to be compiled by the JDK 1.6.x compiler as opposed to the JDK 1.7.x compiler. If you have JDK 1.7.x installed, you hit a problem with dx reporting:
bad class file magic (cafebabe) or version (0033.0000)
However, to avoid forcing a reinstall of JDK 1.6 you might like to modify build.bat batch files and add in extra command line switches to the javac.exe command-lines. You need to insert this after the javac.exe command to force Java 1.6 byte code output, which is digestible by the Android dx command:

-source 1.6 -target 1.6

5) Before deploying Demos on Simulator or Device dont forget : Go to
Project Menu - > Deployment -> Deply

(very important)
6) Make sure delphi default classes.dex file must be unchecked in Deployment window and new
   Classes\classes.dex with classes\ remote path name, must be added in the Deployment window.
   (see classes_dex.png image)

(Thanks to "Brian Long" for merging dex files)

7) in your projects->Options->Uses Permissions : make sure turn on :
 7-1) Access coarse location
 7-2) Access Wifi State
 7-3) Access Network State
 7-4) Access fine location
 7-5) Call phone
 7-6) Camera
 7-7) Internet
 7-8) Read Calendar
 7-9) Read External Storage
 7-10) Read Phone State
 7-11) Write Calendar
 7-12) Write External Storage
 7-13) WAKE LOCK


Enjoy it!

-------------------------------------------------------------------
© 2011-2014 Dadeh Pardazane Faragir (D.P.F). All rights reserved.
http://www.dpfaragir.com

Developed by: Babak Yaghoobi ( b_yaghobi@yahoo.com or bayaghoobi@gmail.com)


D.P.F Delphi Android Native Components

History

--------------------------------------------------------------
v2.8.6 - 2014/12/03
  + Added Map View (Thanks to Roman Yankovsky)
  - Removed Delphi XE5 Supported

--------------------------------------------------------------
v2.8.5 - 2014/09/17
  + Added Delphi XE7 Supported

--------------------------------------------------------------
v2.8.1 - 2014/05/17
  + Added Delphi XE6 Supported
  + Added TDPFJAnimation Component
  + Added TDPFJHTTP (with Authentication, http, https, upload file)
  + Added TDPFJTextView Smooth Scroll
  + Added DPF.Android.DPFUtils (GetIPAddress, isInternetActive, getMACAddress)
  + Added LoadFromStream in TDPFJImageView
  - Fixed Layout position in XE6

--------------------------------------------------------------
v2.6.0 - 2014/02/04
  + Added HTTPDownload Demo
  + Added TDPFJHTTP (with Authentication)
  + Wrapped Android.OS.Environment

--------------------------------------------------------------
v2.5.4 - 2014/02/03
  + Added TDPFJRelativeLayout
  + Added TDPFJLinearLayout
  + Added TDPFJListView 
  + Added TDPFJWebView WebSettings property(JavaScriptEnabled, UserAgentString, ...)
  - Fixed Controls align(s) on any device!
  - Fixed TDPFJTabHost PageIndex Order
  - Fixed build.bat compile error
  * Removed TDPFJView, use : TDPFJRelativeLayout, ...

--------------------------------------------------------------
v2.0.1 - 2014/01/25
  + Wrapped Android.Net.wifi
  + Added modified classes.dex file
  + Added DPF.Android.Common GetWifiMacAddress
  + Added DPF.Android.Common GetDeviceLocation
  + Added DPF.Android.Common GetDeviceInfo
  + Added TDPFJWebView StopLoading
  + Added TDPFJWebView doUpdateVisitedHistory Event
  + Added TDPFJWebView OnFormResubmission Event
  + Added TDPFJWebView OnLoadResource Event
  + Added TDPFJWebView OnPageFinished Event
  + Added TDPFJWebView OnPageStarted Event
  + Added TDPFJWebView OnReceivedError Event
  + Added TDPFJWebView OnReceivedHttpAuthReques Event
  + Added TDPFJWebView OnReceivedSslError Event
  + Added TDPFJWebView OnScaleChanged Event
  + Added TDPFJWebView onUnhandledKeyEvent Event
  + Added TDPFJWebView ShouldOverrideKeyEvent Event
  + Added TDPFJWebView ShouldOverrideUrlLoading Event

--------------------------------------------------------------
v1.6.8 - 2014/01/19
  + Added TDPFJTextClock
  + Added TDPFJWebView
  - Fixed EditText Delete Key
  - Fixed Button resize
  - Fixed TextView Scrolling

--------------------------------------------------------------
v1.5.4 - 2013/12/19
  + Added TDPFJTabHost Component (Tabbed view)
  + Added TDPFJSpinner Component (Dropdown list)
  + Added TDPFJTextSwitcher Component (Fade/Slide Texts)
  - Fixed RadioGroup OnChecked Id

--------------------------------------------------------------
v1.2.1 - 2013/12/15
  + Added TDPFJScrollView component
  + Added TDPFJSeekBar
  * Improved BaseControl 
  - Fixed Width/Height 

--------------------------------------------------------------
v1.0.0 - 2013/12/14
  * The first release 
  + Added TDPFJAlertDialog
  + Added TDPFJAnalogClock
  + Added TDPFJButton
  + Added TDPFJCheckBox
  + Added TDPFJChronometer
  + Added TDPFJDatePicker
  + Added TDPFJDatePickerDialog
  + Added TDPFJEditText
  + Added TDPFJImageView
  + Added TDPFJListView (not completed)
  + Added TDPFJProgressBar
  + Added TDPFJProgressDialog
  + Added TDPFJTextView
  + Added TDPFJTimePickerDialog
  + Added TDPFJToast
  + Added TDPFJView
  + Added TDPFJRadioButton
  + Added TDPFJNumberPicker
  + Wrapped some Android.Widget
  + Wrapped some Android.Net
  + Wrapped some Android.OS
  + Wrapped some Android.R

------------------------------------
Source Code: http://sourceforge.net/projects/dpfdelphiandroid/
------------------------------------
© 2011-2014 Dadeh Pardazane Faragir (D.P.F). All rights reserved.
http://www.dpfaragir.com
