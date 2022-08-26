cls
@echo off

REM ----------------------------------------------------------------------------
REM
REM if you jave Java JDK on your machin you need to add this switch after javac command
REM javac -source 1.6 -target 1.6
REM
REM ----------------------------------------------------------------------------

setlocal

if x%ANDROID% == x set ANDROID="d:\Program Files (x86)\Embarcadero\Studio\14.0\PlatformSDKs\adt-bundle-windows-x86-20131030\sdk"
set ANDROID_PLATFORM=%ANDROID%\platforms\android-19
set DX_LIB=%ANDROID%\build-tools\android-4.4\lib
set DX_PATH=%ANDROID%\build-tools\android-4.4

set EMBO_DEX="d:\Program Files (x86)\Embarcadero\Studio\15.0\lib\android\debug\classes.dex"
set PROJ_DIR=%CD%
set VERBOSE=0

echo.
echo Compiling the Java Sources
echo.
mkdir output 2> nul
mkdir output\classes 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=-verbose
javac %VERBOSE_FLAG% -Xlint:deprecation -cp %ANDROID_PLATFORM%\android.jar -d output\classes ^
src\com\dpfaragir\DPFActivity.java ^
src\com\dpfaragir\DPFTextView.java ^
src\com\dpfaragir\DPFUtils.java ^
src\com\dpfaragir\DPFVideoView.java ^
src\com\dpfaragir\http\DPFHTTP.java ^
src\com\dpfaragir\http\DPFOnHTTPListener.java ^
src\com\dpfaragir\webview\DPFOnWebViewListener.java ^
src\com\dpfaragir\webview\DPFWebClient.java ^
src\com\dpfaragir\webview\DPFWebView.java ^
src\com\dpfaragir\listview\DPFListView.java ^
src\com\dpfaragir\listview\DPFOnListViewListener.java ^
src\com\dpfaragir\Animation\DPFOnAnimationListener.java ^
src\com\dpfaragir\Animation\DPFAnimation.java

echo.
echo Creating jar containing the new classes
echo.
mkdir output\jar 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=v
jar c%VERBOSE_FLAG%f output\jar\test_classes.jar -C output\classes com

echo.
echo Converting from jar to dex...
echo.
mkdir output\dex 2> nul
if x%VERBOSE% == x1 SET VERBOSE_FLAG=--verbose
call %DX_LIB%\dx.jar --dex %VERBOSE_FLAG% --output=%PROJ_DIR%\output\dex\test_classes.dex --positions=lines %PROJ_DIR%\output\jar\test_classes.jar

echo.
echo Merging dex files
echo.
java -cp %DX_LIB%\dx.jar com.android.dx.merge.DexMerger %PROJ_DIR%\output\dex\classes.dex %PROJ_DIR%\output\dex\test_classes.dex %EMBO_DEX%

echo Finished Merging
echo.
del output\classes\com\dpfaragir\*.class

rmdir output\classes\com\dpfaragir /s /q
rmdir output\classes\com /s /q
rmdir output\classes /s /q

del output\dex\test_classes.dex
del output\jar\test_classes.jar
rmdir output\jar /s /q

echo.
echo goto output\dex\classes.dex
copy output\dex\classes.dex ..\Classes\classes.dex

:Exit

endlocal
