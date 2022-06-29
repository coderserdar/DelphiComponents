Cindy components install instructions 


I. Uninstall previous versions : 


In Delphi IDE, select the 'Component' menu and click on 'Install packages' item. 
In the new window showing all packages installed, locate Cindy packages and remove with 'Remove' button. 
Click 'Ok' to close the window. 
Delete all files from Cindy components directory. 



II. Install Cindy components: 

Tips: 
To compile and install packages, you can use the "project manager" (On Delphi IDE main menu, click on "View" and select "Project manager"). The project manager panel will be displayed: right click on *.bpl package file and select "Build" or "compile" and finally "Install" like explained bellow. 

Note: 
- Only runtime packages (pkCindy*.dpk) must be built for Windows 64 bit platform development.
- You may need to add "64-bit Windows" platform for Windows 64 bit platform development.


If you have problems in installing any package:
- Try to delete *.dproj, *.dproj.2007 and *.dproj.local and open *.dpk. You can use "_clean all.bat" file. 
- Remove any {$R *.otares} declaration in *.dpk file
- You may need to add "vcl" in "project"->"options"->"unit scope names" if compilation errors occurs (since Delphi XE).    

1. Unzip downloaded file into the directory of your choice. 



2. Install (required) "Cindy System" package before installing other Cindy packages below!

- Open, build and install designtime package cyComponents\System\dpkCindySystemXXX.dpk for Windows 32 bit platform



3. Install (required) "Cindy Core" package before installing other Cindy packages below!

- Open and build runtime package cyComponents\Core\pkCindyCoreXXX.dpk for Windows 32 bit and/or 64 bit platforms
- Add the "cyComponents\Core" directory to the library paths:
On Delphi IDE, click menu "Tools", "(Environment) Options", and select "Library" and finally "library paths". Add the "cyComponents\Core" directory to the list for 32 and/or 64 bit platform. 


4. (NEW) Install "Cindy DB Core"  package (required for optionals "Cindy VCL DB components" and "DBExpress packages").

- Open and build runtime package cyComponents\Core\pkCindyDBCoreXXX.dpk for Windows 32 bit and/or 64 bit platforms



Now, you can choose from following packages components : 



5. Install (Optional) "Cindy VCL components" packages

- Open and build runtime package cyComponents\VCLPack\pkCindyPackXXX.dpk for Windows 32 bit and/or 64 bit platforms
- Open, build and install designtime package cyComponents\VCLPack\dpkCindyPackXXX.dpk for Windows 32 bit platform
- Add the "cyComponents\VCLPack" directory to the library paths:
On Delphi IDE, click menu "Tools", "(Environment) Options", and select "Library" and finally "library paths". Add the "cyComponents\VCLPack" directory to the list for 32 and/or 64 bit platform. 



6. Install (Optional) "Cindy VCL DB components" packages (not working on Delphi personal versions)

This package needs "Cindy DB Core" (new) and "Cindy VCL components" (previous packages) installed

- Open and build runtime package cyComponents\VCLPack\pkCindyDBCtrlsPackXXX.dpk for Windows 32 bit and/or 64 bit platforms
- Open, build and install designtime package cyComponents\VCLPack\dpkCindyDBCtrlsPackXXX.dpk for Windows 32 bit platform



7. Install (Optional) "Cindy VCL Internet Explorer" packages

Important note:
Because Cindy VCL Internet Explorer packages are based on ActiveX TwebBrowser component from Microsoft Internet Explorer program, you need to install Microsoft Internet explorer (version 4.0 or higher) on your system and import TwebBrowser ActiveX component in Delphi if it is not already installed. 
Note that all computers that runs your program with this package also need Microsoft Internet Explorer installed on the system. 

How to install the TWebBrowser component in Delphi IDE (if you don' t already have it):    
- Start Delphi.
- Click the menu "Component" and select "Import ActiveX Control..."
- In the list, locate and select "Microsoft Internet Controls".
- Choose the palette for the component
- Click "Install..." and proceed until the end. 

- Open and build runtime package cyComponents\VCLInternetExplorer\pkCindyIEXXX.dpk for Windows 32 bit and/or 64 bit platforms
- Open, build and install designtime package cyComponents\VCLInternetExplorer\dpkCindyIEXXX.dpk for Windows 32 bit platform
- Add the "cyComponents\VCLInternetExplorer" directory to the library paths:
On Delphi IDE, click menu "Tools", "(Environment) Options", and select "Library" and finally "library paths". Add the "cyComponents\VCLInternetExplorer" directory to the list for 32 and/or 64 bit platform. 



8. Install (Optional) "Cindy DB Express components" packages

This package needs "Cindy DB Core" (new) installed

- Open and build runtime package cyComponents\DBExpress\pkCindyDBXXXX.dpk for Windows 32 bit and/or 64 bit platforms
- Open, build and install designtime package cyComponents\DBExpress\dpkCindyDBXXXX.dpk for Windows 32 bit platform
- Add the "cyComponents\DBExpress" directory to the library paths:
On Delphi IDE, click menu "Tools", "(Environment) Options", and select "Library" and finally "library paths". Add the "cyComponents\DBExpress" directory to the list for 32 and/or 64 bit platform. 



9. Install (Optional) "Cindy Document Element Recognizer" packages

- Open and build runtime package cyComponents\DocumentElementRecognizer\pkCindyDERXXX.dpk for Windows 32 bit and/or 64 bit platforms
- Open, build and install designtime package cyComponents\DocumentElementRecognizer\dpkCindyDER for Windows 32 bit platform
- Add the "cyComponents\DBExpress" directory to the library paths:
On Delphi IDE, click menu "Tools", "(Environment) Options", and select "Library" and finally "library paths". Add the "cyComponents\DocumentElementRecognizer" directory to the list for 32 and/or 64 bit platform. 


