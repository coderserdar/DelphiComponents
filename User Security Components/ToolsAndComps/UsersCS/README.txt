TUsersCS Security Component v1.971
Tools&Comps - Security Components for Delphi and C++ Builder
Web Site: http://www.toolsandcomps.com
  e-mail: info@toolsandcomps.com
  Addres: Rua do Beijo, 9 - Novo México
             Vila Velha - ES - Brasil
     CEP: 29104-080-020
   Phone: 55 27 33891138/55 27 99602760

Last Update: January 02, 2003

CONTENTS
--------

· Installation
· Sample Applications 
· Changing the idiom of the component
· What's new
· Important information for MS-Access users
· Also check in our web site

Installation
------------

To install TUsersCS in your computer: 

1. Remove any previous versions of TUsersCS you have installed. Go to the menu "Components|Install Packages" and select the packages of TUsersCS, clicking on the button "Removes it". Also check for old DCPs and BPLs (p_editors_pkg.*, users_basic_v19.* and users_cs_v19.*) in the  $(Delphi)\Projects\BPL directory, deleting them.

2. In the menu "File|Open", open the packages users_basic_v19 users_cs_v19 and p_editors_pkg and click on the button Install for each package (in this order). The packages are in the directory C:\ToolsAndComps\UsersCS\Delphi5Trial 

OR

In the menu "Components|Install Packages", click on the button "Add..." and select the BPLs in that are in the directory C:\ToolsAndComps\UsersCS\Delphi5Trial 

OBS: For other Delphi versions, choose the corresponding directory.

3.  Update the Library Path configuration of your Delphi including the directory: 
· c:\ToolsAndComps\UsersCS\Delphi5Trial 
· c:\ToolsAndComps\UsersCS\CRYPTO 
· c:\Program Files\Borland\Delphi5\Source\ToolsAPI (Please, verify the directory of your Delphi installation ) 

NOTE: You can also include those directories in the Search Path of your project.

Sample Applications
--------------------

The sample applications uses a set of Paradox tables and does not needs extra configurations in order tu run. The Paradox tables can be found in ToolsAndComps\UsersCS\Samples\Data.

If you are going to test the SAC.EXE (Security Administration Central) application, you must run all the other applications as the master user first in order to register them in the security database.

C++ Builder Users: note that all samples are referencing the ToolsAndComps\UsersCS\CB5Trial directory. If you are using CB6, you will need to modify these references in the Project | Options | Directories/Conditionals options, updating the "Include Path" and "Library Path" values.

Changing the idiom of the component  
-----------------------------------  
  
In case the language presented by the component it is not the language of your choice, go to the directory UsersCS\Resources and copy the files that are inside of the directory of the language of your choice to the directory where component DCU's are stored. After copying the files, recompile your application.

What's new 
----------

Version 1.971 (January 02, 2003)

* Suport to ActionBands
* D7 version
* some internal changes

Version 1.97 (July 29, 2002)

* The property FormName was not beeing updated correctly when the component was beeing used in inherited forms. 
* The parent of a protected component was not beeing updated in the security database when the developer changed its parent (For example, if you change a TButton from a TPanel to another TPanel)

Version 1.96 (July 07, 2002)

* Bug Fixed: Stack overflow when using TFrames.
* Bug Fixed: Stack overflow when using several nested PageControls
* Added correct support to the TdxSideBar component, from DevExpress. Take a look at the sample application ("Samples\DevExpress SideBar")
* Bug Fixed: problems when installing the package in Delphi 5 

Version 1.95 (May 21, 2002)

* Now the components in the permission treeview (user administration module) are shown in the same order they are in the forms
* a small change in the component logic allows a considerable performance gain when the application is loading .
* fixed a bug in the run-time components registration feature: the user settings were not beeing applied correctly.
* now the Component Registration Form has a Cancel button.
* a small change in the login trace algorithm now allows to clear blocked users in the security database.

Version 1.94 (April 10, 2002)

· Added complete support to the Logout method: the function now disables all the components which are beeing protected by the security component as also finishes the user session in the application.
· Fixed a bug when posting login trace information: the problem was happening when the Database which the security component is connected was destroyed before the security component.
· Now when your application is started and the login method is not executed, all the components beeing protected becomes disabled.
· New property DefaultSetting: now the developer can choose the default security setting for thecomponents when the administrator creates a new user/profile or when you protect a new component.
· Bug Fixed: when there was a user change, throught the login method, some forms which were already open were not beeing updated with the security settings of the new user.

Version 1.93 (March 11, 2002)

· Fixed a problem when posting login trace data in some Windows NT and Windows NT Workstation versions. The component could not obtain the computer name.
· The Component Registration Form now supports correctly more than one Developer Express component in the same form. There was a limitation of only one component in each form.
· a few internal changes.
· Now reports are only available in the registered version, to avoid the compiler error message 'unit xxx.pas was compiled with a different version of unit yyy.pas', when you use a QR version which differs of the QR version used to compile the component.

Version 1.92 (December 27, 2001)

· Fixed BUG: Access Violation when executing a application from Delphi IDE and this application have a TScrollBox beeing secured by the component.
· Fixed BUG: Access Violation when protecting some Info Power Library components.
· Fixed Problem: TimeOut feature was not working properly on Windows 2000
· Fixed BUG: "qryUserInfo: Cannot perform this operation on a closed dataset". This error was happening the user loggins as master with no password until the MaxBadLogins limit is reached, beeing the application unprotected.

Version 1.90 (December 27, 2001)

· New property AutoApplySecurity (TUsersCSReg): allows the developer to choose if the security will be applyied automatically when the user access the form or if the security will be applyied manually by the developer using the method ApplySecurity.
· New method SetDabataseConnection (TUsersCSReg): method which allows to use the component in forms which are in in DLLs. See the sample "Samples\Forms in DLLs".
· New property IsRepositoryForm (TUsersCSReg): method used to inform to the component if the current form/datamodule is a template and it will be stored in the Repositoty Objects. See the sample "Samples\Repository Templates".
· New events BeforeApplySecurity and AfterApplySecurity (TUsersCSReg)
· New property UserIsMaster (Classe TUser)
· New property LastLoginDateTime (Classe TUser)
· Removed warnings and hints.
· Fixed BUG: Button Update Users (Tab Permissions) was not working well in some cases.
· Fixed BUG: the methods GetComponentInfo and GetUserComponentInfo was not working well if executed from the OnCreate event.
· Fixed BUG: Aliases created within other sessions was not beeing visibles by the component.
· Fixed BUG: ActionsItems was showing twice in the Component Registration Form.
· Fixed BUG: Login Trace information was beeing deleted when the same user was using more than one instance of the same application (only when the application was configured to allows multiple logins from the same user)
· Fixed BUG: Users was beeing disabled continually (even when they where enabled again by the administrator) when the Maximum number of days without using the application was reached.

Version 1.83 (September 14, 2001)

· Recording Audit data is faster: an internal change made possible the recording of the audit module data more quickly . 
· Information of Login/Logout are not recorded to the user 'master'
· Fixed BUG: OnApplySecurity was not beeing triggered for some components
· The logout information now are recorded when the connection with the database is closed, and not more when the application is finished. This foresees errors in the finishing of the application when the connection component is in one form different of form where is the security component and the form with the connection component was destroyed before to the finishing of the application. 
· Now is possible to use the TUsersCSReg in inherited forms 

Version 1.82 (August 24, 2001)

· Fixed a Access Violation Bug when the main component was removed from a fomr in the Delphi IDE.
· Added support to ActiveX based applications. Now you can use the security components in ActiveForms.

Version 1.8(July 21, 2001)

· Changes when securing a Dataset: Now when you apply security restrictions to a Dataset which its Fields where not selected for protection, TUsersCS automaticaly copies the Dataset configuration to the Fields 
· Fixed BUG: Registering components at run-time was not working properly.
· New event OnApplySecurity: New TUsersCSReg event, which is triggered for each secured Object. This event has as parameters the Object which will be secured by the component as also the security information of the Object regarding the current user. With this event the developer can perform extra configuration in the Object beeing secured and also inform TUsesCSReg if the component must proceed or not with the security configuration to the Object.
· Now the component also suports TCustomActions

Version 1.7 (July 01, 2001) 

· Fixed BUG: When applying the security configuration into a TPageControl with several TTabSheets, the component was making the last TTabSheet as the Active TabSheet.
· Fixed BUG: In the Component Registration Form, some unnecessary components were beeing showed, like TDatasources.
· Fixed BUG: In the Component Registration Form, now all the controls inside a TCoolbar are beeing showed
· Fixed BUG: In the Component Registration Form, some components were beeing showed twice

Version 1.7 (May 30, 2001) 

· Fixed BUG: New profiles where showed only after re-starting the User Administration Module
· Fixed BUG: The component was not fixing the Login Trace information properly when a aplication have abnormal termination and was load again. This error just was happening when you were using Paradox files with short names (property FileNames83DOS as True)
· New events: BeforeLogins, WhileDoLogin and AfterLogin (See the samples)

Version 1.7 (May 15, 2001) 

· Was Added new propertye MaxCurrentUsers(Integer). The property MaxCurrentUsers indicates the maximum number of users that can be running the application at the same time. 
· Was Added new property HaltOnReachMaxCurrentUsers(Boolean). The property HaltOnReachMaxCurrentUsers indicates if the component will terminate the application if the maximum number of current users is reached
· Was Added new function GetNumberOfCurrentUsers. This function returns the number of users that are using the application when the function is called.

Version 1.7 (April 19, 2001) 
 
· Support to Developer Express components (Express Bars, Express Quantum Grid and Express Quantum TreeList).   
· We did a substantial change in the algorithm for exhibition of components in the registration form and now almost all visual components will be showed in the Component Registration Form.  
· New method and an event for the registration of components in run time. 
· The standard cryptography algorithm of the component was changed. There was a bug in the old algorithm, that didn't allow decrypt texts with letters duplicated in sequence correctly. Old users need to execute the program in order to change the password’s cryptography.  
· We increased the size of the password’s field for 60 characters. That allows the use of other cryptography algorithms. Old users need to execute the program in order to update the security database.
· We also increased the size of the of the user's name field for 30 characters. Old users need to execute the program in order to update the security database.
· New method that allows to verify the status of a component in relation to an user that is not logged in the application. 
· New form for visualization of the registered forms.  
· Corrected referring BUGs to columns that don't accept null values and to names of ambiguous columns in some databases (MS-SQL Server and Oracle).  
· New method that shows the list of all the applications registered in the security database. Now it is possible to have an only application to manage the security administration. 
· Corrected  a installation problem of the packages.  
· New property booleand MSAccess, that indicates to the component if the security database is stored in a MS-Access database.  
· Due to some requisitions, we made available again TUsersCS for Delphi 3.  
· Included new examples.

Version 1.6 and 1.5.5

· Was Added support to Actions
· Event for pasword validation 
· Customization of the user’s cadaster screen
· Access restriction to records (Needs programming)
· Application Time Out 
· Several Reports
	a) User Listing
	b) Profile Listing
	c) User Permissions Listing
	d) Profile Permissions Listing
	e) Listing of the Login Activities of a User 
	f) Listing of the Component Utilization by the Users 
	g) Listing of the User ACtivities 
· Do not allows multiple logins for a same user name 
· Disable a user after N days without using the application
· Disable a user after N attemps of login with no success
· New field ADDITIONAL_INFO to be used to keep extra user information, making possible to restrict records from some users
· Better performance
· More flexibility when configuring the access of the users: now is possible to mix among the options Visible, Invisible and Visible but disabled for each component.

Important information for MS-Access users
-----------------------------------------
The TUsersCS component has a flag that must be modified in order to use a MS-Access file as the database for the security tables. Just select the component in the Object Inspector and set the value of the property MSAccess to true.

Also check in our web site
-------------------------

You can visit our web site or make contact with us to ckeck out other versions of our security components. We have versions for the following databases/data access components:

* DBISAM (http://www.elevatesoft.com)
* ASTA (http://www.astatech.com)
* Interbase Objects (http://www.ibobjects.com)
* Advantage Database System (http://www.advantagedatabase.com)
* MySQL (htpp://www.scibit.com)
* Direct Oracle Access (http://www.allroundautomations.com
* ADO, MIDAS, IBX and dbExpress (Delphi native components)

Thank you for choose TUsersCS

If you find any bug in this component or you can't test it for any reason, please, report us the error so we can fix it.

->>> Read the user manual carefully for better undestanding the new features
->>> Take a look in the sample applications that are located in the directory UsersCS\Samples for a sample of utilization of the component features.

