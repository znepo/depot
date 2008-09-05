; package-win.nsi

; -------------------------------
; Start
 
!define MUI_BRANDINGTEXT "Snepo Depot Ver. 1.0"
CRCCheck On
 
;!include "${NSISDIR}\Contrib\Modern UI\System.nsh"
!include "MUI.nsh"

;--------------------------------
;General

; The name of the installer
Name "Depot" 

; The file to write
OutFile "install-depot.exe" 

; The default installation directory
InstallDir c:\depot

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\Snepo\Depot" "Install_Dir"

; Other settings
ShowInstDetails "nevershow"
ShowUninstDetails "nevershow"

;--------------------------------
;Modern UI Configuration

; MUI Settings / Icons
;!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\orange-install.ico"
!define MUI_ICON "nsis\snepo.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\orange-uninstall.ico"
 
; MUI Settings / Header
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_RIGHT
!define MUI_HEADERIMAGE_BITMAP "nsis\depot-header.bmp"
!define MUI_HEADERIMAGE_UNBITMAP "nsis\depot-header-uninstall.bmp"
 
; MUI Settings / Wizard
!define MUI_WELCOMEFINISHPAGE_BITMAP "nsis\depot-side.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "nsis\depot-side-uninstall.bmp"

; MUI Settings / Installation
!define MUI_INSTFILESPAGE_PROGRESSBAR "smooth"

; MUI Settings / Components
!define MUI_COMPONENTSPAGE_SMALLDESC

; MUI Settings / Directory
!define MUI_DIRECTORYPAGE_TEXT_TOP "Setup will install Depot in the following folder. To install in a different folder, click Browse and select another folder. Click Install to start the installation. NOTE: Depot will not work if the path contains a space!"
!define MUI_DIRECTORYPAGE_TEXT_DESTINATION "Destination Folder - DEPOT WILL NOT WORK IF THE PATH CONTAINS A SPACE!"

; MUI Settings / Finish
!define MUI_FINISHPAGE_SHOWREADME "README.txt"
;!define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
;!define MUI_FINISHPAGE_SHOWREADME_TEXT "Show Readme"

!define MUI_FINISHPAGE_LINK "Depot Website"
!define MUI_FINISHPAGE_LINK_LOCATION "http://depot.snepo.com"
!define MUI_FINISHPAGE_NOREBOOTSUPPORT

; run the Flash IDE install
!define MUI_FINISHPAGE_RUN "install-mxp.bat"
!define MUI_FINISHPAGE_RUN_TEXT "Install Flash IDE Tool"
;!define MUI_FINISHPAGE_RUN_PARAMETERS ""
!define MUI_FINISHPAGE_RUN_NOTCHECKED

;---------------------------------
;Modern UI System
    
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "license.txt"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH  

!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH
 
;Language
!insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Data
 
LicenseData "license.txt"


;--------------------------------
; Installer Sections

Section "Depot Base (required)" Section1

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR

  ; Add files
  File /r staging\win_xp\depot\*

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\Snepo\Depot "Install_Dir" "$INSTDIR"

  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Depot" "DisplayName" "Snepo Depot"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Depot" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Depot" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Depot" "NoRepair" 1
  WriteUninstaller "uninstall.exe"

  ; Add Environment Variable
  !define ALL_USERS
  Push DEPOT_HOME
  Push $INSTDIR
  Call WriteEnvStr
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts" Section2

  CreateDirectory "$SMPROGRAMS\Snepo\Depot"
  CreateShortCut "$SMPROGRAMS\Snepo\Depot\depot-console.lnk" "$INSTDIR\depot-console.exe" "" "$INSTDIR\depot-console.exe" 0
  CreateShortCut "$SMPROGRAMS\Snepo\Depot\start-web.lnk" "$INSTDIR\start-web.bat" "" "$INSTDIR\start-web.bat" 0
  CreateShortCut "$SMPROGRAMS\Snepo\Depot\start-depot-service.lnk" "$INSTDIR\start-depot-service.bat" "" "$INSTDIR\start-depot-service.bat" 0
  CreateShortCut "$SMPROGRAMS\Snepo\Depot\stop-depot-service.lnk" "$INSTDIR\stop-depot-service.bat" "" "$INSTDIR\stop-depot-service.bat" 0
  CreateShortCut "$SMPROGRAMS\Snepo\Depot\Depot Explorer.lnk" "$INSTDIR\flash\Depot Explorer.exe" "" "$INSTDIR\flash\Depot Explorer.exe" 0
  CreateShortCut "$SMPROGRAMS\Snepo\Depot\User Manual.lnk" "$INSTDIR\doc\users.pdf" "" "$INSTDIR\doc\users.pdf" 0
  CreateShortCut "$SMPROGRAMS\Snepo\Depot\Flash Tutorial.lnk" "$INSTDIR\doc\flash-tutorial.pdf" "" "$INSTDIR\doc\flash-tutorial.pdf" 0
  CreateShortCut "$SMPROGRAMS\Snepo\Depot\Wiki Tutorial.lnk" "$INSTDIR\doc\wiki-tutorial.pdf" "" "$INSTDIR\doc\wiki-tutorial.pdf" 0
  ;CreateShortCut "$SMPROGRAMS\Snepo\Depot\Uninstall depot.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Install Depot Service" Section3

  ; Service (auto starting)
  nsSCM::Install /NOUNLOAD "Depot Server" "Depot Server" 16 2 \
                           "$INSTDIR\bin\depot.exe" "" "" "" ""
  Pop $0 ; return error/success

  nsSCM::Start /NOUNLOAD "Depot Server"
  Pop $0 ; return error/success

  
SectionEnd

;--------------------------------
; Uninstaller

Section "Uninstall"

  ; Remove depot service
  nsSCM::Stop /NOUNLOAD "Depot Server"
  Pop $0 ; return error/success
  nsSCM::Remove /NOUNLOAD "Depot Server"
  Pop $0 ; return error/success

  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Depot"
  DeleteRegKey HKLM SOFTWARE\Snepo\Depot

  ; Remove all files
  RMDir /r "$INSTDIR\*.*"

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\Snepo\Depot\*.*"

  ; Remove directories used
  RMDir "$SMPROGRAMS\Snepo\Depot"
  RMDir "$SMPROGRAMS\Snepo"
  RMDir "$INSTDIR"

  ; Remove Environment Variable
  Push DEPOT_HOME
  Call un.DeleteEnvStr

SectionEnd

;--------------------------------    
;MessageBox Section
 
 
;Function that calls a messagebox when installation finished correctly
;Function .onInstSuccess
;  MessageBox MB_OK "You have successfully installed ${MUI_BRANDINGTEXT}."
;FunctionEnd

Function un.onInit
    MessageBox MB_YESNO "This will delete all depot data from the system. Are you sure you want to do continue?" IDYES NoAbort
      Abort ; causes uninstaller to quit.
    NoAbort:
FunctionEnd
 
 
;Function un.onUninstSuccess
;  MessageBox MB_OK "You have successfully uninstalled ${MUI_BRANDINGTEXT}."
;FunctionEnd
 
;--------------------------------
; Component Descriptions

LangString DESC_Section1 ${LANG_ENGLISH} "The depot server and web server are included in the base package."
LangString DESC_Section2 ${LANG_ENGLISH} "Start menu shortcuts to start depot and the webserver."
LangString DESC_Section3 ${LANG_ENGLISH} "Install Depot as a windows service and start automatically."

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${Section1} $(DESC_Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${Section2} $(DESC_Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${Section3} $(DESC_Section3)
!insertmacro MUI_FUNCTION_DESCRIPTION_END

!ifndef _WriteEnvStr_nsh
!define _WriteEnvStr_nsh
 
!include WinMessages.nsh
 
!ifndef WriteEnvStr_RegKey
  !ifdef ALL_USERS
    !define WriteEnvStr_RegKey \
       'HKLM "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"'
  !else
    !define WriteEnvStr_RegKey 'HKCU "Environment"'
  !endif
!endif
 
#
# WriteEnvStr - Writes an environment variable
# Note: Win9x systems requires reboot
#
# Example:
#  Push "HOMEDIR"           # name
#  Push "C:\New Home Dir\"  # value
#  Call WriteEnvStr
#
Function WriteEnvStr
  Exch $1 ; $1 has environment variable value
  Exch
  Exch $0 ; $0 has environment variable name
  Push $2
 
  Call IsNT
  Pop $2
  StrCmp $2 1 WriteEnvStr_NT
    ; Not on NT
    StrCpy $2 $WINDIR 2 ; Copy drive of windows (c:)
    FileOpen $2 "$2\autoexec.bat" a
    FileSeek $2 0 END
    FileWrite $2 "$\r$\nSET $0=$1$\r$\n"
    FileClose $2
    SetRebootFlag true
    Goto WriteEnvStr_done
 
  WriteEnvStr_NT:
      WriteRegExpandStr ${WriteEnvStr_RegKey} $0 $1
      SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} \
        0 "STR:Environment" /TIMEOUT=5000
 
  WriteEnvStr_done:
    Pop $2
    Pop $0
    Pop $1
FunctionEnd
 
#
# un.DeleteEnvStr - Removes an environment variable
# Note: Win9x systems requires reboot
#
# Example:
#  Push "HOMEDIR"           # name
#  Call un.DeleteEnvStr
#
Function un.DeleteEnvStr
  Exch $0 ; $0 now has the name of the variable
  Push $1
  Push $2
  Push $3
  Push $4
  Push $5
 
  Call un.IsNT
  Pop $1
  StrCmp $1 1 DeleteEnvStr_NT
    ; Not on NT
    StrCpy $1 $WINDIR 2
    FileOpen $1 "$1\autoexec.bat" r
    GetTempFileName $4
    FileOpen $2 $4 w
    StrCpy $0 "SET $0="
    SetRebootFlag true
 
    DeleteEnvStr_dosLoop:
      FileRead $1 $3
      StrLen $5 $0
      StrCpy $5 $3 $5
      StrCmp $5 $0 DeleteEnvStr_dosLoop
      StrCmp $5 "" DeleteEnvStr_dosLoopEnd
      FileWrite $2 $3
      Goto DeleteEnvStr_dosLoop
 
    DeleteEnvStr_dosLoopEnd:
      FileClose $2
      FileClose $1
      StrCpy $1 $WINDIR 2
      Delete "$1\autoexec.bat"
      CopyFiles /SILENT $4 "$1\autoexec.bat"
      Delete $4
      Goto DeleteEnvStr_done
 
  DeleteEnvStr_NT:
    DeleteRegValue ${WriteEnvStr_RegKey} $0
    SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} \
      0 "STR:Environment" /TIMEOUT=5000
 
  DeleteEnvStr_done:
    Pop $5
    Pop $4
    Pop $3
    Pop $2
    Pop $1
    Pop $0
FunctionEnd
 
!ifndef IsNT_KiCHiK
!define IsNT_KiCHiK
 
#
# [un.]IsNT - Pushes 1 if running on NT, 0 if not
#
# Example:
#   Call IsNT
#   Pop $0
#   StrCmp $0 1 +3
#     MessageBox MB_OK "Not running on NT!"
#     Goto +2
#     MessageBox MB_OK "Running on NT!"
#
!macro IsNT UN
Function ${UN}IsNT
  Push $0
  ReadRegStr $0 HKLM \
    "SOFTWARE\Microsoft\Windows NT\CurrentVersion" CurrentVersion
  StrCmp $0 "" 0 IsNT_yes
  ; we are not NT.
  Pop $0
  Push 0
  Return
 
  IsNT_yes:
    ; NT!!!
    Pop $0
    Push 1
FunctionEnd
!macroend
!insertmacro IsNT ""
!insertmacro IsNT "un."
 
!endif ; IsNT_KiCHiK
 
!endif ; _WriteEnvStr_nsh 
 
 
;eof 
