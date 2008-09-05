#include "HsFFI.h"
#include <windows.h>
#include <winsvc.h>
#include <stdio.h>
#include "depotsvc.h"
#include "Main_stub.h"

#define STARTED_AS_SERVICE 23
#define STARTED_AS_CONSOLE 32

SERVICE_STATUS DepotServiceStatus;
SERVICE_STATUS_HANDLE DepotServiceHandle;

VOID SvcDebugOut(LPSTR String, DWORD Status) {
/*   CHAR Buffer[1024]; */
/*   FILE *fp; */
/*   if(strlen(String) < 1000) { */
/*     wsprintf(Buffer,String,Status); */
/*     fp = fopen("c:\\tmp\\depot.log","a"); */
/*     fprintf(fp, String, Status); */
/*     fclose(fp); */
/*   }     */
}

VOID WINAPI depotServiceCtrlHandler(DWORD status) {
  switch(status) { 
  
  case SERVICE_CONTROL_PAUSE: 
    // Do whatever it takes to pause here. 
    DepotServiceStatus.dwCurrentState = SERVICE_PAUSED; 
    break; 
    
  case SERVICE_CONTROL_CONTINUE: 
    // Do whatever it takes to continue here. 
    DepotServiceStatus.dwCurrentState = SERVICE_RUNNING; 
    break; 
    
  case SERVICE_CONTROL_STOP: 
    // Do whatever it takes to stop here. 
    DepotServiceStatus.dwWin32ExitCode = 0; 
    DepotServiceStatus.dwCurrentState  = SERVICE_STOPPED; 
    DepotServiceStatus.dwCheckPoint    = 0; 
    DepotServiceStatus.dwWaitHint      = 0; 
    
    if (!SetServiceStatus (DepotServiceHandle, 
			   &DepotServiceStatus))
      { 
	status = GetLastError(); 
	SvcDebugOut(" [DEPOT SERVICE] SetServiceStatus error %ld\n", 
		    status); 
      } 
    
    SvcDebugOut(" [DEPOT SERVICE] Leaving Depot Server \n",0); 
    return; 
    
  case SERVICE_CONTROL_INTERROGATE: 
    // Fall through to send current status. 
    break; 
 
  default: 
    SvcDebugOut(" [DEPOT SERVICE] Unrecognized opcode %ld\n", 
             status); 
  } 
  
  // Send current status. 
  if (!SetServiceStatus (DepotServiceHandle,  &DepotServiceStatus)) 
    { 
      status = GetLastError(); 
      SvcDebugOut(" [DEPOT SERVICE] SetServiceStatus error %ld\n", 
		  status); 
    } 
}

void WINAPI ServiceMain() {
  
  DWORD status;
  DWORD specificError;
  
  SvcDebugOut(" [DEPOT SERVICE] ServiceMain called \n",NULL); 
  
  DepotServiceStatus.dwServiceType      = SERVICE_WIN32;
  DepotServiceStatus.dwCurrentState     = SERVICE_RUNNING;
  DepotServiceStatus.dwControlsAccepted = SERVICE_ACCEPT_STOP;
  
  DepotServiceHandle = RegisterServiceCtrlHandler("Depot Server", depotServiceCtrlHandler);
  
  if(DepotServiceHandle == (SERVICE_STATUS_HANDLE)0) {
    SvcDebugOut(" [DEPOT SERVICE] RegisterServiceCtrlHandler failed %d\n", GetLastError());
    return;
  }      
    
  SvcDebugOut(" [DEPOT SERVICE] Registered \n",NULL); 
  SetServiceStatus(DepotServiceHandle,&DepotServiceStatus);
  
  depotMainFunction();
  
  SvcDebugOut(" [DEPOT SERVICE] ServiceMain exiting... \n",NULL); 
}

int InitServiceTable() {
  int startedAs = STARTED_AS_SERVICE;  
  
  SERVICE_TABLE_ENTRY serviceTable[] = {{"Depot Server", ServiceMain}, {0,0}};
  SvcDebugOut(" [DEPOT SERVICE] depot server starting...\n",NULL);
  
  if(!StartServiceCtrlDispatcher(serviceTable)) {
    DWORD failure = GetLastError();
    SvcDebugOut(" [DEPOT SERVICE] Svc dispatcher failed...%ld\n",failure);
    switch(failure) {      
    case ERROR_FAILED_SERVICE_CONTROLLER_CONNECT:
      SvcDebugOut("Failed to connect to svc controller, running as console\n",NULL);
      // this means that we are running in console mode, all is OK.
      startedAs = STARTED_AS_CONSOLE;
      break;
    case ERROR_INVALID_DATA:
      SvcDebugOut("Fucked up data structure\n",NULL);
      break;
    case ERROR_SERVICE_ALREADY_RUNNING: 
      SvcDebugOut("Service already running!\n",NULL);
      break;
    default:
      SvcDebugOut("Unrecognised! %ld\n",failure);            
    }
  }
  return startedAs;
}
