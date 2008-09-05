#define IN_STG_CODE 0
#include "Rts.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
extern StgClosure DepotziWin32ziService_zdfserviceMain_closure;
void serviceMain(HsInt32 a1, HsPtr a2)
{
SchedulerStatus rc;
HaskellObj ret;
rts_lock();
rc=rts_evalIO(rts_apply((HaskellObj)runIO_closure,rts_apply(rts_apply(&DepotziWin32ziService_zdfserviceMain_closure,rts_mkInt32(a1)),rts_mkPtr(a2))) ,&ret);
rts_checkSchedStatus("serviceMain",rc);
rts_unlock();
}
#ifdef __cplusplus
}
#endif

