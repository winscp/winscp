The following is a copy of post at
https://forums.embarcadero.com/message.jspa?messageID=175480#175480

1) copy libs\mfc\source to $(BDS)\source\mfc

2) copy libs\mfc\include to $(BDS)\include\mfc

3) make following changes to files in $(BDS)\source\mfc

  - change line 64 of borland.mak to read:
    UNICODE=1

  - change line 140 of borland.mak to read:
    MODEL=U

  - add the following line to borland.mak at line 141
    TARGDEFS=/D_UNICODE /DUNICODE /D_AFX_NO_OLEDB_SUPPORT /D_AFX_NO_OCC_SUPPORT

  - replace lines 305-310 of borland.mak with:
    WINDOWS=winhand.obj

  - replace lines 307-312 of borland.mak with:
    DIALOG=

  - replace lines 309-311 of borland.mak with:
    WINMISC=afxcrit.obj winstr.obj winutil.obj auxdata.obj wingdi.obj

  - replace lines 311-316 of borland.mak with:
    DOCVIEW=

  - comment out line 315 of borland.mak to read:
    #INTERNET=$(INTERNET) isapimix.obj

  - replace lines 318-322 of borland.mak with:
    APPLICATION=appterm.obj appui1.obj appinit.obj apphelp.obj thrdcore.obj

  - comment out lines 321-322 of borland.mak to read:
    #APPLICATION=$(APPLICATION) app3ds.obj \
    #	nolib.obj appmodul.obj dllmodul.obj oleexp.obj dumpstak.obj

  - comment out lines 326-328 of borland.mak to read:
    #DB=\
    #	dbcore.obj dbrfx.obj dbview.obj dbflt.obj \
    #	dblong.obj dbvar.obj

  - replace lines 357-367 of borland.mak with:
    OLECTL=

  - change line 516 of borland.mak to read:
    $(LIBDIR)\$(GOAL).lib: $(D)\$(OBJS)

  - change line 517 of borland.mak to read:
    # @-if exist $@ erase $@

  - change line 775 of arccore.cpp to read:
    AfxThrowArchiveException(CArchiveException::generic);

  - change line 781 of arccore.cpp to read:
    AfxThrowArchiveException(CArchiveException::generic);

  - change code of following methods in inet.cpp
    CInternetSession::SetCookie
    CInternetSession::GetCookie (both overloads)
    to read
    AfxThrowInternetException(GetLastError());
    return false;

  - change code of following method in inet.cpp
    CInternetSession::GetCookieLength
    to read
    AfxThrowInternetException(GetLastError());
    return 0;

  - change line 281 of olecli1.cpp to read:
    LPCTSTR pstrIndex = _tcsinc(pstrSource);

  - add the following line to strex.cpp at line 431
    #define _tclen(__a)         (1)

  - comment out line 37 in afxinl1.cpp to read:
    //#include "afxdlgs.inl"

  - comment out line 44 in afxinl1.cpp to read:
    //#include "afxext.inl"

  - comment out lines 64-68 in afxstate.cpp to read:
    /*if (m_pToolTip != NULL)
    {
        m_pToolTip->DestroyWindow();
        delete m_pToolTip;
    }*/

  - comment out lines 144-148 in afxstate.cpp to read:
    /*if (m_pTypeLibCacheMap != NULL)
    {
        m_pTypeLibCacheMap->RemoveAll(&m_typeLibCache);
        delete m_pTypeLibCacheMap;
    }

  - comment out lines 260-277 in afxstate.cpp to read:
    /*void CTypeLibCache::Unlock()
    {
        ASSERT(m_cRef > 0);
        ....
    }*/

  - comment out lines 37-41 in appui1.cpp to read:
    /*// check if notify hook installed
    ASSERT_KINDOF(CFrameWnd, pMainWnd);
    CFrameWnd* pFrameWnd = (CFrameWnd*)pMainWnd;
    if (pFrameWnd->m_pNotifyHook != NULL)
        pFrameWnd->m_pNotifyHook->OnEnableModeless(bEnable);*/

  - comment out lines 157-164 in appui1.cpp to read:
    /*if (hWnd == NULL)
    {
        CFrameWnd* pFrame = CCmdTarget::GetRoutingFrame_();
        if (pFrame != NULL)
            hWnd = pFrame->GetSafeHwnd();
        else
            hWnd = AfxGetMainWnd()->GetSafeHwnd();
     }*/

  - comment out lines 213-216 in appui1.cpp to read:
    /* CFrameWnd* pFrame = AfxGetThreadState()->m_pRoutingFrame;
    if (pFrame != NULL)
        ASSERT_VALID(pFrame);
    return pFrame;*/
    return NULL;

4) compile the mfc library using the command:
MAKE -fborland.mak NO_WARNINGS=1

By default the library will be at $(BDS)\lib\UafxcW.lib

See also:
https://forums.embarcadero.com/thread.jspa?messageID=175481&tstart=0
