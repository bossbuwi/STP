     H DEBUG
     H COPYRIGHT('(c) Misys International Banking Systems Ltd. 2012')
      *****************************************************************
/*STD *  RPGBASEMOD                                                   *
      *****************************************************************
      *                                                               *
      *  Midas - Utilities Module                                     *
      *                                                               *
      *  UT000099 - Send MT Data to an MQ Series Queue                *
      *                                                               *
      *  Function: This program binds the modules that send MT data   *
      *            to an MQ Series Queue from an AS400                *
      *                                                               *
      *  (c) Misys International Banking Systems Ltd. 2012            *
      *                                                               *
      *  Last Amend No. CSW212  *CREATE    Date 03May12               *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *  CSW212 - SWIFT 2012 Changes                                  *
      *                                                               *
      *****************************************************************

      ** +--------------------------------------+
      ** ¦ F-specs                              ¦
      ** ¦ =======                              ¦
      ** +--------------------------------------+

     FUTSNDMTDPDIF   E             DISK

      *****************************************************************
      ** +--------------------------------------+
      ** ¦ Automatically included D-specs       ¦
      ** ¦ ==============================       ¦
      ** +--------------------------------------+
      **
      **--------------------------------------------------------------------------------------------
      ** The following /COPY line includes (among others) the LDA layout
      ** and the copyright array definition:
     D/COPY ZACPYSRC,STD_D_SPEC
      **--------------------------------------------------------------------------------------------

      **--------------------------------------------------------------------------------------------
      ** The following /COPY includes the MM standard declares:
     D/COPY ZACPYSRC,STDDECLARE
      **--------------------------------------------------------------------------------------------

      **--------------------------------------------------------------------------------------------
      ** The following /COPY line includes all the defined fields in the
      ** Program Status Data Structure.  They have meaningful
      ** names, prefixed by 'PS'.
     D/COPY ZACPYSRC,PSDS
      **--------------------------------------------------------------------------------------------

      **--------------------------------------------------------------------------------------------
      ** The following /COPY line includes all the definitions of the error
      ** arrays, including the named constant giving the size of
      ** the arrays.  Note that it uses #MsgID and *MsgData, and so requires
      ** STD_D_SPEC.
     D/COPY ZACPYSRC,ERR_ARRAYS
      **--------------------------------------------------------------------------------------------

      ** +--------------------------------------+
      ** ¦ End of automatically included D-specs¦
      ** ¦ =====================================¦
      ** +--------------------------------------+

      *****************************************************************
      /EJECT
      *****************************************************************

      ** +--------------------------------------+
      ** ¦ Manually included D-specs            ¦
      ** ¦ =========================            ¦
      ** +--------------------------------------+

      ** +--------------------------------------+
      ** ¦ Named constants                      ¦
      ** ¦ ===============                      ¦
      ** +--------------------------------------+


      ** +--------------------------------------+
      ** ¦ Arrays and Data Structures           ¦
      ** ¦ ==========================           ¦
      ** +--------------------------------------+

      ** +--------------------------------------+
      ** ¦ Declared variables                   ¦
      ** ¦ ==================                   ¦
      ** +--------------------------------------+

      ** The transaction queue to send to

     D MQQueue         S             20A
     D PMQQueue        S             20A
     D MQManager       S             20A

      ** String to send to MQSeries queue

     D MQString        S          32000A
     D MQStrPls        S          32000A
     D MQStr           S            256A
     D RetCode         S              7A
     D ErrorCode       S             30A
     D RequestCde      S             10A
     D DQString        S          12000A

      ** Next Line indicator

     D                 DS
     D  W@CRLF                 1      2
     D  W@CR                   1      1    INZ(x'0d')
     D  W@LF                   2      2    INZ(x'25')

      ** +--------------------------------------+
      ** ¦ End of D-specs                       ¦
      ** ¦ ==============                       ¦
      ** +--------------------------------------+
      /EJECT

      ** +--- Start of Main processing -----------------------------------+
      ** ¦                                                                ¦
      ** ¦ Initial processing is performed automatically: the *inzsr is   ¦
      ** ¦ executed at program activation.                                ¦
      ** ¦                                                                ¦
      ** +----------------------------------------------------------------+

      ** Read the file until the end

     C                   EVAL      MQStrPls = *BLANKS
     C                   EVAL      MQString = *BLANKS
     C                   EVAL      MQStr  = *BLANKS
     C                   EVAL      p@qmnager = MQManager
     C                   EVAL      MQQueue = PMQQueue
     C**                 EVAL      p@qmnager = 'MM4MA'
     C**                 EVAL      MQQueue = 'MIDAS.DESTINATION.Q'

     C                   DOU       *IN99 = *ON

     C                   READ      UTSNDMTDPD                             99

     C                   IF        *IN99 = *ON
     C                   LEAVE
     C                   ENDIF

     C                   IF        MQString = *BLANKS
     C                   EVAL      MQString = %TRIMR(MTAGDTA) + W@CRLF
     C                   ELSE
     C                   eval      MQStr = %TRIMR(MTAGDTA)
     C                   eval      MQStr = %TRIMR(MQStr) + W@CRLF
     C                   eval      MQString = %TRIMR(MQString) + %TRIMR(MQStr)
     C                   ENDIF

     C                   ENDDO

     C                   IF        QueueTyp = 'MQ'

     C                   EVAL      MQStrPls = %SUBST(MQStrPls:1:HLEN) +
     C                             %TRIMR(MQString)
     C                   EVAL      MQQueue = %TRIMR(MQQueue)

     C                   EVAL      RequestCde = '*OPEN_PUT'
     C                   EXSR      SRQUEUE
     C                   EVAL      RequestCde = '*PUT'
     C                   EXSR      SRQUEUE
     C                   EVAL      RequestCde = '*CLOSE'
     C                   EXSR      SRQUEUE

     C                   ELSEIF    QueueTyp = 'DQ'

     C                   EVAL      DQString = %TRIMR(MQString)
     C*                  EVAL      DQDtanme = 'MS_MMM2MID'
     C                   MOVEL     CorrId        q@skey
     C                   CALL      'QSNDDTAQ'
     C                   PARM                    DQDtanme         10
     C                   PARM      '*LIBL'       DQLibl           10
     C                   PARM      1000          DQLength          5 0
     C                   PARM                    DQString
     C*                  parm      2             q@klength         3 0
     C                   parm      0             q@klength         3 0
     C                   parm                    q@skey            4
     C                   ENDIF

     C                   EVAL      *INLR = *ON

     C                   RETURN

      *****************************************************************
      /EJECT
      *****************************************************************
      *                                                               *
      * SRQUEUE                                                       *
      *                                                               *
      *****************************************************************

     C     SRQUEUE       BEGSR
     C                   CALL      'UTMQMIO'
     C                   PARM      *BLANKS       RetCode
     C                   PARM      *BLANKS       ErrorCode
     C                   PARM                    RequestCde
     C                   parm                    p@qmnager        48
     C                   PARM                    MQQueue
     C                   PARM                    MQStrPls
     C                   PARM                    CorrID
     C                   IF        RetCode <> *BLANKS
     C                   EXSR      *pssr
     C                   ENDIF
     C                   ENDSR
      *****************************************************************
      /EJECT
      *****************************************************************
      *                                                               *
      * *inzsr - Program Initialisation routine                       *
      *                                                               *
      * Called by: Implicitly on program activation                   *
      *                                                               *
      * Calls: None                                                   *
      *                                                               *
      *****************************************************************

     C     *INZSR        BEGSR

     C     *ENTRY        PLIST
     C                   PARM                    QueueTyp          2
     C                   PARM                    CorrID            2
     C                   PARM                    DQDtanme
     C                   PARM                    PMQQueue
     C                   PARM                    MQManager

     C                   IF         QueueTyp = 'MQ'
     C                   EVAL       FileName = 'APMH17PD  '
     C                   ELSEIF     QueueTyp = 'DQ'
     C                   EVAL       FileName = 'APHEADPD  '
     C                   ENDIF

     C                   CALL      'UTGETRCDLN'

     C                   PARM                    ReturnCode       10
     C                   PARM      *ZERO         HLEN              5 0
     C                   PARM                    FileName         10
     C                   PARM      '*LIBL     '  FileLib          10
     C                   ADD       1             HLEN

     C                   ENDSR

      *****************************************************************
      /EJECT
      *****************************************************************
      **--------------------------------------------------------------------------------------------
      ** The following /COPY contains the standard program status
      ** subroutine, including a bound call to the DBERRCTL module.
     C/COPY ZACPYSRC,PSSR_ILE
      **--------------------------------------------------------------------------------------------
      ********************************************************************
      /EJECT
      ********************************************************************
**  CPY@
(c) Misys International Banking Systems Ltd. 2012
