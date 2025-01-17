     H DEBUG
     H COPYRIGHT('(c) Misys International Banking Systems Ltd. 2012')
      *****************************************************************
/*STD *  RPGBASEMOD                                                   *
      *****************************************************************
      *                                                               *
      *  Midas - Meridian Export Module                               *
      *                                                               *
      *  UTMQMIO - MQ Series Input/Output                             *
      *                                                               *
      *  Function:  This program gets messages from                   *
      *             and puts messages to an MQSeries queue.           *
      *                                                               *
      *  (c) Misys International Banking Systems Ltd. 2001            *
      *                                                               *
      *  Last Amend No. CSW212  *CREATE    Data 03May12               *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      *  CSW212 - SWIFT 2012 Changes                                  *
      *                                                               *
      *****************************************************************

      ** +--------------------------------------+
      ** � Automatically included D-specs       �
      ** � ==============================       �
      ** +--------------------------------------+
      **
      ** Standard D-specs
      ** ================
      **
      ** The following /COPY line includes the LDA layout,
      ** the copyright array definition,
      ** and the following named constants:
      **    True       logical = *on (for indcator processing)
      **    False      logical = *off (for indcator processing)
      **    DBErrCtl   10A     = 'DBERRCTL' (the name of the database error
      **                                    handler)
      ** and the following variables:
      **    RunBefore  1A (for the PSSR)

     D/COPY ZACPYSRC,STD_D_SPEC

      ** Program Status Data Structure
      ** =============================
      ** The following /COPY line includes all the defined fields in the
      ** PSDS.  They have meaningful names, prefixed by 'PS'.

     D/COPY ZACPYSRC,PSDS


      **  Declare MQI structures needed
      * MQI Constants
     D/COPY QMQM/QRPGLESRC,CMQR

      ** Object Descriptor
     D MQOD            DS
     D/COPY QMQM/QRPGLESRC,CMQODR

      ** Message Descriptor
     D MQMD            DS
     D/COPY QMQM/QRPGLESRC,CMQMDR

      ** Get message options
     D MQGMO           DS
     D/COPY QMQM/QRPGLESRC,CMQGMOR

      ** Put message options
     D MQPMO           DS
     D/COPY QMQM/QRPGLESRC,CMQPMOR


      ** The include below brings in the return code structure that
      ** is used in calls to OS/400 APIs.
     D/COPY QSYSINC/QRPGLESRC,QUSEC
      * API Error code parameter


     C     *entry        plist
     C                   PARM                    I#RTCD            7
     C                   PARM                    I#ERMS           30
      * INPUT
      * Request
      * Queue Name
      * Message
     C                   parm                    p@request        10
     C                   parm                    p@qmnager        48
     C                   parm                    p@queuenm        48
     C                   PARM                    p@msgbuf      32000
     C                   PARM                    CorrID            2


     C                   select
      *
      ** OPEN a queue
      *
     C     p@request     wheneq    '*OPEN_GET'
     C     p@request     oreq      '*OPEN_PUT'
     C                   exsr      sr_open
      *
      ** GET a message
      *
     C     p@request     wheneq    '*GET'
     C                   exsr      sr_get
      *
      ** PUT a message
      *
     C     p@request     wheneq    '*PUT'
     C                   exsr      sr_put
      *
      ** CLOSE a queue
      *
     C     p@request     wheneq    '*CLOSE'
     C                   exsr      sr_close
      *
     C                   endsl
      *
     C                   return
      /SPACE 5
      **********************************************************************
      * sr_put         : Put outgoing messages                             *
      * ------                                                             *
      **********************************************************************
     C     sr_put        begsr

      ** Get message length

     C     ' '           checkr    p@msgbuf      p@msglen

      ** MDFMT (Format Name) is a subfield of the MQMD data structure,
      ** defined in the include member CMQMDR.
      ** FMSTR is a named constant defined in CMQR; it contains 'MQSTR   '.
     C                   EVAL      MDFMT = FMSTR

      ** Send message
     C                   Z-ADD     MQPUT         CID
     C                   Z-ADD     PMNMID        PMOPT
     C                   MOVEL     CorrID        MDCID
     C                   CALL      'QMQM'
     C                   PARM                    CID               9 0
     C                   PARM                    HCONN             9 0
     C                   PARM                    HIN               9 0
     C                   PARM                    MQMD
     C                   PARM                    MQPMO
     C                   PARM                    p@msglen          9 0
     C                   PARM                    p@msgbuf
     C                   PARM                    CCODE             9 0
     C                   PARM                    REASON            9 0

      ** If send message failed, indicate abnormal end

     C                   IF        REASON <> RCNONE
     C                   EVAL      I#ERMS = 'ERROR IN MQ SERIES PUT MSG'
     C                   EXSR      *PSSR
     C                   ENDIF

     C                   endsr
      **********************************************************************
      /SPACE 5
      **********************************************************************
      * sr_get         : Get incoming messages                             *
      * ------                                                             *
      **********************************************************************
     C     sr_get        begsr

      ** Get options: WAIT, CONVERT and ALLOW TRUNCATION.
      ** Note: the last of these options means that a message longer than
      ** the buffer length defined in this module (currently 1500 bytes)
      ** will be read and removed from the queue.  Any data in the message
      ** after the 1500th byte will be lost.
     C                   Z-ADD     GMWT          GMOPT
     C                   ADD       GMCONV        GMOPT
     C                   ADD       GMATM         GMOPT

      ** Set wait interval to 2 seconds
     C                   Z-ADD     2000          GMWI

      ** Perform get operation inside commitment control. Commitment
      ** boundary is after the message management file updates.
     C                   ADD       GMSYP         GMOPT

      ** MsgId and CorrelId are selectors cleared to ensure messages
      ** are processed in arrival/priority sequence
     C                   MOVEL     MINONE        MDMID
     C                   MOVEL     CINONE        MDCID
     C                   MOVEL     CorrID        MDCID

      ** Clear message buffer
     C                   clear                   p@msgbuf

      ** Get message
     C                   Z-ADD     MQGET         CID
     C                   CALL      'QMQM'
     C                   PARM                    CID               9 0
     C                   PARM                    HCONN             9 0
     C                   PARM                    HIN               9 0
     C                   PARM                    MQMD
     C                   PARM                    MQGMO
     C                   PARM      32000         p@msglen          9 0
     C                   PARM                    p@msgbuf
     C                   PARM                    MESLEN            9 0
     C                   PARM                    CCODE             9 0
     C                   PARM                    REASON            9 0
      *
      ** If receive message failed, indicate abnormal end
      *
     C     REASON        ifne      RCNONE
     C     REASON        andne     RC2033
     C                   EVAL      I#ERMS = 'ERROR IN MQ SERIES GET MSG'
     C                   EXSR      *PSSR
     C                   ENDIF

     C                   endsr
      **********************************************************************
      /SPACE 5
      **********************************************************************
      * sr_open        : Open MQ series queue                              *
      * -------                                                            *
      **********************************************************************
     C     sr_open       begsr

      ** Use default connection handle, and implicit connection
     C                   Z-ADD     HCDEFH        HCONN

      ** Queue name
     C                   MOVEL     p@queuenm     ODON             48

      ** Queue Manager
     C                   Eval      ODMN = p@qmnager
      *
      ** Connect to Queue Manager
     C                   Eval      CID = MQCONN
      *
     C                   Call      'QMQM'
     C                   Parm                    CID               9 0
     C                   Parm                    ODMN
     C                   Parm                    HCONN             9 0
     C                   Parm                    OCODE             9 0
     C                   Parm                    REASON            9 0
      *
      ** Report reason and stop if it failed
     C                   If        (REASON<>RCNONE)
     C                   Eval      I#ERMS = 'ERROR IN QUEUE MANAGER CONNECT'
     C                   Exsr      *PSSR
     C                   Endif
      *
      ** Open queue for INPUT

     C     p@request     ifeq      '*OPEN_GET'

      ** Open options: INPUT and FAIL_IF_QUIESCING
     C     OOINPQ        ADD       OOFIQ         OPTS

      ** Open queue for OUTPUT

     C                   else

      ** Open options: OUTPUT and FAIL_IF_QUIESCING
     C     OOOUT         ADD       OOFIQ         OPTS
     C                   endif

      ** Open queue
     C                   Z-ADD     MQOPEN        CID
     C                   CALL      'QMQM'
     C                   PARM                    CID               9 0
     C                   PARM                    HCONN             9 0
     C                   PARM                    MQOD
     C                   PARM                    OPTS              9 0
     C                   PARM                    HIN               9 0
     C                   PARM                    OCODE             9 0
     C                   PARM                    REASON            9 0

      ** If open queue failed, indicate abnormal end

     C     REASON        IFNE      RCNONE
     C                   EVAL      I#ERMS = 'ERROR IN MQ SERIES OPEN Q'
     C                   EXSR      *PSSR
     C                   ENDIF

     C                   endsr
      **********************************************************************
      /SPACE 5
      **********************************************************************
      * sr_close       : Close MQ series queue                             *
      * --------                                                           *
      **********************************************************************
     C     sr_close      begsr

      ** Close options: NONE
     C                   Z-ADD     CONONE        OPTS

      ** Close queue
     C                   Z-ADD     MQCLOS        CID
     C                   CALL      'QMQM'
     C                   PARM                    CID               9 0
     C                   PARM                    HCONN             9 0
     C                   PARM                    HIN               9 0
     C                   PARM                    OPTS              9 0
     C                   PARM                    CCODE             9 0
     C                   PARM                    REASON            9 0

      ** If close queue failed, indicate abnormal end

     C     REASON        IFNE      RCNONE
     C                   EVAL      I#ERMS = 'ERROR IN MQ SERIES CLOSE Q'
     C                   EXSR      *PSSR
     C                   ENDIF

      ** Disconnect
     C                   Eval      CID = MQDISC
      *
     C                   Call      'QMQM'
     C                   Parm                    CID               9 0
     C                   Parm                    HCONN             9 0
     C                   Parm                    OCODE             9 0
     C                   Parm                    REASON            9 0
      *
      ** Report reason and stop if it failed
     C                   If        (REASON<>RCNONE)
     C                   Eval      I#ERMS = 'ERROR IN MQ SERIES CLOSE CONN'
     C                   Exsr      *PSSR
     C                   Endif
      *
     C                   ENDSR
      *******************************************************************
      /SPACE 5
      ********************************************************************
      * * P S S R  --- A B N O R M A L   E R R O R   C O N D I T I O N S
      /COPY MXCPYSRC,MXPSSR
      *****************************************************************
**  CPY@
(c) Misys International Banking Systems Ltd. 2012
