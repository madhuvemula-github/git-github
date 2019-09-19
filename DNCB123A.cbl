      **************************************************
      *            IDENTIFICATION DIVISION             *
      **************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DNCB123A.
       AUTHOR.        Jumar Solutions.
      *   Written on 09/01/2019 10:35:37 using XCG v3.2.50
      *   Name: CREATE_DENORMALIZED_EMPL_TABLE
      * 
      * 
      **************************************************
      *            ENVIRONMENT DIVISION                *
      **************************************************
       ENVIRONMENT DIVISION.
      * 
      **************************************************
      *            DATA DIVISION                       *
      **************************************************
       DATA DIVISION.
      * 
       WORKING-STORAGE SECTION.
       01  WS-DB-PV-ERROR              PIC  X VALUE 'N'.
           88 DB-PV-STATUS-OK           VALUE 'N'.
           88 DB-PV-STATUS-NOT-OK       VALUE 'Y'.
       01  WS001-CONTROL-VARIABLES.
           05 WS-FLOW-CONTROL.
              10 WS-FLOW-TRNCD         PIC  X(8).
           05 WS-ESCAPE-MECHANISM.
              10 WS-ESC-CURRENT-LEVEL  PIC  99 COMP-3.
              10 WS-ESC-QUIT-LEVEL     PIC  99 COMP-3.
              10 WS-ESCAPE-LOOP-YN     PIC  X.
                 88 ESCAPE-LOOP         VALUE 'Y'.
                 88 STAY-IN-LOOP        VALUE 'N'.
           05 WS-PGM-JSCDBERR          PIC  X(8) VALUE 'JSCDBERR'.

      *
       01  WS-DNCB123A-LOCALS.

      *
           05 WS002-TEMP.
              10 WS-IEF-SUPPLIED.
                 15 WS-COUNT                        PIC S9(9).
      *
           05 WS003-LOCAL-CURRENT.
              10 WS-DMS-SCREEN-DATE.
                 15 WS-CONCAT-DATE                  PIC S9(8).
      *
      *
       01  WS004-DISTRIBUTOR.
           05 WS004-HAS-DATA   PIC  X VALUE 'N'.
           05 WS004-IS-LOCKED  PIC  X VALUE 'N'.
           05 WS-YDMSPRTY.
              10 WS-ID         PIC S9(15).

      *
      *
       01  WS005-EMPLOYEE.
           05 WS005-HAS-DATA   PIC  X VALUE 'N'.
           05 WS005-IS-LOCKED  PIC  X VALUE 'N'.
           05 WS-YDMSPRTY.
              10 WS-ID         PIC S9(15).

      *
      *
       01  WS006-EAV.
           05 WS006-HAS-DATA            PIC  X VALUE 'N'.
           05 WS006-IS-LOCKED           PIC  X VALUE 'N'.
           05 WS-YDMSEMDN.
              10 WS-DISTRIBUTOR-ID      PIC  X(8).
              10 WS-DISTRIBUTOR-PARTY   PIC S9(15).
              10 WS-EMPLOYEE-PARTY-ID   PIC S9(15).
              10 WS-LAST-NAME           PIC  X(20).
              10 WS-FIRST-NAME          PIC  X(15).
              10 WS-MIDDLE-INITIAL      PIC  X.
              10 WS-SOCIAL-SECURITY-NB  PIC  X(9).
              10 WS-BIRTH-DTE           PIC S9(8).
              10 WS-EFF-DTE             PIC S9(8).
              10 WS-EXP-DTE             PIC S9(8).
              10 WS-LAST-UPD-TMSP       PIC  X(20).
              10 WS-LAST-UPD-UID        PIC  X(8).
              10 WS-AS-OF-DTE           PIC S9(8).

      *
      *
       01  WS007-EAV.
           05 WS007-HAS-DATA   PIC  X VALUE 'N'.
           05 WS007-IS-LOCKED  PIC  X VALUE 'N'.
           05 WS-YDMSDOUT.
              10 WS-ID         PIC S9(8).

      *  Foreign Key store for EAV for Entity DISTRIBUTOR_OUTLET
           05 WS007-FK-STORE.
              10 WS007-FKSET-DEFINES.
                 15 WS007-FKSTATUS-DEFINES PIC 9.
                      88 WS007-FK-UNKNOWN   VALUE 0.
                      88 WS007-FK-NULL      VALUE 1.
                      88 WS007-FK-POPULATED VALUE 2.
                 15 WS007-FK-DIST-OUTL-TYP  PIC X(8).
              10 WS007-FKSET-SPECIFIES.
                 15 WS007-FKSTATUS-SPECIFIES PIC 9.
                      88 WS007-FK-UNKNOWN   VALUE 0.
                      88 WS007-FK-NULL      VALUE 1.
                      88 WS007-FK-POPULATED VALUE 2.
                 15 WS007-FK-PARTY-ID  PIC S9(15) COMP-3.
      *
      *
       01  WS008-EAV.
           05 WS008-HAS-DATA   PIC  X VALUE 'N'.
           05 WS008-IS-LOCKED  PIC  X VALUE 'N'.
           05 WS-YDMSDOEM.
              10 WS-EFF-DTE    PIC S9(8).
              10 WS-EXP-DTE    PIC S9(8).

      *  Foreign Key store for EAV for Entity 
      *    DISTRIBUTOR_OUTLET_EMPLOYEE 
           05 WS008-FK-STORE.
              10 WS008-FKSET-IS-EMPLOYED-AS.
                 15 WS008-FKSTATUS-IS-EMPLOYED-AS PIC 9.
                      88 WS008-FK-UNKNOWN   VALUE 0.
                      88 WS008-FK-NULL      VALUE 1.
                      88 WS008-FK-POPULATED VALUE 2.
                 15 WS008-FK-EMP-PARTY-ID  PIC S9(15) COMP-3.
              10 WS008-FKSET-EMPLOYES.
                 15 WS008-FKSTATUS-EMPLOYES PIC 9.
                      88 WS008-FK-UNKNOWN   VALUE 0.
                      88 WS008-FK-NULL      VALUE 1.
                      88 WS008-FK-POPULATED VALUE 2.
                 15 WS008-FK-DIST-PARTY-ID  PIC S9(15) COMP-3.
      *
      *
       01  WS009-EAV.
           05 WS009-HAS-DATA      PIC  X VALUE 'N'.
           05 WS009-IS-LOCKED     PIC  X VALUE 'N'.
           05 WS-YDMSDEMP.
              10 WS-LAST-UPD-TSP  PIC  X(20).
              10 WS-LAST-UPD-UID  PIC  X(8).

      *  Foreign Key store for EAV for Entity DISTRIBUTOR_EMPLOYEE
           05 WS009-FK-STORE.
              10 WS009-FKSET-SPECIFIES.
                 15 WS009-FKSTATUS-SPECIFIES PIC 9.
                      88 WS009-FK-UNKNOWN   VALUE 0.
                      88 WS009-FK-NULL      VALUE 1.
                      88 WS009-FK-POPULATED VALUE 2.
                 15 WS009-FK-PARTY-ID  PIC S9(15) COMP-3.
      *
      *
       01  WS010-EAV.
           05 WS010-HAS-DATA        PIC  X VALUE 'N'.
           05 WS010-IS-LOCKED       PIC  X VALUE 'N'.
           05 WS-YDMSHDEM.
              10 WS-EFF-DTE         PIC S9(8).
              10 WS-EXP-DTE         PIC S9(8).
              10 WS-SOC-SEC-NUM     PIC  X(9).
              10 WS-FIRST-NME       PIC  X(15).
              10 WS-MIDDLE-INITIAL  PIC  X.
              10 WS-LAST-NME        PIC  X(20).
              10 WS-BRTH-DTE        PIC S9(8).
              10 WS-LAST-UPD-TSP    PIC  X(20).
              10 WS-LAST-UPD-UID    PIC  X(8).

      *  Foreign Key store for EAV for Entity 
      *    DISTRIBUTOR_EMPLOYEE_HISTORY 
           05 WS010-FK-STORE.
              10 WS010-FKSET-HAS.
                 15 WS010-FKSTATUS-HAS PIC 9.
                      88 WS010-FK-UNKNOWN   VALUE 0.
                      88 WS010-FK-NULL      VALUE 1.
                      88 WS010-FK-POPULATED VALUE 2.
                 15 WS010-FK-EMPL-PARTY-ID  PIC S9(15) COMP-3.
      *******************************************
      *   Beginning of STATUS codes
       01  WS-BLANK-STATUS       VALUE SPACES.
           05 WS-STATUS-DSCRN   PIC  X.
           05 WS-STATUS-ENDEX   PIC  X.
           05 WS-STATUS-MSGTYP  PIC  X.
           05 WS-STATUS-NAME    PIC  X(32).
           05 WS-STATUS-MSGTXT  PIC  X(80).
           05 WS-STATUS-TERM    PIC  X.

      * 
       01  WS-ES-INVALID-COMMAND.
           05 WS-STATUS-DSCRN     PIC  X     VALUE 'Y'.
           05 WS-STATUS-ENDEX     PIC  X     VALUE 'Y'.
           05 WS-STATUS-MSGTYP    PIC  X     VALUE 'E'.
           05 WS-STATUS-NAME      PIC  X(32) VALUE 'INVALID_COMMAND'.
           05 WS-STATUS-MSGTXT    PIC  X(80) VALUE 'INVALID_COMMAND'.
           05 WS-STATUS-TERM      PIC  X     VALUE 'R'.

       01  WS-ES-PERMITTED-VALUES.
           05 WS-STATUS-DSCRN      PIC  X     VALUE 'Y'.
           05 WS-STATUS-ENDEX      PIC  X     VALUE 'Y'.
           05 WS-STATUS-MSGTYP     PIC  X     VALUE 'E'.
           05 WS-STATUS-NAME       PIC  X(32) 
                 VALUE 'ES_PERMITTED_VALUES'.
           05 WS-STATUS-MSGTXT     PIC  X(80) VALUE 'VALUES ARE'.
           05 WS-STATUS-TERM       PIC  X     VALUE 'M'.

      *   End of STATUS codes
      *******************************************
       01  WS011-ERROR-REPORTED.
           05 WS011-ERROR-ENCOUNTERED           PIC  X.
           05 WS011-ERROR-FUNC-NAME             PIC  X(8).
           05 WS011-ERROR-FUNC-ERRMSG           PIC  9(4).
       01  WS012-FUNCTION-WORKAREA.
           05 WS012-SCREEN-NUMBER-IN-OUT.
              10 WS012-SCREEN-NUMBER            PIC  X(80).
              10 WS012-COBOL-NUMERIC            PIC S9(18).
              10 WS012-EDIT-PATTERN-NUM         PIC  X(80).
              10 WS012-SCREEN-COBOL-IND-NUM     PIC  X.
              10 WS012-FIELD-FILL-CHAR          PIC  X.
              10 WS012-DECIMAL-CHAR             PIC  X.
              10 WS012-THOUSAND-CHAR            PIC  X.
              10 WS012-BLANK-WHEN-ZERO          PIC  X.
              10 WS012-IS-PROTECTED             PIC  X.
              10 WS012-JUSTIFY                  PIC  X.
           05 WS012-SCREEN-TIME-IN-OUT.
              10 WS012-SCREEN-TIME              PIC  X(8).
              10 WS012-COBOL-TIME               PIC  9(6).
              10 WS012-EDIT-PATTERN-TIME        PIC  X(8).
              10 WS012-SCREEN-COBOL-IND-TIME    PIC  X.
              10 WS012-TIME-FILL-CHAR           PIC  X.
              10 WS012-BLANK-WHEN-ZERO          PIC  X.
              10 WS012-IS-PROTECTED             PIC  X.
           05 WS012-SCREEN-DATE-IN-OUT.
              10 WS012-SCREEN-DATE              PIC  X(10).
              10 WS012-COBOL-DATE               PIC S9(8).
              10 WS012-EDIT-PATTERN-DATE        PIC  X(10).
              10 WS012-SCREEN-COBOL-IND-DATE    PIC  X.
              10 WS012-DATE-FILL-CHAR           PIC  X.
              10 WS012-BLANK-WHEN-ZERO          PIC  X.
              10 WS012-IS-PROTECTED             PIC  X.
           05 WS012-SCREEN-TSTAMP-IN-OUT.
              10 WS012-SCREEN-TSTAMP            PIC  X(26).
              10 WS012-COBOL-TSTAMP             PIC  X(20).
              10 WS012-EDIT-PATTERN-TSTAMP      PIC  X(255).
              10 WS012-SCREEN-COBOL-IND-TSTAMP  PIC  X.
              10 WS012-BLANK-WHEN-ZERO          PIC  X.
              10 WS012-IS-PROTECTED             PIC  X.
           05 WS012-SCREEN-TEXT-IN-OUT.
              10 WS012-SCREEN-TEXT              PIC  X(255).
              10 WS012-COBOL-TEXT               PIC  X(255).
              10 WS012-EDIT-PATTERN-TEXT        PIC  X(255).
              10 WS012-SCREEN-COBOL-IND-TEXT    PIC  X.
              10 WS012-TXT-FILL-CHAR            PIC  X.
              10 WS012-IS-PROTECTED             PIC  X.
           05 WS012-COUNT                       PIC S9(3).
           05 WS-PGM-JSSCRNTX                   PIC  X(8) 
                 VALUE 'JSSCRNTX'.
           05 WS-PGM-JSSCRNDT                   PIC  X(8) 
                 VALUE 'JSSCRNDT'.
           05 WS-PGM-JSSCRNTS                   PIC  X(8) 
                 VALUE 'JSSCRNTS'.
           05 WS-PGM-JSSCRNTM                   PIC  X(8) 
                 VALUE 'JSSCRNTM'.
           05 WS-PGM-JSSCRNNO                   PIC  X(8) 
                 VALUE 'JSSCRNNO'.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   DBMS RESPONSE AREA                                        *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       COPY JSDBMSRW.
       01  WS-DSN-ERROR-MESSAGE.
           05 WS-DSN-ERROR-LEN   PIC S9(4) COMP VALUE 720.
           05 WS-DSN-ERROR-TEXT  PIC  X(72)  OCCURS 10 TIMES.

       77  WS-DSN-ERROR-TEXT-LEN PIC S9(9) COMP VALUE +72.
       01  WS013-FUNCTION-WORKAREA.
           05 WS013-COBOL-TIMESTAMP           PIC  X(21).
       01  WS030-FUNCTION-WORKAREA.
           05 WS-PGM-JSFORMDT                 PIC  X(08) 
                 VALUE 'JSFORMDT'.
           05 WS030-DT-T-TS-INPUT.
              10 WS030-DT-T-TS-INPUT-VAL      PIC  X(26).
              10 WS030-COBOL-DB2-IND          PIC  X.
           05 WS030-DT-T-TS-OUTPUT.
              10 WS030-DT-T-TS-OUTPUT-VAL     PIC  X(26).
       01  WS031-FUNCTION-WORKAREA.
           05 WS-PGM-JSTEXTNO                 PIC  X(08) 
                 VALUE 'JSTEXTNO'.
           05 WS031-TEXTNUM-INPUT.
              10 WS031-TEXTNUM-INPUT-NUMBER   PIC S9(15).
           05 WS031-TEXTNUM-OUTPUT.
              10 WS031-TEXTNUM-OUTPUT-STRING  PIC  X(15).

      *
       01  WS033-TEMP-VARIABLES.
           05 WS034-TEMPTXM1     PIC  X(15).
           05 WS035-TEMPDAT1     PIC S9(8).

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   SQL COMMUNICATION AREA                                    *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE YDMSEMDN END-EXEC.
      *
      *
       77  WS015-EFFECTIVE-DATE  PIC X(10).

       77  WS016-EXPIRATION-DATE  PIC X(10).

       77  WS017-SOCIAL-SECURITY-NUMBER  PIC X(9).

       77  WS018-SOCIAL-SECURITY-NUMBE-NL  PIC S9(4) COMP.

       77  WS019-FIRST-NAME  PIC X(15).

       77  WS020-MIDDLE-INITIAL  PIC X(1).

       77  WS021-MIDDLE-INITIAL-NL  PIC S9(4) COMP.

       77  WS022-LAST-NAME  PIC X(20).

       77  WS023-BIRTH-DATE  PIC X(10).

       77  WS024-LAST-UPDATE-TIMESTAMP  PIC X(26).

       77  WS025-LAST-UPDATE-USERID  PIC X(8).

       77  BV0001-FK-EMPL-PART  PIC S9(15)V COMP-3.

       77  WS026-IDENTIFIER  PIC S9(9) COMP.

       77  BV0002-FK-PARTY-ID  PIC S9(15)V COMP-3.

       77  BV0003-ID  PIC S9(15)V COMP-3.

       77  BV0004-ID  PIC S9(15)V COMP-3.
       01  WS027-NOT-FOUND-FLAG       PIC  9 VALUE ZERO.
           88 RELATIONSHIP-NOT-FOUND   VALUE 1.

       77  WS028-ROW-ID            PIC S9(18) COMP VALUE 0.

       77  WS029-ROW-ID            PIC S9(18) COMP VALUE 0.

       77  WS032-MIDDLE-INITIAL-NL  PIC S9(4) COMP.
      *
      * * * * * * * * * * * * * * * * * * * * * *
      *           CURSOR DECLARATIONS           *
      * * * * * * * * * * * * * * * * * * * * * *
           EXEC SQL DECLARE CRSR014_YDMSHDEM CURSOR FOR
           SELECT  DISTINCT
              RID("YDMSDOUT02"),
              RID("YDMSHDEM01"),
                   CHAR(YDMSHDEM01."EFF_DTE", ISO),
                   CHAR(YDMSHDEM01."EXP_DTE", ISO),
                   YDMSHDEM01."SOC_SEC_NUM",
                   YDMSHDEM01."FIRST_NME",
                   YDMSHDEM01."MIDDLE_INITIAL",
                   YDMSHDEM01."LAST_NME",
                   CHAR(YDMSHDEM01."BRTH_DTE", ISO),
                   YDMSHDEM01."LAST_UPD_TSP",
                   YDMSHDEM01."LAST_UPD_UID",
                   YDMSHDEM01."FK_EMPL_PARTY_ID",
                   YDMSDOUT02."ID",
                   YDMSDOUT02."FK_PARTY_ID",
                   YDMSDOUT02."FK_PARTY_ID",
                   YDMSHDEM01."FK_EMPL_PARTY_ID"
           FROM
               "YDMSHDEM"                      YDMSHDEM01,
               "YDMSDOUT"                      YDMSDOUT02,
               "YDMSDOEM"                      YDMSDOEM06
           WHERE
           (
                YDMSDOEM06."FK_EMP_PARTY_ID" =
                    YDMSHDEM01."FK_EMPL_PARTY_ID" AND
                    YDMSDOEM06."FK_DIST_PARTY_ID" =
                    YDMSDOUT02."FK_PARTY_ID"
           )
             ORDER BY 4 DESC
           END-EXEC
      * 
       01  WS-CRSR014-YDMSHDEM-FLAG  PIC  X VALUE 'N'.

      * 
      * * * * * * * * * * * * * * * * * * * * * *
      *         END CURSOR DECLARATIONS         *
      * * * * * * * * * * * * * * * * * * * * * *
      * 
       LINKAGE SECTION.
       COPY JSFLWCTL.
      * 
      **************************************************
      *            PROCEDURE DIVISION                  *
      **************************************************
       PROCEDURE DIVISION USING LS-FLOW.
       0000-MODULE-ENTRY.
           PERFORM 0010-INITIALIZATION THRU 0010-EXIT
           PERFORM 0500-MAIN-LOGIC THRU 0500-EXIT
           PERFORM 9920-CLOSE-CURSORS THRU 9920-EXIT
           GOBACK.
       0000-MODULE-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       0010-INITIALIZATION.
      *--------------------------------------------------------------*
           MOVE "0010-INITIALIZATION" TO LS-FLOW-CUR-PARA-NAME
           MOVE  "DNCB123A " TO LS-FLOW-CUR-PROG-ID
           MOVE  "CREATE_DENORMALIZED_EMPL_TABLE" TO 
             LS-FLOW-CUR-ACTION-NAME 
           MOVE  "EN" TO LS-FLOW-LANGUAGE-CODE
      * NOTE Initialize all Working-Storage and Linkage variables that 
      *   require it. 
           INITIALIZE WS002-TEMP OF WS-DNCB123A-LOCALS
           INITIALIZE WS003-LOCAL-CURRENT OF WS-DNCB123A-LOCALS
           INITIALIZE WS004-DISTRIBUTOR
           INITIALIZE WS005-EMPLOYEE
           INITIALIZE WS006-EAV
           MOVE ALL ZEROS TO 
             WS-LAST-UPD-TMSP OF WS-YDMSEMDN OF WS006-EAV 
           INITIALIZE WS007-EAV
           INITIALIZE WS008-EAV
           INITIALIZE WS009-EAV
           MOVE ALL ZEROS TO WS-LAST-UPD-TSP OF WS-YDMSDEMP OF WS009-EAV
           INITIALIZE WS010-EAV
           MOVE ALL ZEROS TO WS-LAST-UPD-TSP OF WS-YDMSHDEM OF WS010-EAV
      *  Initialise all Foreign Key Stores
           INITIALIZE WS007-FK-STORE OF WS007-EAV
           INITIALIZE WS008-FK-STORE OF WS008-EAV
           INITIALIZE WS009-FK-STORE OF WS009-EAV
           INITIALIZE WS010-FK-STORE OF WS010-EAV
      * NOTE Initialize ESCAPE control variables.
           MOVE 0 TO WS-ESC-CURRENT-LEVEL
           MOVE 99 TO WS-ESC-QUIT-LEVEL
           SET STAY-IN-LOOP TO TRUE
           .
       0010-EXIT.
           EXIT.
      * 
      *--------------------------------------------------------------*
       0500-MAIN-LOGIC.
      *--------------------------------------------------------------*
           ADD 1 TO WS-ESC-CURRENT-LEVEL
           MOVE 99 TO WS-ESC-QUIT-LEVEL
G0001      MOVE FUNCTION CURRENT-DATE TO WS013-COBOL-TIMESTAMP
G0001      MOVE WS013-COBOL-TIMESTAMP (1:8) TO WS035-TEMPDAT1
G0001      MOVE WS035-TEMPDAT1 TO 
G0001        WS-CONCAT-DATE OF WS-DMS-SCREEN-DATE OF WS003-LOCAL-CURRENT 
G0001        OF WS-DNCB123A-LOCALS 
      * NOTE ************************************************
      *      Retrieve the most current employee information
      *      for the denormalized table.
      *      ************************************************
           PERFORM 1000-MOVE-SQLIN-VARS-0003 THRU 1000-EXIT
           PERFORM 1001-OPEN-CURSOR THRU 1001-EXIT
G0003      SET STAY-IN-LOOP TO TRUE
G0003      PERFORM 1003-READ-EACH THRU 1003-EXIT WITH TEST BEFORE UNTIL 
G0003        NOT DBMS-OK OR WS-ESC-QUIT-LEVEL < WS-ESC-CURRENT-LEVEL OR 
G0003        (WS-ESC-QUIT-LEVEL = WS-ESC-CURRENT-LEVEL AND ESCAPE-LOOP) 
      *  Did the loop finish without an error?
G0003      IF DBMS-END-OF-SET OR DBMS-NOT-FOUND
G0003         SET DBMS-OK TO TRUE
G0003      END-IF
G0003      IF NOT DBMS-OK
G0003         MOVE 0 TO WS-ESC-QUIT-LEVEL
G0003      END-IF
G0003      IF WS-ESC-QUIT-LEVEL < WS-ESC-CURRENT-LEVEL
G0003         GO TO 0500-EXIT
G0003      ELSE
G0003         MOVE 99 TO WS-ESC-QUIT-LEVEL
G0003      END-IF
G0003      SET STAY-IN-LOOP TO TRUE
           .
       0500-EXIT.
           EXIT.
       1000-MOVE-SQLIN-VARS-0003.
      *   Initialise NOT FOUND variable
           MOVE 0 TO WS027-NOT-FOUND-FLAG
      *   START EAV Initialisation
           INITIALIZE WS-YDMSHDEM OF WS010-EAV
      *   Initialise the FK Stores
           INITIALIZE WS010-FK-STORE OF WS010-EAV
           INITIALIZE WS-YDMSDOUT OF WS007-EAV
      *   Initialise the FK Stores
           INITIALIZE WS007-FK-STORE OF WS007-EAV
           INITIALIZE WS-YDMSPRTY OF WS004-DISTRIBUTOR
           INITIALIZE WS-YDMSPRTY OF WS005-EMPLOYEE
      *   END EAV Initialisation
           .
      *--------------------------------------------------------------*
       1000-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       1001-OPEN-CURSOR.
           IF WS-CRSR014-YDMSHDEM-FLAG = "Y"
              EXEC SQL CLOSE CRSR014_YDMSHDEM
              END-EXEC
              MOVE "N" TO WS-CRSR014-YDMSHDEM-FLAG
           END-IF
           EXEC SQL OPEN CRSR014_YDMSHDEM
           END-EXEC
           MOVE "Y" TO WS-CRSR014-YDMSHDEM-FLAG
           PERFORM 9250-SAVE-DBMS-INFO THRU 9250-EXIT
           MOVE "YDMSHDEM" TO WS-DBMS-TABLE-NAME
           MOVE "1001-OPEN-CURSOR" TO WS-DBMS-PARA-NAME
           MOVE "OPEN-CUR" TO WS-DBMS-STMT-TYPE
           IF NOT DBMS-OK
              MOVE "ERROR" TO WS-DBMS-STATUS-MESSAGE
              MOVE SQLCA TO LS-FLOW-DB-SQLCA
              SET LS-FLOW-EXEC-DB-ERROR TO TRUE
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1001-OPEN-CURSOR" TO LS-FLOW-ERROR-PARA-NAME
              PERFORM 9990-ABORT THRU 9990-EXIT
           END-IF
             .
      *--------------------------------------------------------------*
       1001-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       1002-FETCH-CURSOR-DATA.
      *    Fetch the data for cursor CRSR014_YDMSHDEM
           MOVE "N" TO WS010-HAS-DATA OF WS010-EAV
           MOVE "N" TO WS007-HAS-DATA OF WS007-EAV
           MOVE "N" TO WS004-HAS-DATA OF WS004-DISTRIBUTOR
           MOVE "N" TO WS005-HAS-DATA OF WS005-EMPLOYEE
      *   Initialise ROWID variable(s)
           MOVE 0 TO WS028-ROW-ID
           MOVE 0 TO WS029-ROW-ID
           MOVE "N" TO WS010-IS-LOCKED OF WS010-EAV
           MOVE "N" TO WS007-IS-LOCKED OF WS007-EAV
           MOVE "N" TO WS004-IS-LOCKED OF WS004-DISTRIBUTOR
           MOVE "N" TO WS005-IS-LOCKED OF WS005-EMPLOYEE
           EXEC SQL FETCH CRSR014_YDMSHDEM INTO
              :WS029-ROW-ID,
              :WS028-ROW-ID,
           :WS015-EFFECTIVE-DATE ,
           :WS016-EXPIRATION-DATE ,
           :WS017-SOCIAL-SECURITY-NUMBER 
             :WS018-SOCIAL-SECURITY-NUMBE-NL, 
           :WS019-FIRST-NAME,
           :WS020-MIDDLE-INITIAL :WS021-MIDDLE-INITIAL-NL,
           :WS022-LAST-NAME,
           :WS023-BIRTH-DATE ,
           :WS024-LAST-UPDATE-TIMESTAMP ,
           :WS025-LAST-UPDATE-USERID,
           :BV0001-FK-EMPL-PART,
           :WS026-IDENTIFIER,
           :BV0002-FK-PARTY-ID,
           :BV0003-ID,
           :BV0004-ID
           END-EXEC
           IF RELATIONSHIP-NOT-FOUND
              MOVE 100 TO SQLCODE
              MOVE 0 TO WS027-NOT-FOUND-FLAG
           END-IF
           PERFORM 9250-SAVE-DBMS-INFO THRU 9250-EXIT
           MOVE "YDMSHDEM" TO WS-DBMS-TABLE-NAME
           MOVE "1002-FETCH-CURSOR-DATA" TO WS-DBMS-PARA-NAME
           MOVE "SELECT" TO WS-DBMS-STMT-TYPE
           EVALUATE SQLCODE
           WHEN 0
      *       READ Succeeds
              IF DB-PV-STATUS-NOT-OK
                 SET DBMS-PV-ERROR TO TRUE
              ELSE
                 SET DBMS-OK TO TRUE
                 PERFORM 1500-MOVE-SQLOUT-VARS-0003 THRU 1500-EXIT
                 MOVE "Y" TO WS010-HAS-DATA OF WS010-EAV
                 MOVE "Y" TO WS007-HAS-DATA OF WS007-EAV
                 MOVE "Y" TO WS004-HAS-DATA OF WS004-DISTRIBUTOR
                 MOVE "Y" TO WS005-HAS-DATA OF WS005-EMPLOYEE
                 MOVE "N" TO WS010-IS-LOCKED OF WS010-EAV
                 MOVE "N" TO WS007-IS-LOCKED OF WS007-EAV
                 MOVE "N" TO WS004-IS-LOCKED OF WS004-DISTRIBUTOR
                 MOVE "N" TO WS005-IS-LOCKED OF WS005-EMPLOYEE
              END-IF
           WHEN 100
      *       END OF SET
      *       This is a standard READ EACH termination
              CONTINUE
      * 
           WHEN OTHER
      *    Any other database response
      *    There is no custom handler for this so default action
              MOVE SQLCA TO LS-FLOW-DB-SQLCA
              SET LS-FLOW-EXEC-DB-ERROR TO TRUE
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1002-FETCH-CURSOR-DATA" TO LS-FLOW-ERROR-PARA-NAME
              PERFORM 9990-ABORT THRU 9990-EXIT
           END-EVALUATE.
             .
      *--------------------------------------------------------------*
       1002-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       1003-READ-EACH.
      *--------------------------------------------------------------*
           ADD 1 TO WS-ESC-CURRENT-LEVEL
           MOVE 99 TO WS-ESC-QUIT-LEVEL
           PERFORM 1002-FETCH-CURSOR-DATA THRU 1002-EXIT
           IF NOT DBMS-OK
              GO TO 1003-EXIT
           END-IF
      *** Beginning of CREATE ***
      * BEGIN TEXTNUM of WS-ID OF WS-YDMSDOUT OF WS007-EAV
G0005         COMPUTE WS031-TEXTNUM-INPUT-NUMBER = 
G0005           WS-ID OF WS-YDMSDOUT OF WS007-EAV 
G0005         CALL WS-PGM-JSTEXTNO USING
G0005               WS031-TEXTNUM-INPUT
G0005               WS031-TEXTNUM-OUTPUT
G0005               WS011-ERROR-REPORTED
G0005         IF WS011-ERROR-ENCOUNTERED NOT = SPACES
G0005            MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
G0005            MOVE "1003-READ-EACH" TO LS-FLOW-ERROR-PARA-NAME
G0005            MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
G0005            STRING
G0005               "FUNCTION CALL ERROR: "
G0005               DELIMITED BY SIZE
G0005               WS011-ERROR-FUNC-NAME
G0005               DELIMITED BY SPACES
G0005               INTO LS-FLOW-ERROR-MESSAGE-1
G0005            STRING
G0005               "FUNCTION DESC: "
G0005               DELIMITED BY SIZE
G0005               WS011-ERROR-FUNC-ERRMSG
G0005               DELIMITED BY SPACES
G0005               INTO LS-FLOW-ERROR-MESSAGE-2
G0005            SET LS-FLOW-EXEC-ERROR TO TRUE
G0005            PERFORM 9990-ABORT THRU 9990-EXIT
G0005         ELSE
G0005            MOVE WS031-TEXTNUM-OUTPUT-STRING TO WS034-TEMPTXM1
G0005         END-IF
      * END TEXTNUM of WS-ID OF WS-YDMSDOUT OF WS007-EAV
G0005         MOVE  WS034-TEMPTXM1(8:) TO 
G0005           WS-DISTRIBUTOR-ID OF WS-YDMSEMDN OF WS006-EAV 
G0006         MOVE 
G0006           WS-ID OF WS-YDMSPRTY OF WS004-DISTRIBUTOR 
G0006           TO 
G0006           WS-DISTRIBUTOR-PARTY OF WS-YDMSEMDN OF WS006-EAV 
G0007         MOVE 
G0007           WS-ID OF WS-YDMSPRTY OF WS005-EMPLOYEE 
G0007           TO 
G0007           WS-EMPLOYEE-PARTY-ID OF WS-YDMSEMDN OF WS006-EAV 
G0008         MOVE 
G0008           WS-LAST-NME OF WS-YDMSHDEM OF WS010-EAV 
G0008           TO 
G0008           WS-LAST-NAME OF WS-YDMSEMDN OF WS006-EAV 
G0009         MOVE 
G0009           WS-FIRST-NME OF WS-YDMSHDEM OF WS010-EAV 
G0009           TO 
G0009           WS-FIRST-NAME OF WS-YDMSEMDN OF WS006-EAV 
G0010         MOVE 
G0010           WS-MIDDLE-INITIAL OF WS-YDMSHDEM OF WS010-EAV 
G0010           TO 
G0010           WS-MIDDLE-INITIAL OF WS-YDMSEMDN OF WS006-EAV 
G0011         MOVE 
G0011           WS-SOC-SEC-NUM OF WS-YDMSHDEM OF WS010-EAV 
G0011           TO 
G0011           WS-SOCIAL-SECURITY-NB OF WS-YDMSEMDN OF WS006-EAV 
G0012         MOVE 
G0012           WS-BRTH-DTE OF WS-YDMSHDEM OF WS010-EAV 
G0012           TO 
G0012           WS-BIRTH-DTE OF WS-YDMSEMDN OF WS006-EAV 
G0013         MOVE 
G0013           WS-EFF-DTE OF WS-YDMSHDEM OF WS010-EAV 
G0013           TO 
G0013           WS-EFF-DTE OF WS-YDMSEMDN OF WS006-EAV 
G0014         MOVE 
G0014           WS-EXP-DTE OF WS-YDMSHDEM OF WS010-EAV 
G0014           TO 
G0014           WS-EXP-DTE OF WS-YDMSEMDN OF WS006-EAV 
G0015         MOVE 
G0015           WS-LAST-UPD-TSP OF WS-YDMSHDEM OF WS010-EAV 
G0015           TO 
G0015           WS-LAST-UPD-TMSP OF WS-YDMSEMDN OF WS006-EAV 
G0016         MOVE 
G0016           WS-LAST-UPD-UID OF WS-YDMSHDEM OF WS010-EAV 
G0016           TO 
G0016           WS-LAST-UPD-UID OF WS-YDMSEMDN OF WS006-EAV 
G0017         MOVE 
G0017           WS-CONCAT-DATE OF WS-DMS-SCREEN-DATE OF 
G0017           WS003-LOCAL-CURRENT OF WS-DNCB123A-LOCALS 
G0017           TO 
G0017           WS-AS-OF-DTE OF WS-YDMSEMDN OF WS006-EAV 
           PERFORM 1501-MOVE-SQLIN-VARS-0004 THRU 1501-EXIT
           EXEC SQL INSERT INTO "YDMSEMDN" (
           "DISTRIBUTOR_ID",
           "DISTRIBUTOR_PARTY",
           "EMPLOYEE_PARTY_ID",
           "LAST_NAME",
           "FIRST_NAME",
           "MIDDLE_INITIAL",
           "SOCIAL_SECURITY_NB",
           "BIRTH_DTE",
           "EFF_DTE",
           "EXP_DTE",
           "LAST_UPD_TMSP",
           "LAST_UPD_UID",
           "AS_OF_DTE"
           ) VALUES (
           :DCLYDMSEMDN.DISTRIBUTOR-ID,
           :DCLYDMSEMDN.DISTRIBUTOR-PARTY,
           :DCLYDMSEMDN.EMPLOYEE-PARTY-ID,
           :DCLYDMSEMDN.LAST-NAME,
           :DCLYDMSEMDN.FIRST-NAME,
           :DCLYDMSEMDN.MIDDLE-INITIAL :WS032-MIDDLE-INITIAL-NL,
           :DCLYDMSEMDN.SOCIAL-SECURITY-NB,
           :DCLYDMSEMDN.BIRTH-DTE,
           :DCLYDMSEMDN.EFF-DTE,
           :DCLYDMSEMDN.EXP-DTE,
           :DCLYDMSEMDN.LAST-UPD-TMSP,
           :DCLYDMSEMDN.LAST-UPD-UID,
           :DCLYDMSEMDN.AS-OF-DTE
           )
           END-EXEC
           PERFORM 9250-SAVE-DBMS-INFO THRU 9250-EXIT
           MOVE "YDMSEMDN" TO WS-DBMS-TABLE-NAME
           MOVE "1003-READ-EACH" TO WS-DBMS-PARA-NAME
           MOVE "INSERT" TO WS-DBMS-STMT-TYPE
           EVALUATE SQLCODE
           WHEN 0
      *       CREATE Succeeds
              PERFORM 1502-MOVE-SQLOUT-VARS-0004 THRU 1502-EXIT
           WHEN -803
      *    DUPLICATE KEY
      *    There is a custom handler for this later so no action here
              CONTINUE
      * 
           WHEN OTHER
      *    Any other database response
      *    There is no custom handler for this so default action
              MOVE SQLCA TO LS-FLOW-DB-SQLCA
              SET LS-FLOW-EXEC-DB-ERROR TO TRUE
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1003-READ-EACH" TO LS-FLOW-ERROR-PARA-NAME
              PERFORM 9990-ABORT THRU 9990-EXIT
           END-EVALUATE
      *** End of CREATE ***
           EVALUATE TRUE
G0004         WHEN DBMS-OK
G0004            PERFORM 1503-CASE THRU 1503-EXIT
G0004            IF WS-ESC-QUIT-LEVEL < WS-ESC-CURRENT-LEVEL
G0004               GO TO 1003-EXIT
G0004            ELSE
G0004               MOVE 99 TO WS-ESC-QUIT-LEVEL
G0004            END-IF
G0004            SET STAY-IN-LOOP TO TRUE
G0004         WHEN DBMS-DUPLICATE
G0004            MOVE "OK" TO WS-DBMS-STATUS-MESSAGE
G0004            PERFORM 1504-CASE THRU 1504-EXIT
G0004            IF WS-ESC-QUIT-LEVEL < WS-ESC-CURRENT-LEVEL
G0004               GO TO 1003-EXIT
G0004            ELSE
G0004               MOVE 99 TO WS-ESC-QUIT-LEVEL
G0004            END-IF
G0004            SET STAY-IN-LOOP TO TRUE
G0004         WHEN DBMS-PV-ERROR
G0004            MOVE "OK" TO WS-DBMS-STATUS-MESSAGE
G0004            PERFORM 1505-CASE THRU 1505-EXIT
G0004            IF WS-ESC-QUIT-LEVEL < WS-ESC-CURRENT-LEVEL
G0004               GO TO 1003-EXIT
G0004            ELSE
G0004               MOVE 99 TO WS-ESC-QUIT-LEVEL
G0004            END-IF
G0004            SET STAY-IN-LOOP TO TRUE
G0004         WHEN OTHER
G0004            CONTINUE
           END-EVALUATE
           CONTINUE.
       1003-EXIT.
      *    ESCAPE level tracking - DO NOT MOVE
           SUBTRACT 1 FROM WS-ESC-CURRENT-LEVEL
           EXIT.
       1500-MOVE-SQLOUT-VARS-0003.
      *
      *  Output to the Foreign Key stores and set usage statuses - 
      *    variant 2 
      *
           MOVE 2 TO WS010-FKSTATUS-HAS OF WS010-EAV
           MOVE BV0001-FK-EMPL-PART TO 
             WS010-FK-EMPL-PARTY-ID OF WS010-EAV 
           MOVE 2 TO WS007-FKSTATUS-SPECIFIES OF WS007-EAV
           MOVE BV0002-FK-PARTY-ID TO WS007-FK-PARTY-ID OF WS007-EAV
      *
      *  Output to the EAV attr stores
      *
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "D" TO WS030-COBOL-DB2-IND
           MOVE WS015-EFFECTIVE-DATE TO WS030-DT-T-TS-INPUT-VAL
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1500-MOVE-SQLOUT-VARS-0003" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:8) TO 
                WS-EFF-DTE OF WS-YDMSHDEM OF WS010-EAV 
           END-IF
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "D" TO WS030-COBOL-DB2-IND
           MOVE WS016-EXPIRATION-DATE TO WS030-DT-T-TS-INPUT-VAL
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1500-MOVE-SQLOUT-VARS-0003" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:8) TO 
                WS-EXP-DTE OF WS-YDMSHDEM OF WS010-EAV 
           END-IF
           IF WS018-SOCIAL-SECURITY-NUMBE-NL = -1
              INITIALIZE WS-SOC-SEC-NUM OF WS-YDMSHDEM OF WS010-EAV
           ELSE
              MOVE WS017-SOCIAL-SECURITY-NUMBER TO 
                WS-SOC-SEC-NUM OF WS-YDMSHDEM OF WS010-EAV 
           END-IF
           MOVE WS019-FIRST-NAME TO 
             WS-FIRST-NME OF WS-YDMSHDEM OF WS010-EAV 
           IF WS021-MIDDLE-INITIAL-NL = -1
              INITIALIZE WS-MIDDLE-INITIAL OF WS-YDMSHDEM OF WS010-EAV
           ELSE
              MOVE WS020-MIDDLE-INITIAL TO 
                WS-MIDDLE-INITIAL OF WS-YDMSHDEM OF WS010-EAV 
           END-IF
           MOVE WS022-LAST-NAME TO 
             WS-LAST-NME OF WS-YDMSHDEM OF WS010-EAV 
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "D" TO WS030-COBOL-DB2-IND
           MOVE WS023-BIRTH-DATE TO WS030-DT-T-TS-INPUT-VAL
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1500-MOVE-SQLOUT-VARS-0003" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:8) TO 
                WS-BRTH-DTE OF WS-YDMSHDEM OF WS010-EAV 
           END-IF
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "D" TO WS030-COBOL-DB2-IND
           MOVE WS024-LAST-UPDATE-TIMESTAMP TO WS030-DT-T-TS-INPUT-VAL
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1500-MOVE-SQLOUT-VARS-0003" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT TO 
                WS-LAST-UPD-TSP OF WS-YDMSHDEM OF WS010-EAV 
           END-IF
           MOVE WS025-LAST-UPDATE-USERID TO 
             WS-LAST-UPD-UID OF WS-YDMSHDEM OF WS010-EAV 
           MOVE WS026-IDENTIFIER TO WS-ID OF WS-YDMSDOUT OF WS007-EAV
           MOVE BV0002-FK-PARTY-ID TO 
             WS-ID OF WS-YDMSPRTY OF WS004-DISTRIBUTOR 
           MOVE BV0002-FK-PARTY-ID TO 
             WS-ID OF WS-YDMSPRTY OF WS005-EMPLOYEE 
           .
      *--------------------------------------------------------------*
       1500-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       1501-MOVE-SQLIN-VARS-0004.
      *   START Bind Variable Initialisation
      *   Initialise bind variables before CREATE
           INITIALIZE DISTRIBUTOR-ID OF DCLYDMSEMDN
           INITIALIZE DISTRIBUTOR-PARTY OF DCLYDMSEMDN
           INITIALIZE EMPLOYEE-PARTY-ID OF DCLYDMSEMDN
           INITIALIZE LAST-NAME OF DCLYDMSEMDN
           INITIALIZE FIRST-NAME OF DCLYDMSEMDN
           INITIALIZE MIDDLE-INITIAL OF DCLYDMSEMDN
           INITIALIZE SOCIAL-SECURITY-NB OF DCLYDMSEMDN
           MOVE "0001-01-01" TO BIRTH-DTE OF DCLYDMSEMDN
           MOVE "0001-01-01" TO EFF-DTE OF DCLYDMSEMDN
           MOVE "0001-01-01" TO EXP-DTE OF DCLYDMSEMDN
           MOVE "0001-01-01 00.00.00" TO LAST-UPD-TMSP OF DCLYDMSEMDN
           INITIALIZE LAST-UPD-UID OF DCLYDMSEMDN
           MOVE "0001-01-01" TO AS-OF-DTE OF DCLYDMSEMDN
      *   END Bind Variable Initialisation
           MOVE 
             WS-DISTRIBUTOR-ID OF WS-YDMSEMDN OF WS006-EAV 
             TO 
             DISTRIBUTOR-ID OF DCLYDMSEMDN 
           MOVE 
             WS-DISTRIBUTOR-PARTY OF WS-YDMSEMDN OF WS006-EAV 
             TO 
             DISTRIBUTOR-PARTY OF DCLYDMSEMDN 
           MOVE 
             WS-EMPLOYEE-PARTY-ID OF WS-YDMSEMDN OF WS006-EAV 
             TO 
             EMPLOYEE-PARTY-ID OF DCLYDMSEMDN 
           MOVE 
             WS-LAST-NAME OF WS-YDMSEMDN OF WS006-EAV 
             TO 
             LAST-NAME OF DCLYDMSEMDN 
           MOVE 
             WS-FIRST-NAME OF WS-YDMSEMDN OF WS006-EAV 
             TO 
             FIRST-NAME OF DCLYDMSEMDN 
           MOVE 
             WS-MIDDLE-INITIAL OF WS-YDMSEMDN OF WS006-EAV 
             TO 
             MIDDLE-INITIAL OF DCLYDMSEMDN 
           MOVE 
             WS-SOCIAL-SECURITY-NB OF WS-YDMSEMDN OF WS006-EAV 
             TO 
             SOCIAL-SECURITY-NB OF DCLYDMSEMDN 
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "C" TO WS030-COBOL-DB2-IND
           MOVE 
             WS-BIRTH-DTE OF WS-YDMSEMDN OF WS006-EAV 
             TO WS030-DT-T-TS-INPUT-VAL 
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1501-MOVE-SQLIN-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:10) TO 
                BIRTH-DTE OF DCLYDMSEMDN 
           END-IF
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "C" TO WS030-COBOL-DB2-IND
           MOVE 
             WS-EFF-DTE OF WS-YDMSEMDN OF WS006-EAV 
             TO WS030-DT-T-TS-INPUT-VAL 
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1501-MOVE-SQLIN-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:10) TO EFF-DTE OF DCLYDMSEMDN
           END-IF
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "C" TO WS030-COBOL-DB2-IND
           MOVE 
             WS-EXP-DTE OF WS-YDMSEMDN OF WS006-EAV 
             TO WS030-DT-T-TS-INPUT-VAL 
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1501-MOVE-SQLIN-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:10) TO EXP-DTE OF DCLYDMSEMDN
           END-IF
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "C" TO WS030-COBOL-DB2-IND
           MOVE 
             WS-LAST-UPD-TMSP OF WS-YDMSEMDN OF WS006-EAV 
             TO WS030-DT-T-TS-INPUT-VAL 
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1501-MOVE-SQLIN-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT TO LAST-UPD-TMSP OF DCLYDMSEMDN
           END-IF
           MOVE 
             WS-LAST-UPD-UID OF WS-YDMSEMDN OF WS006-EAV 
             TO 
             LAST-UPD-UID OF DCLYDMSEMDN 
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "C" TO WS030-COBOL-DB2-IND
           MOVE 
             WS-AS-OF-DTE OF WS-YDMSEMDN OF WS006-EAV 
             TO WS030-DT-T-TS-INPUT-VAL 
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1501-MOVE-SQLIN-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:10) TO 
                AS-OF-DTE OF DCLYDMSEMDN 
           END-IF
           .
      *--------------------------------------------------------------*
       1501-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       1502-MOVE-SQLOUT-VARS-0004.
      *
      *  Output to the EAV attr stores
      *
           MOVE 
             DISTRIBUTOR-ID OF DCLYDMSEMDN 
             TO 
             WS-DISTRIBUTOR-ID OF WS-YDMSEMDN OF WS006-EAV 
           MOVE 
             DISTRIBUTOR-PARTY OF DCLYDMSEMDN 
             TO 
             WS-DISTRIBUTOR-PARTY OF WS-YDMSEMDN OF WS006-EAV 
           MOVE 
             EMPLOYEE-PARTY-ID OF DCLYDMSEMDN 
             TO 
             WS-EMPLOYEE-PARTY-ID OF WS-YDMSEMDN OF WS006-EAV 
           MOVE 
             LAST-NAME OF DCLYDMSEMDN 
             TO 
             WS-LAST-NAME OF WS-YDMSEMDN OF WS006-EAV 
           MOVE 
             FIRST-NAME OF DCLYDMSEMDN 
             TO 
             WS-FIRST-NAME OF WS-YDMSEMDN OF WS006-EAV 
           IF WS032-MIDDLE-INITIAL-NL = -1
              INITIALIZE WS-MIDDLE-INITIAL OF WS-YDMSEMDN OF WS006-EAV
           ELSE
              MOVE 
                MIDDLE-INITIAL OF DCLYDMSEMDN 
                TO 
                WS-MIDDLE-INITIAL OF WS-YDMSEMDN OF WS006-EAV 
           END-IF
           MOVE 
             SOCIAL-SECURITY-NB OF DCLYDMSEMDN 
             TO 
             WS-SOCIAL-SECURITY-NB OF WS-YDMSEMDN OF WS006-EAV 
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "D" TO WS030-COBOL-DB2-IND
           MOVE BIRTH-DTE OF DCLYDMSEMDN TO WS030-DT-T-TS-INPUT-VAL
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1502-MOVE-SQLOUT-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:8) TO 
                WS-BIRTH-DTE OF WS-YDMSEMDN OF WS006-EAV 
           END-IF
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "D" TO WS030-COBOL-DB2-IND
           MOVE EFF-DTE OF DCLYDMSEMDN TO WS030-DT-T-TS-INPUT-VAL
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1502-MOVE-SQLOUT-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:8) TO 
                WS-EFF-DTE OF WS-YDMSEMDN OF WS006-EAV 
           END-IF
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "D" TO WS030-COBOL-DB2-IND
           MOVE EXP-DTE OF DCLYDMSEMDN TO WS030-DT-T-TS-INPUT-VAL
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1502-MOVE-SQLOUT-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:8) TO 
                WS-EXP-DTE OF WS-YDMSEMDN OF WS006-EAV 
           END-IF
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "D" TO WS030-COBOL-DB2-IND
           MOVE LAST-UPD-TMSP OF DCLYDMSEMDN TO WS030-DT-T-TS-INPUT-VAL
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1502-MOVE-SQLOUT-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT TO 
                WS-LAST-UPD-TMSP OF WS-YDMSEMDN OF WS006-EAV 
           END-IF
           MOVE 
             LAST-UPD-UID OF DCLYDMSEMDN 
             TO 
             WS-LAST-UPD-UID OF WS-YDMSEMDN OF WS006-EAV 
           INITIALIZE WS030-DT-T-TS-INPUT
           MOVE "D" TO WS030-COBOL-DB2-IND
           MOVE AS-OF-DTE OF DCLYDMSEMDN TO WS030-DT-T-TS-INPUT-VAL
           CALL WS-PGM-JSFORMDT USING
                           WS030-DT-T-TS-INPUT
                           WS030-DT-T-TS-OUTPUT
                           WS011-ERROR-REPORTED
           IF WS011-ERROR-ENCOUNTERED NOT = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
              MOVE "1502-MOVE-SQLOUT-VARS-0004" TO 
                LS-FLOW-ERROR-PARA-NAME 
              MOVE WS011-ERROR-FUNC-ERRMSG TO LS-FLOW-ERROR-CODE
              STRING
                 "FUNCTION CALL ERROR: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-NAME
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-1
              STRING
                 "FUNCTION DESC: "
                 DELIMITED BY SIZE
                 WS011-ERROR-FUNC-ERRMSG
                 DELIMITED BY SPACES
                 INTO LS-FLOW-ERROR-MESSAGE-2
              SET LS-FLOW-EXEC-ERROR TO TRUE
              PERFORM 9990-ABORT THRU 9990-EXIT
           ELSE
              MOVE WS030-DT-T-TS-OUTPUT(1:8) TO 
                WS-AS-OF-DTE OF WS-YDMSEMDN OF WS006-EAV 
           END-IF
           .
      *--------------------------------------------------------------*
       1502-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *--------------------------------------------------------------*
       1503-CASE.
      *--------------------------------------------------------------*
           ADD 1 TO WS-ESC-CURRENT-LEVEL
           MOVE 99 TO WS-ESC-QUIT-LEVEL
G0018      COMPUTE WS-ESC-QUIT-LEVEL = WS-ESC-CURRENT-LEVEL - 2
G0018      GO TO 1503-EXIT
           CONTINUE.
       1503-EXIT.
      *    ESCAPE level tracking - DO NOT MOVE
           SUBTRACT 1 FROM WS-ESC-CURRENT-LEVEL
           EXIT.
      *--------------------------------------------------------------*
       1504-CASE.
      *--------------------------------------------------------------*
           ADD 1 TO WS-ESC-CURRENT-LEVEL
           MOVE 99 TO WS-ESC-QUIT-LEVEL
G0019      COMPUTE WS-ESC-QUIT-LEVEL = WS-ESC-CURRENT-LEVEL - 2
G0019      GO TO 1504-EXIT
           CONTINUE.
       1504-EXIT.
      *    ESCAPE level tracking - DO NOT MOVE
           SUBTRACT 1 FROM WS-ESC-CURRENT-LEVEL
           EXIT.
      *--------------------------------------------------------------*
       1505-CASE.
      *--------------------------------------------------------------*
           ADD 1 TO WS-ESC-CURRENT-LEVEL
           MOVE 99 TO WS-ESC-QUIT-LEVEL
G0020      COMPUTE WS-ESC-QUIT-LEVEL = WS-ESC-CURRENT-LEVEL - 2
G0020      GO TO 1505-EXIT
           CONTINUE.
       1505-EXIT.
      *    ESCAPE level tracking - DO NOT MOVE
           SUBTRACT 1 FROM WS-ESC-CURRENT-LEVEL
           EXIT.
      *--------------------------------------------------------------*
       9200-SEND-DB2-INFO.
      *--------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *  FUNCTION: PUTS OUT A DB2 INFORMATION ERROR SCREEN             *
      *  - THE DB2 ERROR MESSAGES WERE OBTAINED EARLIER                *
      *  - JUST SET UP TO CALL THE ABEND ROUTINE                       *
      *                                                                *
      ******************************************************************
           PERFORM 9990-ABORT THRU 9990-EXIT.
           .
       9200-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       9250-SAVE-DBMS-INFO.
      *--------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *  FUNCTION: SAVES THE DBMS STATUS INFO IN CUSTOMISED FORM       *
      *                                                                *
      ******************************************************************
           MOVE SQLCODE TO WS-DBMS-SQLCODE
           EVALUATE WS-DBMS-SQLCODE
           WHEN 0
              SET DBMS-OK TO TRUE
           WHEN 100
              SET DBMS-END-OF-SET TO TRUE
           WHEN -913 WHEN -911
              SET DBMS-TIMEOUT-DEADLOCK TO TRUE
           WHEN -803
              SET DBMS-DUPLICATE TO TRUE
           WHEN OTHER
              SET DBMS-ERROR TO TRUE
           END-EVALUATE
           .
      *--------------------------------------------------------------*
       9250-EXIT.
      *--------------------------------------------------------------*
           EXIT.
      *--------------------------------------------------------------*
       9260-SAVE-DBMS-ERROR-INFO.
      *--------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *  FUNCTION: SAVES THE DBMS ERROR MSGS                           *
      *                                                                *
      ******************************************************************
           CALL 'DSNTIAC' USING SQLCA
                                WS-DSN-ERROR-MESSAGE
                                WS-DSN-ERROR-TEXT-LEN
           END-CALL
           MOVE WS-DSN-ERROR-TEXT (1) TO LS-FLOW-ERROR-MESSAGE-1
           MOVE WS-DSN-ERROR-TEXT (2) TO LS-FLOW-ERROR-MESSAGE-2
           MOVE WS-DSN-ERROR-TEXT (3) TO LS-FLOW-ERROR-MESSAGE-3
           MOVE WS-DSN-ERROR-TEXT (4) TO LS-FLOW-ERROR-MESSAGE-4
           MOVE WS-DSN-ERROR-TEXT (5) TO LS-FLOW-ERROR-MESSAGE-5
           MOVE WS-DSN-ERROR-TEXT (6) TO LS-FLOW-ERROR-MESSAGE-6
           MOVE SQLCA TO LS-FLOW-DB-SQLCA
           .
      *--------------------------------------------------------------*
       9260-EXIT.
      *--------------------------------------------------------------*
           EXIT.
      * 
      *--------------------------------------------------------------*
       9920-CLOSE-CURSORS.
      *--------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *  FUNCTION: Close any open DBMS Cursors                         *
      *                                                                *
      ******************************************************************
      * 
           IF WS-CRSR014-YDMSHDEM-FLAG = "Y"
              EXEC SQL CLOSE CRSR014_YDMSHDEM
              END-EXEC
              MOVE "N" TO WS-CRSR014-YDMSHDEM-FLAG
           END-IF
           .
      *--------------------------------------------------------------*
       9920-EXIT.
      *--------------------------------------------------------------*
           EXIT.
      *
       9990-ABORT.
      * NOTE We come here when functions etc are unsuccessful and for 
      *   requests to ABEND 
      *    Handle ABORT request
           IF LS-FLOW-ERROR-PROG-NAME = SPACES
              MOVE "DNCB123A" TO LS-FLOW-ERROR-PROG-NAME
           END-IF
      *    The caller will handle the ABORT request so GOBACK
           GOBACK
           .
       9990-EXIT.
           EXIT.
      * 
      *=*=*=*=*= ACTION DIAGRAM SUMMARY =*=*=*=*=*
G0001 *      SET local_current dms_screen_date concat_date TO 
G0001 *        CURRENT_DATE 
      *      NOTE ************************************************
      *      Retrieve the most current employee information
      *      for the denormalized table.
      *      ************************************************
G0003 *      READ EACH distributor_employee_history, distributor_outlet, 
G0003 *        distributor party, employee party SORTED BY DESCENDING 
G0003 *        distributor_employee_history expiration_date WHERE 
G0003 *        DESIRED distributor_employee_history pertains_to SOME 
G0003 *        distributor_employee AND THAT distributor_employee 
G0003 *        participates_as DESIRED employee party AND THAT 
G0003 *        distributor_employee is_employed_as SOME 
G0003 *        distributor_outlet_employee AND THAT 
G0003 *        distributor_outlet_employee is_employed_by DESIRED 
G0003 *        distributor_outlet AND DESIRED distributor_outlet 
G0003 *        participates_as DESIRED distributor party 
G0004 *         CREATE denormalized_employee
G0005 *            SET distributor_id TO textnum(distributor_outlet 
G0005 *              identifier) 
G0006 *            SET distributor_party_id TO distributor party 
G0006 *              identifier 
G0007 *            SET employee_party_id TO employee party identifier
G0008 *            SET last_name TO distributor_employee_history 
G0008 *              last_name 
G0009 *            SET first_name TO distributor_employee_history 
G0009 *              first_name 
G0010 *            SET middle_initial TO distributor_employee_history 
G0010 *              middle_initial 
G0011 *            SET social_security_nbr TO 
G0011 *              distributor_employee_history social_security_number 
G0012 *            SET birth_date TO distributor_employee_history 
G0012 *              birth_date 
G0013 *            SET effective_date TO distributor_employee_history 
G0013 *              effective_date 
G0014 *            SET expiration_date TO distributor_employee_history 
G0014 *              expiration_date 
G0015 *            SET last_update_timestamp TO 
G0015 *              distributor_employee_history last_update_timestamp 
G0016 *            SET last_update_userid TO 
G0016 *              distributor_employee_history last_update_userid 
G0017 *            SET as_of_date TO local_current dms_screen_date 
G0017 *              concat_date 
G0004 *            WHEN successful
G0018 *               NEXT
G0004 *            WHEN already exists
G0019 *               NEXT
G0004 *            WHEN permitted value violation
G0020 *               NEXT
