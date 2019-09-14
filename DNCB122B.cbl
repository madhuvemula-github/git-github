      **************************************************
      *            IDENTIFICATION DIVISION             *
      **************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DNCB122B.
       AUTHOR.        Jumar Solutions.
      *   Written on 09/01/2019 10:35:37 using XCG v3.2.50
      *   Name: DMS_DENORMALIZE_EMPLOYEE
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
      * Dummy declarations for calls to JSABEND
       01  WS-DUMMY-IO-PCB   PIC  XXXX VALUE SPACES.
       01  WS-DUMMY-ALT-PCB  PIC  XXXX VALUE SPACES.

       COPY JSFLWCTL.
       01  WS-DB-PV-ERROR                 PIC  X VALUE 'N'.
           88 DB-PV-STATUS-OK              VALUE 'N'.
           88 DB-PV-STATUS-NOT-OK          VALUE 'Y'.
       01  WS001-CONTROL-VARIABLES.
           05 WS-FLOW-CONTROL.
              10 WS-FLOW-TRNCD            PIC  X(8).
           05 WS-ESCAPE-MECHANISM.
              10 WS-ESC-CURRENT-LEVEL     PIC  99 COMP-3.
              10 WS-ESC-QUIT-LEVEL        PIC  99 COMP-3.
              10 WS-ESCAPE-LOOP-YN        PIC  X.
                 88 ESCAPE-LOOP            VALUE 'Y'.
                 88 STAY-IN-LOOP           VALUE 'N'.
           05 WS-PGM-DNCB123A             PIC  X(08) VALUE 'DNCB123A'.
           05 WS-PGM-JSCDBERR             PIC  X(8) VALUE 'JSCDBERR'.
           05 WS-PGM-CEE3ABD              PIC  X(08) VALUE 'CEE3ABD'.
           05 WS-ALREADY-SYSTEM-ERROR-SW  PIC  X(1) VALUE 'N'.
              88 WS-ALREADY-SYSTEM-ERROR   VALUE 'Y'.
       01  WS-CEE3ABD-FIELDS.
           05 WS-CEE3ABD                  PIC  X(8) VALUE 'CEE3ABD'.
           05 WS-ABEND-CODE               PIC S9(9) BINARY VALUE +0000.
           05 WS-DUMP-CODE                PIC S9(9) VALUE +1.
              88 WS-DUMP-SYSTEM            VALUE +0.
              88 WS-DUMP-LE370             VALUE +1.

      ************************************
      *    IMPORT/EXPORT Views for CALL to
      *    CREATE_DENORMALIZED_EMPL_TABLE
      *
      *    END OF IMPORT/EXPORT Views
      ************************************
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
       01  WS004-ERROR-REPORTED.
           05 WS004-ERROR-ENCOUNTERED           PIC  X.
           05 WS004-ERROR-FUNC-NAME             PIC  X(8).
           05 WS004-ERROR-FUNC-ERRMSG           PIC  9(4).
       01  WS005-FUNCTION-WORKAREA.
           05 WS005-SCREEN-NUMBER-IN-OUT.
              10 WS005-SCREEN-NUMBER            PIC  X(80).
              10 WS005-COBOL-NUMERIC            PIC S9(18).
              10 WS005-EDIT-PATTERN-NUM         PIC  X(80).
              10 WS005-SCREEN-COBOL-IND-NUM     PIC  X.
              10 WS005-FIELD-FILL-CHAR          PIC  X.
              10 WS005-DECIMAL-CHAR             PIC  X.
              10 WS005-THOUSAND-CHAR            PIC  X.
              10 WS005-BLANK-WHEN-ZERO          PIC  X.
              10 WS005-IS-PROTECTED             PIC  X.
              10 WS005-JUSTIFY                  PIC  X.
           05 WS005-SCREEN-TIME-IN-OUT.
              10 WS005-SCREEN-TIME              PIC  X(8).
              10 WS005-COBOL-TIME               PIC  9(6).
              10 WS005-EDIT-PATTERN-TIME        PIC  X(8).
              10 WS005-SCREEN-COBOL-IND-TIME    PIC  X.
              10 WS005-TIME-FILL-CHAR           PIC  X.
              10 WS005-BLANK-WHEN-ZERO          PIC  X.
              10 WS005-IS-PROTECTED             PIC  X.
           05 WS005-SCREEN-DATE-IN-OUT.
              10 WS005-SCREEN-DATE              PIC  X(10).
              10 WS005-COBOL-DATE               PIC S9(8).
              10 WS005-EDIT-PATTERN-DATE        PIC  X(10).
              10 WS005-SCREEN-COBOL-IND-DATE    PIC  X.
              10 WS005-DATE-FILL-CHAR           PIC  X.
              10 WS005-BLANK-WHEN-ZERO          PIC  X.
              10 WS005-IS-PROTECTED             PIC  X.
           05 WS005-SCREEN-TSTAMP-IN-OUT.
              10 WS005-SCREEN-TSTAMP            PIC  X(26).
              10 WS005-COBOL-TSTAMP             PIC  X(20).
              10 WS005-EDIT-PATTERN-TSTAMP      PIC  X(255).
              10 WS005-SCREEN-COBOL-IND-TSTAMP  PIC  X.
              10 WS005-BLANK-WHEN-ZERO          PIC  X.
              10 WS005-IS-PROTECTED             PIC  X.
           05 WS005-SCREEN-TEXT-IN-OUT.
              10 WS005-SCREEN-TEXT              PIC  X(255).
              10 WS005-COBOL-TEXT               PIC  X(255).
              10 WS005-EDIT-PATTERN-TEXT        PIC  X(255).
              10 WS005-SCREEN-COBOL-IND-TEXT    PIC  X.
              10 WS005-TXT-FILL-CHAR            PIC  X.
              10 WS005-IS-PROTECTED             PIC  X.
           05 WS005-COUNT                       PIC S9(3).
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
       01  WS-ABEND-TYPE   PIC  X(5) VALUE 'BATCH'.
       01  WS-PGM-JSABEND  PIC  X(8) VALUE 'JSABEND'.

      * 
      * 
      * 

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   SQL COMMUNICATION AREA                                    *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
      * 
      **************************************************
      *            PROCEDURE DIVISION                  *
      **************************************************
       PROCEDURE DIVISION.
       0000-MODULE-ENTRY.
           PERFORM 0010-INITIALIZATION THRU 0010-EXIT
           PERFORM 0500-MAIN-LOGIC THRU 0500-EXIT
           PERFORM 7000-CHECK-STATUS THRU 7000-EXIT
           GOBACK.
       0000-MODULE-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       0010-INITIALIZATION.
      *--------------------------------------------------------------*
           MOVE "0010-INITIALIZATION" TO LS-FLOW-CUR-PARA-NAME
           INITIALIZE LS-FLOW
           INITIALIZE WS-DBMS-RESPONSE
           MOVE  "DNCB122B " TO LS-FLOW-CUR-PROG-ID
           MOVE  "DMS_DENORMALIZE_EMPLOYEE" TO LS-FLOW-CUR-ACTION-NAME
           MOVE  "EN" TO LS-FLOW-LANGUAGE-CODE
           MOVE "DNCB122B" TO LS-FLOW-TRANCODE
      * NOTE Initialize all Working-Storage and Linkage variables that 
      *   require it. 
      *  Initialise all Foreign Key Stores
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
      * NOTE ************************************************
      *      The only reason for this procedure is so that
      *      the DB2 is in an action block so that the DBRM
      *      and the module name are the same.
      *      (e.g.DMCB123A).  This is needed in order for
      *      endevor to perform the binds.
      *      ************************************************
G0002      PERFORM 1000-CALL-DNCB123A THRU 1000-EXIT
           .
       0500-EXIT.
           EXIT.
      *--------------------------------------------------------------*
       1000-CALL-DNCB123A.
      *--------------------------------------------------------------*
           ADD 1 TO WS-ESC-CURRENT-LEVEL
           MOVE 99 TO WS-ESC-QUIT-LEVEL
      *    Move statements before CALL
      *    CALL CREATE_DENORMALIZED_EMPL_TABLE
G0002      INITIALIZE LS-FLOW-EXEC-STATUS
G0002      CALL WS-PGM-DNCB123A USING
G0002            LS-FLOW
G0002      IF LS-FLOW-EXEC-STATUS NOT = SPACES
G0002         IF LS-FLOW-ERROR-PARA-NAME = SPACES
G0002            MOVE "1000-CALL-DNCB123A" TO LS-FLOW-ERROR-PARA-NAME
G0002         END-IF
G0002         PERFORM 9990-ABORT THRU 9990-EXIT
G0002      END-IF
      *    Move statements after CALL
           .
       1000-EXIT.
      *    ESCAPE level tracking - DO NOT MOVE
           SUBTRACT 1 FROM WS-ESC-CURRENT-LEVEL
           EXIT.
      *--------------------------------------------------------------*
       7000-CHECK-STATUS.
      *--------------------------------------------------------------*
           EVALUATE LS-FLOW-STATUS-TERM
           WHEN 'R'
      *       Handle ROLLBACK request
              PERFORM 8500-ROLLBACK THRU 8500-EXIT
      * 
           WHEN 'A'
      *       Handle ABORT request
              PERFORM 9990-ABORT THRU 9990-EXIT
      * 
           WHEN 'X'
      *       Handle RETRY TRANSACTION
              PERFORM 8550-RETRY THRU 8550-EXIT
      * 
           WHEN OTHER
      *       No action required here
              CONTINUE
           END-EVALUATE
      * 
      *  Check for PV Errors whilst reading the database
           IF DB-PV-STATUS-NOT-OK
              IF LS-FLOW-ERROR-PARA-NAME = SPACES
                 MOVE "7000-CHECK-STATUS" TO LS-FLOW-ERROR-PARA-NAME
              END-IF
              PERFORM 9100-SYSTEM-ERROR THRU 9100-EXIT
              GO TO 7000-EXIT
           END-IF
           CONTINUE.
      *--------------------------------------------------------------*
       7000-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *--------------------------------------------------------------*
       8400-WRITE-MESSAGE.
      *--------------------------------------------------------------*
           MOVE "8400-WRITE-MESSAGE" TO LS-FLOW-CUR-PARA-NAME
           DISPLAY "DNCB122B".
           DISPLAY "EXIT STATUS: " WITH NO ADVANCING.
           EVALUATE LS-FLOW-STATUS-TERM
              WHEN "R"
                 DISPLAY "ROLLBACK"
              WHEN "A"
                 DISPLAY "ABORT"
              WHEN OTHER
                 DISPLAY "MESSAGE"
           END-EVALUATE.
           DISPLAY "MESSAGE TYPE: " WITH NO ADVANCING.
           EVALUATE LS-FLOW-STATUS-MSGTYP
              WHEN "N"
                 DISPLAY "NORMAL"
              WHEN "E"
                 DISPLAY "ERROR"
              WHEN "I"
                 DISPLAY "INFORMATIONAL"
              WHEN "W"
                 DISPLAY "WARNING"
              WHEN OTHER
                 DISPLAY "UNKNOWN"
           END-EVALUATE.
           DISPLAY "MESSAGE: " LS-FLOW-STATUS-MSGTXT.
             .
      * 
      *--------------------------------------------------------------*
       8400-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      * 
      *--------------------------------------------------------------*
       8500-ROLLBACK.
      *--------------------------------------------------------------*
           MOVE "8500-ROLLBACK" TO LS-FLOW-CUR-PARA-NAME
            EXEC SQL
            ROLLBACK
            END-EXEC
              .
      *--------------------------------------------------------------*
       8500-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      * 
      *--------------------------------------------------------------*
       8550-RETRY.
      *--------------------------------------------------------------*
           MOVE "8550-RETRY" TO LS-FLOW-CUR-PARA-NAME
      *    Simulate an ABEND FLOW STATUS
           MOVE "Y" TO LS-FLOW-STATUS-DSCRN
           MOVE "Y" TO LS-FLOW-STATUS-ENDEX
           MOVE "E" TO LS-FLOW-STATUS-MSGTYP
           MOVE "ABORT DISPLAYING" TO LS-FLOW-STATUS-NAME
           MOVE "RETRY TRANSACTION was requested" TO 
           LS-FLOW-STATUS-MSGTXT 
           MOVE "A" TO LS-FLOW-STATUS-TERM
      *    Save diagnostic information
           MOVE "8550-RETRY" TO LS-FLOW-ERROR-PARA-NAME
           MOVE "DNCB122B" TO LS-FLOW-ERROR-PROG-NAME
           PERFORM 9990-ABORT THRU 9990-EXIT
             .
      * 
      *--------------------------------------------------------------*
       8550-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      * 
       9100-SYSTEM-ERROR.
           MOVE 
           "SYSTEM ERROR ENCOUNTERED -PLEASE CONTACT TECHNICAL SUPPORT" 
           TO 
              LS-FLOW-ERROR-MESSAGE-1
           SET LS-FLOW-SYSTEM-ERROR TO TRUE.
           PERFORM 9990-ABORT THRU 9990-EXIT
           .
       9100-EXIT.
           EXIT.
      * 
      *--------------------------------------------------------------*
       9990-ABORT.
      *--------------------------------------------------------------*
      * NOTE We come here when functions etc are unsuccessful and for
      *   requests to ABEND 
      *    Handle ABORT request
           IF LS-FLOW-ERROR-PROG-NAME = SPACES
              MOVE "DNCB122B" TO LS-FLOW-ERROR-PROG-NAME
           END-IF

      *    Handle ABORT request
           CALL WS-PGM-JSABEND USING
              LS-FLOW
              WS-DBMS-RESPONSE
              WS-DUMMY-IO-PCB
              WS-DUMMY-ALT-PCB
              WS-ABEND-TYPE

           GOBACK
           .
      *--------------------------------------------------------------*
       9990-EXIT.
      *--------------------------------------------------------------*
           EXIT.
      * 
      *=*=*=*=*= ACTION DIAGRAM SUMMARY =*=*=*=*=*
      *      NOTE ************************************************
      *      The only reason for this procedure is so that
      *      the DB2 is in an action block so that the DBRM
      *      and the module name are the same.
      *      (e.g.DMCB123A).  This is needed in order for
      *      endevor to perform the binds.
      *      ************************************************
G0002 *      USE create_denormalized_empl_table
G0002 *
G0002 *      WHICH IMPORTS:
G0002 *
G0002 *      WHICH EXPORTS:
