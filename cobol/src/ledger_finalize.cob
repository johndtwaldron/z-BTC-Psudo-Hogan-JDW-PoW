       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGERFIN.
       AUTHOR. HOGAN-CRYPTO-POC.
       DATE-WRITTEN. 2024.
      *
      * Ledger Finalization Program
      * Processes confirmations and generates audit reports
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONFIRM-FILE ASSIGN TO "CONFIRMIN"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-CONFIRM-STATUS.
           
           SELECT LEDGER-FILE ASSIGN TO "LEDGER"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-LEDGER-STATUS.
           
           SELECT AUDIT-FILE ASSIGN TO "AUDITOUT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-AUDIT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CONFIRM-FILE.
       01  CONFIRM-RECORD.
           05  CONF-TRX-ID         PIC X(10).
           05  CONF-STATUS         PIC X(8).
           05  CONF-HASH           PIC X(64).
           05  FILLER              PIC X(18).

       FD  LEDGER-FILE.
       01  LEDGER-RECORD.
           05  LED-ACCOUNT         PIC X(12).
           05  LED-BALANCE         PIC S9(12)V99.
           05  FILLER              PIC X(66).

       FD  AUDIT-FILE.
       01  AUDIT-RECORD           PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-CONFIRM-STATUS      PIC XX.
       01  WS-LEDGER-STATUS       PIC XX.
       01  WS-AUDIT-STATUS        PIC XX.
       01  WS-EOF-FLAG            PIC X VALUE 'N'.
       01  WS-CONFIRMED-COUNT     PIC 9(6) VALUE ZERO.
       01  WS-REJECTED-COUNT      PIC 9(6) VALUE ZERO.
       
       01  WS-AUDIT-LINE.
           05  FILLER             PIC X(20) VALUE 'AUDIT REPORT - '.
           05  WS-AUDIT-TIMESTAMP PIC X(19).
           05  FILLER             PIC X(93) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-ROUTINE.
           PERFORM INITIALIZE-FILES
           PERFORM WRITE-AUDIT-HEADER
           PERFORM PROCESS-CONFIRMATIONS
           PERFORM WRITE-AUDIT-SUMMARY
           PERFORM CLOSE-FILES
           GOBACK.

       INITIALIZE-FILES.
           OPEN INPUT CONFIRM-FILE
           OPEN I-O LEDGER-FILE
           OPEN OUTPUT AUDIT-FILE.

       WRITE-AUDIT-HEADER.
           MOVE FUNCTION CURRENT-DATE TO WS-AUDIT-TIMESTAMP
           WRITE AUDIT-RECORD FROM WS-AUDIT-LINE.

       PROCESS-CONFIRMATIONS.
           READ CONFIRM-FILE
             AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ
           
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
             IF CONF-STATUS = 'CONFIRMED'
               PERFORM FINALIZE-TRANSACTION
               ADD 1 TO WS-CONFIRMED-COUNT
             ELSE
               PERFORM REJECT-TRANSACTION
               ADD 1 TO WS-REJECTED-COUNT
             END-IF
             READ CONFIRM-FILE
               AT END MOVE 'Y' TO WS-EOF-FLAG
             END-READ
           END-PERFORM.

       FINALIZE-TRANSACTION.
           STRING 'CONFIRMED: ' CONF-TRX-ID ' HASH: ' CONF-HASH
                  DELIMITED BY SIZE
                  INTO AUDIT-RECORD
           WRITE AUDIT-RECORD.

       REJECT-TRANSACTION.
           STRING 'REJECTED: ' CONF-TRX-ID ' STATUS: ' CONF-STATUS
                  DELIMITED BY SIZE
                  INTO AUDIT-RECORD
           WRITE AUDIT-RECORD.

       WRITE-AUDIT-SUMMARY.
           STRING 'SUMMARY - CONFIRMED: ' WS-CONFIRMED-COUNT
                  ' REJECTED: ' WS-REJECTED-COUNT
                  DELIMITED BY SIZE
                  INTO AUDIT-RECORD
           WRITE AUDIT-RECORD.

       CLOSE-FILES.
           CLOSE CONFIRM-FILE
           CLOSE LEDGER-FILE
           CLOSE AUDIT-FILE.
