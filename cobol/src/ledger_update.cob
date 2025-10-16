       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGERUPD.
       AUTHOR. HOGAN-CRYPTO-POC.
       DATE-WRITTEN. 2024.
      *
      * Ledger Update Program
      * Updates ledger entries and generates work CSV
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANX-FILE ASSIGN TO "TRANXIN"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-TRANX-STATUS.
           
           SELECT LEDGER-FILE ASSIGN TO "LEDGER"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-LEDGER-STATUS.
           
           SELECT WORK-FILE ASSIGN TO "WORKFILE"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-WORK-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANX-FILE.
       01  TRANX-RECORD.
           05  TRX-ID              PIC X(10).
           05  TRX-AMOUNT          PIC 9(10)V99.
           05  TRX-ACCOUNT         PIC X(12).
           05  FILLER              PIC X(56).

       FD  LEDGER-FILE.
       01  LEDGER-RECORD.
           05  LED-ACCOUNT         PIC X(12).
           05  LED-BALANCE         PIC S9(12)V99.
           05  FILLER              PIC X(66).

       FD  WORK-FILE.
       01  WORK-RECORD            PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-TRANX-STATUS        PIC XX.
       01  WS-LEDGER-STATUS       PIC XX.
       01  WS-WORK-STATUS         PIC XX.
       01  WS-EOF-FLAG            PIC X VALUE 'N'.
       01  WS-RECORD-COUNT        PIC 9(6) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-ROUTINE.
           PERFORM INITIALIZE-FILES
           PERFORM PROCESS-TRANSACTIONS
           PERFORM CLOSE-FILES
           GOBACK.

       INITIALIZE-FILES.
           OPEN INPUT TRANX-FILE
           OPEN I-O LEDGER-FILE
           OPEN OUTPUT WORK-FILE.

       PROCESS-TRANSACTIONS.
           READ TRANX-FILE
             AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ
           
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
             PERFORM UPDATE-LEDGER
             PERFORM WRITE-WORK-RECORD
             ADD 1 TO WS-RECORD-COUNT
             READ TRANX-FILE
               AT END MOVE 'Y' TO WS-EOF-FLAG
             END-READ
           END-PERFORM.

       UPDATE-LEDGER.
           DISPLAY 'Processing transaction: ' TRX-ID.

       WRITE-WORK-RECORD.
           STRING TRX-ID ',' TRX-AMOUNT ',' TRX-ACCOUNT
                  DELIMITED BY SIZE
                  INTO WORK-RECORD
           WRITE WORK-RECORD.

       CLOSE-FILES.
           CLOSE TRANX-FILE
           CLOSE LEDGER-FILE  
           CLOSE WORK-FILE.
