# COBOL-ReadandWritewithIF
First COBOL program

      *COMMENT LINE...  APPEARS LIKE THIS ... MAKES IT STAND OUT !     *00010059
      ******************************************************************00020059
      * AUTHOR: INSTRUCTOR                                             *00030059
      * DATE: SEMESTER 2                                               *00040059
      * DESCRIPTION: READ RECORDS FROM INPUT TRANSACTION FILE AND PRINT*00050059
      * SYSTEM:      REPORTING                                         *00060059
      * PROGRAM ID:  C0910182 (ORIGINAL)                               *00070061
      * PROGRAM ID:  C0910182                                          *00080061
      ******************************************************************00090059
      * MODIFICATIONS:                                                 *00100059
      ****  DATE *****  PROGRAMMER *****RATIONAL************************00110059
      * 07/11/15 JORDAN THOMPSON CHANGED REPORT TITLE                  *00120077
      * 07/11/15 JORDAN THOMPSON CHANGED DATE FORMAT                   *00130077
      * 07/11/15 JORDAN THOMPSON CHANGED REVIEW COMMENT LAYOUT         *00140077
      * 07/11/15 JORDAN THOMPSON INCLUDED PROGRAMMER NAME      *        00150077
      * 11/11/15 JORDAN THOMPSON CHANGED IF STATEMENT                 * 00160079
      * 11/11/15 JORDAN THOMPSON ADDED CALCULATION PROCEDURE-DNW       *00170077
      * 11/11/15 JORDAN THOMPSON ADDED COLHEAD4 FOR AFTER DATA         *00180079
      * 11/11/15 JORDAN THOMPSON TRIED TO RECIEVE FILE TEXT-DNW        *00210077
      * 11/11/15 JORDAN THOMPSON ADDED REC COUNT AND TOT SALARY      *  00220079
      ******************************************************************00230059
      *                                                                 00240059
      ******************************************************************00250059
      *     THE IDENTIFICATION DIVISION SECTION ... FOLLOWS            *00260059
      ******************************************************************00270059
       IDENTIFICATION DIVISION.                                         00280059
       PROGRAM-ID C0910182.                                             00290061
       ENVIRONMENT DIVISION.                                            00300059
       INPUT-OUTPUT SECTION.                                            00310059
       FILE-CONTROL.                                                    00320059
           SELECT I-TRANSACTION-FILE ASSIGN TO ITRANS.                  00330059
           SELECT O-REPORT-FILE      ASSIGN TO OREPORT.                 00340059
       DATA DIVISION.                                                   00350059
       FILE SECTION.                                                    00360059
       FD I-TRANSACTION-FILE                                            00370059
             RECORDING MODE IS F.                                       00380059
       01 ITRANS-RECORD.                                                00390059
           05  I-EMPLOYEE-ID            PIC X(6).                       00400059
           05  I-FIRST-NAME             PIC X(10).                      00410059
           05  I-SURNAME                PIC X(15).                      00420059
           05  I-TITLES                 PIC X(3) .                      00430059
           05  I-SALARY                 PIC 9(5)V99.                    00440059
           05  I-LAST-REVIEW-DATE       PIC X(8) .                      00450059
           05  I-LAST-REVIEW-COMMENTS   PIC X(20).                      00460059
           05  I-TRAN-CODE              PIC X .                         00470059
               88  I-TRAN-A             VALUE 'A'.                      00480059
               88  I-TRAN-C             VALUE 'C'.                      00490059
               88  I-TRAN-D             VALUE 'D'.                      00500059
           05                           PIC X(10).                      00510059
       FD O-REPORT-FILE                                                 00520059
             RECORDING MODE IS F.                                       00530059
       01 REPORT-OUTPUT                 PIC X(100).                     00540059
       WORKING-STORAGE SECTION.                                         00550059
      * ********************************************************        00560059
      * *   DECLARING THE HEADING LINES                       *         00570059
      * *******************************************************         00580059
       01  WR-COLHEAD0.                                                 00590059
            05  FILLER        PIC X       VALUE SPACES.                 00600059
            05  FILLER        PIC X(20)   VALUE 'PROGRAM :C0910182   '. 00610061
            05  FILLER        PIC X(6)    VALUE SPACES.                 00620072
            05  FILLER        PIC X(20)   VALUE 'INSERT TRANSACTIONS '. 00630067
            05  FILLER        PIC X(20)   VALUE 'REPORT FOR MR TITLES'. 00631064
            05  FILLER        PIC X(05)   VALUE SPACES.                 00640059
            05  FILLER        PIC X(5)    VALUE 'PAGE:'.                00650076
            05  WR-PAGE-COUNT PIC Z9(02).                               00660059
            05  FILLER        PIC X(8)    VALUE '  DATE: '.             00670059
            05  WR-TODAY-DAY  PIC X(2).                                 00681068
            05  FILLER        PIC X(1)    VALUE '/'.                    00681168
            05  WR-TODAY-MON  PIC X(2).                                 00682068
            05  FILLER        PIC X(1)    VALUE '/'.                    00682168
            05  WR-TODAY-YEAR PIC X(2).                                 00683068
            05  FILLER        PIC X(8)    VALUE SPACES.                 00690059
       01  WR-COLHEADP.                                                 00700072
               05  FILLER     PIC X        VALUE SPACES.                00710059
               05  FILLER     PIC X(16)    VALUE 'PROGRAMMER NAME:'.    00711072
               05  FILLER     PIC X(7)     VALUE 'JORDAN '.             00712072
               05  FILLER     PIC X(8)     VALUE 'THOMPSON'.            00713072
       01  WR-COLHEAD1.                                                 00713172
               05  FILLER     PIC X        VALUE SPACES.                00714072
               05  FILLER     PIC X(12)    VALUE 'TRAN    NAME'.        00720059
               05  FILLER     PIC X(5)     VALUE SPACES.                00730059
               05  FILLER     PIC X(10)    VALUE 'NAME'.                00740059
               05  FILLER     PIC X(8)     VALUE SPACES.                00750059
               05  FILLER     PIC X(5)     VALUE 'TITLE'.               00760059
               05  FILLER     PIC X        VALUE SPACES.                00770059
               05  FILLER     PIC X(8)     VALUE 'EMPLOYEE'.            00780059
               05  FILLER     PIC X(4)     VALUE SPACES.                00790059
               05  FILLER     PIC X(6)     VALUE 'SALARY'.              00800059
               05  FILLER     PIC X(4)     VALUE SPACES.                00810059
               05  FILLER     PIC X(8)     VALUE 'REVIEW'.              00820059
               05  FILLER     PIC X(2)     VALUE SPACES.                00830059
               05  FILLER     PIC X(6)     VALUE 'REVIEW'.              00840059
               05 FILLER      PIC X(20)     VALUE SPACES.               00850072
       01  WR-COLHEAD2.                                                 00860059
                  05  FILLER        PIC X        VALUE SPACES.          00870077
                  05  FILLER        PIC X(4)    VALUE 'TYPE'.           00880077
                  05  FILLER        PIC X(2)     VALUE SPACES.          00890077
                  05  FILLER        PIC X(7)     VALUE 'SURNAME'.       00900077
                  05  FILLER        PIC X(6)    VALUE SPACES.           00910077
                  05  FILLER        PIC X(5)    VALUE 'FIRST'.          00920077
                  05  FILLER        PIC X(17)    VALUE SPACES.          00930077
                  05  FILLER        PIC X(6)     VALUE 'ID'.            00940077
                  05  FILLER        PIC X(17)    VALUE SPACES.          00950077
                  05  FILLER        PIC X(4)    VALUE 'DATE'.           00960077
                  05  FILLER        PIC X       VALUE SPACES.           00970077
       01  WR-COLHEAD3.                                                 00980059
                  05  FILLER  PIC X VALUE ' '.                          00990059
       01  WR-PRETTY.                                                   01000059
                  05  FILLER  PIC X(4) VALUE '-**-'  OCCURS 20 TIMES.   01010059
       01  WR-COLHEAD4.                                                 01011077
                  05  FILLER        PIC X       VALUE SPACES.           01012079
                  05  FILLER        PIC X       VALUE SPACES.           01012179
                  05  FILLER        PIC X       VALUE SPACES.           01012279
                  05  FILLER        PIC X(12)    VALUE 'TOTAL RECORD'.  01013077
                  05  FILLER        PIC X(6)     VALUE 'COUNT:'.        01013177
                  05 WR-REC-COUNT   PIC ZZ.                             01013278
                  05  FILLER        PIC X(6)     VALUE SPACES.          01014077
                  05  FILLER        PIC X(7)     VALUE 'OVERALL'.       01015077
                  05  FILLER        PIC X(7)     VALUE ' SALARY'.       01017077
                  05 WR-TOT-SALARY  PIC ZZZ,ZZZ.99.                     01017178
                  05  FILLER        PIC X(17)    VALUE SPACES.          01018077
      * ******************************************************          01020059
      * *   DECLARING THE OUTPUT DETAIL LINES                 *         01030059
      * *******************************************************         01040059
       01  WR-REPORT1.                                                  01050059
               05  FILLER           PIC X        VALUE SPACES.          01060077
               05  WR-TRAN          PIC X        VALUE SPACES.          01070077
               05  FILLER           PIC X(5)     VALUE SPACES.          01080077
               05  WR-SURNAME       PIC X(10)    VALUE SPACES.          01090077
               05  FILLER           PIC X(1)     VALUE SPACES.          01100077
               05  WR-FIRST-NAME    PIC X(15)    VALUE SPACES.          01110077
               05  FILLER           PIC X(3)     VALUE SPACES.          01120077
               05  WR-TITLE         PIC X(3)     VALUE SPACES.          01130078
               05  FILLER           PIC XXX      VALUE SPACES.          01140077
               05  WR-EMPLOYEE-ID   PIC X(6)     VALUE SPACES.          01150077
               05  FILLER           PIC XXX      VALUE SPACES.          01160077
               05  WR-SALARY        PIC 9(5).99  VALUE ZEROS.           01170077
               05  FILLER           PIC X(2)     VALUE SPACES.          01180077
               05  WR-LAST-REV-DATE PIC X(8)     VALUE SPACES.          01190077
               05  FILLER           PIC X        VALUE SPACES.          01200077
               05  WR-LAST-REVIEW-COMMENTS PIC X(10) VALUE SPACES.      01210077
      * THIS IS MY VARIABLE FIELD DEFINITION SECTION                    01220059
      *                                                                 01230059
       01 WS-COUNTERS.                                                  01240059
            05 WS-PAGE-COUNT       PIC 9(02) VALUE 1.                   01250059
            05 WS-LINE-COUNT       PIC 9(03) VALUE 1.                   01260059
            05 WS-PGLN-COUNT       PIC 9(03) VALUE 0.                   01270059
      *                                                                 01280059
       01 WS-COUNTVAR.                                                  01281078
            05 WS-REC-COUNT       PIC 9(02) VALUE 0.                    01282078
            05 WS-TOT-SALARY      PIC 9(07)V99 VALUE 0.                 01283078
       


            01 TODAY-DATE.                                              01290059
            03 WS-YEAR PIC 99.                                          01300059
            03 WS-MONTH PIC 99.                                         01310059
            03  WS-DAY PIC 99.                                          01320059
      * THIS IS MY EOF SWITCH                                           01330059
       01  FLAG-EOF  PIC 9 VALUE ZEROS.                                 01340059
      * THIS IS MY MAIN PROCESSING LOGIC                                01350059
       PROCEDURE DIVISION.                                              01360059
            DISPLAY 'START THE PROGRAM'.                                01370059
            ACCEPT TODAY-DATE FROM DATE.                                01380059
            MOVE WS-DAY     TO WR-TODAY-DAY.                            01391068
            MOVE WS-MONTH   TO WR-TODAY-MON.                            01392068
            MOVE WS-YEAR    TO WR-TODAY-YEAR.                           01393068
            PERFORM 100-BEGIN.                                          01400059
            PERFORM 200-MAIN-PROCESS UNTIL FLAG-EOF EQUAL 1.            01410059
            PERFORM 300-FINISH.                                         01420059
            STOP RUN.                                                   01430059
       100-BEGIN.                                                       01440059
            OPEN INPUT  I-TRANSACTION-FILE.                             01450059
            OPEN OUTPUT O-REPORT-FILE.                                  01460059
               MOVE WS-PAGE-COUNT TO WR-PAGE-COUNT.                     01470059
            WRITE REPORT-OUTPUT FROM WR-COLHEADP AFTER PAGE.            01471072
            WRITE REPORT-OUTPUT FROM WR-COLHEAD0.                       01480077
            WRITE REPORT-OUTPUT FROM WR-COLHEAD1.                       01490077
            WRITE REPORT-OUTPUT FROM WR-COLHEAD2.                       01500059
            WRITE REPORT-OUTPUT FROM WR-COLHEAD3.                       01510059
            WRITE REPORT-OUTPUT FROM WR-PRETTY AFTER 2.                 01520059
            READ I-TRANSACTION-FILE                                     01530059
               AT END                                                   01540059
                     DISPLAY 'EOF - NO RECORDS IN FILE'                 01550059
                     MOVE 1 TO FLAG-EOF.                                01560059
      * **************************************************************  01570059
      * *   THIS OUR MAIN LOOP                                          01580059
      * *                                                               01590059
      * **************************************************************  01600059
       200-MAIN-PROCESS.                                                01610059
              IF WS-LINE-COUNT = 25                                     01620076
               ADD 1 TO WS-PAGE-COUNT                                   01630059
               MOVE 0 TO WS-LINE-COUNT                                  01640059
               MOVE WS-PAGE-COUNT TO WR-PAGE-COUNT                      01650059
               WRITE REPORT-OUTPUT FROM WR-COLHEADP AFTER PAGE          01651076
               WRITE REPORT-OUTPUT FROM WR-COLHEAD0                     01660077
               WRITE REPORT-OUTPUT FROM WR-COLHEAD1                     01670059
               WRITE REPORT-OUTPUT FROM WR-COLHEAD2                     01680059
               WRITE REPORT-OUTPUT FROM WR-COLHEAD3                     01690059
               WRITE REPORT-OUTPUT FROM WR-COLHEAD4                     01691077
               WRITE REPORT-OUTPUT FROM WR-PRETTY AFTER 2               01700072
             END-IF                                                     01700178
             IF I-TITLES = 'MR'                                         01701077
              MOVE I-EMPLOYEE-ID TO WR-EMPLOYEE-ID                      01710077
              MOVE I-TRAN-CODE TO WR-TRAN                               01720077
              MOVE I-LAST-REVIEW-DATE TO WR-LAST-REV-DATE               01730077
              MOVE I-FIRST-NAME TO WR-FIRST-NAME                        01740077
              MOVE I-SURNAME TO WR-SURNAME                              01750077
              MOVE I-TITLES TO WR-TITLE                                 01760077
              MOVE I-SALARY TO WR-SALARY                                01770077
              MOVE I-LAST-REVIEW-COMMENTS TO WR-LAST-REVIEW-COMMENTS    01780077
      * *TRIED END-IF FOR BOTH IF STATEMENTS*************************   01781077
            WRITE REPORT-OUTPUT FROM WR-REPORT1                         01790077
            ADD 1 TO WS-LINE-COUNT                                      01800078
             ADD 1 TO WS-REC-COUNT                                      01800178
              ADD I-SALARY TO WS-TOT-SALARY                             01800278
            END-IF                                                      01801078
            READ I-TRANSACTION-FILE                                     01810059
              AT END                                                    01820059
                 MOVE 1 TO FLAG-EOF.                                    01830059
                                                                        01834077
       300-FINISH.                                                      01840059
            MOVE WS-REC-COUNT TO WR-REC-COUNT.                          01840178
            MOVE WS-TOT-SALARY TO WR-TOT-SALARY.                        01840278
            WRITE REPORT-OUTPUT FROM WR-COLHEAD4.                       01841078
            CLOSE I-TRANSACTION-FILE.                                   01850059
            CLOSE O-REPORT-FILE.                                        01860059
 
