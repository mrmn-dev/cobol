CBL LIB,QUOTE,TEST(SYM)
      ***************************************************
      **                                               **
      ** DLLMAIN - DOUBLE LINKED LIST MAIN             **
      **                                               **
      ** TEST DLLSUBX linked list functionality        **
      **                                               **
      **                                               **
      ***************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DLLMAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTDATA           PIC X(500) VALUE 'TEST DLLSUBX'.
       77 LOOP-COUNT         PIC S9(9) BINARY VALUE 1.

      * DLLPARM IS USED TO CALL DLLSUBX
       COPY DLLPARM.

       LINKAGE SECTION.

       PROCEDURE DIVISION.
      *
           DISPLAY 'In DLLMAIN'.
      *
           SET PARM-DATA-PTR TO ADDRESS OF TESTDATA.
           COMPUTE PARM-DATA-LTH = FUNCTION LENGTH(TESTDATA).
           SET INSERT-FRONT TO TRUE.
           CALL 'DLLSUBX' USING DLL-CONTROL.
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           SET INSERT-FRONT TO TRUE.
           PERFORM DLL-SUBX-CALL 100 TIMES.
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           SET FREE-ALL TO TRUE.
           CALL 'DLLSUBX' USING DLL-CONTROL.
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           SET INSERT-END  TO TRUE.
           PERFORM WITH TEST AFTER
              UNTIL LOOP-COUNT > 110
              CALL 'DLLSUBX' USING DLL-CONTROL
              ADD 1 TO LOOP-COUNT
           END-PERFORM.
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           SET FREE-ALL TO TRUE.
           CALL 'DLLSUBX' USING DLL-CONTROL.
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           SET INSERT-END  TO TRUE.
           MOVE 1 TO LOOP-COUNT.
           PERFORM WITH TEST AFTER
              UNTIL LOOP-COUNT > 10
              CALL 'DLLSUBX' USING DLL-CONTROL
              ADD 1 TO LOOP-COUNT
           END-PERFORM.
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           SET DELETE-END  TO TRUE.
           CALL 'DLLSUBX' USING DLL-CONTROL.
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           SET INSERT-POS TO TRUE.
           MOVE 5 TO PARM-POSITION.
           MOVE 1 TO LOOP-COUNT.
           PERFORM WITH TEST AFTER
              UNTIL LOOP-COUNT > 3
              CALL 'DLLSUBX' USING DLL-CONTROL
              ADD 1 TO LOOP-COUNT
           END-PERFORM.
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           SET DELETE-POS TO TRUE.
           MOVE 4 TO PARM-POSITION.
           CALL 'DLLSUBX' USING DLL-CONTROL
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           SET FREE-ALL TO TRUE.
           CALL 'DLLSUBX' USING DLL-CONTROL.
           DISPLAY 'DLL-NODE-COUNT: ' DLL-NODE-COUNT.
      *
           STOP RUN.
		   
           DLL-SUBX-CALL.                                
		       SET PARM-DATA-PTR TO ADDRESS OF TESTDATA. 
		       MOVE 500 TO PARM-DATA-LTH.                
		       SET INSERT-FRONT TO TRUE.                 
		       CALL 'DLLSUBX' USING DLL-CONTROL.         
		   EXIT.                                     
		   										
		   EXIT PROGRAM.