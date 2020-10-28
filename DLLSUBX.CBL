CBL LIB,QUOTE,TEST(SYM)
      ***************************************************
      **                                               **
      ** DLLSUBX - DOUBLE LINKED LIST                  **
      **                                               **
      ** Basic double linked list functionality        **
      **                                               **
      **                                               **
      ***************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DLLSUBX.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAPID                  PIC S9(9) BINARY.
       01  DYNSTGS                 PIC S9(9) BINARY.
       01  FC.
           02  Condition-Token-Value.
           COPY  CEEIGZCT.
               03  Case-1-Condition-ID.
                   04  Severity    PIC S9(4) BINARY.
                   04  Msg-No      PIC S9(4) BINARY.
               03  Case-2-Condition-ID
                         REDEFINES Case-1-Condition-ID.
                   04  Class-Code  PIC S9(4) BINARY.
                   04  Cause-Code  PIC S9(4) BINARY.
               03  Case-Sev-Ctl    PIC X.
               03  Facility-ID     PIC XXX.
           02  I-S-Info            PIC S9(9) BINARY.

       01 WORK-CTL.
          05 FREE-NODE-PTR USAGE POINTER VALUE NULL.
          05 FREE-DATA-PTR USAGE POINTER VALUE NULL.
          05 DLL-NEW-PTR   USAGE POINTER VALUE NULL.
          05 WORK-A-PTR USAGE POINTER VALUE NULL.
          05 WORK-B-PTR USAGE POINTER VALUE NULL.
          05 WORK-POSITION PIC S9(9) BINARY.

       LINKAGE SECTION.

       COPY  DLLPARM.

       01 NEW-NODE.
          10 PREV-PTR USAGE POINTER.
          10 NEXT-PTR USAGE POINTER.
          10 DATA-PTR USAGE POINTER.
          10 DATA-LTH       PIC S9(9) BINARY.

       01 CURR-NODE.
          10 PREV-PTR USAGE POINTER.
          10 NEXT-PTR USAGE POINTER.
          10 DATA-PTR USAGE POINTER.
          10 DATA-LTH       PIC S9(9) BINARY.

       01 PREV-NODE.
          10 PREV-PTR USAGE POINTER.
          10 NEXT-PTR USAGE POINTER.
          10 DATA-PTR USAGE POINTER.
          10 DATA-LTH       PIC S9(9) BINARY.

       01 NEXT-NODE.
          10 PREV-PTR USAGE POINTER.
          10 NEXT-PTR USAGE POINTER.
          10 DATA-PTR USAGE POINTER.
          10 DATA-LTH       PIC S9(9) BINARY.

       01 DLL-FROM-BYTES    PIC X(32760).
       01 DLL-TO-BYTES      PIC X(32760).

       PROCEDURE DIVISION USING DLL-CONTROL.

           DISPLAY 'In DLLSUBX'.
           EVALUATE TRUE
              WHEN INSERT-FRONT
                  PERFORM DLL-INSERT-HEAD
              WHEN INSERT-END
                  PERFORM DLL-INSERT-TAIL
              WHEN INSERT-POS
                  PERFORM DLL-INSERT-POS
              WHEN DELETE-FRONT
                  PERFORM DLL-DELETE-HEAD
              WHEN DELETE-END
                  PERFORM DLL-DELETE-TAIL
              WHEN DELETE-POS
                  PERFORM DLL-DELETE-POS
              WHEN FREE-ALL
                  PERFORM DLL-FREE-ALL
              WHEN OTHER
                  DISPLAY PARM-REQUEST ' IS INVALID '
           END-EVALUATE.
           GOBACK.


       DLL-INSERT-HEAD.
           PERFORM DLL-ALLOC-NEW-NODE.
           IF SUCCESSFUL
              IF DLL-HEAD-PTR = NULL
                 SET DLL-HEAD-PTR TO DLL-NEW-PTR
                 SET DLL-TAIL-PTR TO DLL-NEW-PTR
              ELSE
                 SET ADDRESS OF CURR-NODE TO DLL-HEAD-PTR
                 SET NEXT-PTR OF NEW-NODE TO DLL-HEAD-PTR
                 SET PREV-PTR OF CURR-NODE TO DLL-NEW-PTR
                 SET DLL-HEAD-PTR TO DLL-NEW-PTR
              END-IF
              ADD 1 TO DLL-NODE-COUNT
           END-IF
           EXIT.

       DLL-INSERT-TAIL.
           PERFORM DLL-ALLOC-NEW-NODE.
           IF SUCCESSFUL
              IF DLL-TAIL-PTR = NULL
                 SET DLL-TAIL-PTR TO DLL-NEW-PTR
                 SET DLL-HEAD-PTR TO DLL-NEW-PTR
              ELSE
                 SET ADDRESS OF CURR-NODE TO DLL-TAIL-PTR
                 SET PREV-PTR OF NEW-NODE TO DLL-TAIL-PTR
                 SET NEXT-PTR OF CURR-NODE TO DLL-NEW-PTR
                 SET DLL-TAIL-PTR TO DLL-NEW-PTR
              END-IF
              ADD 1 TO DLL-NODE-COUNT
           END-IF
           EXIT.

       DLL-INSERT-POS.
           IF PARM-POSITION > 0 AND PARM-POSITION <= DLL-NODE-COUNT
              IF PARM-POSITION = 1
                 PERFORM DLL-INSERT-HEAD
              ELSE IF PARM-POSITION = DLL-NODE-COUNT
                 PERFORM DLL-INSERT-TAIL
              ELSE
                 PERFORM DLL-INSERT-MIDDLE
           END-IF
           EXIT.

       DLL-INSERT-MIDDLE.
           SET ADDRESS OF CURR-NODE TO DLL-HEAD-PTR
           PERFORM DLL-ALLOC-NEW-NODE
           IF SUCCESSFUL
              MOVE 1 TO WORK-POSITION
              PERFORM UNTIL WORK-POSITION = PARM-POSITION
                 SET ADDRESS OF CURR-NODE TO
                    NEXT-PTR OF CURR-NODE
                 SET ADDRESS OF PREV-NODE TO
                    PREV-PTR OF CURR-NODE
                 ADD 1 TO WORK-POSITION
              END-PERFORM

              SET NEXT-PTR OF PREV-NODE TO DLL-NEW-PTR
              SET PREV-PTR OF CURR-NODE TO DLL-NEW-PTR

              SET PREV-PTR OF NEW-NODE TO
                 ADDRESS OF PREV-NODE

              SET NEXT-PTR OF NEW-NODE TO
                 ADDRESS OF CURR-NODE

              ADD 1 TO DLL-NODE-COUNT

           END-IF
           EXIT.

       DLL-DELETE-HEAD.
           IF DLL-HEAD-PTR NOT = NULL
              SET ADDRESS OF CURR-NODE TO DLL-HEAD-PTR
              SET FREE-NODE-PTR TO DLL-HEAD-PTR
              SET FREE-DATA-PTR TO DATA-PTR OF CURR-NODE
              SET WORK-A-PTR TO NEXT-PTR OF CURR-NODE
              PERFORM DLL-FREE-NODE
              SET DLL-HEAD-PTR TO WORK-A-PTR
              IF DLL-HEAD-PTR = NULL
                 SET DLL-TAIL-PTR TO DLL-HEAD-PTR
              ELSE
                 SET ADDRESS OF CURR-NODE TO DLL-HEAD-PTR
                 SET PREV-PTR OF CURR-NODE TO NULLS
              END-IF
              SUBTRACT 1 FROM DLL-NODE-COUNT
           END-IF.
           EXIT.

       DLL-DELETE-TAIL.
           IF DLL-TAIL-PTR NOT = NULL
              SET ADDRESS OF CURR-NODE TO DLL-TAIL-PTR
              SET FREE-NODE-PTR TO DLL-TAIL-PTR
              SET FREE-DATA-PTR TO DATA-PTR OF CURR-NODE
              SET WORK-A-PTR TO PREV-PTR OF CURR-NODE
              PERFORM DLL-FREE-NODE
              SET DLL-TAIL-PTR TO WORK-A-PTR
              IF DLL-TAIL-PTR = NULL
                 SET DLL-HEAD-PTR TO DLL-TAIL-PTR
              ELSE
                 SET ADDRESS OF CURR-NODE TO DLL-TAIL-PTR
                 SET NEXT-PTR OF CURR-NODE TO NULLS
              END-IF
           END-IF
           SUBTRACT 1 FROM DLL-NODE-COUNT
           EXIT.

       DLL-DELETE-POS.
           IF PARM-POSITION > 0 AND PARM-POSITION <= DLL-NODE-COUNT
              IF PARM-POSITION = 1
                 PERFORM DLL-DELETE-HEAD
              ELSE IF PARM-POSITION = DLL-NODE-COUNT
                 PERFORM DLL-DELETE-TAIL
              ELSE
                 PERFORM DLL-DELETE-MIDDLE
           END-IF
           EXIT.

       DLL-DELETE-MIDDLE.
           SET ADDRESS OF CURR-NODE TO DLL-HEAD-PTR
           MOVE 1 TO WORK-POSITION
           PERFORM UNTIL WORK-POSITION = PARM-POSITION
              SET ADDRESS OF CURR-NODE TO
                 NEXT-PTR OF CURR-NODE
              SET ADDRESS OF PREV-NODE TO
                 PREV-PTR OF CURR-NODE
              SET ADDRESS OF NEXT-NODE TO
                 NEXT-PTR OF CURR-NODE
              ADD 1 TO WORK-POSITION
           END-PERFORM
           SET NEXT-PTR OF PREV-NODE TO
              ADDRESS OF NEXT-NODE
           SET PREV-PTR OF NEXT-NODE TO
              ADDRESS OF PREV-NODE
           SET FREE-NODE-PTR TO ADDRESS OF CURR-NODE
           SET FREE-DATA-PTR TO DATA-PTR OF CURR-NODE
           PERFORM DLL-FREE-NODE
           SUBTRACT 1 FROM DLL-NODE-COUNT
           EXIT.

       DLL-FREE-ALL.
           PERFORM UNTIL DLL-NODE-COUNT = 0
              PERFORM DLL-DELETE-HEAD
           END-PERFORM
           EXIT.

      * allocate node and data *
       DLL-ALLOC-NEW-NODE.
           MOVE 0 TO HEAPID.
           MOVE LENGTH OF NEW-NODE TO DYNSTGS.
           SET DLL-NEW-PTR TO NULL.
           PERFORM DLL-ALLOC-NODE.
           EXIT.

       DLL-ALLOC-NODE.

           SET SUCCESSFUL TO TRUE

           CALL "CEEGTST" USING HEAPID, DYNSTGS,
                                DLL-NEW-PTR , FC

           IF CEE000 of FC
               DISPLAY "DLL node " DYNSTGS " bytes of"
                   " storage at location " DLL-NEW-PTR
                   " from heap number " HEAPID
               SET ADDRESS OF NEW-NODE TO DLL-NEW-PTR
               MOVE PARM-DATA-LTH TO
                  DATA-LTH OF NEW-NODE
               SET NEXT-PTR OF NEW-NODE TO NULL
               SET PREV-PTR OF NEW-NODE TO NULL
               SET DATA-PTR OF NEW-NODE TO NULL
               PERFORM DLL-ALLOC-NODE-DATA
           ELSE
               DISPLAY "CEEGTST failed with msg "
                   Msg-No of FC
               MOVE 'ERROR' to DLLSUBX-MSG
               MOVE severity to DLLSUBX-RC
           END-IF.

           EXIT.

       DLL-ALLOC-NODE-DATA.

           SET SUCCESSFUL TO TRUE

           MOVE DATA-LTH OF NEW-NODE TO DYNSTGS
           CALL "CEEGTST" USING HEAPID, DYNSTGS,
                                DATA-PTR OF NEW-NODE, FC
           IF CEE000 of FC
               DISPLAY "DLL data " DYNSTGS " bytes of"
                   " storage at location " DATA-PTR OF NEW-NODE
                   " from heap number " HEAPID
               SET ADDRESS OF DLL-FROM-BYTES TO
                   PARM-DATA-PTR
               SET ADDRESS OF DLL-TO-BYTES TO
                   DATA-PTR OF NEW-NODE
               MOVE LOW-VALUES TO DLL-TO-BYTES(1:DYNSTGS)
               MOVE DLL-FROM-BYTES TO
                   DLL-TO-BYTES(1:DYNSTGS)
           ELSE
               DISPLAY "CEEGTST failed with msg "
                   Msg-No of FC
               MOVE 'ERROR' to DLLSUBX-MSG
               MOVE severity to DLLSUBX-RC
           END-IF
           EXIT.

       DLL-FREE-NODE.
           DISPLAY "FREE-NODE " FREE-NODE-PTR.
           PERFORM DLL-FREE-NODE-DATA.
           CALL "CEEFRST" USING FREE-NODE-PTR, FC.
           EXIT.

       DLL-FREE-NODE-DATA.
           DISPLAY "FREE-DATA " FREE-DATA-PTR.
           CALL "CEEFRST" USING FREE-DATA-PTR, FC.
           EXIT.

