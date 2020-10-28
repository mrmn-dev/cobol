       01 DLL-CONTROL.
          05 PARM-REQUEST    PIC X(1).
             88 INSERT-FRONT VALUE 'A'.
             88 INSERT-END   VALUE 'B'.
             88 INSERT-POS   VALUE 'C'.
             88 DELETE-FRONT VALUE '1'.
             88 DELETE-END   VALUE '2'.
             88 DELETE-POS   VALUE '3'.
             88 FREE-ALL     VALUE '4'.
          05 PARM-DATA-PTR   USAGE POINTER.
          05 PARM-DATA-LTH   PIC S9(9) BINARY.
          05 PARM-POSITION   PIC S9(9) BINARY.
          05 DLLSUBX-RC      PIC S9(9) BINARY.
             88 SUCCESSFUL   VALUE 0.
             88 FAILED       VALUE 12.
          05 DLLSUBX-MSG     PIC X(32).
          05 DLLSUBX-CTL.
             10 DLL-HEAD-PTR    USAGE POINTER VALUE NULL.
             10 DLL-TAIL-PTR    USAGE POINTER VALUE NULL.
             10 DLL-NODE-COUNT  PIC S9(9) BINARY VALUE 0.
