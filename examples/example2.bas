10 EXTERN PRINTF(fmt AS STRING, ...)
40 FUNCTION FIB#(n AS DOUBLE)
50    FIB# = FIB#(n - 1.0) + FIB#(n - 2.0)
60 END
70 PRINTF("%d", FIB#(23.0))

