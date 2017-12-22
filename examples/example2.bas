10 EXTERN PRINTF(fmt AS STRING, ...)

20 LET x = 6
30 LET y = "Hello, World!\n"

40 FUNCTION FIB(n AS DOUBLE)
50    FIB = FIB(n - 1) + FIB(n - 2)
60 END

FUNCTION HELLO$()
    HELLO = "Hello, World!\n"
END

70 PRINTF("%d %s", FIB(23), HELLO())

