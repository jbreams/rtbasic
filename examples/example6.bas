EXTERN printf(fmt AS STRING, ...)

LET x# = 45.5
LET y$ = "foo bar"
LET z = 8
DIM bizz(20, 21) AS INTEGER
DIM buzz AS DOUBLE

FOR i = 0 TO 10
bizz(i, i) = i
printf("Round %d\n", i)
NEXT

printf("Bizz: %d %d\n", bizz(2, 2), bizz(2, 3))

FOR i# = 0.0 TO 9.5
printf("Round %f\n", i#)
NEXT
GOTO exit

printf("Hello, World!\n")

exit:
printf("Goodbye, world!\n")
