EXTERN printf(fmt$, ...)

DIM x

x = 0

WHILE x >< 5
    printf("Reached %d\n", x)
    x = x + 1
WEND

DO
    printf("Reached %d\n", x)
    x = x - 1
LOOP UNTIL x = 0

IF x = 0 THEN
    printf("Everything works!\n")
ELSE
    printf("Woopsy!\n")
END
