LET X = 5
EXTERN PRINTF(fmt AS STRING, ...)

gosubstart:
X = X + 7
PRINTF("X is now %", X)
gosubmiddle:
X = X - 2
RETURN

IF X > 0 THEN GOSUB gosubstart ELSE GOSUB gosubmiddle
