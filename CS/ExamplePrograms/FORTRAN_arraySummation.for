      PROGRAM MAIN
        PARAMETER (MAXSIZ=99)
        REAL A(MAXSIZ)
   10   READ (5,10,END=999) K
  100   FORMAT(I5)
          IF (K.LE.O.OR. K.GT.MAXSIZ) STOP
          READ *,(A(I),I=1,K)
          PRINT *,(A(I),I=1,K)
          PRINT *,'SUM',SUM(A,K)
          GO TO 10
  999   PRINT *,"All Done"
        STOP
        END
C SUMMATION SUBPROGRAM
      FUNCTION SUM(V,N)
        REAL :: V(N) ! New style declaration
        SUM = 0.0
        DO 20 I = 1,N
          SUM = SUM + V(I)
   20     CONTINUE
        RETURN
        END