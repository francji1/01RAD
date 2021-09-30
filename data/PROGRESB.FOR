CC     ------------------------------------------------------------------------
CC     *JNTGR*: SUBROUTINE FOR TRANSFORMING A CHARACTER INTO AN INTEGER
CC     ------------------------------------------------------------------------
       FUNCTION JNTGR(KAR)
       CHARACTER*1 KAR
       IF (KAR.EQ.'0') JNTGR=0
       IF (KAR.EQ.'1') JNTGR=1
       IF (KAR.EQ.'2') JNTGR=2
       IF (KAR.EQ.'3') JNTGR=3
       RETURN
       END
CC     ------------------------------------------------------------------------
CC     *SMISSING*: SUBROUTINE FOR HANDLING MISSING VALUES
CC     ------------------------------------------------------------------------
       SUBROUTINE SMISSING(NVAR,NCAS,MAXP1,MAXN,JCST,X,Y,AW,JNDEX,
     1 MVAL,JMISS,JSUBS,VALMS,NSTOP,LAB,JPRT,LUB)
       DIMENSION X(MAXP1,MAXN),Y(MAXN),AW(MAXN)
       DIMENSION VALMS(MAXP1)
       INTEGER JNDEX(MAXN),JSUBS(MAXP1),JMISS(MAXP1)
       CHARACTER*10 LAB(MAXP1)
       NVAD=NVAR+1
       JHALT=0
       JHLT=0
       MAXM=(NCAS*4)/5
       DO 10 J=1,NVAD
       JSUBS(J)=0
       IF (.NOT.((J.EQ.NVAR.AND.JCST.EQ.1).OR.(JMISS(J).EQ.0)))THEN
	  DO 20 JNC=1,NCAS
	  IF (X(J,JNC).EQ.VALMS(J)) THEN
	     JSUBS(J)=JSUBS(J)+1
	     JNDEX(JNC)=0
	  ENDIF
 20	  CONTINUE
	  IF (JSUBS(J).GT.MAXM) JHALT=JHALT+1
	  IF (MVAL.EQ.1.AND.JPRT.NE.0) WRITE(LUB,8000) LAB(J),JSUBS(J)
       ENDIF
 10    CONTINUE
       IF(JHALT.EQ.0) GOTO 30
       WRITE(LUB,8010) JHALT,MAXM
       NSTOP=1
       RETURN
 30    NCASM=NCAS
       MAXM=NCAS-MAXM
       JL=0
       JHALT=0
       DO 50 JNC=1,NCASM
       IF (JNDEX(JNC).NE.0) GOTO 65
       IF (JHALT.EQ.1.OR.MVAL.EQ.2) GOTO 60
       IF (JPRT.NE.0) WRITE(LUB,8020) NVAD
       JHALT=1
 60    JTVAR=0
       DO 70 J=1,NVAD
       IF(((JCST.EQ.1.AND.J.EQ.NVAR).OR.(JMISS(J).EQ.0)).OR.
     1 (X(J,JNC).NE.VALMS(J))) GOTO 70
       JTVAR=JTVAR+1
       JSUBS(JTVAR)=J
 70    CONTINUE
       IF (JTVAR.EQ.0) GOTO 65
       IF (MVAL.EQ.1.OR.JHLT.EQ.1) GOTO 80
       IF (JPRT.NE.0) WRITE(LUB,8030)
       JHLT=1
 80    IF (MVAL.EQ.1.AND.JPRT.NE.0) THEN
	  IF (JTVAR.GT.10) GOTO 90
	  WRITE(LUB,8040) JNC,(JSUBS(J),J=1,JTVAR)
	  GOTO 100
 90	  WRITE(LUB,8040) JNC,(JSUBS(J),J=1,10)
	  WRITE(LUB,8050) (JSUBS(J),J=11,JTVAR)
       ENDIF
 100   IF (MVAL.EQ.2.AND.X(NVAD,JNC).EQ.VALMS(NVAD)) THEN
	  IF (JPRT.NE.0) WRITE(LUB,8060) JNC
	  JTVAR=JTVAR-1
       ENDIF
       IF (JCST.EQ.1) JTVAR=JTVAR+1
       IF(MVAL.EQ.2.AND.JTVAR.EQ.NVAR.AND.JPRT.NE.0) WRITE(LUB,8070) JNC
       IF (.NOT.(MVAL.EQ.2.AND.JTVAR.LT.NVAR.AND.X(NVAD,JNC).
     1 NE.VALMS(NVAD))) GOTO 50
 65    JL=JL+1
       DO 110 J=1,NVAD
 110   X(J,JL)=X(J,JNC)
       JNDEX(JL)=JNC
 50    CONTINUE
       NCAS=JL
       IF (MVAL.EQ.1) WRITE(LUB,8080) NCAS
       IF (NCAS.LE.(NVAR*1.75)) THEN
	  WRITE (*,8090) NCAS,NVAR
	  IF (JCST.EQ.1) WRITE(*,8100)
	  NSTOP=1
	  RETURN
       ENDIF
       IF (NCAS.LE.MAXM) THEN
	  WRITE(LUB,8110)
	  NSTOP=1
	  RETURN
       ENDIF
       IF (MVAL.NE.1) THEN
	 DO 150 J=1,NVAR
	 IF ((JCST.EQ.1.AND.J.EQ.NVAR).OR.
     1	 (JMISS(J).EQ.0)) GOTO 150
	 JJ=0
	 NCASM=NCAS
	 JPLUS=0
	 DO 160 JNC=1,NCAS
	 JPLUS=JPLUS+1
	 AW(JPLUS)=X(J,JNC)
	 IF (X(J,JNC).EQ.VALMS(J)) THEN
	    NCASM=NCASM-1
	    JJ=JJ+1
	    JPLUS=JPLUS-1
	    Y(JJ)=JNC
	 ENDIF
 160	 CONTINUE
	 IF (JJ.EQ.0) THEN
	    IF (JPRT.NE.0) WRITE(LUB,8120) LAB(J)
		      ELSE
	    NCASM=NCAS-JJ
	    AMED=AMDAN(AW,MAXN,AW,NCASM)
	    DO 180 J2=1,JJ
	    JY=Y(J2)+0.2
 180	    X(J,JY)=AMED
	    IF (JPRT.NE.0) WRITE(LUB,8130) LAB(J),JJ
	    DO 190 J2=1,JJ
	    J2Y=Y(J2)+0.2
 190	    IF (JPRT.NE.0) WRITE(LUB,8140) JNDEX(J2Y)
	 ENDIF
 150   CONTINUE
       ENDIF
       IF (MVAL.EQ.1) WRITE(*,8080) NCAS
 8000  FORMAT(10h Variable ,A10,24h has a missing value for,
     1 I4,7h cases.)
 8010  FORMAT(15h There are(is) ,I4,29h variable(s),which contain(s)/
     1 24h more than 80 percent (=,I4,28h ) cases with missing value.)
 8020  FORMAT(/39h Case has a missing value for variables,
     1 18h (variable number ,I4,17h is the response)/1X,
     1 4(1H-),25X,9(1H-))
 8030  FORMAT(39h The following cases have been deleted./)
 8040  FORMAT(1X,I4,23X,10I4)
 8050  FORMAT(28X,10I4)
 8060  FORMAT(6h Case ,I4,24h has a missing value for,
     1 23h the response variable.)
 8070  FORMAT(6h Case ,I4,32h has a missing value for all the,
     1 23h explanatory variables.)
 8080  FORMAT(/' After treatment of missing values, 'I4,
     1 ' cases remain.'/)     
 8090  FORMAT(/46h Too many coefficients according to the number,
     1 10h of cases.//16h Number of cases,8X,2h= ,I5/
     1 26h Number of coefficients = ,I5/25h The number of cases must,
     1 37h be twice the number of coefficients.)
 8100  FORMAT(31h (Including the constant term!))
 8110  FORMAT(52h More than 80 percent of the cases had to be deleted/
     1 61h because of the missing values. The analysis will be stopped.)
 8120  FORMAT(9h Variable ,A10,28h contains no missing values.)
 8130  FORMAT(15h The values of ,A10,17h will be replaced,
     1 18h by the median for,I5,13h cases namely)
 8140  FORMAT(30X,11hcase number,I6)
       RETURN
       END
CC     ------------------------------------------------------------------------
CC     SUBROUTINE FOR CALCULATING THE NUMBER OF REPLICATIONS IN THE
CC     LMS/LTS ALGORITHM
CC     ------------------------------------------------------------------------
       SUBROUTINE SUBREP(NCAS,NVAR,MAXP1,ALGO,NREP,JREPTAB,JREPLOW,
     1 JREPHI)
       INTEGER JREPTAB(MAXP1)
       INTEGER JREPLOW(MAXP1)
       INTEGER JREPHI(MAXP1)
       CHARACTER ALGO,YNOK
       WRITE(*,8000) NVAR
       IF (NCAS.LE.JREPLOW(NVAR)) THEN
CC-----TAKE ALL COMBINATIONS
	 ALGO='A'
	 NREP=NCOMB(NVAR,NCAS)
	 WRITE(*,8010)
				  ELSE
         IF (NCAS.GT.JREPHI(NVAR)) THEN
CC-----TAKE RANDOM SUBSETS
	   ALGO='R'
	   WRITE(*,8020) NVAR,NCAS,NVAR
				   ELSE
CC-----OTHERWISE CHOOSE BETWEEN RANDOM SUBSETS OR ALL COMBINATIONS
           NREP=NCOMB(NVAR,NCAS)
	   WRITE(*,8030) NREP,NVAR,NCAS
	   IF (NVAR.LE.2) THEN
	     IF (NREP.GE.50000) WRITE(*,8040)
			  ELSE
             IF (NREP.GE.10000) WRITE(*,8040)
           ENDIF
	   WRITE(*,8050) JREPTAB(NVAR),NVAR,NREP,NVAR
 10	   READ(*,9000) YNOK
	   IF (.NOT.(YNOK.EQ.'1'.OR.YNOK.EQ.'2')) THEN
	      WRITE(*,8060) 
	      GOTO 10
           ENDIF
	   MMM=JNTGR(YNOK)
	   ALGO='R'
	   IF (MMM.EQ.2) ALGO='A'
         ENDIF
	 IF (ALGO.EQ.'R') THEN
CC-----CHOOSE NUMBER OF RANDOM SUBSETS
           NREP=JREPTAB(NVAR)
	   WRITE(*,8070) NREP
 20        READ(*,9000) YNOK
	   IF (YNOK.EQ.'y') YNOK='Y'
	   IF (YNOK.EQ.'n') YNOK='N'
           IF (.NOT.(YNOK.EQ.'Y'.OR.YNOK.EQ.'N')) THEN
	     WRITE(*,8060) 
	     GOTO 20
           ENDIF
	   IF (YNOK.EQ.'N') THEN
	     WRITE(*,8080)
	     READ(*,*) NREP
           ENDIF
         ENDIF
       ENDIF
 8000  FORMAT(//' The program will use subsets containing',
     1 I4,' cases.')
 8010  FORMAT(/' All subsets will be considered.')
 8020  FORMAT(/' There are over a million subsets',
     1 ' of',I4,' cases out of',I5,' cases.'/' Therefore, the',
     1 ' program will draw random subsets of',I4,' cases.')
 8030  FORMAT(/' There are',I8,' subsets of',I4,' cases',
     1 ' out of',I6,' cases.')
 8040  FORMAT(/' It would take a lot of time for the',
     1 ' program to consider'/' all these subsets !')
 8050  FORMAT(/' Do you agree to have the program consider ',I6,
     1 /' randomly drawn subsets of',I4,' cases ? (Then',
     1 ' type 1)'/' Or do you prefer instead to have the',
     1 ' program construct'/' all',I8,' subsets of',I4,
     1 ' cases ? (Then type 2) : '$)
 8060  FORMAT(/' Not allowed ! Enter your choice again : '$)
 8070  FORMAT(/' Do you agree with the default choice of',
     1 1X,I6,' subsets ? Yes or no : '$)
 8080  FORMAT(/' Please enter the number of subsets you want : '$)
 9000  FORMAT(A1)
       RETURN
       END
CC     ---------------------------------------------------------------------
CC     *NCOMB* COMPUTES THE NUMBER OF COMBINATIONS OF K POINTS OUT OF N
CC     ---------------------------------------------------------------------
       FUNCTION NCOMB(K,N)
       COMB=1.0
       DO 10 J=1,K
       FACT=(N-J+1.0)/(K-J+1.0)
       COMB=COMB*FACT
 10    CONTINUE
       NCOMB=INT(COMB+0.5)
       RETURN
       END
CC     ------------------------------------------------------------------
CC     *STATIS* : STANDARDIZATION OF THE OBSERVATIONS AND
CC	          CALCULATION OF SOME STATISTICS
CC     ------------------------------------------------------------------
       SUBROUTINE STATIS(X,Y,XMED,XMAD,AW2,WEIGHTS,AW,
     1 JNDEX,C,H,JSUBS,NSTOP,NVAR,NVAD,NFAC,NCAS,JCST,JPRT,
     1 MAXP1,MAXN,LUB,MAXP,PREC,LAB)
       DIMENSION X(MAXP1,MAXN),Y(MAXN),XMED(MAXP1),XMAD(MAXP1)
       DIMENSION AW2(MAXN),WEIGHTS(MAXN),AW(MAXN)
       DIMENSION C(MAXP,MAXP1)
       DOUBLE PRECISION H(MAXP,MAXP1)
       INTEGER JNDEX(MAXN),JSUBS(MAXP1)
       CHARACTER*10 LAB(MAXP1)
       AL=NCAS
       IF (JCST.NE.0) GOTO 60
       DO 50 J=1,NVAD
       XMED(J)=0.0
       DO 10 JNC=1,NCAS
 10    AW2(JNC)=ABS(X(J,JNC))
       XMAD(J)=AMDAN(AW,MAXN,AW2,NCAS)*1.4826
       IF (ABS(XMAD(J)).GT.PREC) GOTO 30
       XMAD(J)=0.0
       DO 20 JNC=1,NCAS
 20    XMAD(J)=XMAD(J)+AW2(JNC)
       XMAD(J)=(XMAD(J)/AL)*1.2533
       IF (ABS(XMAD(J)).GT.PREC) GOTO 30
       WRITE(LUB,8000) LAB(J)
       NSTOP=1
       RETURN
 30    DO 40 JNC=1,NCAS
       X(J,JNC)=X(J,JNC)/XMAD(J)
 40    CONTINUE
 50    CONTINUE
       WRITE(LUB,8015)
       GOTO 150
 60    DO 120 J=1,NVAD
       IF(J.EQ.NVAR) GOTO 120
       DO 70 JNC=1,NCAS
 70    AW2(JNC)=X(J,JNC)
       XMED(J)=AMDAN(AW,MAXN,AW2,NCAS)
       DO 80 JNC=1,NCAS
 80    AW2(JNC)=ABS(AW2(JNC)-XMED(J))
       XMAD(J)=AMDAN(AW,MAXN,AW2,NCAS)*1.4826
       IF (ABS(XMAD(J)).GT.PREC) GOTO 100
       XMAD(J)=0.0
       DO 90 JNC=1,NCAS
 90    XMAD(J)=XMAD(J)+AW2(JNC)
       XMAD(J)=(XMAD(J)/AL)*1.2533
       IF (ABS(XMAD(J)).GT.PREC) GOTO 100
       WRITE(LUB,8000) LAB(J)
       NSTOP=1
       RETURN
 100   DO 110 JNC=1,NCAS
       X(J,JNC)=(X(J,JNC)-XMED(J))/XMAD(J)
 110   CONTINUE
 120   CONTINUE
       XMED(NVAR)=1.0
       WRITE(LUB,8010)
       IF (JCST.EQ.1) WRITE(LUB,8020) (LAB(J),J=1,NFAC),LAB(NVAD)
       IF (JCST.EQ.0) WRITE(LUB,8020) (LAB(J),J=1,NVAD)
       IF ((NVAD.GT.6.AND.JCST.EQ.0).OR.(NVAD.GT.7.AND.JCST.EQ.1))
     1 GOTO 130
       IF (JCST.EQ.0) WRITE(LUB,8030)(XMED(J),J=1,NVAD)
       IF (JCST.EQ.1) WRITE(LUB,8030)(XMED(J),J=1,NFAC),XMED(NVAD)
       GOTO 140
 130   WRITE(LUB,8030)(XMED(J),J=1,6)
       IF (JCST.EQ.0) WRITE(LUB,8040)(XMED(J),J=7,NVAD)
       IF (JCST.EQ.1.AND.NFAC.GE.7)
     1 WRITE(LUB,8040)(XMED(J),J=7,NFAC),XMED(NVAD)
       IF (JCST.EQ.1.AND.NFAC.LT.7) WRITE(LUB,8040) XMED(NVAD)
 140   XMED(NVAR)=0.0
       WRITE(LUB,8050)
       IF (JCST.EQ.1) WRITE(LUB,8020) (LAB(J),J=1,NFAC),LAB(NVAD)
       IF (JCST.EQ.0) WRITE(LUB,8020) (LAB(J),J=1,NVAD)
 150   IF((NVAD.GT.6.AND.JCST.EQ.0).OR.(NVAD.GT.7.AND.JCST.EQ.1))
     1 GOTO 160
       IF (JCST.EQ.0) WRITE(LUB,8030)(XMAD(J),J=1,NVAD)
       IF (JCST.EQ.1) WRITE(LUB,8030)(XMAD(J),J=1,NFAC),XMAD(NVAD)
       GOTO 170
 160   WRITE(LUB,8030)(XMAD(J),J=1,6)
       IF (JCST.EQ.0) WRITE(LUB,8040)(XMAD(J),J=7,NVAD)
       IF (JCST.EQ.1.AND.NFAC.GE.7)
     1 WRITE(LUB,8040)(XMAD(J),J=7,NFAC),XMAD(NVAD)
       IF (JCST.EQ.1.AND.NFAC.LT.7) WRITE(LUB,8040) XMAD(NVAD)
 170   IF(JCST.NE.0) XMAD(NVAR)=1.0
       IF (JPRT.LT.2) GOTO 200
       WRITE(LUB,8060)
       IF (JCST.EQ.1) WRITE(LUB,8025) (LAB(J),J=1,NFAC),LAB(NVAD)
       IF (JCST.EQ.0) WRITE(LUB,8025) (LAB(J),J=1,NVAD)
       DO 190 JNC=1,NCAS
       IF((NVAD.GT.6.AND.JCST.EQ.0).OR.(NVAD.GT.7.AND.JCST.EQ.1))
     1 GOTO 180
       IF (JCST.EQ.1) WRITE(LUB,8070) JNDEX(JNC),(X(J,JNC),J=1,NFAC),
     1 X(NVAD,JNC)
       IF (JCST.EQ.0) WRITE(LUB,8070) JNDEX(JNC),(X(J,JNC),J=1,NVAD)
       GOTO 190
 180   WRITE(LUB,8070) JNDEX(JNC),(X(J,JNC),J=1,6)
       IF (JCST.EQ.1.AND.NFAC.GE.7)
     1 WRITE(LUB,8040) (X(J,JNC),J=7,NFAC),X(NVAD,JNC)
       IF (JCST.EQ.1.AND.NFAC.LT.7) WRITE(LUB,8040) X(NVAD,JNC)
       IF (JCST.EQ.0) WRITE(LUB,8040) (X(J,JNC),J=7,NVAD)
 190   CONTINUE
 200   IF(JPRT.EQ.0) GOTO 260
       DO 220 JSPA=1,NVAR
       IF(JSPA.EQ.NVAR.AND.JCST.EQ.1) GOTO 220
       JS=JSPA+1
       DO 210 JSPB=JS,NVAD
       IF(JSPB.EQ.NVAR.AND.JCST.EQ.1) GOTO 210
       JSM=JSPB-1
       CALL CORR(X,JSPB,JSPA,NCAS,NFAC,JCST,AW,Y,AW2,MAXP1,MAXN,
     1 SPEAR,PEARC,PREC)
       C(JSM,JSPA)=SPEAR
       H(JSM,JSPA)=PEARC
 210   CONTINUE
 220   CONTINUE
       WRITE(LUB,8080) LAB(NVAD)
       DIAG=1.0
       WRITE(LUB,8090) LAB(1),DIAG
       DO 230 JSPA=1,NFAC
       JSPP=JSPA+1
       IF(JCST.EQ.1.AND.JSPP.EQ.NVAR) GOTO 230
       WRITE(LUB,8090) LAB(JSPP),(H(JSPA,JSPB),JSPB=1,JSPA),DIAG
 230   CONTINUE
       IF(JCST.EQ.1) WRITE(LUB,8090) LAB(NVAD),(H(JSPA,JSPB),
     1 JSPB=1,NFAC),DIAG
       IF (JCST.EQ.0) WRITE(LUB,8090) LAB(NVAD),(H(JSPA,JSPB),
     1 JSPB=1,NVAR),DIAG
       DO 240 J=1,NVAR
 240   JSUBS(J)=J
       WRITE(LUB,8100) LAB(NVAD)
       WRITE(LUB,8090) LAB(1),DIAG
       DO 250 JSPA=1,NFAC
       JSPP=JSPA+1
       IF(JCST.EQ.1.AND.JSPP.EQ.NVAR) GOTO 250
       WRITE(LUB,8090) LAB(JSPP),(C(JSPA,JSPB),JSPB=1,JSPA),DIAG
 250   CONTINUE
       IF(JCST.EQ.1) WRITE(LUB,8090) LAB(NVAD),(C(JSPA,JSPB),
     1 JSPB=1,NFAC),DIAG
       IF (JCST.EQ.0) WRITE(LUB,8090) LAB(NVAD),(C(JSPA,JSPB),
     1 JSPB=1,NVAR),DIAG
 260   DO 270 JNC=1,NCAS
CC-----MEANWHILE, INITIALIZATION OF THE WEIGHTS
       WEIGHTS(JNC)=1.0
       Y(JNC)=X(NVAD,JNC)
 270   CONTINUE
 8000  FORMAT(/21h Variable with label ,A10,17h is constant over/
     1 47h all observations hence the program cannot run.,
     1 /50h Please repeat the analysis without this variable./)
 8010  FORMAT(//12h Medians  = /)
 8015  FORMAT(//33h Dispersion of absolute values = /)
 8020  FORMAT(7X,6(A10,1X)/15X,5(A10,1X))
 8025  FORMAT('    i  ',6(A10,1X)/15X,5(A10,1X))
 8030  FORMAT(7X,6(F10.4,1X))
 8040  FORMAT(15X,5(F10.4,1X))
 8050  FORMAT(//15h Dispersions = /)
 8060  FORMAT(//35h The standardized observations are:/)
 8070  FORMAT(I5,2X,6(F10.4,1X))
 8080  FORMAT(//33h Pearson correlation coefficients,
     1 22h between the variables/' (the response variable is ',
     1 A10,')'/)
 8090  FORMAT(1X,A10,(2X,11F6.2)/)
 8100  FORMAT(//39h Spearman rank correlation coefficients,
     1 22h between the variables/' (the response variable is ',
     1 A10,')'/)
       RETURN
       END
CC     -----------------------------------------------------------------
CC     *CORR* : CALCULATES THE SPEARMAN RANK CORRELATION COEFFICIENT
CC              BETWEEN VARIABLES JSPA AND JSPB.
CC     -----------------------------------------------------------------
       SUBROUTINE CORR(X,JSPB,JSPA,NCAS,NFAC,JCST,AW,Y,AW2,MAXP1,MAXN,
     1 SPEAR,PEARC,PREC)
       DIMENSION X(MAXP1,MAXN),AW(MAXN),Y(MAXN),AW2(MAXN)
       SPEAR=0.0
       SUMA=0.0
       SUMB=0.0
       STDA=0.0
       STDB=0.0
       CVVAR=0.0
       AL=NCAS
       IF (((JSPB-JSPA).EQ.1).OR.(JCST.EQ.1.AND.JSPA.EQ.NFAC))
     1 GOTO  5
       GOTO 105
 5     NTWE=0
       DO 10 NJ=1,NCAS
       AW(NJ)=X(JSPA,NJ)
       AW2(NJ)=NJ
 10    CONTINUE
       NCM=NCAS-1
 15    DO 20 NJ=1,NCM
       NJP=NJ+1
       NJA=NJ
       DO 30 NJB=NJP,NCAS
       IF(AW(NJB).GE.AW(NJA)) GOTO 30
       NJA=NJB
 30    CONTINUE
       IF(NJA.EQ.NJ) GOTO 20
       W=AW(NJ)
       AW(NJ)=AW(NJA)
       AW(NJA)=W
       W=AW2(NJA)
       AW2(NJA)=AW2(NJ)
       AW2(NJ)=W
 20    CONTINUE
       DO 40 NJ=1,NCAS
       NDX=AW2(NJ)
       IF(NTWE.EQ.0) Y(NDX)=NJ
       IF(NTWE.NE.0) AW(NDX)=NJ
 40    CONTINUE
       NJ=1
 55    TA=1
       NJA=AW2(NJ)
       IF(NTWE.EQ.0) TB=Y(NJA)
       IF(NTWE.NE.0) TB=AW(NJA)
 60    NJA=AW2(NJ)
       NJAP=AW2(NJ+1)
       IF(NTWE.EQ.0.AND.(X(JSPA,NJA).NE.X(JSPA,NJAP))) GOTO 70
       IF(NTWE.NE.0.AND.(X(JSPB,NJA).NE.X(JSPB,NJAP))) GOTO 70
       TA=TA+1.0
       IF(NTWE.EQ.0) TB=TB+Y(NJAP)
       IF(NTWE.NE.0) TB=TB+AW(NJAP)
       NJ=NJ+1
       IF (NJ.EQ.NCAS) GOTO 70
       GOTO 60
 70    IF(TA.EQ.1.0) GOTO 100
       JTA=TA
       NJB=NJ+1-JTA
       DO 80 K=NJB,NJ
       KK=AW2(K)
       IF(NTWE.EQ.0) Y(KK)=TB/TA
       IF(NTWE.NE.0) AW(KK)=TB/TA
 80    CONTINUE
 100   NJ=NJ+1
       IF (NJ.LT.NCAS) GOTO 55
       IF(NTWE.NE.0) GOTO 150
 105   DO 110 NJ=1,NCAS
       AW(NJ)=X(JSPB,NJ)
       AW2(NJ)=NJ
       SUMA=SUMA+X(JSPA,NJ)
       SUMB=SUMB+X(JSPB,NJ)
 110   CONTINUE
       SUMA=SUMA/AL
       SUMB=SUMB/AL
       NTWE=1
       GOTO 15
 150   DO 120 NJ=1,NCAS
       SPEAR=SPEAR+(Y(NJ)-AW(NJ))**2
       STDA=STDA+(X(JSPA,NJ)-SUMA)**2
       STDB=STDB+(X(JSPB,NJ)-SUMB)**2
       CVVAR=CVVAR+(X(JSPA,NJ)-SUMA)*(X(JSPB,NJ)-SUMB)
 120   CONTINUE
       SPEAR=1.0-6.0*SPEAR/(AL*(AL**2-1.0))
       STDA=SQRT(STDA*STDB)
       IF(ABS(STDA).GT.PREC) PEARC=CVVAR/STDA
       IF(ABS(STDA).LE.PREC) PEARC=99.99
       RETURN
       END
CC     -----------------------------------------------------------------
CC     *PULL* : SEARCHES THE KTH VALUE IN A VECTOR
CC              OF LENGTH 'NCAS'.
CC     -----------------------------------------------------------------
       FUNCTION PULL(AW,MAXN,AA,NCAS,K)
       DIMENSION AA(NCAS),AW(MAXN)
CC-----AW IS THE WORKING VECTOR, AA DOES NOT CHANGE
       DO 10 JNC=1,NCAS
 10    AW(JNC)=AA(JNC)
       L=1
       LR=NCAS
 20    IF(L.GE.LR) GOTO 90
       AX=AW(K)
       JNC=L
       J=LR
 30    IF(JNC.GT.J) GOTO 80
 40    IF(AW(JNC).GE.AX) GOTO 50
       JNC=JNC+1
       GOTO 40
 50    IF(AW(J).LE.AX) GOTO 60
       J=J-1
       GOTO 50
 60    IF(JNC.GT.J) GOTO 70
       WA=AW(JNC)
       AW(JNC)=AW(J)
       AW(J)=WA
       JNC=JNC+1
       J=J-1
 70    GOTO 30
 80    IF(J.LT.K) L=JNC
       IF (K.LT.JNC) LR=J
       GOTO 20
 90    PULL=AW(K)
       RETURN
       END
CC     -----------------------------------------------------------------
CC     *AMDAN* : CALCULATES THE MEDIAN OF A VECTOR
CC               OF LENGTH 'NCAS'.
CC     -----------------------------------------------------------------
       FUNCTION AMDAN(AW,MAXN,AA,NCAS)
       DIMENSION AA(NCAS),AW(MAXN)
CC-----AW IS THE WORKING VECTOR, AA DOES NOT CHANGE
       AL=NCAS
       JNDL=INT(AL/2.0)
       IF (MOD(NCAS,2).EQ.0) THEN
	  AMDAN= (PULL(AW,MAXN,AA,NCAS,JNDL)+
     1		    PULL(AW,MAXN,AA,NCAS,JNDL+1))/2.0
			     ELSE
       AMDAN= PULL(AW,MAXN,AA,NCAS,JNDL+1)
       ENDIF
       RETURN
       END
CC     ------------------------------------------------------------------
CC     *RSQU* : CALCULATES COEFFICIENT OF DETERMINATION AND F-VALUE
CC              OF LS OR RLS
CC     ------------------------------------------------------------------
       FUNCTION RSQU(NCAS,NVAD,JCST,Y,MAXN,SSE,
     1 FVALUE,PREC,XMAD,XMED,MAXP1,WEIGHTS,NNUL)
       DIMENSION Y(MAXN),XMAD(MAXP1),XMED(MAXP1),WEIGHTS(MAXN)
       RYM=0.0
       IF (JCST.EQ.0) GOTO 20
       DO 10 JNC=1,NCAS
 10    RYM=(Y(JNC)*XMAD(NVAD)+XMED(NVAD))*WEIGHTS(JNC)+RYM
       AL=NNUL
       RYM=RYM/AL
 20    SST=0.0
       DO 30 JNC=1,NCAS
 30    SST=SST+(((Y(JNC)*XMAD(NVAD)+XMED(NVAD))-RYM)**2)*WEIGHTS(JNC)
       DF1=(NVAD-1)-JCST
       DF2=NNUL-(NVAD-1)
       IF(SST.LT.PREC)SST=PREC
       RSQU=1.0-(SSE/SST)
       IF (RSQU.LT.0.0) RSQU=0.0
       IF (RSQU.GT.1.0) RSQU=1.0
       SSEL=SSE
       IF(SSEL.LT.PREC)SSEL=PREC
       FVALUE=((SST-SSEL)/DF1)/(SSEL/DF2)
       IF(FVALUE.LT.0.0)FVALUE=0.0
       RETURN
       END
CC     -----------------------------------------------------------------
CC     *RESTT* : CALCULATES FOR ALL JNC (JNC=1,..,NCAS) THE RESIDUAL
CC               Y(JNC)-SUM (GESCF(J)*X(J,JNC)), FOR J=1,..,LTSTE.
CC     -----------------------------------------------------------------
       SUBROUTINE RESTT(GESCF,JABS,JTR,LTSTE,NCAS,NVAD,NZWE,
     1 X,Y,RESDU,WEIGHTS,XMED,XMAD,MAXP1,MAXN,NOPT,SCAL,QUAN,PREC)
       DIMENSION GESCF(LTSTE),X(MAXP1,MAXN),Y(MAXN),WEIGHTS(MAXN)       
       DIMENSION RESDU(MAXN),XMED(MAXP1),XMAD(MAXP1)
CC-----JTR = 1, THEN CALCULATE THE RESIDUALS FOR THE STANDARDIZED OBSERVATIONS
CC-----      0	 "      "             "              RAW
CC-----JABS = 1, CALCULATE THE ABSOLUTE RESIDUALS
CC-----       0 	       RESIDUALS
CC-----NOPT = 1, CALCULATE THE LMS/LTS FINAL SCALE AND THE RESULTING WEIGHTS
CC-----       0, OTHERWISE
       NZWE=0
       SCAL=0.0
       AVLQS=0.0
       ANVAR=NVAD-1
       DO 10 JNC=1,NCAS
       IF (JTR.NE.1)  THEN
	  RESDU(JNC)=Y(JNC)
		      ELSE
	  RESDU(JNC)=Y(JNC)*XMAD(NVAD)+XMED(NVAD)
       ENDIF
       DO 20 J=1,LTSTE
       IF (JTR.NE.1) THEN
	  RESDU(JNC)=RESDU(JNC)-X(J,JNC)*GESCF(J)
		     ELSE
	  RESDU(JNC)=RESDU(JNC)-((X(J,JNC)*XMAD(J)+XMED(J))*GESCF(J))
       ENDIF
 20    CONTINUE
       IF (JABS.EQ.1.AND.NOPT.NE.1) RESDU(JNC)=ABS(RESDU(JNC))
       IF (ABS(RESDU(JNC)).LT.PREC) NZWE=NZWE+1
       IF (NOPT.EQ.0) GOTO 10
       IF (ABS(RESDU(JNC)).LE.(2.5*QUAN)) THEN
	  WSC=1.0
				     ELSE
	  WSC=0.0
       ENDIF
       AVLQS=AVLQS+WSC
       SCAL=SCAL+(RESDU(JNC)*RESDU(JNC))*WSC
 10    CONTINUE
       IF (NOPT.EQ.0) RETURN
       SCAL=SQRT(SCAL/(AVLQS-ANVAR))
       NZWE=0
       DO 40 JNC=1,NCAS
       IF (ABS(RESDU(JNC)).LE.(2.5*SCAL)) THEN
	  WEIGHTS(JNC)=1.0
	  NZWE=NZWE+1
					  ELSE
	  WEIGHTS(JNC)=0.0
       ENDIF
 40    CONTINUE
       RETURN
       END
CC     -----------------------------------------------------------------
CC     *RANGS* : SORTS A VECTOR OF LENGTH 'NCAS'.
CC     -----------------------------------------------------------------
      SUBROUTINE RANGS(AM,LAME)
      DIMENSION AM(LAME)
      INTEGER JLV(30),JRV(30)
      JSS=1
      JLV(1)=1
      JRV(1)=LAME
  10  JNDL=JLV(JSS)
      JR=JRV(JSS)
      JSS=JSS-1
  20  JNC=JNDL
      J=JR
      JTWE=(JNDL+JR)/2
      XX=AM(JTWE)
  30  IF(AM(JNC).GE.XX)GOTO 40
      JNC=JNC+1
      GOTO 30
  40  IF(XX.GE.AM(J))GOTO 50
      J=J-1
      GOTO 40
  50  IF(JNC.GT.J)GOTO 60
      AMM=AM(JNC)
      AM(JNC)=AM(J)
      AM(J)=AMM
      JNC=JNC+1
      J=J-1
  60  IF (JNC.LE.J) GOTO 30
      IF ((J-JNDL).LT.(JR-JNC)) GOTO 80
      IF (JNDL.GE.J) GOTO 70
      JSS=JSS+1
      JLV(JSS)=JNDL
      JRV(JSS)=J
  70  JNDL=JNC
      GOTO 100
  80  IF (JNC.GE.JR) GOTO 90
      JSS=JSS+1
      JLV(JSS)=JNC
      JRV(JSS)=JR
  90  JR=J
  100 IF (JNDL.LT.JR) GOTO 20
      IF (JSS.NE.0) GOTO 10
      RETURN
      END
CC    -------------------------------------------------------------------
CC    *RDUAL* : CALCULATES THE RESIDUALS OF ALL CASES
CC    -------------------------------------------------------------------
       SUBROUTINE RDUAL(AA,JKEUS,JDA,NCAS,NVAR,JCST,JPRT,NVAD,LUB,PREC,
     1 JREG,X,Y,RESDU,WEIGHTS,XMED,XMAD,JPLT,AW,JNDEX,
     1 MAXP1,MAXN,JHEAD,LAB,MROB)
       DIMENSION AA(JDA),X(MAXP1,MAXN),Y(MAXN),RESDU(MAXN)
       DIMENSION WEIGHTS(MAXN)
       DIMENSION XMED(MAXP1),XMAD(MAXP1),AW(MAXN)
       INTEGER JNDEX(MAXN)
       CHARACTER*10 LAB(MAXP1)
       CHARACTER*60 JHEAD
       JPL2=1
       IF(JPRT.EQ.0) GOTO 10
       IF (NVAR.EQ.1.AND.JCST.EQ.1) THEN
	  IF (JKEUS.NE.2) WRITE(LUB,8000) LAB(NVAD)
	  IF (JKEUS.EQ.2) WRITE(LUB,8010) LAB(NVAD)
				    ELSE
	  IF (JKEUS.NE.2) WRITE(LUB,8020) LAB(NVAD),LAB(NVAD)
	  IF (JKEUS.EQ.2) WRITE(LUB,8030) LAB(NVAD),LAB(NVAD)
       ENDIF
 10    DO 20 JNC=1,NCAS
       IF (.NOT.(NVAR.EQ.1.AND.JCST.EQ.1)) GOTO 40
       BBB=Y(JNC)-AA(1)
       GOTO 60
 40    AW(JNC)=0.0
       DO 30 J=1,NVAR
 30    AW(JNC)=AW(JNC)+AA(J)*(X(J,JNC)*XMAD(J)+XMED(J))
       YJ=Y(JNC)*XMAD(NVAD)+XMED(NVAD)
       BBB=YJ-AW(JNC)
 60    IF (AA(NVAD).GT.PREC) GOTO 70
       IF (ABS(BBB).LT.PREC) BBB=0.0
       RESDU(JNC)=BBB
       GOTO 50
 70    RESDU(JNC)=BBB/AA(NVAD)
 50    IF (JPRT.EQ.0) GOTO 20
       IF (NVAR.EQ.1.AND.JCST.EQ.1) GOTO 80
       IF (AA(NVAD).GT.PREC.AND.JKEUS.NE.2)
     1 WRITE(LUB,8040) YJ,AW(JNC),BBB,JNDEX(JNC),RESDU(JNC)
       IF (AA(NVAD).LE.PREC)
     1 WRITE(LUB,8050) YJ,AW(JNC),BBB,JNDEX(JNC)
       IF (AA(NVAD).GT.PREC.AND.JKEUS.EQ.2)
     1 WRITE(LUB,8040) YJ,AW(JNC),BBB,JNDEX(JNC),RESDU(JNC),
     1 WEIGHTS(JNC)
       GOTO 20
 80    IF (JKEUS.NE.2) WRITE(LUB,8060) Y(JNC),BBB,JNC,RESDU(JNC)
       IF (JKEUS.EQ.2) WRITE(LUB,8060) Y(JNC),BBB,JNC,RESDU(JNC),
     1 WEIGHTS(JNC)
 20    CONTINUE
       IF(JPLT.LT.1) GOTO 90
       IF (AA(NVAD).LE.PREC) JPL2=0
       CALL GRAF(AW,RESDU,NCAS,0,JPL2,JPLT,JNDEX,MAXN,LUB,JREG,
     1 JHEAD,MAXP1,NVAD,LAB,MROB)
       IF (JPLT.LT.3) GOTO 90
       JPLT=2
       CALL GRAF(AW,RESDU,NCAS,0,JPL2,JPLT,JNDEX,MAXN,LUB,JREG,
     1 JHEAD,MAXP1,NVAD,LAB,MROB)
       JPLT=3
 90    CONTINUE
 8000  FORMAT(//9X,8hobserved,12X,'residual',5h    i,8h   res/s/7X,A10/)       
 8010  FORMAT(//9X,8hobserved,12X,'residual',5h    i,8h   res/s,'   w'/
     1 7X,A10,33X,5h    i/)
 8020  FORMAT(//9X,8hobserved,11X,9hestimated,12X,8hresidual,3X,2h i,
     1 8h   res/s/,7X,A10,10X,A10/)
 8030  FORMAT(//9X,8hobserved,11X,9hestimated,12X,'residual',3X,2h i,
     1 8h   res/s,'   w'/7X,A10,10X,A10,33X,5h    i/)
 8040  FORMAT(1X,F16.5,2(1X,F19.5),I5,1X,F7.2,F6.1)
 8050  FORMAT(1X,F16.5,2(1X,F19.5),I5,8H   *****)
 8060  FORMAT(1X,F16.5,1X,F19.5,I5,1X,F7.2,F6.1)
       RETURN
       END
CC     ----------------------------------------------------------------
CC     *LMSLOC* : CALCULATES THE LMS IN THE ONE-DIMENSIONAL CASE.
CC                W CONTAINS THE ORDERED OBSERVATIONS
CC     ----------------------------------------------------------------
       SUBROUTINE LMSLOC(W,NCAS,JQU,SLUTN,BSTD,PREC,AW2,FACTOR)
       DIMENSION W(NCAS),AW2(NCAS)
       K=JQU-1
       VERSC=W(K+1)-W(1)
       SLUTN=(W(1)+W(K+1))/2.0
       AW2(1)=SLUTN
       BSTD=(W(K+1)-W(1))/2.0
       IF (NCAS.EQ.2) RETURN
       JTEL=1
       LNK=NCAS-K
       DO 10 JNC=2,LNK
       JPK=JNC+K
       ABSW=W(JPK)-W(JNC)
       IF ( ABS(ABSW-VERSC).GT.PREC) GOTO 20
       JTEL=JTEL+1
       AW2(JTEL)=(W(JNC)+W(JPK))/2.0
       GOTO 10
 20    IF (ABSW.LT.VERSC) THEN
       VERSC=ABSW
       JTEL=1
       BSTD=(W(JPK)-W(JNC))/2.0
       AW2(1)=(W(JNC)+W(JPK))/2.0
       ENDIF
 10    CONTINUE
CC-----IN CASE OF SEVERAL MINIMA, TAKE LOW MEDIAN
       SLUTN=AW2(INT(JTEL/2.0+0.5))
       BSTD=FACTOR*BSTD
       RETURN
       END
CC     ----------------------------------------------------------------
CC     *LTSLOC* : CALCULATES THE LTS IN THE ONE-DIMENSIONAL CASE.
CC                W CONTAINS THE ORDERED OBSERVATIONS
CC     ----------------------------------------------------------------
       SUBROUTINE LTSLOC(W,NCAS,JQU,SLUTN,BSTD,AW,AW2,MAXN,FACTOR)
       DIMENSION W(NCAS),AW(MAXN),AW2(MAXN)
       SQ=0.0
       SQMIN=0.0
       DO 20 JINT=1,NCAS-JQU+1
       AW(JINT)=0.0
       DO 10 J=1,JQU
       AW(JINT)=AW(JINT)+W(J+JINT-1)
       IF (JINT.EQ.1) SQ=SQ+W(J)*W(J)
 10    CONTINUE
       AW2(JINT)=AW(JINT)*AW(JINT)/JQU
       IF (JINT.EQ.1) THEN
	 SQ=SQ-AW2(JINT)
         SQMIN=SQ
	 SLUTN=AW(JINT)
		      ELSE
         SQ=SQ-W(JINT-1)*W(JINT-1)+W(JINT+JQU-1)*W(JINT+JQU-1)
     1        -AW2(JINT) + AW2(JINT-1)
	 IF (SQ.LT.SQMIN) THEN
	   SQMIN=SQ
	   SLUTN=AW(JINT)
         ENDIF
       ENDIF
 20    CONTINUE
       SLUTN=SLUTN/JQU
       BSTD=FACTOR*SQRT(SQMIN/JQU)
       RETURN
       END
CC     -----------------------------------------------------------------
CC     *TRC* : RETRANSFORMATION OF THE VARIANCE-COVARIANCE MATRIX
CC     -----------------------------------------------------------------
       SUBROUTINE TRC(H,DA,MAXP,MAXP1,NVAR,JCST,NFAC,NVAD,
     1 XMED,XMAD)
       DIMENSION DA(MAXP)
       DIMENSION XMED(MAXP1),XMAD(MAXP1)
       DOUBLE PRECISION H(MAXP,MAXP1),XMP2,HNN
       XMP2=DBLE(XMAD(NVAD))*DBLE(XMAD(NVAD))
       IF (JCST.EQ.0) THEN
	  DO 10 J=1,NVAR
	  DO 20 K=1,J
	  H(J,K)=H(J,K)*(XMP2/(DBLE(XMAD(J))*DBLE(XMAD(K))))
 20	  CONTINUE
	  DA(J)=DSQRT(H(J,J))
 10	  CONTINUE
       ELSE
	  DO 25 J=1,NVAR
	  H(J,NVAD)=H(J,J)
 25	  CONTINUE
	  DO 30 J=1,NVAR
	  DO 40 K=1,J
	  H(J,K)=H(J,K)*XMP2/(DBLE(XMAD(J))*DBLE(XMAD(K)))
 40	  CONTINUE
	  DA(J)=DSQRT(H(J,J))
 30	  CONTINUE
	  DO 50 K=1,NFAC
	  H(NVAR,K)=H(K,NVAR)*XMP2/DBLE(XMAD(K))
	  DO 60 K2=1,NVAR
	  IF (K.EQ.K2) THEN
	    H(NVAR,K)=H(NVAR,K)-DBLE(XMED(K))*XMP2/
     1		      (DBLE(XMAD(K2))*DBLE(XMAD(K)))*H(K2,NVAD)
	    GOTO 60
	  ENDIF
	  IF (K.LT.K2) THEN
	       H(NVAR,K)=H(NVAR,K)-(DBLE(XMED(K2))*XMP2)/
     1		      (DBLE(XMAD(K2))*DBLE(XMAD(K)))*H(K,K2)
			ELSE
	       H(NVAR,K)=H(NVAR,K)-DBLE(XMED(K2))*XMP2/
     1		      (DBLE(XMAD(K2))*DBLE(XMAD(K)))*H(K2,K)
	  ENDIF
 60	  CONTINUE
 50	  CONTINUE
	  H(NVAR,NVAR)=H(NVAR,NVAD)*XMP2
	  DO 70 K=1,NVAR
	  H(NVAR,NVAR)=H(NVAR,NVAR)+
     1	    (DBLE(XMED(K))*DBLE(XMED(K)))*XMP2/
     1			 (DBLE(XMAD(K))*DBLE(XMAD(K)))*H(K,NVAD)
 70	  CONTINUE
	  DO 80 K=1,NVAR
	  IF (K.NE.NVAR) THEN
	     H(NVAR,NVAR)=H(NVAR,NVAR)-2.0D0*XMP2*DBLE(XMED(K))/
     1				    (DBLE(XMAD(K)))*H(K,NVAR)
			 ELSE
	     H(NVAR,NVAR)=H(NVAR,NVAR)-2.0D0*XMP2*DBLE(XMED(K))/
     1				    (DBLE(XMAD(K)))*H(NVAR,NVAD)
	  ENDIF
 80	  CONTINUE
	  DO 90 J=1,NFAC
	  JU=J+1
	  DO 90 K=JU,NVAR
	  HNN=2.0D0*DBLE(XMED(J))*DBLE(XMED(K))*XMP2
	  H(NVAR,NVAR)=H(NVAR,NVAR)+HNN/
     1			      (DBLE(XMAD(J))*DBLE(XMAD(K)))*H(J,K)
 90	  CONTINUE
	  DA(NVAR)=DSQRT(H(NVAR,NVAR))
       ENDIF
       RETURN
       END
CC     -----------------------------------------------------------------
CC     *SCHCV* : PRINTS THE VARIANCE-COVARIANCE MATRIX OF THE
CC               REGRESSION COEFFICIENTS.
CC     -----------------------------------------------------------------
       SUBROUTINE SCHCV(NVAR,MAXP1,MAXP,H,LUB)
       DOUBLE PRECISION H(MAXP,MAXP1)
       WRITE(LUB,9000)
       DO 10 J=1,NVAR
       WRITE(LUB,9010) (H(J,K),K=1,J)
 10    CONTINUE
 9000  FORMAT(/' Variance-covariance matrix of the estimated',
     1 ' coefficients :'/)
 9010  FORMAT(5(5X,D10.4))
       RETURN
       END
CC     ----------------------------------------------------------------
CC     *RTRAN* : RETRANSFORMATION OF THE REGRESSION COEFFICIENTS.
CC     ----------------------------------------------------------------
       SUBROUTINE RTRAN(NVAR,JCST,NFAC,NVAD,MAXP1,XMED,XMAD,AA,JAL,FCKW)
       DIMENSION AA(JAL),XMED(MAXP1),XMAD(MAXP1)
       IF (NVAR.LE.1) THEN
	 AA(1)=AA(1)*XMAD(NVAD)/XMAD(1)
	 GOTO 30
       ENDIF
       DO 10 J=1,NFAC
 10    AA(J)=AA(J)*XMAD(NVAD)/XMAD(J)
       IF (JCST.EQ.0) THEN
	 AA(NVAR)=AA(NVAR)*XMAD(NVAD)/XMAD(NVAR)
		      ELSE
	 AA(NVAR)=AA(NVAR)*XMAD(NVAD)
	 DO 20 J=1,NFAC
 20	 AA(NVAR)=AA(NVAR)-AA(J)*XMED(J)
	 AA(NVAR)=AA(NVAR)+XMED(NVAD)
       ENDIF
 30    FCKW=FCKW*(XMAD(NVAD)*XMAD(NVAD))
       RETURN
       END
CC     -----------------------------------------------------------------
CC     *RANPN* : RANDOM NUMBER GENERATOR .
CC     -----------------------------------------------------------------
       SUBROUTINE RANPN(NCAS,NVAR,JSUBS,MAXP1,JEY,JLP,MAXRP)
       INTEGER JSUBS(MAXP1)
CC     WE PROGRAMMED THIS RANDOM NUMBER GENERATOR OURSELVES BECAUSE
CC     WE WANTED IT TO BE MACHINE INDEPENDENT. IT SHOULD RUN ON MOST
CC     COMPUTERS BECAUSE THE LARGEST INTEGER USED IS LESS THAN 2**30.
CC     THE PERIOD IS 2**16=65536, WHICH IS SUFFICIENT FOR THIS
CC     APPLICATION.
       AL=NCAS
       JLP=JLP+1
       IF (JLP.LE.MAXRP) GOTO 10
       RETURN
 10    DO 20 JNC=1,NVAR
 30    JEY=JEY*5761+999
       JEYQT=JEY/65536
       JEY=JEY-JEYQT*65536
       RY=JEY
       RNULE=RY/65536.0
       JRAN=INT(RNULE*AL)+1
       IF (JNC.EQ.1) GOTO 50
       JNCM=JNC-1
       DO 40 J=1,JNCM
       IF (JSUBS(J).EQ.JRAN) GOTO 30
 40    CONTINUE
 50    JSUBS(JNC)=JRAN
 20    CONTINUE
       RETURN
       END
CC     ---------------------------------------------------------------
CC     *GENPN* : GENERATES ALL COMBINATIONS OF 'NVAR' INTEGERS
CC               BETWEEN 1 AND 'NCAS'.
CC     ---------------------------------------------------------------
        SUBROUTINE GENPN(NCAS,NVAR,JSUBS)
        INTEGER JSUBS(NVAR)
        K=NVAR
        JSUBS(K)=JSUBS(K)+1
 10     IF(K.EQ.1.OR.JSUBS(K).LE.(NCAS-(NVAR-K))) GOTO 100
	K=K-1
	JSUBS(K)=JSUBS(K)+1
	DO 50 I=K+1,NVAR
        JSUBS(I)=JSUBS(I-1)+1
 50     CONTINUE
        GOTO 10
 100    RETURN
        END
