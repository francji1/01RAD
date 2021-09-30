CC    ------------------------------------------------------------------
CC     *EQUAT* : SOLVES A SYSTEM OF LINEAR EQUATIONS.
CC    ------------------------------------------------------------------
      SUBROUTINE EQUAT(AM,MAXP,MAXP1,HVEC,MAXPP1,NA,NB,NERR)
      DIMENSION AM(MAXP,MAXP1)
      DOUBLE PRECISION HVEC(MAXPP1),TURN,SWAP,DETER
      JDM=MAXP
      DETER=1.0D0
      N=NA
      JMAT=N+NB
      JNK=0
      DO 10 J=1,JMAT
      JNK=(J-1)*MAXP
      DO 10 NC=1,MAXP
      JNK=JNK+1
      HVEC(JNK)=AM(NC,J)
 10   CONTINUE
      NZNDE=N-1
      LCLPL=-JDM
      DO 120 JHFD=1,N
      TURN=0.D0
      LCLPL=LCLPL+JDM+1
      JDEL=LCLPL+N-JHFD
      DO 40 JNCB=LCLPL,JDEL
      IF(DABS(HVEC(JNCB))-DABS(TURN)) 40,40,30
 30   TURN=HVEC(JNCB)
      LDEL=JNCB
 40   CONTINUE
      IF(TURN) 50,170,50
 50   IF(LDEL-LCLPL) 60,80,60
 60   DETER=-DETER
      LDEL=LDEL-JDM
      JNCB=LCLPL-JDM
      DO 70 JNCC=JHFD,JMAT
      LDEL=LDEL+JDM
      JNCB=JNCB+JDM
      SWAP=HVEC(JNCB)
      HVEC(JNCB)=HVEC(LDEL)
 70   HVEC(LDEL)=SWAP
 80   DETER=DETER*TURN
      IF (JHFD.EQ.N)  GOTO 120
      TURN=1./TURN
      JNCB=LCLPL+1
      DO 90 JNCC=JNCB,JDEL
 90   HVEC(JNCC)=HVEC(JNCC)*TURN
      JNCD=LCLPL
      JROW=JHFD+1
      DO 110 JNCB=JROW,N
      JNCD=JNCD+1
      JNCE=LCLPL
      JNCF=JNCD
      DO 100 JNCC=JROW,JMAT
      JNCE=JNCE+JDM
      JNCF=JNCF+JDM
 100  HVEC(JNCF)=HVEC(JNCF)-HVEC(JNCE)*HVEC(JNCD)
 110  CONTINUE
 120  CONTINUE
      NERR=0
      NEQA=N+1
      JBEGX=NZNDE*JDM+1
      DO 150 JNC=NEQA,JMAT
      JBEGX=JBEGX+JDM
      JENDX=JBEGX+N
      JBEGC=N*JDM+1
      JENDC=JBEGC+NZNDE
      DO 140 JNCB=1,NZNDE
      JENDX=JENDX-1
      JBEGC=JBEGC-JDM
      JENDC=JENDC-JDM-1
      HVEC(JENDX)=HVEC(JENDX)/HVEC(JENDC+1)
      SWAP=HVEC(JENDX)
      JNCD=JBEGX-1
      DO 130 JNCC=JBEGC,JENDC
      JNCD=JNCD+1
 130  HVEC(JNCD)=HVEC(JNCD)-HVEC(JNCC)*SWAP
 140  CONTINUE
      HVEC(JBEGX)=HVEC(JBEGX)/HVEC(1)
 150  CONTINUE
      JNC=-JDM
      JBEGX=NZNDE*JDM+1
      JENDX=JBEGX+NZNDE
      DO 160 JNCB=NEQA,JMAT
      JBEGX=JBEGX+JDM
      JENDX=JENDX+JDM
      JNC=JNC+JDM
      JNCD=JNC
      DO 160 JNCC=JBEGX,JENDX
      JNCD=JNCD+1
 160  HVEC(JNCD)=HVEC(JNCC)
      GOTO 180
 170  NERR=-1
 180  JNK=0
      DO 190 J=1,JMAT
      DO 190 NC=1,MAXP
      JNK=JNK+1
      AM(NC,J)=HVEC(JNK)
 190  CONTINUE
      RETURN
      END
CC    ------------------------------------------------------------------
CC     *LSL* : CALCULATES THE (REWEIGHTED) LS WHEN 'NVAR' EQUALS 1
CC             AND 'JCST'=0.
CC    ------------------------------------------------------------------
       SUBROUTINE LSL(NCAS,MAXN,X,Y,RESDU,A,FCKW,H,MAXP,MAXP1)
       DOUBLE PRECISION H(MAXP,MAXP1)
       DIMENSION A(MAXP),X(MAXP1,MAXN),Y(MAXN),RESDU(MAXN)
       AL=NCAS
       SXY=0.0
       SX2=0.0
       DO 10 JNC=1,NCAS
       SXY=SXY+X(1,JNC)*Y(JNC)*RESDU(JNC)
       SX2=SX2+(X(1,JNC)**2)*RESDU(JNC)
 10    CONTINUE
       A(1)=SXY/SX2
       FCKW=0.0
       DO 20 JNC=1,NCAS
 20    FCKW=FCKW+( (Y(JNC)-X(1,JNC)*A(1))**2 )*RESDU(JNC)
       H(1,1)=DBLE((FCKW/(AL-1.0D0))/SX2)
       RETURN
       END
CC     ----------------------------------------------------------------
CC     *FCN* : PUTS A ROW OF THE MATRIX X IN A VECTOR.
CC     ----------------------------------------------------------------
       SUBROUTINE FCN(K,M,F,X)
       DIMENSION F(K),X(M)
       DO 10 J=1,K
       F(J)=X(J)
 10    CONTINUE
       RETURN
       END
CC     ----------------------------------------------------------------
CC     *LSREG* : CALCULATES THE LEAST SQUARES REGRESSION ESTIMATES.
CC     ----------------------------------------------------------------
      SUBROUTINE LSREG(MAXP1,MAXN,MAXP,K,N,M,F,X,Y,W,DA,
     1 H,FCKW,HVEC,MAXPP1,JMISS)
      DIMENSION X(MAXP1,MAXN),F(K),Y(N),W(N),DA(K)
      DOUBLE PRECISION HVEC(MAXPP1),H(MAXP,MAXP1)
      DOUBLE PRECISION DFCKW,DFACT
      DOUBLE PRECISION DWJNC,DYJ,DFKA
      INTEGER JMISS(MAXP1)
      KPLUS=K+1
      DO 10 JNC=1,K
      DO 20 J=1,KPLUS
      H(JNC,J) = 0.D0
 20   CONTINUE
 10   CONTINUE
      ANUL=0.0
      DO 30 JNC=1,N
      CALL FCN(K,M,F,X(1,JNC))
      DWJNC = DBLE(W(JNC))
      ANUL=ANUL+W(JNC)
      DYJ = DBLE(Y(JNC))
      DO 40 KA=1,K
      DFKA = DBLE(F(KA))
      H(KA,K+1) = H(KA,K+1) + DWJNC*DFKA*DYJ
      DO 50 L=1,KA
      H(KA,L) = H(KA,L) + DWJNC*DFKA*DBLE(F(L))
 50   CONTINUE
 40   CONTINUE
 30   CONTINUE
      DO 60 J=1,K
      DO 70 JNC=1,J
      H(JNC,J)=H(J,JNC)
 70   CONTINUE
 60   CONTINUE
      CALL MATNV(H,MAXP,MAXP1,HVEC,MAXPP1,K,1,JMISS)
      MM=K+1
      FCKW = QLSRG(K,N,M,MAXP1,MAXN,MAXP,F,X,Y,W,H,MM)
      DO 80 JNC=1,K
      F(JNC)=H(JNC,K+1)
 80   CONTINUE
      DFCKW=DBLE(FCKW)
      ANK=ANUL-K
      DFACT=DBLE(ANK)
      DFACT=DFCKW/DFACT
      DO 90 JNC=1,K
      DO 100 J=1,K
      H(JNC,J)=H(JNC,J)*DFACT
 100  CONTINUE
 90   CONTINUE
      DO 110 JNC=1,K
      HDA=H(JNC,JNC)
      DA(JNC)=SQRT(HDA)
 110  CONTINUE
      RETURN
      END
CC    -----------------------------------------------------------------
CC    *QLSRG* : EVALUATES THE OBJECTIVE FUNCTION FOR LS REGRESSION.
CC    -----------------------------------------------------------------
      FUNCTION QLSRG(K,N,M,MAXP1,MAXN,MAXP,F,X,Y,W,H,MM)
      DIMENSION F(K),X(MAXP1,MAXN),Y(N),W(N)
      DOUBLE PRECISION Q,HSUM,H(MAXP,MAXP1)
      Q=0.D0
      DO 30 JNC=1,N
      CALL FCN(K,M,F,X(1,JNC))
      HSUM=0.D0
      DO 20 JNCB=1,K
 20   HSUM=H(JNCB,MM)*F(JNCB)+HSUM
 30   Q=(HSUM-Y(JNC))*(HSUM-Y(JNC))*W(JNC)+Q
      QLSRG = Q
      RETURN
      END
CC    -----------------------------------------------------------------
CC    *MATNV* : PERFORMS A MATRIX INVERSION.
CC    -----------------------------------------------------------------
      SUBROUTINE MATNV(AM,MAXP,MAXP1,HVEC,MAXPP1,NA,NB,JMISS)       
      DOUBLE PRECISION DETER,TURN,SWAP
      DOUBLE PRECISION HVEC(MAXPP1),AM(MAXP,MAXP1)
      INTEGER JMISS(MAXP1)
      DETER=1.0D0
      N=NA
      NPNB=N+NB
      JNK=0
      DO 10 J=1,NPNB
      JNK=(J-1)*MAXP
      DO 10 NC=1,MAXP
      JNK=JNK+1
      HVEC(JNK)=AM(NC,J)
 10   CONTINUE
      JDM=MAXP
      NMA=N-1
      JDELC=1-JDM
      DO 130 JHFD=1,N
      TURN=0.0D0
      JDELC=JDELC+JDM
      JDLA=JDELC+JHFD-1
      JDLB=JDELC +NMA
      DO 40 JNCB=JDLA,JDLB
      IF(DABS(HVEC(JNCB))-DABS(TURN)) 40,40,30
 30   TURN=HVEC(JNCB)
      LDEL=JNCB
 40   CONTINUE
      IF(TURN) 50,180,50
 50   JPAAL=LDEL-JDELC+1
      JMISS(JHFD)=JPAAL
      IF(JPAAL-JHFD) 80,80,60
 60   DETER=-DETER
      JPAAL=JPAAL-JDM
      JNCD=JHFD-JDM
      DO 70 JNC=1,NPNB
      JPAAL=JPAAL+JDM
      JNCD=JNCD+JDM
      SWAP=HVEC(JNCD)
      HVEC(JNCD)=HVEC(JPAAL)
 70   HVEC(JPAAL)=SWAP
 80   DETER=DETER*TURN
      TURN=1./TURN
      JNCD=JDELC+NMA
      DO 90 JNC=JDELC,JNCD
 90   HVEC(JNC)=-HVEC(JNC)*TURN
      HVEC(JDLA)=TURN
      JNCB=JHFD-JDM
      JPAAL=1-JDM
      DO 120 JNC=1,NPNB
      JPAAL=JPAAL+JDM
      JNCB=JNCB+JDM
      IF(JNC-JHFD) 100,120,100
 100  JCL=JPAAL+NMA
      SWAP=HVEC(JNCB)
      JNCD=JDELC-1
      DO 110 JNCC=JPAAL,JCL
      JNCD=JNCD+1
 110  HVEC(JNCC)=HVEC(JNCC)+SWAP*HVEC(JNCD)
      HVEC(JNCB)=SWAP*TURN
 120  CONTINUE
 130  CONTINUE
      DO 160 JNCB=1,N
      JHFD=N+1-JNCB
      LDEL=JMISS(JHFD)
      IF(LDEL-JHFD) 140,160,140
 140  JPAAL=(LDEL-1)*JDM+1
      JCL=JPAAL+NMA
      JDELC=(JHFD-1)*JDM+1-JPAAL
      DO 150 JNCC=JPAAL,JCL
      JNCD=JNCC+JDELC
      SWAP=HVEC(JNCC)
      HVEC(JNCC)=HVEC(JNCD)
 150  HVEC(JNCD)=SWAP
 160  CONTINUE
      GOTO 180
 180  JNK=0
      DO 190 J=1,NPNB
      DO 190 NC=1,MAXP
      JNK=JNK+1
      AM(NC,J)=HVEC(JNK)
 190  CONTINUE
      RETURN
      END
CC    -------------------------------------------------------------------
CC    *LCAT* : CALCULATES LOCATION ESTIMATES.
CC    -------------------------------------------------------------------
       SUBROUTINE LCAT(NCAS,NVAR,JCST,JPRT,NVAD,X,Y,RESDU,WEIGHTS,PREC,        
     1 MROB,XMED,XMAD,NZWE,AVW,JPLT,AW,JNDEX,MAXP1,MAXN,MVAL,LUA,LUB,
     1 LUC,JREG,JHEAD,FNAMEA,FNAMEB,FNAMEC,YNSAVE,LAB,JFMT,JVARS,YN,
     1 JPLACE,AW2,FACLMS,FACLTS)
       DIMENSION A(11),X(MAXP1,MAXN),Y(MAXN),RESDU(MAXN)
       DIMENSION XMED(MAXP1),XMAD(MAXP1)
       DIMENSION WEIGHTS(MAXN),AW(MAXN),AW2(MAXN)
       DIMENSION FACLMS(11),FACLTS(11)
       INTEGER JPLACE(MAXP1),JNDEX(MAXN)
       CHARACTER YN,YNSAVE
       CHARACTER*10 LAB(MAXP1)
       CHARACTER*30 FNAMEA,FNAMEB,FNAMEC
       CHARACTER*60 JFMT,JHEAD
       CHARACTER*3 NAME
       NAME='   '
       IF (MROB.EQ.1) THEN
	  NAME='LMS'
		      ELSE
          NAME='LTS'
       ENDIF
       WRITE(LUB,8000) 
       WRITE(LUB,8005) JHEAD
       WRITE(LUB,8010) NCAS
       IF (MROB.EQ.1) WRITE(LUB,8015)
       IF (MROB.EQ.2) WRITE(LUB,8016)
       IF (FNAMEA.NE.'CON') WRITE(LUB,8017) FNAMEA
       WRITE(LUB,8019) FNAMEB
       IF (JPRT.EQ.0) WRITE(LUB,8020)
       IF (JPRT.EQ.1) WRITE(LUB,8040)
       IF (JPLT.EQ.0) WRITE(LUB,8045)
       IF (JPLT.NE.0) WRITE(LUB,8050)
       IF (MVAL.EQ.0) WRITE(LUB,8055)
       IF (MVAL.EQ.1) WRITE(LUB,8056)
       DO 5 J=1,11
 5     A(J)=0.0
       IF (NCAS.LE.2) THEN
	  WRITE(*,8070) NCAS
	  STOP
       ENDIF
       IF (FNAMEA.EQ.'CON') WRITE(*,8060)
       DO 40 JNC=1,NCAS
       JNDEX(JNC)=JNC
       IF (FNAMEA.EQ.'CON'.AND.JNC.EQ.1) WRITE(*,8080) JNC
       IF (FNAMEA.EQ.'CON'.AND.JNC.NE.1) WRITE(*,8090) JNC
       IF (YN.NE.'Y') GOTO 30
       READ(LUA,*) (AW(J),J=1,JVARS)
       JH=JPLACE(NVAD)
       Y(JNC)=AW(JH)
       IF (YNSAVE.EQ.'Y') WRITE(LUC,*) Y(JNC)
       GOTO 40
 30    READ(LUA,JFMT) (AW(J),J=1,JVARS)
       JH=JPLACE(NVAD)
       Y(JNC)=AW(JH)
       IF (YNSAVE.EQ.'Y') WRITE(LUC,JFMT) Y(JNC)
 40    X(1,JNC)=Y(JNC)
       IF (YNSAVE.EQ.'Y') CLOSE(LUC,STATUS='KEEP')
       IF (MVAL.NE.0) CALL SMISLOC(NCAS,MAXP1,MAXN,X,Y,JNDEX,NSTOP)
       IF (NCAS.LE.2) THEN
	  WRITE(*,8070) NCAS
	  STOP
       ENDIF
       IF (NSTOP.EQ.1) STOP
       IF (MVAL.NE.0) WRITE(LUB,8095) NCAS
       AL=NCAS
       ANVAR=NVAR
       CALL JQUANT(NCAS,NVAR,MROB,JQU,FACTOR,FACLMS,FACLTS)
       IF (JPRT.EQ.0) GOTO 60
       WRITE(LUB,8100)
       DO 50 JNC=1,NCAS
 50    WRITE(LUB,8110) JNC,Y(JNC)
 60    XMED(1)=AMDAN(AW,MAXN,Y,NCAS)
       DO 70 JNC=1,NCAS
       RESDU(JNC)=ABS(Y(JNC)-XMED(1))
 70    A(1)=A(1)+Y(JNC)
       XMAD(1)=AMDAN(AW,MAXN,RESDU,NCAS)
       XMAD(2)=XMAD(1)*1.4826
       WRITE(LUB,8120) XMED(1),XMAD(2)
       IF (ABS(XMAD(1)).LE.1.0E-12) THEN
	  WRITE(LUB,8125)
	  STOP
       ENDIF
       WRITE(LUB,8130)
       A(1)=A(1)/AL
       DO 80 JNC=1,NCAS
 80    A(2)=A(2)+(Y(JNC)-A(1))**2
       A(2)=SQRT(A(2)/(AL-1.0D0))
       WRITE(LUB,8140) A(1),A(2)
       JREG=1
       CALL RDUAL(A,0,NVAD,NCAS,NVAR,JCST,JPRT,NVAD,LUB,PREC,JREG,
     1 X,Y,RESDU,WEIGHTS,XMED,XMAD,JPLT,AW,JNDEX,
     1 MAXP1,MAXN,JHEAD,LAB,MROB)
       WRITE(LUB,8130)
       CALL RANGS(Y,NCAS)
       IF (MROB.EQ.1) THEN
         CALL LMSLOC(Y,NCAS,JQU,SLUTN,BSTD,PREC,AW2,FACTOR)
         FINITF=-10.0/(AL-ANVAR)*(JQU/AL)+(10.0/(AL-ANVAR)+1.0)
         BSTD=BSTD*FINITF
	 WRITE(LUB,8152) JQU
                      ELSE
         CALL LTSLOC(Y,NCAS,JQU,SLUTN,BSTD,AW,AW2,MAXN,FACTOR)
	 WRITE(LUB,8157) JQU
       ENDIF
       JBREAK=NBREAK(JQU,NCAS,NVAR)
       JDEFAUL=(NCAS+NVAR+1)/2
       IF (JBREAK.EQ.NBREAK(JDEFAUL,NCAS,NVAR)) THEN
          WRITE(LUB,8165) JBREAK,NCAS
						ELSE
	  WRITE(LUB,8160) JBREAK
       ENDIF
       WRITE(LUB,8150) NAME,SLUTN,NAME,BSTD
       A(1)=SLUTN
       A(2)=BSTD
CC-----DETERMINATION OF THE WEIGHTS
       NZWE=0
       DO 100 JNC=1,NCAS
       Y(JNC)=X(1,JNC)
       RESDU(JNC)=(Y(JNC)-A(1))/A(2)
       IF (ABS(RESDU(JNC)).LT.2.5) THEN
	  WEIGHTS(JNC)=1.0
	  NZWE=NZWE+1
				   ELSE
	  WEIGHTS(JNC)=0.0
       ENDIF
 100   CONTINUE
       AVW=(NZWE*1.0)/AL
       AL=NZWE
CC-----FINAL SCALE ESTIMATE
       A(2)=0
       DO 105 JNC=1,NCAS
 105   A(2)=A(2)+((Y(JNC)-A(1))**2)*WEIGHTS(JNC)
       A(2)=SQRT(A(2)/(AL-1.0))
       WRITE(LUB,8155) NAME,A(2)
       JREG=2
       CALL RDUAL(A,1,NVAD,NCAS,NVAR,JCST,JPRT,NVAD,LUB,PREC,JREG,
     1 X,Y,RESDU,WEIGHTS,XMED,XMAD,JPLT,AW,JNDEX,
     1 MAXP1,MAXN,JHEAD,LAB,MROB)
       WRITE(LUB,8135)
       IF (NZWE.LE.2) THEN
	  WRITE(*,8170) NZWE
	  STOP
       ENDIF
       WRITE(LUB,8290) NCAS,JQU
       IF (MROB.EQ.1) THEN
	  WRITE(LUB,8300)
		      ELSE
          WRITE(LUB,8305)
       ENDIF
       WRITE(LUB,8310) NAME,NAME
       WRITE(LUB,8130)
       A(1)=0.0
       A(2)=0.0
       DO 110 JNC=1,NCAS
 110   A(1)=A(1)+Y(JNC)*WEIGHTS(JNC)
       A(1)=A(1)/AL
       DO 120 JNC=1,NCAS
 120   A(2)=A(2)+((Y(JNC)-A(1))**2)*WEIGHTS(JNC)
       A(2)=SQRT(A(2)/(AL-1.0))
       WRITE(LUB,8180) A(1),A(2)
       WRITE(LUB,8190) NZWE
       WRITE(LUB,8200) AVW
       JREG=3
       CALL RDUAL(A,2,NVAD,NCAS,NVAR,JCST,JPRT,NVAD,LUB,PREC,JREG,
     1 X,Y,RESDU,WEIGHTS,XMED,XMAD,JPLT,AW,JNDEX,
     1 MAXP1,MAXN,JHEAD,LAB,MROB)
       WRITE (LUB,8130)
       WRITE(*,8210)
       IF (YNSAVE.EQ.'Y') WRITE(*,8220) FNAMEC
       IF (FNAMEA.NE.'CON') WRITE(*,8230) FNAMEA
       IF (FNAMEB.NE.'CON'.AND.FNAMEB.NE.'PRN') WRITE(*,8240) FNAMEB
       IF (JPRT.NE.10) STOP
 8000  FORMAT(1X,
     1  '*******************************************'
     1 /' * ROBUST ESTIMATION OF LOCATION AND SCALE *'/
     1  ' *******************************************'///)
 8005  FORMAT(/' Title:   ',A60)
 8010  FORMAT(' Number of cases in the data set: ',I5)
 8015  FORMAT(' You have chosen the least median of squares '
     1 '(LMS) method.')
 8016  FORMAT(' You have chosen the least trimmed squares '
     1 '(LTS) method.')
 8017  FORMAT(' Your data reside in the file: ',A30) 
 8019  FORMAT(' This output is written in the file: ',A30)
 8020  FORMAT(' You have chosen small output.')
 8040  FORMAT(' You have chosen large output.')
 8045  FORMAT(' You have chosen not to plot the residuals.')
 8050  FORMAT(' You have chosen to plot residuals versus their index.')
 8055  FORMAT(' The data are assumed not to have missing values.')
 8056  FORMAT(' Any case with a missing value will be deleted.')
 8070  FORMAT(16h There are only ,I4,25h cases. The analysis must,
     1 12h be stopped./)
 8060  FORMAT(//24h Please enter your data./)
 8080  FORMAT(1X,26h The data for case number ,I4,3H : ,$)
 8090  FORMAT(26h The data for case number ,I4,3H : ,$)
 8095  FORMAT(/' After treatment of missing values, ',I4,
     1 ' cases remain.')      
 8100  FORMAT(/22h The observations are:/)
 8110  FORMAT(I5,2X,F10.4)
 8120  FORMAT(//11h The median,8X,2H= ,F11.4,2X,
     1 19h The MAD (x 1.4826),10X,2H= ,F11.4/)
 8125  FORMAT(/38h More than half of the data are equal.//)
 8130  FORMAT(/1X,78(1H*)/)
 8135  FORMAT(/1X,78(1H-)/)
 8140  FORMAT(//9h The mean,10X,2H= ,F11.4,2X,
     1 19h Standard deviation,10X,2H= ,F11.4//)
 8150  FORMAT(//1X,A3,' location      = ',F11.4,2X,
     1 ' Preliminary ',A3,' scale       = ',F11.4)
 8155  FORMAT(34X,' Final ',A3,' scale estimate    = ',F11.4/)
 8152  FORMAT(' The program minimizes the ',I4,'th ordered squared',
     1 ' residual,')
 8157  FORMAT(' The program minimizes the sum of the ',I4,' smallest',
     1 ' squared residuals,')
 8160  FORMAT(' hence its breakdown value is ',I2,'%.')
 8165  FORMAT(' hence its breakdown value is ',I2,'% (= highest'
     1 ' possible value '/' for n = ',I4,' cases).')
 8290  FORMAT(/' Corresponding formulas: (for this data set ',
     1 'n = ',I5,', h = ',I5,')')
 8300  FORMAT(//
     1 ' LMS objective = obj(y ) = minimum |y - theta|'/
     1 '                      i     theta    i        h:n')
 8305  FORMAT(//
     1 '                                              | 1   h ',
     1 '            2   |'/
     1 ' LTS objective = obj(y ) = minimum squareroot | -  sum',
     1 ' |y - theta|    |'/
     1 '                      i     theta             | h  j=1',
     1 '   i        j:n |')
 8310  FORMAT(//
     1 ' Preliminary ',A3,' scale = constant*obj(y )'/
     1 '                                       i '///
     1 '                                       |    n       2   |'/
     1 '                                       |   sum  w *r    |'/
     1 '                                       |   i=1   i  i   |'/
     1 ' Final ',A3,' scale estimate = squareroot | -------------- |'/
     1 '                                       |    n           |'/
     1 '                                       |  (sum w ) - 1  |'/
     1 '                                       |   i=1  i       |')
 8170  FORMAT(/16h There are only ,I4,28h cases with non-zero weight./
     1 30h The analysis must be stopped./)
 8180  FORMAT(/' Weighted mean     = ',F11.4,2X,
     1 ' Weighted standard deviation = ',F11.4/)
 8190  FORMAT(/10h There are,2X,I4,30h points with non-zero weight. )
 8200  FORMAT(/' Average weight    = ',F8.6/)
 8210  FORMAT(////41h The run has been executed successfully.  //)
 8220  FORMAT(/' The data are saved in the file: ',A30)
 8230  FORMAT(' The data has been read from the file: ',A30)
 8240  FORMAT(' The output has been written in the file: ',A30)
       RETURN
       END
CC     ----------------------------------------------------------------
CC     *SMISLOC* : HANDLING OF MISSING VALUES IN THE CASE OF LOCATION
CC     ----------------------------------------------------------------
       SUBROUTINE SMISLOC(NCAS,MAXP1,MAXN,X,Y,JNDEX,NSTOP)
       DIMENSION X(MAXP1,MAXN),Y(MAXN)
       INTEGER JNDEX(MAXN)
       MAXM=(NCAS*4)/5
       JND=0
       JCAS=0
CC-----GIVE THE MISSING VALUE CODE
       WRITE(*,8000)
  5    READ(*,*,ERR=5)CODE
       DO 10 JNC=1,NCAS
       IF (Y(JNC).EQ.CODE) THEN
	  JND=JND+1
			   ELSE
	  JCAS=JCAS+1
	  X(1,JCAS)=Y(JNC)
	  JNDEX(JCAS)=JNC
       ENDIF
 10    CONTINUE
       NCAS=JCAS
       WRITE(*,8010) NCAS
       IF (JND.GT.MAXM) THEN
	  WRITE (*,8020)
	  NSTOP=1
	  RETURN
       ENDIF
       DO 20 JNC=1,NCAS
	 Y(JNC)=X(1,JNC)
 20    CONTINUE
 8000  FORMAT(/46h Please enter the value of this variable which/
     1 51h has to be interpreted as the missing value code : $)
 8010  FORMAT(/' After treatment of missing values,'I4,' cases remain.')      
 8020  FORMAT(/52h More than 80 percent of the cases had to be deleted/
     1 61h because of the missing values. The analysis will be stopped.)
       RETURN
       END
CC     -------------------------------------------------------------------
CC     *MOVE* : MOVES A CHARACTER FROM LEFT TO RIGHT
CC              (USED FOR ADJUSTING VARIABLE LABELS TO THE RIGHT)
CC     -------------------------------------------------------------------
       SUBROUTINE MOVE(KARAK)
       CHARACTER KARAK(10)
       NB=0
       DO 10 J=1,10
       JPL=11-J
       IF (KARAK(JPL).EQ.' ') NB=NB+1
       IF((JPL.NE.1.AND.KARAK(JPL-1).NE.' ').OR.(NB.EQ.10))GOTO 20
 10    CONTINUE
 20    IF(NB.EQ.10.OR.NB.EQ.0) RETURN
       NBM=10-NB
       DO 30 J=1,NBM
       JPB=NBM+1-J
       JPL=11-J
       KARAK(JPL)=KARAK(JPB)
       KARAK(JPB)=' '
 30    CONTINUE
       RETURN
       END
CC     ---------------------------------------------------------------------
CC    *PVAL* : THIS IS A FORTRAN VERSION OF THE LACKRITZ ALGORITHM
CC             (AMERICAN STATISTICIAN 1984, VOL. 38, P.312-314)
CC             FOR CALCULATING THE ONE-SIDED TAIL AREA OF AN F-DISTRIBUTION
CC             WITH N1 AND N2 DEGREES OF FREEDOM. IMPLEMENTED BY
CC             J. VAN SOEST AND B. VAN ZOMEREN, JUNE 1986.
CC     ---------------------------------------------------------------------
       FUNCTION PVAL(F,N1,N2)
       DOUBLE PRECISION F,PI
       DOUBLE PRECISION PVAL,SUM,T,TINV,T1,TINV1,DSUM,T5,A,B
       LOGICAL ODD
       DATA PI/3.14159265/
       IF(F.GT.(0.000001))GOTO 5
       PVAL=1.0D0
       RETURN
  5    T=N1*F/N2
       TINV=1/T
       T1=T+1
       TINV1=TINV+1
       IF (.NOT.ODD(N1) .AND. .NOT. ODD(N2)) THEN
	  M1=N1/2
	  M2=N2/2
	  PVAL=SUM(M1-1,DBLE(M2-1),T/T1)/DEXP(DBLE(M2)*DLOG(T1))
       ENDIF
       IF (.NOT.ODD(N1).AND.ODD(N2)) THEN
	   M1=N1/2
	   M2=(N2-1)/2
	   A=SUM(M1-1,DBLE(M2-0.5),T/T1)
	   PVAL=A/DEXP(DBLE(M2+0.5)*DLOG(T1))
       ENDIF
       IF (ODD(N1).AND..NOT.ODD(N2)) THEN
	   M1=(N1-1)/2
	   M2=N2/2
	   A=SUM(M2-1,DBLE(M1-0.5),TINV/TINV1)
	   PVAL=1-A/DEXP(DBLE(M1+0.5)*DLOG(TINV1))
       ENDIF
       IF (ODD(N1).AND.ODD(N2)) THEN
	   M1=(N1-1)/2
	   M2=(N2-1)/2
	   A=0.0
	   IF (M1.GT.0) THEN
	      DSUM=2*(DSQRT(T))/(PI*T1)
	      DO 10 I=1,M2
		 DSUM=DSUM*DBLE(I)/(T1*DBLE(I-0.5))
 10		 CONTINUE
	      A=DSUM
	      DO 20 I=2,M1
		 DSUM=DSUM*T*2*DBLE(M2+I-1)/(T1*DBLE(2*I-1))
		 A=A+DSUM
 20		 CONTINUE
	   ENDIF
	   B=0.0
	   IF (M2.GT.0) THEN
	      DSUM=DSQRT(TINV)*2/(PI*TINV1)
	      B=DSUM
	      T5=2*TINV/TINV1
	      DO 30 I=2,M2
	      DSUM=DSUM*T5*DBLE(I-1)/DBLE(2*I-1)
	      B=B+DSUM
 30	      CONTINUE
	   ENDIF
	   PVAL=A-B+(2/PI)*DATAN(DSQRT(TINV))
       ENDIF
       RETURN
       END
CC     -------------------------------------------------------------------
CC     *PTVAL* : FUNCTION (BASED ON THE FUNCTION PVAL) FOR CALCULATING
CC               THE TWO-SIDED TAIL AREA OF A STUDENT DISTRIBUTION
CC               WITH N DEGREES OF FREEDOM
CC     -------------------------------------------------------------------
       FUNCTION PTVAL(TVAL,N)
       DOUBLE PRECISION PVAL,PTVAL
       DOUBLE PRECISION TVAL
       PTVAL=PVAL(TVAL*TVAL,1,N)
       RETURN
       END
CC     -------------------------------------------------------------------
CC     *SUM* : AUXILIARY FUNCTION (FOR FUNCTION PVAL)
CC     -------------------------------------------------------------------
       FUNCTION SUM(N,F1,F2)
       DOUBLE PRECISION F1,F2,DSUM,TSUM,SUM
       DSUM=1
       TSUM=1
       IF (N.NE.0) THEN
       DO 10 I=1,N
	  DSUM=DSUM*(F1+I)*F2/I
	  TSUM=TSUM+DSUM
 10    CONTINUE
       ENDIF
       SUM=TSUM
       RETURN
       END
CC     -------------------------------------------------------------------
CC     *ODD* : AUXILIARY FUNCTION (FOR FUNCTION PVAL)
CC     -------------------------------------------------------------------
       FUNCTION ODD(N)
       LOGICAL ODD
       ODD=.TRUE.
       IF(2*(N/2).EQ.N) ODD=.FALSE.
       RETURN
       END
CC     -------------------------------------------------------------------
CC     *GRAF* : PERFORMS RESIDUAL PLOTS AND/OR A PLOT OF THE
CC              OBSERVATIONS IN THE TWO-DIMENSIONAL CASE.
CC     -------------------------------------------------------------------
       SUBROUTINE GRAF(X,Y,NCAS,JPLXY,JPL2,JPLT,JNDEX,MAXN,LUB,JREG,
     1 JHEAD,MAXP1,NVAD,LAB,MROB)
       DIMENSION X(NCAS),Y(NCAS)
       INTEGER JNDEX(MAXN),MGRAF(41,51)
       CHARACTER JDRAW(51),NUM(14)
       CHARACTER*10 LAB(MAXP1)
       CHARACTER*60 JHEAD
       LOGICAL ODD
       NUM(1)=' '
       NUM(2)='1'
       NUM(3)='2'
       NUM(4)='3'
       NUM(5)='4'
       NUM(6)='5'
       NUM(7)='6'
       NUM(8)='7'
       NUM(9)='8'
       NUM(10)='9'
       NUM(11)='*'
       NUM(12)='-'
       NUM(13)='+'
       NUM(14)=' '
       JXSCA=51
       JYSCA=41
       AFR=10.0E-02
       DO 10 J=1,JXSCA
       DO 10 K=1,JYSCA
 10    MGRAF(K,J)=0
       IF (JPLT.EQ.2.AND.JPLXY.NE.1) X(1)=JNDEX(1)
       XMIN=X(1)
       XMAX=X(1)
       YMAX2=2.5
       YKLE2=-2.5
       YKLE=Y(1)
       YMAX=Y(1)
       DO 20 JNC=1,NCAS
       IF (JPLT.EQ.2.AND.JPLXY.NE.1) X(JNC)=JNDEX(JNC)
       IF (X(JNC).GT.XMAX) XMAX=X(JNC)
       IF (X(JNC).LT.XMIN) XMIN=X(JNC)
       IF (Y(JNC).GT.YMAX) YMAX=Y(JNC)
       IF (Y(JNC).LT.YKLE) YKLE=Y(JNC)
 20    CONTINUE
       IF((YMAX-YKLE).LE.AFR.AND.JPLXY.EQ.1)THEN
         YKLE=YKLE-1.0
         YMAX=YMAX+1.0
       ENDIF
       IF (NVAD.EQ.2.AND.JPLXY.EQ.1) THEN
       IF(0.0.GT.XMAX)XMAX=0.0
       IF(0.0.LT.XMIN)XMIN=0.0
       IF(0.0.GT.YMAX)YMAX=0.0
       IF(0.0.LT.YKLE)YKLE=0.0
       ENDIF
       IF (YMAX.GT.2.5.OR.JPLXY.EQ.1.OR.(JPL2.EQ.0.AND.YKLE.NE.YMAX))
     1 YMAX2=YMAX
       IF (YKLE.LT.(-2.5).OR.JPLXY.EQ.1.OR.(JPL2.EQ.0.AND.YKLE.NE.YMAX))
     1 YKLE2=YKLE
       XSCA=JXSCA
       YSCA=JYSCA
       IF((XMAX-XMIN).LE.AFR)THEN
         XMIN=XMIN-1.0
         XMAX=XMAX+1.0
       ENDIF
       A=(XSCA-1.0)/(XMAX-XMIN)
       B=1.0-A*XMIN
       C=(YSCA-1.0)/(YMAX2-YKLE2)
       D=1.0-C*YKLE2
       IF (JPLXY.EQ.1) GOTO 50
       JNUL=INT(YSCA-D)
       IF ((JNUL.LE.1.OR.ABS(YMAX2).LT.AFR).AND.JPL2.NE.0) THEN
	  J2P=1
	  JNUL=2
	  J2M=3
	  GOTO 50
       ENDIF
       IF ((JNUL.GE.JYSCA.OR.ABS(YKLE2).LT.AFR).AND.JPL2.NE.0) THEN
	  J2P=JYSCA-2
	  JNUL=JYSCA-1
	  J2M=JYSCA
	  GOTO 50
       ENDIF
       IF (JPL2.EQ.0) THEN
	  IF (JNUL.LE.1.OR.ABS(YMAX2).LE.AFR) JNUL=1
	  IF (JNUL.GT.JYSCA.OR.ABS(YKLE2).LE.AFR) JNUL=JYSCA
       ENDIF
       IF ((JPL2.EQ.0.AND.YKLE.NE.YMAX)) GOTO 50
       J2P=INT(YSCA-(2.5*C+D))
       IF (JNUL.EQ.J2P) J2P=JNUL-1
       IF (ABS(YMAX2-2.5).LT.AFR) J2P=1
       IF (J2P.LE.0) THEN
	  IF (ABS(YMAX2-2.5).LT.AFR) THEN
	     J2P=1
				     ELSE
	     J2P=2
	  ENDIF
       ENDIF
       IF (J2P.GT.JYSCA) J2P=JYSCA-2
       IF (ABS(YKLE2-2.5).LT.AFR) J2P=JYSCA-2
       J2M=2*JNUL-J2P
       IF (J2M.LE.0.OR.ABS(YMAX2+2.5).LT.AFR) THEN
	  J2P=1
	  JNUL=2
	  J2M=3
	  GOTO 50
       ENDIF
       IF (J2M.GT.JYSCA.OR.ABS(YKLE2+2.5).LT.AFR) THEN
	  J2M=JYSCA
	  JS=J2M+J2P
	  IF (ODD(JS)) THEN
	     J2P=J2P+1
	  ENDIF
	  JNUL=(J2M+J2P)/2
       ENDIF
 50    DO 110 JNC=1,NCAS
       XJNC=X(JNC)
       YJNC=Y(JNC)
 60    JX=INT(XJNC*A+B)
       IF (JX.LE.0) JX=1
       IF (JX.GT.JXSCA) JX=JXSCA
       IF(.NOT.(ABS(YJNC-YMAX2).LT.AFR.OR.ABS(YJNC-YKLE2).LT.AFR))
     1 GOTO 70
       IF (ABS(YJNC-YMAX2).LT.AFR) JEY=1
       IF (ABS(YJNC-YKLE2).LT.AFR) JEY=JYSCA
       IF (ABS(YJNC).LT.AFR.AND.JPLXY.EQ.0) JEY=JNUL
       GOTO 90
 70    JEY=INT(YSCA-(YJNC*C+D))
       IF (JPLXY.NE.1) THEN
	 IF (ABS(YJNC).LT.AFR) JEY=JNUL
	 IF (JPL2.EQ.0) GOTO 80
	 IF (JEY.EQ.J2P.AND.YJNC.LT.(2.5-AFR)) JEY=J2P+1
	 IF (JEY.EQ.J2P.AND.YJNC.GT.(2.5+AFR)) JEY=J2P-1
	 IF (JEY.EQ.J2M.AND.YJNC.LT.(-2.5-AFR)) JEY=J2M+1
	 IF (JEY.EQ.J2M.AND.YJNC.GT.(-2.5+AFR)) JEY=J2M-1
	 IF (ABS(YJNC-2.5).LT.AFR.OR.(JEY.GT.J2P.AND.YJNC.GT.2.5)
     1	    .OR.(JEY.LT.J2P.AND.YJNC.LT.2.5)) JEY=J2P
	 IF (ABS(YJNC+2.5).LT.AFR.OR.(JEY.GT.J2M.AND.YJNC.GT.-2.5)
     1	    .OR.(JEY.LT.J2M.AND.YJNC.LT.-2.5)) JEY=J2M
       ENDIF
 80    IF (JEY.LE.0) JEY=1
       IF (JEY.GT.JYSCA) JEY=JYSCA
 90    MGRAF(JEY,JX)=MGRAF(JEY,JX)+1
       IF(NUM(14).NE.'0') GOTO 100
       MGRAF(JEY,JX)=-20
       GOTO 120
 100   IF (.NOT.(JNC.EQ.NCAS.AND.JPLXY.EQ.1.AND.NVAD.EQ.2)) GOTO 110
       XJNC=0.0
       YJNC=0.0
       NUM(14)='0'
       GOTO 60
 110   CONTINUE
 120   IF (JREG.EQ.0) WRITE(LUB,8000) JHEAD
       IF (JREG.EQ.1) WRITE(LUB,8010) JHEAD
       IF (JREG.EQ.2.AND.MROB.EQ.1) WRITE(LUB,8020) JHEAD
       IF (JREG.EQ.2.AND.MROB.EQ.2) WRITE(LUB,8025) JHEAD
       IF (JREG.EQ.3) WRITE(LUB,8030) JHEAD
       IF (JPLXY.EQ.0.AND.JPL2.NE.0) WRITE(LUB,8050)
       IF (JPLXY.EQ.0.AND.JPL2.EQ.0) WRITE(LUB,8060)
       IF (JPLXY.EQ.1) WRITE(LUB,8070) LAB(NVAD)
       WRITE(LUB,8075)
       DO 240 J=1,JYSCA
       IF (JPLXY.EQ.1) GOTO 160
       IF(J.NE.JNUL) GOTO 140
       DO 130 K=1,JXSCA
       MJK=MGRAF(J,K)+1
       IF (MJK.EQ.1) JDRAW(K)=NUM(12)
       IF (MJK.GT.10) JDRAW(K)=NUM(11)
       IF (MJK.GE.2.AND.MJK.LE.10) JDRAW(K)=NUM(MJK)
 130   CONTINUE
       GOTO 180
 140   IF ((JPL2.EQ.0.AND.YKLE.NE.YMAX)) GOTO 160
       IF (J.NE.J2P.AND.J.NE.J2M) GOTO 160
       DO 150 K=1,JXSCA
       MJK=MGRAF(J,K)+1
       IF (MJK.EQ.1) JDRAW(K)=NUM(13)
       IF (MJK.GT.10) JDRAW(K)=NUM(11)
       IF (MJK.GE.2.AND.MJK.LE.10) JDRAW(K)=NUM(MJK)
 150   CONTINUE
       GOTO 180
 160   DO 170 K=1,JXSCA
       MJK=MGRAF(J,K)+1
       IF(MJK.GT.10) JDRAW(K)=NUM(11)
       IF(MJK.LE.10) JDRAW(K)=NUM(MJK)
       IF(MJK.LT.0) JDRAW(K)=NUM(14)
 170   CONTINUE
 180   JREST=MOD(J,5)
       IF (JPLXY.EQ.1) GOTO 190
       IF ((JPL2.EQ.0.AND.YKLE.NE.YMAX)) GOTO 190
       IF (J.NE.J2P) GOTO 190
       IF (JREST.EQ.1) WRITE(LUB,8080)(JDRAW(K),K=1,JXSCA)
       IF (JREST.NE.1) WRITE(LUB,8085)(JDRAW(K),K=1,JXSCA)
       GOTO 240
 190   IF (J.NE.1) GOTO 200
       WRITE(LUB,8130) YMAX2,(JDRAW(K),K=1,JXSCA)
       GOTO 240
 200   IF (JPLXY.EQ.1) GOTO 220
       IF (J.NE.JNUL) GOTO 210
       IF (JREST.EQ.1) WRITE(LUB,8090)(JDRAW(K),K=1,JXSCA)
       IF (JREST.NE.1) WRITE(LUB,8100)(JDRAW(K),K=1,JXSCA)
       GOTO 240
 210   IF (J.NE.J2M.OR.(JPL2.EQ.0.AND.YKLE.NE.YMAX)) GOTO 220
       IF (JREST.EQ.1) WRITE(LUB,8110) (JDRAW(K),K=1,JXSCA)
       IF (JREST.NE.1) WRITE(LUB,8120) (JDRAW(K),K=1,JXSCA)
       GOTO 240
 220   IF(J.NE.JYSCA) GOTO 230
       WRITE(LUB,8130) YKLE2,(JDRAW(K),K=1,JXSCA)
       GOTO 240
 230   IF (JREST.EQ.1) WRITE(LUB,8140)(JDRAW(K),K=1,JXSCA)
       IF (JREST.NE.1) WRITE(LUB,8150)(JDRAW(K),K=1,JXSCA)
 240   CONTINUE
       WRITE(LUB,8075)
       IF (JPLT.NE.2.OR.JPLXY.EQ.1) WRITE(LUB,8160) XMIN,XMAX
       IF ((JPLT.EQ.1.OR.JPLT.EQ.3).AND.JPLXY.EQ.0)
     1 WRITE(LUB,8170) LAB(NVAD)
       IF (JPLXY.EQ.1) WRITE(LUB,8180) LAB(1)
       IF (JPLT.NE.2.OR.JPLXY.EQ.1) GOTO 250
       J1=XMIN
       JN=XMAX
       WRITE(LUB,8190) J1,JN
       WRITE(LUB,8200)
 8000  FORMAT(//16X,A60)
 8010  FORMAT(//16X,A60//29X,32H--- L E A S T  S Q U A R E S ---)
 8020  FORMAT(//16X,A60//
     1 20X,'--- L E A S T  M E D I A N  O F  S Q U A R E S ---')
 8025  FORMAT(//16X,A60//
     1 22X,'--- L E A S T  T R I M M E D  S Q U A R E S ---')
 8030  FORMAT(//16X,A60//29X,'--- R E W E I G H T E D',3X,7HL S ---)
 8050  FORMAT(/'   residual/scale',1X,3HI-+,10(5H----+),2H-I)
 8060  FORMAT(/7X,8hresidual,3X,3HI-+,10(5H----+),2H-I)
 8070  FORMAT(/6X,'  observed'/6X,A10,2X,3HI-+,10(5H----+),2H-I)
 8075  FORMAT(18X,1HI,53X,1HI)
 8080  FORMAT(14X,3H2.5,1X,2H++,51A1,2H++)
 8085  FORMAT(14X,3H2.5,1X,2HI+,51A1,2H+I)
 8090  FORMAT(14X,3H0.0,1X,2H+-,51A1,2H-+)
 8100  FORMAT(14X,3H0.0,1X,2HI-,51A1,2H-I)
 8110  FORMAT(13X,4H-2.5,1X,2H++,51A1,2H++)
 8120  FORMAT(13X,4H-2.5,1X,2HI+,51A1,2H+I)
 8130  FORMAT(6X,E11.4,1X,1H+,1X,51A1,1X,1H+)
 8140  FORMAT(18X,1H+,1X,51A1,1X,1H+)
 8150  FORMAT(18X,1HI,1X,51A1,1X,1HI)
 8160  FORMAT(18X,3HI-+,10(5H----+),2H-I/10X,E11.4,46X,E11.4)
 8170  FORMAT(/49X,11h estimated ,A10//)
 8180  FORMAT(/54X,9hobserved ,A10//)
 8190  FORMAT(18X,3HI-+,10(5H----+),2H-I/17X,I4,46X,I4)
 8200  FORMAT(/48X,25h index of the observation//)
 250   RETURN
       END
CC     -----------------------------------------------------------------
CC     *JQUANT* : ASKS FOR THE QUANTILE TO BE MINIMIZED
CC     -----------------------------------------------------------------
       SUBROUTINE JQUANT(NCAS,NVAR,MROB,JQU,FACTOR,FACLMS,FACLTS)
       DIMENSION FACLMS(11),FACLTS(11)
       CHARACTER YN
CC-----OPTIMAL BREAKDOWN VALUE IF H=(NCAS+NVAR+1)/2
       JDEFAUL=(NCAS+NVAR+1)/2
       JQU=JDEFAUL
       IF (MROB.EQ.1) THEN
         WRITE(*,8000) JDEFAUL
		      ELSE
         WRITE(*,8010) JDEFAUL
       ENDIF
       JMIN=(NCAS/2)+1
       JMAX=MAX((3*NCAS/4)+(NVAR+1)/4,JDEFAUL)
       IF (JMIN.NE.JMAX) THEN
         WRITE(*,8020) JDEFAUL
 10      READ(*,8100) YN
         IF (YN.EQ.'y') YN='Y'
         IF (YN.EQ.'n') YN='N'
         IF (YN.NE.'Y'.AND.YN.NE.'N') THEN
	   WRITE(*,8030)
	   GOTO 10
         ENDIF
       ENDIF
       IF (YN.EQ.'N') THEN
         JBRMIN=NBREAK(JMIN,NCAS,NVAR)
         JBRMAX=NBREAK(JMAX,NCAS,NVAR)
	 WRITE(*,8040) JMIN,JBRMIN,JMAX,JBRMAX
 20      READ(*,8110) JQU
	 IF (JQU.LT.JMIN.OR.JQU.GT.JMAX) THEN
	   WRITE(*,8030)
	   GOTO 20
         ENDIF
       ENDIF
       NQUANT=MIN(NINT(REAL(((JQU*1.0/NCAS)-0.5)*40))+1,11)
       IF (MROB.EQ.1) FACTOR=FACLMS(NQUANT)
       IF (MROB.EQ.2) FACTOR=FACLTS(NQUANT)
 8000  FORMAT(//' The program will minimize the', I4,'th ordered'
     1 ' squared residual.')
 8010  FORMAT(//' The program will minimize the sum of the',I4,
     1 ' smallest squared residuals.')       
 8020  FORMAT(' Do you agree with the default choice of ',I4,' ?',
     1 ' Yes or no : ' $)
 8030  FORMAT(/' Not allowed ! Enter your choice again : '$)
 8040  FORMAT(' Give a number between ',I4,' (then the breakdown',
     1 ' value is ',I2,'%)'/
     1 '                   and ',I4,
     1 ' (then the breakdown value is ',I2,'%) : '$)
 8100  FORMAT(A1)
 8110  FORMAT(I4)
       RETURN
       END
CC     -------------------------------------------------------------------
CC     *NBREAK* : COMPUTES BREAKDOWN VALUE OF LMS/LTS        
CC     -------------------------------------------------------------------
       FUNCTION NBREAK(NQU,NCAS,NVAR)
       IF (NQU.LT.(NCAS+NVAR+1)/2) THEN
	  NBREAK=(NQU-NVAR+1)*100/NCAS
			       ELSE
          NBREAK=(NCAS-NQU+1)*100/NCAS
       ENDIF
       RETURN
       END
