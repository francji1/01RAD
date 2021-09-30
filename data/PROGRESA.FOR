$large
CC
CC     PROGRESS : PROGRAM FOR ROBUST REGRESSION
CC-----------------------------------------------------------------------------
CC-----LIST OF VARIABLES AND ARRAYS IN THIS PROGRAM
CC-----------------------------------------------------------------------------
CC     MAXN           = maximal number of cases
CC     MAXP           = maximal number of explanatory variables
CC     MAXP1          = MAXP+1 = maximal number of coefficients,
CC                               including intercept
CC     MAXPP1         = MAXP*(MAXP+1)
CC-----------------------------------------------------------------------------
CC     X(MAXP1,MAXN)  = matrix of the values of the explanatory
CC                      variables (transposed design matrix)
CC     Y(MAXN)        = vector with the values of the response variable
CC     RESDU(MAXN)    = vector for storing the residuals
CC     WEIGHTS(MAXN)  = weight assigned to each observation
CC     AA(MAXN)       = working vector
CC     AM(MAXN)       = working vector
CC     AW(MAXN)       = working vector
CC     AW2(MAXN)      = working vector
CC     A(MAXP+1)      = vector of regression coefficients  (ls or rls)
CC     DA(MAXP+1)     = vector with the stand. deviation of the coefficients
CC     COEFF(MAXP+1)  = vector of coefficients for the lms/lts
CC     XMED(MAXP+1)   = vector with the medians of all variables
CC     XMAD(MAXP+1)   = vector with median absolute deviations
CC     UHYB(MAXN)     = vector with the resistant diagnostics
CC     C(MAXP,MAXP+1) = matrix used for - storing the coefficients
CC                                        of the linear system to be solved
CC                                        at each replication
CC                                      - storing the spearman
CC                                        correlation coefficients
CC     H(MAXP,MAXP+1) = matrix used for - storing the variance-covariance
CC                                        matrix of the ls and rls coefficients
CC                                      - storing the pearson
CC                                        correlation coefficients
CC     HVEC(MAXPP1)   = auxiliary vector
CC-----------------------------------------------------------------------------
CC     JNDEX(MAXN)    = vector with the original index of the cases
CC     JSUBS(MAXP+1)  = vector - containing the index of the cases
CC                               considered in a random subsample
CC                             - counting the number of missing cases for
CC                               each variable
CC     VALMS(MAXP+1)  = vector with the "missing value codes"
CC     JMISS(MAXP+1)  = JMISS(J), (J=1,NVAR+1) is 1 if var(j) contains missing
CC                      values and is 0 otherwise
CC     JBEST(MAXP)    = vector containing the index of the cases
CC                      which yield the "best" lms/lts solution
CC     JPLACE(MAXP+1) = vector containing the positions of the variables
CC                      used in the analysis (from the total data set)
CC     LAB(MAXP+1)    = vector with the label of each variable
CC     JREPTAB(MAXP+1)= default number of random subsets (depending on
CC                      number of regressors)
CC     JREPLOW(MAXP+1)= number of observations which allow to take all
CC                      combinations (depending on number of regressors)
CC     JREPHI(MAXP+1) = number of observations which do not allow to
CC                      take all combinations
CC     FACLMS(11)     = asympt. consistency factors for lms scale estimation
CC     FACLTS(11)     = asympt. consistency factors for lts scale estimation
CC-----------------------------------------------------------------------------
CC-----LIST OF MOST IMPORTANT PARAMETERS:
CC-----------------------------------------------------------------------------
CC     FNAMEA         = name of the file where the input has to be read
CC     FNAMEB         = name of the file where the output has to be written
CC     FNAMEC         = name of the file where the data has to be saved
CC     LUA            = logical unit for 'FNAMEA'
CC     LUB            = logical unit for 'FNAMEB'
CC     LUC            = logical unit for 'FNAMEC'
CC     NVAR           = number of coefficients (including the constant term
CC                      if there is one)
CC     ANVAR	      = real value of NVAR
CC     NFAC	      = NVAR-1
CC     NVAD	      = NVAR+1
CC     NCAS	      = number of cases
CC     AL	      = real value of ncas
CC     ALGO	      = character which is 'A' for drawing all subsets
CC			and 'R' for drawing random subsets
CC     NZWE	      = number of non-zero weights
CC     RSQ	      = R squared (coefficient of determination)
CC     AVW	      = average weight
CC     FCKW	      = objective function value
CC     JCST	      = integer which is 1 in the case of a model with
CC			intercept and 0 otherwise
CC     JPLT	      = plot option (0=none, 1=residual, 2=index, 3=both)
CC     JPRT	      = print option (0=small, 1=medium-sized, 2=large)
CC     MVAL	      = option for missing values (0=none, 1=delete, 2=replace)
CC     YNSAVE	      = character (yes or no) needed for the interactive input
CC     JHEAD	      = title for the output
CC     PREC	      = precision of the calculations
CC     JADJ           = 0 if intercept is not adjusted, otherwise JADJ=1 
CC     JERD           = number of subsets giving singular system of equations
CC     JEY,JLP	      = integer variables used for counting the number of
CC			replications in the lms/lts algorithm
CC     JHYB           = 0 if half of the observations lie on a hyperplane
CC     JPLXY	      = option for xy plot (0=no xy plot, 1=otherwise)
CC     NREP	      = number of (random) subsets to be considered
CC     JREG	      = which regression ? (1=ls, 2=lms/lts, 3=rls)
CC     MROB           = which robust method ? (1=lms, 2=lts)
CC     JDIAG	      = option for diagnostics (0=no diagnostics, else 1)
CC     JQU	      = the quantile to be minimized
CC     JEDAUL         = default quantile which yields highest breakdown value
CC     NSTOP	      = 1 means stop the analysis, otherwise NSTOP=0
CC     LOCA           = 1 for univariate location/scale estimation
CC     BSTD           = univariate lms/lts scale estimate
CC     SLUTN          = univariate lms/lts location estimate
CC     FACTOR         = asympt. consistency factor for lms/lts scale 
CC     FINITF         = finite sample correction factor for lms/lts scale
CC     RDI            = robust distance
CC-----------------------------------------------------------------------------
CC-----LIST OF SUBROUTINES AND FUNCTIONS
CC-----------------------------------------------------------------------------
CC     SUBROUTINE WRDATA  :creates first part of the output
CC     SUBROUTINE WRDATB  :creates second part of the output
CC     SUBROUTINE PRTRLS  :prints the rls (or ls) results
CC     SUBROUTINE SUBLMS  :algorithm for calculating the lms/lts estimates
CC     SUBROUTINE PRTLMS  :prints the lms/lts results
CC     SUBROUTINE RDAT	  :interactive input with reading of the data
CC     FUNCTION JNTGR	  :transforms a character into an integer
CC     SUBROUTINE SMISSING:handles missing values
CC     SUBROUTINE SUBREP  :calculates the number of replications
CC			   for the lms/lts algorithm
CC     SUBROUTINE STATIS  :standardization of the observations
CC			   and calculation of some statistics
CC     SUBROUTINE CORR    :calculates spearman rank correlation coefficient
CC     FUNCTION PULL      :searches the kth value in a vector
CC     FUNCTION AMDAN     :calculates the median
CC     FUNCTION RSQU      :calculates coefficient of determination
CC                         of ls and rls
CC     SUBROUTINE RESTT   :calculates residuals and lms/lts scale
CC     SUBROUTINE RANGS   :sorts a vector
CC     SUBROUTINE RDUAL   :generates a table with observed and estimated
CC                         response, residuals and standardized residuals
CC                         and (if wanted) calls residual plots
CC     SUBROUTINE LMSLOC  :calculates univariate lms 
CC     SUBROUTINE LTSLOC  :calculates univariate lts 
CC     SUBROUTINE TRC     :retransformation of the variance-covariance matrix
CC     SUBROUTINE SCHCV   :prints the variance-covariance matrix
CC     SUBROUTINE RTRAN   :retransformation of the regression coefficients
CC     SUBROUTINE RANPN   :random number generator
CC     SUBROUTINE GENPN   :generates all combinations of p points out of n
CC     SUBROUTINE NCOMB   :computes number of combinations of k points out of n
CC     SUBROUTINE EQUAT   :solves a system of linear equations
CC     SUBROUTINE LSL     :ls or rls in the case p=1
CC     SUBROUTINE FCN     :puts a row of the matrix x in a vector
CC     SUBROUTINE LSREG   :calculates ls or rls regression
CC     FUNCTION QLSRG     :calculates ls or rls objective function
CC     SUBROUTINE MATNV   :matrix inversion
CC     SUBROUTINE LCAT    :treats the univariate location and scale estimators
CC     SUBROUTINE SMISLOC :treatment of missing values in the univariate case 
CC     SUBROUTINE MOVE    :moves characters to the right (in a label)
CC     FUNCTION PVAL      :calculates the p-value associated with an f-value
CC     FUNCTION PTVAL     :calculates the p-value associated with a t-value
CC     FUNCTION SUM       :function needed in PVAL
CC     FUNCTION ODD       :function needed in PVAL
CC     SUBROUTINE GRAF    :makes a scattergram, and residual and/or index plots
CC     SUBROUTINE JQUANT  :asks for quantile to be minimized
CC     FUNCTION NBREAK    :computes breakdown value of lms/lts
CC-----------------------------------------------------------------------------
CC
       CHARACTER YNSAVE,ALGO
       CHARACTER*30 FNAMEA,FNAMEB,FNAMEC
       CHARACTER*60 JHEAD
CC     ------------------------------------------------------------------------
CC     IF YOU LIKE TO CHANGE THE DIMENSIONS OF THE ARRAYS, THEN YOU
CC     ONLY HAVE TO ADAPT THE VALUES IN THE FOLLOWING LINES:
CC     ------------------------------------------------------------------------
       DIMENSION X(15,1000),Y(1000),RESDU(1000),UHYB(1000)
       DIMENSION AW(1000),AW2(1000),WEIGHTS(1000)
       DIMENSION COEFF(15),XMED(15),XMAD(15),C(14,15)
       DIMENSION VALMS(15),A(15),DA(15)
       DOUBLE PRECISION H(14,15),HVEC(210)
       DIMENSION FACLMS(11),FACLTS(11)
       INTEGER JNDEX(1000)
       INTEGER JSUBS(15)
       INTEGER JBEST(14)
       INTEGER JPLACE(15)
       INTEGER JMISS(15)
       CHARACTER*10 LAB(15)
       INTEGER JREPTAB(15)
       INTEGER JREPLOW(15)
       INTEGER JREPHI(15)
CC-----THE FOLLOWING THREE VARIABLES NEED TO HAVE LENGTH = MAXP+1
CC-----YOU DO NOT HAVE TO CHANGE FACLMS AND FACLTS
       DATA JREPTAB/500,1000,1500,2000,2500,10*3000/
       DATA JREPLOW/500,50,22,17,15,14,9*0/
       DATA JREPHI/1000000,1414,182,71,43,32,27,24,23,22,22,22,22,23,23/       
       DATA FACLMS/1.4826,1.3998,1.3238,1.2535,1.1882,1.1272,1.0700,
     1 1.0160,0.9648,0.9161,0.8693/
       DATA FACLTS/2.6477,2.5092,2.3826,2.2662,2.1587,2.0589,1.9660,
     1 1.879,1.7973,1.7203,1.6473/
       MAXP=14
       MAXN=1000
CC     -------------------------------------------------------------------
CC     INITIALIZATION
CC     ------------------------------------------------------------------------
       LUA=1
       LUB=2
       LUC=3
       MAXP1=MAXP+1
       MAXPP1=MAXP*MAXP1
       NZWE=0
       PREC=1.0E-6
       JLP=0
       JREG=0
       NSTOP=0
       DO 5 J=1,MAXP
       JBEST(J)=0
 5     CONTINUE
       JHEAD=' '
       FNAMEA=' '
       FNAMEB=' '
       FNAMEC=' '
CC     ------------------------------------------------------------------------
CC     READ THE DATA
CC     ------------------------------------------------------------------------
       CALL RDAT(NCAS,NVAR,JCST,JPRT,NFAC,NVAD,VALMS,
     1 X,Y,JPLT,JMISS,PREC,JPLACE,LAB,XMED,XMAD,AW,AW2,JNDEX,
     1 RESDU,WEIGHTS,MAXP1,MAXN,MVAL,MAXP,LUA,LUB,LUC,JHEAD,
     1 YNSAVE,JDIAG,NSTOP,FNAMEA,FNAMEB,FNAMEC,MROB,FACLMS,FACLTS)
       IF (NSTOP.EQ.1) GOTO 30
CC     ------------------------------------------------------------------------
CC     CREATE THE BEGINNING OF THE OUTPUT: THE SELECTED OPTIONS
CC     ------------------------------------------------------------------------
       CALL WRDATA(NVAR,NCAS,JCST,MVAL,JPRT,YNSAVE,FNAMEA,FNAMEB,
     1 FNAMEC,JHEAD,LUB,MROB,LAB,MAXP1,NVAD,JPLT)
CC     ------------------------------------------------------------------------
CC     IF THE DATA CONTAIN MISSING VALUES, THEN TREAT THEM
CC     ------------------------------------------------------------------------
       IF (MVAL.NE.0) THEN
       CALL SMISSING(NVAR,NCAS,MAXP1,MAXN,JCST,X,Y,AW,JNDEX,MVAL,
     1 JMISS,JSUBS,VALMS,NSTOP,LAB,JPRT,LUB)
       IF (NSTOP.EQ.1) GOTO 20
       ENDIF
CC     ------------------------------------------------------------------------
CC     ASK FOR QUANTILE AND NUMBER OF SUBSETS USED IN ROBUST REGRESSION
CC     ------------------------------------------------------------------------
       CALL JQUANT(NCAS,NVAR,MROB,JQU,FACTOR,FACLMS,FACLTS)
       CALL SUBREP(NCAS,NVAR,MAXP1,ALGO,NREP,JREPTAB,JREPLOW,JREPHI)
CC     ------------------------------------------------------------------------
CC     CREATE SECOND PART OF THE OUTPUT: THE DATA AND AN X-Y PLOT
CC     ------------------------------------------------------------------------
       CALL WRDATB(NVAR,NCAS,MAXP1,MAXN,JCST,MVAL,LAB,JNDEX,X,Y,
     1 AW,JPLT,JPRT,JREG,JHEAD,LUB,MROB)
CC     ------------------------------------------------------------------------
CC     STANDARDIZATION OF THE DATA (WITH OPTIONAL PRINTING)
CC     AND CALCULATION OF SOME STATISTICS ON THE VARIABLES
CC     ------------------------------------------------------------------------
       CALL STATIS(X,Y,XMED,XMAD,AW2,WEIGHTS,AW,JNDEX,C,H,JSUBS,
     1 NSTOP,NVAR,NVAD,NFAC,NCAS,JCST,JPRT,MAXP1,MAXN,LUB,MAXP,
     1 PREC,LAB)
       IF (NSTOP.EQ.1) GOTO 20
CC     ------------------------------------------------------------------------
CC     CALCULATION OF THE LS ESTIMATES
CC     ------------------------------------------------------------------------
       IF (NCAS.LE.(NVAR*2)) THEN
	  WRITE(*,8050) NCAS,NVAR
	  IF (JCST.EQ.1) WRITE(*,8060)
	  GOTO 20
       ENDIF
       JREG=1
       IF (NVAR.EQ.1) THEN
       CALL LSL(NCAS,MAXN,X,Y,WEIGHTS,A,FCKW,H,MAXP,MAXP1)
       ELSE
       CALL LSREG(MAXP1,MAXN,MAXP,NVAR,NCAS,NVAR,A,X,Y,WEIGHTS,DA,
     1 H,FCKW,HVEC,MAXPP1,JMISS)
       ENDIF
CC     ------------------------------------------------------------------------
CC     RETRANSFORMATION OF THE LS ESTIMATES
CC     ------------------------------------------------------------------------
       CALL RTRAN(NVAR,JCST,NFAC,NVAD,MAXP1,XMED,XMAD,A,NVAD,FCKW)
       CALL TRC(H,DA,MAXP,MAXP1,NVAR,JCST,NFAC,NVAD,XMED,XMAD)
CC     ------------------------------------------------------------------------
CC     PRINTING THE LS RESULTS WITH THE REQUESTED OPTIONS
CC     ------------------------------------------------------------------------
       CALL PRTRLS(NVAR,NCAS,JCST,NFAC,NVAD,MAXP1,MAXN,MAXP,
     1 XMED,XMAD,A,DA,H,AW,RESDU,WEIGHTS,X,Y,JNDEX,AVW,NZWE,FCKW,
     1 JREG,JPRT,JPLT,JHEAD,LAB,PREC,LUB,MROB)
CC     ------------------------------------------------------------------------
CC     CALCULATION OF LMS REGRESSION
CC     ------------------------------------------------------------------------
       WRITE(*,8000)
       CALL SUBLMS(NVAR,NCAS,JCST,MAXP1,MAXN,MAXP,PREC,X,Y,XMED,XMAD,       
     1 JDIAG,ALGO,FNAMEB,JQU,NREP,COEFF,JBEST,RESDU,WEIGHTS,UHYB,AVW,
     1 NZWE,RSQ,JHYB,MAXPP1,JERD,JLP,C,HVEC,JSUBS,AW,AW2,       
     1 MROB,FCKW,PRELSCAL,FACTOR)
CC     ------------------------------------------------------------------------
CC     RETRANSFORMATION OF THE LMS ESTIMATES
CC     ------------------------------------------------------------------------
       CALL RTRAN(NVAR,JCST,NFAC,NVAD,MAXP1,XMED,XMAD,COEFF,NVAD,FCKW)
       COEFF(NVAD)=COEFF(NVAD)*XMAD(NVAD)
       FCKW=FCKW/XMAD(NVAD)
       PRELSCAL=PRELSCAL*XMAD(NVAD)
CC     ------------------------------------------------------------------------
CC     PRINTING THE LMS RESULTS WITH THE REQUESTED OPTIONS
CC     ------------------------------------------------------------------------
       CALL PRTLMS(NVAR,NCAS,JCST,NFAC,NVAD,MAXP1,MAXN,MAXP,XMED,
     1 XMAD,COEFF,AW,RESDU,WEIGHTS,X,Y,JNDEX,UHYB,NZWE,RSQ,JREG,JBEST,
     1 JPRT,JPLT,NREP,JERD,JLP,JQU,JHYB,JDIAG,NSTOP,JHEAD,LAB,PREC,LUB,
     1 FCKW,MROB,ALGO,PRELSCAL)
       IF (NSTOP.NE.1) THEN
CC     ------------------------------------------------------------------------
CC     CALCULATION OF RLS REGRESSION
CC     ------------------------------------------------------------------------
       JREG=3
       WRITE(*,8005)
       DO 10 J=1,MAXP1
       A(J)=0.0
       DA(J)=0.0
       DO 10 JNC=1,MAXP
  10   H(JNC,J)=0.D0
       IF (NVAR.EQ.1) THEN
       CALL LSL(NCAS,MAXN,X,Y,WEIGHTS,A,FCKW,H,MAXP,MAXP1)
       ELSE
       CALL LSREG(MAXP1,MAXN,MAXP,NVAR,NCAS,NVAR,A,X,Y,WEIGHTS,DA,
     1 H,FCKW,HVEC,MAXPP1,JMISS)
       ENDIF
CC     ------------------------------------------------------------------------
CC     RETRANSFORMATION OF THE RLS ESTIMATES
CC     ------------------------------------------------------------------------
       CALL RTRAN(NVAR,JCST,NFAC,NVAD,MAXP1,XMED,XMAD,A,NVAD,FCKW)
       CALL TRC(H,DA,MAXP,MAXP1,NVAR,JCST,NFAC,NVAD,XMED,XMAD)
CC     ------------------------------------------------------------------------
CC     PRINTING THE RLS RESULTS WITH THE REQUESTED OPTIONS
CC     ------------------------------------------------------------------------
       CALL PRTRLS(NVAR,NCAS,JCST,NFAC,NVAD,MAXP1,MAXN,MAXP,
     1 XMED,XMAD,A,DA,H,AW,RESDU,WEIGHTS,X,Y,JNDEX,AVW,NZWE,FCKW,
     1 JREG,JPRT,JPLT,JHEAD,LAB,PREC,LUB,MROB)
       ENDIF
CC     END OF THE RUN
       WRITE (*,8010)
 20    IF (YNSAVE.EQ.'Y') WRITE (*,8020) FNAMEC
       IF (FNAMEA.NE.'CON') WRITE(*,8030) FNAMEA
       IF (FNAMEB.NE.'CON'.AND.FNAMEB.NE.'PRN') WRITE(*,8040)FNAMEB
CC     -----------------------------------------------------------------
CC     FORMATS
CC     -----------------------------------------------------------------
 8000  FORMAT(//' Algorithm for calculating the positive breakdown ',
     1 'regression is started.'//)
 8005  FORMAT(//45h Progress is presently calculating reweighted,
     1 18h least squares .../)
 8010  FORMAT(//41h The run has been executed successfully. //)
 8020  FORMAT(' The data are saved in the file: ',A30)
 8030  FORMAT(' The data has been read from the file: ',A30)
 8040  FORMAT(' The output has been written in the file: ',A30)
 8050  FORMAT(//46h Too many coefficients according to the number,
     1 10h of cases.//16h Number of cases,8X,2H= ,I5/
     1 26h Number of coefficients = ,I5,/25h The number of cases must,
     1 37h be twice the number of coefficients.)
 8060  FORMAT(31h (Including the constant term!))
 30    STOP
       END
CC     ------------------------------------------------------------------------
CC     SUBROUTINE FOR CREATING THE BEGINNING OF THE OUTPUT:
CC		      - DESCRIPTION OF THE SELECTED OPTIONS
CC     ------------------------------------------------------------------------
       SUBROUTINE WRDATA(NVAR,NCAS,JCST,MVAL,JPRT,YNSAVE,FNAMEA,
     1 FNAMEB,FNAMEC,JHEAD,LUB,MROB,LAB,MAXP1,NVAD,JPLT)
       CHARACTER YNSAVE
       CHARACTER*30 FNAMEA,FNAMEB,FNAMEC
       CHARACTER*60 JHEAD
       CHARACTER*10 LAB(MAXP1)
       WRITE(LUB,8000)
       WRITE(LUB,8005)
       WRITE(LUB,8006)
       WRITE(LUB,8010) JHEAD
       IF (JCST.EQ.0) THEN
          WRITE(LUB,8020) NCAS,NVAR,LAB(NVAD)
          WRITE(LUB,8040)
                      ELSE
          WRITE(LUB,8020) NCAS,NVAR-1,LAB(NVAD)
          WRITE(LUB,8030)
       ENDIF
       IF (MROB.EQ.1) WRITE(LUB,8050)
       IF (MROB.EQ.2) WRITE(LUB,8060)
       IF (FNAMEB.EQ.'CON') PAUSE ' '
       IF (YNSAVE.EQ.'Y') WRITE(LUB,8080) FNAMEC
       IF (FNAMEA.NE.'CON') WRITE(LUB,8070) FNAMEA
       WRITE(LUB,8085) FNAMEB
       IF (JPRT.EQ.0) WRITE(LUB,8090)
       IF (JPRT.EQ.1) WRITE(LUB,8100)
       IF (JPRT.EQ.2) WRITE(LUB,8110)
       IF (JPLT.EQ.0) WRITE(LUB,8200)
       IF (JPLT.EQ.1) WRITE(LUB,8210)
       IF (JPLT.EQ.2) WRITE(LUB,8220)
       IF (JPLT.EQ.3) WRITE(LUB,8230)
       IF (MVAL.EQ.0) WRITE(LUB,8120)
       IF (MVAL.EQ.1) WRITE(LUB,8130)
       IF (MVAL.EQ.2) WRITE(LUB,8140)
 8000  FORMAT(//30X,19(1H*)/30X,19H* P R O G R E S S */
     1 30X,19(1H*)///,
     1 ' This program performs a robust regression analysis using'
     1 ' either'/     
     1 '        the least median of squares (LMS) method '/
     1 ' or'/
     1 '        the least trimmed squares (LTS) method'/
     1 ' both described in :'//
     1 '    P.J. Rousseeuw (1984), Least Median of Squares Regression,'/       
     1 '    Journal of the American Statistical Association, 79,'
     1 ' 871-880.'//' A user manual to this program is the book:'//
     1 '    P.J. Rousseeuw and A.M. Leroy (1987), Robust Regression'/
     1 '    and Outlier Detection, Wiley, New York.'//)
 8005  FORMAT(' Since January 16, 1996 the program was upgraded with:'/
     1 '   - better optimization by adjusting the intercept at each'
     1 ' slope '/
     1 '     (this yields the exact LMS for simple regression)')
 8006  FORMAT('   - optimal default value: h = [(n+p+1)/2]'/
     1 '   - option to set a lower breakdown value (higher h) '/
     1 '   - choice between exhaustive and random p-subsets '/
     1 '   - choice of the number of random p-subsets '/
     1 '   - new definition of robust R-squared.'/
     1 ' Therefore, the outputs differ slightly from those',
     1 ' in the 1987 book.'//
     1 ' For a full description of this upgrade see:'/
     1 '    P.J. Rousseeuw and M. Hubert (1997), Recent developments',
     1 ' in PROGRESS,'/
     1 '    in: L1-Statistical Procedures and Related Topics,'
     1 ' ed. Y. Dodge,'/
     1 '    IMS Lecture Notes - Monograph Series No. 31, Institute of'
     1 ' Mathematical'/
     1 '    Statistics: Hayward, California, pp. 201-214.'//)
 8010  FORMAT(/' Title:   ',A60)
 8020  FORMAT(' Number of cases in the data set:',19X,I5/
     1 ' Number of explanatory variables in the regression: ',I5/
     1 ' The response variable is: ',A10)
 8030  FORMAT(' You have chosen a model with a constant term',
     1 ' (regression with intercept).')
 8040  FORMAT(' You have chosen a model without constant term',
     1 ' (no intercept).')
 8050  FORMAT(' You have chosen the least median of squares',
     1 ' (LMS) method.')
 8060  FORMAT(' You have chosen the least trimmed squares',
     1 ' (LTS) method.')
 8070  FORMAT(' Your data reside in the file: ',A30)
 8080  FORMAT(' The data are saved in the file: ',A30)
 8085  FORMAT(' This output is written in the file: ',A30)
 8090  FORMAT(' You have chosen small output.')
 8100  FORMAT(' You have chosen medium-sized output.')
 8110  FORMAT(' You have chosen large output.')
 8200  FORMAT(' You have chosen not to plot residuals.')
 8210  FORMAT(' You have chosen a plot of residuals versus ',
     1 'estimated y.')      
 8220  FORMAT(' You have chosen a plot of residuals versus ',
     1 'their index.')      
 8230  FORMAT(' You have chosen a plot of residuals versus their index'
     1 ' and '/
     1 '                                     versus estimated y.')
 8120  FORMAT(' The data is assumed not to have missing values.')
 8130  FORMAT(' Treatment of missing values in option 1:'/
     1 '    cases with at least one missing value will be deleted.'/)
 8140  FORMAT(' Treatment of missing values in option 2:'/
     1 '    First, any case with a missing value for the response'
     1 ' variable'/
     1 '    or for all explanatory variables will be deleted.'/
     1 '    For the remaining cases, each missing value is replaced by'/
     1 '    the median of the non-missing values.'/)
       RETURN
       END
CC     ------------------------------------------------------------------------
CC     SUBROUTINE FOR CREATING THE SECOND PART OF THE OUTPUT:
CC                     - WRITING DOWN THE OBSERVATIONS
CC                     - AN X-Y PLOT
CC     ------------------------------------------------------------------------
       SUBROUTINE WRDATB(NVAR,NCAS,MAXP1,MAXN,JCST,MVAL,LAB,JNDEX,X,Y,
     1 AW,JPLT,JPRT,JREG,JHEAD,LUB,MROB)
       DIMENSION X(MAXP1,MAXN),Y(MAXN),AW(MAXN)
       INTEGER JNDEX(MAXN)
       CHARACTER*10 LAB(MAXP1)
       CHARACTER*60 JHEAD
       NFAC=NVAR-1
       NVAD=NVAR+1
       JPLXY=0
       IF ((NVAR.EQ.1.AND.JCST.EQ.0).OR.
     1 (NVAR.EQ.2.AND.JCST.EQ.1)) JPLXY=1
       IF (JPRT.EQ.2) THEN
	 IF (MVAL.EQ.0) WRITE(LUB,8110)
	 IF (MVAL.NE.0) WRITE(LUB,8120)
	 IF (JCST.EQ.1) WRITE(LUB,8130) (LAB(J),J=1,NFAC),LAB(NVAD)
	 IF (JCST.EQ.0) WRITE(LUB,8130) (LAB(J),J=1,NVAD)
       ENDIF
       DO 10 JNC=1,NCAS
       IF(JPRT.GE.2) THEN
       IF((NVAD.GT.6.AND.JCST.EQ.0).OR.(NVAD.GT.7.AND.JCST.EQ.1))
     1 GOTO 15
       IF (JCST.EQ.1) WRITE(LUB,8140) JNDEX(JNC),(X(J,JNC),J=1,NFAC),
     1 X(NVAD,JNC)
       IF (JCST.EQ.0) WRITE(LUB,8140) JNDEX(JNC),(X(J,JNC),J=1,NVAD)
       GOTO 5
 15    WRITE(LUB,8140) JNDEX(JNC),(X(J,JNC),J=1,6)
       IF (JCST.EQ.1.AND.NFAC.GE.7)
     1 WRITE(LUB,8150) (X(J,JNC),J=7,NFAC),X(NVAD,JNC)
       IF (JCST.EQ.1.AND.NFAC.LT.7) WRITE(LUB,8150) X(NVAD,JNC)
       IF (JCST.EQ.0) WRITE(LUB,8150) (X(J,JNC),J=7,NVAD)
       ENDIF
  5    IF (JPLXY.EQ.1) THEN
       AW(JNC)=X(1,JNC)
       Y(JNC)=X(NVAD,JNC)
       ENDIF
  10   CONTINUE
       IF (JPLXY.EQ.1)
     1 CALL GRAF(AW,Y,NCAS,JPLXY,0,JPLT,JNDEX,MAXN,LUB,JREG,
     1 JHEAD,MAXP1,NVAD,LAB,MROB)
 8110  FORMAT(/22h The observations are:/)
 8120  FORMAT(/' The remaining observations are: '/)
 8130  FORMAT('    i  ',6(A10,1X)/15X,5(A10,1X))
 8140  FORMAT(I5,2X,6(F10.4,1X))
 8150  FORMAT(15X,5(F10.4,1X))
       RETURN
       END
CC     ------------------------------------------------------------------------
CC     PRINTING THE RLS ESTIMATES AND SOME OPTIONS
CC     ------------------------------------------------------------------------
       SUBROUTINE PRTRLS(NVAR,NCAS,JCST,NFAC,NVAD,MAXP1,MAXN,MAXP,
     1 XMED,XMAD,A,DA,H,AW,RESDU,WEIGHTS,X,Y,JNDEX,AVW,NZWE,FCKW,
     1 JREG,JPRT,JPLT,JHEAD,LAB,PREC,LUB,MROB)
       DIMENSION X(MAXP1,MAXN),Y(MAXN),WEIGHTS(MAXN)
       DIMENSION XMED(MAXP1),XMAD(MAXP1),AW(MAXN),RESDU(MAXN)
       DIMENSION A(MAXP),DA(MAXP)
       DOUBLE PRECISION H(MAXP,MAXP1),DS,PTVAL,DST,PVAL
       INTEGER JNDEX(MAXN)
       CHARACTER*10 LAB(MAXP1)
       CHARACTER*60 JHEAD
       AL=NCAS
       ANVAR=NVAR
       WRITE(LUB,8000)
       IF (JREG.EQ.1) THEN
       WRITE(LUB,8010)
       NNUL=NCAS
       AVW=1
                      ELSE
       IF (MROB.EQ.1) THEN
         WRITE(LUB,8020)
                      ELSE
         WRITE(LUB,8025)
       ENDIF  
       NNUL=NZWE
       ENDIF
       NDF=NNUL-NVAR
CC-----PRINTING THE COEFFICIENTS WITH THEIR STANDARD ERROR,
CC-----AND T-VALUE
       WRITE(LUB,8030)
       DO 10 J=1,NFAC
       IF(DA(J).LT.PREC)DA(J)=PREC
       STUD=A(J)/DA(J)
       DS=DBLE(STUD)
       DST=PTVAL(DS,NDF)
       RDST=DST
       WRITE(LUB,8040) LAB(J),A(J),DA(J),STUD,RDST
 10    CONTINUE
       IF(DA(NVAR).LT.PREC)DA(NVAR)=PREC
       STUD=A(NVAR)/DA(NVAR)
       DS=DBLE(STUD)
       DST=PTVAL(DS,NDF)
       RDST=DST
       IF (JCST.EQ.0) WRITE(LUB,8040) LAB(NVAR),A(NVAR),DA(NVAR),
     1 STUD,RDST
       IF (JCST.EQ.1) WRITE(LUB,8050) A(NVAR),DA(NVAR),STUD,RDST
CC-----CALCULATION OF THE COEFFICIENT OF DETERMINATION AND F-VALUE
       RSQ=RSQU(NCAS,NVAD,JCST,Y,MAXN,FCKW,FVALUE,PREC,
     1 XMAD,XMED,MAXP1,WEIGHTS,NNUL)
CC-----CALCULATION OF THE SCALE ESTIMATE
       A(NVAD)=SQRT(FCKW/(AL*AVW-ANVAR))
       IF (JREG.EQ.1) THEN
       WRITE(LUB,8080) FCKW
		      ELSE
       WRITE(LUB,8090) FCKW
       ENDIF
       WRITE(LUB,8060) NDF
       IF (JREG.EQ.1) THEN
          WRITE(LUB,8070) A(NVAD)
		      ELSE
          WRITE(LUB,8075) A(NVAD)
       ENDIF
CC-----PRINTING THE VARIANCE-COVARIANCE MATRIX
       CALL SCHCV(NVAR,MAXP1,MAXP,H,LUB)
       FCKW=FCKW/(AL*AVW-ANVAR)
CC-----PRINT COEFFICIENT OF DETERMINATION, F-VALUE, AND P-VALUE
       IF (JREG.EQ.1) THEN
          WRITE(LUB,8100) RSQ
		      ELSE
          WRITE(LUB,8105) RSQ
       ENDIF
       NDF1=NVAR-JCST
       DS=DBLE(FVALUE)
       DST=PVAL(DS,NDF1,NDF)
       RDST=DST
       WRITE(LUB,8110) FVALUE,NDF1,NDF,RDST
       IF (JREG.EQ.3) THEN
	  WRITE(LUB,8200) NNUL
	  WRITE(LUB,8210) AVW
	  JKEUS=2
		      ELSE
	  JKEUS=0
       ENDIF
CC-----GIVE RESIDUAL PLOTS IF REQUESTED
       IF(JPRT.NE.0.OR.JPLT.NE.0) THEN
       CALL RDUAL(A,JKEUS,NVAD,NCAS,NVAR,JCST,JPRT,NVAD,LUB,PREC,JREG,
     1 X,Y,RESDU,WEIGHTS,XMED,XMAD,JPLT,AW,JNDEX,MAXP1,MAXN,
     1 JHEAD,LAB,MROB)
       ENDIF
 8000  FORMAT(/1X,78(1H*)/)
 8010  FORMAT(25H LEAST SQUARES REGRESSION /1X,24(1H*)/)
 8020  FORMAT(42H REWEIGHTED LEAST SQUARES BASED ON THE LMS/
     1 1X,41(1H*)/)
 8025  FORMAT(42H REWEIGHTED LEAST SQUARES BASED ON THE LTS/
     1 1X,41(1H*)/)
 8030  FORMAT(/5X,8hvariable,5X,11hcoefficient,
     1 4X,12hstand. error,5X,'t-statistic',5X,'p-value'/
     1 3X,70(1H-))
 8040  FORMAT(3X,A10,5X,F11.5,4X,F12.5,7X,F9.5,3X,F9.5)
 8050  FORMAT(5X,8hconstant,5X,F11.5,4X,F12.5,7X,F9.5,3X,F9.5)
 8060  FORMAT(/20h Degrees of freedom ,5X,2H= ,4X,I5)
 8070  FORMAT(/' LS scale estimate',7X,2H= ,F15.5)
 8075  FORMAT(/' RLS scale estimate',6X,2H= ,F15.5)
 8080  FORMAT(/15h Sum of squares,10X,2H= ,F15.5)
 8090  FORMAT(/24h Weighted sum of squares,3H = ,F15.5)
 8100  FORMAT(/' R-squared (coefficient of determination) = ',F7.5/)
 8105  FORMAT(/' Weighted R-squared (coefficient of determination) = ',
     1 F7.5/)
 8110  FORMAT(' F-statistic = ',F12.3,7h (with ,I3,5h and ,
     1 I4,4h df),5X,'p-value = ',F7.5)
 8200  FORMAT(/' There are ',I4,' points with nonzero weight.')
 8210  FORMAT(/' Average weight = ',F7.5/)
       RETURN
       END
CC     ------------------------------------------------------------------------
CC     SUBROUTINE FOR CALCULATING THE LMS/LTS REGRESSION
CC     ------------------------------------------------------------------------
       SUBROUTINE SUBLMS(NVAR,NCAS,JCST,MAXP1,MAXN,MAXP,PREC,X,Y,XMED,
     1 XMAD,JDIAG,ALGO,FNAMEB,JQU,NREP,COEFF,JBEST,RESDU,WEIGHTS,UHYB,
     1 AVW,NZWE,RSQ,JHYB,MAXPP1,JERD,JLP,C,HVEC,JSUBS,AW,AW2,
     1 MROB,FCKW,PRELSCAL,FACTOR)
       DIMENSION X(MAXP1,MAXN),Y(MAXN),C(MAXP,MAXP1),COEFF(MAXP1)       
       DIMENSION XMED(MAXP1),XMAD(MAXP1)
       DIMENSION AW(MAXN),RESDU(MAXN),UHYB(MAXN),WEIGHTS(MAXN),AW2(MAXN)       
       DOUBLE PRECISION HVEC(MAXPP1)
       INTEGER JBEST(MAXP),JSUBS(MAXP1)
       CHARACTER ALGO
       CHARACTER*30 FNAMEB
CC-----LIST OF VARIABLES WHICH ENTER "SUBLMS" (WITHOUT CHANGING INSIDE)
CC     NVAR, NCAS, JCST, MAXP1, MAXN, MAXP, MAXPP1, PREC,
CC     X, Y, XMED, XMAD, JDIAG, ALGO, FNAMEB, JQU, NREP, MROB, FACTOR
CC-----LIST OF VARIABLES THE VALUE OF WHICH IS CALCULATED IN "SUBLMS"
CC     COEFF, JBEST, RESDU, WEIGHTS, UHYB, AVW, NZWE, RSQ,
CC     JHYB, JERD, JLP, FCKW, PRELSCAL
CC-----LIST OF VARIABLES WHICH ARE NEEDED FOR THE CALCULATIONS
CC     C, HVEC, JSUBS, AW, JADJ, AW2
CC-----LIST OF SUBROUTINES USED IN "SUBLMS"
CC     SUBROUTINE GENPN
CC     SUBROUTINE RANPN
CC     SUBROUTINE EQUAT
CC     SUBROUTINE RESTT
CC     SUBROUTINE RANGS
CC     SUBROUTINE LMSLOC
CC     SUBROUTINE LTSLOC
CC     FUNCTION PULL
CC-----INITIALIZATIONS
       JADJ=0
       JERD=0
       JFRST=0
       JEY=0
       AVW=0.0
       NZWE=0
       JREADZ=0
       JHYB=JDIAG
       AL=NCAS
       ANVAR=NVAR
CC-----MAXIMAL NUMBER OF REPLICATIONS
       MAXRP=3*NREP
       NFAC=NVAR-1
       NVAD=NVAR+1
CC-----START OF THE ALGORITHM : THE "10" LOOP PERFORMS THE REPLICATIONS
       DO 10 JJJ=1,NREP
 20    IF (ALGO.EQ.'A') THEN
	 IF (JJJ.EQ.1) THEN
	   DO 5 J=1,NFAC
	   JSUBS(J)=J
 5         CONTINUE
	   JSUBS(NVAR)=NVAR-1
         ENDIF
         CALL GENPN(NCAS,NVAR,JSUBS)
        ELSE
	   IF (JJJ.GT.2) THEN
	     CALL RANPN(NCAS,NVAR,JSUBS,MAXP1,JEY,JLP,MAXRP)
	     IF (JLP.GT.MAXRP) GOTO 210
	   ELSE
	     IF (JJJ.EQ.2) THEN
	     JLP=JLP+1
	     DO 30 MNDX=1,NVAR
 30	     JSUBS(MNDX)=NCAS-MNDX+1
			   ELSE
	     JLP=JLP+1
	     DO 40 MNDX=1,NVAR
 40	     JSUBS(MNDX)=MNDX
	     ENDIF
	   ENDIF
       ENDIF
CC-----STORE THE NVAR SELECTED OBSERVATIONS IN THE MATRIX C
       DO 50 MNDX=1,NVAR
	 DO 60 KJ=1,NVAD
	   NDR=JSUBS(MNDX)
	   C(MNDX,KJ)=X(KJ,NDR)
 60	 CONTINUE
 50    CONTINUE
CC-----THE FOLLOWING FIVE LINES ARE INSTRUCTIONS FOR INFORMING
CC-----THE USER ABOUT HOW FAR THE CALCULATIONS ARE ALREADY ADVANCED
CC-----(THESE LINES MAY BE DELETED IF USED ON OTHER COMPUTER)
       JREADY=(JJJ*100)/NREP
       IF (MOD(JREADY,5).EQ.0.AND.JREADY.NE.JREADZ)
     1 WRITE(*,8010) JREADY
       IF (FNAMEB.EQ.'CON'.AND.JREADY.GE.100) PAUSE ' '
       JREADZ=JREADY
CC-----SOLVE THE SYSTEM OF EQUATIONS STORED IN C
       IF(NVAR.GT.1) THEN
	 CALL EQUAT(C,MAXP,MAXP1,HVEC,MAXPP1,NVAR,1,NERR)
	 IF(NERR.GE.0) GOTO 70
	 JERD=JERD+1
	 IF (ALGO.EQ.'R'.AND.JJJ.GT.2) GOTO 20
	 GOTO 10
		     ELSE
	 IF(C(1,1).NE.0.0) C(1,1)=C(1,2)/C(1,1)
       ENDIF
 70    JADJ=0
CC-----CALCULATE THE RESIDUALS
 71    DO 80 JNC=1,NCAS
       RESDU(JNC)=0.0
       DO 90 J=1,NVAR
       RESDU(JNC)=RESDU(JNC)+C(J,1)*X(J,JNC)
 90    CONTINUE
       RESDU(JNC)=X(NVAD,JNC)-RESDU(JNC)
 80    CONTINUE
CC-----ADJUST INTERCEPT AND RECALCULATE THE RESIDUALS
       IF (JCST.EQ.1.AND.JADJ.EQ.0) THEN
         CALL RANGS(RESDU,NCAS)
	 IF (MROB.EQ.1) THEN 
	   CALL LMSLOC(RESDU,NCAS,JQU,SLUTN,BSTD,PREC,AW2,FACTOR) 
	  ELSE
	   CALL LTSLOC(RESDU,NCAS,JQU,SLUTN,BSTD,AW,AW2,MAXN,FACTOR)
         ENDIF
	 C(NVAR,1)=C(NVAR,1)+SLUTN
	 JADJ=1
	 GOTO 71
       ENDIF
CC-----CALCULATE THE ASSOCIATED OBJECTIVE FUNCTION VALUE
CC-----FOR THE TRIAL ESTIMATE OBTAINED ABOVE
       IF (MROB.EQ.1) THEN
         DO 95 JNC=1,NCAS
	 AW2(JNC)=ABS(RESDU(JNC))
 95      CONTINUE
         FCTW=PULL(AW,MAXN,AW2,NCAS,JQU)
        ELSE
         DO 97 JNC=1,NCAS
	 AW2(JNC)=RESDU(JNC)**2
 97      CONTINUE
         CALL RANGS(AW2,NCAS)
	 FCTW=0
	 DO 98 J=1,JQU
	 FCTW=FCTW+AW2(J)
 98      CONTINUE
	 FCTW=SQRT(FCTW/JQU)
       ENDIF
CC-----INITIALIZE THE "BEST" ESTIMATE AND OBJECTIVE FUNCTION VALUE
       IF(JFRST.NE.1) THEN
	 JFRST=1
	 COEFF(NVAD)=FCTW
	 DO 100 J=1,NVAR
	 JBEST(J)=JSUBS(J)
	 COEFF(J)=C(J,1)
 100	 CONTINUE
	 IF (FCTW*XMAD(NVAD).LT.PREC) JHYB=0
CC-----IF DIAGNOSTICS ARE WANTED, THEN CALCULATE THE u_i
	 IF (JHYB.EQ.0) GOTO 200
	 DO 110 JNC=1,NCAS
	 UHYB(JNC)=ABS(RESDU(JNC))/FCTW
 110	 CONTINUE
		      ELSE
 	 IF(FCTW*XMAD(NVAD).LT.PREC) JHYB=0
	 IF (JHYB.NE.0) THEN
	   DO 130 JNC=1,NCAS
	   UH=ABS(RESDU(JNC))/FCTW
	   IF(UHYB(JNC).LT.UH) UHYB(JNC)=UH
 130	   CONTINUE
	 ENDIF
CC-----UPDATE THE "BEST" ESTIMATE AND OBJECTIVE FUNCTION VALUE
CC-----IF NECESSARY
	 IF (FCTW.LT.COEFF(NVAD)) THEN
	   COEFF(NVAD)=FCTW
	   DO 140 J=1,NVAR
	   JBEST(J)=JSUBS(J)
 140	   COEFF(J)=C(J,1)
	 ENDIF
       ENDIF
CC-----IF FCTW IS LESS THAN PREC (A PRECISION CONSTANT) THEN ONE IS
CC-----RESORTED TO THE EXACT FIT CASE
 200   IF (FCTW*XMAD(NVAD).LE.PREC) THEN
	  COEFF(NVAD)=FCTW
	  CALL RESTT(COEFF,1,0,NVAR,NCAS,NVAD,NZWE,X,Y,RESDU,WEIGHTS,
     1	  XMED,XMAD,MAXP1,MAXN,0,SCAL,QUAN,PREC)
	  AVW=NZWE/AL
	  RETURN
       ENDIF
 10    CONTINUE
 210   CONTINUE
CC-----FCTW : LOWEST VALUE OF OBJECTIVE FUNCTION
CC-----PRELSCAL : PRELIMINARY SCALE ESTIMATE
       FCKW=COEFF(NVAD)
       PRELSCAL=FCKW*FACTOR
       FINITF=-10.0/(AL-ANVAR)*(JQU/AL)+(10.0/(AL-ANVAR)+1.0)
       IF (MROB.EQ.1) PRELSCAL=PRELSCAL*FINITF        
       COEFF(NVAD)=PRELSCAL
CC-----COEFFICIENT OF DETERMINATION CORRESPONDING TO THE LMS/LTS
       RSQ=PRELSCAL
       DO 230 JNC=1,NCAS
       RESDU(JNC)=Y(JNC)
 230   CONTINUE
       IF (JCST.NE.0) THEN
         CALL RANGS(RESDU,NCAS)
         IF (MROB.EQ.1) THEN 
           CALL LMSLOC(RESDU,NCAS,JQU,SLUTN,BSTD,PREC,AW2,FACTOR) 
          ELSE
           CALL LTSLOC(RESDU,NCAS,JQU,SLUTN,BSTD,AW,AW2,MAXN,FACTOR)
         ENDIF
         RSY=BSTD
                      ELSE
CC-----REGRESSION TROUGH ORIGIN, FIRST TRANSFORM Y
         DO 240 JNC=1,NCAS
	 RESDU(JNC)=RESDU(JNC)*XMAD(NVAD)+XMED(NVAD)
 240     CONTINUE
	 RSQ=RSQ*XMAD(NVAD)
         IF (MROB.EQ.1) THEN
           DO 295 JNC=1,NCAS
	   AW2(JNC)=ABS(RESDU(JNC))
 295       CONTINUE
           RSY=PULL(AW,MAXN,AW2,NCAS,JQU)*FACTOR
                        ELSE
           DO 297 JNC=1,NCAS
	   AW2(JNC)=RESDU(JNC)**2
 297       CONTINUE
           CALL RANGS(AW2,NCAS)
	   RSY=0
	   DO 298 J=1,JQU
	   RSY=RSY+AW2(J)
 298       CONTINUE
	   RSY=SQRT(RSY/JQU)*FACTOR
         ENDIF
       ENDIF
       IF (MROB.EQ.1) RSY=RSY*FINITF
       RSQ=1.0-(RSQ/RSY)**2
       IF (RSQ.LT.0.0) RSQ=0.0
       IF (RSQ.GT.1.0) RSQ=1.0
CC-----COMPUTING THE FINAL SCALE ESTIMATE OF THE LMS OR THE LTS 
CC-----AND THE RESULTING WEIGHTS
       CALL RESTT(COEFF,1,0,NVAR,NCAS,NVAD,NZWE,
     1 X,Y,RESDU,WEIGHTS,XMED,XMAD,MAXP1,MAXN,1,SCAL,PRELSCAL,PREC)
CC-----COEFF(NVAD) : FINAL SCALE ESTIMATE
       COEFF(NVAD)=SCAL
       AVW=NZWE/AL
 8010  FORMAT(I4,48h percent of the calculations have been executed.)
       RETURN
       END
CC     ------------------------------------------------------------------------
CC     PRINTING LMS/LTS ESTIMATES AND SOME OPTIONS
CC     ------------------------------------------------------------------------
       SUBROUTINE PRTLMS(NVAR,NCAS,JCST,NFAC,NVAD,MAXP1,MAXN,MAXP,
     1 XMED,XMAD,COEFF,AW,RESDU,WEIGHTS,X,Y,JNDEX,UHYB,NZWE,RSQ,JREG,
     1 JBEST,JPRT,JPLT,NREP,JERD,JLP,JQU,JHYB,JDIAG,NSTOP,JHEAD,LAB,       
     1 PREC,LUB,FCKW,MROB,ALGO,PRELSCAL)
       DIMENSION X(MAXP1,MAXN),Y(MAXN),XMED(MAXP1),XMAD(MAXP1)
       DIMENSION COEFF(MAXP),AW(MAXN),RESDU(MAXN)
       DIMENSION WEIGHTS(MAXN),UHYB(MAXN)
       INTEGER JNDEX(MAXN)
       INTEGER JBEST(MAXP)
       CHARACTER ALGO
       CHARACTER*10 LAB(MAXP1)
       CHARACTER*60 JHEAD
       CHARACTER*3 NAME
       NAME='   '
       IF (MROB.EQ.1) THEN
	  NAME='LMS'
		      ELSE
          NAME='LTS'
       ENDIF
       WRITE(LUB,8000)
       IF (MROB.EQ.1) THEN
         WRITE(LUB,8010)
         WRITE(LUB,8020) JQU
               	      ELSE
         WRITE(LUB,8015)
         WRITE(LUB,8025) JQU
       ENDIF
       JBREAK=NBREAK(JQU,NCAS,NVAR)
       JDEFAUL=(NCAS+NVAR+1)/2
       IF (JBREAK.EQ.NBREAK(JDEFAUL,NCAS,NVAR)) THEN
          WRITE(LUB,8028) JBREAK,NCAS,NVAR
					ELSE
          WRITE(LUB,8027) JBREAK
       ENDIF
       NSTOP=0
       IF (ALGO.EQ.'R') THEN
          WRITE(LUB,8030) JLP,NVAR,NCAS
          IF (JERD.NE.0) THEN
	    IF  (JERD.EQ.JLP) THEN
	        WRITE(LUB,8035) 
	        NSTOP=1
	        RETURN
          	              ELSE
	        WRITE(LUB,8040) JLP,JERD
	    ENDIF
			 ELSE
            WRITE(LUB,8045) JLP
          ENDIF
		        ELSE
          WRITE(LUB,8050) NREP,NVAR,NCAS
          IF (JERD.NE.0) THEN
	    IF (JERD.EQ.NREP) THEN
	       WRITE(LUB,8035) 
	       NSTOP=1
	       RETURN
	         	      ELSE
	       WRITE(LUB,8040) NREP,JERD
            ENDIF
			 ELSE
            WRITE(LUB,8045) NREP
	  ENDIF
       ENDIF
       WRITE(LUB,8065) (JBEST(J),J=1,NVAR)
CC-----LMS/LTS IN THE CASE OF EXACT FIT
       IF (COEFF(NVAD).LE.PREC) THEN
	  WRITE(LUB,8070)
	  DO 10 J=1,NFAC
 10	  WRITE(LUB,8080) LAB(J),COEFF(J)
	  IF (JCST.EQ.0) WRITE(LUB,8080) LAB(NVAR),COEFF(NVAR)
	  IF (JCST.NE.0) WRITE(LUB,8090) COEFF(NVAR)
	  WRITE(LUB,8100) COEFF(NVAD)
	  WRITE(LUB,8110) NZWE
	  JREG=2
CC-----RESIDUAL PLOT FOR THE LMS/LTS IN THE CASE OF EXACT FIT
	  CALL RDUAL(COEFF,0,NVAD,NCAS,NVAR,JCST,JPRT,NVAD,LUB,PREC,
     1	  JREG, X,Y,RESDU,WEIGHTS,XMED,XMAD,JPLT,AW,JNDEX,
     1	  MAXP1,MAXN,JHEAD,LAB,MROB)
	  NSTOP=1
	  RETURN
       ENDIF
CC-----LMS/LTS IN THE GENERAL CASE
       WRITE(LUB,8130)
       DO 20 J=1,NFAC
 20    WRITE(LUB,8080) LAB(J),COEFF(J)
       IF (JCST.EQ.0) WRITE(LUB,8080) LAB(NVAR),COEFF(NVAR)
       IF (JCST.NE.0) WRITE(LUB,8090) COEFF(NVAR)
       WRITE(LUB,8190) NAME,FCKW
       WRITE(LUB,8200) NAME,PRELSCAL
       WRITE(LUB,8210) RSQ
       WRITE(LUB,8215) NAME,COEFF(NVAD)
       WRITE(LUB,8235)
       WRITE(LUB,8240) NCAS,JQU,NVAR
       IF (MROB.EQ.1) THEN
	 WRITE(LUB,8250)
	 WRITE(LUB,8300)
        	      ELSE
	 WRITE(LUB,8260)
	 WRITE(LUB,8310)
       ENDIF
       IF (JCST.NE.0) THEN
         WRITE(LUB,8320)
		      ELSE
         WRITE(LUB,8325)
       ENDIF 
       WRITE(LUB,8335) NAME
       WRITE(LUB,8235)
       JREG=2
CC-----RESIDUAL PLOTS IF REQUESTED
       CALL RDUAL(COEFF,1,NVAD,NCAS,NVAR,JCST,JPRT,NVAD,LUB,PREC,JREG,
     1 X,Y,RESDU,WEIGHTS,XMED,XMAD,JPLT,AW,JNDEX,MAXP1,MAXN,
     1 JHEAD,LAB,MROB)
CC-----RESISTANT DIAGNOSTICS
       IF (JHYB.NE.0.AND.JDIAG.EQ.1) THEN
	  WRITE(LUB,8350)
	  WRITE(LUB,8360) NAME
	  UH=AMDAN(AW,MAXN,UHYB,NCAS)
	  DO 40 JNC=1,NCAS
	  RDI=UHYB(JNC)/UH
	  WRITE(LUB,8390) JNDEX(JNC),UHYB(JNC),RDI
 40	  CONTINUE
          IF (MROB.EQ.1) THEN
	   WRITE(LUB,8400)
	       	         ELSE
           WRITE(LUB,8410)
          ENDIF
          WRITE(LUB,8420)
       ENDIF
       IF(JCST.EQ.1) THEN
          WRITE(LUB,8430) NAME
          WRITE(LUB,8440)
       ENDIF
 8000  FORMAT(/1X,78(1H*)/)
 8010  FORMAT(' LEAST MEDIAN OF SQUARES REGRESSION'/,
     1 1X,34(1H*)/)
 8015  FORMAT(' LEAST TRIMMED SQUARES REGRESSION'/,
     1 1X,32(1H*)/)
 8020  FORMAT(' The estimator minimizes the ',I4,
     1 'th ordered squared residual,')
 8027  FORMAT(' hence its breakdown value is ',I2,'%.')
 8028  FORMAT(' hence its breakdown value is ',I2,'% (= highest'
     1 ' possible value '/' for n = ',I4,' cases and p = ',I3,
     1 ' coefficients).')
 8025  FORMAT(' The estimator minimizes the sum of the ',I4,
     1 ' smallest squared residuals,')
 8030  FORMAT(/' The program has drawn ',I8,' random subsets of ',I3,
     1 ' cases out of ',I5,'.')
 8035  FORMAT(//' Each of these subsets',
     1 ' gave a singular system of equations.'/
     1 48h This means that there is an enormous problem of,
     1 30h collinearity in the data set./
     1 45h Try the analysis again in a lower dimension./)
 8040  FORMAT(' Among these ',I8,' subsets,',I8,' were discarded'/
     1 ' because they gave a singular system of equations.')
 8045  FORMAT(' Each of these ',I8,' subsets gave a nonsingular system',
     1 ' of equations.')
 8050  FORMAT(/' The program has considered all ',I7,' subsets of ',I3,
     1 ' cases out of ',I5,'.')
 8065  FORMAT(/' The best subset consisted of the observations'/14I5//)
 8070  FORMAT(/' More than the half of the data lie on the same '
     1 'hyperplane,'/' with coefficients = '//)
 8080  FORMAT(3X,A10,F20.5)
 8090  FORMAT(3X,10h  constant,F20.5)
 8100  FORMAT(/15h Scale estimate,10X,2H= ,F15.5/)
 8110  FORMAT(10h There are,2X,I4,' points on the hyperplane.')
 8130  FORMAT(//5X,8hvariable,9X,11hcoefficient/3X,30(1H-))
 8190  FORMAT(//1X,A3,' objective function = ',F15.5)
 8200  FORMAT(/' Preliminary ',A3,' scale  = ',F15.5)
 8210  FORMAT(/' Robust R-squared (coefficient of determination) = ',
     1 F7.5)      
 8215  FORMAT(/' Final ',A3,' scale estimate = ',F15.5)
 8235  FORMAT(/' ---------------------------------------------------'
     1 '-------------------------')
 8240  FORMAT(/' Corresponding formulas: (for this data set ',
     1 'n = ',I5,', h = ',I5,', p = ',I3,')')
 8250  FORMAT(//
     1 ' LMS objective = obj(x ,y ) = minimum |y - x *theta|'/ 
     1 '                      i  i     theta    i   i       h:n')
 8260  FORMAT(//
     1 '                                                 | 1   h ',
     1 '               2   |'/
     1 ' LTS objective = obj(x ,y ) = minimum squareroot | -  sum',
     1 ' |y - x *theta|    |'/
     1 '                      i  i     theta             | h  j=1',
     1 '   i   i       j:n |')
 8300  FORMAT(//
     1 ' Preliminary LMS scale = s(x ,y ) = constant*obj(x ,y )'/
     1 '                            i  i                  i  i ')
 8310  FORMAT(//
     1 ' Preliminary LTS scale = s(x ,y ) = constant*obj(x ,y )'/
     1 '                            i  i                  i  i ')
 8320  FORMAT(//
     1 '                        |  s(x ,y )  | 2'/
     1 '                        |     i  i   |  '/
     1 ' Robust R-squared = 1 - | ---------- |  '/
     1 '                        |   s(1,y )  |  '/
     1 '                        |        i   |  ')
 8325  FORMAT(//
     1 '                        |  s(x ,y )  | 2'/
     1 '                        |     i  i   |  '/
     1 ' Robust R-squared = 1 - | ---------- |  '/
     1 '                        |   s(0,y )  |  '/
     1 '                        |        i   |  ')
 8335  FORMAT(//
     1 '                                       |    n       2   |'/
     1 '                                       |   sum  w *r    |'/
     1 '                                       |   i=1   i  i   |'/
     1 ' Final ',A3,' scale estimate = squareroot | -------------- |'/
     1 '                                       |    n           |'/
     1 '                                       |  (sum w ) - p  |'/
     1 '                                       |   i=1  i       |')
 8350  FORMAT(/' ---------------------------------------------------'
     1 '-------------------------'//' Resistant diagnostic: '/
     1 ' *********************'/)
 8360  FORMAT(' This diagnostic is based on the ',A3,' and becomes',      
     1 ' larger than 2.5 (say) '/' in regression outliers as well',
     1 ' as in good leverage points'/' and in bad leverage points.'//
     1 1X,'     ',3X,13h        u    ,3X,10hresistant /
     1 1X,'   i ',12X,'i',6X,10hdiagnostic/1X,34(1H-))
 8390  FORMAT(1X,I4,3X,F13.4,3X,F10.4)
 8400  FORMAT(//
     1 '                        | y - x *theta |   '/
     1 '                           i   i           '/
     1 ' where    u  = maximum ------------------- '/
     1 '           i    theta   | y - x *theta |   '/
     1 '                           j   j        h:n')
 8410  FORMAT(//
     1 '                                | y - x *theta |   '/       
     1 '                                   i   i           '/
     1 ' where    u  = maximum -------------------------------------- '/       
     1 '           i    theta             | 1  h                2   |'/      
     1 '                       squareroot | - sum |y - x *theta|    |'/
     1 '                                  | h j=1   k   k       j:n |')
 8420  FORMAT(//
     1 '                                         n         '/
     1 ' and      resistant diagnostic  = u  / median (u ) '/
     1 '                              i    i    j=1     j  ')
 8430  FORMAT(//' ---------------------------------------------------'
     1 '-------------------------'//
     1 ' A complete diagnosis of the cases can be obtained as',
     1 ' follows:'//
     1 ' (1) The standardized ',A3,' residuals provided by this',
     1 ' program'/
     1 '     indicate regression outliers, i.e. cases for which'//
     1 '                  |res /s| > 2.5    (roughly).'/
     1 '                      i                       '//
     1 ' (2) Run the program MINVOL on the data set formed by the',
     1 ' explanatory '/
     1 '     variables only. This yields the minimum',
     1 ' volume ellipsoid (MVE)'/
     1 '     scatter matrix and the corresponding robust distances RD '/
     1 '     with their cutoff value c.                              i'/
     1 '     Leverage points (= x-outliers) are then cases with'//
     1 '                     RD  > c    (roughly).'/
     1 '                       i                  ')
 8440  FORMAT(/' (3) Combining this information classifies the cases',
     1 ' into four types:'//
     1 '     (a) regular observation:  RD  <= c  and  |res /s| <= 2.5'/
     1 '                                 i                i          '//
     1 '     (b) vertical outlier   :  RD  <= c  and  |res /s| >  2.5'/
     1 '                                 i                i          '//
     1 '     (c) good leverage point:  RD  >  c  and  |res /s| <= 2.5'/
     1 '                                 i                i          '//
     1 '     (d) bad leverage point :  RD  >  c  and  |res /s| >  2.5'/
     1 '                                 i                i          '//
     1 ' For more background and some examples see'//
     1 '     Rousseeuw, P.J., and van Zomeren, B.C. (1990),'/
     1 '     Unmasking Multivariate Outliers and Leverage Points,'/
     1 '     Journal of the American Statistical Association, 85,',
     1 ' 633-639'//
     1 ' where a plot of res /s versus RD  is proposed.'/
     1 '                    i            i             ')
       RETURN
       END
CC     ------------------------------------------------------------------------
CC     *RDAT*: READ DATA AND OPTIONS
CC     ------------------------------------------------------------------------
       SUBROUTINE RDAT(NCAS,NVAR,JCST,JPRT,NFAC,NVAD,VALMS,
     1 X,Y,JPLT,JMISS,PREC,JPLACE,LAB,XMED,XMAD,AW,AW2,JNDEX,
     1 RESDU,WEIGHTS,MAXP1,MAXN,MVAL,MAXP,LUA,LUB,LUC,JHEAD,
     1 YNSAVE,JDIAG,NSTOP,FNAMEA,FNAMEB,FNAMEC,MROB,FACLMS,FACLTS)
       DIMENSION X(MAXP1,MAXN),Y(MAXN)
       DIMENSION AW(MAXN),AW2(MAXN),RESDU(MAXN)
       DIMENSION WEIGHTS(MAXN),XMED(MAXP1)
       DIMENSION XMAD(MAXP1),VALMS(MAXP1)
       DIMENSION FACLMS(11),FACLTS(11)
       INTEGER JMISS(MAXP1),JNDEX(MAXN),JPLACE(MAXP1)
       CHARACTER YN,YNOK,YNSAVE,YNM
       CHARACTER*30 FNAMEA,FNAMEB,FNAMEC
       CHARACTER*10 LAB(MAXP1)
       CHARACTER*60 JFMT,JHEAD
       CHARACTER*10 LABJ
       CHARACTER HELP(10)
       LOGICAL EXISTA
       EQUIVALENCE (LABJ,HELP)
       JDIAG=0
       LOCA=0
       WRITE (*,8000)
CC-----GIVE THE NUMBER OF CASES
 10    WRITE (*,8010)
  11   READ(*,*,ERR=11)NCAS
       IF (NCAS.GT.MAXN.OR.NCAS.LE.1) THEN
	 IF (NCAS.GT.MAXN) WRITE (*,8020) MAXN
	 IF (NCAS.LE.1) WRITE(*,8030)
	 GOTO 10
       ENDIF
CC-----CONSTANT TERM OR NOT
       WRITE (*,8040)
 20    READ (*,9000) YN
       IF (YN.EQ.'y') YN='Y'
       IF (YN.EQ.'n') YN='N'
       IF (YN.NE.'Y'.AND.YN.NE.'N') THEN
	 WRITE (*,8050)
	 GOTO 20
       ELSE
	 IF (YN.EQ.'Y') JCST=1
	 IF (YN.EQ.'N') JCST=0
       ENDIF
CC-----TOTAL NUMBER OF VARIABLES IN THE DATA SET
       WRITE(*,8060)
 30    READ(*,*,ERR=30)JVARS
       IF (JVARS.LT.1.OR.JVARS.GT.50) THEN
	 WRITE(*,8050)
	 GOTO 30
       ENDIF
       IF (JVARS.EQ.1) THEN
CC-----THE PROBLEM IS REDUCED TO LOCATION
	  LOCA=1
	  NVAR=1
	  JCST=1
	  WRITE(*,8070)
	  JPLACE(MAXP1)=1
		       ELSE
CC-----WHICH VARIABLE WILL BE TAKEN AS THE RESPONSE VARIABLE
	  WRITE(*,8080) JVARS
 40	  READ(*,*,ERR=40)JPLACE(MAXP1)
	  IF (JPLACE(MAXP1).LT.1.OR.JPLACE(MAXP1).GT.JVARS) THEN
	   WRITE(*,8050)
	   GOTO 40
	 ENDIF
       ENDIF
CC-----GIVE A LABEL FOR THIS VARIABLE
       WRITE(*,8090)
       READ(*,9010) LABJ
       CALL MOVE(HELP)
       LAB(MAXP1)=LABJ
       IF (LOCA.EQ.1) GOTO 100
       IF (JCST.EQ.0) THEN
CC-----JWR = MAXIMAL NUMBER OF EXPLANATORY VARIABLES POSSIBLE
	 JWR=MAXP
	 IF((JVARS-1).LT.MAXP) JWR=JVARS-1
		      ELSE
	 JWR=MAXP-1
	 IF ((JVARS-1).LT.MAXP) JWR=JVARS-1
       ENDIF
CC-----HOW MANY EXPLANATORY VARIABLES DO YOU WANT TO USE
       WRITE(*,8100) JWR
 60    READ(*,*,ERR=60)NVAR
       IF (NVAR.EQ.0) THEN
	 WRITE(*,8070)
	 LOCA=1
	 NVAR=1
	 JCST=1
	 GOTO 100
       ENDIF
       IF (NVAR.LT.1.OR.NVAR.GT.JWR) THEN
	 WRITE(*,8050)
	 GOTO 60
       ENDIF
       IF (NVAR.NE.(JVARS-1)) GOTO 80
       IF (JCST.EQ.1) NVAR=JVARS
       IF (JCST.EQ.0) NVAR=JVARS-1
       IF (NVAR.GT.MAXP) WRITE (*,8115) MAXP
       IF (NCAS.GT.MAXN.OR.NVAR.GT.MAXP) GOTO 10
       IF (NCAS.LE.(NVAR*2)) THEN
CC-----  TOO MANY CASES
	 WRITE (*,8110) NCAS,NVAR
	 IF (JCST.EQ.1) WRITE(*,8120)
	 GOTO 10
       ENDIF
       NVAD=NVAR+1
       JPLACE(NVAD)=JPLACE(MAXP1)
       LAB(NVAD)=LAB(MAXP1)
       JP=0
       WRITE(*,8130)
       DO 70 J=1,NVAR
       JP=JP+1
       IF (.NOT.(JCST.EQ.1.AND.J.EQ.NVAR)) THEN
	 IF (J.EQ.JPLACE(MAXP1)) JP=JP+1
	 JPLACE(J)=JP
	 WRITE(*,8140) J,JP
	 READ(*,9010) LABJ
	  CALL MOVE(HELP)
	  LAB(J)=LABJ
       ENDIF
 70    CONTINUE
       GOTO 135
 80    IF (JCST.EQ.1) NVAR=NVAR+1
       IF (NCAS.LE.(NVAR*2)) THEN
	 WRITE (*,8110) NCAS,NVAR
	 IF (JCST.EQ.1) WRITE(*,8120)
	 GOTO 10
       ENDIF
 100   NVAD=NVAR+1
       JPLACE(NVAD)=JPLACE(MAXP1)
       LAB(NVAD)=LAB(MAXP1)
       IF (LOCA.EQ.1) GOTO 135
CC-----EXPLANATORY VARIABLES
       WRITE(*,8130)
       DO 110 J=1,NVAR
       JTT=J-1
       IF (JCST.EQ.1.AND.J.EQ.NVAR) GOTO 110
 120   WRITE(*,8150) J
       READ(*,9020) JPLACE(J),LABJ
	  CALL MOVE(HELP)
	  LAB(J)=LABJ
       IF (JPLACE(J).LT.1.OR.JPLACE(J).GT.JVARS) GOTO 120
       IF (JPLACE(J).EQ.JPLACE(NVAD)) THEN
	 WRITE (*,8160)
	 GOTO 120
       ENDIF
       IF (J.EQ.1) GOTO 110
       DO 130  JK=1,JTT
       IF (JPLACE(JK).NE.JPLACE(J))GOTO 130
       WRITE(*,8160)
       GOTO 120
 130   CONTINUE
 110   CONTINUE
CC-----LMS OR LTS?
 135   WRITE (*,8165)
 136   READ (*,9000) YN
       IF(.NOT.(YN.EQ.'1'.OR.YN.EQ.'2')) THEN
	 WRITE (*,8050)
	 GOTO 136
       ENDIF
       MROB=JNTGR(YN)
CC-----HOW MUCH OUTPUT?
       IF (LOCA.NE.1) THEN
	  WRITE (*,8170)
 150      READ (*,9000) YN
          IF(.NOT.(YN.EQ.'0'.OR.YN.EQ.'1'.OR.YN.EQ.'2')) THEN
	    WRITE (*,8050)
	    GOTO 150
          ENDIF
		      ELSE
	  WRITE (*,8175)
 155      READ (*,9000) YN
          IF(.NOT.(YN.EQ.'0'.OR.YN.EQ.'1')) THEN
	    WRITE (*,8050)
	    GOTO 155
          ENDIF
       ENDIF
       JPRT=JNTGR(YN)
CC-----DO YOU WANT TO LOOK AT THE RESIDUALS?
       IF (LOCA.NE.1) THEN
		      WRITE(*,8180)
		      ELSE
		      WRITE (*,8190)
       ENDIF
 160   READ (*,9000) YN
       IF(.NOT.(YN.EQ.'0'.OR.YN.EQ.'1'.OR.(YN.EQ.'2'.AND.LOCA.NE.1)
     1	       .OR.(YN.EQ.'3'.AND.LOCA.NE.1) ) ) THEN
	 WRITE (*,8050)
	 GOTO 160
       ENDIF
       JPLT=JNTGR(YN)
       IF (JCST.EQ.1.AND.NVAR.EQ.1.AND.JPLT.NE.0) JPLT=2
CC-----DO YOU WANT TO COMPUTE OUTLIER DIAGNOSTICS
       IF (LOCA.NE.1) THEN
	  WRITE(*,8200)
 162	  READ (*,9000) YN
	  IF (YN.EQ.'y') YN='Y'
	  IF (YN.EQ.'n') YN='N'
	  IF (YN.NE.'Y'.AND.YN.NE.'N') THEN
	    WRITE (*,8050)
	    GOTO 162
	  ENDIF
	  IF (YN.EQ.'Y') THEN
		JDIAG=1
                         ELSE
		JDIAG=0
	  ENDIF
       ENDIF
CC-----NAME OF THE FILE CONTAINING THE DATA
 165   WRITE(*,8210)
       READ(*,9030) FNAMEA
       IF (FNAMEA.EQ.'Key'.OR.FNAMEA.EQ.'key'.OR.FNAMEA.EQ.'KEY') THEN
	  FNAMEA='CON'
					       ELSE
	  INQUIRE(FILE=FNAMEA,EXIST=EXISTA,IOSTAT=NER)
	  IF (NER.NE.0.OR.(.NOT.EXISTA)) THEN
	    IF (NER.EQ.7.OR.NER.EQ.10.OR.NER.EQ.1027.OR.NER.EQ.1032.OR.       
     1	       NER.EQ.1033.OR.NER.EQ.1002.OR.NER.EQ.1003.OR.
     1	       NER.EQ.1004.OR.NER.EQ.1005.OR.NER.EQ.1007.OR.
     1         (.NOT.EXISTA))     THEN
	         WRITE(*,8220)
	         GOTO 165
             ELSE
	         WRITE(*,8230) NER
	         NSTOP=1
	         RETURN
	    ENDIF
	  ENDIF
       ENDIF
       IF (FNAMEA.EQ.'CON') THEN
	 WRITE(*,8240)
CC-----DO YOU WANT TO SAVE THE DATA
 170	 READ(*,9000) YNSAVE
	 IF (YNSAVE.EQ.'y') YNSAVE='Y'
	 IF (YNSAVE.EQ.'n') YNSAVE='N'
	 IF (YNSAVE.NE.'Y'.AND.YNSAVE.NE.'N') THEN
	    WRITE (*,8050)
	    GOTO 170
	 ENDIF
	 IF (YNSAVE.EQ.'Y') THEN
 175	    WRITE(*,8250)
	    READ(*,9030) FNAMEC
	    INQUIRE(FILE=FNAMEC,EXIST=EXISTA)
	    IF (EXISTA) THEN
	      WRITE(*,8225)
	      GOTO 175
            ENDIF
	    OPEN(LUC,FILE=FNAMEC,STATUS='NEW')
	 ENDIF
       ENDIF
       OPEN(LUA,FILE=FNAMEA)
CC-----WHERE DO YOU WANT THE OUTPUT
 185   WRITE(*,8260)
       READ(*,9030) FNAMEB
       IF (FNAMEB.EQ.'con'.OR.FNAMEB.EQ.'Con') FNAMEB='CON'
       IF (FNAMEB.EQ.'prn'.OR.FNAMEB.EQ.'Prn') FNAMEB='PRN'
       IF (FNAMEB.EQ.'CON'.OR.FNAMEB.EQ.'PRN') THEN
	  OPEN(LUB,FILE=FNAMEB)
	 ELSE
	  INQUIRE(FILE=FNAMEB,EXIST=EXISTA)
	  IF (EXISTA) THEN
	    WRITE(*,8225)
	    GOTO 185
          ENDIF
	  OPEN(LUB,FILE=FNAMEB,STATUS='NEW')
       ENDIF
CC-----PLEASE ENTER A TITLE FOR THE OUTPUT
       WRITE (*,8270)
       READ (*,9040) JHEAD
       NFAC=NVAR-1
CC-----DO YOU WANT TO READ THE DATA IN FREE FORMAT
       WRITE (*,8280)
 180   READ (*,9000) YN
       IF (YN.EQ.'y') YN='Y'
       IF (YN.EQ.'n') YN='N'
       IF (YN.NE.'Y'.AND.YN.NE.'N') THEN
	 WRITE (*,8050)
	 GOTO 180
       ENDIF
       IF (YN.NE.'Y') THEN
CC-----READ FORMAT FOR THE INPUT
	 WRITE(*,8290)
	 READ (*,9040) JFMT
       ENDIF
CC-----OPTION FOR THE TREATMENT OF MISSING VALUE
       IF (LOCA.NE.1) THEN
	  WRITE (*,8310)
		      ELSE
	  WRITE (*,8320)
       ENDIF
 200   READ (*,9000) YNOK
       IF (.NOT.(YNOK.EQ.'0'.OR.YNOK.EQ.'1'.OR.
     1		  (YNOK.EQ.'2'.AND.LOCA.NE.1) ) ) THEN
	 WRITE (*,8050)
	 GOTO 200
       ENDIF
       MVAL=JNTGR(YNOK)
CC-----ABSTRACT OF ALL THE OPTIONS
       WRITE(*,8325)
       WRITE(*,8326) JHEAD
       WRITE(*,8330) NCAS
       IF (LOCA.EQ.1) THEN
         WRITE(*,8340) LAB(NVAD)
	 WRITE(*,8345) 
		      ELSE
	 IF (JCST.EQ.0) WRITE(*,8335) NVAR
	 IF (JCST.EQ.1) WRITE(*,8335) NFAC
         WRITE(*,8340) LAB(NVAD)
	 IF (JCST.EQ.0) WRITE(*,8355) 
	 IF (JCST.EQ.1) WRITE(*,8350) 
       ENDIF
       IF (MROB.EQ.1) WRITE(*,8360)
       IF (MROB.EQ.2) WRITE(*,8365)
       IF (FNAMEA.NE.'CON') WRITE(*,8370) FNAMEA
       IF (FNAMEA.EQ.'CON') WRITE(*,8380)
       IF (YNSAVE.EQ.'Y') WRITE(*,8390) FNAMEC
       IF (YN.EQ.'Y') WRITE(*,8410)
       IF (YN.EQ.'N') WRITE(*,8420) JFMT
       IF (JPRT.EQ.0) WRITE (*,8430)
       IF (JPRT.EQ.1.AND.LOCA.NE.1) WRITE (*,8440)
       IF (JPRT.EQ.2.OR.(JPRT.EQ.1.AND.LOCA.EQ.1)) WRITE (*,8450)
       IF (JPLT.EQ.0) WRITE(*,8460)
       IF (JPLT.EQ.1) WRITE(*,8470)
       IF (JPLT.EQ.2) WRITE(*,8480)
       IF (JPLT.EQ.3) WRITE(*,8490)
       IF (MVAL.EQ.0) WRITE(*,8520)
       IF (MVAL.EQ.1.AND.LOCA.NE.1) WRITE(*,8530)
       IF (MVAL.EQ.1.AND.LOCA.EQ.1) WRITE(*,8535)
       IF (MVAL.EQ.2) WRITE(*,8540)
       WRITE(*,8550) FNAMEB
       WRITE (*,8560)
 210   READ (*,9000) YNOK
       IF (YNOK.EQ.'y') YNOK='Y'
       IF (YNOK.EQ.'n') YNOK='N'
       IF (YNOK.NE.'Y'.AND.YNOK.NE.'N') THEN
	 WRITE (*,8050)
	 GOTO 210
       ENDIF
       IF (YNOK.NE.'Y') GOTO 10
       IF (LOCA.EQ.1)
     1 CALL LCAT(NCAS,NVAR,JCST,JPRT,NVAD,X,Y,RESDU,WEIGHTS,PREC,MROB,
     1 XMED,XMAD,NZWE,AVW,JPLT,AW,JNDEX,MAXP1,MAXN,MVAL,LUA,LUB,LUC,
     1 JREG,JHEAD,FNAMEA,FNAMEB,FNAMEC,YNSAVE,LAB,JFMT,JVARS,YN,JPLACE,
     1 AW2,FACLMS,FACLTS)
       IF (MVAL.NE.0) THEN
	 DO 220 J=1,NVAD
	 IF (J.EQ.1) THEN
	   WRITE (*,8570)
 225	   READ (*,9000) YNM
	   IF (YNM.EQ.'y') YNM='Y'
	   IF (YNM.EQ.'n') YNM='N'
	   IF (YNM.NE.'Y'.AND.YNM.NE.'N') THEN
	      WRITE(*,8050)
	      GOTO 225
	   ENDIF
	   IF (YNM.EQ.'Y'.OR.YNM.EQ.'y') THEN
	      YNM='Y'
	      WRITE (*,8580)
 226       READ(*,*,ERR=226)CODE
	   ENDIF
	 ENDIF
	 IF (YNM.NE.'Y') THEN
	   IF (J.EQ.NVAR.AND.JCST.EQ.1) GOTO 220
	   IF (J.NE.NVAD) WRITE (*,8590) LAB(J)
	   IF (J.EQ.NVAD) WRITE (*,8600)
 230	   READ (*,9000) YNOK
	   IF (YNOK.EQ.'y') YNOK='Y'
	   IF (YNOK.EQ.'n') YNOK='N'
	   IF (YNOK.NE.'Y'.AND.YNOK.NE.'N') THEN
	     WRITE (*,8050)
	     GOTO 230
	   ENDIF
			 ELSE
	   YNOK='Y'
	 ENDIF
	 IF (YNOK.NE.'Y') THEN
	    JMISS(J)=0
			  ELSE
	    JMISS(J)=1
CC-----READ THE VALUE WHICH HAS TO BE INTERPRETED AS THE MISSING VALUE CODE
	    IF (YNM.NE.'Y')  THEN
	      WRITE(*,8610)
  231      READ(*,*,ERR=231)VALMS(J)
			     ELSE
	      VALMS(J)=CODE
	    ENDIF
	 ENDIF
 220   CONTINUE
       ENDIF
CC-----ENTER THE DATA FOR EACH CASE
       IF (FNAMEA.EQ.'CON') WRITE(*,8615)
       DO 250 JNC=1,NCAS
       IF (FNAMEA.EQ.'CON') THEN
	 IF (JNC.EQ.1) WRITE(*,8620) JNC
	 IF (JNC.NE.1) WRITE(*,8630) JNC
       ENDIF
       JNDEX(JNC)=JNC
       IF (JCST.EQ.0) THEN
	 IF (YN.EQ.'Y') THEN
	    READ(LUA,*)(AW(J),J=1,JVARS)
	    DO 260 J=1,NVAD
	    JH=JPLACE(J)
	    X(J,JNC)=AW(JH)
 260	    CONTINUE
	    IF (YNSAVE.EQ.'Y') WRITE(LUC,*)(AW(J),J=1,JVARS)
			ELSE
	    READ(LUA,JFMT)(AW(J),J=1,JVARS)
	    DO 270 J=1,NVAD
	    JH=JPLACE(J)
	    X(J,JNC)=AW(JH)
 270	    CONTINUE
	    IF (YNSAVE.EQ.'Y') WRITE(LUC,*)(AW(J),J=1,JVARS)
	 ENDIF
       ELSE
	 X(NVAR,JNC)=1.0
	 IF (YN.EQ.'Y') THEN
	   READ(LUA,*)(AW(J),J=1,JVARS)
	   DO 280 J=1,NVAD
	   IF (.NOT.(J.EQ.NVAR.AND.JCST.EQ.1)) THEN
	     JH=JPLACE(J)
	     X(J,JNC)=AW(JH)
	   ENDIF
 280	   CONTINUE
	   IF (YNSAVE.EQ.'Y') WRITE(LUC,*)(AW(J),J=1,JVARS)
	 ELSE
	   READ(LUA,JFMT)(AW(J),J=1,JVARS)
	   DO 290 J=1,NVAD
	   IF (.NOT.(J.EQ.NVAR.AND.JCST.EQ.1)) THEN
	     JH=JPLACE(J)
	     X(J,JNC)=AW(JH)
	   ENDIF
 290	   CONTINUE
	   IF (YNSAVE.EQ.'Y') WRITE(LUC,*) (AW(J),J=1,JVARS)
	 ENDIF
       ENDIF
 250   CONTINUE
       IF (YNSAVE.EQ.'Y') CLOSE(LUC,STATUS='KEEP')
       IF (YNSAVE.EQ.'Y') WRITE(*,8395) FNAMEC
CC-----FORMATS
 8000  FORMAT(/////////////////30X,19(1H*)/
     1 30X,19H* P R O G R E S S */30X,19(1H*)///////)
 8010  FORMAT(/36h Enter the number of cases please : $)
 8020  FORMAT(/34h There are too many cases (at most,I5,2H ),
     1 24h according to the limits
     1 /58h of the arrays in the program. The user has to adapt these
     1 /59h limits such that the concerning limits are greater than or
     1 /48h equal to the number of cases in his data set or
     1 /37h another regression can be performed.//)
 8030  FORMAT(/27h There are not enough cases
     1 27h (at least 2 are required).//)
 8040  FORMAT(/47h Do you want a constant term in the regression?/
     1 20h Answer yes or no : $)
 8050  FORMAT(42h Not allowed !  Enter your choice again : $)
 8060  FORMAT(/46h What is the total number of variables in your,
     1 10h data set?/1X,55(1H-)/
     1 41h Please give a number between 1 and 50 : $)
 8070  FORMAT(//' The problem is reduced to estimating univariate',
     1 ' location and scale.')
 8080  FORMAT(/51h Which variable do you choose as response variable?/
     1 1X,50(1H-)/14h Out of these ,I4,21h give its position : $)
 8090  FORMAT(/31h Give a label for this variable,
     1 27h (at most 10 characters) : $)
 8100  FORMAT(/50h How many explanatory variables do you want to use,
     1 17h in the analysis?/1X,66(1H-)/
     1 10h (at most ,I4,5H ) : $)
 8110  FORMAT(//46h Too many coefficients according to the number,
     1 10h of cases.//16h Number of cases,8X,2H= ,I5/
     1 26h Number of coefficients = ,I5/25h The number of cases must,
     1 37h be twice the number of coefficients.)
 8115  FORMAT(//41h There are too many coefficients (at most,I3,2H ),
     1 24h according to the limits/30h of the arrays in the program.,
     1 28h The user has to adapt these/21h limits such that the,
     1 38h concerning limits are greater than or/
     1 50h equal to the number of coefficients in the model.//)
 8120  FORMAT(31h (Including the constant term!))
 8130  FORMAT(//40h Explanatory variables      :   position,
     1 32h   label (at most 10 characters)/
     1 1X,32(1h-),4(1h+),6(1h-),10(1h+),19(1h-))
 8140  FORMAT(9h Number  ,I4,15X,1H:,4X,I4,6X$)
 8150  FORMAT(9h Number  ,I4,15X,':    ',$)
 8160  FORMAT(/51h This position has already been choosen for another,
     1 10h variable./35h Enter the right position please : )
 8165  FORMAT(/' Which robust method do you want to use? '/
     1 ' --------------------------------------- '/
     1 ' 1 = Least median of squares (LMS)'/
     1 ' 2 = Least trimmed squares (LTS)'/
     1 ' Enter your choice : '$)
 8170  FORMAT(/29h How much output do you want?/1X,28(1H-)/
     1 17h 0 = small output,7X,26h: limited to basic results/
     1 25h 1 = medium-sized output:,
     1 53h also includes a table with the observed values of y,/
     1 25X,50h the estimates of y, the residuals and the weights/
     1 17h 2 = large output,7X,
     1 45h: also includes the raw and standardized data/
     1 21h Enter your choice : $)
 8175  FORMAT(/29h How much output do you want?/1X,28(1H-)/
     1 ' 0 = small output: limited to basic results'/
     1 ' 1 = large output:',
     1 ' also includes a table with the observed values of y,'/
     1 18X,' the residuals and the weights'/
     1 21h Enter your choice : $)
 8180  FORMAT(/38h Do you want to look at the residuals?/1X,37(1H-)/
     1 22h 0 = no residual plots/
     1 56h 1 = plot of standardized residuals versus the estimated,
     1 11h value of y/39h 2 = plot of the standardized residuals,
     1 36h versus the index of the observation/13h 3 = performs,
     1 29h both types of residual plots/21h Enter your choice : $)
 8190  FORMAT(/38h Do you want to look at the residuals?/1X,37(1H-)/
     1 22h 0 = no residual plots/
     1 39h 1 = plot of the standardized residuals,
     1 36h versus the index of the observation/
     1 21h Enter your choice : $)
 8200  FORMAT(/' Do you want the corresponding resistant diagnostic?'/
     1 20h Answer yes or no : $)
 8210  FORMAT(/46h Give the name of the file containing the data,
     1 29h (e.g. type  a:example.dat ),/8h or type,
     1 51h  key  if you prefer to enter the data by keyboard./
     1 22h What do you choose ? $)
 8220  FORMAT(//52h This file does not exist, please enter another one.)
 8225  FORMAT(//52h This file already exists, please enter another one.)
 8230  FORMAT(/22h Fortran error code : ,I8/)
 8240  FORMAT(/42h Do you want to save your data in a file ?/
     1 20h Answer yes or no : $)
 8250  FORMAT(/45h In which file do you want to save your data?/
     1 26h type e.g.  b:save.dat  : $)
 8260  FORMAT(/31h Where do you want your output?/1X,30(1H-)/
     1 43h    type  con  if you want it on the screen/
     1 44h or type  prn  if you want it on the printer/
     1 48h or type the name of a file (e.g. b:example.out)/
     1 23h What do you choose ?  $)
 8270  FORMAT(/36h Please enter a title for the output,
     1 25h (at most 60 characters)./1X,60(1H-)/1X,$)
 8280  FORMAT(/45h Do you want to read the data in free format?/
     1 1X,44(1H-)/49h This means that you only have to insert blank(s),
     1 17h between numbers./25h (We advise users without,
     1 45h knowledge of fortran formats to answer yes.)/
     1 28h Make your choice (yes/no): $)
 8290  FORMAT(/33h Your desired fortran format is :/
     1 22h (between brackets and,
     1 43h at most 60 characters, e.g. (2F3.0,F1.0) ))
 8310  FORMAT(/53h Choose an option for the treatment of missing values/
     1 1X,52(1H-)/44h 0 = there are no missing values in the data/
     1 52h 1 = elimination of the cases for which at least one,
     1 20h variable is missing/32h 2 = estimates are filled in for,
     1 18h unobserved values/21h Enter your choice : $)
 8320  FORMAT(/53h Choose an option for the treatment of missing values/
     1 1X,52(1H-)/44h 0 = there are no missing values in the data/
     1 43h 1 = elimination of the cases for which the,
     1 20h variable is missing/21h Enter your choice : $)
 8325  FORMAT(//////////35X,11('*')/35X,'* Summary *'/35X,11('*')//)
 8326  FORMAT(' Title:   ',A60)
 8330  FORMAT(' Number of cases in the data set: ',18X,I5)
 8335  FORMAT(' Number of explanatory variables in the regression: ',I5)
 8340  FORMAT(' The response variable is: ',A10)
 8345  FORMAT(' You have chosen univariate location and scale '
     1 'estimation.')
 8350  FORMAT(' You have chosen a model with a constant term ',
     1 '(regression with intercept).')
 8355  FORMAT(' You have chosen a model without constant term ',
     1 '(no intercept).')
 8360  FORMAT(' You have chosen the least median of squares (LMS)',
     1 ' method.')
 8365  FORMAT(' You have chosen the least trimmed squares (LTS)',
     1 ' method.')
 8370  FORMAT(' Your data reside in the file: ',A30)
 8380  FORMAT(41h The data will be read from the keyboard.)
 8390  FORMAT(' The data will be saved in the file: ',A30)
 8395  FORMAT(//' The data will be saved in the file: ',A30//)
 8410  FORMAT(38h The data will be read in free format.)
 8420  FORMAT(' Data input format:'/5X,A60)
 8430  FORMAT(' You have chosen small output.')
 8440  FORMAT(' You have chosen medium-sized output.')
 8450  FORMAT(' You have chosen large output.')
 8460  FORMAT(' You have chosen not to plot residuals.')
 8470  FORMAT(' You have chosen a plot of residuals versus ',
     1 'estimated y.')      
 8480  FORMAT(' You have chosen a plot of residuals versus ',
     1 'their index.')      
 8490  FORMAT(' You have chosen a plot of residuals versus their index'
     1 ' and '/
     1 '                                     versus estimated y.')
 8520  FORMAT(' The data is assumed not to have missing values.')
 8530  FORMAT(' Treatment of missing values in option 1:'/
     1 '    cases with at least one missing value will be deleted.')
 8535  FORMAT(' Any case with a missing value will be deleted.')
 8540  FORMAT(41h Treatment of missing values in option 2:/
     1 '    First, any case with a missing value for the response'
     1 ' variable'/
     1 '    or for all explanatory variables will be deleted.'/
     1 '    For the remaining cases, each missing value is replaced by'/       
     1 '    the median of the non-missing values.')
 8550  FORMAT(' The output will be written on the file: ',A30)
 8560  FORMAT(/27h Are all these options ok ?,13H yes or no : $)
 8570  FORMAT(/39h Is there a unique value which is to be,
     1 12h interpreted/43h as a missing measurement for any variable?/
     1 20h Answer yes or no : $)
 8580  FORMAT(/27h Please enter this value : $)
 8590  FORMAT(15h Does variable ,A10,
     1 26h contain missing value(s)?/20h Answer yes or no : $)
 8600  FORMAT(53h Does the response variable contain missing value(s)?/
     1 20h Answer yes or no : $)
 8610  FORMAT(33h Enter the value of this variable,
     1 31h which has to be interpreted as/
     1 26h the missing value code : $)
 8615  FORMAT(//31h Enter your data for each case.//)
 8620  FORMAT(1X,26h The data for case number ,I4,3H : $)
 8630  FORMAT(26h The data for case number ,I4,3H : $)
 9000  FORMAT(A1)
 9010  FORMAT(A10)
 9020  FORMAT(bnI4,6X,A10)
 9030  FORMAT(A30)
 9040  FORMAT(A60)
       RETURN
       END
