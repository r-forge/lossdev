/*
##################################################################################################
##                                                                                              ##
##    lossDev is an R-package.                                                                  ##
##    It is a Bayesian time series model of loss development.                                   ##
##    Features include skewed Student-t distribution with time-varying scale parameters,        ##
##    an expert prior for the calendar year effect,                                             ##
##    and accommodation for structural breaks in the consumption path of services.              ##
##                                                                                              ##
##    Copyright © 2008, National Council On Compensation Insurance Inc.,                        ##
##                                                                                              ##
##    This file is part of lossDev.                                                             ##
##                                                                                              ##
##    lossDev is free software: you can redistribute it and/or modify                           ##
##    it under the terms of the GNU General Public License as published by                      ##
##    the Free Software Foundation, either version 3 of the License, or                         ##
##    (at your option) any later version.                                                       ##
##                                                                                              ##
##    This program is distributed in the hope that it will be useful,                           ##
##    but WITHOUT ANY WARRANTY; without even the implied warranty of                            ##
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                             ##
##    GNU General Public License for more details.                                              ##
##                                                                                              ##
##    You should have received a copy of the GNU General Public License                         ##
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.                     ##
##                                                                                              ##
##################################################################################################
*/

#ifndef JAGS_LAPACK_H_
#define JAGS_LAPACK_H_

#define F77_FUNC(x,y) x##_


#define F77_DGEEV F77_FUNC(dgeev,DGEEV)
#define F77_DSYEV F77_FUNC(dsyev,DSYEV)
#define F77_DGESV F77_FUNC(dgesv,DGESV)
#define F77_DSYSV F77_FUNC(dsysv,DSYSV)
#define F77_DPOSV F77_FUNC(dposv,DPOSV)
/* #define F77_DGEQP3 F77_FUNC(dgeqp3,DGEQP3) */
#define F77_DPOTRF F77_FUNC(dpotrf,DPOTRF)
#define F77_DLANGE F77_FUNC(dlange,DLANGE)
#define F77_DGEMM  F77_FUNC(dgemm,DGEMM)
#define F77_DGEMV  F77_FUNC(dgemv,DGEMV)
#define F77_DCOPY  F77_FUNC(dcopy,DCOPY)
#define F77_DSCAL  F77_FUNC(dscal,DSCAL)
#define F77_DAXPY  F77_FUNC(daxpy,DAXPY)
#define F77_DSYR   F77_FUNC(dsyr,DSYR)
#define F77_DSYMM  F77_FUNC(dsymm, DSYMM)

extern "C" {
/*
  void F77_DGEEV (const char* jobvl, const char* jobvr,
  const int* n, double* a, const int* lda,
  double* wr, double* wi, double* vl, const int* ldvl,
  double* vr, const int* ldvr,
  double* work, const int* lwork, int* info);
*/
    void F77_DSYEV (const char* jobz, const char* uplo,
		    const int* n, double* a, const int* lda,
		    double* w, 
		    double* work, const int* lwork, int* info);
	
	
    void F77_DGESV (const int* n, const int* nrhs, double* a, 
		    const int* lda,	int* ipiv, double* b, const int* ldb,
		    int* info);

    void F77_DSYSV (const char *uplo, const int *n, const int *nrhs,
		    double *a, const int *lda, int *ipiv, double *b,
		    const int *ldb, double *work, const int *lwork,
		    int *info);
	
    void F77_DPOSV (const char* uplo, const int *n, const int *nrhs,
		    double* a, const int *lda, double* b, const int* ldb,
		    int* info);

    void F77_DPOTRF (const char *uplo, const int *n, double *a,
		     const int *lda, const int *info);

    double F77_DLANGE (const char *norm, const int *m, const int *n,
		       const double *a, const int *lda, double *work);

    /* BLAS routines */

    void F77_DSYMM (const char *side, const char *uplo, const int *m,
		    const int *n, const double *alpha,
		    const double *a, const int *lda,
		    const double *b, const int *ldb,
		    const double *beta, double *c, const int *ldc);
        
    void F77_DGEMV (const char *trans, const int *m, const int *n,
		    const double *alpha, const double *a, const int *lda,
		    const double *x, const int *incx, const double *beta,
		    double *y, const int *incy);
    
    void F77_DGEMM (const char* transa, const char* transb, const int* m,
		    const int* n , const int* k, const double* alpha,
		    const double* A, const int* lda, 
		    const double* B, const int* ldb,
		    const double* beta, double* C, const int* ldc);
 
    void F77_DSYR (const char *uplo, const int *n, const double *alpha,
		   const double *x, const int *incx,
		   double *a, const int *lda);

    void F77_DAXPY (const int *n, const double *alpha,
		    const double *dx, const int *incx,
		    double *dy, const int *incy);

    void F77_DCOPY (const int* n, double const *X, const int* incx, double *Y,
		    const int *incy);
    
    void F77_DSCAL (const int* n, double const *alpha, double *X, 
		    const int *incx);
}

#endif
