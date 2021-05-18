subroutine dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, &
  ldc )

!*****************************************************************************80
!
!! DGEMM computes C = alpha * A * B and related operations.
!
!  Discussion:
!
!    DGEMM performs one of the matrix-matrix operations
!
!     C := alpha * op ( A ) * op ( B ) + beta * C,
!
!    where op ( X ) is one of
!
!      op ( X ) = X   or   op ( X ) = X',
!
!    ALPHA and BETA are scalars, and A, B and C are matrices, with op ( A )
!    an M by K matrix, op ( B ) a K by N matrix and C an N by N matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!    
!  Modified:
!
!    12 February 2014
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra.
!    This version by John Burkardt.
!
!  Parameters:
!
!    Input, character * 1 TRANSA, specifies the form of op( A ) to be used in
!    the matrix multiplication as follows:
!    'N' or 'n', op ( A ) = A.
!    'T' or 't', op ( A ) = A'.
!    'C' or 'c', op ( A ) = A'.
!
!    Input, character * 1 TRANSB, specifies the form of op ( B ) to be used in
!    the matrix multiplication as follows:
!    'N' or 'n', op ( B ) = B.
!    'T' or 't', op ( B ) = B'.
!    'C' or 'c', op ( B ) = B'.
!
!    Input, integer ( kind = 4 ) M, the number of rows of the  matrix op ( A ) 
!    and of the matrix C.  0 <= M.
!
!    Input, integer ( kind = 4 ) N, the number  of columns of the matrix 
!    op ( B ) and the number of columns of the matrix C.  0 <= N.
!
!    Input, integer ( kind = 4 ) K, the number of columns of the matrix 
!    op ( A ) and the number of rows of the matrix op ( B ).  0 <= K.
!
!    Input, real ( kind = 8 ) ALPHA, the scalar multiplier 
!    for op ( A ) * op ( B ).
!
!    Input, real ( kind = 8 ) A(LDA,KA), where:
!    if TRANSA is 'N' or 'n', KA is equal to K, and the leading M by K
!    part of the array contains A;
!    if TRANSA is not 'N' or 'n', then KA is equal to M, and the leading
!    K by M part of the array must contain the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared 
!    in the calling routine.  When TRANSA = 'N' or 'n' then LDA must be at 
!    least max ( 1, M ), otherwise LDA must be at least max ( 1, K ).
!
!    Input, real ( kind = 8 ) B(LDB,KB), where:
!    if TRANSB is 'N' or 'n', kB is N, and the leading K by N 
!    part of the array contains B;
!    if TRANSB is not 'N' or 'n', then KB is equal to K, and the leading
!    n by k  part of the array must contain the matrix B.
!
!    Input, integer ( kind = 4 ) LDB, the first dimension of B as declared in 
!    the calling routine.  When TRANSB = 'N' or 'n' then LDB must be at least 
!    max ( 1, K ), otherwise LDB must be at least max ( 1, N ).
!
!    Input, real ( kind = 8 ) BETA, the scalar multiplier for C.
!
!    Input/output, real ( kind = 8 ) C(LDC,N).
!    On input, the leading M by N part of this array must contain the 
!    matrix C, except when BETA is zero, in which case C need not be set.
!    On output, the array C is overwritten by the M by N matrix
!      alpha * op ( A ) * op ( B ) + beta * C.
!
!    Input, integer ( kind = 4 ) LDC, the first dimension of C as declared 
!    in the calling routine.  max ( 1, M ) <= LDC.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) ldc

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(ldb,*)
  real ( kind = 8 ) beta
  real ( kind = 8 ) c(ldc,*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncola
  integer ( kind = 4 ) nrowa
  integer ( kind = 4 ) nrowb
  logical nota
  logical notb
  real ( kind = 8 ) temp
  character * 1 transa
  character * 1 transb
!
!  Set NOTA and NOTB as true if A and B respectively are not
!  transposed and set NROWA, NCOLA and NROWB as the number of rows
!  and columns of A and the number of rows of B respectively.
!
  nota = ( transa == 'N' .or. transa == 'n' )

  if ( nota ) then
    nrowa = m
    ncola = k
  else
    nrowa = k
    ncola = m
  end if

  notb = ( transb == 'N' .or. transb == 'n' )

  if ( notb ) then
    nrowb = k
  else
    nrowb = n
  end if
!
!  Test the input parameters.
!
  info = 0

  if ( transa /= 'N' .and. transa /= 'n' .and. &
       transa /= 'C' .and. transa /= 'c' .and. &
       transa /= 'T' .and. transa /= 't' ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DGEMM - Fatal error!'
    write ( *, '(a)' ) '  Input TRANSA had illegal value.'
    stop 1
  end if

  if ( transb /= 'N' .and. transb /= 'n' .and. &
       transb /= 'C' .and. transb /= 'c' .and. &
       transb /= 'T' .and. transb /= 't' ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DGEMM - Fatal error!'
    write ( *, '(a)' ) '  Input TRANSB had illegal value.'
    stop 1
  end if

  if ( m < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DGEMM - Fatal error!'
    write ( *, '(a)' ) '  Input M had illegal value.'
    stop 1
  end if

  if ( n < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DGEMM - Fatal error!'
    write ( *, '(a)' ) '  Input N had illegal value.'
    stop 1
  end if

  if ( k < 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DGEMM - Fatal error!'
    write ( *, '(a)' ) '  Input K had illegal value.'
    stop 1
  end if

  if ( lda < max ( 1, nrowa ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DGEMM - Fatal error!'
    write ( *, '(a)' ) '  Input LDA had illegal value.'
    stop 1
  end if

  if ( ldb < max ( 1, nrowb ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DGEMM - Fatal error!'
    write ( *, '(a)' ) '  Input LDB had illegal value.'
    stop 1
  end if

  if ( ldc < max ( 1, m ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'DGEMM - Fatal error!'
    write ( *, '(a)' ) '  Input LDC had illegal value.'
    stop 1
  end if
!
!  Quick return if possible.
!
  if ( m == 0 ) then
    return
  end if

  if ( n == 0 ) then
    return
  end if

  if ( ( alpha == 0.0D+00 .or. k == 0 ) .and. beta == 1.0D+00 ) then
    return
  end if
!
!  And if alpha is zero.
!
  if ( alpha == 0.0D+00 ) then
    if ( beta == 0.0D+00 ) then
      c(1:m,1:n) = 0.0D+00
    else
      c(1:m,1:n) = beta * c(1:m,1:n)
    end if
    return
  end if
!
!  Start the operations.
!
  if ( notb ) then
!
!  Form  C := alpha*A*B + beta*C.
!
    if ( nota ) then

      do j = 1, n

        if ( beta == 0.0D+00 ) then
          c(1:m,j) = 0.0D+00
        else if ( beta /= 1.0D+00 ) then
          c(1:m,j) = beta * c(1:m,j)
        end if

        do l = 1, k
          if ( b(l,j) /= 0.0D+00 ) then
            c(1:m,j) = c(1:m,j) + alpha * b(l,j) * a(1:m,l)
          end if
        end do

      end do
!
!  Form  C := alpha*A'*B + beta*C
!
    else

      do j = 1, n
        do i = 1, m

          temp = dot_product ( a(1:k,i), b(1:k,j) )

          if ( beta == 0.0D+00 ) then
            c(i,j) = alpha * temp
          else
            c(i,j) = alpha * temp + beta * c(i,j)
          end if

        end do
      end do

    end if
!
!  Form  C := alpha*A*B' + beta*C
!
  else

    if ( nota ) then

      do j = 1, n

        if ( beta == 0.0D+00 ) then
          c(1:m,j) = 0.0D+00
        else if ( beta /= 1.0D+00 ) then
          c(1:m,j) = beta * c(1:m,j)
        end if

        do l = 1, k
          if ( b(j,l) /= 0.0D+00 ) then
            c(1:m,j) = c(1:m,j) + alpha * b(j,l) * a(1:m,l)
          end if
        end do

      end do
!
!  Form  C := alpha*A'*B' + beta*C
!
    else

      do j = 1, n
        do i = 1, m

          temp = dot_product ( a(1:k,i), b(j,1:k) )
          if ( beta == 0.0D+00 ) then
            c(i,j) = alpha * temp
          else
            c(i,j) = alpha * temp + beta * c(i,j)
          end if
        end do
      end do

    end if

  end if

  return
end
subroutine dsymm ( side, uplo, m, n, alpha, a, lda, b, ldb, beta, c, ldc )

!*****************************************************************************80
!
!! DSYMM performs C:=alpha*A*B+beta*C, A symmetric, B and C rectangular.
!
!  Discussion:
!
!    DSYMM  performs one of the matrix-matrix operations
!      C := alpha*A*B + beta*C,
!    or
!      C := alpha*B*A + beta*C,
!    where alpha and beta are scalars,  A is a symmetric matrix and  B and
!    C are  m by n matrices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 April 2014
!
!  Author:
!
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character SIDE, specifies whether the symmetric matrix A
!    appears on the left or right in the operation as follows:
!    'L' or 'l'   C := alpha*A*B + beta*C,
!    'R' or 'r'   C := alpha*B*A + beta*C,
!
!    Input, character UPLO, specifies whether the upper or lower triangular 
!    part of A is to be referenced:
!    'U' or 'u': reference the upper triangular part.
!    'L' or 'l': reference the lower triangular part.
!
!    Input, integer ( kind = 4 ), the number of rows of the matrix C.
!    0 <= M.
!
!    Input, integer ( kind = 4 ) N, the number of columns of the matrix C.
!    0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the first scalar multiplier.
!
!    Input, real ( kind = 8 ) A(LDA,KA), where ka is m when 
!    SIDE = 'L' or 'l' and is n otherwise.
!    Before entry with  SIDE = 'L' or 'l', the m by m part of
!    the array A must contain the  symmetric matrix, such that
!    when UPLO = 'U' or 'u', the leading m by m upper triangular
!    part of the array A must contain the upper triangular part
!    of the symmetric matrix and the strictly lower triangular
!    part of A is not referenced, and when UPLO = 'L' or 'l',
!    the leading m by m lower triangular part of the array A
!    must contain the lower triangular part of the symmetric
!    matrix and the strictly upper triangular part of A is not
!    referenced.
!    Before entry with SIDE = 'R' or 'r', the n by n part of
!    the array A must contain the symmetric matrix, such that
!    when UPLO = 'U' or 'u', the leading n by n upper triangular
!    part of the array A must contain the upper triangular part
!    of the symmetric matrix and the strictly lower triangular
!    part of A is not referenced, and when UPLO = 'L' or 'l',
!    the leading n by n lower triangular part of the array A
!    must contain the lower triangular part of the symmetric
!    matrix and the strictly upper triangular part of A is not
!    referenced.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of C as declared
!    in the calling program.  When  SIDE = 'L', max( 1, m ) <= LDA.
!    Otherwise, max(1,n) <= LDA.
!
!    Input, real ( kind = 8 ) B(LDB,N), the leading m by n part contains 
!    the array B.
!
!    Input, integer ( kind = 4) LDB, the first dimension of B as declared
!    in the calling program.  max(1,m) <= LDB.
!
!    Input, real ( kind = 8 ) BETA, the second scalar multiplier.
!    When BETA is zero, C need not be set on input.
!
!    Input/output, real ( kind = 8 ) C(LDC,N).
!    Before entry, the leading m by n  part of the array C must contain the 
!    matrix C, except when beta is zero, when C need not be set on entry.
!    On exit, the array C is overwritten by the m by n updated matrix.
!
!    Input, integer ( kind = 4) LDC, the first dimension of C as declared
!    in the calling program.  max(1,m) <= LDC.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) ldc

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(ldb,*)
  real ( kind = 8 ) beta
  real ( kind = 8 ) c(ldc,*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  logical lsame
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nrowa
  real ( kind = 8 ), parameter :: one = 1.0D+00
  character side
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  character uplo
  logical upper
  real ( kind = 8 ), parameter :: zero = 0.0D+00
!
!  Set NROWA as the number of rows of A.
!
  if ( lsame ( side, 'L' ) ) then
    nrowa = m
  else
    nrowa = n
  end if

  upper = lsame ( uplo, 'U' )
!
!  Test the input parameters.
!
  info = 0
  if ( ( .not. lsame ( side, 'L' ) ) .and. &
       ( .not. lsame ( side, 'R' ) ) ) then
    info = 1
  else if ( ( .not. upper ) .and. &
            ( .not. lsame ( uplo, 'L' ) ) ) then
    info = 2
  else if ( m < 0 ) then
    info = 3
  else if ( n < 0 ) then
    info = 4
  else if ( lda < max ( 1, nrowa ) ) then
    info = 7
  else if ( ldb < max ( 1, m ) ) then
    info = 9
  else if ( ldc < max ( 1, m ) ) then
    info = 12
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSYMM', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( m == 0 ) then
    return
  else if ( n == 0 ) then
    return
  else if ( alpha == zero .and. beta == one ) then
    return
  end if
!
!  And when alpha is zero.
!
  if ( alpha == zero ) then
    if ( beta == zero ) then
      c(1:m,1:n) = zero
    else
      c(1:m,1:n) = beta * c(1:m,1:n)
    end if
    return
  end if
!
!  Start the operations.
!
  if ( lsame ( side, 'L' ) ) then
!
!  Form  C := alpha*A*B + beta*C.
!
    if ( upper ) then
      do j = 1, n
        do i = 1, m
          temp1 = alpha * b(i,j)
          temp2 = zero
          do k = 1, i - 1
            c(k,j) = c(k,j) + temp1 * a(k,i)
            temp2 = temp2 + b(k,j) * a(k,i)
          end do
          if ( beta == zero ) then
            c(i,j) = temp1 * a(i,i) + alpha * temp2
          else
            c(i,j) = beta * c(i,j) + temp1 * a(i,i) + alpha * temp2
          end if
        end do
      end do
    else
      do j = 1, n
        do i = m, 1, -1
          temp1 = alpha * b(i,j)
          temp2 = zero
          do k = i + 1, m
            c(k,j) = c(k,j) + temp1 * a(k,i)
            temp2 = temp2 + b(k,j) * a(k,i)
          end do
          if ( beta == zero ) then
            c(i,j) = temp1 * a(i,i) + alpha * temp2
          else
            c(i,j) = beta * c(i,j) + temp1 * a(i,i) + alpha * temp2
          end if
        end do
      end do
    end if
  else
!
!  Form  C := alpha*B*A + beta*C.
!
    do j = 1, n

      temp1 = alpha * a(j,j)
      if ( beta == zero ) then
        c(1:m,j) = temp1 * b(1:m,j)
      else
        do i = 1, m
          c(i,j) = beta * c(i,j) + temp1 * b(i,j)
        end do
      end if

      do k = 1, j - 1
        if ( upper ) then
          temp1 = alpha * a(k,j)
        else
          temp1 = alpha * a(j,k)
        end if
        do i = 1, m
          c(i,j) = c(i,j) + temp1 * b(i,k)
        end do
      end do

      do k = j + 1, n
        if ( upper ) then
          temp1 = alpha * a(j,k)
        else
          temp1 = alpha * a(k,j)
        end if
        do i = 1, m
          c(i,j) = c(i,j) + temp1 * b(i,k)
        end do
      end do

    end do

  end if

  return
end
subroutine dsyr2k ( uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, ldc )

!*****************************************************************************80
!
!! DSYR2K performs C:=alpha*A*B'+alpha*B*A'+beta*C, C symmetric.
!
!  Discussion:
!
!    DSYR2K performs one of the symmetric rank 2k operations
!      C := alpha*A*B' + alpha*B*A' + beta*C,
!    or
!      C := alpha*A'*B + alpha*B'*A + beta*C,
!    where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
!    and  A and B  are  n by k  matrices  in the  first  case  and  k by n
!    matrices in the second case.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 April 2014
!
!  Author:
!
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the upper or lower triangular 
!    part of the array C is to be referenced:
!    'U' or 'u'   Only the  upper triangular part of  C is to be referenced.
!    'L' or 'l'   Only the  lower triangular part of  C is to be referenced.
!
!    Input, character TRANS, specifies the operation to be performed:
!    'N' or 'n'   C := alpha*A*B' + alpha*B*A' + beta*C.
!    'T' or 't'   C := alpha*A'*B + alpha*B'*A + beta*C.
!    'C' or 'c'   C := alpha*A'*B + alpha*B'*A + beta*C.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.  0 <= N.
!
!    Input, integer ( kind = 4 ) K.
!    On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!    of  columns  of the  matrices  A and B,  and on  entry  with
!    TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
!    of rows of the matrices  A and B.  K must be at least  zero.
!
!    Input, real ( kind = 8 ) ALPHA, the first scalar multiplier.
!
!    Input, real ( kind = 8 ) A(LDA,KA), where ka is k when TRANS = 'N' or 'n', 
!    and is n otherwise.  Before entry with  TRANS = 'N' or 'n',  the  
!    leading n by k part of the array  A  must contain the matrix  A,  
!    otherwise the leading k by n part of the array A must contain  
!    the matrix A.
!
!    Input, integer ( kind = 4) LDA, the first dimension of A as declared
!    in the calling program.  When TRANS = 'N', max(1,n) <= LDA.
!    Otherwise max(1,k) <= LDA.
!
!    Input, real ( kind = 8 ) B(LDB,KB), where kb is k when TRANS = 'N' or 'n', 
!    and is  n  otherwise.  Before entry with  TRANS = 'N' or 'n',  the  
!    leading  n by k part of the array  B  must contain the matrix  B,  
!    otherwise the leading  k by n  part of the array  B  must contain the
!    matrix B.
!
!    Input, integer ( kind = 4) LDB, the first dimension of B as declared
!    in the calling program.  When  TRANS = 'N', max(1,n) <= LDB.
!    Otherwise, max(1,k) <= LDB.
!
!    Input, real ( kind = 8 ) BETA, the second scalar multiplier.
!
!    Input/output, real ( kind = 8 ) C(LDC,N).
!    Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!    upper triangular part of the array C must contain the upper
!    triangular part  of the  symmetric matrix  and the strictly
!    lower triangular part of C is not referenced.  On exit, the
!    upper triangular part of the array  C is overwritten by the
!    upper triangular part of the updated matrix.
!    Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!    lower triangular part of the array C must contain the lower
!    triangular part  of the  symmetric matrix  and the strictly
!    upper triangular part of C is not referenced.  On exit, the
!    lower triangular part of the array  C is overwritten by the
!    lower triangular part of the updated matrix.
!
!    Input, integer ( kind = 4) LDC, the first dimension of C as declared
!    in the calling program.  max(1,n) <= LDC.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb
  integer ( kind = 4 ) ldc

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(ldb,*)
  real ( kind = 8 ) beta
  real ( kind = 8 ) c(ldc,*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  logical lsame
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nrowa
  real ( kind = 8 ), parameter :: one = 1.0D+00
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  character trans
  character uplo
  logical upper
  real ( kind = 8 ), parameter :: zero = 0.0D+00
!
!  Test the input parameters.
!
  if ( lsame ( trans, 'N' ) ) then
    nrowa = n
  else
    nrowa = k
  end if

  upper = lsame ( uplo, 'U' )

  info = 0
  if ( ( .not. upper ) .and. &
       ( .not. lsame ( uplo , 'L' ) ) ) then
    info = 1
  else if ( ( .not. lsame ( trans, 'N' ) ) .and. &
              ( .not. lsame ( trans, 'T' ) ) .and. &
              ( .not. lsame ( trans, 'C' ) ) ) then
    info = 2
  else if ( n < 0 ) then
    info = 3
  else if ( k < 0 ) then
    info = 4
  else if ( lda < max ( 1, nrowa ) ) then
    info = 7
  else if ( ldb < max ( 1, nrowa ) ) then
    info = 9
  else if ( ldc < max ( 1, n ) ) then
    info = 12
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSYR2K', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  else if ( ( alpha == zero .or. k == 0 ) .and. beta == one ) then
    return
  end if
!
!  And when alpha is zero.
!
  if ( alpha == zero ) then
    if ( upper ) then
      if ( beta == zero ) then
        do j = 1, n
          c(1:j,j) = zero
        end do
      else
        do j = 1, n
          c(1:j,j) = beta * c(1:j,j)
        end do
      end if
    else
      if ( beta == zero ) then
        do j = 1, n
          c(j:n,j) = zero
        end do
      else
        do j = 1, n
          c(j:n,j) = beta * c(j:n,j)
        end do
      end if
    end if
    return
  end if
!
!  Start the operations.
!
  if ( lsame ( trans, 'N' ) ) then
!
!  Form  C := alpha*A*B' + alpha*B*A' + C.
!
    if ( upper ) then

      do j = 1, n

        if ( beta == zero ) then
          c(1:j,j) = zero
        else if ( beta /= one ) then
          c(1:j,j) = beta * c(1:j,j)
        end if

        do l = 1, k
          if ( ( a(j,l) /= zero ) .or. ( b(j,l) /= zero ) ) then
            temp1 = alpha * b(j,l)
            temp2 = alpha * a(j,l)
            do i = 1, j
              c(i,j) = c(i,j) + a(i,l) * temp1 + b(i,l) * temp2
            end do
          end if
        end do

      end do

    else

      do j = 1, n

        if ( beta == zero ) then
          do i = j, n
            c(i,j) = zero
          end do
        else if ( beta /= one ) then
          do i = j, n
            c(i,j) = beta * c(i,j)
          end do
        end if

        do l = 1, k

          if ( ( a(j,l) /= zero ) .or. ( b(j,l) /= zero ) ) then
            temp1 = alpha * b(j,l)
            temp2 = alpha * a(j,l)
            do i = j, n
              c(i,j) = c(i,j) + a(i,l) * temp1 + b(i,l) * temp2
            end do
          end if

        end do

      end do

    end if

  else
!
!  Form  C := alpha*A'*B + alpha*B'*A + C.
!
    if ( upper ) then

      do j = 1, n

        do i = 1, j

          temp1 = zero
          temp2 = zero
          do l = 1, k
            temp1 = temp1 + a(l,i) * b(l,j)
            temp2 = temp2 + b(l,i) * a(l,j)
          end do

          if ( beta == zero ) then
            c(i,j) = alpha * temp1 + alpha * temp2
          else
            c(i,j) = beta * c(i,j) + alpha * temp1 + alpha * temp2
          end if

        end do

      end do

    else

      do j = 1, n

        do i = j, n

          temp1 = zero
          temp2 = zero
          do l = 1, k
            temp1 = temp1 + a(l,i) * b(l,j)
            temp2 = temp2 + b(l,i) * a(l,j)
          end do

          if ( beta == zero ) then
            c(i,j) = alpha * temp1 + alpha * temp2
          else
            c(i,j) = beta * C(i,j) + alpha * temp1 + alpha * temp2
          end if

        end do

      end do

    end if

  end if

  return
end
subroutine dsyrk ( uplo, trans, n, k, alpha, a, lda, beta, c, ldc )

!*****************************************************************************80
!
!! DSYRK performs C:=alpha*A*TRANSPOSE(A)+beta*C, C symmetric.
!
!  Discussion:
!
!    DSYRK performs one of the symmetric rank k operations
!      C := alpha*A*A' + beta*C,
!    or
!      C := alpha*A'*A + beta*C,
!    where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
!    and  A  is an  n by k  matrix in the first case and a  k by n  matrix
!    in the second case.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 April 2014
!
!  Author:
!
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the upper or lower triangular 
!    part of the array C is to be referenced:
!    'U' or 'u'  the upper triangular part of C is to be referenced.
!    'L' or 'l'  the lower triangular part of C is to be referenced.
!
!    Input, character TRANS, specifies the operation to be performed:
!    'N' or 'n'   C := alpha*A*A' + beta*C.
!    'T' or 't'   C := alpha*A'*A + beta*C.
!    'C' or 'c'   C := alpha*A'*A + beta*C.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.  0 <= N.
!
!    Input, integer ( kind = 4 ) K.
!    On entry with  TRANS = 'N' or 'n', K specifies the number of columns
!    of the matrix A, and on entry with TRANS = 'T' or 't' or 'C' or 'c',  
!    K specifies the number of rows of the matrix  A.  0 <= K.
!
!    Input, real ( kind = 8 ) ALPHA, the first scalar multiplier.
!
!    Input, real ( kind = 8 ) A(LDA,KA), where ka is k when TRANS = 'N' or 'n',
!    and is n otherwise.  Before entry with  TRANS = 'N' or 'n',  the  
!    leading n by k part of the array A must contain the matrix A,  
!    otherwise the leading k by n part of the array A must contain  
!    the matrix A.
!
!    Input, integer ( kind = 4) LDA, the first dimension of A as declared
!    in the calling program.  When TRANS = 'N', max(1,n) <= LDA;
!    otherwise max(1,K) <= LDA.
!
!    Input, real ( kind = 8 ) BETA, the second scalar multiplier.
!
!    Input/output, real ( kind = 8 ) C(LDC,N).
!    Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!    upper triangular part of the array C must contain the upper
!    triangular part  of the  symmetric matrix  and the strictly
!    lower triangular part of C is not referenced.  On exit, the
!    upper triangular part of the array  C is overwritten by the
!    upper triangular part of the updated matrix.
!    Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!    lower triangular part of the array C must contain the lower
!    triangular part  of the  symmetric matrix  and the strictly
!    upper triangular part of C is not referenced.  On exit, the
!    lower triangular part of the array  C is overwritten by the
!    lower triangular part of the updated matrix.
!
!    Input, integer ( kind = 4) LDC, the first dimension of C as declared
!    in the calling program.  max(1,n) <= LDC.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldc

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  real ( kind = 8 ) c(ldc,*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  logical lsame
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nrowa
  real ( kind = 8 ), parameter :: one = 1.0D+00
  real ( kind = 8 ) temp
  character trans
  character uplo
  logical upper
  real ( kind = 8 ), parameter :: zero = 0.0D+00
!
!  Test the input parameters.
!
  if ( lsame ( trans, 'N' ) ) then
    nrowa = n
  else
    nrowa = k
  end if

  upper = lsame ( uplo, 'U' )

  info = 0
  if ( ( .not. upper ) .and. &
             ( .not. lsame ( uplo, 'L' ) ) ) then
    info = 1
  else if ( ( .not. lsame ( trans, 'N' ) ) .and. &
            ( .not. lsame ( trans, 'T' ) ) .and. &
            ( .not. lsame ( trans, 'C' ) ) ) then
    info = 2
  else if ( n < 0 ) then
    info = 3
  else if ( k < 0 ) then
    info = 4
  else if ( lda < max ( 1, nrowa ) ) then
    info = 7
  else if ( ldc < max ( 1, n ) ) then
    info = 10
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSYRK', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  end if

  if ( ( alpha == zero .or. k == 0 ) .and. beta == one ) then
    return
  end if
!
!  And when alpha is zero.
!
  if ( alpha == zero ) then
    if ( upper ) then
      if ( beta == zero ) then
        do j = 1, n
          c(1:j,1:n) = zero
        end do
      else
        do j = 1, n
          c(1:j,j) = beta * c(1:j,j)
        end do
      end if
    else
      if ( beta == zero ) then
        do j = 1, n
          c(j:n,j) = zero
        end do
      else
        do j = 1, n
          c(j:n,j) = beta * c(j:n,j)
        end do
      end if
    end if
    return
  end if
!
!  Start the operations.
!
  if ( lsame ( trans, 'N' ) ) then
!
!  Form  C := alpha*A*A' + beta*C.
!
    if ( upper ) then

      do j = 1, n

        if ( beta == zero ) then
          c(1:j,j) = zero
        else if ( beta /= one ) then
          c(1:j,j) = beta * c(1:j,j)
        end if

        do l = 1, k

          if ( a(j,l) /= zero ) then
            temp = alpha * a(j,l)
            do i = 1, j
              c(i,j) = c(i,j) + temp * a(i,l)
            end do
          end if
        end do
      end do

    else

      do j = 1, n

        if ( beta == zero ) then
          c(j:n,j) = zero
        else if ( beta /= one ) then
          c(j:n,j) = beta * c(j:n,j)
        end if

        do l = 1, k
          if ( a(j,l) /= zero ) then
            temp = alpha * a(j,l)
            c(j:n,j) = c(j:n,j) + temp * a(j:n,l)
          end if
        end do

      end do

    end if

  else
!
!  Form  C := alpha*A'*A + beta*C.
!
    if ( upper ) then

      do j = 1, n

        do i = 1, j
          temp = zero
          do l = 1, k
            temp = temp + a(l,i) * a(l,j)
          end do
          if ( beta == zero ) then
            c(i,j) = alpha * temp
          else
            c(i,j) = alpha * temp + beta * c(i,j)
          end if
        end do

      end do

    else

      do j = 1, n
        do i = j, n
          temp = zero
          do l = 1, k
            temp = temp + a(l,i) * a(l,j)
          end do
          if ( beta == zero ) then
            c(i,j) = alpha * temp
          else
            c(i,j) = alpha * temp + beta * c(i,j)
          end if
        end do
      end do

    end if
  end if

  return
end
subroutine dtrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

!*****************************************************************************80
!
!! DTRMM performs B:=A*B or B:=B*A, A triangular, B rectangular.
!
!  Discussion:
!
!    This routine performs one of the matrix-matrix operations
!      B := alpha*op( A )*B,
!    or
!      B := alpha*B*op( A ),
!    where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
!    non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!      op( A ) = A   or   op( A ) = A'.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2005
!
!  Author:
!
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character SIDE, specifies whether op(A) multiplies B from
!    the left or right as follows:
!    'L' or 'l': B := alpha*op( A )*B.
!    'R' or 'r': B := alpha*B*op( A ).
!
!    Input, character UPLO, specifies whether the matrix A is an upper or
!    lower triangular matrix as follows:
!    'U' or 'u': A is an upper triangular matrix.
!    'L' or 'l': A is a lower triangular matrix.
!
!    Input, character TRANS, specifies the form of op( A ) to be used in
!    the matrix multiplication as follows:
!    'N' or 'n': op( A ) = A.
!    'T' or 't': op( A ) = A'.
!    'C' or 'c': op( A ) = A'.
!
!    Input, character DIAG, specifies whether or not A is unit triangular
!    as follows:
!    'U' or 'u': A is assumed to be unit triangular.
!    'N' or 'n': A is not assumed to be unit triangular.
!
!    Input, integer ( kind = 4 ) M, the number of rows of B.  0 <= M.
!
!    Input, integer ( kind = 4 ) N, the number of columns of B.  
!    0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the scalar  alpha.  When alpha is
!    zero, A is not referenced and B need not be set before entry.
!
!    Input, real ( kind = 8 ) A(LDA,K), where k is m when  SIDE = 'L' or 'l'  
!    and is  n  when  SIDE = 'R' or 'r'.
!    Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!    upper triangular part of the array  A must contain the upper
!    triangular matrix  and the strictly lower triangular part of
!    A is not referenced.
!    Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!    lower triangular part of the array  A must contain the lower
!    triangular matrix  and the strictly upper triangular part of
!    A is not referenced.
!    Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!    A  are not referenced either,  but are assumed to be  unity.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program.  When SIDE = 'L' or 'l' then LDA must be at 
!    least max ( 1, M ); when SIDE = 'R' or 'r', LDA must be at least 
!    max ( 1, N ).
!
!    Input/output, real ( kind = 8 ) B(LDB,N).
!    Before entry, the leading m by n part of the array  B must contain 
!    the matrix  B, and on exit is overwritten by the transformed matrix.
!
!    Input, integer ( kind = 4 ) LDB, the first dimension of B as declared
!    in  the  calling program.   max ( 1, M ) <= LDB.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) ldb

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(ldb,*)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  logical lsame
  logical lside
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  logical nounit
  integer ( kind = 4 ) nrowa
  real ( kind = 8 ), parameter :: one = 1.0D+00
  character side
  real ( kind = 8 ) temp
  character transa
  character uplo
  logical upper
  real ( kind = 8 ), parameter :: zero = 0.0D+00
!
!  Test the input parameters.
!
  lside  = lsame ( side, 'L' )

  if ( lside ) then
    nrowa = m
  else
    nrowa = n
  end if

  nounit = lsame ( diag, 'N' )
  upper = lsame ( uplo, 'U' )

  info = 0
  if ( ( .not. lside ) .and. &
       ( .not. lsame ( side, 'R' ) ) ) then
    info = 1
  else if ( ( .not. upper ) .and. &
              ( .not. lsame ( uplo, 'L' ) ) ) then
    info = 2
  else if ( ( .not. lsame ( transa, 'N' ) ) .and. &
              ( .not. lsame ( transa, 'T' ) ) .and. &
             ( .not. lsame ( transa, 'C' ) ) ) then
    info = 3
  else if ( ( .not. lsame ( diag, 'U' ) ) .and. &
              ( .not. lsame ( diag, 'N' ) ) ) then
    info = 4
  else if ( m < 0 ) then
    info = 5
  else if ( n < 0 ) then
    info = 6
  else if ( lda < max ( 1, nrowa ) ) then
    info = 9
  else if ( ldb < max ( 1, m ) ) then
    info = 11
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DTRMM', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  end if
!
!  And when alpha is zero.
!
  if ( alpha == zero ) then
    b(1:m,1:n) = zero
    return
  end if
!
!  Start the operations.
!
  if ( lside ) then
!
!  Form  B := alpha*A*B.
!
    if ( lsame ( transa, 'N' ) ) then

      if ( upper ) then
        do j = 1, n
          do k = 1, m
            if ( b(k,j) /= zero ) then
              temp = alpha * b(k,j)
              do i = 1, k - 1
                b(i,j) = b(i,j) + temp * a(i,k)
              end do
              if ( nounit ) then
                temp = temp * a(k,k)
              end if
              b(k,j) = temp
            end if
          end do
        end do
      else
        do j = 1, n
          do k = m, 1, -1
            if ( b(k,j) /= zero ) then
              temp = alpha * b(k,j)
              b(k,j) = temp
              if ( nounit ) then
                b(k,j) = b(k,j) * a(k,k)
              end if
              do i = k + 1, m
                b(i,j) = b(i,j) + temp * a(i,k)
              end do
            end if
          end do
        end do
      end if
!
!  Form  B := alpha*A'*B.
!
    else

      if ( upper ) then
        do j = 1, n
          do i = m, 1, -1
            temp = b(i,j)
            if ( nounit ) then
              temp = temp * a(i,i)
            end if
            do k = 1, i - 1
              temp = temp + a(k,i) * b(k,j)
            end do
            b(i,j) = alpha * temp
          end do
        end do
      else
        do j = 1, n
          do i = 1, m
            temp = b(i,j)
            if ( nounit ) then
              temp = temp * a(i,i)
            end if
            do k = i + 1, m
              temp = temp + a(k,i) * b(k,j)
            end do
            b(i,j) = alpha * temp
          end do
        end do
      end if
    end if
!
!  Form  B := alpha*B*A.
!
  else

    if ( lsame ( transa, 'N' ) ) then

      if ( upper ) then

        do j = n, 1, -1
          temp = alpha
          if ( nounit ) then
            temp = temp * a(j,j)
          end if
          do i = 1, m
            b(i,j) = temp * b(i,j)
          end do
          do k = 1, j - 1
            if ( a(k,j) /= zero ) then
              temp = alpha * a(k,j)
              do i = 1, m
                b(i,j) = b(i,j) + temp * b(i,k)
              end do
            end if
          end do
        end do

      else

        do j = 1, n
          temp = alpha
          if ( nounit ) then
            temp = temp * a(j,j)
          end if
          do i = 1, m
            b(i,j) = temp * b(i,j)
          end do
          do k = j + 1, n
            if ( a(k,j) /= zero ) then
              temp = alpha * a(k,j)
              do i = 1, m
                b(i,j) = b(i,j) + temp * b(i,k)
              end do
            end if
          end do
        end do

      end if
!
!  Form  B := alpha*B*A'.
!
    else

      if ( upper ) then
 
        do k = 1, n
          do j = 1, k - 1
            if ( a(j,k) /= zero ) then
              temp = alpha * a(j,k)
              do i = 1, m
                b(i,j) = b(i,j) + temp * b(i,k)
              end do
            end if
          end do
          temp = alpha
          if ( nounit ) then
            temp = temp * a(k,k)
          end if
          if ( temp /= one ) then
            do i = 1, m
              b(i,k) = temp * b(i,k)
            end do
          end if
        end do

      else

        do k = n, 1, -1
          do j = k + 1, n
            if ( a(j,k) /= zero ) then
              temp = alpha * a(j,k)
              do i = 1, m
                b(i,j) = b(i,j) + temp * b(i,k)
              end do
            end if
          end do
          temp = alpha
          if ( nounit ) then
            temp = temp * a(k,k)
          end if
          if ( temp /= one ) then
            do i = 1, m
              b(i,k) = temp * b(i,k)
            end do
          end if
        end do

      end if

    end if

  end if

  return
end
subroutine dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )

!*****************************************************************************80
!
!! DTRSM solves A*X=alpha*B or X*A=alpha*B, for triangular A, rectangular B.
!
!  Discussion:
!
!    DTRSM solves one of the matrix equations
!      op( A )*X = alpha*B,   
!    or
!      X*op( A ) = alpha*B,
!    where alpha is a scalar, X and B are m by n matrices, A is a unit, or
!    non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!      op( A ) = A   or   op( A ) = A'.
!    The matrix X is overwritten on B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 June 2005
!
!  Author:
!
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character SIDE, specifies whether op( A ) appears on the left
!    or right of X as follows:
!    'L' or 'l': op( A )*X = alpha*B.
!    'R' or 'r': X*op( A ) = alpha*B.
!
!    Input, character UPLO, specifies whether the matrix A is an upper or
!    lower triangular matrix as follows:
!    'U' or 'u': A is an upper triangular matrix.
!    'L' or 'l': A is a lower triangular matrix.
!
!    Input, character TRANSA, specifies the form of op( A ) to be used in
!    the matrix multiplication as follows:
!    'N' or 'n': op( A ) = A.
!    'T' or 't': op( A ) = A'.
!    'C' or 'c': op( A ) = A'.
!
!    Input, character DIAG, specifies whether or not A is unit triangular
!    as follows:
!    'U' or 'u': A is assumed to be unit triangular.
!    'N' or 'n': A is not assumed to be unit triangular.
!
!    Input, integer ( kind = 4 ) M, the number of rows of B.  0 <= M.
!
!    Input, integer ( kind = 4 ) N, the number of columns of B.  0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the scalar alpha.  When alpha is
!    zero then A is not referenced and B need not be set before entry.
!
!    Input, real ( kind = 8 ) A(LDA,K) where K is M when SIDE = 'L' or 'l'  
!    and K is N when SIDE = 'R' or 'r'.
!    Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!    upper triangular part of the array  A must contain the upper
!    triangular matrix  and the strictly lower triangular part of
!    A is not referenced.
!    Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!    lower triangular part of the array  A must contain the lower
!    triangular matrix  and the strictly upper triangular part of
!    A is not referenced.
!    Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!    A  are not referenced either,  but are assumed to be  unity.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!    LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!    then LDA must be at least max( 1, n ).
!
!    Input/output, real ( kind = 8 ) B(LDB,N).
!    Before entry, the leading m by n part of the array B must
!    contain the right-hand side matrix B, and on exit is
!    overwritten by the solution matrix X.
!
!    Input, integer ( kind = 4 ) LDB, the first dimension of B as declared
!    in the calling program.  LDB must be at least max ( 1, M ).
!
  implicit none

  integer lda
  integer ldb

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(ldb,*)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  logical lsame
  logical lside
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  logical nounit
  integer ( kind = 4 ) nrowa
  real ( kind = 8 ), parameter :: one = 1.0D+00
  character side
  real ( kind = 8 ) temp
  character transa
  character uplo
  logical upper
  real ( kind = 8 ), parameter :: zero = 0.0D+00
!
!  Test the input parameters.
!
  lside  = lsame ( side, 'L' )

  if ( lside ) then
    nrowa = m
  else
    nrowa = n
  end if

  nounit = lsame ( diag, 'N' )
  upper = lsame ( uplo, 'U' )

  info = 0

  if (      ( .not. lside ) .and. &
            ( .not. lsame ( side, 'R' ) ) ) then
    info = 1
  else if ( ( .not. upper ) .and. &
            ( .not. lsame ( uplo, 'L' ) ) ) then
    info = 2
  else if ( ( .not. lsame ( transa, 'N' ) ) .and. &
            ( .not. lsame ( transa, 'T' ) ) .and. &
            ( .not. lsame ( transa, 'C' ) ) ) then
    info = 3
  else if ( ( .not. lsame ( diag, 'U' ) ) .and. &
            ( .not. lsame ( diag, 'N' ) ) ) then
    info = 4
  else if ( m < 0 ) then
    info = 5
  else if ( n < 0 ) then
    info = 6
  else if ( lda < max ( 1, nrowa ) ) then
    info = 9
  else if ( ldb < max ( 1, m ) ) then
    info = 11
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DTRSM', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  end if
!
!  and when alpha is zero.
!
  if ( alpha == zero ) then
    b(1:m,1:n) = 0.0D+00
    return
  end if
!
!  Start the operations.
!
  if ( lside ) then
!
!  Form  B := alpha*inv( a )*B.
!
    if ( lsame ( transa, 'N' ) ) then

      if ( upper ) then
        do j = 1, n
          if ( alpha /= one ) then
            do i = 1, m
              b(i,j) = alpha * b(i,j)
            end do
          end if
          do k = m, 1, -1
            if ( b(k,j) /= zero ) then
              if ( nounit ) then
                b(k,j) = b(k,j) / a(k,k)
              end if
              do i = 1, k - 1
                b(i,j) = b(i,j) - b(k,j) * a(i,k)
              end do
            end if
          end do
        end do
      else
        do j = 1, n
          if ( alpha /= one ) then
            do i = 1, m
              b(i,j) = alpha * b(i,j)
            end do
          end if
          do k = 1, m
            if ( b(k,j) /= zero ) then
              if ( nounit ) then
                b(k,j) = b(k,j) / a(k,k)
              end if
              do i = k + 1, m
                b(i,j) = b(i,j) - b(k,j) * a(i,k)
              end do
            end if
          end do
        end do
      end if
!
!  Form  B := alpha*inv( A' )*B.
!
    else

      if ( upper ) then
        do j = 1, n
          do i = 1, m
            temp = alpha * b(i,j)
            do k = 1, i - 1
              temp = temp - a(k,i) * b(k,j)
            end do
            if ( nounit ) then
              temp = temp / a(i,i)
            end if
            b(i,j) = temp
          end do
        end do
      else
        do j = 1, n
          do i = m, 1, -1
            temp = alpha * b(i,j)
            do k = i + 1, m
              temp = temp - a(k,i) * b(k,j)
            end do
            if ( nounit ) then
              temp = temp / a(i,i)
            end if
            b(i,j) = temp
          end do
        end do
      end if
    end if
!
!  Form  B := alpha*B*inv( A ).
!
  else

    if ( lsame ( transa, 'N' ) ) then

      if ( upper ) then

        do j = 1, n
          if ( alpha /= one ) then
            do i = 1, m
              b(i,j) = alpha * b(i,j)
            end do
          end if
          do k = 1, j - 1
            if ( a(k,j) /= zero ) then
              do i = 1, m
                b(i,j) = b(i,j) - a(k,j) * b(i,k)
              end do
            end if
          end do
          if ( nounit ) then
            temp = one / a(j,j)
            do i = 1, m
              b(i,j) = temp * b(i,j)
            end do
          end if
        end do

      else

        do j = n, 1, -1
          if ( alpha /= one ) then
            do i = 1, m
              b(i,j) = alpha * b(i,j)
            end do
          end if
          do k = j + 1, n
            if ( a(k,j) /= zero ) then
              do i = 1, m
                b(i,j) = b(i,j) - a(k,j) * b(i,k)
              end do
            end if
          end do
          if ( nounit ) then
            temp = one / a(j,j)
            do i = 1, m
              b(i,j) = temp * b(i,j)
            end do
          end if
        end do
      end if
!
!  Form  B := alpha*B*inv( A' ).
!
    else

      if ( upper ) then
        do k = n, 1, -1
          if ( nounit ) then
            temp = one / a(k,k)
            do i = 1, m
              b(i,k) = temp * b(i,k)
            end do
          end if
          do j = 1, k - 1
            if ( a(j,k) /= zero ) then
              temp = a(j,k)
              do i = 1, m
                b(i,j) = b(i,j) - temp * b(i,k)
              end do
            end if
          end do
          if ( alpha /= one ) then
            do i = 1, m
              b(i,k) = alpha * b(i,k)
            end do
          end if
        end do
      else
        do k = 1, n
          if ( nounit ) then
            temp = one / a(k,k)
            do i = 1, m
              b(i,k) = temp * b(i,k)
            end do
          end if
          do j = k + 1, n
            if ( a(j,k) /= zero ) then
              temp = a(j,k)
              do i = 1, m
                b(i,j) = b(i,j) - temp * b(i,k)
              end do
            end if
          end do
          if ( alpha /= one ) then
            do i = 1, m
              b(i,k) = alpha * b(i,k)
            end do
          end if
        end do
      end if
    end if
  end if

  return
end
