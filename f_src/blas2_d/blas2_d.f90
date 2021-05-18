subroutine dgbmv ( trans, m, n, kl, ku, alpha, a, lda, x, incx, beta, y, incy )

!*****************************************************************************80
!
!! DGBMV computes y: = alpha*A*x + beta*y for banded matrix A.
!
!  Discussion:
!
!    DGBMV performs one of the matrix-vector operations
!
!      y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
!
!    where alpha and beta are scalars, x and y are vectors and A is an
!    m by n band matrix, with kl sub-diagonals and ku super-diagonals.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character TRANS, the operation to be performed:
!    'n' or 'N'   y := alpha*A*x + beta*y.
!    't' or 'T'   y := alpha*A'*x + beta*y.
!    'c' or 'C'   y := alpha*A'*x + beta*y.
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrix A.
!    0 <= M.
!
!    Input, integer ( kind = 4 ) N, the number of columns of the matrix A.
!    0 <= N.
!
!    Input, integer ( kind = 4 ) KL, the number of subdiagonals.
!    0 <= KL.
!
!    Input, integer ( kind = 4 ) KU, the number of superdiagonals.
!    0 <= KU.
!
!    Input, real ( kind = 8 ) ALPHA, the first scalar multiplier.
!
!    Input, real ( kind = 8 ) A(LDA,N).
!    Before entry, the leading ( kl + ku + 1 ) by n part of the
!    array A must contain the matrix of coefficients, supplied
!    column by column, with the leading diagonal of the matrix in
!    row ( ku + 1 ) of the array, the first super-diagonal
!    starting at position 2 in row ku, the first sub-diagonal
!    starting at position 1 in row ( ku + 2 ), and so on.
!    Elements in the array A that do not correspond to elements
!    in the band matrix (such as the top left ku by ku triangle)
!    are not referenced.
!    The following program segment will transfer a band matrix
!    from conventional full matrix storage to band storage:
!      do J = 1, N
!        K = KU + 1 - J
!        do I = max ( 1, J - KU ), min ( M, J + KL )
!          A( K + I, J ) = matrix( I, J )
!        end do
!      end do

!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling routine.  KL+KU+1 <= LDA.
!
!    Input, real ( kind = 8 ) X(1+(n-1)*abs(INCX)) when TRANS = 'n' or 'N',
!    or real ( kind = 8 ) X((1+(m-1)*abs(INCX)) otherwise.
!    Before entry, the incremented array X must contain the vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
!    Input, real ( kind = 8 ) BETA, the second scalar multiplier.  When BETA is
!    supplied as zero then Y need not be set on input.
!
!    Input/output, real ( kind = 8 ) Y(1+(m-1)*abs(INCY)) if TRANS = 'n' or 'N',
!    real ( kind = 8 ) Y(1+(n-1)*abs(INCY)) otherwise.
!    Before entry, the incremented array Y must contain the
!    vector y.  On exit, Y is overwritten by the updated vector y.
!
!    Input, integer ( kind = 4 ) INCY, the increment for the elements of
!    Y.  INCY must not be zero.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) jy
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kl
  integer ( kind = 4 ) ku
  integer ( kind = 4 ) kup1
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) ky
  integer ( kind = 4 ) lenx
  integer ( kind = 4 ) leny
  logical lsame
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp
  character trans
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( trans, 'N' ) .and.  &
       .not. lsame ( trans, 'T' ) .and.  &
       .not. lsame ( trans, 'C' ) ) then
    info = 1
  else if ( m < 0 ) then
    info = 2
  else if ( n < 0 ) then
    info = 3
  else if ( kl < 0 ) then
    info = 4
  else if ( ku < 0 ) then
    info = 5
  else if ( lda < ( kl + ku + 1 ) ) then
    info = 8
  else if ( incx == 0 ) then
    info = 10
  else if ( incy == 0 ) then
    info = 13
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DGBMV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( m == 0 ) then
    return
  else if ( n == 0 ) then
    return
  else if ( alpha == 0.0D+00 .and. beta == 1.0D+00 ) then
    return
  end if
!
!  Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!  up the start points in  X  and  Y.
!
  if ( lsame ( trans, 'N' ) ) then
    lenx = n
    leny = m
  else
    lenx = m
    leny = n
  end if

  if ( 0 < incx ) then
    kx = 1
  else
    kx = 1 - ( lenx - 1 ) * incx
  end if

  if ( 0 < incy ) then
    ky = 1
  else
    ky = 1 - ( leny - 1 ) * incy
  end if
!
!  Start the operations.  In this version the elements of A are
!  accessed sequentially with one pass through the band part of A.
!
!  First form y := beta*y.
!
  if ( beta /= 1.0D+00 ) then
    if ( incy == 1 ) then
      if ( beta == 0.0D+00 ) then
        y(1:leny) = 0.0D+00
      else
        y(1:leny) = beta * y(1:leny)
      end if
    else
      iy = ky
      if ( beta == 0.0D+00 ) then
        do i = 1, leny
          y(iy) = 0.0D+00
          iy = iy + incy
        end do
      else
        do i = 1, leny
          y(iy) = beta * y(iy)
          iy = iy + incy
        end do
      end if
    end if
  end if

  if ( alpha == 0.0D+00 ) then
    return
  end if

  kup1 = ku + 1
  if ( lsame ( trans, 'N' ) ) then
!
!  Form  y := alpha*A*x + y.
!
    jx = kx
    if ( incy == 1 ) then
      do j = 1, n
        if ( x(jx) /= 0.0D+00 ) then
          temp = alpha * x(jx)
          k = kup1 - j
          do i = max ( 1, j - ku ), min ( m, j + kl )
            y(i) = y(i) + temp * a(k+i,j)
          end do
        end if
        jx = jx + incx
      end do
    else
      do j = 1, n
        if ( x(jx) /= 0.0D+00 ) then
          temp = alpha * x(jx)
          iy = ky
          k = kup1 - j
          do i = max ( 1, j - ku ), min ( m, j + kl )
            y(iy) = y(iy) + temp * a(k+i,j)
            iy = iy + incy
          end do
        end if
        jx = jx + incx
        if ( ku < j ) then
         ky = ky + incy
        end if

      end do
    end if
  else
!
!  Form y := alpha*A'*x + y.
!
    jy = ky
    if ( incx == 1 ) then
      do j = 1, n
        temp = 0.0D+00
        k = kup1 - j
        do i = max ( 1, j - ku ), min ( m, j + kl )
          temp = temp + a(k+i,j) * x(i)
        end do
        y(jy) = y(jy) + alpha * temp
        jy = jy + incy
      end do
    else
      do j = 1, n
        temp = 0.0D+00
        ix = kx
        k = kup1 - j
        do i = max ( 1, j - ku ), min ( m, j + kl )
          temp = temp + a(k+i,j) * x(ix)
          ix = ix + incx
        end do
        y(jy) = y(jy) + alpha * temp
        jy = jy + incy

        if ( ku < j ) then
          kx = kx + incx
        end if

      end do
    end if
  end if

  return
end
subroutine dgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )

!*****************************************************************************80
!
!! DGEMV computes y := alpha * A * x + beta * y for general matrix A.
!
!  Discussion:
!
!    DGEMV performs one of the matrix-vector operations
!      y := alpha*A *x + beta*y
!    or
!      y := alpha*A'*x + beta*y,
!    where alpha and beta are scalars, x and y are vectors and A is an
!    m by n matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 2014
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character TRANS, specifies the operation to be performed:
!    'n' or 'N'   y := alpha*A *x + beta*y.
!    't' or 'T'   y := alpha*A'*x + beta*y.
!    'c' or 'C'   y := alpha*A'*x + beta*y.
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrix A.
!    0 <= M.
!
!    Input, integer ( kind = 4 ) N, the number of columns of the matrix A.
!    0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the scalar multiplier for A * x.
!
!    Input, real ( kind = 8 ) A(LDA,N).  The M x N subarray contains
!    the matrix A.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling routine.  max ( 1, M ) <= LDA.
!
!    Input, real ( kind = 8 ) X(*), an array containing the vector to be 
!    multiplied by the matrix A.  
!    If TRANS = 'N' or 'n', then X must contain N entries, stored in INCX 
!    increments in a space of at least ( 1 + ( N - 1 ) * abs ( INCX ) ) 
!    locations.
!    Otherwise, X must contain M entries, store in INCX increments
!    in a space of at least ( 1 + ( M - 1 ) * abs ( INCX ) ) locations.
!
!    Input, integer ( kind = 4 ) INCX, the increment for the elements of
!    X.  INCX must not be zero.
!
!    Input, real ( kind = 8 ) BETA, the scalar multiplier for Y.
!
!    Input/output, real ( kind = 8 ) Y(*), an array containing the vector to
!    be scaled and incremented by A*X.
!    If TRANS = 'N' or 'n', then Y must contain M entries, stored in INCY
!    increments in a space of at least ( 1 + ( M - 1 ) * abs ( INCY ) ) 
!    locations.
!    Otherwise, Y must contain N entries, store in INCY increments
!    in a space of at least ( 1 + ( N - 1 ) * abs ( INCY ) ) locations.
!
!    Input, integer ( kind = 4 ) INCY, the increment for the elements of
!    Y.  INCY must not be zero.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) jy
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) ky
  integer ( kind = 4 ) lenx
  integer ( kind = 4 ) leny
  logical lsame
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp
  character trans
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( trans, 'N' ) .and.  &
       .not. lsame ( trans, 'T' ) .and.  &
       .not. lsame ( trans, 'C' ) ) then
    info = 1
  else if ( m < 0 ) then
    info = 2
  else if ( n < 0 ) then
    info = 3
  else if ( lda < max ( 1, m ) ) then
    info = 6
  else if ( incx == 0 ) then
    info = 8
  else if ( incy == 0 ) then
    info = 11
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DGEMV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( ( m == 0 ) .or. &
       ( n == 0 ) .or. &
       ( ( alpha == 0.0D+00 ) .and. ( beta == 1.0D+00 ) ) ) then
   return
  end if
!
!  Set LENX and LENY, the lengths of the vectors x and y, and set
!  up the start points in X and Y.
!
  if ( lsame ( trans, 'N' ) ) then
    lenx = n
    leny = m
  else
    lenx = m
    leny = n
  end if

  if ( 0 < incx ) then
    kx = 1
  else
    kx = 1 - ( lenx - 1 ) * incx
  end if

  if ( 0 < incy ) then
    ky = 1
  else
    ky = 1 - ( leny - 1 ) * incy
  end if
!
!  Start the operations. In this version the elements of A are
!  accessed sequentially with one pass through A.
!
!  First form  y := beta*y.
!
  if ( beta /= 1.0D+00 ) then
    if ( incy == 1 ) then
      if ( beta == 0.0D+00 ) then
        y(1:leny) = 0.0D+00
      else
        y(1:leny) = beta * y(1:leny)
      end if
    else
      iy = ky
      if ( beta == 0.0D+00 ) then
        do i = 1, leny
          y(iy) = 0.0D+00
          iy = iy + incy
        end do
      else
        do i = 1, leny
          y(iy) = beta * y(iy)
          iy = iy + incy
        end do
      end if
    end if
  end if

  if ( alpha == 0.0D+00 ) then
    return
  end if
!
!  Form y := alpha*A*x + y.
!
  if ( lsame ( trans, 'N' ) ) then
    jx = kx
    if ( incy == 1 ) then
      do j = 1, n
        if ( x(jx) /= 0.0D+00 ) then
          temp = alpha * x(jx)
          do i = 1, m
            y(i) = y(i) + temp * a(i,j)
          end do
        end if
        jx = jx + incx
      end do
    else
      do j = 1, n
        if ( x(jx) /= 0.0D+00 ) then
          temp = alpha * x(jx)
          iy = ky
          do i = 1, m
            y(iy) = y(iy) + temp * a(i,j)
            iy = iy + incy
          end do
        end if
        jx = jx + incx
      end do
    end if
!
!  Form y := alpha*A'*x + y.
!
  else
    jy = ky
    if ( incx == 1 ) then
      do j = 1, n
        temp = 0.0D+00
        do i = 1, m
          temp = temp + a(i,j) * x(i)
        end do
        y(jy) = y(jy) + alpha * temp
        jy = jy + incy
      end do
    else
      do j = 1, n
        temp = 0.0D+00
        ix = kx
        do i = 1, m
          temp = temp + a(i,j) * x(ix)
          ix = ix + incx
        end do
        y(jy) = y(jy) + alpha * temp
        jy = jy + incy
      end do
    end if
  end if

  return
end
subroutine dger ( m, n, alpha, x, incx, y, incy, a, lda )

!*****************************************************************************80
!
!! DGER computes A := alpha*x*y' + A.
!
!  Discussion:
!
!    DGER performs the rank 1 operation
!
!      A := alpha*x*y' + A,
!
!    where alpha is a scalar, x is an m element vector, y is an n element
!    vector and A is an m by n matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2014
!
!  Author:
!
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrix A.
!    0 <= M.
!
!    Input, integer ( kind = 4 ) N, the number of columns of the matrix A.
!    0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the scalar multiplier.
!
!    Input, real ( kind = 8 ) X(1+(M-1)*abs(INCX)), the first vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
!    Input, real ( kind = 8 ) Y(1+(N-1)*abs(INCY)), the second vector.
!
!    Input, integer ( kind = 4 ) INCY, the increment for elements of Y.
!    INCY must not be zero.
!
!    Input/output, real ( kind = 8 ) A(LDA,N).  On entry, the leading M by N 
!    part of the array contains the matrix of coefficients. On exit, A is
!    overwritten by the updated matrix.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program. max ( 1, M ) <= LDA.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jy
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)
!
!  Test the input parameters.
!
  info = 0
  if ( m < 0 ) then
    info = 1
  else if ( n < 0 ) then
    info = 2
  else if ( incx == 0 ) then
    info = 5
  else if ( incy == 0 ) then
    info = 7
  else if ( lda < max ( 1, m ) ) then
    info = 9
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DGER', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( ( m == 0 ) .or. ( n == 0 ) .or. ( alpha == 0.0D+00 ) ) then
    return
  end if
!
!  Start the operations. In this version the elements of A are
!  accessed sequentially with one pass through A.
!
  if ( 0 < incy ) then
    jy = 1
  else
    jy = 1 - ( n - 1 ) * incy
  end if

  if ( incx == 1 ) then
    do j = 1, n
      if ( y(jy) /= 0.0D+00 ) then
        temp = alpha * y(jy)
        a(1:m,j) = a(1:m,j) + x(1:m) * temp
      end if
      jy = jy + incy
    end do
  else
    if ( 0 < incx ) then
      kx = 1
    else
      kx = 1 - ( m - 1 ) * incx
    end if
    do j = 1, n
      if ( y(jy) /= 0.0D+00 ) then
        temp = alpha * y(jy)
        ix = kx
        do i = 1, m
          a(i,j) = a(i,j) + x(ix) * temp
          ix = ix + incx
        end do
      end if
      jy = jy + incy
    end do
  end if

  return
end
subroutine dsbmv ( uplo, n, k, alpha, a, lda, x, incx, beta, y, incy )

!*****************************************************************************80
!
!! DSBMV computes y := alpha*A*x + beta*y for symmetric banded matrix A.
!
!  Discussion:
!
!    DSBMV performs the matrix-vector  operation
!
!      y := alpha*A*x + beta*y,
!
!    where alpha and beta are scalars, x and y are n element vectors and
!    A is an n by n symmetric band matrix, with k super-diagonals.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the upper or lower
!    triangular part of the band matrix A is being supplied:
!    'u' or 'U': the upper triangular part of A is being supplied.
!    'l' or 'L': the lower triangular part of A is being supplied.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, integer ( kind = 4 ) K, the number of super-diagonals of the
!    matrix A.  0 <= K.
!
!    Input, real ( kind = 8 ) ALPHA, the first scalar multiplier.
!
!    Input, real ( kind = 8 ) A(LDA,N).
!    Before entry with UPLO = 'U' or 'U', the leading ( k + 1 )
!    by n part of the array A must contain the upper triangular
!    band part of the symmetric matrix, supplied column by
!    column, with the leading diagonal of the matrix in row
!    ( k + 1 ) of the array, the first super-diagonal starting at
!    position 2 in row k, and so on. The top left k by k triangle
!    of the array A is not referenced.
!    The following program segment will transfer the upper
!    triangular part of a symmetric band matrix from conventional
!    full matrix storage to band storage:
!      do J = 1, N
!        M = K + 1 - J
!        do I = max ( 1, J - K ), J
!          A( M + I, J ) = matrix( I, J )
!        end do
!      end do
!    Before entry with UPLO = 'l' or 'L', the leading ( k + 1 )
!    by n part of the array A must contain the lower triangular
!    band part of the symmetric matrix, supplied column by
!    column, with the leading diagonal of the matrix in row 1 of
!    the array, the first sub-diagonal starting at position 1 in
!    row 2, and so on. The bottom right k by k triangle of the
!    array A is not referenced.
!    The following program segment will transfer the lower
!    triangular part of a symmetric band matrix from conventional
!    full matrix storage to band storage:
!      do J = 1, N
!        M = 1 - J
!        do I = J, min ( N, J + K )
!          A( M + I, J ) = matrix( I, J )
!        end do
!      end do
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program. K + 1 <= LDA.
!
!    Input, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
!    Input, real ( kind = 8 ) BETA, the second scalar multiplier.
!
!    Input/output, real ( kind = 8 ) Y(1+(n-1)*abs(INCY)).
!    Before entry, the incremented array Y must contain the
!    vector y.  On exit, Y is overwritten by the updated vector y.
!
!    Input, integer ( kind = 4 ) INCY, the increment for elements of Y.
!    INCY must not be zero.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) jy
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kplus1
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) ky
  integer ( kind = 4 ) l
  logical lsame
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  character uplo
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not.  lsame ( uplo, 'U' ) .and.  &
       .not.  lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( n < 0 ) then
    info = 2
  else if ( k < 0 ) then
    info = 3
  else if ( lda < ( k + 1 ) ) then
    info = 6
  else if ( incx == 0 ) then
    info = 8
  else if ( incy == 0 ) then
    info = 11
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSBMV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( ( n == 0 ) .or. ( ( alpha == 0.0D+00 ) .and. ( beta == 1.0D+00 ) ) ) then
    return
  end if
!
!  Set up the start points in  X  and  Y.
!
  if ( 0 < incx ) then
    kx = 1
  else
    kx = 1 - ( n - 1 ) * incx
  end if

  if ( 0 < incy ) then
    ky = 1
  else
    ky = 1 - ( n - 1 ) * incy
  end if
!
!  Start the operations. In this version the elements of the array A
!  are accessed sequentially with one pass through A.
!
!  First form y := beta*y.
!
  if ( beta /= 1.0D+00 ) then
    if ( incy == 1 ) then
      if ( beta == 0.0D+00 ) then
        do i = 1, n
          y(i) = 0.0D+00
        end do
      else
        do i = 1, n
          y(i) = beta * y(i)
        end do
      end if
    else
      iy = ky
      if ( beta == 0.0D+00 ) then
        do i = 1, n
          y(iy) = 0.0D+00
          iy = iy + incy
        end do
      else
        do i = 1, n
          y(iy) = beta * y(iy)
          iy = iy + incy
        end do
      end if
    end if
  end if

  if ( alpha == 0.0D+00 ) then
    return
  end if

  if ( lsame ( uplo, 'U' ) ) then
!
!  Form y when upper triangle of A is stored.
!
    kplus1 = k + 1
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        temp1 = alpha * x(j)
        temp2 = 0.0D+00
        l = kplus1 - j
        do i = max ( 1, j - k ), j - 1
          y(i) = y(i) + temp1 * a(l+i,j)
          temp2 = temp2 + a(l+i,j) * x(i)
        end do
        y(j) = y(j) + temp1 * a(kplus1,j) + alpha * temp2
      end do
    else
      jx = kx
      jy = ky
      do j = 1, n
        temp1 = alpha * x(jx)
        temp2 = 0.0D+00
        ix = kx
        iy = ky
        l = kplus1 - j
        do i = max ( 1, j - k ), j - 1
          y(iy) = y(iy) + temp1 * a(l+i,j)
          temp2 = temp2 + a(l+i,j) * x(ix)
          ix = ix + incx
          iy = iy + incy
        end do
        y(jy) = y(jy) + temp1 * a(kplus1,j) + alpha * temp2
        jx = jx + incx
        jy = jy + incy
        if ( k < j ) then
          kx = kx + incx
          ky = ky + incy
        end if
      end do
    end if
  else
!
!  Form y when lower triangle of A is stored.
!
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        temp1 = alpha * x(j)
        temp2 = 0.0D+00
        y(j) = y(j) + temp1 * a(1,j)
        l = 1 - j
        do i = j + 1, min ( n, j + k )
          y(i) = y(i) + temp1 * a(l+i,j)
          temp2 = temp2 + a(l+i,j) * x(i)
        end do
        y(j) = y(j) + alpha * temp2
      end do
    else
      jx = kx
      jy = ky
      do j = 1, n
        temp1 = alpha * x(jx)
        temp2 = 0.0D+00
        y(jy) = y(jy) + temp1 * a(1,j)
        l = 1 - j
        ix = jx
        iy = jy
        do i = j + 1, min ( n, j + k )
          ix = ix + incx
          iy = iy + incy
          y(iy) = y(iy) + temp1 * a(l+i,j)
          temp2 = temp2 + a(l+i,j) * x(ix)
        end do
        y(jy) = y(jy) + alpha * temp2
        jx = jx + incx
        jy = jy + incy
      end do
    end if
  end if

  return
end
subroutine dspmv ( uplo, n, alpha, ap, x, incx, beta, y, incy )

!*****************************************************************************80
!
!! DSPMV computes y := alpha*A*x + beta*y for symmetric packed matrix A.
!
!  Discussion:
!
!    DSPMV performs the matrix-vector operation
!
!      y := alpha*A*x + beta*y,
!
!    where alpha and beta are scalars, x and y are n element vectors and
!    A is an n by n symmetric matrix, supplied in packed form.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the upper or lower
!    triangular part of the matrix A is being supplied in AP:
!    'u' or 'U': the upper triangular part of A is being supplied.
!    'l' or 'L': the lower triangular part of A is being supplied.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of 
!    the matrix.  0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the first scalar multiplier.
!
!    Input, real ( kind = 8 ) AP((n*(n+1))/2).
!    Before entry with UPLO = 'u' or 'U', the array AP must
!    contain the upper triangular part of the symmetric matrix
!    packed sequentially, column by column, so that AP(1)
!    contains a(1,1), AP(2) and AP(3) contain a(1,2)
!    and a(2,2) respectively, and so on.
!    Before entry with UPLO = 'l' or 'L', the array AP must
!    contain the lower triangular part of the symmetric matrix
!    packed sequentially, column by column, so that AP(1)
!    contains a(1,1), AP(2) and AP(3) contain a(2,1)
!    and a(3,1) respectively, and so on.
!
!    Input, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n element vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
!    Input, real ( kind = 8 ) BETA, the second scalar multiplier.
!    When BETA is supplied as zero then Y need not be set on input.
!
!    Input/output, real ( kind = 8 ) Y(1+(n-1)*abs(INCY) ).
!    Before entry, the incremented array Y must contain the n
!    element vector y. On exit, Y is overwritten by the updated vector y.
!
!    Input, integer ( kind = 4 ) INCY, the increment for elements of Y.
!    INCY must not be zero.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) ap(*)
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) jy
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) ky
  logical lsame
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  character uplo
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo, 'U' ) .and.  &
       .not. lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( n < 0 ) then
    info = 2
  else if ( incx == 0 ) then
    info = 6
  else if ( incy == 0 ) then
    info = 9
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSPMV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( ( n == 0 ) .or. ( ( alpha == 0.0D+00 ) .and. ( beta == 1.0D+00 ) ) ) then
    return
  end if
!
!  Set up the start points in X and Y.
!
  if ( 0 < incx ) then
    kx = 1
  else
    kx = 1 - ( n - 1 ) * incx
  end if

  if ( 0 < incy ) then
    ky = 1
  else
    ky = 1 - ( n - 1 ) * incy
  end if
!
!  Start the operations.  In this version the elements of the array AP
!  are accessed sequentially with one pass through AP.
!
!  First form  y := beta*y.
!
  if ( beta /= 1.0D+00 ) then
    if ( incy == 1 ) then
      if ( beta == 0.0D+00 ) then
        do i = 1, n
          y(i) = 0.0D+00
        end do
      else
        do i = 1, n
          y(i) = beta * y(i)
        end do
      end if
    else
      iy = ky
      if ( beta == 0.0D+00 ) then
        do i = 1, n
          y(iy) = 0.0D+00
          iy = iy + incy
        end do
      else
        do i = 1, n
          y(iy) = beta * y(iy)
          iy = iy + incy
        end do
      end if
    end if
  end if

  if ( alpha == 0.0D+00 ) then
    return
  end if

  kk = 1
  if ( lsame ( uplo, 'U' ) ) then
!
!  Form y when AP contains the upper triangle.
!
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        temp1 = alpha * x(j)
        temp2 = 0.0D+00
        k = kk
        do i = 1, j - 1
          y(i) = y(i) + temp1 * ap(k)
          temp2 = temp2 + ap(k) * x(i)
          k = k + 1
        end do
        y(j) = y(j) + temp1 * ap(kk+j-1) + alpha * temp2
        kk = kk + j
      end do
    else
      jx = kx
      jy = ky
      do j = 1, n
        temp1 = alpha * x(jx)
        temp2 = 0.0D+00
        ix = kx
        iy = ky
        do k = kk, kk + j - 2
          y(iy) = y(iy) + temp1 * ap(k)
          temp2 = temp2 + ap(k) * x(ix)
          ix = ix + incx
          iy = iy + incy
        end do
        y(jy) = y(jy) + temp1 * ap(kk+j-1) + alpha * temp2
        jx = jx + incx
        jy = jy + incy
        kk = kk + j
      end do
    end if
  else
!
!  Form y when AP contains the lower triangle.
!
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        temp1 = alpha * x(j)
        temp2 = 0.0D+00
        y(j) = y(j) + temp1 * ap(kk)
        k = kk + 1
        do i = j + 1, n
          y(i) = y(i) + temp1 * ap(k)
          temp2 = temp2 + ap(k) * x(i)
          k = k + 1
        end do
        y(j) = y(j) + alpha * temp2
        kk = kk + ( n - j + 1 )
      end do
    else
      jx = kx
      jy = ky
      do j = 1, n
        temp1 = alpha * x(jx)
        temp2 = 0.0D+00
        y(jy) = y(jy) + temp1 * ap(kk)
        ix = jx
        iy = jy
        do k = kk + 1, kk + n - j
          ix = ix + incx
          iy = iy + incy
          y(iy) = y(iy) + temp1 * ap(k)
          temp2 = temp2 + ap(k) * x(ix)
        end do
        y(jy) = y(jy) + alpha * temp2
        jx = jx + incx
        jy = jy + incy
        kk = kk + ( n - j + 1 )
      end do
    end if
  end if

  return
end
subroutine dspr ( uplo, n, alpha, x, incx, ap )

!*****************************************************************************80
!
!! DSPR computes A := alpha*x*x' + A, where A is a symmetric packed matrix.
!
!  Discussion:
!
!    DSPR performs the symmetric rank 1 operation
!
!      A := alpha*x*x' + A,
!
!    where alpha is a real scalar, x is an n element vector and A is an
!    n by n symmetric matrix, supplied in packed form.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the upper or lower
!    triangular part of the matrix A is being supplied in AP:
!    'u' or 'U': the upper triangular part of A is being supplied.
!    'l' or 'L': the lower triangular part of A is being supplied.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the scalar multiplier.
!
!    Input, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n
!    element vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
!    Input/output, real ( kind = 8 ) AP((n*(n+1))/2).
!    Before entry with  UPLO = 'u' or 'U', the array AP must
!    contain the upper triangular part of the symmetric matrix
!    packed sequentially, column by column, so that AP(1)
!    contains a(1,1), AP(2) and AP(3) contain a(1,2)
!    and a(2,2) respectively, and so on. On exit, the array
!    AP is overwritten by the upper triangular part of the
!    updated matrix.
!    Before entry with UPLO = 'l' or 'L', the array AP must
!    contain the lower triangular part of the symmetric matrix
!    packed sequentially, column by column, so that AP(1)
!    contains a(1,1), AP(2) and AP(3) contain a(2,1)
!    and a(3,1) respectively, and so on. On exit, the array
!    AP is overwritten by the lower triangular part of the
!    updated matrix.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) ap(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) kx
  logical lsame
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp
  character uplo
  real ( kind = 8 ) x(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo, 'U' ) .and.  &
         .not. lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( n < 0 ) then
    info = 2
  else if ( incx == 0 ) then
    info = 5
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSPR', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( ( n == 0 ) .or. ( alpha == 0.0D+00 ) ) then
    return
  end if
!
!  Set the start point in X if the increment is not unity.
!
  if ( incx <= 0 ) then
    kx = 1 - ( n - 1 ) * incx
  else if ( incx /= 1 ) then
    kx = 1
  end if
!
!  Start the operations. In this version the elements of the array AP
!  are accessed sequentially with one pass through AP.
!
  kk = 1
  if ( lsame ( uplo, 'U' ) ) then
!
!  Form A when upper triangle is stored in AP.
!
    if ( incx == 1 ) then
      do j = 1, n
        if ( x(j) /= 0.0D+00 ) then
          temp = alpha * x(j)
          k = kk
          do i = 1, j
            ap(k) = ap(k) + x(i) * temp
            k = k + 1
          end do
        end if
        kk = kk + j
      end do
    else
      jx = kx
      do j = 1, n
        if ( x(jx) /= 0.0D+00 ) then
          temp = alpha * x(jx)
          ix = kx
          do k = kk, kk + j - 1
            ap(k) = ap(k) + x(ix) * temp
            ix = ix + incx
          end do
        end if
        jx = jx + incx
        kk = kk + j
      end do
    end if
  else
!
!  Form A when lower triangle is stored in AP.
!
    if ( incx == 1 ) then
      do j = 1, n
        if ( x(j) /= 0.0D+00 ) then
          temp = alpha * x(j)
          k = kk
          do i = j, n
            ap(k) = ap(k) + x(i) * temp
            k = k + 1
          end do
        end if
        kk = kk + n - j + 1
      end do
    else
      jx = kx
      do j = 1, n
        if ( x(jx) /= 0.0D+00 ) then
          temp = alpha * x(jx)
          ix = jx
          do k = kk, kk + n - j
            ap(k) = ap(k) + x(ix) * temp
            ix = ix + incx
          end do
        end if
        jx = jx + incx
        kk = kk + n - j + 1
      end do
    end if
  end if

  return
end
subroutine dspr2 ( uplo, n, alpha, x, incx, y, incy, ap )

!*****************************************************************************80
!
!! DSPR2 sets A := alpha*x*y' + alpha*y*x' + A, A is a symmetric packed matrix.
!
!  Discussion:
!
!    DSPR2 performs the symmetric rank 2 operation
!
!      A := alpha*x*y' + alpha*y*x' + A,
!
!    where alpha is a scalar, x and y are n element vectors and A is an
!    n by n symmetric matrix, supplied in packed form.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the upper or lower
!    triangular part of the matrix A is being supplied in AP:
!    'u' or 'U': the upper triangular part of A is being supplied.
!    'l' or 'L': the lower triangular part of A is being supplied.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the scalar multiplier.
!
!    Input, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n element vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
!    Input, real ( kind = 8 ) Y(1+(n-1)*abs(INCY)).
!    Before entry, the incremented array Y must contain the n element vector y.
!
!    Input, integer ( kind = 4 ) INCY, the increment for elements of Y.
!    INCY must not be zero.
!
!    Input/output, real ( kind = 8 ) AP((n*(n+1))/2).
!    Before entry with  UPLO = 'u' or 'U', the array AP must
!    contain the upper triangular part of the symmetric matrix
!    packed sequentially, column by column, so that AP(1)
!    contains a(1,1), AP(2) and AP(3) contain a(1,2)
!    and a(2,2) respectively, and so on. On exit, the array
!    AP is overwritten by the upper triangular part of the
!    updated matrix.
!    Before entry with UPLO = 'l' or 'L', the array AP must
!    contain the lower triangular part of the symmetric matrix
!    packed sequentially, column by column, so that AP(1)
!    contains a(1,1), AP(2) and AP(3) contain a(2,1)
!    and a(3,1) respectively, and so on. On exit, the array
!    AP is overwritten by the lower triangular part of the
!    updated matrix.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) ap(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) jy
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) ky
  logical lsame
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  character uplo
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo, 'U' ) .and.  &
        .not. lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( n < 0 ) then
    info = 2
  else if ( incx == 0 ) then
    info = 5
  else if ( incy == 0 ) then
    info = 7
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSPR2', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( ( n == 0 ) .or. ( alpha == 0.0D+00 ) ) then
    return
  end if
!
!  Set up the start points in X and Y if the increments are not both
!  unity.
!
  if ( ( incx /= 1 ) .or. ( incy /= 1 ) ) then

    if ( 0 < incx ) then
      kx = 1
    else
      kx = 1 - ( n - 1 ) * incx
    end if

    if ( 0 < incy ) then
      ky = 1
    else
      ky = 1 - ( n - 1 ) * incy
    end if

    jx = kx
    jy = ky

  end if
!
!  Start the operations. In this version the elements of the array AP
!  are accessed sequentially with one pass through AP.
!
  kk = 1
  if ( lsame ( uplo, 'U' ) ) then
!
!  Form  A  when upper triangle is stored in AP.
!
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        if ( ( x(j) /= 0.0D+00 ) .or. ( y(j) /= 0.0D+00 ) ) then
          temp1 = alpha * y(j)
          temp2 = alpha * x(j)
          k = kk
          do i = 1, j
            ap(k) = ap(k) + x(i) * temp1 + y(i) * temp2
            k = k + 1
          end do
        end if
        kk = kk + j
      end do
    else
      do j = 1, n
        if ( ( x(jx) /= 0.0D+00 ) .or. ( y(jy) /= 0.0D+00 ) ) then
          temp1 = alpha * y(jy)
          temp2 = alpha * x(jx)
          ix = kx
          iy = ky
          do k = kk, kk + j - 1
            ap(k) = ap(k) + x(ix) * temp1 + y(iy) * temp2
            ix = ix + incx
            iy = iy + incy
          end do
        end if
        jx = jx + incx
        jy = jy + incy
        kk = kk + j
      end do
    end if
  else
!
!  Form  A  when lower triangle is stored in AP.
!
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        if ( ( x(j) /= 0.0D+00 ) .or. ( y(j) /= 0.0D+00 ) ) then
          temp1 = alpha * y(j)
          temp2 = alpha * x(j)
          k = kk
          do i = j, n
            ap(k) = ap(k) + x(i) * temp1 + y(i) * temp2
            k = k + 1
          end do
        end if
        kk = kk + n - j + 1
      end do
    else
      do j = 1, n
        if ( ( x(jx) /= 0.0D+00 ) .or. ( y(jy) /= 0.0D+00 ) ) then
          temp1 = alpha * y(jy)
          temp2 = alpha * x(jx)
          ix = jx
          iy = jy
          do k = kk, kk + n - j
            ap(k) = ap(k) + x(ix) * temp1 + y(iy) * temp2
            ix = ix + incx
            iy = iy + incy
          end do
        end if
        jx = jx + incx
        jy = jy + incy
        kk = kk + n - j + 1
      end do
    end if
  end if

  return
end
subroutine dsymv ( uplo, n, alpha, a, lda, x, incx, beta, y, incy )

!*****************************************************************************80
!
!! DSYMV computes y := alpha*A*x + beta*y for symmetric matrix A.
!
!  Discussion:
!
!    DSYMV performs the matrix-vector operation
!
!      y := alpha*A*x + beta*y,
!
!    where alpha and beta are scalars, x and y are n element vectors and
!    A is an n by n symmetric matrix.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the upper or lower
!    triangular part of the matrix A is to be referenced:
!    'u' or 'U': the upper triangular part of A is to be referenced.
!    'l' or 'L': the lower triangular part of A is to be referenced.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the first scalar multiplier.
!
!    Input, real ( kind = 8 ) A(LDA,n).
!    Before entry with  UPLO = 'u' or 'U', the leading n by n
!    upper triangular part of the array A must contain the upper
!    triangular part of the symmetric matrix and the strictly
!    lower triangular part of A is not referenced.
!    Before entry with UPLO = 'l' or 'L', the leading n by n
!    lower triangular part of the array A must contain the lower
!    triangular part of the symmetric matrix and the strictly
!    upper triangular part of A is not referenced.
!    Unchanged on exit.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program. max ( 1, N ) <= LDA.
!
!    Input, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n
!    element vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
!    Input, real ( kind = 8 ) BETA, the second scalar multiplier.  When BETA is
!    supplied as zero then Y need not be set on input.
!
!    Input, real ( kind = 8 ) Y(1+(n-1)*abs(INCY)).
!    Before entry, the incremented array Y must contain the n
!    element vector y. On exit, Y is overwritten by the updated vector y.
!
!    Input, integer ( kind = 4 ) INCY, the increment for elements of Y.
!    INCY must not be zero.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) jy
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) ky
  logical lsame
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  character uplo
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo, 'U' ) .and.  &
       .not. lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( n < 0 ) then
    info = 2
  else if ( lda < max ( 1, n ) ) then
    info = 5
  else if ( incx == 0 ) then
    info = 7
  else if ( incy == 0 ) then
    info = 10
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSYMV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( ( n == 0 ) .or. ( ( alpha == 0.0D+00 ) .and. ( beta == 1.0D+00 ) ) ) then
    return
  end if
!
!  Set up the start points in X and Y.
!
  if ( 0 < incx ) then
    kx = 1
  else
    kx = 1 - ( n - 1 ) * incx
  end if

  if ( 0 < incy ) then
    ky = 1
  else
    ky = 1 - ( n - 1 ) * incy
  end if
!
!  Start the operations. In this version the elements of A are
!  accessed sequentially with one pass through the triangular part
!  of A.
!
!  First form  y := beta*y.
!
  if ( beta /= 1.0D+00 ) then
    if ( incy == 1 ) then
      if ( beta == 0.0D+00 ) then
        do i = 1, n
          y(i) = 0.0D+00
        end do
      else
        do i = 1, n
          y(i) = beta * y(i)
        end do
      end if
    else
      iy = ky
      if ( beta == 0.0D+00 ) then
        do i = 1, n
          y(iy) = 0.0D+00
          iy = iy + incy
        end do
      else
        do i = 1, n
          y(iy) = beta * y(iy)
          iy = iy + incy
        end do
      end if
    end if
  end if

  if ( alpha == 0.0D+00 ) then
   return
  end if

  if ( lsame ( uplo, 'U' ) ) then
!
!  Form  y  when A is stored in upper triangle.
!
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        temp1 = alpha * x(j)
        temp2 = 0.0D+00
        do i = 1, j - 1
          y(i) = y(i) + temp1 * a(i,j)
          temp2 = temp2 + a(i,j) * x(i)
        end do
        y(j) = y(j) + temp1 * a(j,j) + alpha * temp2
      end do
    else
      jx = kx
      jy = ky
      do j = 1, n
        temp1 = alpha * x(jx)
        temp2 = 0.0D+00
        ix = kx
        iy = ky
        do i = 1, j - 1
          y(iy) = y(iy) + temp1 * a(i,j)
          temp2 = temp2 + a(i,j) * x(ix)
          ix = ix + incx
          iy = iy + incy
        end do
        y(jy) = y(jy) + temp1 * a(j,j) + alpha * temp2
        jx = jx + incx
        jy = jy + incy
      end do
    end if
  else
!
!  Form y when A is stored in lower triangle.
!
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        temp1 = alpha * x(j)
        temp2 = 0.0D+00
        y(j) = y(j) + temp1 * a(j,j)
        do i = j + 1, n
          y(i) = y(i) + temp1 * a(i,j)
          temp2 = temp2 + a(i,j) * x(i)
        end do
        y(j) = y(j) + alpha * temp2
      end do
    else
      jx = kx
      jy = ky
      do j = 1, n
        temp1 = alpha * x(jx)
        temp2 = 0.0D+00
        y(jy) = y(jy) + temp1 * a(j,j)
        ix = jx
        iy = jy
        do i = j + 1, n
          ix = ix + incx
          iy = iy + incy
          y(iy) = y(iy) + temp1 * a(i,j)
          temp2 = temp2 + a(i,j) * x(ix)
        end do
        y(jy) = y(jy) + alpha * temp2
        jx = jx + incx
        jy = jy + incy
      end do
    end if
  end if

  return
end
subroutine dsyr ( uplo, n, alpha, x, incx, a, lda )

!*****************************************************************************80
!
!! DSYR computes A := alpha*x*x' + A where A is a symmetric matrix.
!
!  Discussion:
!
!    DSYR performs the symmetric rank 1 operation
!
!      A := alpha*x*x' + A,
!
!    where alpha is a real scalar, x is an n element vector and A is an
!    n by n symmetric matrix.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the upper or lower
!    triangular part of the matrix A is to be referenced:
!    'u' or 'U': the upper triangular part of A is to be referenced.
!    'l' or 'L': the lower triangular part of A is to be referenced.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the scalar multiplier.
!
!    Input, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n
!    element vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
!    Input/output, real ( kind = 8 ) A(LDA,N).
!    Before entry with  UPLO = 'u' or 'U', the leading n by n
!    upper triangular part of the array A must contain the upper
!    triangular part of the symmetric matrix and the strictly
!    lower triangular part of A is not referenced. On exit, the
!    upper triangular part of the array A is overwritten by the
!    upper triangular part of the updated matrix.
!    Before entry with UPLO = 'l' or 'L', the leading n by n
!    lower triangular part of the array A must contain the lower
!    triangular part of the symmetric matrix and the strictly
!    upper triangular part of A is not referenced. On exit, the
!    lower triangular part of the array A is overwritten by the
!    lower triangular part of the updated matrix.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program. max ( 1, M ) <= LDA.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) kx
  logical lsame
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp
  character uplo
  real ( kind = 8 ) x(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo, 'U' ) .and.  &
        .not. lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( n < 0 ) then
    info = 2
  else if ( incx == 0 ) then
    info = 5
  else if ( lda < max ( 1, n ) ) then
    info = 7
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSYR', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( ( n == 0 ) .or. ( alpha == 0.0D+00 ) ) then
    return
  end if
!
!  Set the start point in X if the increment is not unity.
!
  if ( incx <= 0 ) then
    kx = 1 - ( n - 1 ) * incx
  else if ( incx /= 1 ) then
    kx = 1
  end if
!
!  Start the operations. In this version the elements of A are
!  accessed sequentially with one pass through the triangular part
!  of A.
!
  if ( lsame ( uplo, 'U' ) ) then
!
!  Form  A  when A is stored in upper triangle.
!
    if ( incx == 1 ) then
      do j = 1, n
        if ( x(j) /= 0.0D+00 ) then
          temp = alpha * x(j)
          do i = 1, j
            a(i,j) = a(i,j) + x(i) * temp
          end do
        end if
      end do
    else
      jx = kx
      do j = 1, n
        if ( x(jx) /= 0.0D+00 ) then
          temp = alpha * x(jx)
          ix = kx
          do i = 1, j
            a(i,j) = a(i,j) + x(ix) * temp
            ix = ix + incx
          end do
        end if
        jx = jx + incx
      end do
    end if
  else
!
!  Form A when A is stored in lower triangle.
!
    if ( incx == 1 ) then
      do j = 1, n
        if ( x(j) /= 0.0D+00 ) then
          temp = alpha * x(j)
          do i = j, n
            a(i,j) = a(i,j) + x(i) * temp
          end do
        end if
      end do
    else
      jx = kx
      do j = 1, n
        if ( x(jx) /= 0.0D+00 ) then
          temp = alpha * x(jx)
          ix = jx
          do i = j, n
            a(i,j) = a(i,j) + x(ix) * temp
            ix = ix + incx
          end do
        end if
        jx = jx + incx
      end do
    end if
  end if

  return
end
subroutine dsyr2 ( uplo, n, alpha, x, incx, y, incy, a, lda )

!*****************************************************************************80
!
!! DSYR2 computes A := alpha*x*y' + alpha*y*x' + A, where A is symmetric.
!
!  Discussion:
!
!    DSYR2 performs the symmetric rank 2 operation
!
!      A := alpha*x*y' + alpha*y*x' + A,
!
!    where alpha is a scalar, x and y are n element vectors and A is an n
!    by n symmetric matrix.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the upper or lower
!    triangular part of the matrix A is to be referenced:
!    'u' or 'U': the upper triangular part of A is to be referenced.
!    'l' or 'L': the lower triangular part of A is to be referenced.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, real ( kind = 8 ) ALPHA, the first scalar multiplier.
!
!    Input, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n
!    element vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
!    Input, real ( kind = 8 ) Y(1+(n-1)*abs(INCY)).
!    Before entry, the incremented array Y must contain the n element vector y.
!
!    Input, integer ( kind = 4 ) INCY, the increment for elements of Y.
!    INCY must not be zero.
!
!    Input/output, real ( kind = 8 ) A(LDA,n).
!    Before entry with  UPLO = 'U' or 'U', the leading n by n
!    upper triangular part of the array A must contain the upper
!    triangular part of the symmetric matrix and the strictly
!    lower triangular part of A is not referenced. On exit, the
!    upper triangular part of the array A is overwritten by the
!    upper triangular part of the updated matrix.
!    Before entry with UPLO = 'L' or 'L', the leading n by n
!    lower triangular part of the array A must contain the lower
!    triangular part of the symmetric matrix and the strictly
!    upper triangular part of A is not referenced. On exit, the
!    lower triangular part of the array A is overwritten by the
!    lower triangular part of the updated matrix.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program. max ( 1, N ) <= LDA.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  real ( kind = 8 ) alpha
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) jy
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) ky
  logical lsame
  integer ( kind = 4 ) n
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  character uplo
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo, 'U' ) .and.  &
     .not. lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( n < 0 ) then
    info = 2
  else if ( incx == 0 ) then
    info = 5
  else if ( incy == 0 ) then
    info = 7
  else if ( lda < max ( 1, n ) ) then
    info = 9
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DSYR2', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( ( n == 0 ) .or. ( alpha == 0.0D+00 ) ) then
    return
  end if
!
!  Set up the start points in X and Y if the increments are not both
!  unity.
!
  if ( ( incx /= 1 ) .or. ( incy /= 1 ) ) then
    if ( 0 < incx ) then
      kx = 1
    else
      kx = 1 - ( n - 1 ) * incx
    end if
    if ( 0 < incy ) then
      ky = 1
    else
      ky = 1 - ( n - 1 ) * incy
    end if
    jx = kx
    jy = ky
  end if
!
!  Start the operations. In this version the elements of A are
!  accessed sequentially with one pass through the triangular part
!  of A.
!
  if ( lsame ( uplo, 'U' ) ) then
!
!  Form A when A is stored in the upper triangle.
!
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        if ( ( x(j) /= 0.0D+00 ) .or. ( y(j) /= 0.0D+00 ) ) then
          temp1 = alpha * y(j)
          temp2 = alpha * x(j)
          do i = 1, j
            a(i,j) = a(i,j) + x(i) * temp1 + y(i) * temp2
          end do
        end if
      end do
    else
      do j = 1, n
        if ( ( x(jx) /= 0.0D+00 ) .or. ( y(jy) /= 0.0D+00 ) ) then
          temp1 = alpha * y(jy)
          temp2 = alpha * x(jx)
          ix = kx
          iy = ky
          do i = 1, j
            a(i,j) = a(i,j) + x(ix) * temp1 + y(iy) * temp2
            ix = ix + incx
            iy = iy + incy
          end do
        end if
        jx = jx + incx
        jy = jy + incy
      end do
    end if
  else
!
!  Form A when A is stored in the lower triangle.
!
    if ( ( incx == 1 ) .and. ( incy == 1 ) ) then
      do j = 1, n
        if ( ( x(j) /= 0.0D+00 ) .or. ( y(j) /= 0.0D+00 ) ) then
          temp1 = alpha * y(j)
          temp2 = alpha * x(j)
          do i = j, n
            a(i,j) = a(i,j) + x(i) * temp1 + y(i) * temp2
          end do
        end if
      end do
    else
      do j = 1, n
        if ( ( x(jx) /= 0.0D+00 ) .or. ( y(jy) /= 0.0D+00 ) ) then
          temp1 = alpha * y(jy)
          temp2 = alpha * x(jx)
          ix = jx
          iy = jy
          do i = j, n
            a(i,j) = a(i,j) + x(ix) * temp1 + y(iy) * temp2
            ix = ix + incx
            iy = iy + incy
          end do
        end if
        jx = jx + incx
        jy = jy + incy
      end do
    end if
  end if

  return
end
subroutine dtbmv ( uplo, trans, diag, n, k, a, lda, x, incx )

!*****************************************************************************80
!
!! DTBMV computes x = A*x or x = A'*x for a triangular band matrix A.
!
!  Discussion:
!
!    DTBMV performs one of the matrix-vector operations
!
!      x := A*x,   or   x := A'*x,
!
!    where x is an n element vector and  A is an n by n unit, or non-unit,
!    upper or lower triangular band matrix, with ( k + 1 ) diagonals.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the matrix A is upper or
!    lower triangulat:
!    'u' or 'U': A is upper triangular.
!    'l' or 'L': A is lower triangular.
!
!    Input, character TRANS, specifies the operation to be performed:
!    'n' or 'N'   x := A*x.
!    't' or 'T'   x := A'*x.
!    'c' or 'C'   x := A'*x.
!
!    Input, character DIAG, specifies whether A is unit triangular:
!    'u' or 'U'   A is assumed to be unit triangular.
!    'n' or 'N'   A is not assumed to be unit triangular.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, integer ( kind = 4 ) K.
!    On entry with UPLO = 'u' or 'U', K specifies the number of
!    super-diagonals of the matrix A.
!    On entry with UPLO = 'l' or 'L', K specifies the number of
!    sub-diagonals of the matrix A.  0 <= K.
!
!    Input, real ( kind = 8 ) A(LDA,N).
!    Before entry with UPLO = 'u' or 'U', the leading ( k + 1 )
!    by n part of the array A must contain the upper triangular
!    band part of the matrix of coefficients, supplied column by
!    column, with the leading diagonal of the matrix in row
!    ( k + 1 ) of the array, the first super-diagonal starting at
!    position 2 in row k, and so on. The top left k by k triangle
!    of the array A is not referenced.
!    The following program segment will transfer an upper
!    triangular band matrix from conventional full matrix storage
!    to band storage:
!      do J = 1, N
!        M = K + 1 - J
!        do I = max ( 1, J - K ), J
!          A( M + I, J ) = matrix( I, J )
!        end do
!      end do
!    Before entry with UPLO = 'l' or 'L', the leading ( k + 1 )
!    by n part of the array A must contain the lower triangular
!    band part of the matrix of coefficients, supplied column by
!    column, with the leading diagonal of the matrix in row 1 of
!    the array, the first sub-diagonal starting at position 1 in
!    row 2, and so on. The bottom right k by k triangle of the
!    array A is not referenced.
!    The following program segment will transfer a lower
!    triangular band matrix from conventional full matrix storage
!    to band storage:
!      do J = 1, N
!        M = 1 - J
!        do I = J, min ( N, J + K )
!          A( M + I, J ) = matrix( I, J )
!        end do
!      end do
!    Note that when DIAG = 'U' or 'U' the elements of the array A
!    corresponding to the diagonal elements of the matrix are not
!    referenced, but are assumed to be unity.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program. K+1 <= LDA.
!
!    Input/output, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n
!    element vector x. On exit, X is overwritten with the tranformed vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kplus1
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) l
  logical lsame
  integer ( kind = 4 ) n
  logical nounit
  real ( kind = 8 ) temp
  character trans
  character uplo
  real ( kind = 8 ) x(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo , 'U' ) .and.  &
       .not. lsame ( uplo , 'L' ) ) then
    info = 1
  else if ( .not. lsame ( trans, 'N' ) .and.  &
            .not. lsame ( trans, 'T' ) .and.  &
            .not. lsame ( trans, 'C' ) ) then
    info = 2
  else if ( .not. lsame ( diag , 'U' ) .and.  &
            .not. lsame ( diag , 'N' ) ) then
    info = 3
  else if ( n < 0 ) then
    info = 4
  else if ( k < 0 ) then
    info = 5
  else if ( lda < ( k + 1 ) ) then
    info = 7
  else if ( incx == 0 ) then
    info = 9
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DTBMV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  end if

  nounit = lsame ( diag, 'N' )
!
!  Set up the start point in X if the increment is not unity. This
!  will be ( N - 1 ) * INCX too small for descending loops.
!
  if ( incx <= 0 ) then
    kx = 1 - ( n - 1 ) * incx
  else if ( incx /= 1 ) then
    kx = 1
  end if
!
!  Start the operations. In this version the elements of A are
!  accessed sequentially with one pass through A.
!
  if ( lsame ( trans, 'N' ) ) then
!
!  Form x := A*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      kplus1 = k + 1
      if ( incx == 1 ) then
        do j = 1, n
          if ( x(j) /= 0.0D+00 ) then
            temp = x(j)
            l = kplus1 - j
            do i = max ( 1, j - k ), j - 1
              x(i) = x(i) + temp * a(l+i,j)
            end do
            if ( nounit ) then
              x(j) = x(j) * a(kplus1,j)
            end if
          end if
        end do
      else
        jx = kx
        do j = 1, n
          if ( x(jx) /= 0.0D+00 ) then
            temp = x(jx)
            ix = kx
            l = kplus1 - j
            do i = max ( 1, j - k ), j - 1
              x(ix) = x(ix) + temp * a(l+i,j)
              ix = ix + incx
            end do
            if ( nounit ) then
              x(jx) = x(jx) * a(kplus1,j)
            end if
          end if
          jx = jx + incx
          if ( k < j ) then
           kx = kx + incx
          end if
        end do
      end if
    else
      if ( incx == 1 ) then
        do j = n, 1, -1
          if ( x(j) /= 0.0D+00 ) then
            temp = x(j)
            l = 1 - j
            do i = min ( n, j + k ), j + 1, -1
              x(i) = x(i) + temp * a(l+i,j)
            end do
            if ( nounit ) then
              x(j) = x(j) * a(1,j)
            end if
          end if
        end do
      else
        kx = kx + ( n - 1 ) * incx
        jx = kx
        do j = n, 1, -1
          if ( x(jx) /= 0.0D+00 ) then
            temp = x(jx)
            ix = kx
            l = 1 - j
            do i = min ( n, j + k ), j + 1, -1
              x(ix) = x(ix) + temp * a(l+i,j)
              ix = ix - incx
            end do
            if ( nounit ) then
              x(jx) = x(jx) * a(1,j)
            end if
          end if
          jx = jx - incx
          if ( k <= ( n - j ) ) then
            kx = kx - incx
          end if
        end do
      end if
    end if
  else
!
!  Form x := A'*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      kplus1 = k + 1
      if ( incx == 1 ) then
        do j = n, 1, -1
          temp = x(j)
          l = kplus1 - j
          if ( nounit ) then
            temp = temp * a(kplus1,j)
          end if
          do i = j - 1, max ( 1, j - k ), -1
            temp = temp + a(l+i,j) * x(i)
          end do
          x(j) = temp
        end do
      else
        kx = kx + ( n - 1 ) * incx
        jx = kx
        do j = n, 1, -1
          temp = x(jx)
          kx = kx - incx
          ix = kx
          l = kplus1 - j
          if ( nounit ) then
            temp = temp * a(kplus1,j)
          end if
          do i = j - 1, max ( 1, j - k ), -1
            temp = temp + a(l+i,j) * x(ix)
            ix = ix - incx
          end do
          x(jx) = temp
          jx = jx - incx
        end do
      end if
    else
      if ( incx == 1 ) then
        do j = 1, n
          temp = x(j)
          l = 1 - j
          if ( nounit ) then
            temp = temp * a(1,j)
          end if
          do i = j + 1, min ( n, j + k )
            temp = temp + a(l+i,j) * x(i)
          end do
          x(j) = temp
        end do
      else
        jx = kx
        do j = 1, n
          temp = x(jx)
          kx = kx + incx
          ix = kx
          l = 1 - j
          if ( nounit ) then
            temp = temp * a(1,j)
          end if
          do i = j + 1, min ( n, j + k )
            temp = temp + a(l+i,j) * x(ix)
            ix = ix + incx
          end do
          x(jx) = temp
          jx = jx + incx
        end do
      end if
    end if
  end if

  return
end
subroutine dtbsv ( uplo, trans, diag, n, k, a, lda, x, incx )

!*****************************************************************************80
!
!! DTBSV solves A*x = b or A'*x = b for triangular band matrix A.
!
!  Discussion:
!
!    DTBSV solves one of the systems of equations
!
!      A*x = b,   or   A'*x = b,
!
!    where b and x are n element vectors and A is an n by n unit, or
!    non-unit, upper or lower triangular band matrix, with ( k + 1 )
!    diagonals.
!
!    No test for singularity or near-singularity is included in this
!    routine. Such tests must be performed before calling this routine.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the matrix A is upper or
!    lower triangular.
!    'u' or 'U': A is upper triangular.
!    'l' or 'L': A is lower triangular.
!
!    Input, character TRANS, specifies the equations to be solved:
!    'n' or 'N'   A*x = b.
!    't' or 'T'   A'*x = b.
!    'c' or 'C'   A'*x = b.
!
!    Input, character DIAG, specifies whether or not A is unit triangular:
!    'u' or 'U': A is assumed to be unit triangular.
!    'n' or 'N': A is not assumed to be unit triangular.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, integer ( kind = 4 ), K.
!    On entry with UPLO = 'u' or 'U', K specifies the number of
!    super-diagonals of the matrix A.
!    On entry with UPLO = 'l' or 'L', K specifies the number of
!    sub-diagonals of the matrix A.  0 <= K.
!
!    Input, real ( kind = 8 ) A(LDA,N).
!    Before entry with UPLO = 'u' or 'U', the leading ( k + 1 )
!    by n part of the array A must contain the upper triangular
!    band part of the matrix of coefficients, supplied column by
!    column, with the leading diagonal of the matrix in row
!    ( k + 1 ) of the array, the first super-diagonal starting at
!    position 2 in row k, and so on. The top left k by k triangle
!    of the array A is not referenced.
!    The following program segment will transfer an upper
!    triangular band matrix from conventional full matrix storage
!    to band storage:
!      do J = 1, N
!        M = K + 1 - J
!        do I = max ( 1, J - K ), J
!          A( M + I, J ) = matrix( I, J )
!        end do
!      end do
!    Before entry with UPLO = 'l' or 'L', the leading ( k + 1 )
!    by n part of the array A must contain the lower triangular
!    band part of the matrix of coefficients, supplied column by
!    column, with the leading diagonal of the matrix in row 1 of
!    the array, the first sub-diagonal starting at position 1 in
!    row 2, and so on. The bottom right k by k triangle of the
!    array A is not referenced.
!    The following program segment will transfer a lower
!    triangular band matrix from conventional full matrix storage
!    to band storage:
!      do J = 1, N
!        M = 1 - J
!        do I = J, min ( N, J + K )
!          A( M + I, J ) = matrix( I, J )
!        end do
!      end do
!    Note that when DIAG = 'u' or 'U' the elements of the array A
!    corresponding to the diagonal elements of the matrix are not
!    referenced, but are assumed to be unity.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program. K+1 <= LDA.
!
!    Input/output, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n
!    element right-hand side vector b. On exit, X is overwritten
!    with the solution vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kplus1
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) l
  logical lsame
  integer ( kind = 4 ) n
  logical nounit
  real ( kind = 8 ) temp
  character trans
  character uplo
  real ( kind = 8 ) x(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo , 'U' ) .and.  &
       .not. lsame ( uplo , 'L' ) ) then
    info = 1
  else if ( .not. lsame ( trans, 'N' ) .and.  &
          .not. lsame ( trans, 'T' ) .and.  &
          .not. lsame ( trans, 'C' ) ) then
    info = 2
  else if ( .not. lsame ( diag , 'U' ) .and.  &
         .not. lsame ( diag , 'N' ) ) then
    info = 3
  else if ( n < 0 ) then
    info = 4
  else if ( k < 0 ) then
    info = 5
  else if ( lda < ( k + 1 ) ) then
    info = 7
  else if ( incx == 0 ) then
    info = 9
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DTBSV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  end if

  nounit = lsame ( diag, 'N' )
!
!  Set up the start point in X if the increment is not unity. This
!  will be  ( N - 1 ) * INCX  too small for descending loops.
!
  if ( incx <= 0 ) then
    kx = 1 - ( n - 1 ) * incx
  else if ( incx /= 1 ) then
    kx = 1
  end if
!
!  Start the operations. In this version the elements of A are
!  accessed by sequentially with one pass through A.
!
  if ( lsame ( trans, 'N' ) ) then
!
!  Form  x := inv( A )*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      kplus1 = k + 1
      if ( incx == 1 ) then
        do j = n, 1, -1
          if ( x(j) /= 0.0D+00 ) then
            l = kplus1 - j
            if ( nounit ) then
              x(j) = x(j) / a(kplus1,j)
            end if
            temp = x(j)
            do i = j - 1, max ( 1, j - k ), -1
              x(i) = x(i) - temp * a(l+i,j)
            end do
          end if
        end do
      else
        kx = kx + ( n - 1 ) * incx
        jx = kx
        do j = n, 1, -1
          kx = kx - incx
          if ( x(jx) /= 0.0D+00 ) then
            ix = kx
            l = kplus1 - j
            if ( nounit ) then
              x(jx) = x(jx) / a(kplus1,j)
            end if
            temp = x(jx)
            do i = j - 1, max ( 1, j - k ), -1
              x(ix) = x(ix) - temp * a(l+i,j)
              ix = ix - incx
            end do
          end if
          jx = jx - incx
        end do
      end if
    else
      if ( incx == 1 ) then
        do j = 1, n
          if ( x(j) /= 0.0D+00 ) then
            l = 1 - j
            if ( nounit ) then
              x(j) = x(j) / a(1,j)
            end if
            temp = x(j)
            do i = j + 1, min ( n, j + k )
              x(i) = x(i) - temp * a(l+i,j)
            end do
          end if
        end do
      else
        jx = kx
        do j = 1, n
          kx = kx + incx
          if ( x(jx) /= 0.0D+00 ) then
            ix = kx
            l = 1  - j
            if ( nounit ) then
              x(jx) = x(jx) / a(1,j)
            end if
            temp = x(jx)
            do i = j + 1, min ( n, j + k )
              x(ix) = x(ix) - temp * a(l+i,j)
              ix = ix + incx
            end do
          end if
          jx = jx + incx
        end do
      end if
    end if
  else
!
!  Form  x := inv( A')*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      kplus1 = k + 1
      if ( incx == 1 ) then
        do j = 1, n
          temp = x(j)
          l = kplus1 - j
          do i = max ( 1, j - k ), j - 1
            temp = temp - a(l+i,j) * x(i)
          end do
          if ( nounit ) then
            temp = temp / a(kplus1,j)
          end if
          x(j) = temp
        end do
      else
        jx = kx
        do j = 1, n
          temp = x(jx)
          ix = kx
          l = kplus1  - j
          do i = max ( 1, j - k ), j - 1
            temp = temp - a(l+i,j) * x(ix)
            ix = ix + incx
          end do
          if ( nounit ) then
            temp = temp / a(kplus1,j)
          end if
          x(jx) = temp
          jx = jx + incx
          if ( k < j ) then
            kx = kx + incx
          end if
        end do
      end if
    else
      if ( incx == 1 ) then
        do j = n, 1, -1
          temp = x(j)
          l = 1      - j
          do i = min ( n, j + k ), j + 1, -1
            temp = temp - a(l+i,j) * x(i)
          end do
          if ( nounit ) then
            temp = temp / a(1,j)
          end if
          x(j) = temp
        end do
      else
        kx = kx + ( n - 1 ) * incx
        jx = kx
        do j = n, 1, -1
          temp = x(jx)
          ix = kx
          l = 1 - j
          do i = min ( n, j + k ), j + 1, -1
            temp = temp - a(l+i,j) * x(ix)
            ix = ix   - incx
          end do
          if ( nounit ) then
            temp = temp / a(1,j)
          end if
          x(jx) = temp
          jx = jx   - incx
          if ( k <= ( n - j ) ) then
            kx = kx - incx
          end if
        end do
      end if
    end if
  end if

  return
end
subroutine dtpmv ( uplo, trans, diag, n, ap, x, incx )

!*****************************************************************************80
!
!! DTPMV computes x := A*x or x = A'*x for a packed triangular matrix A.
!
!  Discussion:
!
!    DTPMV performs one of the matrix-vector operations
!
!      x := A*x,   or   x := A'*x,
!
!    where x is an n element vector and  A is an n by n unit, or non-unit,
!    upper or lower triangular matrix, supplied in packed form.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the matrix is an upper or
!    lower triangular matrix as follows:
!    'u' or 'U': A is an upper triangular matrix.
!    'l' or 'L': A is a lower triangular matrix.
!
!    Input, character TRANS, specifies the operation to be performed:
!    'n' or 'N': x := A*x.
!    't' or 'T': x := A'*x.
!    'c' or 'C': x := A'*x.
!
!    Input, character DIAG, specifies whether A is unit triangular:
!    'u' or 'U'   A is assumed to be unit triangular.
!    'n' or 'N'   A is not assumed to be unit triangular.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, real ( kind = 8 ) A((n*(n+1))/2).
!    Before entry with  UPLO = 'u' or 'U', the array AP must
!    contain the upper triangular matrix packed sequentially,
!    column by column, so that AP(1) contains a(1,1),
!    AP(2) and AP(3) contain a(1,2) and a(2,2)
!    respectively, and so on.
!    Before entry with UPLO = 'l' or 'L', the array AP must
!    contain the lower triangular matrix packed sequentially,
!    column by column, so that AP(1) contains a(1,1),
!    AP(2) and AP(3) contain a(2,1) and a(3,1)
!    respectively, and so on.
!    Note that when DIAG = 'u' or 'U', the diagonal elements of
!    A are not referenced, but are assumed to be unity.
!
!    Input/output, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n
!    element vector x. On exit, X is overwritten with the
!    tranformed vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
  implicit none

  real ( kind = 8 ) ap(*)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) kx
  logical lsame
  integer ( kind = 4 ) n
  logical nounit
  real ( kind = 8 ) temp
  character trans
  character uplo
  real ( kind = 8 ) x(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo , 'U' ) .and.  &
       .not. lsame ( uplo , 'L' ) ) then
    info = 1
  else if ( .not. lsame ( trans, 'N' ) .and.  &
            .not. lsame ( trans, 'T' ) .and.  &
            .not. lsame ( trans, 'C' ) ) then
    info = 2
  else if ( .not. lsame ( diag , 'U' ) .and.  &
            .not. lsame ( diag , 'N' ) ) then
    info = 3
  else if ( n < 0 ) then
    info = 4
  else if ( incx == 0 ) then
    info = 7
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DTPMV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  end if

  nounit = lsame ( diag, 'N' )
!
!  Set up the start point in X if the increment is not unity. This
!  will be  ( N - 1 ) * INCX too small for descending loops.
!
  if ( incx <= 0 ) then
    kx = 1 - ( n - 1 ) * incx
  else if ( incx /= 1 ) then
    kx = 1
  end if
!
!  Start the operations. In this version the elements of AP are
!  accessed sequentially with one pass through AP.
!
  if ( lsame ( trans, 'N' ) ) then
!
!  Form x:= A*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      kk = 1
      if ( incx == 1 ) then
        do j = 1, n
          if ( x(j) /= 0.0D+00 ) then
            temp = x(j)
            k = kk
            do i = 1, j - 1
              x(i) = x(i) + temp * ap(k)
              k = k + 1
            end do
            if ( nounit ) then
              x(j) = x(j) * ap(kk+j-1)
            end if
          end if
          kk = kk + j
        end do
      else
        jx = kx
        do j = 1, n
          if ( x(jx) /= 0.0D+00 ) then
            temp = x(jx)
            ix = kx
            do k = kk, kk + j - 2
              x(ix) = x(ix) + temp * ap(k)
              ix = ix + incx
            end do
            if ( nounit ) then
              x(jx) = x(jx) * ap(kk+j-1)
            end if
          end if
          jx = jx + incx
          kk = kk + j
        end do
      end if
    else
      kk = ( n * ( n + 1 ) ) / 2
      if ( incx == 1 ) then
        do j = n, 1, -1
          if ( x(j) /= 0.0D+00 ) then
            temp = x(j)
            k = kk
            do i = n, j + 1, -1
              x(i) = x(i) + temp * ap(k)
              k = k - 1
            end do
            if ( nounit ) then
              x(j) = x(j) * ap(kk-n+j)
            end if
          end if
          kk = kk - ( n - j + 1 )
        end do
      else
        kx = kx + ( n - 1 ) * incx
        jx = kx
        do j = n, 1, -1
          if ( x(jx) /= 0.0D+00 ) then
            temp = x(jx)
            ix = kx
            do k = kk, kk - ( n - ( j + 1 ) ), -1
              x(ix) = x(ix) + temp * ap(k)
              ix = ix - incx
            end do
            if ( nounit ) then
              x(jx) = x(jx) * ap(kk-n+j)
            end if
          end if
          jx = jx - incx
          kk = kk - ( n - j + 1 )
        end do
      end if
    end if
  else
!
!  Form  x := A'*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      kk = ( n * ( n + 1 ) ) / 2
      if ( incx == 1 ) then
        do j = n, 1, -1
          temp = x(j)
          if ( nounit ) then
            temp = temp * ap(kk)
          end if
          k = kk - 1
          do i = j - 1, 1, -1
            temp = temp + ap(k) * x(i)
            k = k    - 1
          end do
          x(j) = temp
          kk = kk - j
        end do
      else
        jx = kx + ( n - 1 ) * incx
        do j = n, 1, -1
          temp = x(jx)
          ix = jx
          if ( nounit ) then
            temp = temp * ap(kk)
          end if
          do k = kk - 1, kk - j + 1, -1
            ix = ix   - incx
            temp = temp + ap(k) * x(ix)
          end do
          x(jx) = temp
          jx = jx - incx
          kk = kk - j
        end do
      end if
    else
      kk = 1
      if ( incx == 1 ) then
        do j = 1, n
          temp = x(j)
          if ( nounit ) then
            temp = temp * ap(kk)
          end if
          k = kk + 1
          do i = j + 1, n
            temp = temp + ap(k) * x(i)
            k = k + 1
          end do
          x(j) = temp
          kk = kk + ( n - j + 1 )
        end do
      else
        jx = kx
        do j = 1, n
          temp = x(jx)
          ix = jx
          if ( nounit ) then
            temp = temp * ap(kk)
          end if
          do k = kk + 1, kk + n - j
            ix = ix + incx
            temp = temp + ap(k) * x(ix)
          end do
          x(jx) = temp
          jx = jx + incx
          kk = kk + ( n - j + 1 )
        end do
      end if
    end if
  end if

  return
end
subroutine dtpsv ( uplo, trans, diag, n, ap, x, incx )

!*****************************************************************************80
!
!! DTPSV solves A*x = b or A'*x = b for a triangular packed matrix A.
!
!  Discussion:
!
!    DTPSV solves one of the systems of equations
!
!      A*x = b,   or   A'*x = b,
!
!    where b and x are n element vectors and A is an n by n unit, or
!    non-unit, upper or lower triangular matrix, supplied in packed form.
!
!    No test for singularity or near-singularity is included in this
!    routine. Such tests must be performed before calling this routine.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the matrix is upper or lower
!    triangular:
!    'u' or 'U': A is upper triangular.
!    'l' or 'L': A is lower triangular.
!
!    Input, character TRANS, specifies the equations to be solved:
!    'n' or 'N'   A*x = b.
!    't' or 'T'   A'*x = b.
!    'c' or 'C'   A'*x = b.
!
!    Input, character DIAG, specifies whether A is unit triangular:
!    'u' or 'U'   A is assumed to be unit triangular.
!    'n' or 'N'   A is not assumed to be unit triangular.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, real ( kind = 8 ) A((n*(n+1))/2).
!    Before entry with  UPLO = 'u' or 'U', the array AP must
!    contain the upper triangular matrix packed sequentially,
!    column by column, so that AP(1) contains a(1,1),
!    AP(2) and AP(3) contain a(1,2) and a(2,2)
!    respectively, and so on.
!    Before entry with UPLO = 'l' or 'L', the array AP must
!    contain the lower triangular matrix packed sequentially,
!    column by column, so that AP(1) contains a(1,1),
!    AP(2) and AP(3) contain a(2,1) and a(3,1)
!    respectively, and so on.
!    Note that when DIAG = 'u' or 'U', the diagonal elements of
!    A are not referenced, but are assumed to be unity.
!
!    Input/output, real ( kind = 8 ) X(1+(n-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n
!    element right-hand side vector b. On exit, X is overwritten
!    with the solution vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
  implicit none

  real ( kind = 8 ) ap(*)
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) kx
  logical lsame
  integer ( kind = 4 ) n
  logical nounit
  real ( kind = 8 ) temp
  character trans
  character uplo
  real ( kind = 8 ) x(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo, 'U' ) .and.  &
       .not. lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( .not. lsame ( trans, 'N' ) .and.  &
          .not. lsame ( trans, 'T' ) .and.  &
          .not. lsame ( trans, 'C' ) ) then
    info = 2
  else if ( .not. lsame ( diag, 'U' ) .and.  &
         .not. lsame ( diag, 'N' ) ) then
    info = 3
  else if ( n < 0 ) then
    info = 4
  else if ( incx == 0 ) then
    info = 7
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DTPSV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  end if

  nounit = lsame ( diag, 'N' )
!
!  Set up the start point in X if the increment is not unity. This
!  will be  ( N - 1 ) * INCX  too small for descending loops.
!
  if ( incx <= 0 ) then
    kx = 1 - ( n - 1 ) * incx
  else if ( incx /= 1 ) then
    kx = 1
  end if
!
!  Start the operations. In this version the elements of AP are
!  accessed sequentially with one pass through AP.
!
  if ( lsame ( trans, 'N' ) ) then
!
!  Form  x := inv( A )*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      kk = ( n * ( n + 1 ) ) / 2
      if ( incx == 1 ) then
        do j = n, 1, -1
          if ( x(j) /= 0.0D+00 ) then
            if ( nounit ) then
              x(j) = x(j) / ap(kk)
            end if
            temp = x(j)
            k = kk - 1
            do i = j - 1, 1, -1
              x(i) = x(i) - temp * ap(k)
              k = k - 1
            end do
          end if
          kk = kk - j
        end do
      else
        jx = kx + ( n - 1 ) * incx
        do j = n, 1, -1
          if ( x(jx) /= 0.0D+00 ) then
            if ( nounit ) then
              x(jx) = x(jx) / ap(kk)
            end if
            temp = x(jx)
            ix = jx
            do k = kk - 1, kk - j + 1, -1
              ix = ix - incx
              x(ix) = x(ix) - temp * ap(k)
            end do
          end if
          jx = jx - incx
          kk = kk - j
        end do
      end if
    else
      kk = 1
      if ( incx == 1 ) then
        do j = 1, n
          if ( x(j) /= 0.0D+00 ) then
            if ( nounit ) then
              x(j) = x(j) / ap(kk)
            end if
            temp = x(j)
            k = kk + 1
            do i = j + 1, n
              x(i) = x(i) - temp * ap(k)
              k = k + 1
            end do
          end if
          kk = kk + ( n - j + 1 )
        end do
      else
        jx = kx
        do j = 1, n
          if ( x(jx) /= 0.0D+00 ) then
            if ( nounit ) then
              x(jx) = x(jx) / ap(kk)
            end if
            temp = x(jx)
            ix = jx
            do k = kk + 1, kk + n - j
              ix = ix + incx
              x(ix) = x(ix) - temp * ap(k)
            end do
          end if
          jx = jx + incx
          kk = kk + ( n - j + 1 )
        end do
      end if
    end if
  else
!
!  Form  x := inv( A' )*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      kk = 1
      if ( incx == 1 ) then
        do j = 1, n
          temp = x(j)
          k = kk
          do i = 1, j - 1
            temp = temp - ap(k) * x(i)
            k = k + 1
          end do
          if ( nounit ) then
            temp = temp / ap(kk+j-1)
          end if
          x(j) = temp
          kk = kk + j
        end do
      else
        jx = kx
        do j = 1, n
          temp = x(jx)
          ix = kx
          do k = kk, kk + j - 2
            temp = temp - ap(k) * x(ix)
            ix = ix + incx
          end do
          if ( nounit ) then
            temp = temp / ap(kk+j-1)
          end if
          x(jx) = temp
          jx = jx + incx
          kk = kk + j
        end do
      end if
    else
      kk = ( n * ( n + 1 ) ) / 2
      if ( incx == 1 ) then
        do j = n, 1, -1
          temp = x(j)
          k = kk
          do i = n, j + 1, -1
            temp = temp - ap(k) * x(i)
            k = k - 1
          end do
          if ( nounit ) then
            temp = temp / ap(kk-n+j)
          end if
          x(j) = temp
          kk = kk - ( n - j + 1 )
        end do
      else
        kx = kx + ( n - 1 ) * incx
        jx = kx
        do j = n, 1, -1
          temp = x(jx)
          ix = kx
          do k = kk, kk - ( n - ( j + 1 ) ), -1
            temp = temp - ap(k) * x(ix)
            ix = ix - incx
          end do
          if ( nounit ) then
            temp = temp / ap(kk-n+j)
          end if
          x(jx) = temp
          jx = jx - incx
          kk = kk - ( n - j + 1 )
        end do
      end if
    end if
  end if

  return
end
subroutine dtrmv ( uplo, trans, diag, n, a, lda, x, incx )

!*****************************************************************************80
!
!! DTRMV computes x: = A*x or x = A'*x for a triangular matrix A.
!
!  Discussion:
!
!    DTRMV performs one of the matrix-vector operations
!
!      x := A*x,   or   x := A'*x,
!
!    where x is an n element vector and  A is an n by n unit, or non-unit,
!    upper or lower triangular matrix.
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
!    Input, character UPLO, specifies whether the matrix is an upper or
!    lower triangular matrix as follows:
!    'u' or 'U': A is an upper triangular matrix.
!    'l' or 'L': A is a lower triangular matrix.
!
!    Input, character TRANS, specifies the operation to be performed as
!    follows:
!    'n' or 'N': x := A*x.
!    't' or 'T': x := A'*x.
!    'c' or 'C': x := A'*x.
!
!    Input, character DIAG, specifies whether or not A is unit
!    triangular as follows:
!    'u' or 'U': A is assumed to be unit triangular.
!    'n' or 'N': A is not assumed to be unit triangular.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!    0 <= N.
!
!    Input, real ( kind = 8 ) A(LDA,N).
!    Before entry with  UPLO = 'u' or 'U', the leading n by n
!    upper triangular part of the array A must contain the upper
!    triangular matrix and the strictly lower triangular part of
!    A is not referenced.
!    Before entry with UPLO = 'l' or 'L', the leading n by n
!    lower triangular part of the array A must contain the lower
!    triangular matrix and the strictly upper triangular part of
!    A is not referenced.
!    Note that when  DIAG = 'u' or 'U', the diagonal elements of
!    A are not referenced either, but are assumed to be unity.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program. max ( 1, N ) <= LDA.
!
!    Input/output, real ( kind = 8 ) X(1+(N-1)*abs( INCX)).
!    Before entry, the incremented array X must contain the n
!    element vector x. On exit, X is overwritten with the
!    tranformed vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for the elements of
!    X.  INCX must not be zero.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a( lda, * )
  character diag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) kx
  integer ( kind = 4 ) n
  logical nounit
  logical, external :: lsame
  real ( kind = 8 ) temp
  character trans
  character uplo
  real ( kind = 8 ) x( * )
  external xerbla
  intrinsic max
!
!  Test the input parameters.
!
  info = 0
  if  ( .not. lsame ( uplo, 'U' ) .and.  &
        .not. lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( .not. lsame ( trans, 'N' ) .and.  &
            .not. lsame ( trans, 'T' ) .and.  &
            .not. lsame ( trans, 'C' ) ) then
    info = 2
  else if ( .not. lsame ( diag, 'U' ) .and.  &
            .not. lsame ( diag, 'N' ) ) then
    info = 3
  else if ( n < 0 ) then
    info = 4
  else if ( lda < max ( 1, n ) ) then
    info = 6
  else if ( incx == 0 ) then
    info = 8
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DTRMV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  end if

  nounit = lsame ( diag, 'N' )
!
!  Set up the start point in X if the increment is not unity. This
!  will be  ( N - 1 ) * INCX  too small for descending loops.
!
  if ( incx <= 0 ) then
    kx = 1 - ( n - 1 ) * incx
  else if ( incx /= 1 ) then
    kx = 1
  end if
!
!  Start the operations. In this version the elements of A are
!  accessed sequentially with one pass through A.
!
  if ( lsame ( trans, 'N' ) ) then
!
!  Form x := A*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      if ( incx == 1 ) then
        do j = 1, n
          if ( x(j) /= 0.0D+00 ) then
            temp = x(j)
            do i = 1, j - 1
              x(i) = x(i) + temp * a(i,j)
            end do
            if ( nounit ) then
              x(j) = x(j) * a(j,j)
            end if
          end if
        end do
      else
        jx = kx
        do j = 1, n
          if ( x(jx) /= 0.0D+00 ) then
            temp = x(jx)
            ix = kx
            do i = 1, j - 1
              x(ix) = x(ix) + temp * a(i,j)
              ix = ix + incx
            end do
            if ( nounit ) then
              x(jx) = x(jx) * a(j,j)
            end if
          end if
          jx = jx + incx
        end do
      end if
    else
      if ( incx == 1 ) then
        do j = n, 1, -1
          if ( x(j) /= 0.0D+00 ) then
            temp = x(j)
            do i = n, j + 1, -1
              x(i) = x(i) + temp * a(i,j)
            end do
            if ( nounit ) then
              x(j) = x(j) * a(j,j)
            end if
          end if
        end do
      else
        kx = kx + ( n - 1 ) * incx
        jx = kx
        do j = n, 1, -1
          if ( x(jx) /= 0.0D+00 ) then
            temp = x(jx)
            ix = kx
            do i = n, j + 1, -1
              x(ix) = x(ix) + temp * a(i,j)
              ix = ix - incx
            end do
            if ( nounit ) then
              x(jx) = x(jx) * a(j,j)
            end if
          end if
          jx = jx - incx
        end do
      end if
    end if
  else
!
!  Form x := A'*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      if ( incx == 1 ) then
        do j = n, 1, -1
          temp = x(j)
          if ( nounit ) then
            temp = temp * a(j,j)
          end if
          do i = j - 1, 1, -1
            temp = temp + a(i,j) * x(i)
          end do
          x(j) = temp
        end do
      else
        jx = kx + ( n - 1 ) * incx
        do j = n, 1, -1
          temp = x(jx)
          ix = jx
          if ( nounit ) then
            temp = temp * a(j,j)
          end if
          do i = j - 1, 1, -1
            ix = ix   - incx
            temp = temp + a(i,j) * x(ix)
          end do
          x(jx) = temp
          jx = jx - incx
        end do
      end if
    else
      if ( incx == 1 ) then
        do j = 1, n
          temp = x(j)
          if ( nounit ) then
            temp = temp * a(j,j)
          end if
          do i = j + 1, n
            temp = temp + a(i,j) * x(i)
          end do
          x(j) = temp
        end do
      else
        jx = kx
        do j = 1, n
          temp = x(jx)
          ix = jx
          if ( nounit ) then
            temp = temp * a(j,j)
          end if
          do i = j + 1, n
            ix = ix + incx
            temp = temp + a(i,j) * x(ix)
          end do
          x(jx) = temp
          jx = jx + incx
        end do
      end if
    end if
  end if

  return
end
subroutine dtrsv ( uplo, trans, diag, n, a, lda, x, incx )

!*****************************************************************************80
!
!! DTRSV solves A*x = b or A'*x = b for triangular matrix A.
!
!  Discussion:
!
!    DTRSV solves one of the systems of equations
!
!      A*x = b,   or   A'*x = b,
!
!    where b and x are n element vectors and A is an n by n unit, or
!    non-unit, upper or lower triangular matrix.
!
!    No test for singularity or near-singularity is included in this
!    routine. Such tests must be performed before calling this routine.
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
!    Original FORTRAN77 version by Jack Dongarra,  Jeremy Du Croz,  
!    Sven Hammarling,  Richard Hanson.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character UPLO, specifies whether the matrix A is upper or lower
!    triangular:
!    'u' or 'U': A is upper triangular.
!    'l' or 'L': A is lower triangular.
!
!    Input, character TRANS, specifies the equations to be solved:
!    'n' or 'N'   A*x = b.
!    't' or 'T'   A'*x = b.
!    'c' or 'C'   A'*x = b.
!
!    Input, character DIAG, specifies whether A is unit triangular:
!    'u' or 'U'   A is assumed to be unit triangular.
!    'n' or 'N'   A is not assumed to be unit triangular.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of the 
!    matrix.  0 <= N.
!
!    Input, real ( kind = 8 ) A(LDA,N).
!    Before entry with  UPLO = 'u' or 'U', the leading n by n
!    upper triangular part of the array A must contain the upper
!    triangular matrix and the strictly lower triangular part of
!    A is not referenced.
!    Before entry with UPLO = 'l' or 'L', the leading n by n
!    lower triangular part of the array A must contain the lower
!    triangular matrix and the strictly upper triangular part of
!    A is not referenced.
!    Note that when  DIAG = 'u' or 'U', the diagonal elements of
!    A are not referenced either, but are assumed to be unity.
!
!    Input, integer ( kind = 4 ) LDA, the first dimension of A as declared
!    in the calling program. max ( 1, N ) <= LDA.
!
!    Input/output, real ( kind = 8 ) X(1+(N-1)*abs(INCX)).
!    Before entry, the incremented array X must contain the n
!    element right-hand side vector b.  On exit, X is overwritten
!    with the solution vector x.
!
!    Input, integer ( kind = 4 ) INCX, the increment for elements of X.
!    INCX must not be zero.
!
  implicit none

  integer ( kind = 4 ) lda

  real ( kind = 8 ) a(lda,*)
  character diag
  integer ( kind = 4 )  i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jx
  integer ( kind = 4 ) kx
  logical lsame
  integer ( kind = 4 ) n
  logical nounit
  real ( kind = 8 ) temp
  character trans
  character uplo
  real ( kind = 8 ) x(*)
!
!  Test the input parameters.
!
  info = 0
  if ( .not. lsame ( uplo, 'U' ) .and.  &
       .not. lsame ( uplo, 'L' ) ) then
    info = 1
  else if ( .not. lsame ( trans, 'N' ) .and.  &
            .not. lsame ( trans, 'T' ) .and.  &
            .not. lsame ( trans, 'C' ) ) then
    info = 2
  else if ( .not. lsame ( diag, 'U' ) .and.  &
            .not. lsame ( diag, 'N' ) ) then
    info = 3
  else if ( n < 0 ) then
    info = 4
  else if ( lda < max ( 1, n ) ) then
    info = 6
  else if ( incx == 0 ) then
    info = 8
  end if

  if ( info /= 0 ) then
    call xerbla ( 'DTRSV', info )
    return
  end if
!
!  Quick return if possible.
!
  if ( n == 0 ) then
    return
  end if

  nounit = lsame ( diag, 'N' )
!
!  Set up the start point in X if the increment is not unity. This
!  will be  ( N - 1 ) * INCX too small for descending loops.
!
  if ( incx <= 0 ) then
    kx = 1 - ( n - 1 ) * incx
  else if ( incx /= 1 ) then
    kx = 1
  end if
!
!  Start the operations.  In this version the elements of A are
!  accessed sequentially with one pass through A.
!
  if ( lsame ( trans, 'N' ) ) then
!
!  Form x := inv( A )*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      if ( incx == 1 ) then
        do j = n, 1, -1
          if ( x(j) /= 0.0D+00 ) then
            if ( nounit ) then
              x(j) = x(j) / a(j,j)
            end if
            temp = x(j)
            do i = j - 1, 1, -1
              x(i) = x(i) - temp * a(i,j)
            end do
          end if
        end do
      else
        jx = kx + ( n - 1 ) * incx
        do j = n, 1, -1
          if ( x(jx) /= 0.0D+00 ) then
            if ( nounit ) then
              x(jx) = x(jx) / a(j,j)
            end if
            temp = x(jx)
            ix = jx
            do i = j - 1, 1, -1
              ix = ix - incx
              x(ix) = x(ix) - temp * a(i,j)
            end do
          end if
          jx = jx - incx
        end do
      end if
    else
      if ( incx == 1 ) then
        do j = 1, n
          if ( x(j) /= 0.0D+00 ) then
            if ( nounit ) then
              x(j) = x(j) / a(j,j)
            end if
            temp = x(j)
            do i = j + 1, n
              x(i) = x(i) - temp * a(i,j)
            end do
          end if
        end do
      else
        jx = kx
        do j = 1, n
          if ( x(jx) /= 0.0D+00 ) then
            if ( nounit ) then
              x(jx) = x(jx) / a(j,j)
            end if
            temp = x(jx)
            ix = jx
            do i = j + 1, n
              ix = ix + incx
              x(ix) = x(ix) - temp * a(i,j)
            end do
          end if
          jx = jx + incx
        end do
      end if
    end if
  else
!
!  Form  x := inv( A' )*x.
!
    if ( lsame ( uplo, 'U' ) ) then
      if ( incx == 1 ) then
        do j = 1, n
          temp = x(j)
          do i = 1, j - 1
            temp = temp - a(i,j) * x(i)
          end do
          if ( nounit ) then
            temp = temp / a(j,j)
          end if
          x(j) = temp
        end do
      else
        jx = kx
        do j = 1, n
          temp = x(jx)
          ix = kx
          do i = 1, j - 1
            temp = temp - a(i,j) * x(ix)
            ix = ix + incx
          end do
          if ( nounit ) then
            temp = temp / a(j,j)
          end if
          x(jx) = temp
          jx = jx + incx
        end do
      end if
    else
      if ( incx == 1 ) then
        do j = n, 1, -1
          temp = x(j)
          do i = n, j + 1, -1
            temp = temp - a(i,j) * x(i)
          end do
          if ( nounit ) then
            temp = temp / a(j,j)
          end if
          x(j) = temp
        end do
      else
        kx = kx + ( n - 1 ) * incx
        jx = kx
        do j = n, 1, -1
          temp = x(jx)
          ix = kx
          do i = n, j + 1, -1
            temp = temp - a(i,j) * x(ix)
            ix = ix - incx
          end do
          if ( nounit ) then
            temp = temp / a(j,j)
          end if
          x(jx) = temp
          jx = jx - incx
        end do
      end if
    end if
  end if

  return
end
