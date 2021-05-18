program main

!*****************************************************************************80
!
!! MAIN is the main program for LINPACK_BENCH.
!
!  Discussion:
!
!    LINPACK_BENCH drives the double precision LINPACK benchmark program.
!
!  Modified:
!
!    07 March 2008
!
!  Reference:
!
!    Jack Dongarra,
!    Performance of Various Computers Using Standard Linear Equations Software,
!    Technical Report CS-89-85,
!    Electrical Engineering and Computer Science Department,
!    University of Tennessee, 2008.
!
!  Parameters:
!
!    N is the problem size.
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1000
  integer ( kind = 4 ), parameter :: lda = n + 1

  real ( kind = 8 ), dimension ( lda, n ) :: a
  real ( kind = 8 ) a_max
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) b_max
  real ( kind = 8 ), parameter :: cray = 0.056D+00
  real ( kind = 8 ) eps
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  real ( kind = 8 ) ops
  real ( kind = 8 ) resid(n)
  real ( kind = 8 ) resid_max
  real ( kind = 8 ) residn
  real ( kind = 8 ) rhs(n)
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) time(6)
  real ( kind = 8 ) total
  real ( kind = 8 ) x(n)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINPACK_BENCH'
  write ( *, '(a)' ) '  The LINPACK benchmark.'
  write ( *, '(a)' ) '  Language: FORTRAN90'
  write ( *, '(a)' ) '  Datatype: Real Double Precision'
  write ( *, '(a,i8)' ) '  Matrix order N =               ', n
  write ( *, '(a,i8)' ) '  Leading matrix dimension LDA = ', lda

  ops = real ( 2 * n * n * n, kind = 8 ) / 3.0D+00 &
    + 2.0D+00 * real ( n * n, kind = 8 )

  call d_matgen ( a, lda, n )

  a_max = maxval ( maxval ( a(1:n,1:n), dim = 2 ), dim = 1 )

  x(1:n) = 1.0D+00
  b(1:n) = matmul ( a(1:n,1:n), x(1:n) )

  call cpu_time ( t1 )

  call dgefa ( a, lda, n, ipvt, info )

  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LINPACK_BENCH - Fatal error!'
    write ( *, '(a)' ) '  The matrix A is apparently singular.'
    write ( *, '(a)' ) '  Abnormal end of execution.'
    stop 1
  end if

  call cpu_time ( t2 )
  time(1) = t2 - t1

  call cpu_time ( t1 )

  call dgesl ( a, lda, n, ipvt, b, 0 )

  call cpu_time ( t2 )
  time(2) = t2 - t1

  total = time(1) + time(2)
!
!  Compute a residual to verify results.
!
  call d_matgen ( a, lda, n )
  x(1:n) = 1.0D+00
  rhs(1:n) = matmul ( a(1:n,1:n), x(1:n) )

  resid(1:n) = matmul(a(1:n,1:n), b(1:n) ) - rhs(1:n)

  resid_max = maxval ( abs ( resid(1:n) ) )
  b_max = maxval ( abs ( b(1:n) ) ) 

  eps = epsilon ( eps )

  residn = resid_max /  ( n * a_max * b_max * eps )

  time(3) = total
  time(4) = ops / ( 1.0D+06 * total )
  time(5) = 2.0D+00 / time(4)
  time(6) = total / cray

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     Norm. Resid      Resid           MACHEP' // &
    '         X(1)          X(N)'
  write ( *, '(a)' ) ' '
  write ( *, '(1p5e16.8)'  ) residn, resid_max, eps, b(1), b(n)
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '      Factor     Solve      Total     MFLOPS       Unit      Ratio'
  write ( *, '(a)' ) ' '
  write ( *, '(6(1pe11.3))' ) time(1:6)
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LINPACK_BENCH'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine d_matgen ( a, lda, n )

!*****************************************************************************80
!
!! D_MATGEN generates a random matrix.
!
!  Modified:
!
!    01 April 2003
!
!  Parameters:
!
!    Output, real ( kind = 8 ) A(LDA,N), the N by N matrix.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of the matrix.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) d_random
  integer ( kind = 4 ) i
  integer ( kind = 4 ) init(4)
  integer ( kind = 4 ) j

  init(1:4) = (/ 1, 2, 3, 1325 /)

  do j = 1, n
     do i = 1, n
       a(i,j) = d_random ( init ) - 0.5D+00
    end do
  end do

  return
end
function d_random ( iseed )

!*****************************************************************************80
!
!! D_RANDOM returns a uniformly distributed random number between 0 and 1.
!
!  Discussion:
!
!    This routine uses a multiplicative congruential method with modulus
!    2**48 and multiplier 33952834046453.
!
!    48-bit integers are stored in 4 integer array elements with 12 bits
!    per element.  Hence the routine is portable across machines with
!    integer ( kind = 4 )s of 32 bits or more.
!
!  Reference:
!
!    GS Fishman,
!    Multiplicative congruential random number generators with modulus
!    2**b: an exhaustive analysis for b = 32 and a partial analysis for b = 48,
!    Mathematics of Computation,
!    Volume 189, 1990, pages 331-344.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) ISEED(4).
!    On entry, the seed of the random number generator; the array
!    elements must be between 0 and 4095, and ISEED(4) must be odd.
!    On exit, the seed is updated.
!
!    Output, real ( kind = 8 ) D_RANDOM, the next pseudorandom number.
!
  implicit none

  real ( kind = 8 ) d_random
  integer ( kind = 4 ), parameter :: ipw2 = 4096
  integer ( kind = 4 ) iseed(4)
  integer ( kind = 4 ) it1
  integer ( kind = 4 ) it2
  integer ( kind = 4 ) it3
  integer ( kind = 4 ) it4
  integer ( kind = 4 ), parameter :: m1 = 494
  integer ( kind = 4 ), parameter :: m2 = 322
  integer ( kind = 4 ), parameter :: m3 = 2508
  integer ( kind = 4 ), parameter :: m4 = 2549
  real ( kind = 8 ) , parameter :: one = 1.0D+00
  real ( kind = 8 ) , parameter :: r = 1.0D+00 / 4096.0D+00
!
!  Multiply the seed by the multiplier modulo 2**48.
!
  it4 = iseed(4) * m4
  it3 = it4 / ipw2
  it4 = it4 - ipw2 * it3
  it3 = it3 + iseed(3) * m4 + iseed(4) * m3
  it2 = it3 / ipw2
  it3 = it3 - ipw2 * it2
  it2 = it2 + iseed(2) * m4 + iseed(3) * m3 + iseed(4) * m2
  it1 = it2 / ipw2
  it2 = it2 - ipw2 * it1
  it1 = it1 + iseed(1) * m4 + iseed(2) * m3 + iseed(3) * m2 + iseed(4) * m1
  it1 = mod ( it1, ipw2 )
!
!  Return updated seed
!
  iseed(1) = it1
  iseed(2) = it2
  iseed(3) = it3
  iseed(4) = it4
!
!  Convert 48-bit integer to a real number in the interval (0,1)
!
  d_random = &
      r * ( real ( it1, kind = 8 ) &
    + r * ( real ( it2, kind = 8 ) &
    + r * ( real ( it3, kind = 8 ) &
    + r * ( real ( it4, kind = 8 ) ) ) ) )

  return
end
subroutine daxpy ( n, sa, x, incx, y, incy )

!*****************************************************************************80
!
!! DAXPY adds a constant times one vector to another.
!
!  Modified:
!
!    08 April 1999
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Algorithm 539: 
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) SA, the multiplier.
!
!    Input, real ( kind = 8 ) X(*), the vector to be scaled and added to Y.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive entries of X.
!
!    Input/output, real ( kind = 8 ) Y(*), the vector to which a 
!    multiple of X is to be added.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive entries of Y.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  real ( kind = 8 ) sa
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) y(*)

  if ( n <= 0 ) then

  else if ( sa == 0.0D+00 ) then

  else if ( incx == 1 .and. incy == 1 ) then

    y(1:n) = y(1:n) + sa * x(1:n)

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    do i = 1, n
      y(iy) = y(iy) + sa * x(ix)
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  return
end
subroutine dgefa ( a, lda, n, ipvt, info )

!*****************************************************************************80
!
!! DGEFA factors a real matrix.
!
!  Modified:
!
!    07 March 2001
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) A(LDA,N).
!    On intput, the matrix to be factored.
!    On output, an upper triangular matrix and the multipliers used to obtain
!    it.  The factorization can be written A=L*U, where L is a product of
!    permutation and unit lower triangular matrices, and U is upper triangular.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Output, integer ( kind = 4 ) IPVT(N), the pivot indices.
!
!    Output, integer ( kind = 4 ) INFO, singularity indicator.
!    0, normal value.
!    K, if U(K,K) == 0.  This is not an error condition for this subroutine,
!    but it does indicate that DGESL or DGEDI will divide by zero if called.
!    Use RCOND in DGECO for a reliable indication of singularity.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(lda,n)
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) idamax
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) t
!
!  Gaussian elimination with partial pivoting.
!
  info = 0

  do k = 1, n - 1
!
!  Find L = pivot index.
!
    l = idamax ( n-k+1, a(k,k), 1 ) + k - 1
    ipvt(k) = l
!
!  Zero pivot implies this column already triangularized.
!
    if ( a(l,k) == 0.0D+00 ) then
      info = k
      cycle
    end if
!
!  Interchange if necessary.
!
    if ( l /= k ) then
      t      = a(l,k)
      a(l,k) = a(k,k)
      a(k,k) = t
    end if
!
!  Compute multipliers.
!
    a(k+1:n,k) = - a(k+1:n,k) / a(k,k)
!
!  Row elimination with column indexing.
!
    do j = k+1, n
      t = a(l,j)
      if ( l /= k ) then
        a(l,j) = a(k,j)
        a(k,j) = t
      end if
      call daxpy ( n-k, t, a(k+1,k), 1, a(k+1,j), 1 )
    end do

  end do

  ipvt(n) = n

  if ( a(n,n) == 0.0D+00 ) then
    info = n
  end if

  return
end
subroutine dgesl ( a, lda, n, ipvt, b, job )

!*****************************************************************************80
!
!! DGESL solves a real general linear system A * X = B.
!
!  Discussion:
!
!    DGESL can solve either of the systems A * X = B or A' * X = B.
!
!    The system matrix must have been factored by DGECO or DGEFA.
!
!    A division by zero will occur if the input factor contains a
!    zero on the diagonal.  Technically this indicates singularity
!    but it is often caused by improper arguments or improper
!    setting of LDA.  It will not occur if the subroutines are
!    called correctly and if DGECO has set 0.0 < RCOND 
!    or DGEFA has set INFO == 0.
!
!  Modified:
!
!    07 March 2001
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(LDA,N), the output from DGECO or DGEFA.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, integer ( kind = 4 ) IPVT(N), the pivot vector from DGECO or DGEFA.
!
!    Input/output, real ( kind = 8 ) B(N).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Input, integer ( kind = 4 ) JOB.
!    0, solve A * X = B;
!    nonzero, solve A' * X = B.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) job
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) t
!
!  Solve A * X = B.
!
  if ( job == 0 ) then

    do k = 1, n-1

      l = ipvt(k)
      t = b(l)

      if ( l /= k ) then
        b(l) = b(k)
        b(k) = t
      end if

      call daxpy ( n-k, t, a(k+1,k), 1, b(k+1), 1 )

    end do

    do k = n, 1, -1
      b(k) = b(k) / a(k,k)
      t = -b(k)
      call daxpy ( k-1, t, a(1,k), 1, b(1), 1 )
    end do

  else
!
!  Solve A' * X = B.
!
    do k = 1, n
      t = dot_product ( a(1:k-1,k), b(1:k-1) )
      b(k) = ( b(k) - t ) / a(k,k)
    end do

    do k = n-1, 1, -1

      b(k) = b(k) + dot_product ( a(k+1:n,k), b(k+1:n) )
      l = ipvt(k)

      if ( l /= k ) then
        t    = b(l)
        b(l) = b(k)
        b(k) = t
      end if

    end do

  end if

  return
end
function idamax ( n, x, incx )

!*****************************************************************************80
!
!! IDAMAX finds the index of the vector element of maximum absolute value.
!
!  Modified:
!
!    08 April 1999
!
!  Reference:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Algorithm 539: 
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!    Dongarra, Moler, Bunch and Stewart,
!    LINPACK User's Guide,
!    SIAM, (Society for Industrial and Applied Mathematics),
!    3600 University City Science Center,
!    Philadelphia, PA, 19104-2688.
!    ISBN 0-89871-172-X
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(*), the vector to be examined.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive entries of SX.
!
!    Output, integer ( kind = 4 ) IDAMAX, the index of the element of SX of maximum
!    absolute value.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) idamax
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 8 ) samax
  real ( kind = 8 ) x(*)

  if ( n <= 0 ) then

    idamax = 0

  else if ( n == 1 ) then

    idamax = 1

  else if ( incx == 1 ) then

    idamax = 1
    samax = abs ( x(1) )

    do i = 2, n

      if ( samax < abs ( x(i) ) ) then
        idamax = i
        samax = abs ( x(i) )
      end if

    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    idamax = 1
    samax = abs ( x(ix) )

    ix = ix + incx

    do i = 2, n
      if ( samax < abs ( x(ix) ) ) then
        idamax = i
        samax = abs ( x(ix) )
      end if
      ix = ix + incx
    end do

  end if

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Modified:
!
!    31 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  character ( len = 8 ) date
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  character ( len = 10 )  time
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y
  character ( len = 5 ) zone

  call date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
