subroutine burgers_etdrk4 ( nx, nt, vis, x, tplt, uplt )

!*****************************************************************************80
!
!! burgers_etdrk4 solves the Burgers equation using the ETD RK4 method.
!
!  Discussion:
!
!    The system being solved is:
!
!      ut = -1/2 d/dx(u^2) + vis * uxx
!
!    over
!
!      -pi < x < +pi
!
!    with initial condition
!
!      u = exp ( - 10.0 * ( sin ( 0.5 * x ) ).^2 );
!
!    and periodic boundary conditions.
!
!  Modified:
!
!    20 April 2020
!
!  Author:
!
!    Original MATLAB version by Lloyd Trefethen.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Stephen Cox, Paul Matthews,
!    Exponential time differencing for stiff systems,
!    Journal of Computational Physics,
!    Volume 176, pages 430-455, 2002.
!
!    Aly-Khan Kassam, Lloyd Trefethen,
!    Fourth-order time-stepping for stiff ODE's,
!    SIAM Journal on Scientific Computing,
!    Volume 26, Number 4, pages 1214-1233, 2005.
!
!    Lloyd Trefethen,
!    Spectral methods in MATLAB,
!    SIAM, 2000,
!    LC: QA377.T65
!    ISBN: 978-0-898714-65-4
!
!  Input:
!
!    integer ( kind = 4 ) NX: the number of nodes.
!    NX should be even.
!
!    integer ( kind = 4 ) NT: the number of time points.
!
!    real ( kind = 8 ) VIS: the viscosity.  
!    "Interesting" values are VIS = 0.03 and 0.
!
!  Output:
!
!    real ( kind = 8 ) X(NX): the spatial grid.
!
!    real ( kind = 8 ) TPLT(NT): the time values.
!
!    real ( kind = 8 ) UPLT(NX,NT): solution values.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 64
  integer ( kind = 4 ) nt
  integer ( kind = 4 ) nx

  complex ( kind = 8 ) a(nx)
  complex ( kind = 8 ) b(nx)
  complex ( kind = 8 ) c(nx)
  complex ( kind = 8 ), parameter :: c8_i = cmplx ( 0.0D+00, 1.0D+00, kind = 8 )
  complex ( kind = 8 ), parameter :: c8_zero = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
  real ( kind = 8 ) dt
  complex ( kind = 8 ) e(nx)
  complex ( kind = 8 ) e2(nx)
  complex ( kind = 8 ) f1(nx)
  complex ( kind = 8 ) f2(nx)
  complex ( kind = 8 ) f3(nx)
  complex ( kind = 8 ) g(nx)
  real ( kind = 8 ) hi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ier
  integer ( kind = 4 ), parameter :: inc = 1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jplt
  integer ( kind = 4 ) jstep
  integer ( kind = 4 ) k(nx)
  complex ( kind = 8 ) l(nx)
  complex ( kind = 8 ) lr(nx,m)
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk
  real ( kind = 8 ) lo
  complex ( kind = 8 ) na(nx)
  complex ( kind = 8 ) nb(nx)
  complex ( kind = 8 ) nc(nx)
  integer ( kind = 4 ) nmax
  complex ( kind = 8 ) nv(nx)
  complex ( kind = 8 ) q(nx)
  complex ( kind = 8 ) r(m)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) s(m)
  real ( kind = 8 ) t
  real ( kind = 8 ) tmax
  real ( kind = 8 ) tplt(nt)
  real ( kind = 8 ) u(nx)
  real ( kind = 8 ) uplt(nx,nt)
  complex ( kind = 8 ) v(nx)
  real ( kind = 8 ) vis
  complex ( kind = 8 ) w(nx)
  real ( kind = 8 ), allocatable :: work ( : )
  real ( kind = 8 ), allocatable :: wsave ( : )
  real ( kind = 8 ) x(nx)
!
!  Initialize FFT.
!
  lenwrk = 2 * nx
  allocate ( work(1:lenwrk) )
  lensav = 2 * nx + int ( log ( real ( nx ) ) ) + 4
  allocate ( wsave(1:lensav) )
  call zfft1i ( nx, wsave, lensav, ier )
  if ( ier /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'burgers_etdrk4 - Fatal error!'
    write ( *, '(a,i2)' ) '  zfft1i returns ier = ', ier
    stop ( 1 )
  end if
!
!  Set up grid.
!
  lo = - r8_pi
  hi = + r8_pi
  do j = 1, nx
    x(j) = ( real ( nx - j + 1, kind = 8 ) * lo   &
           + real (      j - 1, kind = 8 ) * hi ) &
           / real ( nx,         kind = 8 )
  end do
!
!  Initial condition.
!
  u = exp ( - 10.0D+00 * ( sin ( 0.5D+00 * x ) )**2 )
!
!  Compute V = FFT(U).
!
  v = u
  call zfft1f ( nx, inc, v, nx, wsave, lensav, work, lenwrk, ier )
  if ( ier /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'burgers_etdrk4 - Fatal error!'
    write ( *, '(a,i2)' ) '  zfft1f returns ier = ', ier
    stop ( 1 )
  end if
!
!  Set the time step.
!
  dt = 0.4D+00 / nx**2
!
!  Set the wave numbers.
!
  do j = 1, ( nx / 2 )
    k(j) = j - 1
  end do

  k(nx/2+1) = 0
  do j = nx/2+2, nx
    k(j) = j - nx - 1
  end do
!
!  Fourier multipliers.
!
  l = cmplx ( 0.0D+00, vis * k**2, kind = 8 )
  e = exp ( dt * l )
  e2 = exp ( dt * l / 2.0 )
!
!  Roots of unity.
!
  lo = 1.0D+00
  hi = real ( m, kind = 8 )
  call r8vec_linspace ( m, lo, hi, s )
  s = ( s - 0.5D+00 ) / real ( m, kind = 8 )
  r = exp ( 2.0D+00 * c8_i * r8_pi * s )
!
!  LR = dt * L(:,ones(M,1)) + r(ones(N,1),:)
!
  do i = 1, nx
    do j = 1, m
      lr(i,j) = dt * l(i) + r(j)
    end do
  end do
!
!  Estimate Q, f1, f2, f3 by contour integrals.
!
  q  = c8_zero
  f1 = c8_zero
  f2 = c8_zero
  f3 = c8_zero
  do j = 1, m
    q  = q  + ( exp ( lr(1:nx,j) / 2.0 ) - 1.0 ) / lr(1:nx,j)
    f1 = f1 + ( - 4.0 - lr(1:nx,j) + exp ( lr(1:nx,j) ) &
       * ( 4.0 - 3.0 * lr(1:nx,j) + lr(1:nx,j)**2 ) ) / lr(1:nx,j)**3
    f2 = f2 + ( 2.0 + lr(1:nx,j) + exp ( lr(1:nx,j) ) &
       * ( - 2.0 + lr(1:nx,j) ) ) / lr(1:nx,j)**3
    f3 = f3 + ( - 4.0 - 3.0 * lr(1:nx,j) - lr(1:nx,j)**2 &
       + exp ( lr(1:nx,j) ) * ( 4.0 - lr(1:nx,j) ) ) / lr(1:nx,j)**3
  end do
  q  = dt * q  / real ( m, kind = 8 )
  f1 = dt * f1 / real ( m, kind = 8 )
  f2 = dt * f2 / real ( m, kind = 8 )
  f3 = dt * f3 / real ( m, kind = 8 )
!
!  Time stepping.
!
  tmax = 1.0D+00
  nmax = nint ( tmax / dt )
  jstep = nmax / ( nt - 1 )

  jplt = 1
  tplt(jplt) = 0.0D+00
  uplt(1:nx,jplt) = u(1:nx)

  g = - 0.5D+00 * c8_i * k

  do i = 1, nmax

    t = i * dt
!
!   Nv = g .* fft ( real ( ifft ( v ) ) .^2 )
!
    nv = v
    call zfft1b ( nx, inc, nv, nx, wsave, lensav, work, lenwrk, ier )
    nv = real ( nv )
    nv = nv**2
    call zfft1f ( nx, inc, nv, nx, wsave, lensav, work, lenwrk, ier )
    nv = g * nv
!
!   a = E2 .* v + Q .* Nv
!   Na = g .* fft ( real ( ifft ( a ) ) .^2 )
!
    a = e2 * v + q * nv
    na = a
    call zfft1b ( nx, inc, na, nx, wsave, lensav, work, lenwrk, ier )
    na = real ( na )
    na = na**2
    call zfft1f ( nx, inc, na, nx, wsave, lensav, work, lenwrk, ier )
    na = g * na
!
!   b = E2 .* v + Q .* Na
!   Nb = g .* fft ( real ( ifft ( b ) ) .^2 )
!
    b = e2 * v + q * na
    nb = b
    call zfft1b ( nx, inc, nb, nx, wsave, lensav, work, lenwrk, ier )
    nb = real ( nb )
    nb = nb**2
    call zfft1f ( nx, inc, nb, nx, wsave, lensav, work, lenwrk, ier )
    nb = g * nb
!
!   c = E2 .* a + Q .* ( 2.0 * Nb - Nv )
!   Nc = g .* fft ( real ( ifft ( c ) ) .^2 )
!
    c = e2 * a + q * ( 2.0 * nb - nv )
    nc = c
    call zfft1b ( nx, inc, nc, nx, wsave, lensav, work, lenwrk, ier )
    nc = real ( nc )
    nc = nc**2
    call zfft1f ( nx, inc, nc, nx, wsave, lensav, work, lenwrk, ier )
    nc = g * nc

    v = e * v + nv * f1 + 2.0 * ( na + nb ) * f2 + nc * f3

    if ( mod ( i, jstep ) == 0 ) then
      w = v
      call zfft1b ( nx, inc, w, nx, wsave, lensav, work, lenwrk, ier )
      if ( ier /= 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'burgers_etdrk4 - Fatal error!'
        write ( *, '(a,i2)' ) '  zfft1b returns ier = ', ier
        stop ( 1 )
      end if
      u = real ( w )
      jplt = jplt + 1
      tplt(jplt) = t
      uplt(1:nx,jplt) = u(1:nx)
    end if

  end do
!
!  Free FFT memory.
!
  deallocate ( work )
  deallocate ( wsave )

  return
end
subroutine c8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! C8MAT_PRINT prints a C8MAT.
!
!  Discussion:
!
!    A C8MAT is a matrix of C8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    complex ( kind = 8 ) A(M,N), the matrix.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  complex ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call c8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine c8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! C8MAT_PRINT_SOME prints some of a C8MAT.
!
!  Discussion:
!
!    A C8MAT is a matrix of C8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    complex ( kind = 8 ) A(M,N), the matrix.
!
!    integer ( kind = 4 ) ILO, JLO, IHI, JHI, the first row and
!    column, and the last row and column to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 4
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(m,n)
  character ( len = 20 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title
  complex ( kind = 8 ) zero

  zero = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)' 
    return
  end if
!
!  Print the columns of the matrix, in strips of INCX.
!
  do j2lo = jlo, min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i10,10x)' ) j
    end do

    write ( *, '(a,4a20)' ) '  Col: ', ( ctemp(j2), j2 = 1, inc )
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) '  ---'
!
!  Determine the range of the rows in this strip.
!
    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi
!
!  Print out (up to) INCX entries in row I, that lie in the current strip.
!
      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( imag ( a(i,j) ) == 0.0D+00 ) then
          write ( ctemp(j2), '(g10.3,10x)' ) real ( a(i,j), kind = 8 )
        else
          write ( ctemp(j2), '(2g10.3)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,a1,4a20)' ) i, ':', ( ctemp(j2), j2 = 1, inc )

    end do

  end do

  return
end
function c8vec_norm ( n, a )

!*****************************************************************************80
!
!! c8vec_norm returns the L2 norm of a C8VEC.
!
!  Discussion:
!
!    A C8VEC is a vector of C8's.
!
!    The vector L2 norm is defined as:
!
!      C8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) conjg ( A(I) ) * A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    complex ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) C8VEC_NORM, the norm.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(n)
  real ( kind = 8 ) c8vec_norm

  c8vec_norm = sqrt ( &
               sum ( &
               ( abs ( a(1:n) ) )**2 &
               ) )

  return
end
subroutine c8vec_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! c8vec_print_part prints "part" of a C8VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the vector.
!
!    complex ( kind = 8 ) A(N), the vector to be printed.
!
!    integer ( kind = 4 ) MAX_PRINT, the maximum number of lines to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(i)
    end do
    write ( *, '(a)' ) '  ........  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(i), &
      '...more entries...'

  end if

  return
end
subroutine cheb ( n, d, x )

!*****************************************************************************80
!
!! cheb computes the Chebyshev differentiation matrix and grid.
!
!  Discussion:
!
!    Given a grid function v defined on the points of a Chebyshev grid,
!    the discrete derivative w is found by w = D * v.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2020
!
!  Author:
!
!    Original MATLAB version by Lloyd Trefethen.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Lloyd Trefethen,
!    Spectral methods in MATLAB,
!    SIAM, 2000,
!    LC: QA377.T65
!    ISBN: 978-0-898714-65-4
!
!  Input:
!
!    integer ( kind = 4 ) N: the number of grid points minus 1.
!
!  Output:
!
!    real ( kind = 8 ) D(N+1,N+1): the differentiation matrix.
!
!    real ( kind = 8 ) X(N+1): the Chebyshev grid.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) c(n+1)
  real ( kind = 8 ) d(n+1,n+1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_delta
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) s
  real ( kind = 8 ) x(n+1)

  if ( n == 0 ) then
    x(1) = 1.0D+00 
    return
  end if

  call r8vec_linspace ( n + 1, 0.0D+00, r8_pi, x )
  x = cos ( x )

  c(1:n+1) = 1.0D+00
  c(1) = 2.0
  c(n+1) = 2.0
  c(2:n+1:2) = - c(2:n+1:2)
!
!  Set offdiagonal entries.
! 
  do i = 1, n + 1
    do j = 1, n + 1
      d(i,j) = c(i) / c(j) / ( x(i) - x(j) + r8_delta ( i, j ) )
    end do
  end do
!
!  Set diagonal entries.
!
  do i = 1, n + 1
    s = sum ( d(i,1:n+1) )
    d(i,i) = d(i,i) - s
  end do

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen
 
  iunit = 0
 
  do i = 1, 99
 
    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )
 
      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if
 
  end do

  return
end
subroutine i4vec_linspace ( kfirst, klast, inc, k )

!*****************************************************************************80
!
!! i4vec_linspace sets an I4VEC to linearly spaced values.
!
!  Discussion:
!
!    The first entry will have value KFIRST.  
!    The last entry will be N increments of INC from KFIRST, without
!    exceeding the value KLAST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) KFIRST, KLAST: the first and last values.
!    The values of KFIRST and KLAST must be distinct.
!
!    integer ( kind = 4 ) INC, the increment.
!    INC must not be zero.
!    If ( KFIRST < KLAST ) then INC must be positive.
!    If ( KLAST < KFIRST ) then INC must be negative.
!
!  Output:
!
!    integer ( kind = 4 ) K(*), contains the values.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) k(*)
  integer ( kind = 4 ) kfirst
  integer ( kind = 4 ) klast
  integer ( kind = 4 ) value

  i = 1
  value = kfirst

  if ( kfirst < klast .and. 0 < inc ) then

    do while ( value <= klast )
      k(i) = value
      i = i + 1
      value = value + inc
    end do

  else if ( klast < kfirst .and. inc < 0 ) then

    do while ( klast <= value )
      k(i) = value
      i = i + 1
      value = value + inc
    end do

  else

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'i4vec_linspace - Fatal error!'
    write ( *, '(a)' ) '  kfirst equals klast, or'
    write ( *, '(a)' ) '  inc is zero, or'
    write ( *, '(a)' ) '  inc does not have the same sign as klast-kfirst.'
    stop 1

  end if

  return
end
subroutine kdv_etdrk4 ( nx, nt, x, tplt, uplt )

!*****************************************************************************80
!
!! kdv_etdrk4 solves the Korteweg-de Vries equation using the ETD RK4 method.
!
!  Discussion:
!
!    The system being solved is:
!
!    ut + u ux + uxxx = 0 on -pi < x < pi
!
!    or, equivalently,
!
!    ut = -1/2 d/dx(u^2) - uxxx
!
!  Modified:
!
!    18 April 2020
!
!  Author:
!
!    Original MATLAB version by Lloyd Trefethen.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Stephen Cox, Paul Matthews,
!    Exponential time differencing for stiff systems,
!    Journal of Computational Physics,
!    Volume 176, pages 430-455, 2002.
!
!    Aly-Khan Kassam, Lloyd Trefethen,
!    Fourth-order time-stepping for stiff ODE's,
!    SIAM Journal on Scientific Computing,
!    Volume 26, Number 4, pages 1214-1233, 2005.
!
!    Lloyd Trefethen,
!    Spectral methods in MATLAB,
!    SIAM, 2000,
!    LC: QA377.T65
!    ISBN: 978-0-898714-65-4
!
!  Input:
!
!    integer ( kind = 4 ) NX: the number of nodes.
!    NX should be even.
!
!    integer ( kind = 4 ) NT: the number of time points.
!
!  Output:
!
!    real ( kind = 8 ) X(NX): the spatial grid.
!
!    real ( kind = 8 ) TPLT(NT): the time values.
!
!    real ( kind = 8 ) UPLT(NX,NT): solution values.
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 64
  integer ( kind = 4 ) nt
  integer ( kind = 4 ) nx

  complex ( kind = 8 ) a(nx)
  complex ( kind = 8 ) b(nx)
  complex ( kind = 8 ) c(nx)
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  complex ( kind = 8 ), parameter :: c8_i = cmplx ( 0.0D+00, 1.0D+00, kind = 8 )
  complex ( kind = 8 ), parameter :: c8_zero = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
  real ( kind = 8 ) dt
  complex ( kind = 8 ) e(nx)
  complex ( kind = 8 ) e2(nx)
  complex ( kind = 8 ) f1(nx)
  complex ( kind = 8 ) f2(nx)
  complex ( kind = 8 ) f3(nx)
  complex ( kind = 8 ) g(nx)
  real ( kind = 8 ) hi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ier
  integer ( kind = 4 ), parameter :: inc = 1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jplt
  integer ( kind = 4 ) jstep
  integer ( kind = 4 ) k(nx)
  complex ( kind = 8 ) l(nx)
  complex ( kind = 8 ) lr(nx,m)
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk
  real ( kind = 8 ) lo
  complex ( kind = 8 ) na(nx)
  complex ( kind = 8 ) nb(nx)
  complex ( kind = 8 ) nc(nx)
  integer ( kind = 4 ) nmax
  complex ( kind = 8 ) nv(nx)
  complex ( kind = 8 ) q(nx)
  complex ( kind = 8 ) r(m)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) s(m)
  real ( kind = 8 ) t
  real ( kind = 8 ) tmax
  real ( kind = 8 ) tplt(nt)
  real ( kind = 8 ) u(nx)
  real ( kind = 8 ) uplt(nx,nt)
  complex ( kind = 8 ) v(nx)
  complex ( kind = 8 ) w(nx)
  real ( kind = 8 ), allocatable :: work ( : )
  real ( kind = 8 ), allocatable :: wsave ( : )
  real ( kind = 8 ) x(nx)
!
!  Initialize FFT.
!
  lenwrk = 2 * nx
  allocate ( work(1:lenwrk) )
  lensav = 2 * nx + int ( log ( real ( nx ) ) ) + 4
  allocate ( wsave(1:lensav) )
  call zfft1i ( nx, wsave, lensav, ier )
  if ( ier /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'kdv_etdrk4 - Fatal error!'
    write ( *, '(a,i2)' ) '  zfft1i returns ier = ', ier
    stop ( 1 )
  end if
!
!  Set up grid.
!
  lo = - r8_pi
  hi = + r8_pi
  do j = 1, nx
    x(j) = ( real ( nx - j + 1, kind = 8 ) * lo   &
           + real (      j - 1, kind = 8 ) * hi ) &
           / real ( nx,         kind = 8 )
  end do
!
!  Set up two-soliton initial data.
!
  c1 = 25.0
  c2 = 16.0

  u = 3.0 * c1**2 / ( cosh ( 0.5 * ( c1 * ( x + 2.0 ) ) ) )**2 &
    + 3.0 * c2**2 / ( cosh ( 0.5 * ( c2 * ( x + 1.0 ) ) ) )**2
!
!  Compute V = FFT(U).
!
  v = u
  call zfft1f ( nx, inc, v, nx, wsave, lensav, work, lenwrk, ier )
  if ( ier /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'kdv_etdrk4 - Fatal error!'
    write ( *, '(a,i2)' ) '  zfft1f returns ier = ', ier
    stop ( 1 )
  end if
!
!  Set the time step.
!
  dt = 0.4D+00 / nx**2
!
!  Set the wave numbers.
!
  do j = 1, ( nx / 2 )
    k(j) = j - 1
  end do

  k(nx/2+1) = 0
  do j = nx/2+2, nx
    k(j) = j - nx - 1
  end do
!
!  Fourier multipliers.
!  E and E2 are defined differently in kdv_ift!
!
  l = cmplx ( 0.0D+00, k**3, kind = 8 )
  e = exp ( dt * l )
  e2 = exp ( dt * l / 2.0 )
!
!  Roots of unity.
!
  lo = 1.0D+00
  hi = real ( m, kind = 8 )
  call r8vec_linspace ( m, lo, hi, s )
  s = ( s - 0.5D+00 ) / real ( m, kind = 8 )
  r = exp ( 2.0D+00 * c8_i * r8_pi * s )
!
! LR = dt * L(:,ones(M,1)) + r(ones(N,1),:)
!
  do i = 1, nx
    do j = 1, m
      lr(i,j) = dt * l(i) + r(j)
    end do
  end do
!
!  Estimate Q, f1, f2, f3 by contour integrals.
!
  q  = c8_zero
  f1 = c8_zero
  f2 = c8_zero
  f3 = c8_zero
  do j = 1, m
    q  = q  + ( exp ( lr(1:nx,j) / 2.0 ) - 1.0 ) / lr(1:nx,j)
    f1 = f1 + ( - 4.0 - lr(1:nx,j) + exp ( lr(1:nx,j) ) &
       * ( 4.0 - 3.0 * lr(1:nx,j) + lr(1:nx,j)**2 ) ) / lr(1:nx,j)**3
    f2 = f2 + ( 2.0 + lr(1:nx,j) + exp ( lr(1:nx,j) ) &
       * ( - 2.0 + lr(1:nx,j) ) ) / lr(1:nx,j)**3
    f3 = f3 + ( - 4.0 - 3.0 * lr(1:nx,j) - lr(1:nx,j)**2 &
       + exp ( lr(1:nx,j) ) * ( 4.0 - lr(1:nx,j) ) ) / lr(1:nx,j)**3
  end do
  q  = dt * q  / real ( m, kind = 8 )
  f1 = dt * f1 / real ( m, kind = 8 )
  f2 = dt * f2 / real ( m, kind = 8 )
  f3 = dt * f3 / real ( m, kind = 8 )
!
!  Time stepping.
!
  tmax = 0.006D+00
  nmax = nint ( tmax / dt )
  jstep = nmax / ( nt - 1 )

  jplt = 1
  tplt(jplt) = 0.0D+00
  uplt(1:nx,jplt) = u(1:nx)

  g = - 0.5D+00 * c8_i * k

  do i = 1, nmax

    t = i * dt
!
!   Nv = g .* fft ( real ( ifft ( v ) ) .^2 )
!
    nv = v
    call zfft1b ( nx, inc, nv, nx, wsave, lensav, work, lenwrk, ier )
    nv = real ( nv )
    nv = nv**2
    call zfft1f ( nx, inc, nv, nx, wsave, lensav, work, lenwrk, ier )
    nv = g * nv
!
!   a = E2 .* v + Q .* Nv
!   Na = g .* fft ( real ( ifft ( a ) ) .^2 )
!
    a = e2 * v + q * nv
    na = a
    call zfft1b ( nx, inc, na, nx, wsave, lensav, work, lenwrk, ier )
    na = real ( na )
    na = na**2
    call zfft1f ( nx, inc, na, nx, wsave, lensav, work, lenwrk, ier )
    na = g * na
!
!   b = E2 .* v + Q .* Na
!   Nb = g .* fft ( real ( ifft ( b ) ) .^2 )
!
    b = e2 * v + q * na
    nb = b
    call zfft1b ( nx, inc, nb, nx, wsave, lensav, work, lenwrk, ier )
    nb = real ( nb )
    nb = nb**2
    call zfft1f ( nx, inc, nb, nx, wsave, lensav, work, lenwrk, ier )
    nb = g * nb
!
!   c = E2 .* a + Q .* ( 2.0 * Nb - Nv )
!   Nc = g .* fft ( real ( ifft ( c ) ) .^2 )
!
    c = e2 * a + q * ( 2.0 * nb - nv )
    nc = c
    call zfft1b ( nx, inc, nc, nx, wsave, lensav, work, lenwrk, ier )
    nc = real ( nc )
    nc = nc**2
    call zfft1f ( nx, inc, nc, nx, wsave, lensav, work, lenwrk, ier )
    nc = g * nc

    v = e * v + nv * f1 + 2.0 * ( na + nb ) * f2 + nc * f3

    if ( mod ( i, jstep ) == 0 ) then
      w = v
      call zfft1b ( nx, inc, w, nx, wsave, lensav, work, lenwrk, ier )
      if ( ier /= 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'kdv_etdrk4 - Fatal error!'
        write ( *, '(a,i2)' ) '  zfft1b returns ier = ', ier
        stop ( 1 )
      end if
      u = real ( w )
      jplt = jplt + 1
      tplt(jplt) = t
      uplt(1:nx,jplt) = u(1:nx)
    end if

  end do
!
!  Free FFT memory.
!
  deallocate ( work )
  deallocate ( wsave )

  return
end
subroutine kdv_ift ( nx, nt, x, tplt, uplt )

!*****************************************************************************80
!
!! kdv_ift solves the Korteweg-deVries equation using the IFT method.
!
!  Discussion:
!
!    The system being solved is:
!
!    ut + u ux + uxxx = 0 on -pi < x < pi
!    by FFT with integrating factor v = exp(-ik^3t)*uhat.
!
!    This code is related to p27.m in the Trefethen reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2020
!
!  Author:
!
!    Original MATLAB version by Lloyd Trefethen.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Aly-Khan Kassam, Lloyd Trefethen,
!    Fourth-order time-stepping for stiff ODE's,
!    SIAM Journal on Scientific Computing,
!    Volume 26, Number 4, pages 1214-1233, 2005.
!
!    Lloyd Trefethen,
!    Spectral methods in MATLAB,
!    SIAM, 2000,
!    LC: QA377.T65
!    ISBN: 978-0-898714-65-4
!
!  Input:
!
!    integer ( kind = 4 ) NX: the number of nodes.
!    NX should be even.
!
!    integer ( kind = 4 ) NT: the number of time points.
!
!  Output:
!
!    real ( kind = 8 ) X(NX): the spatial grid.
!
!    real ( kind = 8 ) TPLT(NT): the time values.
!
!    real ( kind = 8 ) UPLT(NX,NT): solution values.
!
  implicit none

  integer ( kind = 4 ) nt
  integer ( kind = 4 ) nx

  complex ( kind = 8 ) a(nx)
  complex ( kind = 8 ) b(nx)
  complex ( kind = 8 ) c(nx)
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  complex ( kind = 8 ) d(nx)
  real ( kind = 8 ) dt
  complex ( kind = 8 ) e(nx)
  complex ( kind = 8 ) e2(nx)
  complex ( kind = 8 ) g(nx)
  complex ( kind = 8 ), parameter :: i = cmplx ( 0.0D+00, 1.0D+00, kind = 8 )
  integer ( kind = 4 ) ier
  integer ( kind = 4 ), parameter :: inc = 1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jplt
  integer ( kind = 4 ) jstep
  integer ( kind = 4 ) k(nx)
  complex ( kind = 8 ) l(nx)
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk
  integer ( kind = 4 ) nmax
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) t
  real ( kind = 8 ) tmax
  real ( kind = 8 ) tplt(nt)
  real ( kind = 8 ) u(nx)
  real ( kind = 8 ) uplt(nx,nt)
  complex ( kind = 8 ) v(nx)
  complex ( kind = 8 ) w(nx)
  real ( kind = 8 ), allocatable :: work ( : )
  real ( kind = 8 ), allocatable :: wsave ( : )
  real ( kind = 8 ) x(nx)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
!
!  Initialize FFT.
!
  lenwrk = 2 * nx
  allocate ( work(1:lenwrk) )
  lensav = 2 * nx + int ( log ( real ( nx ) ) ) + 4
  allocate ( wsave(1:lensav) )
  call zfft1i ( nx, wsave, lensav, ier )
  if ( ier /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'kdv_ift - Fatal error!'
    write ( *, '(a,i2)' ) '  zfft1i returns ier = ', ier
    stop ( 1 )
  end if
!
!  Set up grid.
!
  dt = 0.4D+00 / nx**2
  xlo = - r8_pi
  xhi = + r8_pi
  do j = 1, nx
    x(j) = ( real ( nx - j + 1, kind = 8 ) * xlo   &
           + real (      j - 1, kind = 8 ) * xhi ) &
           / real ( nx,         kind = 8 )
  end do
!
!  Set up two-soliton initial data.
!
  c1 = 25.0
  c2 = 16.0

  u = 3.0 * c1**2 / ( cosh ( 0.5 * ( c1 * ( x + 2.0 ) ) ) )**2 &
    + 3.0 * c2**2 / ( cosh ( 0.5 * ( c2 * ( x + 1.0 ) ) ) )**2
!
!  Compute V = FFT(U).
!
  v = u
  call zfft1f ( nx, inc, v, nx, wsave, lensav, work, lenwrk, ier )
  if ( ier /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'kdv_ift - Fatal error!'
    write ( *, '(a,i2)' ) '  zfft1f returns ier = ', ier
    stop ( 1 )
  end if
!
!  Set the time step.
!
  dt = 0.4D+00 / nx**2
!
!  Set the wave numbers.
!
  do j = 1, ( nx / 2 )
    k(j) = j - 1
  end do

  k(nx/2+1) = 0
  do j = nx/2+2, nx
    k(j) = j - nx - 1
  end do
!
!  Fourier multipliers.
!
  l = cmplx ( 0.0D+00, k**3, kind = 8 )
  e = exp ( dt * l )
  e2 = exp ( dt * l / 2.0 )
!
!  Solve the PDE.
!
  tmax = 0.006D+00
  nmax = nint ( tmax / dt )
  jstep = nmax / ( nt - 1 )
  
  jplt = 1
  tplt(jplt) = 0.0D+00
  uplt(1:nx,jplt) = u(1:nx)

  g = - 0.5D+00 * i * dt * k

  do j = 1, nmax

    t = j * dt
!
!   a = g * fft ( real ( ifft ( v ) ) ** 2 )
!
    a = v
    call zfft1b ( nx, inc, a, nx, wsave, lensav, work, lenwrk, ier )
    a = real ( a )
    a = a**2
    call zfft1f ( nx, inc, a, nx, wsave, lensav, work, lenwrk, ier )
    a = g * a
!
!   b = g * fft ( real ( ifft ( e  * ( v + a / 2.0 ) ) ) ** 2 )
!
    b = e * ( v + 0.5D+00 * a )
    call zfft1b ( nx, inc, b, nx, wsave, lensav, work, lenwrk, ier )
    b = real ( b )
    b = b**2
    call zfft1f ( nx, inc, b, nx, wsave, lensav, work, lenwrk, ier )
    b = g * b
!
!   c = g * fft ( real ( ifft ( e  *   v + b / 2.0   ) ) ** 2 )
!
    c = e * v + 0.5D+00 * b
    call zfft1b ( nx, inc, c, nx, wsave, lensav, work, lenwrk, ier )
    c = real ( c )
    c = c**2
    call zfft1f ( nx, inc, c, nx, wsave, lensav, work, lenwrk, ier )
    c = g * c
!
!   d = g * fft ( real ( ifft ( e2 *   v + e * c     ) ) ** 2 )
!
    d = e2 * v + e * c
    call zfft1b ( nx, inc, d, nx, wsave, lensav, work, lenwrk, ier )
    d = real ( d )
    d = d**2
    call zfft1f ( nx, inc, d, nx, wsave, lensav, work, lenwrk, ier )
    d = g * d

    v = e2 * v + ( e2 * a + 2.0 * e * ( b + c ) + d ) / 6.0

    if ( mod ( j, jstep ) == 0 ) then
      w = v
      call zfft1b ( nx, inc, w, nx, wsave, lensav, work, lenwrk, ier )
      if ( ier /= 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'kdv_ift - Fatal error!'
        write ( *, '(a,i2)' ) '  zfft1b returns ier = ', ier
        stop ( 1 )
      end if
      u = real ( w )
      jplt = jplt + 1
      tplt(jplt) = t
      uplt(1:nx,jplt) = u(1:nx)
    end if

  end do
!
!  Free FFT memory.
!
  deallocate ( work )
  deallocate ( wsave )

  return
end
function r8_delta ( i, j )

!*****************************************************************************80
!
!! r8_delta evaluates the delta function on a pair of integers.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) I, J: the values to compare.
!
!  Output:
!
!    real ( kind = 8 ) R8_DELTA: 1 if I=J, 0 otherwise.
!
  implicit none

  real ( kind = 8 ) r8_delta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  if ( i == j ) then
    r8_delta = 1.0D+00
  else
    r8_delta = 0.0D+00
  end if

  return
end

subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 8 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end

subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A, B, the first and last entries.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

  return
end
function r8vec_norm ( n, a )

!*****************************************************************************80
!
!! r8vec_norm returns the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector whose L2 norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM, the L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm

  r8vec_norm = sqrt ( sum ( a(1:n) ** 2 ) )

  return
end
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    real ( kind = 8 ) A(N), the vector to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
