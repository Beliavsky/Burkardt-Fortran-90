subroutine muller ( func, fatol, itmax, x1, x2, x3, xatol, xrtol, &
  xnew, fxnew )

!*****************************************************************************80
!
!! muller carries out Muller's method, using C8 arithmetic.
!
!  Discussion:
!
!    "C8" arithmetic is complex double precision arithmetic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gisela Engeln-Muellges, Frank Uhlig,
!    Numerical Algorithms with C,
!    Springer, 1996,
!    ISBN: 3-540-60530-4,
!    LC: QA297.E56213.
!
!  Input:
!
!    external FUNC, the name of the routine that evaluates the function.
!    FUNC should have the form:
!      subroutine func ( x, fx )
!      complex ( kind = 8 ) fx
!      complex ( kind = 8 ) x
!
!    real ( kind = 8 ) FATOL, the absolute error tolerance for F(X).
!
!    integer ( kind = 4 ) ITMAX, the maximum number of steps allowed.
!
!    complex ( kind = 8 ) X1, X2, X3, three distinct points to start the
!    iteration.
!
!    real ( kind = 8 ) XATOL, XRTOL, absolute and relative
!    error tolerances for the root.
!
!  Output:
!
!    complex ( kind = 8 ) XNEW, the estimated root.
!
!    complex ( kind = 8 ) FXNEW, the value of the function at XNEW.
!
  implicit none

  complex ( kind = 8 ) a
  complex ( kind = 8 ) b
  complex ( kind = 8 ) c
  complex ( kind = 8 ) c8_temp
  complex ( kind = 8 ) discrm
  real ( kind = 8 ) fatol
  complex ( kind = 8 ) fminus
  complex ( kind = 8 ) fplus
  external func
  complex ( kind = 8 ) fxmid
  complex ( kind = 8 ) fxnew
  complex ( kind = 8 ) fxold
  integer ( kind = 4 ) iterate
  integer ( kind = 4 ) itmax
  real ( kind = 8 ) x_ave
  complex ( kind = 8 ) x_inc
  complex ( kind = 8 ) x1
  complex ( kind = 8 ) x2
  complex ( kind = 8 ) x3
  real ( kind = 8 ) xatol
  complex ( kind = 8 ) xlast
  complex ( kind = 8 ) xmid
  complex ( kind = 8 ) xminus
  complex ( kind = 8 ) xnew
  complex ( kind = 8 ) xold
  complex ( kind = 8 ) xplus
  real ( kind = 8 ) xrtol

  xnew = x1
  xmid = x2
  xold = x3

  call func ( xnew, fxnew )
  call func ( xmid, fxmid )
  call func ( xold, fxold )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MULLER:'
  write ( *, '(a)' ) '  Muller''s method (complex root version)'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Iteration     x_real              x_imag             ||fx||           ||disc||'
  write ( *, '(a)' ) ' '

  iterate = -2
  write ( *, '(i6,f20.10,f20.10,f20.10)' ) iterate, xold, abs ( fxold )
  iterate = -1
  write ( *, '(i6,f20.10,f20.10,f20.10)' ) iterate, xmid, abs ( fxmid )
  iterate = 0
  write ( *, '(i6,f20.10,f20.10,f20.10)' ) iterate, xnew, abs ( fxnew )

  if ( abs ( fxnew ) < fatol ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'C4_MULLER:'
    write ( *, '(a)' ) '  |F(X)| is below the tolerance.'
    return
  end if

  do
!
!  You may need to swap (XMID,FXMID) and (XNEW,FXNEW).
!
    if ( abs ( fxmid ) <= abs ( fxnew ) ) then

      c8_temp = xnew
      xnew = xmid
      xmid = c8_temp

      c8_temp = fxnew
      fxnew = fxmid
      fxmid = c8_temp

    end if

    xlast = xnew
    iterate = iterate + 1

    if ( itmax < iterate ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'C8_MULLER:'
      write ( *, '(a)' ) '  Maximum number of steps taken.'
      exit
    end if

    a =  ( ( xmid - xnew ) * ( fxold - fxnew ) &
         - ( xold - xnew ) * ( fxmid - fxnew ) )

    b = ( ( xold - xnew )**2 * ( fxmid - fxnew ) &
        - ( xmid - xnew )**2 * ( fxold - fxnew ) )

    c = ( ( xold - xnew ) * ( xmid - xnew ) * ( xold - xmid ) * fxnew )

    xold = xmid
    xmid = xnew
!
!  Apply the quadratic formula to get roots XPLUS and XMINUS.
!
    discrm = b**2 - 4.0D+00 * a * c

    if ( a == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'C8_MULLER:'
      write ( *, '(a)' ) '  The algorithm has broken down.'
      write ( *, '(a)' ) '  The quadratic coefficient A is zero.'
      exit
    end if

    xplus = xnew + ( ( - b + sqrt ( discrm ) ) / ( 2.0D+00 * a ) )

    call func ( xplus, fplus )

    xminus = xnew + ( ( - b - sqrt ( discrm ) ) / ( 2.0D+00 * a ) )

    call func ( xminus, fminus )
!
!  Choose the root with smallest function value.
!
    if ( abs ( fminus ) < abs ( fplus ) ) then
      xnew = xminus
    else
      xnew = xplus
    end if

    fxold = fxmid
    fxmid = fxnew
    call func ( xnew, fxnew )
    write ( *, '(i6,f20.10,f20.10,f20.10,f20.10)' ) &
      iterate, xnew, abs ( fxnew ), abs ( discrm )
!
!  Check for convergence.
!
    x_ave = abs ( xnew + xmid + xold ) / 3.0D+00
    x_inc = xnew - xmid

    if ( abs ( x_inc ) <= xatol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'C8_MULLER:'
      write ( *, '(a)' ) '  Absolute convergence of the X increment.'
      exit
    end if

    if ( abs ( x_inc ) <= xrtol * x_ave ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'C8_MULLER:'
      write ( *, '(a)' ) '  Relative convergence of the X increment.'
      exit
    end if

    if ( abs ( fxnew ) <= fatol ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'C8_MULLER:'
      write ( *, '(a)' ) '  Absolute convergence of |F(X)|.'
      exit
    end if

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

