function jacobi_cn ( u, m )

!*****************************************************************************80
!
!! jacobi_cn evaluates the Jacobi elliptic function CN(U,M).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    Original ALGOL version by Roland Bulirsch.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Roland Bulirsch,
!    Numerical calculation of elliptic integrals and elliptic functions,
!    Numerische Mathematik,
!    Volume 7, Number 1, 1965, pages 78-90.
!
!  Input:
!
!    real ( kind = 8 ) U, M, the arguments.
!
!  Output:
!
!    real ( kind = 8 ) JACOBI_CN, the function value.
!
  implicit none

  real ( kind = 8 ) cn
  real ( kind = 8 ) dn
  real ( kind = 8 ) jacobi_cn
  real ( kind = 8 ) m
  real ( kind = 8 ) sn
  real ( kind = 8 ) u

  call sncndn ( u, m, sn, cn, dn )

  jacobi_cn = cn

  return
end
function jacobi_dn ( u, m )

!*****************************************************************************80
!
!! jacobi_dn evaluates the Jacobi elliptic function DN(U,M).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    Original ALGOL version by Roland Bulirsch.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Roland Bulirsch,
!    Numerical calculation of elliptic integrals and elliptic functions,
!    Numerische Mathematik,
!    Volume 7, Number 1, 1965, pages 78-90.
!
!  Input:
!
!    real ( kind = 8 ) U, M, the arguments.
!
!  Output:
!
!    real ( kind = 8 ) JACOBI_DN, the function value.
!
  implicit none

  real ( kind = 8 ) cn
  real ( kind = 8 ) dn
  real ( kind = 8 ) jacobi_dn
  real ( kind = 8 ) m
  real ( kind = 8 ) sn
  real ( kind = 8 ) u

  call sncndn ( u, m, sn, cn, dn )

  jacobi_dn = dn

  return
end
function jacobi_sn ( u, m )

!*****************************************************************************80
!
!! jacobi_sn evaluates the Jacobi elliptic function SN(U,M).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    Original ALGOL version by Roland Bulirsch.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Roland Bulirsch,
!    Numerical calculation of elliptic integrals and elliptic functions,
!    Numerische Mathematik,
!    Volume 7, Number 1, 1965, pages 78-90.
!
!  Input:
!
!    real ( kind = 8 ) U, M, the arguments.
!
!  Output:
!
!    real ( kind = 8 ) JACOBI_SN, the function value.
!
  implicit none

  real ( kind = 8 ) cn
  real ( kind = 8 ) dn
  real ( kind = 8 ) jacobi_sn
  real ( kind = 8 ) m
  real ( kind = 8 ) sn
  real ( kind = 8 ) u

  call sncndn ( u, m, sn, cn, dn )

  jacobi_sn = sn

  return
end
subroutine sncndn ( u, m, sn, cn, dn )

!*****************************************************************************80
!
!! sncndn evaluates Jacobi elliptic functions SN, CN, and DN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2018
!
!  Author:
!
!    Original ALGOL version by Roland Bulirsch.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Roland Bulirsch,
!    Numerical calculation of elliptic integrals and elliptic functions,
!    Numerische Mathematik,
!    Volume 7, Number 1, 1965, pages 78-90.
!
!  Input:
!
!    real ( kind = 8 ) U, M, the arguments.
!
!  Output:
!
!    real ( kind = 8 ) SN, CN, DN, the value of the Jacobi
!    elliptic functions sn(u,m), cn(u,m), and dn(u,m).
!
  implicit none

  integer ( kind = 4 ), parameter :: maxit = 25

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) ca
  real ( kind = 8 ) cn
  real ( kind = 8 ) d
  real ( kind = 8 ) dn
  real ( kind = 8 ) m_array(maxit)
  real ( kind = 8 ) n_array(maxit)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) l
  real ( kind = 8 ) m
  real ( kind = 8 ) m_comp
  real ( kind = 8 ) sn
  real ( kind = 8 ) u
  real ( kind = 8 ) u_copy

  m_comp = 1.0D+00 - m
  u_copy = u

  if ( m_comp == 0.0D+00 ) then
    cn = 1.0D+00 / cosh ( u_copy )
    dn = cn
    sn = tanh ( u_copy )
    return
  end if

  if ( 1.0D+00 < m ) then
    d = 1.0D+00 - m_comp
    m_comp = - m_comp / d
    d = sqrt ( d )
    u_copy = d * u_copy
  end if

  ca = sqrt ( epsilon ( ca ) )

  a = 1.0D+00
  dn = 1.0D+00
  l = maxit

  do i = 1, maxit

    m_array(i) = a
    m_comp = sqrt ( m_comp )
    n_array(i) = m_comp
    c = 0.5D+00 * ( a + m_comp )

    if ( abs ( a - m_comp ) <= ca * a ) then
      l = i
      exit
    end if

    m_comp = a * m_comp
    a = c

  end do

  u_copy = c * u_copy
  sn = sin ( u_copy )
  cn = cos ( u_copy )

  if ( sn /= 0.0D+00 ) then

    a = - cn / sn
    c = a * c

    do i = l, 1, -1
      b = m_array(i)
      a = c * a
      c = dn * c
      dn = ( n_array(i) + a ) / ( b + a )
      a = c / b
    end do

    a = 1.0D+00 / sqrt ( c * c + 1.0D+00 )

    if ( sn < 0.0D+00 ) then
      sn = - a
    else
      sn = a
    end if

    cn = c * sn

  end if

  if ( 1.0D+00 < m ) then
    a = dn
    dn = cn
    cn = a
    sn = sn / d
  end if

  return
end
subroutine jacobi_cn_values ( n_data, u, m, cn )

!*****************************************************************************80
!
!! jacobi_cn_values returns some values of the Jacobi elliptic function CN(U,M).
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      JacobiCN[ u, m ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 before 
!    the first call.  On further calls, N_DATA should simply be left
!    as the output value from the previous call.
!
!  Output:
!
!    integer ( kind = 4 ) N_DATA, usually, the incremented input value.
!    If N_DATA is zero, there is no more data to return.
!
!    real ( kind = 8 ) U, the argument of the function.
!
!    real ( kind = 8 ) M, the parameter of the function.
!
!    real ( kind = 8 ) CN, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) m
  real ( kind = 8 ), save, dimension ( n_max ) :: m_vec = (/ &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00 /)
  real ( kind = 8 ) cn
  real ( kind = 8 ), save, dimension ( n_max ) :: cn_vec = (/ &
     0.9950041652780258D+00, &
     0.9800665778412416D+00, &
     0.8775825618903727D+00, &
     0.5403023058681397D+00, &
    -0.4161468365471424D+00, &
     0.9950124626090582D+00, &
     0.9801976276784098D+00, &
     0.8822663948904403D+00, &
     0.5959765676721407D+00, &
    -0.1031836155277618D+00, &
     0.9950207489532265D+00, &
     0.9803279976447253D+00, &
     0.8868188839700739D+00, &
     0.6480542736638854D+00, &
     0.2658022288340797D+00, &
     0.3661899347368653D-01, &
     0.9803279976447253D+00, &
     0.8868188839700739D+00, &
     0.6480542736638854D+00, &
     0.2658022288340797D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) u
  real ( kind = 8 ), save, dimension ( n_max ) :: u_vec = (/ &
     0.1D+00, &
     0.2D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     0.1D+00, &
     0.2D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     0.1D+00, &
     0.2D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     4.0D+00, &
    -0.2D+00, &
    -0.5D+00, &
    -1.0D+00, &
    -2.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    m = 0.0D+00
    u = 0.0D+00
    cn = 0.0D+00
  else
    m = m_vec(n_data)
    u = u_vec(n_data)
    cn = cn_vec(n_data)
  end if

  return
end
subroutine jacobi_dn_values ( n_data, u, m, dn )

!*****************************************************************************80
!
!! jacobi_dn_values returns some values of the Jacobi elliptic function DN(U,M).
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      JacobiDN[ u, m ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 before 
!    the first call.  On further calls, N_DATA should simply be left
!    as the output value from the previous call.
!
!  Output:
!
!    integer ( kind = 4 ) N_DATA, usually, the incremented input value.
!    If N_DATA is zero, there is no more data to return.
!
!    real ( kind = 8 ) U, the argument of the function.
!
!    real ( kind = 8 ) M, the parameter of the function.
!
!    real ( kind = 8 ) DN, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) m
  real ( kind = 8 ), save, dimension ( n_max ) :: m_vec = (/ &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00 /)
  real ( kind = 8 ) dn
  real ( kind = 8 ), save, dimension ( n_max ) :: dn_vec = (/ &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.9975093485144243D+00, &
    0.9901483195224800D+00, &
    0.9429724257773857D+00, &
    0.8231610016315963D+00, &
    0.7108610477840873D+00, &
    0.9950207489532265D+00, &
    0.9803279976447253D+00, &
    0.8868188839700739D+00, &
    0.6480542736638854D+00, &
    0.2658022288340797D+00, &
    0.3661899347368653D-01, &
    0.9803279976447253D+00, &
    0.8868188839700739D+00, &
    0.6480542736638854D+00, &
    0.2658022288340797D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) u
  real ( kind = 8 ), save, dimension ( n_max ) :: u_vec = (/ &
     0.1D+00, &
     0.2D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     0.1D+00, &
     0.2D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     0.1D+00, &
     0.2D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     4.0D+00, &
    -0.2D+00, &
    -0.5D+00, &
    -1.0D+00, &
    -2.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    m = 0.0D+00
    u = 0.0D+00
    dn = 0.0D+00
  else
    m = m_vec(n_data)
    u = u_vec(n_data)
    dn = dn_vec(n_data)
  end if

  return
end
subroutine jacobi_sn_values ( n_data, u, m, sn )

!*****************************************************************************80
!
!! jacobi_sn_values returns some values of the Jacobi elliptic function SN(U,M).
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      JacobiSN[ u, m ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 before 
!    the first call.  On further calls, N_DATA should simply be left
!    as the output value from the previous call.
!
!  Output:
!
!    integer ( kind = 4 ) N_DATA, usually, the incremented input value.
!    If N_DATA is zero, there is no more data to return.
!
!    real ( kind = 8 ) U, the argument of the function.
!
!    real ( kind = 8 ) M, the parameter of the function.
!
!    real ( kind = 8 ) SN, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) m
  real ( kind = 8 ), save, dimension ( n_max ) :: m_vec = (/ &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00 /)
  real ( kind = 8 ) sn
  real ( kind = 8 ), save, dimension ( n_max ) :: sn_vec = (/ &
     0.9983341664682815D-01, &
     0.1986693307950612D+00, &
     0.4794255386042030D+00, &
     0.8414709848078965D+00, &
     0.9092974268256817D+00, &
     0.9975068547462484D-01, &
     0.1980217429819704D+00, &
     0.4707504736556573D+00, &
     0.8030018248956439D+00, &
     0.9946623253580177D+00, &
     0.9966799462495582D-01, &
     0.1973753202249040D+00, &
     0.4621171572600098D+00, &
     0.7615941559557649D+00, &
     0.9640275800758169D+00, &
     0.9993292997390670D+00, &
    -0.1973753202249040D+00, &
    -0.4621171572600098D+00, &
    -0.7615941559557649D+00, &
    -0.9640275800758169D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) u
  real ( kind = 8 ), save, dimension ( n_max ) :: u_vec = (/ &
     0.1D+00, &
     0.2D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     0.1D+00, &
     0.2D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     0.1D+00, &
     0.2D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     4.0D+00, &
    -0.2D+00, &
    -0.5D+00, &
    -1.0D+00, &
    -2.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    m = 0.0D+00
    u = 0.0D+00
    sn = 0.0D+00
  else
    m = m_vec(n_data)
    u = u_vec(n_data)
    sn = sn_vec(n_data)
  end if

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! timestamp prints the current YMDHMS date as a time stamp.
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
