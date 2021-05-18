program main

!*****************************************************************************80
!
!! MAIN is the main program for CUBE_EXACTNESS_TEST.
!
!  Discussion:
!
!    CUBE_EXACTNESS_TEST tests the CUBE_EXACTNESS library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CUBE_EXACTNESS_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CUBE_EXACTNESS library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'CUBE_EXACTNESS_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests product Gauss-Legendre rules for the Legendre 3D integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(3)
  real ( kind = 8 ) b(3)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) nz
  integer ( kind = 4 ) p_max
  integer ( kind = 4 ) t
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ), allocatable :: z(:)

  a(1:3) = -1.0D+00
  b(1:3) = +1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Product Gauss-Legendre rules for the 3D Legendre integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1.'
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '          -1 <= y <= +1.'
  write ( *, '(a)' ) '          -1 <= z <= +1.'
  write ( *, '(a)' ) '  Level: L'
  write ( *, '(a)' ) '  Exactness: 2*L+1'
  write ( *, '(a)' ) '  Order: N = (L+1)*(L+1)*(L+1)'

  do l = 0, 5

    nx = l + 1
    ny = l + 1
    nz = l + 1
    n = nx * ny * nz
    t = 2 * l + 1

    allocate ( x(1:n) )
    allocate ( y(1:n) )
    allocate ( z(1:n) )
    allocate ( w(1:n) )

    call legendre_3d_set ( a, b, nx, ny, nz, x, y, z, w )

    p_max = t + 1
    call legendre_3d_exactness ( a, b, n, x, y, z, w, p_max )

    deallocate ( x )
    deallocate ( y )
    deallocate ( z )
    deallocate ( w )

  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests product Gauss-Legendre rules for the Legendre 3D integral.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a(3)
  real ( kind = 8 ) b(3)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) nz
  integer ( kind = 4 ) p_max
  real ( kind = 8 ), allocatable :: w(:)
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)
  real ( kind = 8 ), allocatable :: z(:)

  a(1:3) = -1.0D+00
  b(1:3) = +1.0D+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Product Gauss-Legendre rules for the 3D Legendre integral.'
  write ( *, '(a)' ) '  Density function rho(x) = 1.'
  write ( *, '(a)' ) '  Region: -1 <= x <= +1.'
  write ( *, '(a)' ) '          -1 <= y <= +1.'
  write ( *, '(a)' ) '          -1 <= z <= +1.'
  write ( *, '(a)' ) '  Exactness: 3 = 2 * min ( 2, 3, 4 ) - 1'
  write ( *, '(a)' ) '  Order: N = 2 * 3 * 4'

  nx = 2
  ny = 3
  nz = 4
  n = nx * ny * nz

  allocate ( x(1:n) )
  allocate ( y(1:n) )
  allocate ( z(1:n) )
  allocate ( w(1:n) )

  call legendre_3d_set ( a, b, nx, ny, nz, x, y, z, w )

  p_max = 4

  call legendre_3d_exactness ( a, b, n, x, y, z, w, p_max )

  deallocate ( x )
  deallocate ( y )
  deallocate ( z )
  deallocate ( w )

  return
end
subroutine legendre_3d_set ( a, b, nx, ny, nz, x, y, z, w )

!*****************************************************************************80
!
!! LEGENDRE_3D_SET: set a 3D Gauss-Legendre quadrature rule.
!
!  Discussion:
!
!    The integral:
!
!      integral ( z1 <= z <= z2 )
!               ( y1 <= y <= y2 ) 
!               ( x1 <= x <= x2 ) f(x,y,z) dx dy dz
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * f ( x(i),y(i),z(i) )
!
!    where n = nx * ny * nz.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3), B(3), the lower and upper integration
!    limits.
!
!    Input, integer ( kind = 4 ) NX, NY, NZ, the orders in the X and Y directions.
!    These orders must be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), Y(N), Z(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  integer ( kind = 4 ) nz

  real ( kind = 8 ) a(3)
  real ( kind = 8 ) b(3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) w(nx*ny*nz)
  real ( kind = 8 ) wx(nx)
  real ( kind = 8 ) wy(ny)
  real ( kind = 8 ) wz(nz)
  real ( kind = 8 ) x(nx*ny*nz)
  real ( kind = 8 ) xx(nx)
  real ( kind = 8 ) y(nx*ny*nz)
  real ( kind = 8 ) yy(ny)
  real ( kind = 8 ) z(nx*ny*nz)
  real ( kind = 8 ) zz(nz)
!
!  Get the rules for [-1,+1].
!
  call legendre_set ( nx, xx, wx )
  call legendre_set ( ny, yy, wy )
  call legendre_set ( nz, zz, wz )
!
!  Adjust from [-1,+1] to [A,B].
!
  do i = 1, nx
    xx(i) = ( ( 1.0D+00 - xx(i) ) * a(1)   &
            + ( 1.0D+00 + xx(i) ) * b(1) ) &
            /   2.0D+00
    wx(i) = wx(i) * ( b(1) - a(1) ) / 2.0D+00
  end do

  do j = 1, ny
    yy(j) = ( ( 1.0D+00 - yy(j) ) * a(2)   &
            + ( 1.0D+00 + yy(j) ) * b(2) ) &
            /   2.0D+00
    wy(j) = wy(j) * ( b(2) - a(2) ) / 2.0D+00
  end do

  do k = 1, nz
    zz(k) = ( ( 1.0D+00 - zz(k) ) * a(3)   &
            + ( 1.0D+00 + zz(k) ) * b(3) ) &
            /   2.0D+00
    wz(k) = wz(k) * ( b(3) - a(3) ) / 2.0D+00
  end do
!
!  Compute the product rule.
!
  l = 0
  do k = 1, nz
    do j = 1, ny
      do i = 1, nx
        l = l + 1
        x(l) = xx(i)
        y(l) = yy(j)
        z(l) = zz(k)
        w(l) = wx(i) * wy(j) * wz(k)
      end do
    end do
  end do

  return
end
subroutine legendre_set ( n, x, w )

!*****************************************************************************80
!
!! LEGENDRE_SET sets abscissas and weights for Gauss-Legendre quadrature.
!
!  Discussion:
!
!    The integral:
!
!      integral ( -1 <= x <= 1 ) f(x) dx
!
!    The quadrature rule:
!
!      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
!
!    The quadrature rule is exact for polynomials through degree 2*N-1.
!
!    The abscissas are the zeroes of the Legendre polynomial P(N)(X).
!
!    Mathematica can compute the abscissas and weights of a Gauss-Legendre
!    rule of order N for the interval [A,B] with P digits of precision
!    by the commands:
!
!    Needs["NumericalDifferentialEquationAnalysis`"]
!    GaussianQuadratureWeights [ n, a, b, p ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2010
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
!    Vladimir Krylov,
!    Approximate Calculation of Integrals,
!    Dover, 2006,
!    ISBN: 0486445798,
!    LC: QA311.K713.
!
!    Arthur Stroud, Don Secrest,
!    Gaussian Quadrature Formulas,
!    Prentice Hall, 1966,
!    LC: QA299.4G3S7.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996,
!    ISBN: 0-8493-2479-3,
!    LC: QA47.M315.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    N must be between 1 and 10.
!
!    Output, real ( kind = 8 ) X(N), the abscissas.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = 0.000000000000000000000000000000D+00

    w(1) = 2.000000000000000000000000000000D+00

  else if ( n == 2 ) then

    x(1) = -0.577350269189625764509148780502D+00
    x(2) = 0.577350269189625764509148780502D+00

    w(1) = 1.000000000000000000000000000000D+00
    w(2) = 1.000000000000000000000000000000D+00

  else if ( n == 3 ) then

    x(1) = -0.774596669241483377035853079956D+00
    x(2) = 0.000000000000000000000000000000D+00
    x(3) = 0.774596669241483377035853079956D+00

    w(1) = 0.555555555555555555555555555556D+00
    w(2) = 0.888888888888888888888888888889D+00
    w(3) = 0.555555555555555555555555555556D+00

  else if ( n == 4 ) then

    x(1) = -0.861136311594052575223946488893D+00
    x(2) = -0.339981043584856264802665759103D+00
    x(3) = 0.339981043584856264802665759103D+00
    x(4) = 0.861136311594052575223946488893D+00

    w(1) = 0.347854845137453857373063949222D+00
    w(2) = 0.652145154862546142626936050778D+00
    w(3) = 0.652145154862546142626936050778D+00
    w(4) = 0.347854845137453857373063949222D+00

  else if ( n == 5 ) then

    x(1) = -0.906179845938663992797626878299D+00
    x(2) = -0.538469310105683091036314420700D+00
    x(3) = 0.000000000000000000000000000000D+00
    x(4) = 0.538469310105683091036314420700D+00
    x(5) = 0.906179845938663992797626878299D+00

    w(1) = 0.236926885056189087514264040720D+00
    w(2) = 0.478628670499366468041291514836D+00
    w(3) = 0.568888888888888888888888888889D+00
    w(4) = 0.478628670499366468041291514836D+00
    w(5) = 0.236926885056189087514264040720D+00

  else if ( n == 6 ) then

    x(1) = -0.932469514203152027812301554494D+00
    x(2) = -0.661209386466264513661399595020D+00
    x(3) = -0.238619186083196908630501721681D+00
    x(4) = 0.238619186083196908630501721681D+00
    x(5) = 0.661209386466264513661399595020D+00
    x(6) = 0.932469514203152027812301554494D+00

    w(1) = 0.171324492379170345040296142173D+00
    w(2) = 0.360761573048138607569833513838D+00
    w(3) = 0.467913934572691047389870343990D+00
    w(4) = 0.467913934572691047389870343990D+00
    w(5) = 0.360761573048138607569833513838D+00
    w(6) = 0.171324492379170345040296142173D+00

  else if ( n == 7 ) then

    x(1) = -0.949107912342758524526189684048D+00
    x(2) = -0.741531185599394439863864773281D+00
    x(3) = -0.405845151377397166906606412077D+00
    x(4) = 0.000000000000000000000000000000D+00
    x(5) = 0.405845151377397166906606412077D+00
    x(6) = 0.741531185599394439863864773281D+00
    x(7) = 0.949107912342758524526189684048D+00

    w(1) = 0.129484966168869693270611432679D+00
    w(2) = 0.279705391489276667901467771424D+00
    w(3) = 0.381830050505118944950369775489D+00
    w(4) = 0.417959183673469387755102040816D+00
    w(5) = 0.381830050505118944950369775489D+00
    w(6) = 0.279705391489276667901467771424D+00
    w(7) = 0.129484966168869693270611432679D+00

  else if ( n == 8 ) then

    x(1) = -0.960289856497536231683560868569D+00
    x(2) = -0.796666477413626739591553936476D+00
    x(3) = -0.525532409916328985817739049189D+00
    x(4) = -0.183434642495649804939476142360D+00
    x(5) = 0.183434642495649804939476142360D+00
    x(6) = 0.525532409916328985817739049189D+00
    x(7) = 0.796666477413626739591553936476D+00
    x(8) = 0.960289856497536231683560868569D+00

    w(1) = 0.101228536290376259152531354310D+00
    w(2) = 0.222381034453374470544355994426D+00
    w(3) = 0.313706645877887287337962201987D+00
    w(4) = 0.362683783378361982965150449277D+00
    w(5) = 0.362683783378361982965150449277D+00
    w(6) = 0.313706645877887287337962201987D+00
    w(7) = 0.222381034453374470544355994426D+00
    w(8) = 0.101228536290376259152531354310D+00

  else if ( n == 9 ) then

    x(1) = -0.968160239507626089835576203D+00
    x(2) = -0.836031107326635794299429788D+00
    x(3) = -0.613371432700590397308702039D+00
    x(4) = -0.324253423403808929038538015D+00
    x(5) = 0.000000000000000000000000000D+00
    x(6) = 0.324253423403808929038538015D+00
    x(7) = 0.613371432700590397308702039D+00
    x(8) = 0.836031107326635794299429788D+00
    x(9) = 0.968160239507626089835576203D+00

    w(1) = 0.081274388361574411971892158111D+00
    w(2) = 0.18064816069485740405847203124D+00
    w(3) = 0.26061069640293546231874286942D+00
    w(4) = 0.31234707704000284006863040658D+00
    w(5) = 0.33023935500125976316452506929D+00
    w(6) = 0.31234707704000284006863040658D+00
    w(7) = 0.26061069640293546231874286942D+00
    w(8) = 0.18064816069485740405847203124D+00
    w(9) = 0.081274388361574411971892158111D+00

  else if ( n == 10 ) then

    x(1) = -0.973906528517171720077964012D+00
    x(2) = -0.865063366688984510732096688D+00
    x(3) = -0.679409568299024406234327365D+00
    x(4) = -0.433395394129247190799265943D+00
    x(5) = -0.148874338981631210884826001D+00
    x(6) = 0.148874338981631210884826001D+00
    x(7) = 0.433395394129247190799265943D+00
    x(8) = 0.679409568299024406234327365D+00
    x(9) = 0.865063366688984510732096688D+00
    x(10) = 0.973906528517171720077964012D+00

    w(1) = 0.066671344308688137593568809893D+00
    w(2) = 0.14945134915058059314577633966D+00
    w(3) = 0.21908636251598204399553493423D+00
    w(4) = 0.26926671930999635509122692157D+00
    w(5) = 0.29552422471475287017389299465D+00
    w(6) = 0.29552422471475287017389299465D+00
    w(7) = 0.26926671930999635509122692157D+00
    w(8) = 0.21908636251598204399553493423D+00
    w(9) = 0.14945134915058059314577633966D+00
    w(10) = 0.066671344308688137593568809893D+00

  else

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'LEGENDRE_SET - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of N.'
    stop 1

  end if

  return
end

