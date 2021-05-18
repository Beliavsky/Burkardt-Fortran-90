program main

!*****************************************************************************80
!
!! MAIN is the main program for VANDERMONDE_APPROX_2D_TEST.
!
!  Discussion:
!
!    VANDERMONDE_APPROX_2D_TEST tests the VANDERMONDE_APPROX_2D library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 September 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m_test_num = 5

  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  integer ( kind = 4 ), dimension(m_test_num) :: m_test = (/ 0, 1, 2, 4, 8 /)
  integer ( kind = 4 ) grid
  integer ( kind = 4 ) prob
  integer ( kind = 4 ) prob_num

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VANDERMONDE_APPROX_2D_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the VANDERMONDE_APPROX_2D library.'
  write ( *, '(a)' ) '  The QR_SOLVE library is needed.'
  write ( *, '(a)' ) '  The R8LIB library is needed.'
  write ( *, '(a)' ) '  This test also needs the TEST_INTERP_2D library.'

  call f00_num ( prob_num )

  do prob = 1, prob_num
    grid = 1
    do j = 1, m_test_num
      m = m_test(j)
      call test01 ( prob, grid, m )
    end do
  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VANDERMONDE_APPROX_2D_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( prob, grd, m )

!*****************************************************************************80
!
!! VANDERMONDE_APPROX_2D_TEST01 tests VANDERMONDE_APPROX_2D_MATRIX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) PROB, the problem number.
!
!    Input, integer ( kind = 4 ) GRD, the grid number.
!    (Can't use GRID as the name because that's also a plotting function.)
!
!    Input, integer ( kind = 4 ) M, the total polynomial degree.
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ) app_error
  real ( kind = 8 ), allocatable :: c(:)
  integer ( kind = 4 ) grd
  integer ( kind = 4 ) m
  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni
  integer ( kind = 4 ) prob
  real ( kind = 8 ) r8vec_norm_affine
  integer ( kind = 4 ) tm
  integer ( kind = 4 ) triangle_num
  real ( kind = 8 ), allocatable :: xd(:)
  real ( kind = 8 ), allocatable :: xi(:)
  real ( kind = 8 ), allocatable :: yd(:)
  real ( kind = 8 ), allocatable :: yi(:)
  real ( kind = 8 ), allocatable :: zd(:)
  real ( kind = 8 ), allocatable :: zi(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a,i4)' ) '  Approximate data from TEST_INTERP_2D problem #', prob
  write ( *, '(a,i4)' ) '  Use grid from TEST_INTERP_2D with index #', grd
  write ( *, '(a,i4)' ) '  Using polynomial approximant of total degree ', m

  call g00_size ( grd, nd )
  write ( *, '(a,i6)' ) '  Number of data points = ', nd

  allocate ( xd(1:nd) )
  allocate ( yd(1:nd) )
  call g00_xy ( grd, nd, xd, yd )

  allocate ( zd(1:nd) )
  call f00_f0 ( prob, nd, xd, yd, zd )

  if ( nd < 10 ) then
    call r8vec3_print ( nd, xd, yd, zd, '  X, Y, Z data:' )
  end if
!
!  Compute the Vandermonde matrix.
!
  tm = triangle_num ( m + 1 );
  allocate ( a(1:nd,1:tm) )
  call vandermonde_approx_2d_matrix ( nd, m, tm, xd, yd, a )
!
!  Solve linear system.
!
  allocate ( c(1:tm) )
  call qr_solve ( nd, tm, a, zd, c )
!
!  #1:  Does approximant match function at data points?
!
  ni = nd
  allocate ( xi(1:ni) )
  allocate ( yi(1:ni) )
  xi(1:ni) = xd(1:ni)
  yi(1:ni) = yd(1:ni)

  allocate ( zi(1:ni) )
  call r8poly_value_2d ( m, c, ni, xi, yi, zi )

  app_error = r8vec_norm_affine ( ni, zi, zd ) / real ( ni, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  L2 data approximation error = ', app_error

  deallocate ( a )
  deallocate ( c )
  deallocate ( xd )
  deallocate ( xi )
  deallocate ( yd )
  deallocate ( yi )
  deallocate ( zd )
  deallocate ( zi )

  return
end
subroutine r8poly_value_2d ( m, c, n, x, y, p )

!*****************************************************************************80
!
!! R8POLY_VALUE_2D evaluates a polynomial in 2 variables, X and Y.
!
!  Discussion:
!
!    We assume the polynomial is of total degree M, and has the form:
!
!      p(x,y) = c00
!             + c10 * x                + c01 * y
!             + c20 * x^2   + c11 * xy + c02 * y^2
!             + ...
!             + cm0 * x^(m) + ...      + c0m * y^m.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the degree of the polynomial.
!
!    Input, real ( kind = 8 ) C(T(M+1)), the polynomial coefficients.
!    C(1) is the constant term.  T(M+1) is the M+1-th triangular number.
!    The coefficients are stored consistent with the following ordering
!    of monomials: 1, X, Y, X^2, XY, Y^2, X^3, X^2Y, XY^2, Y^3, X^4, ...
!
!    Input, integer ( kind = 4 ) N, the number of evaluation points.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the evaluation points.
!
!    Output, real ( kind = 8 ) P(N), the value of the polynomial at the
!    evaluation points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) c(*)
  integer ( kind = 4 ) ex
  integer ( kind = 4 ) ey
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m
  real ( kind = 8 ) p(n)
  integer ( kind = 4 ) s
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  p(1:n) = 0.0D+00

  j = 0
  do s = 0, m
    do ex = s, 0, -1
      ey = s - ex
      j = j + 1
      p(1:n) = p(1:n) + c(j) * x(1:n) ** ex * y(1:n) ** ey
    end do
  end do

  return
end

