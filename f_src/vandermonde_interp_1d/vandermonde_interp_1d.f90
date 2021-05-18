subroutine vandermonde_coef_1d ( nd, xd, yd, cd )

!*****************************************************************************80
!
!! VANDERMONDE_COEF_1D computes coefficients of a 1D Vandermonde interpolant.
!
!  Discussion:
!
!    We assume the interpolant has the form
!
!      p(x) = c1 + c2 * x + c3 * x^2 + ... + cn * x^(n-1).
!
!    We have n data values (x(i),y(i)) which must be interpolated:
!
!      p(x(i)) = c1 + c2 * x(i) + c3 * x(i)^2 + ... + cn * x(i)^(n-1) = y(i)
!
!    This can be cast as an NxN linear system for the polynomial
!    coefficients:
!
!      [ 1 x1 x1^2 ... x1^(n-1) ] [  c1 ] = [  y1 ]
!      [ 1 x2 x2^2 ... x2^(n-1) ] [  c2 ] = [  y2 ]
!      [ ...................... ] [ ... ] = [ ... ]
!      [ 1 xn xn^2 ... xn^(n-1) ] [  cn ] = [  yn ]
!
!    and if the x values are distinct, the system is theoretically
!    invertible, so we can retrieve the coefficient vector c and
!    evaluate the interpolant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!
!    Input, real ( kind = 8 ) XD(ND), YD(ND), the data values.
!
!    Output, real ( kind = 8 ) CD(0:ND-1), the coefficients of the interpolating
!    polynomial.  CD(0) is the constant term, and CD(ND-1) multiplies X^(ND-1).
!
  implicit none

  integer ( kind = 4 ) nd

  real ( kind = 8 ) ad(nd,nd)
  real ( kind = 8 ) cd(0:nd-1)
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) yd(nd)

  call vandermonde_matrix_1d ( nd, xd, ad )

  call qr_solve ( nd, nd, ad, yd, cd )

  return
end
subroutine vandermonde_matrix_1d ( nd, xd, ad )

!*****************************************************************************80
!
!! VANDERMONDE_MATRIX_1D computes a Vandermonde 1D interpolation matrix.
!
!  Discussion:
!
!    We assume the interpolant has the form
!
!      p(x) = c1 + c2 * x + c3 * x^2 + ... + cn * x^(n-1).
!
!    We have n data values (x(i),y(i)) which must be interpolated:
!
!      p(x(i)) = c1 + c2 * x(i) + c3 * x(i)^2 + ... + cn * x(i)^(n-1) = y(i)
!
!    This can be cast as an NxN linear system for the polynomial
!    coefficients:
!
!      [ 1 x1 x1^2 ... x1^(n-1) ] [  c1 ] = [  y1 ]
!      [ 1 x2 x2^2 ... x2^(n-1) ] [  c2 ] = [  y2 ]
!      [ ...................... ] [ ... ] = [ ... ]
!      [ 1 xn xn^2 ... xn^(n-1) ] [  cn ] = [  yn ]
!
!    and if the x values are distinct, the matrix A is theoretically
!    invertible (though in fact, generally badly conditioned).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!
!    Input, real ( kind = 8 ) XD(ND), the data values.
!
!    Output, real ( kind = 8 ) AD(ND,ND), the Vandermonde matrix for X.
!
  implicit none

  integer ( kind = 4 ) nd

  real ( kind = 8 ) ad(nd,nd)
  integer ( kind = 4 ) j
  real ( kind = 8 ) xd(nd)

  ad(1:nd,1) = 1.0D+00
  do j = 2, nd
    ad(1:nd,j) = ad(1:nd,j-1) * xd(1:nd)
  end do

  return
end
subroutine vandermonde_value_1d ( nd, cd, ni, xi, yi )

!*****************************************************************************80
!
!! VANDERMONDE_VALUE_1D evaluates a Vandermonde 1D interpolant.
!
!  Discussion:
!
!    The polynomial 
!
!      p(x) = cd0 + cd1 * x + cd2 * x^2 + ... + cd(nd-1) * x^(nd-1)
!
!    is to be evaluated at the vector of NI values XI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ND, the number of data values.
!
!    Input, real ( kind = 8 ) CD(0:ND-1), the polynomial coefficients.  
!    CD(I) is the coefficient of X^I.
!
!    Input, integer ( kind = 4 ) NI, the number of interpolation points.
!
!    Input, real ( kind = 8 ) XI(NI), the interpolation points.
!
!    Output, real ( kind = 8 ) YI(NI), the interpolation values.
!
  implicit none

  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni

  real ( kind = 8 ) cd(0:nd-1)
  integer ( kind = 4 ) i
  real ( kind = 8 ) xi(ni)
  real ( kind = 8 ) yi(ni)

  yi(1:ni) = cd(nd-1)
  do i = nd - 2, 0, -1
    yi(1:ni) = yi(1:ni) * xi(1:ni) + cd(i)
  end do

  return
end
