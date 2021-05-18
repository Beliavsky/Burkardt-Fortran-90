subroutine newton_coef_1d ( nd, xd, yd, cd )

!*****************************************************************************80
!
!! NEWTON_COEF_1D computes coefficients of a Newton 1D interpolant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carl deBoor,
!    A Practical Guide to Splines,
!    Springer, 2001,
!    ISBN: 0387953663,
!    LC: QA1.A647.v27.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!
!    Input, real ( kind = 8 ) XD(ND), the X values at which data was taken.
!
!    Input, real ( kind = 8 ) YD(ND), the corresponding Y values.
!
!    Output, real ( kind = 8 ) CD(ND), the divided difference coefficients.
!
  implicit none

  integer ( kind = 4 ) nd

  real ( kind = 8 ) cd(nd)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) yd(nd)
!
!  Copy the data values.
!
  cd(1:nd) = yd(1:nd)
!
!  Compute the divided differences.
!
  do i = 2, nd
    do j = nd, i, -1

      cd(j) = ( cd(j) - cd(j-1) ) / ( xd(j) - xd(j+1-i) )

    end do
  end do

  return
end
subroutine newton_value_1d ( nd, xd, cd, ni, xi, yi )

!*****************************************************************************80
!
!! NEWTON_VALUE_1D evaluates a Newton 1D interpolant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carl deBoor,
!    A Practical Guide to Splines,
!    Springer, 2001,
!    ISBN: 0387953663,
!    LC: QA1.A647.v27.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ND, the order of the difference table.
!
!    Input, real ( kind = 8 ) XD(ND), the X values of the difference table.
!
!    Input, real ( kind = 8 ) CD(ND), the divided differences.
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

  real ( kind = 8 ) cd(nd)
  integer ( kind = 4 ) i
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) xi(ni)
  real ( kind = 8 ) yi(ni)

  yi(1:ni) = cd(nd)
  do i = nd - 1, 1, -1
    yi(1:ni) = cd(i) + ( xi(1:ni) - xd(i) ) * yi(1:ni)
  end do

  return
end

