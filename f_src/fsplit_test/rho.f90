1: subroutine rho ( abserr, fxname, iprint, maxstp, maxtab, &
3:   relerr, xroot, xtry1, xtry2 )
3: 
3: !*****************************************************************************80
3: !
3: !! rho() includes an INTERFACE statement.
3: !
3: !  Discussion:
3: !
3: !    The method uses the idea of considering the related function
3: !
3: !      H(X) = 1 / F(X)
3: !
3: !    The iteration begins with two estimates for the root supplied by
3: !    the user.
3: !
3: !    From the most recent approximation to the root, X(K), the next
3: !    approximation X(K+1) is determined by:
3: !
3: !      X(K+1) = X(K) + H(X(K-R),...,X(K-1)) / H(X(K-R),...,X(K-1),X(K))
3: !
3: !    where K-R = 1 until the maximal order NTAB is reached.
3: !
3: !    Generally, the next iterate X(K+1) is the zero of a rational function
3: !    which passes through the previous data points.
3: !
3: !  Licensing:
3: !
3: !    This code is distributed under the GNU LGPL license.
3: !
3: !  Modified:
3: !
3: !    11 April 1999
3: !
3: !  Author:
3: !
3: !    John Burkardt
3: !
3: !  Reference:
3: !
3: !    FM Larkin,
3: !    Root Finding by Divided Differences,
3: !    Numerische Mathematik,
3: !    Volume 37, pages 93-104, 1981.
3: !
3: !  Parameters:
3: !
3: !    Input, real ( kind = 8 ) ABSERR, a positive absolute error tolerance.
3: !    If an estimate X for the root is found with ABS ( F(X) ) <= ABSERR,
3: !    the iteration is stopped.
3: !
3: !    Input, external FXNAME, the name of the function routine which evaluates
3: !    F(X).  The form of FXNAME must be similar to the following function which
3: !    has F(X) = ( X - 1 ) * ( X + 1 ).
3: !
3: !    function parab ( x )
3: !
3: !      real ( kind = 8 ) parab
3: !      real ( kind = 8 ) x
3: !
3: !      parab = ( x - 1.0D+00 ) * ( x + 1.0D+00 )
3: !
3: !      return
3: !    end
3: !
3: !    Input, integer ( kind = 4 ) IPRINT, a switch controlling printed output:
3: !    0, only print error messages.
3: !    nonzero, also print a table of the iterative process.
3: !
3: !    Input, integer ( kind = 4 ) MAXSTP, the limit on how many iterations
3: !    may be tried.
3: !
3: !    Input, integer ( kind = 4 ) MAXTAB, the limit on how high an order can be
3: !    used in the divided difference table.  MAXTAB must be at least 2, and
3: !    probably should not be too large.  Perhaps a value of 5 or 6 is reasonable,
3: !    20 is too large.
3: !
3: !    Input, real ( kind = 8 ) RELERR, a tolerance on the size of the change
3: !    in the root estimates.  If a step is taken, and the change in the root
3: !    estimate is less than RELERR, the iteration will stop.
3: !
3: !    Output, real ( kind = 8 ) XROOT, the point which the program has
3: !    produced as an approximate root.
3: !    Either ABS ( F(XROOT) ) <= ABSERR, or the maximum number of steps was
3: !    reached, or the current estimate of the root could not be significantly
3: !    improved.
3: !
3: !    Input, real ( kind = 8 ) XTRY1, XTRY2, two initial approximations to
3: !    the root, supplied by the user, which must be distinct.
3: !
3:   implicit none
3: 
3:   integer ( kind = 4 ), intent(in) :: maxtab
3: 
3:   real ( kind = 8 ), intent(in) :: abserr
3:   real ( kind = 8 ) diftab(maxtab)
3:   real ( kind = 8 ) froot
3:   real ( kind = 8 ) ftemp1
3:   real ( kind = 8 ) ftemp2
3: !  real ( kind = 8 ), external :: fxname
3:   integer ( kind = 4 ), intent(in) :: iprint
3:   integer ( kind = 4 ) istep
3:   integer ( kind = 4 ), intent(in) :: maxstp
3:   integer ( kind = 4 ) ntab
3:   real ( kind = 8 ), intent(in) :: relerr
3:   real ( kind = 8 ) xdelt
3:   real ( kind = 8 ) xold
3:   real ( kind = 8 ), intent(out) :: xroot
3:   real ( kind = 8 ) xtab(maxtab)
3:   real ( kind = 8 ), intent(in) :: xtry1, xtry2
3:   real ( kind = 8 ) yval
3: !
3:   interface
3:     function fxname(x)
3:       implicit none
3:       real ( kind = 8 ) :: fxname
3:       real ( kind = 8 ), intent(in) :: x
3:     end function
3:   end interface
3: !
3: !  Make sure XTRY1 and XTRY2 are not equal.
3: !
3:   if ( xtry1 == xtry2 ) then
3:     write ( *, '(a)' ) ' '
3:     write ( *, '(a)' ) 'rho - Fatal error!'
3:     write ( *, '(a)' ) '  XTRY1 = XTRY2 on input.'
3:     stop 1
3:   end if
3: !
3: !  Make sure MAXTAB is at least 2.
3: !
3:   if ( maxtab < 2 ) then
3:     write ( *, '(a)' ) ' '
3:     write ( *, '(a)' ) 'rho - Fatal error!'
3:     write ( *, '(a)' ) '  MAXTAB < 2 on input!'
3:     stop 1
3:   end if
3: 
3:   xtab(1) = xtry1
3:   xtab(2) = xtry2
3:   ftemp1 = fxname ( xtry1 )
3:   ftemp2 = fxname ( xtry2 )
3: 
3:   if ( abs ( ftemp2 ) < abs ( ftemp1 ) ) then
3:     xtab(1) = xtry2
3:     xtab(2) = xtry1
3:     call r8_swap ( ftemp1, ftemp2 )
3:   end if
3: !
3: !  Initialize the number of steps.
3: !
3:   istep = 0
3: !
3: !  Initialize the number of data points.
3: !
3:   ntab = 2
3: 
3:   if ( 0 < iprint ) then
3:     write ( *, '(a)' ) ' '
3:     write ( *, '(a)' ) '   Step  NTAB    XROOT        F(XROOT)      XDELT'
3:     write ( *, '(a)' ) ' '
3:   end if
3: !
3: !  Initialize the divided difference table data.
3: !
3:   diftab(1) = 1.0D+00 / ftemp1
3:   diftab(2) = 1.0D+00 / ftemp2
3: 
3:   call data_to_dif ( ntab, xtab, diftab, diftab )
3: !
3: !  Initialize values used in the iteration.
3: !
3:   xroot = xtry1
3:   froot = ftemp1
3:   xdelt = xtry1 - xtry2
3: !
3: !  Does the starting data already satisfy the function norm
3: !  error tolerance ABSERR, or the interval norm error tolerance
3: !  RELERR?
3: !
3:   do
3: 
3:     if ( 0 < iprint ) then
3:       write ( *, '(3x,i4,4x,i2, 3g14.6)' ) istep, ntab, xroot, froot, xdelt
3:     end if
3: 
3:     if ( abs ( froot ) <= abserr ) then
3:       write ( *, '(a)' ) ' '
3:       write ( *, '(a)' ) 'rho - Absolute convergence,'
3:       write ( *, '(a)' ) '  The function value meets the error tolerance.'
3:       exit
3:     end if
3: 
3:     if ( abs ( xdelt ) <= relerr ) then
3:       write ( *, '(a)' ) ' '
3:       write ( *, '(a)' ) 'rho - Relative convergence.'
3:       write ( *, '(a)' ) '  The stepsize meets the error tolerance.'
3:       exit
3:     end if
3: !
3: !  Check the number of steps taken.
3: !
3:     if ( maxstp <= istep ) then
3:       write ( *, '(a)' ) ' '
3:       write ( *, '(a)' ) 'rho - Nonconvergence!'
3:       write ( *, '(a)' ) '  The maximum number of steps was taken.'
3:       exit
3:     end if
3: !
3: !  Generate the next point, XVAL.
3: !
3:     xold = xroot
3:     istep = istep + 1
3: !
3: !  Algorithm breakdown: The divisor DIFTAB(NTAB) is zero.
3: !
3:     if ( diftab(ntab) == 0.0D+00 ) then
3:       write ( *, '(a)' ) ' '
3:       write ( *, '(a)' ) 'rho - Fatal error!'
3:       write ( *, '(a,i6)' ) '  Algorithm using differences of order ', ntab
3:       write ( *, '(a)' ) '  A zero-divisor was computed.'
3:       write ( *, '(a)' ) '  The algorithm has broken down.'
3:       write ( *, '(a)' ) '  Examine the results.  They may be useful.'
3:       write ( *, '(a)' ) '  Perhaps a lower value of MAXTAB would help.'
3:       stop 1
3:     end if
3: 
3:     xroot = xtab(ntab) + ( diftab(ntab-1) / diftab(ntab) )
3:     xdelt = xroot - xold
3:     froot = fxname ( xroot )
3: 
3:     if ( abs ( froot ) <= abserr ) then
3:       cycle
3:     end if
3: 
3:     yval = 1.0D+00 / froot
3: !
3: !  If we are now using MAXTAB points, we have to remove an old
3: !  one before adding the new one.
3: !
3:     if ( maxtab <= ntab ) then
3:       ntab = ntab - 1
3:     end if
3: 
3:     call dif_append ( ntab, xtab, diftab, xroot, yval, ntab, xtab, diftab )
3: 
3:   end do
3: 
3:   return
2: end
