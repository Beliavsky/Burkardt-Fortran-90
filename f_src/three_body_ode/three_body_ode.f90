
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
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
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
subroutine r8mat_write ( output_filename, m, n, table )

!*****************************************************************************80
!
!! R8MAT_WRITE writes an R8MAT file.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the output file name.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) TABLE(M,N), the data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  character ( len = * ) output_filename
  integer ( kind = 4 ) output_status
  integer ( kind = 4 ) output_unit
  character ( len = 30 ) string
  real ( kind = 8 ) table(m,n)
!
!  Open the file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, &
    status = 'replace', iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_WRITE - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the output file "' // &
      trim ( output_filename ) // '" on unit ', output_unit
    output_unit = -1
    stop
  end if
!
!  Create a format string.
!
!  For less precision in the output file, try:
!
!                                            '(', m, 'g', 14, '.', 6, ')'
!
  if ( 0 < m .and. 0 < n ) then

    write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'
!
!  Write the data.
!
    do j = 1, n
      write ( output_unit, string ) table(1:m,j)
    end do

  end if
!
!  Close the file.
!
  close ( unit = output_unit )

  return
end
subroutine three_body_deriv ( t, y, yp )

!*****************************************************************************80
!
!! three_body_deriv returns the right hand side of the three body ODE system.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T, the value of the independent variable.
!
!    Input, real ( kind = 8 ) Y(NEQN), the value of the dependent variable.
!
!    Output, real ( kind = 8 ) YP(NEQN), the value of the derivative
!    dY(1:NEQN)/dT.
!
  implicit none

  integer ( kind = 4 ), parameter :: neqn = 12

  real ( kind = 8 ) m0
  real ( kind = 8 ) m1
  real ( kind = 8 ) m2
  real ( kind = 8 ) n0
  real ( kind = 8 ) n1
  real ( kind = 8 ) n2
  real ( kind = 8 ) t
  real ( kind = 8 ) x0
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) y(neqn)
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) yp(neqn)

  m0 = 5.0D+00
  m1 = 3.0D+00
  m2 = 4.0D+00

  x0 = y(1)
  y0 = y(2)

  x1 = y(5)
  y1 = y(6)

  x2 = y(9)
  y2 = y(10)

  n0 = sqrt ( ( ( x2 - x1 )**2 + ( y2 - y1 )**2 )**3 ) 
  n1 = sqrt ( ( ( x0 - x2 )**2 + ( y0 - y2 )**2 )**3 ) 
  n2 = sqrt ( ( ( x1 - x0 )**2 + ( y1 - y0 )**2 )**3 ) 

  yp(1)  =  y(3)
  yp(2)  =  y(4)
  yp(3)  = - m1 * ( x0 - x1 ) / n2 - m2 * ( x0 - x2 ) / n1
  yp(4)  = - m1 * ( y0 - y1 ) / n2 - m2 * ( y0 - y2 ) / n1
  yp(5)  =  y(7)
  yp(6)  =  y(8)
  yp(7)  = - m2 * ( x1 - x0 ) / n0 - m0 * ( x1 - x2 ) / n2
  yp(8)  = - m2 * ( y1 - y0 ) / n0 - m0 * ( y1 - y2 ) / n2
  yp(9)  = y(11)
  yp(10) = y(12)
  yp(11) = - m0 * ( x2 - x0 ) / n1 - m1 * ( x2 - x1 ) / n0
  yp(12) = - m0 * ( y2 - y0 ) / n1 - m1 * ( y2 - y1 ) / n0

  return
end
