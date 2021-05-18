program main

!*****************************************************************************80
!
!! MAIN is the main program for HILBERT_CURVE_TEST.
!
!  Discussion:
!
!    HILBERT_CURVE_TEST tests the HILBERT_CURVE library.
!
!  Modified:
!
!    02 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HILBERT_CURVE_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the HILBERT_CURVE library.'

  call d2xy_test ( )
  call rot_test ( )
  call xy2d_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HILBERT_CURVE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine d2xy_test ( )

!*****************************************************************************80
!
!! D2XY_TEST tests D2XY.
!
!  Modified:
!
!    02 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'D2XY_TEST:'
  write ( *, '(a)' ) '  D2XY converts a Hilbert linear D coordinate to an (X,Y) 2D coordinate.'

  m = 3
  n = 2 ** m

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    D    X    Y'
  write ( *, '(a)' ) ''
  do d = 0, n * n - 1
    call d2xy ( m, d, x, y )
    write ( *, '(2x,i3,2x,i3,2x,i3)' ) d, x, y
  end do

  return
end
subroutine rot_test ( )

!*****************************************************************************80
!
!! ROT_TEST tests ROT.
!
!  Modified:
!
!    02 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rx
  integer ( kind = 4 ) ry
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x0
  integer ( kind = 4 ) x1
  integer ( kind = 4 ) y
  integer ( kind = 4 ) y0
  integer ( kind = 4 ) y1

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROT_TEST:'
  write ( *, '(a)' ) '  ROT rotates and flips a quadrant appropriately.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   X   Y  X0  Y0  X1  Y1'
  write ( *, '(a)' ) ''

  m = 3
  n = 2 ** m
  ry = 0

  do y = 0, 7
    do x = 0, 7
      rx = 0
      x0 = x
      y0 = y
      call rot ( n, x0, y0, rx, ry )
      rx = 1
      x1 = x
      y1 = y
      call rot ( n, x1, y1, rx, ry )
      write ( *, '(2x,i2,2x,i2,2x,i2,2x,i2,2x,i2,2x,i2)' ) x, y, x0, y0, x1, y1
    end do
  end do

  return
end
subroutine xy2d_test ( )

!*****************************************************************************80
!
!! XY2D_TEST tests XY2D.
!
!  Modified:
!
!    01 January 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'XY2D_TEST:'
  write ( *, '(a)' ) '  XY2D converts an (X,Y) 2D coordinate to a Hilbert linear D coordinate.'

  m = 3
  n = 2 ** m

  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) '        '
  do x = 0, n - 1
    write ( *, '(i3)', advance = 'no' ) x
  end do
  write ( *, '(a)' ) ''

  write ( *, '(a)' ) ''
  do y = n - 1, 0, -1
    write ( *, '(2x,i3,a)', advance = 'no' ) y, ':  '
    do x = 0, n - 1
      call xy2d ( m, x, y, d )
      write ( *, '(i3)', advance = 'no' ) d
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
