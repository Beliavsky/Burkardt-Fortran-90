program main

!*****************************************************************************80
!
!! MAIN is the main program for PADUA_TEST.
!
!  Discussion:
!
!    PADUA_TEST tests the PADUA library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PADUA_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the PADUA library.'

  call padua_order_test ( )
  call padua_plot_test ( )
  call padua_points_test ( )
  call padua_points_set_test ( )
  call padua_weights_test ( )
  call padua_weights_set_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PADUA_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine padua_order_test ( )

!*****************************************************************************80
!
!! PADUA_ORDER_TEST tests PADUA_ORDER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PADUA_ORDER_TEST'
  write ( *, '(a)' ) '  PADUA_ORDER converts the level L into the order N'
  write ( *, '(a)' ) '  of any Padua rule.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     L         N'
  write ( *, '(a)' ) ' '

  do l = 0, 10
    call padua_order ( l, n )
    write ( *, '(2x,i4,2x,i8)' ) l, n
  end do

  return
end
subroutine padua_plot_test ( )

!*****************************************************************************80
!
!! PADUA_PLOT_TEST tests PADUA_PLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 255 ) filename
  integer ( kind = 4 ) l
 
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PADUA_PLOT_TEST'
  write ( *, '(a)' ) '  PADUA_PLOT plots the Padua points.'

  filename = 'padua_00'

  do l = 0, 10
    call padua_plot ( l, filename )
    call filename_inc ( filename )
  end do

  return
end
subroutine padua_points_test ( )

!*****************************************************************************80
!
!! PADUA_POINTS_TEST tests PADUA_POINTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) l
  character ( len = 80 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: xy(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PADUA_POINTS_TEST'
  write ( *, '(a)' ) '  PADUA_POINTS returns the points of a Padua rule.'

  do l = 0, 10
    call padua_order ( l, n )
    allocate ( xy(1:2,1:n) )
    call padua_points ( l, xy )
    write ( label, '(a,i2,a)' ) '  Level ', l, ' Padua points:'
    call r8mat_transpose_print ( 2, n, xy, label )
    deallocate ( xy )
  end do

  return
end
subroutine padua_points_set_test ( )

!*****************************************************************************80
!
!! PADUA_POINTS_SET_TEST tests PADUA_POINTS_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: xy1(:,:)
  real ( kind = 8 ), allocatable :: xy2(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PADUA_POINTS_SET_TEST'
  write ( *, '(a)' ) '  PADUA_POINTS_SET looks up Padua points in a table.'
 
  do l = 3, 4
    call padua_order ( l, n )
    allocate ( xy1(1:2,1:n) )
    allocate ( xy2(1:2,1:n) )
    call padua_points ( l, xy1 )
    call padua_points_set ( l, xy2 )
    write ( *, '(a)' ) ' '
    write ( *, '(a,i1,a)' ) '  Level ', l, '  Padua points'
    write ( *, '(a)' ) ' '
    do j = 1, n
      write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) j, xy1(1,j), xy1(2,j)
      write ( *, '(2x,4x,2x,g14.6,2x,g14.6)' )    xy2(1,j), xy2(2,j)
    end do
    deallocate ( xy1 )
    deallocate ( xy2 )
  end do

  return
end
subroutine padua_weights_test ( )

!*****************************************************************************80
!
!! PADUA_WEIGHTS_TEST tests PADUA_WEIGHTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) l
  character ( len = 80 ) label
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PADUA_WEIGHTS_TEST'
  write ( *, '(a)' ) '  PADUA_WEIGHTS returns the weights of a Padua rule.'

  do l = 0, 10
    call padua_order ( l, n )
    allocate ( w(1:n) )
    call padua_weights ( l, w )
    write ( label, '(a,i2,a)' ) '  Level ', l, ' Padua weights:'
    call r8vec_print ( n, w, label )
    deallocate ( w )
  end do

  return
end
subroutine padua_weights_set_test ( )

!*****************************************************************************80
!
!! PADUA_WEIGHTS_SET_TEST tests PADUA_WEIGHTS_SET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) diff
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: w1(:)
  real ( kind = 8 ), allocatable :: w2(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PADUA_WEIGHTS_SET_TEST'
  write ( *, '(a)' ) '  PADUA_WEIGHTS_SET looks up Padua weights in a table.'
 
  do l = 3, 4
    call padua_order ( l, n )
    allocate ( w1(1:n) )
    allocate ( w2(1:n) )
    call padua_weights ( l, w1 )
    call padua_weights_set ( l, w2 )
    write ( *, '(a)' ) ' '
    write ( *, '(a,i1,a)' ) '  Level ', l, '  Padua points'
    write ( *, '(a)' ) ' '
    diff = 0.0D+00
    do j = 1, n
      write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) j, w1(j), w2(j)
      diff = max ( diff, abs ( w1(j) - w2(j) ) )
    end do
    write ( *, '(a)' ) ''
    write ( *, '(a,e10.2)' ) '  Maximum difference = ', diff
    deallocate ( w1 )
    deallocate ( w2 )
  end do

  return
end

