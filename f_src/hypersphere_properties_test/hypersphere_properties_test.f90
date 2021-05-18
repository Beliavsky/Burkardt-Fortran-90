program main

!*****************************************************************************80
!
!! MAIN is the main program for HYPERSPHERE_PROPERTIES_TEST.
!
!  Discussion:
!
!    HYPERSPHERE_PROPERTIES_TEST tests the HYPERSPHERE_PROPERTIES library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HYPERSPHERE_PROPERTIES_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the HYPERSPHERE_PROPERTIES library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
  call test06 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HYPERSPHERE_PROPERTIES_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests the coordinate conversion routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: c(:)
  real ( kind = 8 ) err
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)
  real ( kind = 8 ) r8mat_norm_fro_affine
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable :: theta(:,:)
  real ( kind = 8 ), allocatable :: x(:,:)
  real ( kind = 8 ), allocatable :: x2(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Test the coordinate conversion routines:'
  write ( *, '(a)' ) '  CARTESIAN_TO_HYPERSPHERE: X       -> R,Theta'
  write ( *, '(a)' ) '  HYPERSPHERE_TO_CARTESIAN: R,Theta -> X.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Pick a random X, and compute X2 by converting X'
  write ( *, '(a)' ) '  to hypersphere and back.  Consider norm of difference.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  M    || X - X2 ||'

  seed = 123456789

  n = 1

  allocate ( r(n) )

  do m = 1, 5

    write ( *, '(a)' ) ''

    allocate ( x(1:m,1:n) )
    allocate ( x2(1:m,1:n) )
    allocate ( c(1:m) )
    allocate ( theta(m-1,n) )

    do test = 1, 5
      call r8mat_uniform_01 ( m, n, seed, x )
      call r8vec_uniform_01 ( m, seed, c )
      call cartesian_to_hypersphere ( m, n, c, x, r, theta )
      call hypersphere_to_cartesian ( m, n, c, r, theta, x2 )
      err = r8mat_norm_fro_affine ( m, n, x, x2 )
      write ( *, '(2x,i2,2x,g14.6)' ) m, err
    end do

    deallocate ( c )
    deallocate ( theta )
    deallocate ( x )
    deallocate ( x2 )

  end do

  deallocate ( r )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests HYPERSPHERE_01_SURFACE_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  HYPERSPHERE_01_SURFACE_UNIFORM samples uniformly from the'
  write ( *, '(a)' ) '  surface of the unit hypersphere'

  seed = 123456789

  n = 1
  do m = 1, 5
    allocate ( x(1:m,1:n) )
    do test = 1, 3
      call hypersphere_01_surface_uniform ( m, n, seed, x )
      call r8vec_transpose_print ( m, x, '  Random hypersphere point:' )
    end do
    deallocate ( x )
  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests HYPERSPHERE_01_AREA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) area
  real ( kind = 8 ) area2
  real ( kind = 8 ) hypersphere_01_area
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n_data

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  HYPERSPHERE_01_AREA evaluates the area of the unit'
  write ( *, '(a)' ) '  hypersphere in M dimensions.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       M      Exact       Computed'
  write ( *, '(a)' ) '              Area        Area'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call hypersphere_01_area_values ( n_data, m, area )

    if ( n_data == 0 ) then
      exit
    end if

    area2 = hypersphere_01_area ( m )

    write ( *, '(2x,i6,2x,f10.4,2x,f10.4)' ) m, area, area2

  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests HYPERSPHERE_01_VOLUME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) hypersphere_01_volume
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) volume
  real ( kind = 8 ) volume2

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04:'
  write ( *, '(a)' ) '  HYPERSPHERE_01_VOLUME evaluates the area of the unit'
  write ( *, '(a)' ) '  hypersphere in M dimensions.'
  write ( *, '(a)' ) '  HYPERSPHERE_01_VOLUME_VALUES returns some test values.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '       M      Exact       Computed'
  write ( *, '(a)' ) '              Volume      Volume'
  write ( *, '(a)' ) ''

  n_data = 0

  do

    call hypersphere_01_volume_values ( n_data, m, volume )

    if ( n_data == 0 ) then
      exit
    end if

    volume2 = hypersphere_01_volume ( m )

    write ( *, '(2x,i6,2x,f10.4,2x,f10.4)' ) m, volume, volume2

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests HYPERSPHERE_AREA, HYPERSPHERE_VOLUME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) area
  real ( kind = 8 ) hypersphere_area
  real ( kind = 8 ) hypersphere_volume
  integer ( kind = 4 ) m
  real ( kind = 8 ) r
  real ( kind = 8 ) volume

  r = 1.5D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  For a hypersphere in M dimensions:'
  write ( *, '(a)' ) '  HYPERSPHERE_AREA computes the area'
  write ( *, '(a)' ) '  HYPERSPHERE_VOLUME computes the volume.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Notice that both quantities eventually decrease!'
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  We use a radius of R = ', r
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    M        Area          Volume    Area / Volume '
  write ( *, '(a)' ) ''

  do m = 1, 20
    area = hypersphere_area ( m, r )
    volume = hypersphere_volume ( m, r )
    write ( *, '(2x,i3,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      m, area, volume, area / volume
  end do

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 tests the stereographic mapping.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) err
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8mat_norm_fro_affine
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable :: x1(:,:)
  real ( kind = 8 ), allocatable :: x2(:,:)
  real ( kind = 8 ), allocatable :: x3(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  Test the stereographic mapping:'
  write ( *, '(a)' ) '  HYPERSPHERE_STEREOGRAPH maps hypersphere points to the plane.'
  write ( *, '(a)' ) '  HYPERSPHERE_STEREOGRAPH_INVERSE inverts the mapping.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Pick a random X1 on the hypersphere.'
  write ( *, '(a)' ) '  Map it to a point X2 on the plane.'
  write ( *, '(a)' ) '  Map it back to a point X3 on the hypersphere.'
  write ( *, '(a)' ) '  Consider norm of difference.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  M    || X1 - X3 ||'

  seed = 123456789

  n = 1
  do m = 2, 5 
    write ( *, '(a)' ) ''
    allocate ( x1(m,n) )
    allocate ( x2(m-1,n) )
    allocate ( x3(m,n) )
    do test = 1, 5
      call hypersphere_01_surface_uniform ( m, n, seed, x1 )
      call hypersphere_stereograph ( m, n, x1, x2 )
      call hypersphere_stereograph_inverse ( m, n, x2, x3 )
      err = r8mat_norm_fro_affine ( m, n, x1, x3 )
      write ( *, '(2x,i2,2x,g14.6)' ) m, err
    end do
    deallocate ( x1 )
    deallocate ( x2 )
    deallocate ( x3 )
  end do

  return
end

