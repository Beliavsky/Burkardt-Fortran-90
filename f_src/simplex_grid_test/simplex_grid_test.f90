program main

!*****************************************************************************80
!
!! MAIN is the main program for SIMPLEX_GRID_TEST.
!
!  Discussion:
!
!    SIMPLEX_GRID_TEST tests the SIMPLEX_GRID library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SIMPLEX_GRID_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SIMPLEX_GRID library.'

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
  write ( *, '(a)' ) 'SIMPLEX_GRID_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests SIMPLEX_GRID_SIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01:'
  write ( *, '(a)' ) '  SIMPLEX_GRID_SIZE counts the points in a regular grid'
  write ( *, '(a)' ) '  with N+1 points on a side, in an M-dimensional simplex.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        M: 0     1     2     3     4     5'
  write ( *, '(a)' ) '    N:'
  do n = 0, 10
    write ( *, '(a,i3,a)', advance = 'no' ) '  ', n, ':'
    do m = 0, 5
      call simplex_grid_size ( m, n, ng )
      write ( *, '(i6)', advance = 'no' ) ng
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests SIMPLEX_GRID_INDEX_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3

  integer ( kind = 4 ) g(m+1)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02:'
  write ( *, '(a)' ) '  SIMPLEX_GRID_INDEX_NEXT lists, one by one, the indices'
  write ( *, '(a)' ) '  of a simplex grid that uses N+1 points on a side, '
  write ( *, '(a)' ) '  in an M-dimensional simplex.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   #:  1  2  3  (*)'
  write ( *, '(a)' ) ''

  n = 3

  j = 0
  g(1:m) = 0
  g(m+1) = n
  
  do

    write ( *, '(2x,i2,a,3(i3),1x,a,i3,a)' ) j, ':', g(1:m), '(', g(m+1), ')'

    if ( g(1) == n ) then
      exit
    end if

    call simplex_grid_index_next ( m, n, g )

    j = j + 1

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests SIMPLEX_GRID_INDEX_SAMPLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 3

  integer ( kind = 4 ) g(m+1)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03:'
  write ( *, '(a)' ) '  SIMPLEX_GRID_INDEX_SAMPLE returns a randomly selected'
  write ( *, '(a)' ) '  index of a simplex grid that uses N+1 points on a side, '
  write ( *, '(a)' ) '  in an M-dimensional simplex.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   #:  1  2  3  (*)'
  write ( *, '(a)' ) ''

  n = 3
  seed = 123456789

  do j = 1, 20

    call simplex_grid_index_sample ( m, n, seed, g )

    write ( *, '(2x,i2,a,3(i3),1x,a,i3,a)' ) j, ':', g(1:m), '(', g(m+1), ')'

  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests SIMPLEX_GRID_INDEX_TO_POINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2

  integer ( kind = 4 ) g(m+1)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ), dimension ( m, m + 1 ) :: v = reshape ( (/ &
    20.0,  0.0, &
    30.0, 40.0, &
    10.0, 20.0 /), (/ m, m + 1 /) )
  real ( kind = 8 ) x(m)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04:'
  write ( *, '(a)' ) '  SIMPLEX_GRID_INDEX_TO_POINT returns the physical point'
  write ( *, '(a)' ) '  corresponding to a grid index of a simplex grid that '
  write ( *, '(a)' ) '  that uses N+1 points on a side, '
  write ( *, '(a)' ) '  in an M-dimensional simplex.'

  n = 5

  call r8mat_transpose_print ( m, m + 1, v, '  Simplex vertices:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Choosing random simplex indices to convert:'
  write ( *, '(a)' ) '   #:  1  2  3     X        Y'
  write ( *, '(a)' ) ''

  seed = 123456789

  do j = 1, 10

    call simplex_grid_index_sample ( m, n, seed, g )
    call simplex_grid_index_to_point ( m, n, 1, g, v, x )

    write ( *, '(2x,i2,a,3(i3),2(2x,f8.4))' ) &
      j, ':', g(1:m+1), x(1:m)

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests SIMPLEX_GRID_INDEX_ALL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: grid(:,:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST05:'
  write ( *, '(a)' ) '  SIMPLEX_GRID_INDEX_ALL returns all the indices'
  write ( *, '(a)' ) '  of a simplex grid that uses N+1 points on a side, '
  write ( *, '(a)' ) '  in an M-dimensional simplex.'

  m = 3
  n = 3
  call simplex_grid_size ( m, n, ng )

  allocate ( grid(m+1,ng) )

  call simplex_grid_index_all ( m, n, ng, grid )

  call i4mat_transpose_print ( m + 1, ng, grid, &
    '  Transposed Simplex Grid Index Matrix:' )

  deallocate ( grid )

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 tests SIMPLEX_GRID_INDEX_TO_POINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 2

  integer ( kind = 4 ), allocatable :: grid(:,:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ng
  real ( kind = 8 ), dimension ( m, m + 1 ) :: v = reshape ( (/ &
    20.0,  0.0, &
    30.0, 40.0, &
    10.0, 20.0 /), (/ m, m + 1 /) )
  real ( kind = 8 ), allocatable :: x(:,:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST06:'
  write ( *, '(a)' ) '  SIMPLEX_GRID_INDEX_TO_POINT returns the physical point'
  write ( *, '(a)' ) '  corresponding to a grid index of a simplex grid that '
  write ( *, '(a)' ) '  that uses N+1 points on a side, '
  write ( *, '(a)' ) '  in an M-dimensional simplex.'

  n = 5
  call simplex_grid_size ( m, n, ng )

  call r8mat_transpose_print ( m, m + 1, v, '  Simplex vertices:' )

  allocate ( grid(1:m+1,1:ng) )

  call simplex_grid_index_all ( m, n, ng, grid )

  allocate ( x(1:m,1:ng) )
  call simplex_grid_index_to_point ( m, n, ng, grid, v, x )

  call r8mat_transpose_print ( m, ng, x, '  Grid Point Coordinates:' )

  deallocate ( grid )
  deallocate ( x )

  return
end

