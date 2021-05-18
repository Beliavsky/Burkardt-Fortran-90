program main

!*****************************************************************************80
!
!! MAIN is the main program for SPHERE_LLQ_GRID_TEST.
!
!  Discussion:
!
!    SPHERE_LLQ_GRID_TEST tests the SPHERE_LLQ_GRID library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_LLQ_GRID_TEST'
  write ( *, '(a)' ) '  MATLAB version'
  write ( *, '(a)' ) '  Test the SPHERE_LLQ_GRID library.'

  call sphere_llq_grid_point_count_test ( )
  call sphere_llq_grid_points_test ( )
  call sphere_llq_grid_line_count_test ( )
  call sphere_llq_grid_lines_test ( )
  call sphere_llq_grid_display_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_LLQ_GRID_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine sphere_llq_grid_point_count_test ( )

!*****************************************************************************80
!
!! SPHERE_LLQ_GRID_POINT_COUNT_TEST tests SPHERE_LLQ_GRID_POINT_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ) long_log
  integer ( kind = 4 ) long_num
  integer ( kind = 4 ) point_num

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_LLQ_GRID_POINT_COUNT_TEST'
  write ( *, '(a)' ) '  SPHERE_LLQ_GRID_POINT_COUNT counts the points used for a'
  write ( *, '(a)' ) '  grid based on quadrilaterals defined by latitude and longitude'
  write ( *, '(a)' ) '  lines on a sphere in 3D.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     LAT_NUM    LONG_NUM   POINT_NUM'

  do lat_num = 1, 17, 2
    write ( *, '(a)' ) ''
    long_num = 1
    do long_log = 1, 4
      long_num = long_num * 2
      call sphere_llq_grid_point_count ( lat_num, long_num, point_num )
      write ( *, '(2x,i8,2x,i8,2x,i8)' ) lat_num, long_num, point_num
    end do
  end do

  return
end
subroutine sphere_llq_grid_points_test ( )

!*****************************************************************************80
!
!! SPHERE_LLQ_GRID_POINTS_TEST tests SPHERE_LLQ_GRID_POINTS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ) long_num
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), allocatable :: node_xyz(:,:)
  real ( kind = 8 ), dimension ( 3 ) :: pc = (/ &
    0.0D+00, 0.0D+00, 0.0D+00 /)
  real ( kind = 8 ) r

  lat_num = 3
  long_num = 4

  r = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_LLQ_GRID_POINTS_TEST'
  write ( *, '(a)' ) '  SPHERE_LLQ_POINTS produces latitude/longitude'
  write ( *, '(a)' ) '  points on a sphere in 3D.'

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Radius = ', r

  call r8vec_print ( 3, pc, '  Center:' )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The number of latitudes =  ', lat_num
  write ( *, '(a,i4)' ) '  The number of longitudes = ', long_num

  call sphere_llq_grid_point_count ( lat_num, long_num, node_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The number of grid points is ', node_num

  allocate ( node_xyz(1:3,1:node_num) )

  call sphere_llq_grid_points ( r, pc, lat_num, long_num, node_num, node_xyz )

  write ( *, '(a)' ) ''

  k = 1
  write ( *, '(2x,i8)' ) k
  write ( *, '(3g14.6)' ) node_xyz(1:3,k)

  write ( *, '(a)' ) ''

  do i = 1, lat_num
    write ( *, '(a)' ) ''
    do j = 0, long_num - 1
      k = k + 1
      write ( *, '(2x,i8)' ) k
      write ( *, '(3g14.6)' ) node_xyz(1:3,k)
      write ( *, '(a)' ) ''
    end do
  end do

  write ( *, '(a)' ) ''

  k = k + 1
  write ( *, '(2x,i8)' ) k
  write ( *, '(3g14.6)' ) node_xyz(1:3,k)
  write ( *, '(a)' ) ''

  deallocate ( node_xyz )

  return
end
subroutine sphere_llq_grid_line_count_test ( )

!*****************************************************************************80
!
!! SPHERE_LLQ_GRID_LINE_COUNT_TEST tests SPHERE_LLQ_GRID_LINE_COUNT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) long_log
  integer ( kind = 4 ) long_num

  lat_num = 3
  long_num = 4

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_LLQ_GRID_LINE_COUNT_TEST'
  write ( *, '(a)' ) '  SPHERE_LLQ_GRID_LINE_COUNT counts the lines used for a'
  write ( *, '(a)' ) '  grid based on quadrilaterals defined by latitude and longitude'
  write ( *, '(a)' ) '  lines on a sphere in 3D.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     LAT_NUM    LONG_NUM   LINE_NUM'

  do lat_num = 1, 17, 2
    write ( *, '(a)' ) ''
    long_num = 1
    do long_log = 1, 4
      long_num = long_num * 2
      call sphere_llq_grid_line_count ( lat_num, long_num, line_num )
      write ( *, '(2x,i8,2x,i8,2x,i8)' ) lat_num, long_num, line_num
    end do
  end do

  return
end
subroutine sphere_llq_grid_lines_test ( )

!*****************************************************************************80
!
!! SPHERE_LLQ_GRID_LINES_TEST tests SPHERE_LLQ_GRID_LINES.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ), allocatable :: line_data(:,:)
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) long_num

  lat_num = 3
  long_num = 4

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_LLQ_GRID_LINES_TEST'
  write ( *, '(a)' ) '  SPHERE_LLQ_GRID_LINES computes grid lines'
  write ( *, '(a)' ) '  on a sphere in 3D.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of latitudes is  ', lat_num
  write ( *, '(a,i4)' ) '  Number of longitudes is ', long_num

  call sphere_llq_grid_line_count ( lat_num, long_num, line_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of line segments is ', line_num

  allocate ( line_data(2,line_num) )

  call sphere_llq_grid_lines ( lat_num, long_num, line_num, line_data )

  call i4mat_transpose_print ( 2, line_num, line_data, &
    '  Grid line vertices:' );

  deallocate ( line_data )

  return
end
subroutine sphere_llq_grid_display_test ( )

!*****************************************************************************80
!
!! SPHERE_LLQ_GRID_DISPLAY_TEST tests SPHERE_LLQ_GRID_DISPLAY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) lat_num
  integer ( kind = 4 ), allocatable :: line_data(:,:)
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) long_num
  integer ( kind = 4 ) node_num
  real ( kind = 8 ), allocatable :: node_xyz(:,:)
  real ( kind = 8 ), dimension ( 3 ) :: pc = (/ &
    0.0D+00, 0.0D+00, 0.0D+00 /)
  character ( len = 255 ) prefix
  real ( kind = 8 ) r

  lat_num = 10
  long_num = 12

  r = 10.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SPHERE_LLQ_GRID_DISPLAY_TEST'
  write ( *, '(a)' ) '  SPHERE_LLQ_GRID_DISPLAY displays an LLQ grid on a sphere.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of latitudes is  ', lat_num
  write ( *, '(a,i4)' ) '  Number of longitudes is ', long_num
!
!  Get points.
!
  call sphere_llq_grid_point_count ( lat_num, long_num, node_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The number of grid points is ', node_num

  allocate ( node_xyz(1:3,1:node_num) )

  call sphere_llq_grid_points ( r, pc, lat_num, long_num, node_num, node_xyz )
!
!  Get lines.
!
  call sphere_llq_grid_line_count ( lat_num, long_num, line_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of line segments is ', line_num

  allocate ( line_data(1:2,1:line_num) )

  call sphere_llq_grid_lines ( lat_num, long_num, line_num, line_data )

  prefix = 'sphere_llq_grid'

  call sphere_llq_grid_display ( node_num, node_xyz, line_num, line_data, &
    prefix )

  deallocate ( line_data )
  deallocate ( node_xyz )

  return
end
