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
  logical ( kind = 4 ) lopen

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
subroutine r8mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    write ( *, '(a)' ) ' '

    do i = i2lo, i2hi
      i2 = i + 1 - i2lo
      write ( ctemp(i2), '(i8,6x)' ) i
    end do

    write ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc
        i = i2lo - 1 + i2
        write ( ctemp(i2), '(g14.6)' ) a(i,j)
      end do

      write ( *, '(i5,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

    end do

  end do

  return
end
subroutine sphere_cubed_grid_ijk_to_xyz ( n, i, j, k, xyz )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_IJK_TO_XYZ: cubed sphere IJK to XYZ coordinates.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of sections into which each 
!    face of the cube is to be divided.
!
!    Input, integer ( kind = 4 ) I, J, K, indices between 0 and N.  Normally,
!    at least one of the indices should have the value 0 or N.
!
!    Output, real ( kind = 8 ) XYZ(3), coordinates of the point.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) xc
  real ( kind = 8 ) xyz(3)
  real ( kind = 8 ) xyzn
  real ( kind = 8 ) yc
  real ( kind = 8 ) zc

  if ( i == 0 ) then
    xc = -1.0D+00
  else if ( i == n ) then
    xc = +1.0D+00
  else
    xc = tan ( real ( 2 * i - n, kind = 8 ) * 0.25D+00 * r8_pi &
      / real ( n, kind = 8 ) )
  end if

  if ( j == 0 ) then
    yc = -1.0D+00
  else if ( j == n ) then
    yc = +1.0D+00
  else
    yc = tan ( real ( 2 * j - n, kind = 8 ) * 0.25D+00 * r8_pi &
      / real ( n, kind = 8 ) )
  end if

  if ( k == 0 ) then
    zc = -1.0D+00
  else if ( k == n ) then
    zc = +1.0D+00
  else
    zc = tan ( real ( 2 * k - n, kind = 8 ) * 0.25D+00 * r8_pi &
      / real ( n, kind = 8 ) )
  end if

  xyzn = sqrt ( xc ** 2 + yc ** 2 + zc ** 2 )

  xyz(1) = xc / xyzn
  xyz(2) = yc / xyzn
  xyz(3) = zc / xyzn

  return
end
subroutine sphere_cubed_grid_line_count ( n, line_num )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_LINE_COUNT counts lines on a cubed sphere grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of sections into which each 
!    face of the cube is to be divided.
!
!    Output, integer ( kind = 4 ) LINE_NUM, the number of lines.
!
  implicit none

  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) n

  line_num = 0
!
!  If N = 1, the corners form 12 lines.
!
  if ( n == 1 ) then
    line_num = 12
    return
!
!  If 1 < N, each of 8 corners connects to three neighboring edges.
!
  else
    line_num = line_num + 8 * 3
  end if
!
!  If 2 < N, then each of the 12 edges includes lines.
!
  if ( 2 < n ) then
    line_num = line_num + 12 * ( n - 2 )
  end if
!
!  Lines that belong to one of the six faces.
!
  if ( 1 < n ) then
    line_num = line_num + 6 * 2 * n * ( n - 1 )
  end if

  return
end
subroutine sphere_cubed_grid_lines ( n, line_num, line_data )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_LINES computes the lines on a cubed sphere grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of sections into which each face 
!    of the cube is to be divided.
!
!    Input, integer ( kind = 4 ) LINE_NUM, the number of lines.
!
!    Output, real ( kind = 8 ) LINE_DATA(3,2,LINE_NUM), for each line I, the 
!    X/Y/Z coordinates of the start and end of a line segment on the grid.
!
  implicit none

  integer ( kind = 4 ) line_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  real ( kind = 8 ) line_data(3,2,line_num)
  integer ( kind = 4 ) n

  l = 0
!
!  If N = 1, the corners form 12 lines.
!
  if ( n == 1 ) then

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,2,l) )

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,2,l) )

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n, line_data(1:3,2,l) )
    return
!
!  If 1 < N, each of 8 corners connects to three neighboring edges.
!
  else

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 1, 0, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 1, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 1, line_data(1:3,2,l) )

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n,   0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n-1, 0, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 1, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 1, line_data(1:3,2,l) )

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n,   n, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n-1, n, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n,   0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n-1, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 1, line_data(1:3,2,l) )

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 1, n, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n,   0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-1, 0, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 1, line_data(1:3,2,l) )

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 1, 0, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 1, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n,   line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n-1, line_data(1:3,2,l) )

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n,   0, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n-1, 0, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 1, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n,   line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n-1, line_data(1:3,2,l) )

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n,   n, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n-1, n, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n,   n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n-1, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n,   line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n-1, line_data(1:3,2,l) )

    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 1, n, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n,   n, line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-1, n, line_data(1:3,2,l) )
    l = l + 1
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n,   line_data(1:3,1,l) )
    call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n-1, line_data(1:3,2,l) )

  end if
!
!  If 2 < N, then each of the 12 edges includes lines.
!
  if ( 2 < n ) then

    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, i,   0, 0, line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, i+1, 0, 0, line_data(1:3,2,l) )
    end do
    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, n,   i, 0, line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, n, i+1, 0, line_data(1:3,2,l) )
    end do
    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, n-i,   n, 0, line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, n-i-1, n, 0, line_data(1:3,2,l) )
    end do
    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-i,   0, line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-i-1, 0, line_data(1:3,2,l) )
    end do

    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, i,   0, n, line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, i+1, 0, n, line_data(1:3,2,l) )
    end do
    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, n,   i, n, line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, n, i+1, n, line_data(1:3,2,l) )
    end do
    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, n-i,   n, n, line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, n-i-1, n, n, line_data(1:3,2,l) )
    end do
    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-i,   n, line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-i-1, n, line_data(1:3,2,l) )
    end do

    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, i,   line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, i+1, line_data(1:3,2,l) )
    end do
    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, i,   line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, i+1, line_data(1:3,2,l) )
    end do
    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, n, n, i,   line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, n, n, i+1, line_data(1:3,2,l) )
    end do
    do i = 1, n - 2
      l = l + 1
      call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, i,   line_data(1:3,1,l) )
      call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, i+1, line_data(1:3,2,l) )
    end do

  end if
!
!  Lines that belong to one of the six faces.
!
  if ( 1 < n ) then
!
!  000 : nn0
!
    do i = 1, n - 1
      do j = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, i, j,   0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, i, j+1, 0, line_data(1:3,2,l) )
      end do
    end do
    do j = 1, n - 1
      do i = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, i,   j, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, i+1, j, 0, line_data(1:3,2,l) )
      end do
    end do
!
!  00n : nnn
!
    do i = 1, n - 1
      do j = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, i, j,   n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, i, j+1, n, line_data(1:3,2,l) )
      end do
    end do
    do j = 1, n - 1
      do i = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, i,   j, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, i+1, j, n, line_data(1:3,2,l) )
      end do
    end do
!
!  000:n0n
!
    do i = 1, n - 1
      do j = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, i, 0, j,   line_data(1:3,1,l)   )
        call sphere_cubed_grid_ijk_to_xyz ( n, i, 0, j+1, line_data(1:3,2,l) )
      end do
    end do
    do j = 1, n - 1
      do i = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, i,   0, j, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, i+1, 0, j, line_data(1:3,2,l) )
      end do
    end do
!
!  0n0:nnn
!
    do i = 1, n - 1
      do j = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, i, n, j,   line_data(1:3,1,l)   )
        call sphere_cubed_grid_ijk_to_xyz ( n, i, n, j+1, line_data(1:3,2,l) )
      end do
    end do
    do j = 1, n - 1
      do i = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, i,   n, j, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, i+1, n, j, line_data(1:3,2,l) )
      end do
    end do
!
!  000:0nn
!
    do i = 1, n - 1
      do j = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, i, j,   line_data(1:3,1,l)   )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, i, j+1, line_data(1:3,2,l) )
      end do
    end do
    do j = 1, n - 1
      do i = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, i,   j, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, i+1, j, line_data(1:3,2,l) )
      end do
    end do
!
!  n00:nnn
!
    do i = 1, n - 1
      do j = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, i, j,   line_data(1:3,1,l)   )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, i, j+1, line_data(1:3,2,l) )
      end do
    end do
    do j = 1, n - 1
      do i = 0, n - 1
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, i,   j, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, i+1, j, line_data(1:3,2,l) )
      end do
    end do

  end if

  if ( l /= line_num ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_LINES - Fatal error!'
    write ( *, '(a,i6)' ) '  LINE_NUM = ', line_num
    write ( *, '(a,i6)' ) '  L = ', l
    stop 1
  end if

  return
end
subroutine sphere_cubed_grid_lines_display ( line_num, line_data, prefix )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_LINES_DISPLAY displays a cubed grid on a sphere.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NG, the number of points.
!
!    Input, real ( kind = 8 ) XG(3,NG), the Fibonacci spiral points.
!
!    Input, integer ( kind = 4 ) LINE_NUM, the number of grid lines.
!
!    Input, real ( kind = 8 ) LINE_DATA(3,2,LINE_NUM), contains pairs of 
!    point indices for line segments that make up the grid.
!
!    Input, character ( len = * ) PREFIX, a prefix for the filenames.
!
  implicit none

  integer ( kind = 4 ) line_num

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  integer ( kind = 4 ) l
  real ( kind = 8 ) line_data(3,2,line_num)
  character ( len = 255 ) line_filename
  integer ( kind = 4 ) line_unit
  character ( len = 255 ) plot_filename
  character ( len = * ) prefix
!
!  Create graphics data files.
!
  call get_unit ( line_unit )
  line_filename = trim ( prefix ) // '_lines.txt'
  open ( unit = line_unit, file = line_filename, status = 'replace' )
  do l = 1, line_num
    if ( 1 < l ) then
      write ( line_unit, '(a)' ) ''
      write ( line_unit, '(a)' ) ''
    end if
    write ( line_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) line_data(1:3,1,l)
    write ( line_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) line_data(1:3,2,l)
  end do
  close ( unit = line_unit )
  write ( *, '(a)' ) '  Created line file "' // trim ( line_filename ) // '".'
!
!  Create graphics command file.
!
  call get_unit ( command_unit )
  command_filename = trim ( prefix ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  plot_filename = trim ( prefix ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( prefix ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set key off'
  write ( command_unit, '(a)' ) 'set style data points'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set view equal xyz'
  write ( command_unit, '(a)' ) 'splot "' // &
    trim ( line_filename ) // &
    '" with lines lw 3'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
subroutine sphere_cubed_grid_point_count ( n, ns )

!*****************************************************************************80
!
!! SPHERE_CUBED_POINT_COUNT counts the points on a cubed sphere grid.
!
!  Discussion:
!
!    For a value of N = 3, for instance, each of the 6 cube faces will
!    be divided into 3 sections, so that a single cube face will have
!    (3+1)x(3+1) points:
!
!      X---X---X---X
!      | 1 | 4 | 7 |
!      X---X---X---X
!      | 2 | 5 | 8 |
!      X---X---X---X
!      | 3 | 6 | 9 |
!      X---X---X---X
!
!    The number of points is simply (N+1)^3 - (N-1)^3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of sections into which 
!    each face of the cube is to be divided.
!
!    Output, integer ( kind = 4 ) NS, the number of points.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns

  ns = ( n + 1 ) ** 3 - ( n - 1 ) ** 3

  return
end
subroutine sphere_cubed_grid_points ( n, ns, xyz )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_POINTS computes the points on a cubed sphere grid.
!
!  Discussion:
!
!    For a value of N = 3, for instance, each of the 6 cube faces will
!    be divided into 3 sections, so that a single cube face will have
!    (3+1)x(3+1) points:
!
!      X---X---X---X
!      | 1 | 4 | 7 |
!      X---X---X---X
!      | 2 | 5 | 8 |
!      X---X---X---X
!      | 3 | 6 | 9 |
!      X---X---X---X
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of sections into which each 
!    face of the cube is to be divided.
!
!    Input, integer ( kind = 4 ) NS, the number of points.
!
!    Output, real ( kind = 8 ) XYZ(3,NS), distinct points on the unit sphere
!    generated by a cubed sphere grid.
!
  implicit none

  integer ( kind = 4 ) ns

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns2
  real ( kind = 8 ) xyz(3,ns)

  ns2 = 0
!
!  Bottom full.
!
  call sphere_cubed_grid_points_face ( n, 0, 0, 0, n, n, 0, ns2, xyz )
!
!  To avoid repetition, draw the middles as grids of n-2 x n-1 points.
!
  call sphere_cubed_grid_points_face ( n, 0, 0, 1, 0,   n-1, n-1, ns2, xyz )
  call sphere_cubed_grid_points_face ( n, 0, n, 1, n-1, n,   n-1, ns2, xyz )
  call sphere_cubed_grid_points_face ( n, n, 1, 1, n,   n,   n-1, ns2, xyz )
  call sphere_cubed_grid_points_face ( n, 1, 0, 1, n,   0,   n-1, ns2, xyz )
!
!  Top full.
!
  call sphere_cubed_grid_points_face ( n, 0, 0, n, n, n, n, ns2, xyz )
  
  if ( ns2 /= ns ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINTS - Fatal error!'
    write ( *, '(a,i8,a)' ) '  Expected to generated NS = ', ns, ' points.'
    write ( *, '(a,i8,a)' ) '  Generated ', ns2, ' points.'
    stop
  end if

  return
end
subroutine sphere_cubed_grid_points_display ( ng, xg, prefix )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_POINTS_DISPLAY displays an LLQ grid on a sphere.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NG, the number of points.
!
!    Input, real ( kind = 8 ) XG(3,NG), the Fibonacci spiral points.
!
!    Input, character ( len = * ) PREFIX, a prefix for the filenames.
!
  implicit none

  integer ( kind = 4 ) ng

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  integer ( kind = 4 ) j
  character ( len = 255 ) node_filename
  integer ( kind = 4 ) node_unit
  character ( len = 255 ) plot_filename
  character ( len = * ) prefix
  real ( kind = 8 ) xg(3,ng)
!
!  Create graphics data files.
!
  call get_unit ( node_unit )
  node_filename = trim ( prefix ) // '_nodes.txt'
  open ( unit = node_unit, file = node_filename, status = 'replace' )
  do j = 1, ng
    write ( node_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xg(1:3,j)
  end do
  close ( unit = node_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created node file "' // trim ( node_filename ) // '".'
!
!  Create graphics command file.
!
  call get_unit ( command_unit )
  command_filename = trim ( prefix ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  plot_filename = trim ( prefix ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) 'set title "' // trim ( prefix ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set key off'
  write ( command_unit, '(a)' ) 'set style data points'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'set view equal xyz'
  write ( command_unit, '(a)' ) 'splot "' // &
    trim ( node_filename ) // '" with points pt 7 lt 0'
  write ( command_unit, '(a)' ) 'quit'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
subroutine sphere_cubed_grid_points_face ( n, i1, j1, k1, i2, j2, k2, ns, xyz )

!*****************************************************************************80
!
!! SPHERE_CUBED_GRID_POINTS_FACE: points on one face of a cubed sphere grid.
!
!  Discussion:
!
!    This routine starts with NS = 0, and is called repeatedly to
!    add points for another face.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of sections into which each face 
!    of the cube is to be divided.
!
!    Input, integer ( kind = 4 ) I1, J1, K1, I2, J2, K2, the logical indices, 
!    between 0 and N, of two corners of the face grid.  It is guaranteed that 
!    I1 <= I2, J1 <= J2, and K1 <= K2.  
!
!    Input/output, integer ( kind = 4 ) NS, the number of points.
!
!    Input/output, real XYZ(3,NS), distinct points on the unit sphere
!    generated by a cubed sphere grid.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) k
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) k2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) xyz(3,*)
  real ( kind = 8 ) xyzn
  real ( kind = 8 ) xc
  real ( kind = 8 ) yc
  real ( kind = 8 ) zc

  do i = i1, i2

    if ( i1 < i2 ) then
      xc = tan ( real ( 2 * i - n, kind = 8 ) * 0.25D+00 * r8_pi &
        / real ( n, kind = 8 ) )
    else if ( i1 == 0 ) then
      xc = -1.0D+00
    else if ( i1 == n ) then
      xc = +1.0D+00
    else
      xc = 0.0D+00
    end if

    do j = j1, j2

      if ( j1 < j2 ) then
        yc = tan ( real ( 2 * j - n, kind = 8 ) * 0.25D+00 * r8_pi &
          / real ( n, kind = 8 ) )
      else if ( j1 == 0 ) then
        yc = -1.0D+00
      else if ( j1 == n ) then
        yc = +1.0D+00
      else
        yc = 0.0D+00
      end if

      do k = k1, k2

        if ( k1 < k2 ) then
          zc = tan ( real ( 2 * k - n, kind = 8 ) * 0.25D+00 * r8_pi &
            / real ( n, kind = 8 ) )
        else if ( k1 == 0 ) then
          zc = -1.0D+00
        else if ( k1 == n ) then
          zc = +1.0D+00
        else
          zc = 0.0D+00
        end if

        xyzn = sqrt ( xc ** 2 + yc ** 2 + zc ** 2 )

        ns = ns + 1
        xyz(1,ns) = xc / xyzn
        xyz(2,ns) = yc / xyzn
        xyz(3,ns) = zc / xyzn

      end do
    end do
  end do

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
