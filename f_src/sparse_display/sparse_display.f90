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
subroutine spy_file ( header, data_filename )

!*****************************************************************************80
!
!! SPY_FILE plots a sparsity pattern stored in a file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) HEADER, the name to be used for the
!    title of the plot, and as part of the names of the command
!    and plot files.
!
!    Input, character ( len = * ) DATA_FILENAME, the name of the file
!    containing the indices of nonzero matrix entries.
!
  implicit none

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_status
  integer ( kind = 4 ) data_unit
  character ( len = * ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) m0
  character ( len = 6 ) m0_s
  integer ( kind = 4 ) m1
  character ( len = 6 ) m1_s
  integer ( kind = 4 ) n0
  character ( len = 6 ) n0_s
  integer ( kind = 4 ) n1
  character ( len = 6 ) n1_s
  integer ( kind = 4 ) nz_num
  character ( len = 255 ) png_filename

  n0 = + i4_huge
  n1 = - i4_huge
  m0 = + i4_huge
  m1 = - i4_huge
  nz_num = 0

  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'old' )

  do

    read ( data_unit, *, iostat = data_status ) i, j

    if ( data_status /= 0 ) then
      exit
    end if

    nz_num = nz_num + 1
    m0 = min ( m0, i )
    m1 = max ( m1, i )
    n0 = min ( n0, j )
    n1 = max ( n1, j )

  end do

  close ( unit = data_unit )
!
!  Create command file.
!
  command_filename = trim ( header ) // '_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set term png'

  png_filename = trim ( header ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( png_filename ) // '"'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) 'set xlabel "<--- J --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- I --->"'
  
  write ( command_unit, '(a,i6,a)' ) &
    'set title "', nz_num, ' nonzeros for ''' // trim ( header ) // '''"'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( n0_s, '(i6)' ) n0
  write ( n1_s, '(i6)' ) n1
  write ( m0_s, '(i6)' ) m0
  write ( m1_s, '(i6)' ) m1
  m0_s = adjustl ( m0_s )
  m1_s = adjustl ( m1_s )
  n0_s = adjustl ( n0_s )
  n1_s = adjustl ( n1_s )
  write ( command_unit, '(a)' ) &
    'plot [y=' // trim ( m0_s ) // ':' // trim ( m1_s ) // &
       '] [x=' // trim ( n0_s ) // ':' // trim ( n1_s ) // ' ] "' // &
    trim ( data_filename ) // '" with points pt 5'

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

  return
end
subroutine spy_ge ( m, n, a, header )

!*****************************************************************************80
!
!! SPY_GE plots a sparsity pattern for a general storage (GE) matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the matrix.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) HEADER, the name to be used for the
!    title of the plot, and as part of the names of the data, command
!    and plot files.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = 6 ) m_s
  character ( len = 6 ) n_s
  integer ( kind = 4 ) nz_num
  character ( len = 255 ) png_filename
!
!  Create data file.
!
  data_filename = trim ( header ) // '_data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  nz_num = 0
  do j = 1, n
    do i = 1, m
      if ( a(i,j) /= 0.0D+00 ) then
        write ( data_unit, '(2x,i6,2x,i6)' ) j, i
        nz_num = nz_num + 1
      end if
    end do
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created sparsity data file "' // trim ( data_filename ) // '".'
!
!  Create command file.
!
  command_filename = trim ( header ) // '_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set term png'

  png_filename = trim ( header ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( png_filename ) // '"'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) 'set xlabel "<--- J --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- I --->"'
  
  write ( command_unit, '(a,i6,a)' ) &
    'set title "', nz_num, ' nonzeros for ''' // trim ( header ) // '''"'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( n_s, '(i6)' ) n
  write ( m_s, '(i6)' ) m
  m_s = adjustl ( m_s )
  n_s = adjustl ( n_s )
  write ( command_unit, '(a)' ) &
    'plot [x=1:' // trim ( n_s) // '] [y=' // trim ( m_s ) // ':1] "' // &
    trim ( data_filename ) // '" with points pt 5'

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
