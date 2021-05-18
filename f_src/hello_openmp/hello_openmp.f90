program main

!*****************************************************************************80
!
!! MAIN is the main program for HELLO.
!
!  Discussion:
!
!    HELLO is a "Hello, World" program for OpenMP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 June 2010
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  implicit none

  integer ( kind = 4 ) id
  real ( kind = 8 ) wtime

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HELLO_OPENMP'
  write ( *, '(a)' ) '  FORTRAN90/OpenMP version'

  wtime = omp_get_wtime ( )

  write ( *, '(a,i8)' ) &
    '  The number of processors available = ', omp_get_num_procs ( )
  write ( *, '(a,i8)' ) &
    '  The number of threads available    = ', omp_get_max_threads ( )
!
!  OUTSIDE THE PARALLEL REGION, have each thread say hello (there's only 1).
!
  id = omp_get_thread_num ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  OUTSIDE the parallel region.'
  write ( *, '(a)' ) ' '

  write ( *, '(a,i8,a,i8)' ) '  HELLO from process ', id

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Going INSIDE the parallel region:'
  write ( *, '(a)' ) ' '
!
!  INSIDE THE PARALLEL REGION, have each thread say hello.
!
!$omp parallel &
!$omp private ( id )
  id = omp_get_thread_num ( )

  write ( *, '(a,i8,a,i8)' ) '  HELLO from process ', id

!$omp end parallel
!
!  Finish up by measuring the elapsed time.
!
  wtime = omp_get_wtime ( ) - wtime

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Back OUTSIDE the parallel region.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Elapsed wall clock time = ', wtime
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HELLO_OPENMP'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! timestamp prints the current YMDHMS date as a time stamp.
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

