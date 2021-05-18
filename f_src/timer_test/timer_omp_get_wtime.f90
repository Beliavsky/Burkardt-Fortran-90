program main

!*****************************************************************************80
!
!! MAIN is the main program for TIMER_OMP_GET_WTIME.
!
!  Discussion:
!
!    TIMER_OMP_GET_WTIME uses OMP_GET_WTIME as the timer.
!
!    OMP_GET_WTIME is a timing utility accessible to C codes that
!    support OpenMP.  It returns the elapsed wallclock time in seconds.
!
!    Here, we run on as many threads as there are processors.  We could
!    force the number of threads to be 1 to make a better comparison to
!    timers that run on a single processor.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2008
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  integer ( kind = 4 ) proc_num
  integer ( kind = 4 ) thread_num

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TIMER_OMP_GET_WTIME'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Demonstrate the OMP_GET_WTIME timer.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  omp_get_wtime ( ) is an OpenMP function.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  It returns elapsed wall clock time in seconds.'
!
!  How many processors are available?
!
  proc_num = omp_get_num_procs ( )

  thread_num = proc_num

  call omp_set_num_threads ( thread_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of processors is       ', proc_num
  write ( *, '(a,i8)' ) '  Number of threads requested = ', thread_num

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TIMER_OMP_GET_WTIME'
  write ( *, '(a)' ) '  Normal end of execution.'

  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 times the FORTRAN90 RANDOM_NUMBER routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 May 2009
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  implicit none

  integer ( kind = 4 ), parameter :: n_log_min = 0
  integer ( kind = 4 ), parameter :: n_log_max = 18
  integer ( kind = 4 ), parameter :: n_min = 2**n_log_min
  integer ( kind = 4 ), parameter :: n_max = 2**n_log_max
  integer ( kind = 4 ), parameter :: rep_num = 5

  real ( kind = 8 ) delta(n_log_min:n_log_max,rep_num+3)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_log
  integer ( kind = 4 ) rep
  real ( kind = 8 ) wtime
  real ( kind = 8 ) x(n_max)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Time the FORTRAN90 RANDOM_NUMBER routine:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    call random_number ( x(1:n) )'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Data vectors will be of minimum size ', n_min
  write ( *, '(a,i12)' ) '  Data vectors will be of maximum size ', n_max
  write ( *, '(a,i12)' ) '  Number of repetitions of the operation: ', rep_num

  do n_log = n_log_min, n_log_max

    do rep = 1, rep_num

      n = 2**( n_log )

      wtime = omp_get_wtime ( )

      call random_number ( harvest = x(1:n) )

      wtime = omp_get_wtime ( ) - wtime

      delta(n_log,rep) = wtime

    end do

    delta(n_log,rep_num+1) = minval ( delta(n_log,1:rep_num) )
    delta(n_log,rep_num+2) = sum    ( delta(n_log,1:rep_num) ) &
      / real ( rep_num, kind = 8 )
    delta(n_log,rep_num+3) = maxval ( delta(n_log,1:rep_num) )

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Timing results:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Vector Size  Rep #1        Rep #2        Rep #3        ' &
    // 'Rep #4        Rep #5           Min           Ave           Max'
  write ( *, '(a)' ) ' '
  do n_log = n_log_min, n_log_max
    n = 2**( n_log )
    write ( *, '(i10,8f14.6)' ) n, delta(n_log,1:rep_num+3)
  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 times the vectorized EXP routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2008
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  integer ( kind = 4 ), parameter :: n_log_min = 12
  integer ( kind = 4 ), parameter :: n_log_max = 18
  integer ( kind = 4 ), parameter :: n_min = 2**n_log_min
  integer ( kind = 4 ), parameter :: n_max = 2**n_log_max
  integer ( kind = 4 ), parameter :: n_rep = 5

  real ( kind = 8 ) delta(n_log_max,n_rep)
  integer ( kind = 4 ) func
  integer ( kind = 4 ) i_rep
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_log
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) wtime1
  real ( kind = 8 ) wtime2
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) y(n_max)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Time vectorized operations:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    y(1:n) =        x(1:n)  '
  write ( *, '(a)' ) '    y(1:n) = PI *   x(1:n)  '
  write ( *, '(a)' ) '    y(1:n) = sqrt ( x(1:n) )'
  write ( *, '(a)' ) '    y(1:n) = exp  ( x(1:n) )'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Data vectors will be of minimum size ', n_min
  write ( *, '(a,i12)' ) '  Data vectors will be of maximum size ', n_max
  write ( *, '(a,i12)' ) '  Number of repetitions of the operation: ', n_rep

  do func = 1, 4

    do i_rep = 1, n_rep

      do n_log = n_log_min, n_log_max

        n = 2**( n_log )

        call random_number ( harvest = x(1:n) )

        wtime1 = omp_get_wtime ( )

        if ( func == 1 ) then
          y(1:n) = x(1:n)
        else if ( func == 2 ) then
          y(1:n) = pi * x(1:n)
        else if ( func == 3 ) then
          y(1:n) = sqrt ( x(1:n) )
        else if ( func == 4 ) then
          y(1:n) = exp ( x(1:n) )
        end if

        wtime2 = omp_get_wtime ( )

        delta(n_log,i_rep) = wtime2 - wtime1

      end do

    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Timing results:'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    Vector Size  Rep #1        Rep #2        ' &
      //  'Rep #3        Rep #4        Rep #5'
    write ( *, '(a)' ) ' '
    do n_log = n_log_min, n_log_max
      n = 2**( n_log )
      write ( *, '(i10,5f14.6)' ) n, delta(n_log,1:n_rep)
    end do

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 times the unvectorized EXP routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2008
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  integer ( kind = 4 ), parameter :: n_log_min = 12
  integer ( kind = 4 ), parameter :: n_log_max = 18
  integer ( kind = 4 ), parameter :: n_min = 2**n_log_min
  integer ( kind = 4 ), parameter :: n_max = 2**n_log_max
  integer ( kind = 4 ), parameter :: n_rep = 5

  real ( kind = 8 ) delta(n_log_max,n_rep)
  integer ( kind = 4 ) func
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_rep
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_log
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) wtime1
  real ( kind = 8 ) wtime2
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) y(n_max)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Time the unvectorized loops:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    do i = 1, n'
  write ( *, '(a)' ) '      y(i) =        x(i)  '
  write ( *, '(a)' ) '      y(i) = PI *   x(i)  '
  write ( *, '(a)' ) '      y(i) = sqrt ( x(i) )'
  write ( *, '(a)' ) '      y(i) = exp  ( x(i) )'
  write ( *, '(a)' ) '    end do'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Data vectors will be of minimum size ', n_min
  write ( *, '(a,i12)' ) '  Data vectors will be of maximum size ', n_max
  write ( *, '(a,i12)' ) '  Number of repetitions of the operation: ', n_rep

  do func = 1, 4

    do i_rep = 1, n_rep

      do n_log = n_log_min, n_log_max

        n = 2**( n_log )

        call random_number ( harvest = x(1:n) )

        wtime1 = omp_get_wtime ( )

        if ( func == 1 ) then
          do i = 1, n
            y(i) = x(i)
          end do
        else if ( func == 2 ) then
          do i = 1, n
            y(i) = pi * x(i)
          end do
        else if ( func == 3 ) then
          do i = 1, n
            y(i) = sqrt ( x(i) )
          end do
        else if ( func == 4 ) then
          do i = 1, n
            y(i) = exp ( x(i) )
          end do
        end if

        wtime2 = omp_get_wtime ( )

        delta(n_log,i_rep) = wtime2 - wtime1

      end do

    end do

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Timing results:'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '    Vector Size  Rep #1        Rep #2        ' &
      //  'Rep #3        Rep #4        Rep #5'
    write ( *, '(a)' ) ' '
    do n_log = n_log_min, n_log_max
      n = 2**( n_log )
      write ( *, '(i10,5f14.6)' ) n, delta(n_log,1:n_rep)
    end do

  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 times the 2D nearest neighbor problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2008
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  integer ( kind = 4 ), parameter :: n_log_min = 10
  integer ( kind = 4 ), parameter :: n_log_max = 18
  integer ( kind = 4 ), parameter :: n_min = 2**n_log_min
  integer ( kind = 4 ), parameter :: n_max = 2**n_log_max
  integer ( kind = 4 ), parameter :: n_rep = 5
  integer ( kind = 4 ), parameter :: n_test = 1

  real ( kind = 8 ) delta(n_log_max,n_rep)
  real ( kind = 8 ) dist_i
  real ( kind = 8 ) dist_min
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_min
  integer ( kind = 4 ) i_rep
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_log
  real ( kind = 8 ) wtime1
  real ( kind = 8 ) wtime2
  real ( kind = 8 ) x(2,n_max)
  real ( kind = 8 ) y(2)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  Time the 2D nearest neighbor problem.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Given X(2,N) and Y(2),'
  write ( *, '(a)' ) '    find X(2,*) closest to Y(2).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    do i = 1, n'
  write ( *, '(a)' ) '      if distance ( x(2,i), y ) < minimum so far'
  write ( *, '(a)' ) '        x_min = x(2,i)'
  write ( *, '(a)' ) '    end do'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Data vectors will be of minimum size ', n_min
  write ( *, '(a,i12)' ) '  Data vectors will be of maximum size ', n_max
  write ( *, '(a,i12)' ) '  Number of repetitions of the operation: ', n_rep

  call random_number ( harvest = x(1:2,1:n_max) )
  call random_number ( harvest = y(1:2) )

  do i_rep = 1, n_rep

    do n_log = n_log_min, n_log_max

      n = 2**( n_log )

      wtime1 = omp_get_wtime ( )

      dist_min = huge ( dist_min )
      i_min = 0
      do i = 1, n
        dist_i = sum ( ( x(1:2,i) - y(1:2) )**2 )
        if ( dist_i < dist_min ) then
          dist_min = dist_i
          i_min = i
        end if
      end do

      wtime2 = omp_get_wtime ( )
      delta(n_log,i_rep) = wtime2 - wtime1

    end do

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Timing results:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Vector Size  Rep #1        Rep #2        Rep #3        ' &
    // 'Rep #4        Rep #5'
  write ( *, '(a)' ) ' '
  do n_log = n_log_min, n_log_max
    n = 2**( n_log )
    write ( *, '(i10,5f14.6)' ) n, delta(n_log,1:n_rep)
  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 times the matrix multiplication problem problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2008
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  integer ( kind = 4 ), parameter :: l_log_min = 1
  integer ( kind = 4 ), parameter :: l_log_max = 5
  integer ( kind = 4 ), parameter :: l_min = 4**l_log_min
  integer ( kind = 4 ), parameter :: l_max = 4**l_log_max
  integer ( kind = 4 ), parameter :: rep_num = 5

  real ( kind = 8 ), allocatable, dimension ( :, : ) :: a
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: b
  real ( kind = 8 ), allocatable, dimension ( :, : ) :: c
  real ( kind = 8 ) delta(l_log_min:l_log_max,rep_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) l_log
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) rep
  real ( kind = 8 ) wtime1
  real ( kind = 8 ) wtime2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  Time the matrix multiplication problem.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Compute C = A * B'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  where'
  write ( *, '(a)' ) '    A is an L by M matrix,'
  write ( *, '(a)' ) '    B is an M by N matrix,'
  write ( *, '(a)' ) '  and so'
  write ( *, '(a)' ) '    C is an L by N matrix.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Minimum value of L = M = N = ', l_min
  write ( *, '(a,i12)' ) '  Maximum value of L = M = N = ', l_max
  write ( *, '(a,i12)' ) '  Number of repetitions of the operation: ', rep_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Use nested DO loops for matrix multiplication.'

  do rep = 1, rep_num

    do l_log = l_log_min, l_log_max - 1

      l = 4**( l_log )
      m = l
      n = l

      allocate ( a(1:l,1:l) )
      allocate ( b(1:l,1:l) )
      allocate ( c(1:l,1:l) )

      call random_number ( harvest = a(1:l,1:l) )
      call random_number ( harvest = b(1:l,1:l) )

      wtime1 = omp_get_wtime ( )

      do i = 1, l
        do j = 1, l
          c(i,j) = 0.0D+00
          do k = 1, l
            c(i,j) = c(i,j) + a(i,k) * b(k,j)
          end do
        end do
      end do

      wtime2 = omp_get_wtime ( )

      delta(l_log,rep) = wtime2 - wtime1

      deallocate ( a )
      deallocate ( b )
      deallocate ( c )

    end do

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Timing results using nested DO loops:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Vector Size  Rep #1        Rep #2        Rep #3        ' &
    // 'Rep #4        Rep #5'
  write ( *, '(a)' ) ' '
  do l_log = l_log_min, l_log_max - 1
    l = 4**( l_log )
    write ( *, '(i10,5f14.6)' ) l, delta(l_log,1:rep_num)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Use the MATMUL routine for matrix multiplication.'

  do rep = 1, rep_num

    do l_log = l_log_min, l_log_max

      l = 4**( l_log )
      m = l
      n = l

      allocate ( a(1:l,1:l) )
      allocate ( b(1:l,1:l) )
      allocate ( c(1:l,1:l) )

      call random_number ( harvest = a(1:l,1:l) )
      call random_number ( harvest = b(1:l,1:l) )

      wtime1 = omp_get_wtime ( )

      c = matmul ( a, b )

      wtime2 = omp_get_wtime ( )

      delta(l_log,rep) = wtime2 - wtime1

      deallocate ( a )
      deallocate ( b )
      deallocate ( c )

    end do

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Timing results using MATMUL:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    Vector Size  Rep #1        Rep #2        Rep #3        ' &
    // 'Rep #4        Rep #5'
  write ( *, '(a)' ) ' '
  do l_log = l_log_min, l_log_max
    l = 4**( l_log )
    write ( *, '(i10,5f14.6)' ) l, delta(l_log,1:rep_num)
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
!    May 31 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2001
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
  character ( len = 8 ) date
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  character ( len = 10 ) time
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y
  character ( len = 5 ) zone

  call date_and_time ( date, time, zone, values )

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

  write ( *, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
