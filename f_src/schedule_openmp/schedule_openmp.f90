program main

!*****************************************************************************80
!
!! MAIN is the main program for SCHEDULE_OPENMP.
!
!  Discussion:
!
!    This program demonstrates the difference between default,
!    static and dynamic scheduling for a loop parallelized in OpenMP.
!
!    The purpose of scheduling is to deal with loops in which there is
!    known or suspected imbalance in the work load.  In this example,
!    if the work is divided in the default manner between two threads,
!    the second thread has 3 times the work of the first.  
!
!    Both static and dynamic scheduling, if used, even out the work
!    so that both threads have about the same load.  This could be
!    expected to decrease the run time of the loop by about 1/3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 2010
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_factor
  integer ( kind = 4 ) n_hi
  integer ( kind = 4 ) n_lo
  integer ( kind = 4 ) primes
  real ( kind = 8 ) time1
  real ( kind = 8 ) time2
  real ( kind = 8 ) time3

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SCHEDULE_OPENMP'
  write ( *, '(a)' ) '  FORTRAN90/OpenMP version'
  write ( *, '(a)' ) '  Count the primes from 1 to N.'
  write ( *, '(a)' ) &
    '  This is an unbalanced work load, particular for two threads.'
  write ( *, '(a)' ) '  Demonstrate default, static and dynamic scheduling.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) &
    '  Number of processors available = ', omp_get_num_procs ( )
  write ( * ,'(a,i8)' ) &
    '  Number of threads =              ', omp_get_max_threads ( )

  n_lo = 1
  n_hi = 131072
  n_factor = 2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '                           Default        Static       Dynamic'
  write ( *, '(a)' ) &
    '         N     Pi(N)          Time          Time          Time'
  write ( *, '(a)' ) ' '

  n = n_lo

  do while ( n <= n_hi )

    time1 = omp_get_wtime ( )
    call prime_default ( n, primes )
    time1 = omp_get_wtime ( ) - time1

    time2 = omp_get_wtime ( )
    call prime_static ( n, primes )
    time2 = omp_get_wtime ( ) - time2

    time3 = omp_get_wtime ( )
    call prime_dynamic ( n, primes )
    time3 = omp_get_wtime ( ) - time3

    write ( *, '(2x,i8,2x,i8,g14.6,g14.6,g14.6)' ) &
      n, primes, time1, time2, time3

    n = n * n_factor

  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SCHEDULE_OPENMP'
  write ( *, '(a)' ) '  Normal end of execution.'

  stop
end
subroutine prime_default ( n, total )

!*****************************************************************************80
!
!! PRIME_DEFAULT counts primes, using default scheduling.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the maximum number to check.
!
!    Output, integer ( kind = 4 ) TOTAL, the number of prime numbers up to N.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) prime
  integer ( kind = 4 ) total

  total = 0

!$omp parallel &
!$omp shared ( n ) &
!$omp private ( i, j, prime )

!$omp do reduction ( + : total )

  do i = 2, n

    prime = 1

    do j = 2, i - 1
      if ( mod ( i, j ) == 0 ) then
        prime = 0
        exit
      end if
    end do

    total = total + prime

  end do

!$omp end do

!$omp end parallel

  return
end
subroutine prime_static ( n, total )

!*****************************************************************************80
!
!! PRIME_STATIC counts primes, using static scheduling.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the maximum number to check.
!
!    Output, integer ( kind = 4 ) TOTAL, the number of prime numbers up to N.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) prime
  integer ( kind = 4 ) total

  total = 0

!$omp parallel &
!$omp shared ( n ) &
!$omp private ( i, j, prime )

!$omp do reduction ( + : total ) schedule ( static, 100 )

  do i = 2, n

    prime = 1

    do j = 2, i - 1
      if ( mod ( i, j ) == 0 ) then
        prime = 0
        exit
      end if
    end do

    total = total + prime

  end do

!$omp end do

!$omp end parallel

  return
end
subroutine prime_dynamic ( n, total )

!*****************************************************************************80
!
!! PRIME_DYNAMIC counts primes, using dynamic scheduling.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    10 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the maximum number to check.
!
!    Output, integer ( kind = 4 ) TOTAL, the number of prime numbers up to N.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) prime
  integer ( kind = 4 ) total

  total = 0

!$omp parallel &
!$omp shared ( n ) &
!$omp private ( i, j, prime )

!$omp do reduction ( + : total ) schedule ( dynamic, 100 )

  do i = 2, n

    prime = 1

    do j = 2, i - 1
      if ( mod ( i, j ) == 0 ) then
        prime = 0
        exit
      end if
    end do

    total = total + prime

  end do

!$omp end do

!$omp end parallel

  return
end
