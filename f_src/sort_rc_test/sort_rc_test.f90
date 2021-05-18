program main

!*****************************************************************************80
!
!! MAIN is the main program for SORT_RC_TEST.
!
!  Discussion:
!
!    SORT_RC_TEST tests the SORT_RC library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SORT_RC_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SORT_RC library.'

  call sort_rc_test ( )
  call sort_safe_rc_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SORT_RC_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine sort_rc_test ( )

!*****************************************************************************80
!
!! SORT_RC_TEST tests SORT_RC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SORT_RC_TEST'
  write ( *, '(a)' ) '  SORT_RC sorts objects externally.'
  write ( *, '(a)' ) '  This function relies on the use of persistent'
  write ( *, '(a)' ) '  data stored internally.'
!
!  Generate some data to sort.
!
  i4_lo = 1
  i4_hi = n
  seed = 123456789

  call i4vec_uniform_ab ( n, i4_lo, i4_hi, seed, a )
 
  call i4vec_print ( n, a, '  Unsorted array:' )
!
!  Sort the data.
!
  indx = 0

  do

    call sort_rc ( n, indx, i, j, isgn )
 
    if ( indx < 0 ) then
      isgn = 1
      if ( a(i) <= a(j) ) then
        isgn = -1
      end if
    else if ( 0 < indx ) then
      k    = a(i)
      a(i) = a(j)
      a(j) = k
    else
      exit
    end if

  end do
!
!  Display the sorted data.
!
  call i4vec_print ( n, a, '  Sorted array:' )
 
  return
end
subroutine sort_safe_rc_test ( )

!*****************************************************************************80
!
!! SORT_SAFE_RC_TEST tests SORT_SAFE_RC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_save
  integer ( kind = 4 ) i4_hi
  integer ( kind = 4 ) i4_lo
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j_save
  integer ( kind = 4 ) k
  integer ( kind = 4 ) k_save
  integer ( kind = 4 ) l_save
  integer ( kind = 4 ) n_save
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SORT_SAFE_RC_TEST'
  write ( *, '(a)' ) '  SORT_SAFE_RC sorts objects externally.'
  write ( *, '(a)' ) '  This version of the algorithm does not rely on'
  write ( *, '(a)' ) '  internally saved or "persistent" or "static" memory.'
!
!  Generate some data to sort.
!
  i4_lo = 1
  i4_hi = n
  seed = 123456789

  call i4vec_uniform_ab ( n, i4_lo, i4_hi, seed, a )
 
  call i4vec_print ( n, a, '  Unsorted array:' )
!
!  Sort the data.
!
  indx = 0

  do

    call sort_safe_rc ( n, indx, i, j, isgn, &
      i_save, j_save, k_save, l_save, n_save )
 
    if ( indx < 0 ) then
      isgn = 1
      if ( a(i) <= a(j) ) then
        isgn = -1
      end if
    else if ( 0 < indx ) then
      k    = a(i)
      a(i) = a(j)
      a(j) = k
    else
      exit
    end if

  end do
!
!  Display the sorted data.
!
  call i4vec_print ( n, a, '  Sorted array:' )
 
  return
end
