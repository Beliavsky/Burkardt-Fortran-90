program main

!*****************************************************************************80
!
!! MAIN is the main program for UNICYCLE_TEST.
!
!  Discussion:
!
!    UNICYCLE_TEST tests the UNICYCLE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the UNICYCLE library.'

  call perm_is_unicycle_test ( )
  call unicycle_enum_test ( )
  call unicycle_index_test ( )
  call unicycle_index_to_sequence_test ( )
  call unicycle_inverse_test ( )
  call unicycle_next_test ( )
  call unicycle_random_test ( )
  call unicycle_rank_test ( )
  call unicycle_unrank_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine perm_is_unicycle_test ( )

!*****************************************************************************80
!
!! PERM_IS_UNICYCLE_TEST tests PERM_IS_UNICYCLE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: test_num = 10

  integer ( kind = 4 ) p(n)
  logical perm_is_unicycle
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) u(n)
  logical value

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PERM_IS_UNICYCLE_TEST'
  write ( *, '(a)' ) '  PERM_IS_UNICYCLE determines whether a permutation'
  write ( *, '(a)' ) '  is a unicyle'

  do test = 1, test_num

    call perm_random ( n, seed, p )

    value = perm_is_unicycle ( n, p )

    if ( value ) then

      call perm_print ( n, p, '  This permutation is a unicycle' )
      call unicycle_index_to_sequence ( n, p, u )
      call unicycle_print ( n, u, '  The permutation in sequence form' )

    else

      call perm_print ( n, p, '  This permutation is NOT a unicycle' )

    end if

  end do

  return
end
subroutine unicycle_enum_test ( )

!*****************************************************************************80
!
!! UNICYCLE_ENUM_TEST tests UNICYCLE_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 10

  integer ( kind = 4 ) n
  integer ( kind = 4 ) num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_ENUM_TEST'
  write ( *, '(a)' ) '  UNICYCLE_ENUM enumerates the unicycles of N objects.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  N    Number'
  write ( *, '(a)' ) ' '

  do n = 0, n_max

    call unicycle_enum ( n, num )
    write ( *, '(2x,i3,2x,i8)' ) n, num

  end do

  return
end
subroutine unicycle_index_test ( )

!*****************************************************************************80
!
!! UNICYCLE_INDEX_TEST tests UNICYCLE_INDEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) u(n)
  integer ( kind = 4 ) u_index(n)
  integer ( kind = 4 ) u2(n)
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) :: test_num = 5

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_INDEX_TEST'
  write ( *, '(a)' ) '  UNICYCLE_INDEX converts a unicycle to index form.'

  do test = 1, test_num 

    call unicycle_random ( n, seed, u )

    call unicycle_print ( n, u, '  The unicycle:' )

    call unicycle_index ( n, u, u_index )
    
    call unicycle_index_print ( n, u_index, '  The index form:' )

    call unicycle_index_to_sequence ( n, u_index, u2 )

    call unicycle_print ( n, u2, '  The unicycle recovered:' )

  end do

  return
end
subroutine unicycle_index_to_sequence_test ( )

!*****************************************************************************80
!
!! UNICYCLE_INDEX_TO_SEQUENCE_TEST tests UNICYCLE_INDEX_TO_SEQUENCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ) u(n)
  integer ( kind = 4 ) u_index(n)
  integer ( kind = 4 ) u2(n)
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) test
  integer ( kind = 4 ) :: test_num = 5

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_INDEX_TO_SEQUENCE_TEST'
  write ( *, '(a)' ) '  UNICYCLE_INDEX_TO_SEQUENCE converts an index to unicycle form.'

  do test = 1, test_num 

    call unicycle_random ( n, seed, u )

    call unicycle_print ( n, u, '  The unicycle:' )

    call unicycle_index ( n, u, u_index )
    
    call unicycle_index_print ( n, u_index, '  The index form:' )

    call unicycle_index_to_sequence ( n, u_index, u2 )

    call unicycle_print ( n, u2, '  The unicycle recovered:' )

  end do

  return
end
subroutine unicycle_inverse_test ( )

!*****************************************************************************80
!
!! UNICYCLE_INVERSE_TEST tests UNICYCLE_INVERSE;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 January 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ), dimension ( n ) :: u = (/ 1, 7, 6, 2, 4, 3, 5 /)
  integer ( kind = 4 ) u_inverse(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_INVERSE_TEST'
  write ( *, '(a)' ) '  UNICYCLE_INVERSE inverts a unicycle;'

  call unicycle_print ( n, u, '  The original unicycle:' )
 
  call unicycle_inverse ( n, u, u_inverse )
 
  call unicycle_print ( n, u_inverse, '  The inverse unicycle:' )
 
  return
end
subroutine unicycle_next_test ( )

!*****************************************************************************80
!
!! UNICYCLE_NEXT_TEST tests UNICYCLE_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    17 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer rank
  integer ( kind = 4 ) u(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_NEXT_TEST'
  write ( *, '(a)' ) '  UNICYCLE_NEXT generates unicycles in lex order.'
  write ( *, '(a)' ) ' '
  rank = -1
 
  do

    call unicycle_next ( n, u, rank )

    if ( rank == - 1 ) then
      exit
    end if

    write ( *, '(2x,i3,a1,2x,10i2)' ) rank, ':', u(1:n)

  end do
 
  return
end
subroutine unicycle_random_test ( )

!*****************************************************************************80
!
!! UNICYCLE_RANDOM_TEST tests UNICYCLE_RANDOM;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) u(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_RANDOM_TEST'
  write ( *, '(a)' ) '  UNICYCLE_RANDOM produces a random unicyle;'
  write ( *, '(a,i8)' ) '  For this test, N = ', n
  write ( *, '(a)' ) ' '

  seed = 123456789

  do i = 1, 5
    call unicycle_random ( n, seed, u )
    call unicycle_print ( n, u, ' ' )
  end do
 
  return
end
subroutine unicycle_rank_test ( )

!*****************************************************************************80
!
!! UNICYCLE_RANK_TEST tests UNICYCLE_RANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ), save, dimension ( n ) :: u = (/ 1, 5, 2, 3, 4 /)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_RANK_TEST'
  write ( *, '(a)' ) '  UNICYCLE_RANK ranks a unicycle.'

  call unicycle_print ( n, u, '  The unicycle:' )
 
  call unicycle_rank ( n, u, rank )
 
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The rank is:', rank
 
  return
end
subroutine unicycle_unrank_test ( )

!*****************************************************************************80
!
!! UNICYCLE_UNRANK_TEST tests UNICYCLE_UNRANK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 June 2012
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) u(n)
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'UNICYCLE_UNRANK_TEST'
  write ( *, '(a)' ) '  UNICYCLE_UNRANK, given a rank, computes the'
  write ( *, '(a)' ) '  corresponding unicycle.'
  write ( *, '(a)' ) ' '
  rank = 6
  write ( *, '(a,i8)' ) '  The requested rank is ', rank
 
  call unicycle_unrank ( n, rank, u )
 
  call unicycle_print ( n, u, '  The unicycle:' )
 
  return
end

