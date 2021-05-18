program main

!*****************************************************************************80
!
!! MAIN is the main program for UBVEC_TEST.
!
!  Discussion:
!
!    UBVEC_TEST tests the UBVEC library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the UBVEC library.'

  call morse_thue_test ( )
  call nim_sum_test ( )
  call ubvec_add_test ( )
  call ubvec_and_test ( )
  call ubvec_complement1_test ( )
  call ubvec_enum_test ( )
  call ubvec_next_test ( )
  call ubvec_next_gray_test ( )
  call ubvec_next_grlex_test ( )
  call ubvec_or_test ( )
  call ubvec_print_test ( )
  call ubvec_random_test ( )
  call ubvec_rank_gray_test ( )
  call ubvec_reverse_test ( )
  call ubvec_to_ui4_test ( )
  call ubvec_unrank_gray_test ( )
  call ubvec_unrank_grlex_test ( )
  call ubvec_xor_test ( )
  call ui4_rank_gray_test ( )
  call ui4_to_ubvec_test ( )
  call ui4_unrank_gray_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine morse_thue_test ( )

!*****************************************************************************80
!
!! MORSE_THUE_TEST tests MORSE_THUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 100

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) s(0:n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'MORSE_THUE_TEST'
  write ( *, '(a)' ) '  MORSE_THUE computes the Morse-Thue numbers.'
  write ( *, '(a)' ) ''

  do i = 0, n
    call morse_thue ( i, s(i) )
  end do

  do ilo = 0, n, 10
    ihi = min ( ilo + 9, n )
    write ( *, '(4x,40i1)' ) s(ilo:ihi)
  end do

  return
end
subroutine nim_sum_test ( )

!*****************************************************************************80
!
!! NIM_SUM_TEST tests NIM_SUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 32

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i1vec(n)
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i2vec(n)
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) i3vec(n)
  integer ( kind = 4 ), parameter :: ihi = 1000
  integer ( kind = 4 ), parameter :: ilo = 0
  integer ( kind = 4 ), parameter :: test_num = 5
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NIM_SUM_TEST'
  write ( *, '(a)' ) '  NIM_SUM computes the Nim sum of two integers.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I    J    Nim(I+J)'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, test_num

    i1 = i4_uniform_ab ( ilo, ihi, seed )
    call ui4_to_ubvec ( i1, n, i1vec )

    i2 = i4_uniform_ab ( ilo, ihi, seed )
    call ui4_to_ubvec ( i2, n, i2vec )

    call nim_sum ( i1, i2, i3 )
    call ui4_to_ubvec ( i3, n, i3vec )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  I1, I2, I3 in decimal:'
    write ( *, '(a)' ) ''
    write ( *, '(i5)' ) i1
    write ( *, '(i5)' ) i2
    write ( *, '(i5)' ) i3
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  I1, I2, I3 in binary:'
    write ( *, '(a)' ) ''
    call ubvec_print ( n, i1vec, '' )
    call ubvec_print ( n, i2vec, '' )
    call ubvec_print ( n, i3vec, '' )

  end do

  return
end
subroutine ubvec_add_test ( )

!*****************************************************************************80
!
!! UBVEC_ADD_TEST tests UBVEC_ADD;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) k2
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)
  integer ( kind = 4 ) ubvec3(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_ADD_TEST'
  write ( *, '(a)' ) '  UBVEC_ADD adds unsigned binary vectors'
  write ( *, '(a)' ) '  representing unsigned integers;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I        J        K = I + J    K = I + J'
  write ( *, '(a)' ) '                          Directly     UBVEC_ADD'
  write ( *, '(a)' ) ''

  do test = 1, 10
    
    i = i4_uniform_ab ( 0, 100, seed )
    j = i4_uniform_ab ( 0, 100, seed )
    k1 = i + j

    call ui4_to_ubvec ( i, n, ubvec1 )
    call ui4_to_ubvec ( j, n, ubvec2 )
    call ubvec_add ( n, ubvec1, ubvec2, ubvec3 )
    call ubvec_to_ui4 ( n, ubvec3, k2 )

    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k1, k2

  end do

  return
end
subroutine ubvec_and_test ( )

!*****************************************************************************80
!
!! UBVEC_AND_TEST tests UBVEC_AND;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)
  integer ( kind = 4 ) ubvec3(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_AND_TEST'
  write ( *, '(a)' ) '  UBVEC_AND computes the AND of two'
  write ( *, '(a)' ) '  unsigned binary vectors representing unsigned integers;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I        J        K = I AND J'
  write ( *, '(a)' ) ''

  do test = 1, 10
    
    i = i4_uniform_ab ( 0, 100, seed )
    j = i4_uniform_ab ( 0, 100, seed )

    call ui4_to_ubvec ( i, n, ubvec1 )
    call ui4_to_ubvec ( j, n, ubvec2 )
    call ubvec_and ( n, ubvec1, ubvec2, ubvec3 )

    call ubvec_to_ui4 ( n, ubvec3, k )

    write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, j, k

  end do

  return
end
subroutine ubvec_complement1_test ( )

!*****************************************************************************80
!
!! UBVEC_COMPLEMENT1_TEST tests UBVEC_COMPLEMENT1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_COMPLEMENT1_TEST'
  write ( *, '(a)' ) '  UBVEC_COMPLEMENT1 returns the 1''s complement'
  write ( *, '(a)' ) '  of an unsigned binary vector.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UBVEC  Comp1'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 5
    call ubvec_random ( n, seed, ubvec1 )
    call ubvec_complement1 ( n, ubvec1, ubvec2 )
    write ( *, '(2x,5i1,2x,5i1)' ) ubvec1(1:n), ubvec2(1:n)
  end do

  return
end
subroutine ubvec_enum_test ( )

!*****************************************************************************80
!
!! UBVEC_ENUM_TEST tests UBVEC_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) ubvec_enum

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_ENUM_TEST'
  write ( *, '(a)' ) '  UBVEC_ENUM enumerates unsigned binary vectors'
  write ( *, '(a)' ) '  of N digits'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   N      Number'
  write ( *, '(a)' ) ''

  do n = 0, 10
    
    n2 = ubvec_enum ( n )

    write ( *, '(2x,i2,2x,i8)' ) n, n2

  end do

  return
end
subroutine ubvec_next_test ( )

!*****************************************************************************80
!
!! UBVEC_NEXT_TEST tests UBVEC_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
 
  integer ( kind = 4 ) ubvec(n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_NEXT_TEST'
  write ( *, '(a)' ) '  UBVEC_NEXT computes the "next" unsigned binary vector.'
  write ( *, '(a)' ) ''

  ubvec(1:n) = 0

  do i = 0, 16
    call ubvec_print ( n, ubvec, '' )
    call ubvec_next ( n, ubvec )
  end do

  return
end
subroutine ubvec_next_gray_test ( )

!*****************************************************************************80
!
!! UBVEC_NEXT_GRAY_TEST tests UBVEC_NEXT_GRAY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) k
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_NEXT_GRAY_TEST'
  write ( *, '(a)' ) '  UBVEC_NEXT_GRAY returns the next UBVEC in the Gray code.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   K  UBVEC'
  write ( *, '(a)' ) ''

  k = 0
  ubvec(1:n) = 0

  do

    write ( *, '(2x,i2,2x,4i2)' ) k, ubvec(1:n)

    k = k + 1
    call ubvec_next_gray ( n, ubvec )

    if ( sum ( ubvec(1:n) ) == 0 ) then
      exit
    end if
    
  end do

  return
end
subroutine ubvec_next_grlex_test ( )

!*****************************************************************************80
!
!! UBVEC_NEXT_GRLEX_TEST tests UBVEC_NEXT_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
 
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_NEXT_GRLEX_TEST'
  write ( *, '(a)' ) '  UBVEC_NEXT_GRLEX computes unsigned binary vectors in GRLEX order.'
  write ( *, '(a)' ) ''

  ubvec(1:n) = 0

  do i = 0, 16
    write ( *, '(2x,i2,a)', advance = 'no' ) i, ':  '
    do j = 1, n
      write ( *, '(i1)', advance = 'no' ) ubvec(j)
    end do
    write ( *, '(a)' ) ''
    call ubvec_next_grlex ( n, ubvec )
  end do

  return
end
subroutine ubvec_or_test ( )

!*****************************************************************************80
!
!! UBVEC_OR_TEST tests UBVEC_OR;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)
  integer ( kind = 4 ) ubvec3(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_OR_TEST'
  write ( *, '(a)' ) '  UBVEC_OR computes the OR of two'
  write ( *, '(a)' ) '  unsigned binary vectors representing unsigned integers;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I        J        K = I OR J'
  write ( *, '(a)' ) ''

  do test = 1, 10
    
    i = i4_uniform_ab ( 0, 100, seed )
    j = i4_uniform_ab ( 0, 100, seed )

    call ui4_to_ubvec ( i, n, ubvec1 )
    call ui4_to_ubvec ( j, n, ubvec2 )
    call ubvec_or ( n, ubvec1, ubvec2, ubvec3 )

    call ubvec_to_ui4 ( n, ubvec3, k )

    write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, j, k

  end do

  return
end
subroutine ubvec_print_test ( )

!*****************************************************************************80
!
!! UBVEC_PRINT_TEST tests UBVEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ), dimension ( n ) :: ubvec = (/ &
    1, 0, 0, 1, 0, 1, 1, 1, 0, 0 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_PRINT_TEST'
  write ( *, '(a)' ) '  UBVEC_PRINT prints an unsigned binary vector.'

  call ubvec_print ( n, ubvec, '  UBVEC:' )

  return
end
subroutine ubvec_random_test ( )

!*****************************************************************************80
!
!! UBVEC_RANDOM_TEST tests UBVEC_RANDOM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_RANDOM_TEST'
  write ( *, '(a)' ) '  UBVEC_RANDOM randomizes an unsigned binary vector.'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 5
    call ubvec_random ( n, seed, ubvec )
    call ubvec_print ( n, ubvec, '' )
  end do

  return
end
subroutine ubvec_rank_gray_test ( )

!*****************************************************************************80
!
!! UBVEC_RANK_GRAY_TEST tests UBVEC_RANK_GRAY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_RANK_GRAY_TEST'
  write ( *, '(a)' ) '  UBVEC_RANK_GRAY ranks a UBVEC in the Gray ordering.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UBVEC   Rank'
  write ( *, '(a)' ) ''
 
  do i4 = 0, 31
    call ui4_to_ubvec ( i4, n, ubvec )
    call ubvec_rank_gray ( n, ubvec, rank )

    write ( *, '(2x,5(i2),2x,i2)' ) ubvec(1:n), rank
  end do

  return
end
subroutine ubvec_reverse_test ( )

!*****************************************************************************80
!
!! UBVEC_REVERSE_TEST tests UBVEC_REVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_REVERSE_TEST'
  write ( *, '(a)' ) '  UBVEC_REVERSE reverses an unsigned binary vector.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  UBVEC  Reversed'
  write ( *, '(a)' ) ''

  seed = 123456789

  do i = 1, 5
    call ubvec_random ( n, seed, ubvec1 )
    call ubvec_reverse ( n, ubvec1, ubvec2 )
    write ( *, '(2x,5i1,2x,5i1)' ) ubvec1(1:n), ubvec2(1:n)
  end do

  return
end
subroutine ubvec_to_ui4_test ( )

!*****************************************************************************80
!
!! UBVEC_TO_UI4_TEST tests UBVEC_TO_UI4;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_TO_UI4_TEST'
  write ( *, '(a)' ) '  UBVEC_TO_UI4 converts an unsigned binary vector'
  write ( *, '(a)' ) '  to an unsigned integer;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   UBVEC  -->  I'
  write ( *, '(a)' ) ''
  do i = 0, 10
    call ui4_to_ubvec ( i, n, ubvec )
    call ubvec_to_ui4 ( n, ubvec, i2 )
    write ( *, '(2x,10i1,2x,i3)' ) ubvec(1:n), i2
  end do

  return
end
subroutine ubvec_unrank_gray_test ( )

!*****************************************************************************80
!
!! UBVEC_UNRANK_GRAY_TEST tests UBVEC_UNRANK_GRAY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_UNRANK_GRAY_TEST'
  write ( *, '(a)' ) '  UBVEC_UNRANK_GRAY unranks a UBVEC.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank  UBVEC'
  write ( *, '(a)' ) ''
 
  do rank = 0, 31
    call ubvec_unrank_gray ( rank, n, ubvec )
    write ( *, '(2x,i4,2x,5(i2))' ) rank, ubvec(1:n)
  end do

  return
end
subroutine ubvec_unrank_grlex_test ( )

!*****************************************************************************80
!
!! UBVEC_UNRANK_GRLEX_TEST tests UBVEC_UNRANK_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) rank
  integer ( kind = 4 ) s
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_UNRANK_GRLEX_TEST'
  write ( *, '(a)' ) '  UBVEC_UNRANK_GRLEX returns the UBVEC of given rank'
  write ( *, '(a)' ) '  in the graded lexicographical ordering.'

  s = -1

  do rank = 0, 15
    call ubvec_unrank_grlex ( rank, n, ubvec )
    if ( s < sum ( ubvec(1:n) ) ) then
      write ( *, '(a)' ) '  --  --------'
      s = sum ( ubvec(1:n) )
    end if
    write ( *, '(2x,i2,2x,4i2)' ) rank, ubvec(1:n)
  end do

  return
end
subroutine ubvec_xor_test ( )

!*****************************************************************************80
!
!! UBVEC_XOR_TEST tests UBVEC_XOR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 May 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ) ubvec1(n)
  integer ( kind = 4 ) ubvec2(n)
  integer ( kind = 4 ) ubvec3(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UBVEC_XOR_TEST'
  write ( *, '(a)' ) '  UBVEC_XOR computes the exclusive OR of two'
  write ( *, '(a)' ) '  unsigned binary vectors representing unsigned integers;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '        I        J        K = I XOR J'
  write ( *, '(a)' ) ''

  do test = 1, 10
    
    i = i4_uniform_ab ( 0, 100, seed )
    j = i4_uniform_ab ( 0, 100, seed )

    call ui4_to_ubvec ( i, n, ubvec1 )
    call ui4_to_ubvec ( j, n, ubvec2 )
    call ubvec_xor ( n, ubvec1, ubvec2, ubvec3 )

    call ubvec_to_ui4 ( n, ubvec3, k )

    write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, j, k

  end do

  return
end
subroutine ui4_rank_gray_test ( )

!*****************************************************************************80
!
!! UI4_RANK_GRAY_TEST tests UI4_RANK_GRAY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UI4_RANK_GRAY_TEST'
  write ( *, '(a)' ) '  UI4_RANK_GRAY ranks an unsigned I4 in the Gray ordering.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) ' UI4  Rank  (binary)'
  write ( *, '(a)' ) ''
 
  do i4 = 0, 31
    call ui4_rank_gray ( i4, rank )
    call ui4_to_ubvec ( i4, n, ubvec )
    write ( *, '(2x,i2,4x,i2,2x,5(i2))' ) i4, rank, ubvec(1:n)
  end do

  return
end
subroutine ui4_to_ubvec_test ( )

!*****************************************************************************80
!
!! UI4_TO_UBVEC_TEST tests UI4_TO_UBVEC;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UI4_TO_UBVEC_TEST'
  write ( *, '(a)' ) '  UI4_TO_UBVEC converts an unsigned integer to an '
  write ( *, '(a)' ) '  unsigned binary vector;'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  I --> UBVEC'
  write ( *, '(a)' ) ''
  do i = 0, 10
    call ui4_to_ubvec ( i, n, ubvec )
    write ( *, '(2x,i3,2x,10i1)' ) i, ubvec(1:n)
  end do

  return
end
subroutine ui4_unrank_gray_test ( )

!*****************************************************************************80
!
!! UI4_UNRANK_GRAY_TEST tests UI4_UNRANK_GRAY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 September 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) rank
  integer ( kind = 4 ) ubvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'UI4_UNRANK_GRAY_TEST'
  write ( *, '(a)' ) '  UI4_UNRANK_GRAY unranks a Gray code.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank   I  (binary)'
  write ( *, '(a)' ) ''
 
  do rank = 0, 31
    call ui4_unrank_gray ( rank, i4 )
    call ui4_to_ubvec ( i4, n, ubvec )
    write ( *, '(2x,i2,4x,i2,2x,5(i2))' ) rank, i4, ubvec(1:n)
  end do

  return
end
