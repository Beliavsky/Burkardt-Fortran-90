program main

!*****************************************************************************80
!
!! MAIN is the main program for BVEC_TEST.
!
!  Discussion:
!
!    BVEC_TEST tests the BVEC library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the BVEC library.'

  call bvec_add_test ( )
  call bvec_complement2_test ( )
  call bvec_mul_test ( )
  call bvec_next_test ( )
  call bvec_next_grlex_test ( )
  call bvec_print_test ( )
  call bvec_sub_test ( )
  call bvec_to_i4_test ( )
  call bvec_uniform_test ( )
  call i4_bclr_test ( )
  call i4_bset_test ( )
  call i4_btest_test ( )
  call i4_to_bvec_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine bvec_add_test ( )

!*****************************************************************************80
!
!! BVEC_ADD_TEST tests BVEC_ADD;
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

  integer ( kind = 4 ) bvec1(n)
  integer ( kind = 4 ) bvec2(n)
  integer ( kind = 4 ) bvec3(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_ADD_TEST'
  write ( *, '(a)' ) '  BVEC_ADD adds binary vectors representing integers;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         J      I + J    BVEC_ADD'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    
    i = i4_uniform_ab ( -100, 100, seed )
    j = i4_uniform_ab ( -100, 100, seed )

    k = i + j

    call i4_to_bvec ( i, n, bvec1 )
    call i4_to_bvec ( j, n, bvec2 )

    call bvec_add ( n, bvec1, bvec2, bvec3 )
    call bvec_to_i4 ( n, bvec3, l )

    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l

  end do

  return
end
subroutine bvec_complement2_test ( )

!*****************************************************************************80
!
!! BVEC_COMPLEMENT2_TEST tests BVEC_COMPLEMENT2;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 November 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) bvec1(n)
  integer ( kind = 4 ) bvec2(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 5

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_COMPLEMENT2_TEST'
  write ( *, '(a)' ) '  BVEC_COMPLEMENT2 returns the two''s complement'
  write ( *, '(a)' ) '  of a (signed) binary vector;'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    
    i = i4_uniform_ab ( -100, 100, seed )

    call i4_to_bvec ( i, n, bvec1 )

    call bvec_complement2 ( n, bvec1, bvec2 )

    call bvec_to_i4 ( n, bvec2, j )

    write ( *, '(a)' ) ' '
    write ( *, '(a,2x,i8)' ) '  I = ', i
    write ( *, '(a,2x,i8)' ) '  J = ', j
    call bvec_print ( n, bvec1, ' ' )
    call bvec_print ( n, bvec2, ' ' )

  end do

  return
end
subroutine bvec_mul_test ( )

!*****************************************************************************80
!
!! BVEC_MUL_TEST tests BVEC_MUL;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 15

  integer ( kind = 4 ) bvec1(n)
  integer ( kind = 4 ) bvec2(n)
  integer ( kind = 4 ) bvec3(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_MUL_TEST'
  write ( *, '(a)' ) '  BVEC_MUL multiplies binary vectors '
  write ( *, '(a)' ) '  representing integers;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         I         J        I * J  BVEC_MUL'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    
    i = i4_uniform_ab ( -100, 100, seed )
    j = i4_uniform_ab ( -100, 100, seed )

    k = i * j

    call i4_to_bvec ( i, n, bvec1 )
    call i4_to_bvec ( j, n, bvec2 )
    call bvec_mul ( n, bvec1, bvec2, bvec3 )
    call bvec_to_i4 ( n, bvec3, l )

    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l

  end do

  return
end
subroutine bvec_next_test ( )

!*****************************************************************************80
!
!! BVEC_NEXT_TEST tests BVEC_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
 
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_NEXT_TEST'
  write ( *, '(a)' ) '  BVEC_NEXT computes the "next" BVEC.'
  write ( *, '(a)' ) ''

  b(1:n) = 0

  do i = 0, 16
    call bvec_print ( n, b, '' )
    call bvec_next ( n, b )
  end do

  return
end
subroutine bvec_next_grlex_test ( )

!*****************************************************************************80
!
!! BVEC_NEXT_GRLEX_TEST tests BVEC_NEXT_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 March 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
 
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_NEXT_GRLEX_TEST'
  write ( *, '(a)' ) '  BVEC_NEXT_GRLEX computes binary vectors in GRLEX order.'
  write ( *, '(a)' ) ''

  b(1:n) = 0

  do i = 0, 16
    write ( *, '(2x,i2,a)', advance = 'no' ) i, ':  '
    do j = 1, n
      write ( *, '(i1)', advance = 'no' ) b(j)
    end do
    write ( *, '(a)' ) ''
    call bvec_next_grlex ( n, b )
  end do

  return
end
subroutine bvec_print_test ( )

!*****************************************************************************80
!
!! BVEC_PRINT_TEST tests BVEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ), dimension ( n ) :: bvec = (/ &
    1, 0, 0, 1, 0, 1, 1, 1, 0, 0 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_PRINT_TEST'
  write ( *, '(a)' ) '  BVEC_PRINT prints a binary vector.'

  call bvec_print ( n, bvec, '  BVEC:' )

  return
end
subroutine bvec_sub_test ( )

!*****************************************************************************80
!
!! BVEC_SUB_TEST tests BVEC_SUB;
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

  integer ( kind = 4 ) bvec1(n)
  integer ( kind = 4 ) bvec2(n)
  integer ( kind = 4 ) bvec4(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) :: seed = 123456789
  integer ( kind = 4 ) test
  integer ( kind = 4 ), parameter :: test_num = 10

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_SUB_TEST'
  write ( *, '(a)' ) '  BVEC_SUB subtracts binary vectors representing integers;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '        I        J        I - J    BVEC_SUB'
  write ( *, '(a)' ) ' '

  do test = 1, test_num
    
    i = i4_uniform_ab ( -100, 100, seed )
    j = i4_uniform_ab ( -100, 100, seed )

    k = i - j

    call i4_to_bvec ( i, n, bvec1 )
    call i4_to_bvec ( j, n, bvec2 )
    call bvec_sub ( n, bvec1, bvec2, bvec4 )
    call bvec_to_i4 ( n, bvec4, l )

    write ( *, '(2x,i8,2x,i8,2x,i8,2x,i8)' ) i, j, k, l

  end do

  return
end
subroutine bvec_to_i4_test ( )

!*****************************************************************************80
!
!! BVEC_TO_I4_TEST tests BVEC_TO_I4;
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) bvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_TO_I4_TEST'
  write ( *, '(a)' ) '  BVEC_TO_I4 converts a signed binary vector'
  write ( *, '(a)' ) '  to an integer;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  I --> BVEC  -->  I'
  write ( *, '(a)' ) ' '
  do i = -3, 10
    call i4_to_bvec ( i, n, bvec )
    call bvec_to_i4 ( n, bvec, i2 )
    write ( *, '(2x,i3,2x,10i1,2x,i3)' ) i, bvec(1:n), i2
  end do

  return
end
subroutine bvec_uniform_test ( )

!*****************************************************************************80
!
!! BVEC_UNIFORM_TEST tests BVEC_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 December 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 10

  logical ( kind = 4 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BVEC_UNIFORM_TEST'
  write ( *, '(a)' ) '  BVEC_UNIFORM computes a binary vector.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  The initial seed is ', seed

  write ( *, '(a)' ) ''
  do i = 1, 10
    call bvec_uniform ( n, seed, b )
    call bvec_print ( n, b, '' )
  end do

  return
end
subroutine i4_bclr_test ( )

!*****************************************************************************80
!
!! I4_BCLR_TEST tests I4_BCLR.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 2

  integer ( kind = 4 ) i4
  integer ( kind = 4 ), dimension ( test_num ) :: i4_test = (/ &
    101, -31 /)
  integer ( kind = 4 ) i4_bclr
  integer ( kind = 4 ) ivec(0:31)
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) pos
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_BCLR_TEST'
  write ( *, '(a)' ) '  I4_BCLR sets a given bit to 0.'
  write ( *, '(a)' ) '  IBCLR is a FORTRAN90 function which does the same.'

  do test = 1, test_num

    i4 = i4_test(test)

    call i4_to_bvec ( i4, 32, ivec )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Working on I4 = ', i4
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '       Pos     Digit       I4_BCLR         IBCLR'
    write ( *, '(a)' ) ' '

    do pos = 0, 31
  
      j1 = i4_bclr ( i4, pos )
      j2 = ibclr ( i4, pos )

      write ( *, '(2x,i8,2x,i8,2x,i12,2x,i12)' ) pos, ivec(pos), j1, j2

    end do

  end do

  return
end
subroutine i4_bset_test ( )

!*****************************************************************************80
!
!! I4_BSET_TEST tests I4_BSET.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 2

  integer ( kind = 4 ) i4
  integer ( kind = 4 ), dimension ( test_num ) :: i4_test = (/ &
    101, -31 /)
  integer ( kind = 4 ) i4_bset
  integer ( kind = 4 ) ivec(0:31)
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) pos
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_BSET_TEST'
  write ( *, '(a)' ) '  I4_BSET sets a given bit to 0.'
  write ( *, '(a)' ) '  IBSET is a FORTRAN90 function which does the same.'

  do test = 1, test_num

    i4 = i4_test(test)

    call i4_to_bvec ( i4, 32, ivec )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Working on I4 = ', i4
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '       Pos     Digit       I4_BSET         IBSET'
    write ( *, '(a)' ) ' '

    do pos = 0, 31
  
      j1 = i4_bset ( i4, pos )
      j2 = ibset ( i4, pos )

      write ( *, '(2x,i8,2x,i8,2x,i12,2x,i12)' ) pos, ivec(pos), j1, j2

    end do

  end do

  return
end
subroutine i4_btest_test ( )

!*****************************************************************************80
!
!! I4_BTEST_TEST tests I4_BTEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    22 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 2

  integer ( kind = 4 ) i4
  integer ( kind = 4 ), dimension ( test_num ) :: i4_test = (/ &
    101, -31 /)
  logical i4_btest
  integer ( kind = 4 ) ivec(0:31)
  logical j1
  logical j2
  integer ( kind = 4 ) pos
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_BTEST_TEST'
  write ( *, '(a)' ) '  I4_BTEST reports whether a given bit is 0 or 1.'
  write ( *, '(a)' ) '  BTEST is a FORTRAN90 function which does the same.'

  do test = 1, test_num

    i4 = i4_test(test)

    call i4_to_bvec ( i4, 32, ivec )

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Analyze the integer I4 = ', i4
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '       Pos     Digit  I4_BTEST     BTEST'
    write ( *, '(a)' ) ' '

    do pos = 0, 31
  
      j1 = i4_btest ( i4, pos )
      j2 = btest ( i4, pos )

      write ( *, '(2x,i8,2x,i8,2x,7x,l1,2x,7x,l1)' ) pos, ivec(pos), j1, j2

    end do

  end do

  return
end
subroutine i4_to_bvec_test ( )

!*****************************************************************************80
!
!! I4_TO_BVEC_TEST tests I4_TO_BVEC;
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

  integer ( kind = 4 ), parameter :: n = 10

  integer ( kind = 4 ) bvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_TO_BVEC_TEST'
  write ( *, '(a)' ) '  I4_TO_BVEC converts an integer to a '
  write ( *, '(a)' ) '  signed binary vector;'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  I --> BVEC  -->  I'
  write ( *, '(a)' ) ' '
  do i = -3, 10
    call i4_to_bvec ( i, n, bvec )
    call bvec_to_i4 ( n, bvec, i2 )
    write ( *, '(2x,i3,2x,10i1,2x,i3)' ) i, bvec(1:n), i2
  end do

  return
end

